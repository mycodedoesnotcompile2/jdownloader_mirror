/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.os.tests;

import java.security.MessageDigest;
import java.security.cert.X509Certificate;
import java.util.List;

import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.net.httpconnection.tests.CertificateFactory;
import org.appwork.utils.net.httpconnection.tests.CertificateFactory.ServerCertificateResult;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsCertUtils;
import org.appwork.utils.os.WindowsCertUtils.CertListEntry;
import org.appwork.utils.os.WindowsCertUtils.KeyStore;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinUser;

/**
 * Test suite for WindowsCertUtils focusing on certificate installation, listing, and uninstallation with friendly names. Based on
 * HTTPSIntegrationTest but without HTTP functionality.
 *
 * @author thomas
 * @date 03.02.2026
 */
@TestDependency({ "org.appwork.utils.os.WindowsCertUtils" })
public class WindowsCertUtilsTest extends AWTest {
    private static final String TEST_FRIENDLY_NAME = "AppWorkTestCert";
    private static final String TEST_CA_NAME       = "AppWork AWTest CA";
    /** OID id-kp-serverAuth (Erweiterte Schlüsselverwendung / Zweck) */
    private static final String EKU_SERVER_AUTH    = "1.3.6.1.5.5.7.3.1";
    /** OID id-kp-clientAuth (Erweiterte Schlüsselverwendung / Zweck) */
    private static final String EKU_CLIENT_AUTH    = "1.3.6.1.5.5.7.3.2";
    private X509Certificate     caCertificate;
    private String              caCertificateFingerPrint;

    public static void main(final String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            logInfoAnyway("Test must run on Windows!");
            return;
        }
        LogV3.info("Starting WindowsCertUtils test...");
        // Create test certificates
        try {
            createTestCertificates();
            // Verify CA certificate has correct Extended Key Usage (Zweck)
            testCaCertificateExtendedKeyUsage();
            // Clean up any existing test certificates
            cleanupExistingTestCertificates();
            // Test installation with friendly name
            testInstallCertificate();
            // Test listing certificates
            testListCertificates();
            // Test uninstallation
            testUninstallCertificate();
        } finally {
        }
        LogV3.info("WindowsCertUtils test completed successfully");
    }

    /**
     * Creates test certificates using CertificateFactory.
     */
    private void createTestCertificates() throws Exception {
        LogV3.info("Creating test certificates using CertificateFactory...");
        final ServerCertificateResult certResult = CertificateFactory.createCACertificateAndServerCertificate(TEST_CA_NAME, "localhost");
        assertNotNull(certResult, "Certificate creation should succeed");
        assertNotNull(certResult.getCaCertificate(), "CA certificate should not be null");
        assertNotNull(certResult.getServerCertificate(), "Server certificate should not be null");
        assertNotNull(certResult.getServerKeyPair(), "Server key pair should not be null");
        this.caCertificate = certResult.getCaCertificate();
        // Calculate CA certificate fingerprint
        final MessageDigest md = MessageDigest.getInstance("SHA-1");
        md.update(caCertificate.getEncoded());
        final byte[] thumbprintBytes = md.digest();
        final StringBuilder thumbprint = new StringBuilder();
        for (final byte b : thumbprintBytes) {
            thumbprint.append(String.format("%02X", b));
        }
        this.caCertificateFingerPrint = thumbprint.toString();
        LogV3.info("Test certificates created successfully. Fingerprint: " + this.caCertificateFingerPrint);
    }

    /**
     * Verifies that the CA certificate has the expected Extended Key Usage (Zweck): Server-Authentifizierung und Client-Authentifizierung.
     */
    private void testCaCertificateExtendedKeyUsage() throws Exception {
        LogV3.info("Test: CA certificate Extended Key Usage (Zweck)...");
        final List<String> eku = caCertificate.getExtendedKeyUsage();
        assertNotNull(eku, "CA certificate should have Extended Key Usage extension");
        assertTrue(eku.contains(EKU_SERVER_AUTH), "CA certificate should contain id-kp-serverAuth (1.3.6.1.5.5.7.3.1), got: " + eku);
        assertTrue(eku.contains(EKU_CLIENT_AUTH), "CA certificate should contain id-kp-clientAuth (1.3.6.1.5.5.7.3.2), got: " + eku);
        LogV3.info("CA certificate Extended Key Usage test passed (serverAuth, clientAuth)");
    }

    /**
     * Cleans up any existing test certificates from previous test runs.
     */
    private void cleanupExistingTestCertificates() throws Exception {
        LogV3.info("Cleaning up any existing test certificates...");
        List<CertListEntry> list = WindowsCertUtils.listCertificates(KeyStore.CURRENT_USER, TEST_CA_NAME, null, null);
        for (CertListEntry c : list) {
            LogV3.info("Removing existing certificate: " + c.thumbprint);
            removeCertificateWithAutoConfirm(c.thumbprint, KeyStore.CURRENT_USER);
        }
        // Also check by friendly name
        list = WindowsCertUtils.listCertificates(KeyStore.CURRENT_USER, null, null, TEST_FRIENDLY_NAME);
        for (CertListEntry c : list) {
            LogV3.info("Removing existing certificate by friendly name: " + c.thumbprint);
            removeCertificateWithAutoConfirm(c.thumbprint, KeyStore.CURRENT_USER);
        }
        // Verify cleanup
        boolean isInstalled = WindowsCertUtils.isCertificateInstalled(caCertificateFingerPrint, KeyStore.CURRENT_USER);
        assertFalse(isInstalled, "Certificate should not be installed before test");
        LogV3.info("Cleanup completed");
    }

    /**
     * Tests certificate installation with friendly name.
     */
    private void testInstallCertificate() throws Exception {
        LogV3.info("Test: Installing certificate with friendly name...");
        // Verify certificate is not installed
        boolean isInstalled = WindowsCertUtils.isCertificateInstalled(caCertificateFingerPrint, KeyStore.CURRENT_USER);
        assertFalse(isInstalled, "Certificate should not be installed before installation");
        // Install certificate with friendly name
        installCertificateWithAutoConfirm(caCertificate, KeyStore.CURRENT_USER, TEST_FRIENDLY_NAME);
        // Verify installation
        isInstalled = WindowsCertUtils.isCertificateInstalled(caCertificateFingerPrint, KeyStore.CURRENT_USER);
        assertTrue(isInstalled, "Certificate should be installed after installation");
        LogV3.info("Certificate installation test passed");
    }

    /**
     * Tests listing certificates and verifying friendly name.
     */
    private void testListCertificates() throws Exception {
        LogV3.info("Test: Listing certificates and verifying friendly name...");
        // List by thumbprint
        List<CertListEntry> list = WindowsCertUtils.listCertificates(KeyStore.CURRENT_USER, TEST_CA_NAME, null, null);
        assertTrue(list.size() > 0, "Should find at least one certificate");
        boolean found = false;
        for (CertListEntry entry : list) {
            if (caCertificateFingerPrint.equalsIgnoreCase(entry.thumbprint)) {
                found = true;
                LogV3.info("Found certificate: subject=" + entry.subject + ", issuer=" + entry.issuer + ", friendlyName=" + entry.friendlyName + ", thumbprint=" + entry.thumbprint);
                assertTrue(TEST_FRIENDLY_NAME.equals(entry.friendlyName), "Friendly name should be set to '" + TEST_FRIENDLY_NAME + "' but was '" + entry.friendlyName + "'");
                break;
            }
        }
        assertTrue(found, "Should find the installed certificate");
        // List by friendly name
        list = WindowsCertUtils.listCertificates(KeyStore.CURRENT_USER, null, null, TEST_FRIENDLY_NAME);
        assertTrue(list.size() > 0, "Should find certificate by friendly name");
        found = false;
        for (CertListEntry entry : list) {
            if (TEST_FRIENDLY_NAME.equals(entry.friendlyName) && caCertificateFingerPrint.equalsIgnoreCase(entry.thumbprint)) {
                found = true;
                break;
            }
        }
        assertTrue(found, "Should find certificate by friendly name");
        LogV3.info("Certificate listing test passed");
    }

    /**
     * Tests certificate uninstallation.
     */
    private void testUninstallCertificate() throws Exception {
        LogV3.info("Test: Uninstalling certificate...");
        // Verify certificate is installed
        boolean isInstalled = WindowsCertUtils.isCertificateInstalled(caCertificateFingerPrint, KeyStore.CURRENT_USER);
        assertTrue(isInstalled, "Certificate should be installed before uninstallation");
        // Uninstall certificate
        boolean removed = removeCertificateWithAutoConfirm(caCertificateFingerPrint, KeyStore.CURRENT_USER);
        assertTrue(removed, "Certificate should be removed");
        // Verify uninstallation
        isInstalled = WindowsCertUtils.isCertificateInstalled(caCertificateFingerPrint, KeyStore.CURRENT_USER);
        assertFalse(isInstalled, "Certificate should not be installed after uninstallation");
        LogV3.info("Certificate uninstallation test passed");
    }

    /**
     * Test-only helper: Attempts to automatically confirm the Windows certificate installation/removal dialog.
     */
    private static boolean autoConfirmCertificateDialog(final int maxWaitMs) {
        if (!CrossSystem.isWindows()) {
            return false;
        }
        final long startTime = System.currentTimeMillis();
        final int checkInterval = 100; // Check every 100ms
        // Typical dialog titles (may vary by Windows version/language)
        final String[] possibleTitles = { "Windows Security", "Windows-Sicherheit", "Security Warning", "Sicherheitswarnung", "Stammzertifikatspeicher" };
        final String[] buttonTexts = { "&OK", "&Ja", "&Yes", "&Installieren", "&Install" };
        while (System.currentTimeMillis() - startTime < maxWaitMs) {
            WinDef.HWND dialogWindow = null;
            // Search for dialog window
            for (final String title : possibleTitles) {
                dialogWindow = User32.INSTANCE.FindWindow(null, title);
                if (dialogWindow != null && Pointer.nativeValue(dialogWindow.getPointer()) != 0L) {
                    break;
                }
            }
            if (dialogWindow != null && Pointer.nativeValue(dialogWindow.getPointer()) != 0L) {
                // Found dialog, try to find and click OK button
                final WinDef.HWND okButton = findButtonByText(dialogWindow, buttonTexts);
                if (okButton != null && Pointer.nativeValue(okButton.getPointer()) != 0L) {
                    // Send click message
                    final int BM_CLICK = 0x00F5;
                    User32.INSTANCE.SendMessage(okButton, BM_CLICK, null, null);
                    LogV3.info("Automatically confirmed certificate dialog");
                    return true;
                }
            }
            // Wait before next check
            try {
                Thread.sleep(checkInterval);
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
                return false;
            }
        }
        return false;
    }

    /**
     * Test-only helper: Finds a button by its text within a parent window.
     */
    private static WinDef.HWND findButtonByText(final WinDef.HWND parent, final String[] buttonTexts) {
        final WinDef.HWND[] found = { null };
        User32.INSTANCE.EnumChildWindows(parent, new WinUser.WNDENUMPROC() {
            @Override
            public boolean callback(final WinDef.HWND hWnd, final Pointer userData) {
                // Check if it's a Button
                final char[] className = new char[256];
                User32.INSTANCE.GetClassName(hWnd, className, 256);
                final String classNameStr = Native.toString(className);
                if ("Button".equals(classNameStr)) {
                    // Check button text
                    final int length = User32.INSTANCE.GetWindowTextLength(hWnd);
                    if (length > 0) {
                        final char[] text = new char[length + 1];
                        User32.INSTANCE.GetWindowText(hWnd, text, length + 1);
                        final String buttonText = Native.toString(text);
                        for (final String searchText : buttonTexts) {
                            if (buttonText.equalsIgnoreCase(searchText)) {
                                found[0] = hWnd;
                                return false; // Stop enumeration
                            }
                        }
                    }
                }
                return true; // Continue enumeration
            }
        }, null);
        return found[0];
    }

    /**
     * Test-only helper: Installs certificate with auto-confirmation of Windows dialog.
     */
    private static void installCertificateWithAutoConfirm(final X509Certificate certificate, final WindowsCertUtils.KeyStore target, final String friendlyName) throws Exception {
        // Start auto-confirmation in background
        final Thread confirmationThread = new Thread(new Runnable() {
            @Override
            public void run() {
                autoConfirmCertificateDialog(15000); // Wait up to 15 seconds
            }
        }, "CertificateDialogAutoConfirm");
        confirmationThread.setDaemon(true);
        confirmationThread.start();
        try {
            WindowsCertUtils.installCertificate(certificate, target, friendlyName);
        } finally {
            try {
                confirmationThread.join(6000); // Wait for confirmation thread
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    /**
     * Test-only helper: Removes certificate with auto-confirmation of Windows dialog.
     */
    private static boolean removeCertificateWithAutoConfirm(final String thumbprintHex, final WindowsCertUtils.KeyStore target) throws Exception {
        // Start auto-confirmation in background
        final Thread confirmationThread = new Thread(new Runnable() {
            @Override
            public void run() {
                autoConfirmCertificateDialog(5000); // Wait up to 5 seconds
            }
        }, "CertificateDialogAutoConfirm");
        confirmationThread.setDaemon(true);
        confirmationThread.start();
        try {
            return WindowsCertUtils.removeCertificate(thumbprintHex, target);
        } finally {
            try {
                confirmationThread.join(6000); // Wait for confirmation thread
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }
}
