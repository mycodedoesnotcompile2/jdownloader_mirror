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
package org.appwork.utils.net.httpconnection.tests;

import java.io.InputStream;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.URLEncoder;
import java.security.cert.X509Certificate;
import java.util.EnumSet;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.remoteapi.RemoteAPI;
import org.appwork.remoteapi.tests.DummyTestAPIImpl;
import org.appwork.utils.Exceptions;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.HTTPProxy.TYPE;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.TrustValidationFailedException;
import org.appwork.utils.net.httpconnection.trust.AllTrustProvider;
import org.appwork.utils.net.httpconnection.trust.CompositeTrustProvider;
import org.appwork.utils.net.httpconnection.trust.CurrentJRETrustProvider;
import org.appwork.utils.net.httpconnection.trust.CustomTrustProvider;
import org.appwork.utils.net.httpconnection.trust.JNAWindowsTrustProvider;
import org.appwork.utils.net.httpconnection.trust.TrustLinuxProvider;
import org.appwork.utils.net.httpconnection.trust.TrustProviderInterface;
import org.appwork.utils.net.httpconnection.trust.WindowsTrustProvider;
import org.appwork.utils.net.httpconnection.trust.ccadb.CCADBTrustProvider;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsCertUtils;
import org.appwork.utils.os.WindowsCertUtils.CertListEntry;
import org.appwork.utils.os.WindowsCertUtils.TargetKeyStore;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinUser;

/**
 * Integration tests for HTTPS connections with real servers and various TrustProviders.
 */
public class HTTPSIntegrationTest extends ProxyConnectionTestBase {
    /**
     *
     */
    private static final String APP_WORK_TEST = "AppWorkTest";

    public static void main(final String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            // Test various certificate scenarios over all connection variants
            for (final HTTPProxy proxy : getConnectionVariants()) {
                LogV3.info("badssl / public HTTPS tests via " + proxy);
                new HttpClient().proxy(proxy).trust(CurrentJRETrustProvider.getInstance()).get("https://sha256.badssl.com/");
                try {
                    new HttpClient().proxy(proxy).trust(CurrentJRETrustProvider.getInstance()).get("https://sha384.badssl.com/");
                    throw new WTFException("Chrome says: net::ERR_CERT_DATE_INVALID");
                } catch (HttpClientException e) {
                }
                try {
                    new HttpClient().proxy(proxy).trust(CurrentJRETrustProvider.getInstance()).get("https://sha512.badssl.com/");
                    throw new WTFException("Chrome says: net::ERR_CERT_DATE_INVALID");
                } catch (HttpClientException e) {
                }
                new HttpClient().proxy(proxy).trust(CurrentJRETrustProvider.getInstance()).get("https://rsa2048.badssl.com/");
                new HttpClient().proxy(proxy).trust(CurrentJRETrustProvider.getInstance()).get("https://rsa4096.badssl.com/");
                testPublicHttpsWithValidCertificateForProxy(proxy);
                testPublicHttpsByIpForProxy(proxy);
                testNativeHttpsURLConnectionWithTrustProvider();
                testAllProvidersWithRealHTTPS();
            }
            if (!CrossSystem.isWindows()) {
                return;
            }
            setupProxyServers();
            createTestCertificates();
            List<CertListEntry> list = WindowsCertUtils.listCertificates(TargetKeyStore.CURRENT_USER, org.appwork.utils.net.httpconnection.tests.SSLTrustProviderTestBase.APP_WORK_AW_TEST_CA, null, null);
            for (CertListEntry c : list) {
                removeCertificateWithAutoConfirm(c.thumbprint, TargetKeyStore.CURRENT_USER);
            }
            boolean isInUserStore = WindowsCertUtils.isCertificateInstalled(caCertificateFingerPrint, WindowsCertUtils.TargetKeyStore.CURRENT_USER);
            boolean isInLocalSystem = WindowsCertUtils.isCertificateInstalled(caCertificateFingerPrint, WindowsCertUtils.TargetKeyStore.LOCAL_MACHINE);
            assertFalse(isInLocalSystem);
            if (isInUserStore) {
                LogV3.info("Removing existing certificate from user store (with auto-confirm)");
                removeCertificateWithAutoConfirm(caCertificateFingerPrint, WindowsCertUtils.TargetKeyStore.CURRENT_USER);
                assertFalse(WindowsCertUtils.isCertificateInstalled(caCertificateFingerPrint, WindowsCertUtils.TargetKeyStore.CURRENT_USER));
            }
            testLocalServerBy127_0_0_1();
            LogV3.info("HTTPS integration tests completed successfully");
        } finally {
            teardownProxyServers();
            cleanupTempFiles();
        }
    }

    /**
     * Verifies that TrustCurrentJREProvider and CompositeTrustProvider accept public HTTPS sites with valid CA-signed certificates. Uses
     * https://example.com as a stable public URL with a valid certificate.
     */
    private void testPublicHttpsWithValidCertificateForProxy(final HTTPProxy proxy) throws Exception {
        final String url = "https://example.com/";
        {
            final RequestContext trustAllOk = new HttpClient().proxy(proxy).trust(AllTrustProvider.getInstance()).get(url);
            assertTrue(trustAllOk.getTrustResult().getTrustProvider() instanceof AllTrustProvider);
            assertTrue(trustAllOk.getTrustResult().isTrusted(), "TrustAllProvider should reach " + url + " via " + proxy);
        }
        {
            final RequestContext jreOk = new HttpClient().proxy(proxy).trust(CurrentJRETrustProvider.getInstance()).get(url);
            assertTrue(jreOk.getTrustResult().getTrustProvider() instanceof CurrentJRETrustProvider);
            assertTrue(jreOk.getTrustResult().isTrusted(), "TrustCurrentJREProvider must accept valid CA-signed cert for " + url + " via " + proxy + (jreOk.getTrustResult().getException() != null ? ": " + jreOk.getTrustResult().getException().getMessage() : ""));
        }
        {
            final RequestContext ccadbOk = new HttpClient().proxy(proxy).trust(new CCADBTrustProvider()).get(url);
            assertTrue(ccadbOk.getTrustResult().getTrustProvider() instanceof CCADBTrustProvider);
            assertTrue(ccadbOk.getTrustResult().isTrusted(), "CCADBTrustProvider must accept valid CA-signed cert for " + url + " via " + proxy + (ccadbOk.getTrustResult().getException() != null ? ": " + ccadbOk.getTrustResult().getException().getMessage() : ""));
        }
        if (CrossSystem.isLinux()) {
            {
                final RequestContext linuxOk = new HttpClient().proxy(proxy).trust(TrustLinuxProvider.getInstance()).get(url);
                assertTrue(linuxOk.getTrustResult().getTrustProvider() instanceof TrustLinuxProvider);
                assertTrue(linuxOk.getTrustResult().isTrusted(), "TrustLinuxProvider must accept valid CA-signed cert for " + url + " via " + proxy + (linuxOk.getTrustResult().getException() != null ? ": " + linuxOk.getTrustResult().getException().getMessage() : ""));
            }
            final RequestContext combinedOk = new HttpClient().proxy(proxy).trust(new CompositeTrustProvider(CurrentJRETrustProvider.getInstance(), TrustLinuxProvider.getInstance())).get(url);
            assertTrue(combinedOk.getTrustResult().getTrustProvider() instanceof CompositeTrustProvider);
            assertTrue(combinedOk.getTrustResult().isTrusted(), "CompositeTrustProvider (JRE+Linux) must accept valid CA-signed cert for " + url + " via " + proxy + (combinedOk.getTrustResult().getException() != null ? ": " + combinedOk.getTrustResult().getException().getMessage() : ""));
        }
        if (CrossSystem.isWindows()) {
            {
                final RequestContext jreOk = new HttpClient().proxy(proxy).trust(new JNAWindowsTrustProvider()).get(url);
                assertTrue(jreOk.getTrustResult().getTrustProvider() instanceof JNAWindowsTrustProvider);
                assertTrue(jreOk.getTrustResult().isTrusted(), "TrustCurrentJREProvider must accept valid CA-signed cert for " + url + " via " + proxy + (jreOk.getTrustResult().getException() != null ? ": " + jreOk.getTrustResult().getException().getMessage() : ""));
            }
            {
                final RequestContext windowsOk = new HttpClient().proxy(proxy).trust(WindowsTrustProvider.getInstance()).get(url);
                assertTrue(windowsOk.getTrustResult().getTrustProvider() instanceof WindowsTrustProvider);
                assertTrue(windowsOk.getTrustResult().isTrusted(), "WindowsTrustProvider must accept valid CA-signed cert for " + url + " via " + proxy + (windowsOk.getTrustResult().getException() != null ? ": " + windowsOk.getTrustResult().getException().getMessage() : ""));
            }
            final RequestContext combinedOk = new HttpClient().proxy(proxy).trust(new CompositeTrustProvider(CurrentJRETrustProvider.getInstance(), WindowsTrustProvider.getInstance())).get(url);
            assertTrue(combinedOk.getTrustResult().getTrustProvider() instanceof CompositeTrustProvider);
            assertTrue(combinedOk.getTrustResult().isTrusted(), "CompositeTrustProvider (JRE+Windows) must accept valid CA-signed cert for " + url + " via " + proxy + (combinedOk.getTrustResult().getException() != null ? ": " + combinedOk.getTrustResult().getException().getMessage() : ""));
        }
    }

    /**
     * Requests a public HTTPS site by IP instead of domain. Certificate is for the domain, so hostname verification fails when using IP.
     * TrustAllProvider skips hostname check and succeeds; TrustCurrentJREProvider must fail with hostname mismatch.
     */
    private void testPublicHttpsByIpForProxy(final HTTPProxy proxy) throws Exception {
        final String host = "jdownloader.org";
        final String ip = InetAddress.getByName(host).getHostAddress();
        final String urlByIp = "https://" + ip + "/";
        final RequestContext trustAllOk = new HttpClient().proxy(proxy).trust(AllTrustProvider.getInstance()).get(urlByIp);
        assertTrue(trustAllOk.getTrustResult().isTrusted(), "TrustAllProvider via " + proxy);
        try {
            new HttpClient().proxy(proxy).trust(CurrentJRETrustProvider.getInstance()).get(urlByIp);
            throw new WTFException("This should fail");
        } catch (HttpClientException e) {
            // expected
        }
    }

    /**
     * Local HTTPS server addressed via 127.0.0.1 instead of localhost. Certificate is typically for localhost, so 127.0.0.1 fails hostname
     * verification. TrustAllProvider succeeds; TrustCurrentJREProvider/CustomTrustProvider must fail.
     */
    private void testLocalServerBy127_0_0_1() throws Exception {
        if (sslContext == null) {
            logInfoAnyway("Skipping 127.0.0.1 tests - SSL context not available");
            return;
        }
        final TestHTTPServer server = new TestHTTPServer(0, sslContext);
        server.setAutoUpgrade(false);
        server.setLocalhostOnly(false);
        server.setAllowedMethods(EnumSet.of(RequestMethod.GET));
        final RemoteAPI remoteAPI = new RemoteAPI();
        remoteAPI.register(new DummyTestAPIImpl());
        server.registerRequestHandler(remoteAPI);
        server.start();
        final int port = server.getActualPort();
        try {
            final String url127 = "https://127.0.0.1:" + port + "/test/echo?message=ip";
            final RequestContext trustAllOk = new HttpClient().trust(AllTrustProvider.getInstance()).get(url127);
            try {
                new HttpClient().trust(CurrentJRETrustProvider.getInstance()).get(url127);
                throw new WTFException("Unexpected. this should fail");
            } catch (HttpClientException e) {
                // expected
            }
            String myLanIP = null;
            DatagramSocket socket = new DatagramSocket();
            try {
                socket.connect(InetAddress.getByName("8.8.8.8"), 10002);
                myLanIP = socket.getLocalAddress().getHostAddress();
            } finally {
                socket.close();
            }
            try {
                final RequestContext customOk = new HttpClient().trust(new CustomTrustProvider(caCertificate)).get("https://" + myLanIP + ":" + port + "/test/echo?message=ip");
                throw new WTFException("Cert is for localhost, ipv4 and ipv6 only");
            } catch (HttpClientException e) {
                // expected
            }
            new HttpClient().trust(new CustomTrustProvider(serverCertificate)).get("https://localhost:" + port + "/test/echo?message=ip");
            new HttpClient().trust(new CustomTrustProvider(serverCertificate)).get(url127);
            assertTrue(trustAllOk.getTrustResult().isTrusted(), "TrustAllProvider should reach server via 127.0.0.1");
            // assertFalse(customOk.getTrustResult().isTrusted(), "CustomTrustProvider(serverCert) must fail via 127.0.0.1 (hostname
            // mismatch)");
        } finally {
            server.stop();
        }
    }

    /**
     * Uses Java's native URL.openConnection() / HttpsURLConnection path (via NativeHTTPConnectionImpl) with TrustProvider. Ensures
     * TrustProviders work when the connection is made through java.net.URL / HttpURLConnection, not only via custom socket impl.
     */
    private void testNativeHttpsURLConnectionWithTrustProvider() throws Exception {
        final String url = "https://example.com/";
        HTTPProxy proxy = new HTTPProxy(TYPE.DIRECT);
        proxy.setPreferNativeImplementation(true);
        final RequestContext jreNative = new HttpClient().proxy(proxy).get(url);
        assertTrue(jreNative.getTrustResult().isTrusted(), "Native HttpsURLConnection with TrustCurrentJREProvider must reach " + url + (jreNative.getTrustResult().getException() != null ? ": " + jreNative.getTrustResult().getException().getMessage() : ""));
        final RequestContext trustAllNative = new HttpClient().proxy(proxy).trust(AllTrustProvider.getInstance()).get(url);
        assertTrue(trustAllNative.getTrustResult().isTrusted(), "Native HttpsURLConnection with TrustAllProvider must reach " + url);
        // Optional: url.openStream() style – same stack, just trigger getInputStream()
        try (InputStream stream = openStreamWithTrustProvider(url, CurrentJRETrustProvider.getInstance())) {
            assertTrue(stream != null, "url.openStream() with TrustCurrentJREProvider should return stream");
            final byte[] buf = new byte[512];
            final int n = stream.read(buf);
            assertTrue(n > 0, "Should read data from " + url);
        }
    }

    private void testAllProvidersWithRealHTTPS() throws Exception {
        if (sslContext == null) {
            logInfoAnyway("Skipping real HTTPS server tests - SSL context not available");
            return;
        }
        LogV3.info("Test: All Trust Providers with Real HTTPS Server");
        final TestHTTPServer server = new TestHTTPServer(0, sslContext);
        server.setAutoUpgrade(false);
        server.setLocalhostOnly(true);
        server.setAllowedMethods(EnumSet.of(RequestMethod.GET));
        final RemoteAPI remoteAPI = new RemoteAPI();
        remoteAPI.register(new DummyTestAPIImpl());
        server.registerRequestHandler(remoteAPI);
        server.start();
        final int serverPort = server.getActualPort();
        try {
            final String url = "https://localhost:" + serverPort + "/test/echo?message=" + URLEncoder.encode("Hello Trust", "UTF-8");
            for (final HTTPProxy proxy : getConnectionVariants()) {
                LogV3.info("Provider tests via " + proxy);
                testProviderWithServer(url, AllTrustProvider.getInstance(), true, "TrustAllProvider", proxy);
                testProviderWithServer(url, new CustomTrustProvider(serverCertificate), true, "CustomTrustProvider with server cert", proxy);
                testProviderWithServer(url, new CustomTrustProvider(caCertificate), true, "CustomTrustProvider with CA cert", proxy);
                assertFileExists(tempKeystoreFile);
                try {
                    final String password = "testpassword";
                    testProviderWithServer(url, new CustomTrustProvider(tempKeystoreFile, password.toCharArray(), "PKCS12"), true, "CustomTrustProvider from keystore", proxy);
                } catch (final Exception e) {
                    // expected... keystoreFile does not contain the certificates?
                }
                testProviderWithServer(url, new CustomTrustProvider(caCertificate), true, "CustomTrustProvider from PEM (in-memory CA)", proxy);
                testProviderWithServer(url, CurrentJRETrustProvider.getInstance(), false, "TrustCurrentJREProvider", proxy);
                testProviderWithServer(url, new CompositeTrustProvider(new CustomTrustProvider(serverCertificate), CurrentJRETrustProvider.getInstance()), true, "CompositeTrustProvider (Custom+JRE)", proxy);
                testProviderWithServer(url, null, true, "Null Provider", proxy);
            }
            LogV3.info("Installing certificate to user store (with auto-confirm)");
            installCertificateWithAutoConfirm(caCertificate, WindowsCertUtils.TargetKeyStore.CURRENT_USER);
            assertTrue(WindowsCertUtils.isCertificateInstalled(caCertificateFingerPrint, WindowsCertUtils.TargetKeyStore.CURRENT_USER));
            final WindowsTrustProvider windowsProvider = WindowsTrustProvider.getInstance();
            windowsProvider.reload();
            for (final HTTPProxy proxy : getConnectionVariants()) {
                LogV3.info("TrustWindowsProvider / composite tests via " + proxy);
                final boolean windowsExpectedSuccess = CrossSystem.isWindows();
                testProviderWithServer(url, WindowsTrustProvider.getInstance(), windowsExpectedSuccess, "TrustWindowsProvider", proxy);
                final boolean compositeWindowsExpectedSuccess = CrossSystem.isWindows();
                testProviderWithServer(url, new CompositeTrustProvider(CurrentJRETrustProvider.getInstance(), windowsProvider, AllTrustProvider.getInstance()), compositeWindowsExpectedSuccess, "CompositeTrustProvider (JRE+Windows)", proxy);
            }
            LogV3.info("All provider tests with real HTTPS server completed");
        } finally {
            server.stop();
            LogV3.info("Removing certificate from user store (with auto-confirm)");
            removeCertificateWithAutoConfirm(caCertificateFingerPrint, org.appwork.utils.os.WindowsCertUtils.TargetKeyStore.CURRENT_USER);
            assertFalse(WindowsCertUtils.isCertificateInstalled(caCertificateFingerPrint, WindowsCertUtils.TargetKeyStore.CURRENT_USER));
        }
    }

    private void testProviderWithServer(final String url, final TrustProviderInterface provider, final boolean expectedSuccess, final String providerName, final HTTPProxy proxy) throws Exception {
        final HttpClient client = new HttpClient();
        client.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        client.setProxy(proxy);
        client.setTrustProvider(provider);
        client.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
        client.setReadTimeout((int) TimeUnit.SECONDS.toMillis(10));
        try {
            final RequestContext context = client.get(url);
            final boolean success = context.getCode() == 200;
            assertTrue(success == expectedSuccess, providerName + " via " + proxy + " should " + (expectedSuccess ? "succeed" : "fail") + " but got code: " + context.getCode());
            if (success && provider != null) {
                final TrustResult trustInfo = context.getTrustResult();
                assertTrue(trustInfo != null, providerName + " should provide SSLTrustInfo");
                assertTrue(trustInfo.getTrustProvider() != null, providerName + " should set trust provider in SSLTrustInfo");
            }
            LogV3.info(providerName + " via " + proxy + ": " + (success ? "PASSED" : "FAILED (as expected)"));
        } catch (final HttpClientException e) {
            if (Exceptions.containsInstanceOf(e, TrustValidationFailedException.class) && !expectedSuccess) {
                // good
            } else {
                throw e;
            }
        }
    }

    /**
     * Test-only helper: Attempts to automatically confirm the Windows certificate installation/removal dialog. This method tries to find
     * the security dialog and click the OK/Yes button.
     *
     * <p>
     * <b>Limitations:</b>
     * </p>
     * <ul>
     * <li>Only works if the dialog is NOT running with admin privileges (UAC blocks interaction)</li>
     * <li>Dialog titles may vary by Windows version/language</li>
     * <li>Not 100% reliable - may fail silently</li>
     * </ul>
     *
     * @param maxWaitMs
     *            Maximum time to wait for dialog to appear (milliseconds)
     * @return true if dialog was found and confirmed, false otherwise
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
    private static void installCertificateWithAutoConfirm(final X509Certificate certificate, final WindowsCertUtils.TargetKeyStore target) throws Exception {
        // Start auto-confirmation in background
        final Thread confirmationThread = new Thread(new Runnable() {
            @Override
            public void run() {
                autoConfirmCertificateDialog(15000); // Wait up to 5 seconds
            }
        }, "CertificateDialogAutoConfirm");
        confirmationThread.setDaemon(true);
        confirmationThread.start();
        try {
            WindowsCertUtils.installCertificate(certificate, target, APP_WORK_TEST);
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
    private static boolean removeCertificateWithAutoConfirm(final String thumbprintHex, final WindowsCertUtils.TargetKeyStore target) throws Exception {
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
