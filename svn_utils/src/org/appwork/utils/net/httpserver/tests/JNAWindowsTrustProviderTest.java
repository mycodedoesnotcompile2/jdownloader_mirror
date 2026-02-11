/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpserver.tests;

import java.net.URLEncoder;
import java.security.KeyStore;
import java.security.cert.X509Certificate;
import java.util.EnumSet;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.SSLContext;

import org.appwork.JNAHelper;
import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.remoteapi.RemoteAPI;
import org.appwork.remoteapi.tests.DummyTestAPIImpl;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.net.httpclient.HttpClientException;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpconnection.tests.CertificateFactory;
import org.appwork.utils.net.httpconnection.tests.CertificateFactory.CACertificateResult;
import org.appwork.utils.net.httpconnection.tests.CertificateFactory.ServerCertificateResult;
import org.appwork.utils.net.httpconnection.trust.JNAWindowsTrustProvider;
import org.appwork.utils.net.httpconnection.trust.JNAWindowsTrustResult;
import org.appwork.utils.net.httpserver.SSLHttpServer;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.WindowsCertUtils;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinUser;

/**
 * Test for {@link JNAWindowsTrustProvider}: verifies that validation uses the live Windows trust store (no reload).
 * <p>
 * <b>Test sequence:</b>
 * </p>
 * <ol>
 * <li><b>CA in store:</b> Install test CA in Windows Root (Current User), then HTTPS request with JNAWindowsTrustProvider → must succeed (200).</li>
 * <li><b>CA not in store:</b> Remove CA from Windows store, then same HTTPS request → must fail (connection must be rejected or TrustResult not trusted). This proves that the provider really uses the current store.</li>
 * <li><b>Re-install:</b> Install CA again, request again → must succeed (no reload needed).</li>
 * </ol>
 * <p>
 * Windows + JNA only. Uses auto-confirmation of the Windows certificate dialog for install/remove.
 * </p>
 */
public class JNAWindowsTrustProviderTest extends AWTest {
    private static final String WINDOWS_TEST_CA_FRIENDLY_NAME_JNA = "AppWork JNAWindowsTrustProviderTest CA";

    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        if (!CrossSystem.isWindows()) {
            LogV3.info("Skipping JNAWindowsTrustProviderTest – not Windows");
            return;
        }
        if (!JNAHelper.isJNAAvailable()) {
            LogV3.info("Skipping JNAWindowsTrustProviderTest – JNA not available");
            return;
        }
        CACertificateResult caResult = CertificateFactory.createCACertificate("JNA Test CA");
        assertTrue(caResult != null, "CA creation should succeed");
        ServerCertificateResult serverResult = CertificateFactory.createServerCertificate(caResult.getCaCertificate(), caResult.getCaKeyPair().getPrivate(), "localhost");
        assertTrue(serverResult != null, "Server certificate creation should succeed");
        final char[] password = "testpassword".toCharArray();
        KeyStore serverKeyStore = CertificateFactory.createPKCS12KeyStore(serverResult.getServerCertificate(), serverResult.getServerKeyPair().getPrivate(), password, "server", serverResult.getCaCertificate());
        SSLContext sslContext = SSLHttpServer.createSSLContextFromKeyStore(serverKeyStore, password);
        assertTrue(sslContext != null, "SSL context should not be null");
        SSLHttpServer server = new SSLHttpServer(0, sslContext);
        server.setLocalhostOnly(true);
        server.setAllowedMethods(EnumSet.of(RequestMethod.GET, RequestMethod.POST));
        RemoteAPI remoteAPI = new RemoteAPI();
        remoteAPI.register(new DummyTestAPIImpl());
        server.registerRequestHandler(remoteAPI);
        server.start();
        final int serverPort = server.getActualPort();
        LogV3.info("JNAWindowsTrustProviderTest: SSL HTTP Server started on port " + serverPort);
        try {
            final X509Certificate caCert = serverResult.getCaCertificate();
            final String thumbprint = WindowsCertUtils.getCertificateFingerprint(caCert);
            final String url = "https://localhost:" + serverPort + "/test/echo?message=" + URLEncoder.encode("JNATrustOK", "UTF-8");
            try {
                installCertificateWithAutoConfirm(caCert, WindowsCertUtils.KeyStore.CURRENT_USER, WINDOWS_TEST_CA_FRIENDLY_NAME_JNA);
                assertTrue(WindowsCertUtils.isCertificateInstalled(thumbprint, WindowsCertUtils.KeyStore.CURRENT_USER), "CA should be in Windows store after install");
                HttpClient client = new HttpClient();
                client.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
                client.setReadTimeout((int) TimeUnit.SECONDS.toMillis(10));
                client.putRequestHeader(HTTPConstants.X_APPWORK, "1");
                client.setTrustProvider(JNAWindowsTrustProvider.getInstance());
                RequestContext ctx = client.get(url);
                assertTrue(ctx.getCode() == 200, "JNAWindowsTrustProvider: HTTPS request should return 200 when CA installed, was " + ctx.getCode());
                assertTrue(ctx.getResponseString() != null && ctx.getResponseString().contains("JNATrustOK"), "Response should contain message");
                assertTrustResultContainsCa(ctx, thumbprint, "after first install");
                LogV3.info("JNAWindowsTrustProviderTest: request with CA installed passed");
                removeCertificateWithAutoConfirm(thumbprint, WindowsCertUtils.KeyStore.CURRENT_USER);
                assertFalse(WindowsCertUtils.isCertificateInstalled(thumbprint, WindowsCertUtils.KeyStore.CURRENT_USER), "CA should be removed from Windows store");
                // Request must fail when server CA is NOT in Windows store (proof that we use live store)
                client = new HttpClient();
                client.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
                client.setReadTimeout((int) TimeUnit.SECONDS.toMillis(10));
                client.putRequestHeader(HTTPConstants.X_APPWORK, "1");
                client.setTrustProvider(JNAWindowsTrustProvider.getInstance());
                boolean requestFailedWithoutCA = false;
                try {
                    ctx = client.get(url);
                    if (ctx.getCode() != 200 || (ctx.getTrustResult() != null && !ctx.getTrustResult().isTrusted())) {
                        requestFailedWithoutCA = true;
                    }
                } catch (final HttpClientException e) {
                    requestFailedWithoutCA = true;
                    LogV3.info("JNAWindowsTrustProviderTest: request without CA correctly failed with exception: " + e.getMessage());
                }
                assertTrue(requestFailedWithoutCA, "Request MUST fail when server CA is not in Windows store (JNAWindowsTrustProvider must reject untrusted chain)");
                installCertificateWithAutoConfirm(caCert, WindowsCertUtils.KeyStore.CURRENT_USER, WINDOWS_TEST_CA_FRIENDLY_NAME_JNA);
                assertTrue(WindowsCertUtils.isCertificateInstalled(thumbprint, WindowsCertUtils.KeyStore.CURRENT_USER), "CA should be in Windows store after re-install");
                client = new HttpClient();
                client.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
                client.setReadTimeout((int) TimeUnit.SECONDS.toMillis(10));
                client.putRequestHeader(HTTPConstants.X_APPWORK, "1");
                client.setTrustProvider(JNAWindowsTrustProvider.getInstance());
                ctx = client.get(url);
                assertTrue(ctx.getCode() == 200, "JNAWindowsTrustProvider: HTTPS request after re-install should return 200, was " + ctx.getCode());
                assertTrue(ctx.getResponseString() != null && ctx.getResponseString().contains("JNATrustOK"), "Response should contain message");
                assertTrustResultContainsCa(ctx, thumbprint, "after re-install");
                LogV3.info("JNAWindowsTrustProviderTest passed (install → success, remove → fail, re-install → success, no reload)");
            } finally {
                try {
                    removeCertificateWithAutoConfirm(thumbprint, WindowsCertUtils.KeyStore.CURRENT_USER);
                    assertFalse(WindowsCertUtils.isCertificateInstalled(thumbprint, WindowsCertUtils.KeyStore.CURRENT_USER), "CA should be removed from Windows store after test");
                } catch (final Throwable e) {
                    LogV3.log(e);
                }
            }
        } finally {
            server.shutdown();
            LogV3.info("JNAWindowsTrustProviderTest: SSL HTTP Server stopped");
        }
    }

    /**
     * Asserts that the request context's trust result is a {@link JNAWindowsTrustResult} and that
     * {@link JNAWindowsTrustResult#getTrustedRootCertificate()} is the test CA (same fingerprint).
     */
    private void assertTrustResultContainsCa(final RequestContext ctx, final String expectedCaThumbprint, final String when) throws Exception {
        assertTrue(ctx.getTrustResult() != null, "TrustResult should be present " + when);
        assertTrue(ctx.getTrustResult() instanceof JNAWindowsTrustResult, "TrustResult should be JNAWindowsTrustResult " + when);
        final JNAWindowsTrustResult jnaResult = (JNAWindowsTrustResult) ctx.getTrustResult();
        assertTrue(jnaResult.getTrustedRootCertificate() != null, "Trusted root CA should be set in TrustResult " + when);
        final String actualThumbprint = WindowsCertUtils.getCertificateFingerprint(jnaResult.getTrustedRootCertificate());
        assertTrue(expectedCaThumbprint.equalsIgnoreCase(actualThumbprint), "TrustResult's trusted root should be the test CA " + when + "; expected thumbprint " + expectedCaThumbprint + ", got " + actualThumbprint);
    }

    private static boolean autoConfirmCertificateDialog(final int maxWaitMs) {
        if (!CrossSystem.isWindows()) {
            return false;
        }
        final long startTime = System.currentTimeMillis();
        final int checkInterval = 100;
        final String[] possibleTitles = { "Windows Security", "Windows-Sicherheit", "Security Warning", "Sicherheitswarnung", "Stammzertifikatspeicher" };
        final String[] buttonTexts = { "&OK", "&Ja", "&Yes", "&Installieren", "&Install" };
        while (System.currentTimeMillis() - startTime < maxWaitMs) {
            WinDef.HWND dialogWindow = null;
            for (final String title : possibleTitles) {
                dialogWindow = User32.INSTANCE.FindWindow(null, title);
                if (dialogWindow != null && Pointer.nativeValue(dialogWindow.getPointer()) != 0L) {
                    break;
                }
            }
            if (dialogWindow != null && Pointer.nativeValue(dialogWindow.getPointer()) != 0L) {
                final WinDef.HWND okButton = findButtonByText(dialogWindow, buttonTexts);
                if (okButton != null && Pointer.nativeValue(okButton.getPointer()) != 0L) {
                    final int BM_CLICK = 0x00F5;
                    User32.INSTANCE.SendMessage(okButton, BM_CLICK, null, null);
                    LogV3.info("Automatically confirmed certificate dialog");
                    return true;
                }
            }
            try {
                Thread.sleep(checkInterval);
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
                return false;
            }
        }
        return false;
    }

    private static WinDef.HWND findButtonByText(final WinDef.HWND parent, final String[] buttonTexts) {
        final WinDef.HWND[] found = { null };
        User32.INSTANCE.EnumChildWindows(parent, new WinUser.WNDENUMPROC() {
            @Override
            public boolean callback(final WinDef.HWND hWnd, final Pointer userData) {
                final char[] className = new char[256];
                User32.INSTANCE.GetClassName(hWnd, className, 256);
                final String classNameStr = Native.toString(className);
                if ("Button".equals(classNameStr)) {
                    final int length = User32.INSTANCE.GetWindowTextLength(hWnd);
                    if (length > 0) {
                        final char[] text = new char[length + 1];
                        User32.INSTANCE.GetWindowText(hWnd, text, length + 1);
                        final String buttonText = Native.toString(text);
                        for (final String searchText : buttonTexts) {
                            if (buttonText.equalsIgnoreCase(searchText)) {
                                found[0] = hWnd;
                                return false;
                            }
                        }
                    }
                }
                return true;
            }
        }, null);
        return found[0];
    }

    private static void installCertificateWithAutoConfirm(final X509Certificate certificate, final WindowsCertUtils.KeyStore target, final String friendlyName) throws Exception {
        final Thread confirmationThread = new Thread(new Runnable() {
            @Override
            public void run() {
                autoConfirmCertificateDialog(15000);
            }
        }, "CertificateDialogAutoConfirm");
        confirmationThread.setDaemon(true);
        confirmationThread.start();
        try {
            WindowsCertUtils.installCertificate(certificate, target, friendlyName);
        } finally {
            try {
                confirmationThread.join(6000);
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    private static boolean removeCertificateWithAutoConfirm(final String thumbprintHex, final WindowsCertUtils.KeyStore target) throws Exception {
        final Thread confirmationThread = new Thread(new Runnable() {
            @Override
            public void run() {
                autoConfirmCertificateDialog(5000);
            }
        }, "CertificateDialogAutoConfirm");
        confirmationThread.setDaemon(true);
        confirmationThread.start();
        try {
            return WindowsCertUtils.removeCertificate(thumbprintHex, target);
        } finally {
            try {
                confirmationThread.join(6000);
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }
}
