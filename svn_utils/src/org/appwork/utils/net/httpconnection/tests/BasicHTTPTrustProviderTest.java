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
 * === Definition of Commercial Usage ===
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

import java.net.URL;
import java.net.URLEncoder;
import java.util.EnumSet;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.remoteapi.RemoteAPI;
import org.appwork.remoteapi.tests.DummyTestAPIImpl;
import org.appwork.utils.net.BasicHTTP.BasicHTTP;
import org.appwork.utils.net.BasicHTTP.BasicHTTPException;
import org.appwork.utils.net.BasicHTTP.InvalidResponseCode;
import org.appwork.utils.net.httpconnection.HTTPConnection;
import org.appwork.utils.net.httpconnection.RequestMethod;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.trust.CustomTrustProvider;
import org.appwork.utils.net.httpconnection.trust.TrustCurrentJREProvider;
import org.appwork.utils.net.httpconnection.trust.TrustProviderInterface;
import org.appwork.utils.net.httpconnection.trust.TrustUtils;

/**
 * AWTest for BasicHTTP TrustProvider support: getter/setter and that the created HTTP connection receives the TrustProvider.
 */
public class BasicHTTPTrustProviderTest extends SSLTrustProviderTestBase {
    public static void main(final String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        createTestCertificates();
        testTrustProviderGetterSetter();
        testBasicHTTPConnectionReceivesTrustProvider();
        testBasicHTTPRealHttpRequest();
        testBasicHTTPHTTPSFailsWithWrongTrustProvider();
        testBasicHTTPInvalidResponseCode();
        cleanupTempFiles();
        LogV3.info("BasicHTTP TrustProvider tests completed successfully");
    }

    /**
     * Verifies getTrustProvider() / setTrustProvider(): default is default provider (never null), set and get roundtrip, set null resets to default.
     */
    private void testTrustProviderGetterSetter() throws Exception {
        LogV3.info("Test: BasicHTTP TrustProvider getter/setter");
        final BasicHTTP basicHTTP = new BasicHTTP();
        assertTrue(basicHTTP.getTrustProvider() == TrustUtils.getDefaultProvider(), "Default TrustProvider should be default provider (never null)");
        final TrustProviderInterface provider = TrustCurrentJREProvider.getInstance();
        basicHTTP.setTrustProvider(provider);
        assertTrue(basicHTTP.getTrustProvider() == provider, "getTrustProvider should return set provider");
        basicHTTP.setTrustProvider(null);
        assertTrue(basicHTTP.getTrustProvider() == TrustUtils.getDefaultProvider(), "After setTrustProvider(null) should return default provider (never null)");
    }

    /**
     * Verifies that a connection created by BasicHTTP receives the TrustProvider and that an HTTPS request with that provider succeeds.
     */
    private void testBasicHTTPConnectionReceivesTrustProvider() throws Exception {
        if (sslContext == null) {
            logInfoAnyway("Skipping BasicHTTP HTTPS test - SSL context not available");
            return;
        }
        LogV3.info("Test: BasicHTTP connection receives TrustProvider on HTTPS");
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
            final TrustProviderInterface ourProvider = new CustomTrustProvider(caCertificate);
            final BasicHTTP basicHTTP = new BasicHTTP();
            basicHTTP.setTrustProvider(ourProvider);
            basicHTTP.setAllowedResponseCodes(200);
            basicHTTP.getRequestHeader().put(HTTPConstants.X_APPWORK, "1");
            basicHTTP.setConnectTimeout(10000);
            basicHTTP.setReadTimeout(10000);
            final String urlString = "https://localhost:" + serverPort + "/test/echo?message=BasicHTTPTrustProvider";
            try {
                final String page = basicHTTP.getPage(new URL(urlString));
                assertNotNull(page, "getPage should return response body");
                final HTTPConnection connection = basicHTTP.getConnection();
                assertNotNull(connection, "Connection should be set after getPage");
                assertTrue(connection.getTrustProvider() == ourProvider, "Connection must have the TrustProvider set on BasicHTTP");
                final TrustResult trustResult = connection.getTrustResult();
                assertNotNull(trustResult, "TrustResult should be available after HTTPS request");
                assertTrue(trustResult.isTrusted(), "CustomTrustProvider(CA) should trust the server certificate");
            } catch (final BasicHTTPException e) {
                throw new Exception("BasicHTTP getPage failed: " + e.getMessage(), e);
            }
        } finally {
            server.stop();
        }
    }

    /**
     * Real HTTP request via BasicHTTP.getPage against a plain HTTP server (no SSL).
     */
    private void testBasicHTTPRealHttpRequest() throws Exception {
        LogV3.info("Test: BasicHTTP real HTTP request (plain HTTP)");
        final TestHTTPServer server = new TestHTTPServer(0, null);
        server.setLocalhostOnly(true);
        server.setAllowedMethods(EnumSet.of(RequestMethod.GET));
        final RemoteAPI remoteAPI = new RemoteAPI();
        remoteAPI.register(new DummyTestAPIImpl());
        server.registerRequestHandler(remoteAPI);
        server.start();
        final int serverPort = server.getActualPort();
        try {
            final BasicHTTP basicHTTP = new BasicHTTP();
            basicHTTP.setAllowedResponseCodes(200);
            basicHTTP.getRequestHeader().put(HTTPConstants.X_APPWORK, "1");
            basicHTTP.setConnectTimeout(10000);
            basicHTTP.setReadTimeout(10000);
            final String urlString = "http://localhost:" + serverPort + "/test/echo?message=" + URLEncoder.encode("BasicHTTPRealHttp", "UTF-8");
            final String page = basicHTTP.getPage(new URL(urlString));
            assertNotNull(page, "getPage over HTTP should return response body");
            assertTrue(page.contains("BasicHTTPRealHttp") || page.contains("echo"), "Response should contain echo payload");
        } finally {
            server.stop();
        }
    }

    /**
     * Negative test: HTTPS with TrustProvider that does not trust the server certificate must fail.
     */
    private void testBasicHTTPHTTPSFailsWithWrongTrustProvider() throws Exception {
        if (sslContext == null) {
            logInfoAnyway("Skipping BasicHTTP HTTPS negative test - SSL context not available");
            return;
        }
        LogV3.info("Test: BasicHTTP HTTPS fails with wrong TrustProvider");
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
            final BasicHTTP basicHTTP = new BasicHTTP();
            basicHTTP.setTrustProvider(TrustCurrentJREProvider.getInstance());
            basicHTTP.setAllowedResponseCodes(200);
            basicHTTP.getRequestHeader().put(HTTPConstants.X_APPWORK, "1");
            basicHTTP.setConnectTimeout(5000);
            basicHTTP.setReadTimeout(5000);
            final String urlString = "https://localhost:" + serverPort + "/test/echo?message=shouldFail";
            try {
                basicHTTP.getPage(new URL(urlString));
                assertTrue(false, "getPage over HTTPS with wrong TrustProvider should throw");
            } catch (final BasicHTTPException e) {
                assertNotNull(e.getCause(), "BasicHTTPException should have a cause (SSL/cert failure)");
                LogV3.info("Expected failure with wrong TrustProvider: " + e.getCause().getMessage());
            }
        } finally {
            server.stop();
        }
    }

    /**
     * Negative test: BasicHTTP must throw when response code is not in allowed list (e.g. 404).
     */
    private void testBasicHTTPInvalidResponseCode() throws Exception {
        LogV3.info("Test: BasicHTTP InvalidResponseCode when response not allowed");
        final TestHTTPServer server = new TestHTTPServer(0, null);
        server.setLocalhostOnly(true);
        server.setAllowedMethods(EnumSet.of(RequestMethod.GET));
        final RemoteAPI remoteAPI = new RemoteAPI();
        remoteAPI.register(new DummyTestAPIImpl());
        server.registerRequestHandler(remoteAPI);
        server.start();
        final int serverPort = server.getActualPort();
        try {
            final BasicHTTP basicHTTP = new BasicHTTP();
            basicHTTP.setAllowedResponseCodes(200);
            basicHTTP.getRequestHeader().put(HTTPConstants.X_APPWORK, "1");
            basicHTTP.setConnectTimeout(5000);
            basicHTTP.setReadTimeout(5000);
            final String urlString = "http://localhost:" + serverPort + "/test/nonexistent";
            try {
                basicHTTP.getPage(new URL(urlString));
                assertTrue(false, "getPage with disallowed response code should throw");
            } catch (final BasicHTTPException e) {
                final Throwable cause = e.getCause();
                assertTrue(cause instanceof InvalidResponseCode, "Cause should be InvalidResponseCode, got: " + (cause != null ? cause.getClass().getSimpleName() : "null"));
                LogV3.info("Expected InvalidResponseCode: " + cause.getMessage());
            }
        } finally {
            server.stop();
        }
    }
}
