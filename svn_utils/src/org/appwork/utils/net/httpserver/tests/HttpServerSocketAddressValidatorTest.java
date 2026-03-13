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
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
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
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpserver.tests;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.remoteapi.ParseException;
import org.appwork.remoteapi.RemoteAPI;
import org.appwork.remoteapi.tests.DummyTestAPIImpl;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpclient.HttpClient;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpserver.DefaultSocketAddressValidator;
import org.appwork.utils.net.httpserver.HttpServer;
import org.appwork.utils.net.httpserver.RejectAllSocketAddressValidator;
import org.appwork.utils.net.httpserver.SocketAddressValidator;

/**
 * Tests HttpServer behaviour with different {@link SocketAddressValidator} configurations: no validator (null),
 * {@link RejectAllSocketAddressValidator}, and {@link DefaultSocketAddressValidator#get()}.
 */
public class HttpServerSocketAddressValidatorTest extends AWTest {
    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        logInfoAnyway("=== HttpServerSocketAddressValidatorTest ===");
        testNoValidatorAcceptsConnections();
        testRejectAllValidatorRejectsConnections();
        testDefaultValidatorSameProcess();
    }

    /**
     * When validator is null, all connections are allowed. Same-process GET must return 200.
     *
     * @throws Exception
     */
    private void testNoValidatorAcceptsConnections() throws Exception {
        logInfoAnyway("Test: no validator (null) -> connections accepted");
        HttpServer server = null;
        try {
            server = createServerWithValidator(null);
            final int port = server.getActualPort();
            final HttpClient client = newHttpClient();
            final HttpClient.RequestContext ctx = client.get("http://127.0.0.1:" + port + "/test/echo?message=hello");
            assertTrue(ctx.getCode() == 200, "With null validator request must succeed (200), got: " + ctx.getCode());
            logInfoAnyway("  OK: request accepted as expected");
        } finally {
            if (server != null) {
                server.shutdown();
            }
        }
    }

    /**
     * When validator is {@link RejectAllSocketAddressValidator}, all connections are rejected (connection closed without response).
     *
     * @throws Exception
     */
    private void testRejectAllValidatorRejectsConnections() throws Exception {
        logInfoAnyway("Test: RejectAllSocketAddressValidator -> connections rejected");
        HttpServer server = null;
        try {
            server = createServerWithValidator(new RejectAllSocketAddressValidator());
            final int port = server.getActualPort();
            final HttpClient client = newHttpClient();
            boolean rejected = false;
            try {
                client.get("http://127.0.0.1:" + port + "/test/echo?message=hello");
            } catch (org.appwork.utils.net.httpclient.HttpClientException e) {
                rejected = true;
            } catch (IOException e) {
                rejected = true;
            }
            assertTrue(rejected, "With RejectAllSocketAddressValidator request must be rejected (exception or connection closed)");
            logInfoAnyway("  OK: request rejected as expected");
        } finally {
            if (server != null) {
                server.shutdown();
            }
        }
    }

    /**
     * With default validator (DefaultSocketAddressValidator.get()): on Windows same-process connection is same user so accepted; on
     * non-Windows the default is RejectAll so connection is rejected.
     *
     * @throws Exception
     */
    private void testDefaultValidatorSameProcess() throws Exception {
        logInfoAnyway("Test: DefaultSocketAddressValidator.get() -> same process");
        HttpServer server = null;
        try {
            server = createServerWithValidator(DefaultSocketAddressValidator.get());
            final int port = server.getActualPort();
            final HttpClient client = newHttpClient();
            final boolean isWindows = org.appwork.utils.os.CrossSystem.isWindows();
            try {
                final HttpClient.RequestContext ctx = client.get("http://127.0.0.1:" + port + "/test/echo?message=hello");
                if (isWindows) {
                    assertTrue(ctx.getCode() == 200, "On Windows same-process request must succeed (200), got: " + ctx.getCode());
                    logInfoAnyway("  OK: Windows same-process request accepted as expected");
                } else {
                    assertTrue(false, "On non-Windows default validator rejects all; expected exception, got: " + ctx.getCode());
                }
            } catch (org.appwork.utils.net.httpclient.HttpClientException e) {
                if (!isWindows) {
                    logInfoAnyway("  OK: non-Windows same-process request rejected (default is RejectAll) as expected");
                } else {
                    throw e;
                }
            } catch (IOException e) {
                if (!isWindows) {
                    logInfoAnyway("  OK: non-Windows same-process request rejected (default is RejectAll) as expected");
                } else {
                    throw e;
                }
            }
        } finally {
            if (server != null) {
                server.shutdown();
            }
        }
    }

    private HttpServer createServerWithValidator(final SocketAddressValidator validator) throws IOException, ParseException {
        LogV3.info("Create HttpServer with validator: " + (validator == null ? "null" : validator.getClass().getSimpleName()));
        if (!"system".equals(System.getProperty("java.net.preferIPv6Addresses"))) {
            HTTPConnectionUtils.isGlobalIPv6Available(5000, 5000, -1);
        }
        final HttpServer server = new HttpServer(0);
        server.setLocalhostOnly(true);
        server.setSocketAddressValidator(validator);
        final RemoteAPI remoteAPI = new RemoteAPI();
        remoteAPI.register(new DummyTestAPIImpl());
        server.registerRequestHandler(remoteAPI);
        server.start();
        return server;
    }

    private HttpClient newHttpClient() {
        final HttpClient client = new HttpClient();
        client.setConnectTimeout((int) TimeUnit.SECONDS.toMillis(5));
        client.setReadTimeout((int) TimeUnit.SECONDS.toMillis(10));
        client.putRequestHeader(HTTPConstants.X_APPWORK, "1");
        return client;
    }
}
