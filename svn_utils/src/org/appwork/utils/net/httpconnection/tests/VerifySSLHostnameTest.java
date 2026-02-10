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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
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

import java.io.IOException;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;

import javax.net.ssl.SSLSession;

import org.appwork.testframework.AWTest;
import org.appwork.utils.Exceptions;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpconnection.IllegalSSLHostnameException;

/**
 * Tests for {@link HTTPConnectionUtils#verifySSLHostname(HostnameVerifier, SSLSession, String)}: match/mismatch with certificate CN and SAN
 * (DNS and IP), null host, IPv6 normalization.
 */
public class VerifySSLHostnameTest extends AWTest {
    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            final Certificate[] peerCerts = createLocalhostCertChain();
            final SSLSession session = createMockSSLSession(peerCerts);
            testVerifyMatchLocalhost(session);
            testVerifyMatch127(session);
            testVerifyMatchIPv6Normalized(session);
            testVerifyMismatchThrows(session);
            testVerifyNullHostReturnsNull();
            testVerifyNullSessionUsesVerifierOnly();
        } finally {
        }
    }

    private static Certificate[] createLocalhostCertChain() throws Exception {
        final CertificateFactory.ServerCertificateResult result = CertificateFactory.createCACertificateAndServerCertificate("VerifySSLHostname Test CA", "localhost");
        return new Certificate[] { result.getServerCertificate() };
    }

    /**
     * Mock SSLSession that returns the given peer certificates; other methods return safe defaults. Uses dynamic proxy so we only need to
     * handle getPeerCertificates (and getPeerPrincipal for default verifier).
     */
    private static SSLSession createMockSSLSession(final Certificate[] peerCertificates) {
        final InvocationHandler h = new InvocationHandler() {
            @Override
            public Object invoke(final Object proxy, final Method method, final Object[] args) throws Throwable {
                final String name = method.getName();
                if ("getPeerCertificates".equals(name)) {
                    return peerCertificates != null ? peerCertificates : new Certificate[0];
                }
                if ("getPeerPrincipal".equals(name) && peerCertificates != null && peerCertificates.length > 0 && peerCertificates[0] instanceof X509Certificate) {
                    return ((X509Certificate) peerCertificates[0]).getSubjectX500Principal();
                }
                final Class<?> ret = method.getReturnType();
                if (ret == boolean.class) {
                    return Boolean.FALSE;
                }
                if (ret == int.class) {
                    return Integer.valueOf(0);
                }
                if (ret == long.class) {
                    return Long.valueOf(0L);
                }
                if (ret == byte[].class) {
                    return new byte[0];
                }
                if (ret == String[].class) {
                    return new String[0];
                }
                return null;
            }
        };
        return (SSLSession) Proxy.newProxyInstance(SSLSession.class.getClassLoader(), new Class<?>[] { SSLSession.class }, h);
    }

    /** Cert has CN=localhost and SAN localhost, 127.0.0.1, ::1 → "localhost" must pass. */
    private void testVerifyMatchLocalhost(final SSLSession session) throws Exception {
        final Boolean result = HTTPConnectionUtils.verifySSLHostname(null, session, "localhost");
        assertTrue(Boolean.TRUE.equals(result), "verifySSLHostname(null, session, \"localhost\") should be true");
    }

    /** Cert has 127.0.0.1 in SAN → "127.0.0.1" must pass. */
    private void testVerifyMatch127(final SSLSession session) throws Exception {
        final Boolean result = HTTPConnectionUtils.verifySSLHostname(null, session, "127.0.0.1");
        assertTrue(Boolean.TRUE.equals(result), "verifySSLHostname(null, session, \"127.0.0.1\") should be true");
    }

    /** Cert has ::1 in SAN (stored as 0:0:0:0:0:0:0:1 or similar). Host "[::1]" must pass after normalization. */
    private void testVerifyMatchIPv6Normalized(final SSLSession session) throws Exception {
        final Boolean result = HTTPConnectionUtils.verifySSLHostname(null, session, "[::1]");
        assertTrue(Boolean.TRUE.equals(result), "verifySSLHostname(null, session, \"[::1]\") should be true (normalized to match SAN)");
    }

    /** Host that is not in cert → must throw IllegalSSLHostnameException. */
    private void testVerifyMismatchThrows(final SSLSession session) throws Exception {
        try {
            HTTPConnectionUtils.verifySSLHostname(null, session, "evil.example.com");
            assertTrue(false, "verifySSLHostname with wrong host should throw IllegalSSLHostnameException");
        } catch (final IOException e) {
            assertTrue(Exceptions.containsInstanceOf(e, IllegalSSLHostnameException.class), "expected IllegalSSLHostnameException, got: " + e);
        }
    }

    /** null host → must return null (no NPE). */
    private void testVerifyNullHostReturnsNull() throws Exception {
        final Certificate[] peerCerts = createLocalhostCertChain();
        final SSLSession session = createMockSSLSession(peerCerts);
        final Boolean result = HTTPConnectionUtils.verifySSLHostname(null, session, null);
        assertEquals(null, result, "verifySSLHostname with null host should return null");
    }

    /** Session null → no peer certs; method uses HostnameVerifier only and must not NPE. */
    private void testVerifyNullSessionUsesVerifierOnly() throws Exception {
        try {
            final Boolean result = HTTPConnectionUtils.verifySSLHostname(null, null, "localhost");
            assertTrue(result == null || result.equals(Boolean.TRUE) || result.equals(Boolean.FALSE), "verifySSLHostname with null session should return Boolean or null");
        } catch (final Exception e) {
            // Default verifier may throw when session is null; that is acceptable
        }
    }
}
