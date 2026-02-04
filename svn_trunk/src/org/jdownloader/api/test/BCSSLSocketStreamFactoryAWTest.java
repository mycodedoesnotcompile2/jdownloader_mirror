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
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.jdownloader.api.test;

import java.io.InputStream;
import java.net.URL;

import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpconnection.HTTPConnection;
import org.appwork.utils.net.httpconnection.HTTPConnectionImpl;
import org.appwork.utils.net.httpconnection.NativeHTTPConnectionImpl;
import org.appwork.utils.net.httpconnection.SSLSocketStreamFactory;
import org.appwork.utils.net.httpconnection.SSLSocketStreamOptions;
import org.appwork.utils.net.httpconnection.TrustResult;
import org.appwork.utils.net.httpconnection.trust.TrustAllProvider;
import org.jdownloader.net.AutoBCSSLSocketStreamFactory;
import org.jdownloader.net.BCSSLSocketStreamFactory;

/**
 * AWTest that explicitly tests the JDownloader BouncyCastle SSL classes (BCSSLSocketStreamFactory and AutoBCSSLSocketStreamFactory) with
 * TrustProvider and TrustResult forwarding, equivalent to the AppWorkUtils JavaSSLSocketStreamFactory behaviour.
 */
public class BCSSLSocketStreamFactoryAWTest extends AWTest {
    private static final String HTTPS_TEST_URL = "https://jdownloader.org";

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        testBCSSLSocketStreamFactoryTrustResult();
        testAutoBCSSLSocketStreamFactoryTrustResultWithBC();
        testNativeHTTPConnectionImplWithAutoBCTrustResult();
        LogV3.info("BCSSLSocketStreamFactoryAWTest completed successfully");
    }

    /**
     * Explicitly tests BCSSLSocketStreamFactory: HTTPConnectionImpl with factory forced to BCSSLSocketStreamFactory, TrustAllProvider,
     * connect to HTTPS URL, assert TrustResult is set and trusted.
     */
    public void testBCSSLSocketStreamFactoryTrustResult() throws Exception {
        LogV3.info("testBCSSLSocketStreamFactoryTrustResult: connecting via BCSSLSocketStreamFactory to " + HTTPS_TEST_URL);
        final SSLSocketStreamFactory bcFactory = new BCSSLSocketStreamFactory();
        final HTTPConnection conn = new HTTPConnectionImpl(new URL(HTTPS_TEST_URL), null) {
            @Override
            protected SSLSocketStreamFactory getSSLSocketStreamFactory(final SSLSocketStreamOptions sslSocketStreamOptions) {
                return bcFactory;
            }
        };
        conn.setTrustProvider(TrustAllProvider.getInstance());
        conn.setConnectTimeout(10000);
        conn.setReadTimeout(10000);
        conn.connect();
        try {
            final TrustResult tr = conn.getTrustResult();
            assertTrue(tr != null, "TrustResult must be set when using BCSSLSocketStreamFactory");
            assertTrue(tr.isTrusted(), "TrustResult must be trusted when using TrustAllProvider and BCSSLSocketStreamFactory");
            try (InputStream is = conn.getInputStream()) {
                assertTrue(is != null, "InputStream must be available");
            }
        } finally {
            conn.disconnect();
        }
    }

    /**
     * Explicitly tests AutoBCSSLSocketStreamFactory with BC path: force BC via options so that AutoBC uses BouncyCastle, then assert
     * TrustResult is set and trusted.
     */
    public void testAutoBCSSLSocketStreamFactoryTrustResultWithBC() throws Exception {
        LogV3.info("testAutoBCSSLSocketStreamFactoryTrustResultWithBC: connecting via AutoBCSSLSocketStreamFactory (BC path) to " + HTTPS_TEST_URL);
        final AutoBCSSLSocketStreamFactory autoBCFactory = new AutoBCSSLSocketStreamFactory();
        final HTTPConnection conn = new HTTPConnectionImpl(new URL(HTTPS_TEST_URL), null) {
            @Override
            protected SSLSocketStreamFactory getSSLSocketStreamFactory(final SSLSocketStreamOptions sslSocketStreamOptions) {
                return autoBCFactory;
            }

            @Override
            protected SSLSocketStreamOptions getSSLSocketStreamOptions(final String host, final int port) {
                final SSLSocketStreamOptions options = super.getSSLSocketStreamOptions(host, port);
                if (options != null) {
                    options.getCustomFactorySettings().add("BC_Factory");
                }
                return options;
            }
        };
        conn.setTrustProvider(TrustAllProvider.getInstance());
        conn.setConnectTimeout(10000);
        conn.setReadTimeout(10000);
        conn.connect();
        try {
            final TrustResult tr = conn.getTrustResult();
            assertTrue(tr != null, "TrustResult must be set when using AutoBCSSLSocketStreamFactory (BC path)");
            assertTrue(tr.isTrusted(), "TrustResult must be trusted when using TrustAllProvider and AutoBC (BC path)");
            try (InputStream is = conn.getInputStream()) {
                assertTrue(is != null, "InputStream must be available");
            }
        } finally {
            conn.disconnect();
        }
    }

    /**
     * Tests NativeHTTPConnectionImpl with default SSLSocketStreamFactory set to AutoBCSSLSocketStreamFactory: trustResult must be set after
     * connect (native path uses TrustCallback in getSSLSocketFactory).
     */
    public void testNativeHTTPConnectionImplWithAutoBCTrustResult() throws Exception {
        LogV3.info("testNativeHTTPConnectionImplWithAutoBCTrustResult: NativeHTTPConnectionImpl with AutoBCSSLSocketStreamFactory to " + HTTPS_TEST_URL);
        final SSLSocketStreamFactory previousDefault = NativeHTTPConnectionImpl.getDefaultSSLSocketStreamFactory();
        try {
            NativeHTTPConnectionImpl.setDefaultSSLSocketStreamFactory(new AutoBCSSLSocketStreamFactory());
            final HTTPConnection conn = new NativeHTTPConnectionImpl(new URL(HTTPS_TEST_URL), null);
            conn.setTrustProvider(TrustAllProvider.getInstance());
            conn.setConnectTimeout(10000);
            conn.setReadTimeout(10000);
            conn.connect();
            try {
                final TrustResult tr = conn.getTrustResult();
                assertTrue(tr != null, "TrustResult must be set when using NativeHTTPConnectionImpl with AutoBCSSLSocketStreamFactory");
                assertTrue(tr.isTrusted(), "TrustResult must be trusted when using TrustAllProvider");
                try (InputStream is = conn.getInputStream()) {
                    assertTrue(is != null, "InputStream must be available");
                }
            } finally {
                conn.disconnect();
            }
        } finally {
            NativeHTTPConnectionImpl.setDefaultSSLSocketStreamFactory(previousDefault);
        }
    }
}
