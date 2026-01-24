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
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
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

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.SocketAddress;
import java.util.List;

import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpserver.HttpServer;

/**
 * Tests for HTTP server localhost binding functionality.
 *
 * <p>
 * This test class verifies that:
 * </p>
 * <ul>
 * <li>Server is bound to localhost only</li>
 * <li>No non-loopback addresses are accessible</li>
 * </ul>
 *
 * @author AppWork
 */
public class HttpServerLocalhostBindingTest extends HttpServerTestBase {

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.setupServer();
            this.testIsLocalhostOnlyFlag();
            this.testGetLocalAddressesReturnsOnlyLoopback();
            this.testServerSocketBoundToLoopback();
            this.testNoNonLoopbackAddressesBound();
        } finally {
            this.teardownServer();
        }
    }

    /**
     * Test 1: Check isLocalhostOnly() flag returns true
     */
    private void testIsLocalhostOnlyFlag() throws Exception {
        LogV3.info("Test 1: isLocalhostOnly() flag");

        final boolean isLocalhostOnly = httpServer.isLocalhostOnly();
        assertTrue(isLocalhostOnly, "Server isLocalhostOnly() should return true, was: " + isLocalhostOnly);
        LogV3.info("Test 1 passed: isLocalhostOnly() = " + isLocalhostOnly);
    }

    /**
     * Test 2: Check getLocalAddresses() returns only loopback addresses
     */
    private void testGetLocalAddressesReturnsOnlyLoopback() throws Exception {
        LogV3.info("Test 2: getLocalAddresses() returns only loopback addresses");
        final List<SocketAddress> localAddresses = httpServer.getLocalAddresses();
        assertTrue(localAddresses != null && localAddresses.size() > 0, "getLocalAddresses() should return at least one address");

        for (final SocketAddress socketAddress : localAddresses) {
            assertTrue(socketAddress instanceof InetSocketAddress, "SocketAddress should be InetSocketAddress");
            final InetSocketAddress inetSocketAddress = (InetSocketAddress) socketAddress;
            final InetAddress address = inetSocketAddress.getAddress();
            assertTrue(address != null, "InetAddress should not be null");
            assertTrue(address.isLoopbackAddress(), "All bound addresses should be loopback addresses, but found: " + address.getHostAddress());
            LogV3.info("Test 2 passed: Address " + address.getHostAddress() + ":" + inetSocketAddress.getPort() + " is loopback");
        }
    }

    /**
     * Test 3: Directly inspect ServerSocket instances to verify they are bound to loopback addresses only
     */
    private void testServerSocketBoundToLoopback() throws Exception {
        LogV3.info("Test 3: ServerSocket instances bound to loopback addresses");
        // Use reflection to access controlSockets field for low-level verification
        try {
            final java.lang.reflect.Field controlSocketsField = HttpServer.class.getDeclaredField("controlSockets");
            controlSocketsField.setAccessible(true);
            @SuppressWarnings("unchecked")
            final java.util.concurrent.atomic.AtomicReference<List<ServerSocket>> controlSocketsRef = (java.util.concurrent.atomic.AtomicReference<List<ServerSocket>>) controlSocketsField.get(httpServer);
            final List<ServerSocket> controlSockets = controlSocketsRef.get();

            assertTrue(controlSockets != null && controlSockets.size() > 0, "controlSockets should contain at least one ServerSocket");

            for (final ServerSocket serverSocket : controlSockets) {
                final SocketAddress localSocketAddress = serverSocket.getLocalSocketAddress();
                assertTrue(localSocketAddress instanceof InetSocketAddress, "ServerSocket local address should be InetSocketAddress");
                final InetSocketAddress inetSocketAddress = (InetSocketAddress) localSocketAddress;
                final InetAddress boundAddress = inetSocketAddress.getAddress();
                assertTrue(boundAddress != null, "Bound InetAddress should not be null");
                assertTrue(boundAddress.isLoopbackAddress(), "ServerSocket should be bound to loopback address, but found: " + boundAddress.getHostAddress());

                // Verify it's specifically 127.0.0.1 or ::1 (not 0.0.0.0 or any other address)
                final String hostAddress = boundAddress.getHostAddress();
                assertTrue(hostAddress.equals("127.0.0.1") || hostAddress.equals("::1") || hostAddress.equals("0:0:0:0:0:0:0:1"), "ServerSocket should be bound to 127.0.0.1 or ::1, but found: " + hostAddress);
                LogV3.info("Test 3 passed: ServerSocket bound to " + hostAddress + ":" + inetSocketAddress.getPort());
            }
        } catch (final NoSuchFieldException e) {
            throw new Exception("Failed to access controlSockets field for low-level verification: " + e.getMessage(), e);
        } catch (final IllegalAccessException e) {
            throw new Exception("Failed to access controlSockets field: " + e.getMessage(), e);
        }
    }

    /**
     * Test 4: Verify no non-loopback addresses are bound
     */
    private void testNoNonLoopbackAddressesBound() throws Exception {
        LogV3.info("Test 4: No non-loopback addresses are bound");
        // Get all network interfaces and check that server is NOT bound to any non-loopback address
        try {
            final java.util.Enumeration<java.net.NetworkInterface> networkInterfaces = java.net.NetworkInterface.getNetworkInterfaces();
            while (networkInterfaces.hasMoreElements()) {
                final java.net.NetworkInterface networkInterface = networkInterfaces.nextElement();
                if (networkInterface == null || !networkInterface.isUp() || networkInterface.isLoopback()) {
                    continue;
                }
                final java.util.Enumeration<java.net.InetAddress> inetAddresses = networkInterface.getInetAddresses();
                while (inetAddresses.hasMoreElements()) {
                    final java.net.InetAddress inetAddress = inetAddresses.nextElement();
                    if (inetAddress != null && !inetAddress.isLoopbackAddress()) {
                        // Try to connect to server using non-loopback address - should fail
                        try {
                            final java.net.Socket testSocket = new java.net.Socket();
                            testSocket.setSoTimeout(1000);
                            testSocket.connect(new java.net.InetSocketAddress(inetAddress, this.serverPort), 1000);
                            testSocket.close();
                            // If we can connect, server is accessible from non-loopback - this is a security issue!
                            throw new Exception("SECURITY ISSUE: Server is accessible from non-loopback address " + inetAddress.getHostAddress() + ":" + this.serverPort);
                        } catch (final java.net.ConnectException e) {
                            // Expected - connection should fail
                            LogV3.info("Test 4 passed: Server correctly rejects connection from non-loopback address " + inetAddress.getHostAddress());
                        } catch (final java.net.SocketTimeoutException e) {
                            // Also acceptable - connection timeout
                            LogV3.info("Test 4 passed: Server correctly times out connection from non-loopback address " + inetAddress.getHostAddress());
                        } catch (final java.io.IOException e) {
                            // Any other IOException is acceptable (connection refused, etc.)
                            LogV3.info("Test 4 passed: Server correctly rejects connection from non-loopback address " + inetAddress.getHostAddress() + " (" + e.getClass().getSimpleName() + ")");
                        }
                    }
                }
            }
        } catch (final java.net.SocketException e) {
            LogV3.warning("Could not enumerate network interfaces for Test 4: " + e.getMessage());
            // This is not a failure - some systems may not allow enumeration
        }
        LogV3.info("Test 4 completed: No non-loopback addresses are accessible");
    }
}
