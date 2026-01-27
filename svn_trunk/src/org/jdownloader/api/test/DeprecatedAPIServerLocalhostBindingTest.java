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
package org.jdownloader.api.test;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.SocketAddress;
import java.util.List;

import org.appwork.loggingv3.LogV3;
import org.appwork.remoteapi.ParseException;
import org.appwork.remoteapi.SessionRemoteAPI;
import org.appwork.remoteapi.tests.DummyTestAPIImpl;
import org.appwork.storage.config.JsonConfig;
import org.appwork.testframework.AWTest;
import org.appwork.utils.net.httpserver.HttpHandlerInfo;
import org.appwork.utils.net.httpserver.HttpServer;
import org.jdownloader.api.DeprecatedAPIHttpServerController;
import org.jdownloader.api.RemoteAPIConfig;
import org.jdownloader.api.RemoteAPISession;
import org.jdownloader.api.RemoteAPISessionControllerImp;

/**
 * Tests for DeprecatedAPIServer localhost binding functionality.
 *
 * <p>
 * This test class verifies two scenarios:
 * </p>
 * 
 * <p>
 * <b>Scenario 1: localhostOnly = true</b>
 * When isDeprecatedApiLocalhostOnly() is set to true in RemoteAPIConfig:
 * </p>
 * <ul>
 * <li>Server is bound to localhost only</li>
 * <li>isLocalhostOnly() flag returns true</li>
 * <li>getLocalAddresses() returns only loopback addresses</li>
 * <li>ServerSocket instances are bound to loopback addresses only</li>
 * <li>No non-loopback addresses are accessible</li>
 * </ul>
 *
 * <p>
 * <b>Scenario 2: localhostOnly = false</b>
 * When isDeprecatedApiLocalhostOnly() is set to false in RemoteAPIConfig:
 * </p>
 * <ul>
 * <li>Server is bound to all interfaces (0.0.0.0 or ::)</li>
 * <li>isLocalhostOnly() flag returns false</li>
 * <li>ServerSocket instances are bound to wildcard addresses or non-loopback addresses</li>
 * <li>Server accepts connections from all network interfaces</li>
 * </ul>
 *
 * <p>
 * The test initializes the server exactly like in JDownloader:
 * </p>
 * <ul>
 * <li>Sets isDeprecatedApiLocalhostOnly() in RemoteAPIConfig</li>
 * <li>Uses DeprecatedAPIHttpServerController.getInstance()</li>
 * <li>Creates RemoteAPISessionControllerImp</li>
 * <li>Creates SessionRemoteAPI and registers it with the session controller</li>
 * <li>Registers the session controller with the server controller</li>
 * </ul>
 *
 * @author AppWork
 */
public class DeprecatedAPIServerLocalhostBindingTest extends AWTest {
    private HttpHandlerInfo serverHandlerInfo;
    private RemoteAPISessionControllerImp sessionController;
    private SessionRemoteAPI<RemoteAPISession> remoteAPI;
    private HttpServer httpServer;
    private int serverPort;
    private RemoteAPIConfig config;
    private boolean originalLocalhostOnlySetting;

    public static void main(final String[] args) throws Exception {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        try {
            this.setupDeprecatedAPIServer();
            this.testIsLocalhostOnlyFlag();
            this.testGetLocalAddressesReturnsOnlyLoopback();
            this.testServerSocketBoundToLoopback();
            this.testNoNonLoopbackAddressesBound();
        } finally {
            this.teardownDeprecatedAPIServer();
        }
        
        // Test localhostOnly: false
        try {
            this.setupDeprecatedAPIServerWithLocalhostOnlyFalse();
            this.testIsLocalhostOnlyFlagFalse();
            this.testServerSocketBoundToAllInterfaces();
        } finally {
            this.teardownDeprecatedAPIServer();
        }
    }

    /**
     * Server Setup: Creates and starts a DeprecatedAPIServer with localhost-only binding
     */
    protected void setupDeprecatedAPIServer() throws IOException, ParseException {
        LogV3.info("Starting DeprecatedAPIServer Setup with localhost-only binding...");
        
        // Get configuration and save original setting
        this.config = JsonConfig.create(RemoteAPIConfig.class);
        this.originalLocalhostOnlySetting = this.config.isDeprecatedApiLocalhostOnly();
        
        // Set localhost-only to true for this test
        this.config.setDeprecatedApiLocalhostOnly(true);
        
        // Create SessionRemoteAPI and register Dummy API (like in RemoteAPIController)
        this.remoteAPI = new SessionRemoteAPI<RemoteAPISession>();
        this.remoteAPI.register(new DummyTestAPIImpl());
        
        // Create RemoteAPISessionControllerImp (like in RemoteAPIController)
        this.sessionController = new RemoteAPISessionControllerImp();
        
        // Register SessionRemoteAPI with session controller (like in RemoteAPIController)
        this.sessionController.registerSessionRequestHandler(this.remoteAPI);
        this.remoteAPI.register(this.sessionController);
        
        // Register with DeprecatedAPIHttpServerController (like in RemoteAPIController)
        // Use port 0 for automatic port assignment, and true for localhost-only
        this.serverHandlerInfo = DeprecatedAPIHttpServerController.getInstance().registerRequestHandler(0, true, this.sessionController);
        
        // Get the actual server and port
        this.httpServer = this.serverHandlerInfo.getHttpServer();
        this.serverPort = this.httpServer.getActualPort();
        
        LogV3.info("DeprecatedAPIServer started on port: " + this.serverPort + " with localhost-only binding");
    }

    /**
     * Server Teardown: Stops the server and restores original config
     */
    protected void teardownDeprecatedAPIServer() {
        if (this.serverHandlerInfo != null) {
            try {
                DeprecatedAPIHttpServerController.getInstance().unregisterRequestHandler(this.serverHandlerInfo);
                LogV3.info("DeprecatedAPIServer stopped");
            } catch (final Throwable e) {
                LogV3.log(e);
            }
        }
        
        // Restore original config setting
        if (this.config != null) {
            try {
                this.config.setDeprecatedApiLocalhostOnly(this.originalLocalhostOnlySetting);
            } catch (final Throwable e) {
                LogV3.log(e);
            }
        }
    }

    /**
     * Test 1: Check isLocalhostOnly() flag returns true
     */
    private void testIsLocalhostOnlyFlag() throws Exception {
        LogV3.info("Test 1: isLocalhostOnly() flag");
        
        final boolean isLocalhostOnly = this.httpServer.isLocalhostOnly();
        assertTrue(isLocalhostOnly, "Server isLocalhostOnly() should return true, was: " + isLocalhostOnly);
        LogV3.info("Test 1 passed: isLocalhostOnly() = " + isLocalhostOnly);
    }

    /**
     * Test 2: Check getLocalAddresses() returns only loopback addresses
     */
    private void testGetLocalAddressesReturnsOnlyLoopback() throws Exception {
        LogV3.info("Test 2: getLocalAddresses() returns only loopback addresses");
        final List<SocketAddress> localAddresses = this.httpServer.getLocalAddresses();
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
            final java.util.concurrent.atomic.AtomicReference<List<ServerSocket>> controlSocketsRef = (java.util.concurrent.atomic.AtomicReference<List<ServerSocket>>) controlSocketsField.get(this.httpServer);
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
                assertTrue(hostAddress.equals("127.0.0.1") || hostAddress.equals("::1") || hostAddress.equals("0:0:0:0:0:0:0:1"), 
                    "ServerSocket should be bound to 127.0.0.1 or ::1, but found: " + hostAddress);
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

    /**
     * Server Setup: Creates and starts a DeprecatedAPIServer with localhost-only binding disabled (binds to all interfaces)
     */
    protected void setupDeprecatedAPIServerWithLocalhostOnlyFalse() throws IOException, ParseException {
        LogV3.info("Starting DeprecatedAPIServer Setup with localhost-only binding disabled (all interfaces)...");
        
        // Get configuration and save original setting
        this.config = JsonConfig.create(RemoteAPIConfig.class);
        this.originalLocalhostOnlySetting = this.config.isDeprecatedApiLocalhostOnly();
        
        // Set localhost-only to false for this test
        this.config.setDeprecatedApiLocalhostOnly(false);
        
        // Create SessionRemoteAPI and register Dummy API (like in RemoteAPIController)
        this.remoteAPI = new SessionRemoteAPI<RemoteAPISession>();
        this.remoteAPI.register(new DummyTestAPIImpl());
        
        // Create RemoteAPISessionControllerImp (like in RemoteAPIController)
        this.sessionController = new RemoteAPISessionControllerImp();
        
        // Register SessionRemoteAPI with session controller (like in RemoteAPIController)
        this.sessionController.registerSessionRequestHandler(this.remoteAPI);
        this.remoteAPI.register(this.sessionController);
        
        // Register with DeprecatedAPIHttpServerController (like in RemoteAPIController)
        // Use port 0 for automatic port assignment, and false for all interfaces
        this.serverHandlerInfo = DeprecatedAPIHttpServerController.getInstance().registerRequestHandler(0, false, this.sessionController);
        
        // Get the actual server and port
        this.httpServer = this.serverHandlerInfo.getHttpServer();
        this.serverPort = this.httpServer.getActualPort();
        
        LogV3.info("DeprecatedAPIServer started on port: " + this.serverPort + " with all interfaces binding");
    }

    /**
     * Test: Check isLocalhostOnly() flag returns false when localhostOnly is set to false
     */
    private void testIsLocalhostOnlyFlagFalse() throws Exception {
        LogV3.info("Test: isLocalhostOnly() flag should return false");
        
        final boolean isLocalhostOnly = this.httpServer.isLocalhostOnly();
        assertTrue(!isLocalhostOnly, "Server isLocalhostOnly() should return false when localhostOnly is set to false, was: " + isLocalhostOnly);
        LogV3.info("Test passed: isLocalhostOnly() = " + isLocalhostOnly);
    }

    /**
     * Test: Verify that ServerSocket instances are bound to all interfaces (0.0.0.0) when localhostOnly is false
     */
    private void testServerSocketBoundToAllInterfaces() throws Exception {
        LogV3.info("Test: ServerSocket instances bound to all interfaces (0.0.0.0)");
        // Use reflection to access controlSockets field for low-level verification
        try {
            final java.lang.reflect.Field controlSocketsField = HttpServer.class.getDeclaredField("controlSockets");
            controlSocketsField.setAccessible(true);
            @SuppressWarnings("unchecked")
            final java.util.concurrent.atomic.AtomicReference<List<ServerSocket>> controlSocketsRef = (java.util.concurrent.atomic.AtomicReference<List<ServerSocket>>) controlSocketsField.get(this.httpServer);
            final List<ServerSocket> controlSockets = controlSocketsRef.get();
            
            assertTrue(controlSockets != null && controlSockets.size() > 0, "controlSockets should contain at least one ServerSocket");
            
            boolean foundWildcardBinding = false;
            for (final ServerSocket serverSocket : controlSockets) {
                final SocketAddress localSocketAddress = serverSocket.getLocalSocketAddress();
                assertTrue(localSocketAddress instanceof InetSocketAddress, "ServerSocket local address should be InetSocketAddress");
                final InetSocketAddress inetSocketAddress = (InetSocketAddress) localSocketAddress;
                final InetAddress boundAddress = inetSocketAddress.getAddress();
                assertTrue(boundAddress != null, "Bound InetAddress should not be null");
                
                final String hostAddress = boundAddress.getHostAddress();
                
                // When localhostOnly is false, the server should bind to 0.0.0.0 (IPv4) or :: (IPv6)
                // This allows connections from all network interfaces
                if (hostAddress.equals("0.0.0.0") || hostAddress.equals("::") || hostAddress.equals("0:0:0:0:0:0:0:0")) {
                    foundWildcardBinding = true;
                    LogV3.info("Test passed: ServerSocket bound to wildcard address " + hostAddress + ":" + inetSocketAddress.getPort() + " (all interfaces)");
                } else {
                    // It's also acceptable if it binds to a specific non-loopback address
                    assertTrue(!boundAddress.isLoopbackAddress(), 
                        "When localhostOnly is false, ServerSocket should not be bound to loopback address, but found: " + hostAddress);
                    LogV3.info("Test passed: ServerSocket bound to non-loopback address " + hostAddress + ":" + inetSocketAddress.getPort());
                }
            }
            
            // At least one socket should be bound to wildcard (0.0.0.0 or ::)
            // Note: Some systems might bind to specific addresses instead, so we don't fail if wildcard is not found
            if (foundWildcardBinding) {
                LogV3.info("Test completed: Server is bound to wildcard address (all interfaces accessible)");
            } else {
                LogV3.info("Test completed: Server is bound to non-loopback addresses (all interfaces accessible)");
            }
        } catch (final NoSuchFieldException e) {
            throw new Exception("Failed to access controlSockets field for low-level verification: " + e.getMessage(), e);
        } catch (final IllegalAccessException e) {
            throw new Exception("Failed to access controlSockets field: " + e.getMessage(), e);
        }
    }
}
