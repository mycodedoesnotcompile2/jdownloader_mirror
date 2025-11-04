/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
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
 *     Contact AppWork for details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection.proxy.tests;

import org.appwork.testframework.AWTest;
import org.appwork.testframework.TestDependency;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.proxy.WindowsProxyUtils;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.NotSupportedException;

/**
 * @author thomas
 * @date 2024
 *
 */
@TestDependency({ "org.appwork.utils.net.httpconnection.proxy.WindowsProxyUtils" })
public class WindowsProxyUtilsTest extends AWTest {
    
    public static void main(String[] args) {
        run();
    }

    @Override
    public void runTest() throws Exception {
        // Test NotSupportedException on non-Windows systems
        if (!CrossSystem.isWindows()) {
            System.out.println("WindowsProxyUtilsTest: Testing NotSupportedException on non-Windows system");
            new AWTest.AssertAnException<NotSupportedException>() {
                @Override
                protected void run() throws NotSupportedException {
                    WindowsProxyUtils.getWindowsPACScriptURL();
                }
            };
            new AWTest.AssertAnException<NotSupportedException>() {
                @Override
                protected void run() throws NotSupportedException {
                    WindowsProxyUtils.isWindowsPACScriptConfigured();
                }
            };
            new AWTest.AssertAnException<NotSupportedException>() {
                @Override
                protected void run() throws NotSupportedException {
                    WindowsProxyUtils.getWindowsRegistryProxies();
                }
            };
            System.out.println("WindowsProxyUtilsTest: All NotSupportedException tests passed");
            return;
        }
        
        // Test with JNA (if available) and without JNA, compare results
        final String originalOverride = System.getProperty("JNAHelper.isJNAAvailable.override");
        final boolean jnaAvailable = org.appwork.JNAHelper.isJNAAvailable();
        
        // Note: We can only test both variants if JNA is actually available.
        // If JNA is not available, we can only test the reg.exe fallback.
        if (!jnaAvailable) {
            System.out.println("WindowsProxyUtilsTest: JNA not available, testing only reg.exe fallback");
        }
        
        try {
            // Test 1: With JNA (if available)
            String pacUrlWithJNA = null;
            boolean isConfiguredWithJNA = false;
            java.util.List<HTTPProxy> proxiesWithJNA = null;
            
            if (jnaAvailable) {
                System.out.println("WindowsProxyUtilsTest: Testing with JNA");
            
                pacUrlWithJNA = WindowsProxyUtils.getWindowsPACScriptURL();
                isConfiguredWithJNA = WindowsProxyUtils.isWindowsPACScriptConfigured();
                proxiesWithJNA = WindowsProxyUtils.getWindowsRegistryProxies();
                
                if (pacUrlWithJNA != null) {
                    System.out.println("  PAC Script URL (JNA): " + pacUrlWithJNA);
                } else {
                    System.out.println("  PAC Script URL (JNA): null");
                }
                System.out.println("  Is Configured (JNA): " + isConfiguredWithJNA);
                System.out.println("  Proxies count (JNA): " + proxiesWithJNA.size());
            } else {
                // If JNA is not available, use reg.exe results as baseline
                System.out.println("WindowsProxyUtilsTest: JNA not available, using reg.exe as baseline");
            }
            
            // Test 2: Without JNA (force reg.exe)
            System.out.println("WindowsProxyUtilsTest: Testing without JNA (reg.exe fallback)");
            System.setProperty("JNAHelper.isJNAAvailable.override", "false");
            
            try {
                final String pacUrlWithoutJNA = WindowsProxyUtils.getWindowsPACScriptURL();
                final boolean isConfiguredWithoutJNA = WindowsProxyUtils.isWindowsPACScriptConfigured();
                final java.util.List<HTTPProxy> proxiesWithoutJNA = WindowsProxyUtils.getWindowsRegistryProxies();
                
                if (pacUrlWithoutJNA != null) {
                    System.out.println("  PAC Script URL (reg.exe): " + pacUrlWithoutJNA);
                } else {
                    System.out.println("  PAC Script URL (reg.exe): null");
                }
                System.out.println("  Is Configured (reg.exe): " + isConfiguredWithoutJNA);
                System.out.println("  Proxies count (reg.exe): " + proxiesWithoutJNA.size());
                
                // Validate proxy settings
                // getWindowsRegistryProxies() should not return null
                assertNotNull(proxiesWithoutJNA);
                for (final HTTPProxy proxy : proxiesWithoutJNA) {
                    assertNotNull(proxy);
                    assertNotNull(proxy.getHost());
                    assertTrue(!proxy.getHost().isEmpty(), "Proxy host should not be empty");
                    assertTrue(proxy.getPort() > 0, "Proxy port should be > 0");
                    assertTrue(proxy.getPort() <= 65535, "Proxy port should be <= 65535");
                    assertNotNull(proxy.getType());
                    
                    System.out.println("  Proxy: " + proxy.getType() + "://" + proxy.getHost() + ":" + proxy.getPort());
                }
                
                // Compare results if JNA was available - they must be the same
                if (jnaAvailable) {
                    // PAC URL should be the same (JNA vs reg.exe)
                    assertEquals(pacUrlWithJNA, pacUrlWithoutJNA);
                    // isConfigured should be the same (JNA vs reg.exe)
                    assertEquals(isConfiguredWithJNA, isConfiguredWithoutJNA);
                    
                    // Compare proxy lists
                    // Proxy count should be the same (JNA vs reg.exe)
                    assertEquals(proxiesWithJNA.size(), proxiesWithoutJNA.size());
                    
                    // Compare each proxy
                    for (int i = 0; i < proxiesWithJNA.size(); i++) {
                        final HTTPProxy proxyJNA = proxiesWithJNA.get(i);
                        final HTTPProxy proxyRegExe = proxiesWithoutJNA.get(i);
                        
                        // Proxy host should be the same at index i
                        assertEquals(proxyJNA.getHost(), proxyRegExe.getHost());
                        // Proxy port should be the same at index i
                        assertEquals(proxyJNA.getPort(), proxyRegExe.getPort());
                        // Proxy type should be the same at index i
                        assertEquals(proxyJNA.getType(), proxyRegExe.getType());
                    }
                    
                    System.out.println("WindowsProxyUtilsTest: All tests passed - JNA and reg.exe produce identical results");
                } else {
                    System.out.println("WindowsProxyUtilsTest: All tests passed - reg.exe fallback works correctly");
                }
                
            } finally {
                // Reset override
                if (originalOverride == null) {
                    System.clearProperty("JNAHelper.isJNAAvailable.override");
                } else {
                    System.setProperty("JNAHelper.isJNAAvailable.override", originalOverride);
                }
            }
            
        } finally {
            // Make sure to restore original override
            if (originalOverride == null) {
                System.clearProperty("JNAHelper.isJNAAvailable.override");
            } else {
                System.setProperty("JNAHelper.isJNAAvailable.override", originalOverride);
            }
        }
    }
}

