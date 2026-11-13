/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.utils.os.windows.webview2.tests;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.appwork.testframework.AWTest;
import org.appwork.utils.os.windows.webview2.WebView2DevTools;
import org.appwork.utils.os.windows.webview2.WebView2Exception;
import org.appwork.utils.os.windows.webview2.WebView2ScriptException;
import org.appwork.utils.os.windows.webview2.WebView2UnknownOutcomeException;
import org.appwork.utils.os.windows.webview2.com.WebView2VTable;

/** Headless coverage of the isolated-world script execution and of the COM slots it depends on. Runs on every platform. */
public class WebView2DevToolsTest extends AWTest {
    public static void main(String[] args) {
        AWTest.run();
    }

    @Override
    public void runTest() throws Exception {
        testNativeSlots();
        testFrameEnumerationAndIsolatedExecution();
        testRootContextCacheAndDocumentInvalidation();
        testDocumentScriptAndResultDecoding();
    }

    private void testNativeSlots() throws Exception {
        /* Guard the COM ABI used for pre-commit navigation cancellation. */
        assertEquals(Integer.valueOf(7), Integer.valueOf(WebView2VTable.WV_ADD_NAVIGATION_STARTING));
        assertEquals(Integer.valueOf(3), Integer.valueOf(WebView2VTable.NAV_START_ARGS_GET_URI));
        assertEquals(Integer.valueOf(8), Integer.valueOf(WebView2VTable.NAV_START_ARGS_PUT_CANCEL));
        assertEquals(Integer.valueOf(0), Integer.valueOf(WebView2VTable.release(null)));
    }

    private void testFrameEnumerationAndIsolatedExecution() throws Exception {
        final FakeTransport transport = new FakeTransport();
        final WebView2DevTools devTools = new WebView2DevTools(transport, "/* bridge */");
        devTools.evaluateInAllFrames("window.test=1;");
        assertEquals(Arrays.asList("root", "child-a", "child-b"), transport.createdFrames);
        assertEquals(Integer.valueOf(3), Integer.valueOf(transport.expressions.size()));
        for (String expression : transport.expressions) {
            assertTrue(expression.contains("/* bridge */\nwindow.test=1;"));
            assertTrue(expression.contains("window.test=1;"));
        }
    }

    private void testRootContextCacheAndDocumentInvalidation() throws Exception {
        final FakeTransport transport = new FakeTransport();
        final WebView2DevTools devTools = new WebView2DevTools(transport, "bridge();");
        assertEquals("ok", devTools.evaluate("one();"));
        assertEquals("ok", devTools.evaluate("two();"));
        assertEquals(Integer.valueOf(1), Integer.valueOf(transport.createdFrames.size()));
        devTools.documentChanged();
        assertEquals("ok", devTools.evaluate("three();"));
        assertEquals(Integer.valueOf(2), Integer.valueOf(transport.createdFrames.size()));
    }

    private void testDocumentScriptAndResultDecoding() throws Exception {
        final FakeTransport transport = new FakeTransport();
        final WebView2DevTools devTools = new WebView2DevTools(transport, "bridge();");
        devTools.addScriptOnDocumentCreated("plugin();");
        assertEquals(WebView2DevTools.DEFAULT_WORLD, transport.lastDocumentScript.get("worldName"));
        assertTrue(String.valueOf(transport.lastDocumentScript.get("source")).endsWith("bridge();\nplugin();"));
        /* The bridge is told that it runs in the isolated world, where a message must only use the private channel. */
        assertTrue(String.valueOf(transport.lastDocumentScript.get("source")).startsWith("window.__awWebView2IsolatedWorld=true;"));
        final WebView2DevTools namedWorld = new WebView2DevTools(transport, "", "my_world");
        namedWorld.addScriptOnDocumentCreated("plugin();");
        assertEquals("my_world", transport.lastDocumentScript.get("worldName"));
        assertEquals("plugin();", transport.lastDocumentScript.get("source"));
        assertEquals("text", WebView2DevTools.evaluationValue(WebView2DevTools.mapOf("result", WebView2DevTools.mapOf("value", "text"))));
        assertEquals("[1,2]", WebView2DevTools.evaluationValue(WebView2DevTools.mapOf("result", WebView2DevTools.mapOf("value", Arrays.asList(Integer.valueOf(1), Integer.valueOf(2))))));
        assertEquals(null, WebView2DevTools.evaluationValue(WebView2DevTools.mapOf("result", WebView2DevTools.mapOf("type", "undefined"))));
        try {
            WebView2DevTools.evaluationValue(WebView2DevTools.mapOf("exceptionDetails", "boom"));
            throw new Exception("exceptionDetails must fail the evaluation");
        } catch (WebView2ScriptException expected) {
            assertTrue(expected.getMessage().contains("boom"));
        }
        /* A script that threw is not run a second time; a failed call (stale context) is repeated once with a new context. */
        final FakeTransport failing = new FakeTransport();
        failing.scriptError = true;
        try {
            new WebView2DevTools(failing, "").evaluate("sideEffect();");
            throw new Exception("A script error must be reported");
        } catch (WebView2ScriptException expected) {
            assertEquals(Integer.valueOf(1), Integer.valueOf(failing.expressions.size()));
        }
        final FakeTransport stale = new FakeTransport();
        stale.failNextEvaluate = true;
        assertEquals("ok", new WebView2DevTools(stale, "").evaluate("again();"));
        assertEquals(Integer.valueOf(2), Integer.valueOf(stale.expressions.size()));
        assertEquals(Integer.valueOf(2), Integer.valueOf(stale.createdFrames.size()));
        /* No answer (timeout): the script may have run, so it is not repeated. */
        final FakeTransport silent = new FakeTransport();
        silent.timeoutNextEvaluate = true;
        try {
            new WebView2DevTools(silent, "").evaluate("once();");
            throw new Exception("A timeout must be reported");
        } catch (WebView2UnknownOutcomeException expected) {
            assertEquals(Integer.valueOf(1), Integer.valueOf(silent.expressions.size()));
        }
    }

    private static final class FakeTransport implements WebView2DevTools.Transport {
        private final List<String>  createdFrames = new ArrayList<String>();
        private final List<String>  expressions   = new ArrayList<String>();
        private Map<String, Object> lastDocumentScript;
        private int                 contextId;
        private boolean             scriptError;
        private boolean             failNextEvaluate;
        private boolean             timeoutNextEvaluate;

        public Map<String, Object> call(String method, Map<String, Object> parameters) throws WebView2Exception {
            if ("Page.getFrameTree".equals(method)) {
                final Map<String, Object> childA = WebView2DevTools.mapOf("frame", WebView2DevTools.mapOf("id", "child-a"));
                final Map<String, Object> childB = WebView2DevTools.mapOf("frame", WebView2DevTools.mapOf("id", "child-b"));
                final Map<String, Object> root = WebView2DevTools.mapOf("frame", WebView2DevTools.mapOf("id", "root"), "childFrames", Arrays.asList(childA, childB));
                return WebView2DevTools.mapOf("frameTree", root);
            }
            if ("Page.createIsolatedWorld".equals(method)) {
                createdFrames.add(String.valueOf(parameters.get("frameId")));
                return WebView2DevTools.mapOf("executionContextId", Integer.valueOf(++contextId));
            }
            if ("Runtime.evaluate".equals(method)) {
                expressions.add(String.valueOf(parameters.get("expression")));
                if (failNextEvaluate) {
                    failNextEvaluate = false;
                    throw new WebView2Exception("Cannot find context with specified id");
                }
                if (timeoutNextEvaluate) {
                    timeoutNextEvaluate = false;
                    throw new WebView2UnknownOutcomeException("Timed out waiting for a WebView2 callback");
                }
                if (scriptError) {
                    return WebView2DevTools.mapOf("exceptionDetails", "boom");
                }
                return WebView2DevTools.mapOf("result", WebView2DevTools.mapOf("value", "ok"));
            }
            if ("Page.addScriptToEvaluateOnNewDocument".equals(method)) {
                lastDocumentScript = parameters;
                return WebView2DevTools.mapOf("identifier", "script-1");
            }
            throw new WebView2Exception("Unexpected method " + method);
        }
    }
}
