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
package org.appwork.utils.os.windows.webview2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.utils.StringUtils;

/**
 * The small CDP subset that gives host scripts an isolated JavaScript world, so page scripts cannot see or change them.
 * Native WebView2 owns rendering, navigation and events; CDP is used only for frame enumeration and one-shot script
 * evaluation.
 */
public final class WebView2DevTools {
    /** Name of the isolated world when the host does not choose one. */
    public static final String DEFAULT_WORLD = "appwork_webview2_host";

    public interface Transport {
        Map<String, Object> call(String method, Map<String, Object> parameters) throws WebView2Exception;
    }

    private static final String ISOLATED_WORLD_MARKER = "window.__awWebView2IsolatedWorld=true;\n";
    private final Transport transport;
    private final String    bridgeScript;
    private final String    worldName;
    private Integer         rootContextId;
    private volatile String bindingName;

    public WebView2DevTools(Transport transport, String bridgeScript) {
        this(transport, bridgeScript, null);
    }

    /**
     * @param worldName
     *            name of the isolated world, empty for {@link #DEFAULT_WORLD}
     */
    public WebView2DevTools(Transport transport, String bridgeScript, String worldName) {
        if (transport == null) {
            throw new IllegalArgumentException("transport is required");
        }
        this.transport = transport;
        this.bridgeScript = bridgeScript != null ? bridgeScript : "";
        this.worldName = StringUtils.isEmpty(worldName) ? DEFAULT_WORLD : worldName;
    }

    public String getWorldName() {
        return worldName;
    }

    public synchronized void documentChanged() {
        rootContextId = null;
    }

    public synchronized String evaluate(String script) throws WebView2Exception {
        try {
            return evaluateInContext(scriptWithBridge(script), rootContext());
        } catch (WebView2ScriptException scriptError) {
            /* The script ran and threw. Running it again would repeat its side effects. */
            throw scriptError;
        } catch (WebView2UnknownOutcomeException unknown) {
            /* No answer (timeout): the script may have run. Repeating it could repeat its side effects. */
            throw unknown;
        } catch (WebView2Exception first) {
            /* The call itself failed, typically because the cached context belongs to a document that is gone. */
            rootContextId = null;
            return evaluateInContext(scriptWithBridge(script), rootContext());
        }
    }

    public synchronized void evaluateInAllFrames(String script) throws WebView2Exception {
        if (StringUtils.isEmpty(script)) {
            return;
        }
        final List<String> frameIds = frameIds(transport.call("Page.getFrameTree", null));
        if (frameIds.isEmpty()) {
            evaluate(script);
            return;
        }
        WebView2Exception firstFailure = null;
        int succeeded = 0;
        for (int i = 0; i < frameIds.size(); i++) {
            try {
                final int contextId = createWorld(frameIds.get(i));
                if (i == 0) {
                    rootContextId = Integer.valueOf(contextId);
                }
                evaluateInContext(scriptWithBridge(script), contextId);
                succeeded++;
            } catch (WebView2Exception error) {
                if (firstFailure == null) {
                    firstFailure = error;
                }
            }
        }
        if (succeeded == 0 && firstFailure != null) {
            throw firstFailure;
        }
    }

    public synchronized void addScriptOnDocumentCreated(String script) throws WebView2Exception {
        if (StringUtils.isEmpty(script)) {
            return;
        }
        transport.call("Page.addScriptToEvaluateOnNewDocument", mapOf("source", scriptWithBridge(script), "worldName", worldName));
    }

    private int rootContext() throws WebView2Exception {
        if (rootContextId != null) {
            return rootContextId.intValue();
        }
        final List<String> ids = frameIds(transport.call("Page.getFrameTree", null));
        if (ids.isEmpty()) {
            throw new WebView2Exception("WebView2 CDP returned no root frame");
        }
        rootContextId = Integer.valueOf(createWorld(ids.get(0)));
        return rootContextId.intValue();
    }

    private int createWorld(String frameId) throws WebView2Exception {
        final Map<String, Object> result = transport.call("Page.createIsolatedWorld", mapOf("frameId", frameId, "worldName", worldName, "grantUniveralAccess", Boolean.FALSE));
        final Object context = result != null ? result.get("executionContextId") : null;
        final int contextId;
        if (context instanceof Number) {
            contextId = ((Number) context).intValue();
        } else {
            try {
                contextId = Integer.parseInt(String.valueOf(context));
            } catch (RuntimeException error) {
                throw new WebView2Exception("WebView2 CDP returned no execution context");
            }
        }
        final String binding = bindingName;
        if (binding != null) {
            /* The private channel to the host, installed in exactly this context. */
            transport.call("Runtime.addBinding", mapOf("name", binding, "executionContextId", Integer.valueOf(contextId)));
            /* Messages that host scripts of this world posted before the channel existed. */
            transport.call("Runtime.evaluate", mapOf("expression", "typeof window.__awWebView2Flush==='function'&&window.__awWebView2Flush();void 0;", "contextId", Integer.valueOf(contextId), "returnByValue", Boolean.TRUE));
        }
        return contextId;
    }

    /**
     * Creates the isolated world of the top document, if it does not exist yet, and with it the private channel to the host. Called
     * after a document was loaded, so that scripts registered with {@link #addScriptOnDocumentCreated(String)} can post without
     * waiting for the first {@link #evaluate(String)}.
     */
    public synchronized void prepareRootWorld() throws WebView2Exception {
        rootContext();
    }

    /**
     * Name of a function that is installed in every isolated world this object creates and reports its calls as the DevTools event
     * {@code Runtime.bindingCalled}; {@code null} for none.
     */
    public void setBindingName(String bindingName) {
        this.bindingName = bindingName;
    }

    private String evaluateInContext(String script, int contextId) throws WebView2Exception {
        final Map<String, Object> result = transport.call("Runtime.evaluate", mapOf("expression", script, "contextId", Integer.valueOf(contextId), "returnByValue", Boolean.TRUE, "awaitPromise", Boolean.TRUE, "userGesture", Boolean.TRUE));
        return evaluationValue(result);
    }

    /**
     * Puts the bridge in front of a host script. The marker tells the bridge that it runs in the isolated world, where a message
     * must only leave through the private channel.
     */
    private String scriptWithBridge(String script) {
        if (StringUtils.isEmpty(bridgeScript)) {
            return script;
        }
        return ISOLATED_WORLD_MARKER + bridgeScript + "\n" + script;
    }

    public static String evaluationValue(Map<String, Object> response) throws WebView2Exception {
        if (response != null && response.containsKey("exceptionDetails")) {
            throw new WebView2ScriptException("WebView2 Runtime.evaluate failed: " + String.valueOf(response.get("exceptionDetails")));
        }
        final Map<String, Object> remote = asMap(response != null ? response.get("result") : null);
        if (remote == null || !remote.containsKey("value")) {
            return null;
        }
        final Object value = remote.get("value");
        if (value instanceof String) {
            return (String) value;
        }
        return value != null ? JSonStorage.serializeToJson(value) : null;
    }

    public static List<String> frameIds(Map<String, Object> response) {
        final ArrayList<String> result = new ArrayList<String>();
        collectFrameIds(asMap(response != null ? response.get("frameTree") : null), result);
        return result;
    }

    @SuppressWarnings("unchecked")
    private static void collectFrameIds(Map<String, Object> tree, List<String> result) {
        if (tree == null) {
            return;
        }
        final Map<String, Object> frame = asMap(tree.get("frame"));
        if (frame != null && frame.get("id") != null) {
            final String id = String.valueOf(frame.get("id"));
            if (StringUtils.isNotEmpty(id)) {
                result.add(id);
            }
        }
        final Object children = tree.get("childFrames");
        if (children instanceof List) {
            for (Object child : (List<Object>) children) {
                collectFrameIds(asMap(child), result);
            }
        }
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> asMap(Object value) {
        return value instanceof Map ? (Map<String, Object>) value : null;
    }

    public static Map<String, Object> mapOf(Object... entries) {
        final HashMap<String, Object> result = new HashMap<String, Object>();
        for (int i = 0; i + 1 < entries.length; i += 2) {
            result.put(String.valueOf(entries[i]), entries[i + 1]);
        }
        return result;
    }
}
