/*
 * Host bridge of WindowsWebView2Browser. Defines window.__awHostPost(stringOrObject), which sends a message to the host
 * application. Objects are sent as JSON. Returns false when the message could not be sent.
 *
 * The bridge runs in two kinds of JavaScript worlds and uses a different channel in each:
 *
 * 1. Isolated world of the host (scripts run with executeScript and its relatives). Here the browser provides the private
 *    function __awWebView2Binding. Its calls go straight to the host (Listener.onHostScriptMessage). Nothing passes through
 *    the page, so page scripts can neither read nor change nor forge these messages. If the function is missing, the message
 *    is NOT sent some other way: it must never travel through window.postMessage, where every script of the page listens.
 *
 * 2. Main world of the page (page scripts, or host scripts run with executeScriptInMainWorld). The top document posts
 *    through chrome.webview (Listener.onWebMessage). A frame cannot reach the host itself and asks the top document to pass
 *    its message on; the top document marks it as a frame message with the origin the browser reports for the sender
 *    (Listener.onFrameMessage), so a frame can never speak as the top document. Everything on this path is page-controlled
 *    and untrusted.
 */
(function () {
    if (window.__awWebView2Bridge) {
        return;
    }
    window.__awWebView2Bridge = true;
    var FORWARD = "__awWebView2HostForward";

    function postNative(value) {
        try {
            if (window.chrome && window.chrome.webview && window.chrome.webview.postMessage) {
                window.chrome.webview.postMessage(value);
                return true;
            }
        } catch (eNative) {
        }
        return false;
    }

    function isTop() {
        try {
            return window.top === window;
        } catch (eTop) {
            return false;
        }
    }

    function send(payload) {
        var value = typeof payload === "string" ? payload : JSON.stringify(payload);
        if (window.__awWebView2IsolatedWorld || typeof window.__awWebView2Binding === "function") {
            /*
             * Isolated world: private channel only. The browser installs the function shortly after the document was created.
             * Until then messages wait in a queue of this world; they are never sent through a channel the page can see.
             */
            var queue = window.__awWebView2Queue || (window.__awWebView2Queue = []);
            if (queue.length >= 1000) {
                return false;
            }
            queue.push(JSON.stringify({ top: isTop(), url: String(location.href), payload: value }));
            flush();
            return true;
        }
        if (isTop()) {
            return postNative(value);
        }
        try {
            window.top.postMessage({ type: FORWARD, payload: value }, "*");
            return true;
        } catch (eForward) {
            return false;
        }
    }

    function flush() {
        var queue = window.__awWebView2Queue;
        if (!queue || typeof window.__awWebView2Binding !== "function") {
            return;
        }
        while (queue.length > 0) {
            window.__awWebView2Binding(queue.shift());
        }
    }

    window.__awHostPost = send;
    /* Called by the browser after it installed the private channel in this world. */
    window.__awWebView2Flush = flush;

    if (isTop() && !window.__awWebView2IsolatedWorld && typeof window.__awWebView2Binding !== "function") {
        window.addEventListener("message", function (event) {
            try {
                var data = event.data;
                /* Only messages of other frames are passed on, always marked with the origin of their sender. */
                if (data && data.type === FORWARD && typeof data.payload === "string" && event.source !== window) {
                    postNative(JSON.stringify({ __awWebView2Frame: String(event.origin), payload: data.payload }));
                }
            } catch (e) {
            }
        }, false);
    }
})();
