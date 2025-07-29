package org.jdownloader.extensions.eventscripter;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;

public class ScriptReferenceThread extends Thread {

    private final ScriptThread scriptThread;

    public ScriptThread getScriptThread() {
        return scriptThread;
    }

    public ScriptReferenceThread(ScriptThread env) {
        this.scriptThread = env;
    }

    public void executeCallback(Function callback, Object... params) {
        final Context cx = Context.enter();
        try {
            scriptThread.initContext(cx);
            callback.call(cx, getScriptThread().getScope(), getScriptThread().getScope(), params);
        } finally {
            Context.exit();
        }

    }
}
