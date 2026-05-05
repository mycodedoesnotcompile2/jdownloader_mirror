package org.jdownloader.extensions.eventscripter;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.tools.shell.Global;

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
            final Global scope = getScriptThread().getScope();
            scriptThread.initContext(cx);
            callback.call(cx, scope, scope, params);
        } finally {
            Context.exit();
        }
    }
}
