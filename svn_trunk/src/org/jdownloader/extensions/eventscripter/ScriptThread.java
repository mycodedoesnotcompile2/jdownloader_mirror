package org.jdownloader.extensions.eventscripter;

import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import javax.swing.SwingUtilities;

import jd.http.Browser;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.simplejson.MinimalMemoryMap;
import org.appwork.uio.CloseReason;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.ExceptionDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.ExceptionDialog;
import org.jdownloader.controlling.UniqueAlltimeID;
import org.jdownloader.extensions.eventscripter.sandboxobjects.ScriptEnvironment;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.myjdownloader.client.json.JsonMap;
import org.jdownloader.scripting.JSRhinoPermissionRestricter;
import org.jdownloader.scripting.JSShutterDelegate;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.EcmaError;
import org.mozilla.javascript.NativeJavaMethod;
import org.mozilla.javascript.ScriptRuntime;
import org.mozilla.javascript.ScriptableObject;
import org.mozilla.javascript.UniqueTag;
import org.mozilla.javascript.tools.shell.Global;

public class ScriptThread extends Thread implements JSShutterDelegate {
    private final ScriptEntry                  script;
    private final Map<String, Object>          props;
    private Global                             scope;
    private Context                            cx;
    private final LogSource                    logger;
    private final EventScripterExtension       extension;
    private boolean                            checkPermissions   = true;
    private boolean                            disableOnException = true;
    private boolean                            notifyOnException  = true;
    private boolean                            advancedAlert      = false;
    private final List<ReentrantReadWriteLock> locks              = new ArrayList<ReentrantReadWriteLock>();

    public boolean isNotifyOnException() {
        return notifyOnException;
    }

    public void setNotifyOnException(boolean notifyOnException) {
        this.notifyOnException = notifyOnException;
    }

    public boolean isDisableOnException() {
        return disableOnException;
    }

    public void setDisableOnException(boolean disableOnException) {
        this.disableOnException = disableOnException;
    }

    private final Set<String> permissions = new HashSet<String>();

    public boolean isPermissionSet(String permission) {
        synchronized (permissions) {
            return permission != null && permissions.contains(permission);
        }
    }

    public void setPermissionSet(String permission) {
        if (permission != null) {
            synchronized (permissions) {
                permissions.add(permission);
            }
        }
    }

    public boolean isCheckPermissions() {
        return checkPermissions;
    }

    public void setCheckPermissions(boolean checkPermissions) {
        this.checkPermissions = checkPermissions;
    }

    public boolean isAdvancedAlert() {
        return advancedAlert;
    }

    public void setAdvancedAlert(boolean advancedAlert) {
        this.advancedAlert = advancedAlert;
    }

    public LogSource getLogger() {
        return logger;
    }

    public ScriptThread(EventScripterExtension eventScripterExtension, ScriptEntry script, Map<String, Object> props, LogSource logSource) {
        this.script = script;
        this.props = props;
        this.logger = logSource;
        this.extension = eventScripterExtension;
    }

    @Override
    public void start() {
        startThread();
        if (!isTestRun() && isSynchronous() && (Application.isHeadless() || !SwingUtilities.isEventDispatchThread())) {
            try {
                join();
            } catch (InterruptedException e) {
                getLogger().log(e);
            }
        }
    }

    protected void startThread() {
        super.start();
    }

    private static final WeakHashMap<Object, UniqueAlltimeID> SCRIPTLOCKS = new WeakHashMap<Object, UniqueAlltimeID>();

    private static Object getScriptLock(final ScriptEntry script) {
        synchronized (SCRIPTLOCKS) {
            for (final Entry<Object, UniqueAlltimeID> scriptLock : SCRIPTLOCKS.entrySet()) {
                if (scriptLock.getValue().getID() == script.getID()) {
                    return scriptLock.getKey();
                }
            }
            final Object scriptLock = new Object();
            SCRIPTLOCKS.put(scriptLock, new UniqueAlltimeID(script.getID()));
            return scriptLock;
        }
    }

    public boolean isSynchronous() {
        return false;
    }

    protected boolean isTestRun() {
        return false;
    }

    @Override
    public void run() {
        if (isSynchronous() && !isTestRun()) {
            final Object scriptLock = getScriptLock(script);
            synchronized (scriptLock) {
                if (script.isEnabled()) {
                    executeScipt();
                }
            }
        } else {
            if (script.isEnabled()) {
                executeScipt();
            }
        }
    }

    protected void initContext(Context context) {
        cx.setOptimizationLevel(-1);
        cx.setLanguageVersion(Context.VERSION_ES6);
        cx.getWrapFactory().setJavaPrimitiveWrap(false);// old default
    }

    private synchronized void executeScipt() {
        scope = new Global();
        cx = Context.enter();
        try {
            initContext(cx);
            scope.init(cx);
            final String preloadClasses = preInitClasses();
            evalTrusted(preloadClasses);
            // required by some libraries
            evalTrusted("global=this;");
            initEnvironment();
            cleanupClasses();
            try {
                evalUNtrusted(script.getScript());
            } finally {
                finalizeEnvironment();
            }
            // ProcessBuilderFactory.runCommand(commandline);
        } catch (Throwable e) {
            logger.exception("An Error Occured:ID=" + script.getID() + "|Name=" + script.getName() + "|Trigger=" + script.getEventTrigger() + "|Message:" + e.getMessage(), e);
            notifyAboutException(e);
        } finally {
            try {
                Context.exit();
            } finally {
                releaseLocks();
            }
        }
    }

    protected void releaseLocks() {
        synchronized (locks) {
            while (locks.size() > 0) {
                final ReentrantReadWriteLock lock = locks.remove(0);
                final int readLocks = lock.getReadHoldCount();
                for (int free = 0; free < readLocks; free++) {
                    lock.readLock().unlock();
                }
                final int writeLocks = lock.getWriteHoldCount();
                for (int free = 0; free < writeLocks; free++) {
                    lock.writeLock().unlock();
                }
            }
        }
    }

    public void addLock(final ReentrantReadWriteLock lock) {
        if (lock != null) {
            synchronized (locks) {
                if (!locks.contains(lock)) {
                    locks.add(lock);
                }
            }
        }
    }

    public void notifyAboutException(Throwable e) {
        if (isDisableOnException()) {
            script.setEnabled(false);
            extension.refreshScripts();
        }
        if (isNotifyOnException()) {
            UIOManager.I().show(ExceptionDialogInterface.class, new ExceptionDialog(UIOManager.LOGIC_DONT_SHOW_AGAIN_DELETE_ON_EXIT | UIOManager.BUTTONS_HIDE_CANCEL, "An Error Occured:ID=" + script.getID() + "|Name=" + script.getName() + "|Trigger=" + script.getEventTrigger(), e.getMessage(), e, null, null));
        }
    }

    private List<Class<?>> visibleClasses = null;

    protected String preInitClasses() {
        final HashSet<String> dupes = new HashSet<String>();
        final HashSet<Class<?>> visibleClasses = new HashSet<Class<?>>();
        dupes.add("org.mozilla.javascript.Function");
        dupes.add("void");
        StringBuilder preloadClasses = new StringBuilder("");
        for (Class<?> c : new Class[] { Boolean.class, Byte.class, Short.class, Integer.class, Long.class, String.class, Double.class, Float.class, ArrayList.class, List.class, LinkedList.class, Map.class, HashMap.class, JsonMap.class, Set.class, HashSet.class, MinimalMemoryMap.class, org.mozilla.javascript.EcmaError.class, ScriptEnvironment.class, EnvironmentException.class }) {
            if (c.isArray()) {
                c = c.getComponentType();
            }
            if (!dupes.add(c.getName())) {
                continue;
            } else if (c.isPrimitive()) {
                continue;
            }
            visibleClasses.add(c);
            preloadClasses.append("load=");
            preloadClasses.append(c.getName());
            preloadClasses.append(";\r\n");
        }
        final Collection<Class<?>> clazzes = ScriptEnvironment.getRequiredClasses();
        clazzes.addAll(script.getEventTrigger().getAPIClasses());
        clazzes.add(Object.class);
        for (Class<?> c : clazzes) {
            if (c.isArray()) {
                // preloadClasses += "load=" + c.getName() + ";\r\n";
                c = c.getComponentType();
            }
            if (!dupes.add(c.getName())) {
                continue;
            } else if (c.isPrimitive()) {
                continue;
            }
            preloadClasses.append("load=");
            preloadClasses.append(c.getName());
            preloadClasses.append(";\r\n");
        }
        for (Field f : ScriptEnvironment.class.getDeclaredFields()) {
            if (f.getAnnotation(ScriptAPI.class) != null) {
                Class<?> c = f.getType();
                if (c.isArray()) {
                    c = c.getComponentType();
                }
                if (!dupes.add(c.getName())) {
                    continue;
                } else if (Clazz.isPrimitive(c)) {
                    continue;
                }
                preloadClasses.append("load=");
                preloadClasses.append(c.getName());
                preloadClasses.append(";\r\n");
            }
        }
        preloadClasses.append("delete load;");
        this.visibleClasses = new ArrayList<Class<?>>(visibleClasses);
        return preloadClasses.toString();
    }

    private void initEnvironment() throws IllegalAccessException {
        for (Method f : ScriptEnvironment.class.getDeclaredMethods()) {
            if (f.getAnnotation(ScriptAPI.class) != null) {
                evalTrusted(f.getName() + "=" + ScriptEnvironment.class.getName() + "." + f.getName() + ";");
            }
        }
        for (Field f : ScriptEnvironment.class.getDeclaredFields()) {
            if (f.getAnnotation(ScriptAPI.class) != null) {
                ScriptableObject.putProperty(scope, f.getName(), ScriptEnvironment.toJSObject(f.get(null)));
            }
        }
        for (Entry<String, Object> es : props.entrySet()) {
            final Object existing = ScriptableObject.getProperty(scope, es.getKey());
            if (existing instanceof NativeJavaMethod) {
                // do not replace NativeJavaMethod
                continue;
            } else {
                ScriptableObject.putProperty(scope, es.getKey(), es.getValue());
            }
        }
    }

    protected void finalizeEnvironment() throws IllegalAccessException {
        final List<String> keySet = new ArrayList<String>(props.keySet());
        for (final String key : keySet) {
            final Object value = ScriptableObject.getProperty(scope, key);
            if (UniqueTag.NOT_FOUND.equals(value)) {
                // do not store NOT_FOUND elements
                continue;
            } else if (value instanceof NativeJavaMethod) {
                // do not store NativeJavaMethod
                continue;
            } else {
                props.put(key, value);
            }
        }
    }

    private void evalUNtrusted(String script) {
        cx.evaluateString(getScope(), script, "", 1, null);
    }

    public Object evalTrusted(String preloadClasses) {
        // System.out.println(preloadClasses);
        return JSRhinoPermissionRestricter.evaluateTrustedString(cx, getScope(), preloadClasses, "", 1, null);
    }

    private void cleanupClasses() {
        final List<String> list = JSRhinoPermissionRestricter.getLoaded();
        // Cleanup
        ScriptableObject.deleteProperty(scope, "Packages");
        for (String s : list) {
            while (true) {
                try {
                    ScriptableObject.deleteProperty(scope, s);
                } catch (Throwable e) {
                    // e.printStackTrace();
                }
                int index = s.lastIndexOf(".");
                if (index > 0) {
                    s = s.substring(0, index);
                } else {
                    break;
                }
            }
        }
    }

    public void requireJavascript(final String fileOrUrl) throws IOException {
        final String permissionID = "ASK_TO_REQUIRE_JS_" + fileOrUrl;
        final ConfirmDialog d = new ConfirmDialog(0 | Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN | UIOManager.LOGIC_DONT_SHOW_AGAIN_IGNORES_CANCEL, T.T.securityLoading_title(), T.T.securityLoading(fileOrUrl), new AbstractIcon(IconKey.ICON_SERVER, 32), null, null) {
            @Override
            public String getDontShowAgainKey() {
                return permissionID;
            }

            @Override
            protected int getPreferredWidth() {
                return 600;
            }

            public void windowClosing(final WindowEvent arg0) {
                setReturnmask(false);
                this.dispose();
            }
        };
        d.setDoNotShowAgainSelected(true);
        ConfirmDialogInterface dialog = null;
        if (!isCheckPermissions() || isPermissionSet(permissionID) || (dialog = d.show()).getCloseReason() == CloseReason.OK) {
            if (dialog != null && dialog.isDontShowAgainSelected()) {
                setPermissionSet(permissionID);
            }
            final String js;
            if (fileOrUrl.matches("^https?\\:\\/\\/.+")) {
                // url
                final Browser br = new Browser();
                br.setFollowRedirects(true);
                js = br.getPage(fileOrUrl);
            } else {
                File file = new File(fileOrUrl);
                if (!file.exists()) {
                    file = Application.getResource(fileOrUrl);
                }
                if (file.exists()) {
                    js = IO.readFileToString(file);
                } else {
                    js = "";
                }
            }
            logger.info(js);
            evalUNtrusted(js);
        }
    }

    public Context getContext() {
        return cx;
    }

    public Global getScope() {
        return scope;
    }

    public ScriptEntry getScript() {
        return script;
    }

    private final HashSet<String> loadedLibrary = new HashSet<String>();

    public void ensureLibrary(String string) {
        synchronized (loadedLibrary) {
            if (loadedLibrary.add(string)) {
                try {
                    evalTrusted(IO.readURLToString(ScriptEntry.class.getResource(string)));
                } catch (IOException e) {
                    throw new WTFException(e);
                }
            }
        }
    }

    /**
     * create a native javaobject for settings
     *
     * @param settings
     * @return
     */
    public Object toNative(Object settings) {
        String json = JSonStorage.serializeToJson(settings);
        // final String reviverjs = "(function(key,value) { return value; })";
        // final Callable reviver = (Callable) cx.evaluateString(scope, reviverjs, "reviver", 0, null);
        //
        // Object nativ = NativeJSON.parse(cx, scope, json, reviver);
        // String tmpName = "json_" + System.currentTimeMillis();
        Object ret = cx.evaluateString(scope, "(function(){ return " + json + ";})();", "", 0, null);
        // for (Entry<Object, Object> es : ((NativeObject) ret).entrySet()) {
        // ((NativeObject) ret).setAttributes(es.getKey() + "", NativeObject.READONLY);
        // }
        return ret;
    }

    @Override
    public boolean isClassVisibleToScript(boolean trusted, String className) {
        if (trusted) {
            return true;
        } else if (className.startsWith("adapter")) {
            return true;
        } else if ("org.mozilla.javascript.EcmaError".equals(className) || "net.sourceforge.htmlunit.corejs.javascript.EcmaError".equals(className)) {
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().severe("Javascript error occured");
            return true;
        } else if ("org.mozilla.javascript.ConsString".equals(className) || "net.sourceforge.htmlunit.corejs.javascript.ConsString".equals(className)) {
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().severe("Javascript error occured");
            return true;
        } else if ("org.mozilla.javascript.JavaScriptException".equals(className) || "net.sourceforge.htmlunit.corejs.javascript.JavaScriptException".equals(className)) {
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().severe("Javascript error occured");
            return true;
        } else if (org.jdownloader.extensions.eventscripter.EnvironmentException.class.getName().equals(className)) {
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().severe("Environment error occured");
            return true;
        } else if ("org.mozilla.javascript.WrappedException".equals(className) || "net.sourceforge.htmlunit.corejs.javascript.WrappedException".equals(className)) {
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().severe("Script RuntimeException occured");
            return true;
        } else if ("org.mozilla.javascript.EvaluatorException".equals(className) || "net.sourceforge.htmlunit.corejs.javascript.EvaluatorException".equals(className)) {
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().severe("Javascript error occured");
            return true;
        } else {
            try {
                if (visibleClasses != null) {
                    final Class<?> clazz = Class.forName(className, false, null);
                    for (Class<?> visibleClass : visibleClasses) {
                        if (visibleClass.equals(clazz) || visibleClass.isAssignableFrom(clazz) || visibleClass.equals(clazz.getDeclaringClass())) {
                            return true;
                        }
                    }
                }
            } catch (ClassNotFoundException e) {
                logger.log(e);
            }
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().severe("Security Violation " + className);
            final EcmaError ret = ScriptRuntime.constructError("Security Violation", "Security Violation " + className);
            throw ret;
        }
    }
}
