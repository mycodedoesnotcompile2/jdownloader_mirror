package org.jdownloader.extensions.eventscripter.sandboxobjects;

import java.awt.Dialog.ModalityType;
import java.awt.Font;
import java.awt.event.WindowEvent;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map.Entry;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.BooleanControl;
import javax.sound.sampled.Clip;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.FloatControl;
import javax.sound.sampled.LineEvent;
import javax.sound.sampled.LineEvent.Type;
import javax.sound.sampled.LineListener;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.JTextComponent;

import jd.controlling.AccountController;
import jd.controlling.TaskQueue;
import jd.controlling.accountchecker.AccountChecker;
import jd.controlling.accountchecker.AccountChecker.AccountCheckJob;
import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.downloadcontroller.DownloadSession;
import jd.controlling.downloadcontroller.DownloadSession.SessionState;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.DownloadWatchDogJob;
import jd.controlling.downloadcontroller.ProxyInfoHistory;
import jd.controlling.downloadcontroller.ProxyInfoHistory.WaitingSkipReasonContainer;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.proxy.AbstractProxySelectorImpl;
import jd.controlling.reconnect.Reconnecter;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.config.JsonConfig;
import org.appwork.swing.MigPanel;
import org.appwork.uio.CloseReason;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.BinaryLogic;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Files;
import org.appwork.utils.Hash;
import org.appwork.utils.IO;
import org.appwork.utils.ModifyLock;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.logging2.extmanager.DevNullLogger;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.CrossSystem.OSFamily;
import org.appwork.utils.processes.ProcessBuilderFactory;
import org.appwork.utils.processes.ProcessOutput;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.speedmeter.SpeedMeterInterface.Resolution;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.jdownloader.api.RemoteAPIController;
import org.jdownloader.controlling.packagizer.PackagizerController;
import org.jdownloader.extensions.eventscripter.EnvironmentException;
import org.jdownloader.extensions.eventscripter.ScriptAPI;
import org.jdownloader.extensions.eventscripter.ScriptEntry;
import org.jdownloader.extensions.eventscripter.ScriptReferenceThread;
import org.jdownloader.extensions.eventscripter.ScriptThread;
import org.jdownloader.extensions.eventscripter.T;
import org.jdownloader.extensions.eventscripter.Utils;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.ArraySet;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.WaitingSkipReason;
import org.jdownloader.scripting.JavaScriptEngineFactory;
import org.jdownloader.settings.SoundSettings;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.ScriptableObject;

public class ScriptEnvironment {

    @ScriptAPI(description = "JDownloader Installation Directory")
    public static String                                         JD_HOME           = Application.getResource("").getAbsolutePath();
    private static LogSource                                     LOGGER            = LogController.getInstance().getLogger("ScriptEnvironment");
    private static HashMap<ScriptEntry, HashMap<String, Object>> SCRIPT_PROPERTIES = new HashMap<ScriptEntry, HashMap<String, Object>>();

    @ScriptAPI(description = "Show a Message Box", parameters = { "myObject1", "MyObject2", "..." }, example = "alert(JD_HOME);")
    public static void alert(Object... objects) {
        final Object[] args = JavaScriptEngineFactory.convertJavaScriptToJava(objects);
        String message = null;
        if (args != null && args.length == 1 && args[0] instanceof String) {
            message = args[0].toString();
        } else {
            try {
                message = JSonStorage.serializeToJson(args != null && args.length == 1 ? args[0] : args);
            } catch (Exception e1) {
                try {
                    message = format(toJson(args != null && args.length == 1 ? args[0] : args));
                } catch (Exception e2) {
                    message = StringUtils.valueOfOrNull(args != null && args.length == 1 ? args[0] : args);
                }
            }
        }
        showMessageDialog(message);
    }

    @ScriptAPI(description = "disable permission checks")
    public static void disablePermissionChecks() {
        final ScriptThread env = getScriptThread();
        if (env != null) {
            env.setCheckPermissions(false);
        }
    }

    @ScriptAPI(description = "enable/disable alert with textbox and copy&paste")
    public static void setAdvancedAlert(final boolean b) {
        final ScriptThread env = getScriptThread();
        if (env != null) {
            env.setAdvancedAlert(b);
        }
    }

    @ScriptAPI(description = "enable/disable notification on exceptions")
    public static void setNotifyOnException(final boolean b) {
        final ScriptThread env = getScriptThread();
        if (env != null) {
            env.setNotifyOnException(b);
        }
    }

    @ScriptAPI(description = "enable/disable script on exceptions")
    public static void setDisableOnException(final boolean b) {
        final ScriptThread env = getScriptThread();
        if (env != null) {
            env.setDisableOnException(b);
        }
    }

    @ScriptAPI(description = "is current script run in synchronous mode?")
    public static boolean isSynchronous() {
        final ScriptThread env = getScriptThread();
        return env != null && env.isSynchronous();
    }

    @ScriptAPI(description = "enable permission checks")
    public static void enablePermissionChecks() {
        final ScriptThread env = getScriptThread();
        if (env != null) {
            env.setCheckPermissions(true);
        }
    }

    static synchronized void askForPermission(final String string) throws EnvironmentException {
        final ScriptThread env = getScriptThread();
        if (env.isCheckPermissions()) {
            final String md5 = Hash.getMD5(env.getScript().getScript());
            if (!env.isPermissionSet(md5)) {
                final ConfirmDialog d = new ConfirmDialog(0 | Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN | UIOManager.LOGIC_DONT_SHOW_AGAIN_IGNORES_CANCEL, T.T.permission_title(), T.T.permission_msg(env.getScript().getName(), env.getScript().getEventTrigger().getLabel(), string), new AbstractIcon(IconKey.ICON_SERVER, 32), T.T.allow(), T.T.deny()) {
                    @Override
                    public String getDontShowAgainKey() {
                        return "ASK_FOR_PERMISSION_" + md5 + "_" + string;
                    }

                    @Override
                    protected LogInterface getLogger() {
                        return new DevNullLogger();
                    }

                    @Override
                    protected int getPreferredWidth() {
                        return 600;
                    }

                    @Override
                    public boolean isRemoteAPIEnabled() {
                        return true;
                    }

                    public void windowClosing(final WindowEvent arg0) {
                        setReturnmask(false);
                        this.dispose();
                    }
                };
                d.setDoNotShowAgainSelected(true);
                if (d.show().getCloseReason() != CloseReason.OK) {
                    throw new EnvironmentException("Security Warning: User Denied Access to " + string);
                } else if (d.isDontShowAgainSelected()) {
                    env.setPermissionSet(md5);
                }
            }
        }
    }

    @ScriptAPI(description = "Call the MyJDownloader API locally (no network involved), see API Docs here https://my.jdownloader.org/developers/", parameters = { "\"namespace\"", "\"methodname\"", "parameter1", "parameter2", "..." }, example = "callAPI(\"downloadsV2\", \"queryLinks\", { \"name\": true})")
    public static Object callAPI(String namespace, String method, Object... parameters) throws EnvironmentException {
        askForPermission("call the Remote API: " + namespace + "/" + method);
        try {
            final Object[] args = JavaScriptEngineFactory.convertJavaScriptToJava(parameters);
            final Object ret = RemoteAPIController.getInstance().call(namespace, method, args);
            final ScriptThread env = getScriptThread();
            // convert to javascript object
            final String js = "(function(){ return " + JSonStorage.serializeToJson(ret) + ";}());";
            final Object retObject = env.evalTrusted(js);
            return retObject;
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Call a local Process asynchronous", parameters = { "\"myCallBackFunction\"|null", "\"commandline1\"", "\"commandline2\"", "\"...\"" }, example = "callAsync(function(exitCode,stdOut,errOut){ alert(\"Closed Notepad\");},\"notepad.exe\",JD_HOME+\"\\\\license.txt\");")
    public static void callAsync(final Function callback, final String... commands) throws EnvironmentException {
        askForPermission("Execute a local process");
        try {
            final ScriptThread env = getScriptThread();
            if (commands != null && commands.length > 0) {
                new ScriptReferenceThread(env) {
                    @Override
                    public void run() {
                        try {
                            try {
                                ProcessOutput ret = ProcessBuilderFactory.runCommand(commands);
                                if (callback != null) {
                                    if (CrossSystem.getOSFamily() == OSFamily.WINDOWS) {
                                        executeCallback(callback, ret.getExitCode(), new String(ret.getStdOutString("cp850").getBytes("UTF-8"), "UTF-8"), new String(ret.getErrOutString("cp850").getBytes("UTF-8"), "UTF-8"));
                                    } else {
                                        executeCallback(callback, ret.getExitCode(), ret.getStdOutString("UTF-8"), ret.getErrOutString("UTF-8"));
                                    }
                                }
                            } catch (IOException e) {
                                if (callback != null) {
                                    executeCallback(callback, -1, null, Exceptions.getStackTrace(e));
                                }
                                env.getLogger().log(e);
                                env.notifyAboutException(e);
                            }
                        } catch (Throwable e) {
                            env.notifyAboutException(e);
                        }
                    }
                }.start();
            }
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Call a local Process. Blocks Until the process returns", parameters = { "\"commandline1\"", "\"commandline2\"", "\"...\"" }, example = "var pingResultString = callSync(\"ping\",\"jdownloader.org\");")
    public static String callSync(final String... commands) throws EnvironmentException {
        askForPermission("Execute a local process");
        try {
            ProcessBuilder pb = ProcessBuilderFactory.create(commands);
            pb.redirectErrorStream(true);
            ProcessOutput ret = ProcessBuilderFactory.runCommand(pb);
            return ret.getStdOutString();
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    protected static void collectClasses(Class<? extends Object> cl, ArraySet<Class<?>> clazzes) {
        for (Method m : cl.getDeclaredMethods()) {
            if (cl == ScriptEnvironment.class && m.getAnnotation(ScriptAPI.class) == null) {
                continue;
            }
            if (m.getReturnType() == Object.class || !Modifier.isPublic(m.getModifiers()) || Clazz.isPrimitive(m.getReturnType()) || Clazz.isPrimitiveWrapper(m.getReturnType()) || Clazz.isString(m.getReturnType())) {
                continue;
            }
            if (clazzes.add(m.getReturnType())) {
                collectClasses(m.getReturnType(), clazzes);
            }
            for (Class<?> cl2 : m.getParameterTypes()) {
                if (cl2 == Object.class || Clazz.isPrimitive(cl2) || Clazz.isPrimitiveWrapper(cl2) || Clazz.isString(cl2)) {
                    continue;
                }
                if (clazzes.add(cl2)) {
                    collectClasses(cl2, clazzes);
                }
            }
            for (Field f : cl.getFields()) {
                if (f.getType() == Object.class || !Modifier.isPublic(m.getModifiers()) || Clazz.isPrimitive(f.getType()) || Clazz.isPrimitiveWrapper(f.getType()) || Clazz.isString(f.getType())) {
                    continue;
                }
                if (clazzes.add(f.getType())) {
                    collectClasses(f.getType(), clazzes);
                }
            }
        }
    }

    @ScriptAPI(description = "Delete a file or a directory", parameters = { "path", "recursive" }, example = "var myBooleanResult=deleteFile(JD_HOME+\"/mydirectory/\",false);")
    public static boolean deleteFile(String filepath, boolean recursive) throws EnvironmentException {
        askForPermission("delete a local fole or directory");
        try {
            if (recursive) {
                Files.deleteRecursiv(new File(filepath), true);
            } else {
                new File(filepath).delete();
            }
            return !new File(filepath).exists();
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    protected static boolean doCollectClass(Class<? extends Object> cl) {
        Package pkg = cl.getPackage();
        Package sPkg = ScriptEnvironment.class.getPackage();
        if (pkg == null || !pkg.getName().startsWith(sPkg.getName())) {
            return false;
        }
        return true;
    }

    private static String format(String js) {
        final ScriptThread env = getScriptThread();
        try {
            env.ensureLibrary("js_beautifier.js");
            String parametername;
            ScriptableObject.putProperty(env.getScope(), parametername = "text_" + System.currentTimeMillis(), js);
            String ret = env.evalTrusted("js_beautify(" + parametername + ", {   });") + "";
            return ret;
        } catch (Throwable e) {
            e.printStackTrace();
        }
        return js;
    }

    public static String getAPIDescription(ArraySet<Class<?>> triggerClazzes) {
        StringBuilder sb = new StringBuilder();
        //
        ArraySet<Class<?>> clazzes = new ArraySet<Class<?>>();
        sb.append("/* =============== ").append("Global API").append(" =============== */").append("\r\n");
        getAPIDescriptionForClass(sb, ScriptEnvironment.class);
        sb.append("/* =========  Properties =========*/\r\n");
        for (Field f : Utils.sort(ScriptEnvironment.class.getDeclaredFields())) {
            ScriptAPI ann = f.getAnnotation(ScriptAPI.class);
            if (ann != null) {
                sb.append("//").append(ann.description()).append(";\r\n");
                String simpleName = f.getType().getSimpleName();
                final boolean isArray = f.getType().isArray();
                if (isArray) {
                    simpleName = simpleName.replaceFirst("\\[\\]$", "");
                }
                sb.append("var my").append(simpleName.substring(0, 1).toUpperCase(Locale.ENGLISH)).append(simpleName.substring(1)).append(" = ");
                if (isArray) {
                    sb.append(" [] = ");
                }
                sb.append(f.getName()).append(";\r\n");
                if (StringUtils.isNotEmpty(ann.example())) {
                    sb.append(ann.example()).append("\r\n");
                }
            }
        }
        collectClasses(ScriptEnvironment.class, clazzes);
        clazzes.addAll(triggerClazzes);
        Collections.sort(clazzes, new Comparator<Class<?>>() {
            @Override
            public int compare(Class<?> o1, Class<?> o2) {
                return Utils.cleanUpClass(o1.getSimpleName()).compareTo(Utils.cleanUpClass(o2.getSimpleName()));
            }
        });
        sb.append("/* =============== ").append("Classes").append(" =============== */").append("\r\n");
        for (Class<?> cl : clazzes) {
            if (doCollectClass(cl)) {
                sb.append("/* === ").append(Utils.cleanUpClass(cl.getSimpleName())).append(" === */").append("\r\n");
                getAPIDescriptionForClass(sb, cl);
            }
        }
        return sb.toString();
    }

    /**
     * @param sb
     * @param cl
     */
    public static void getAPIDescriptionForClass(StringBuilder sb, Class<?> cl) {
        ScriptAPI clazzAnn = cl.getAnnotation(ScriptAPI.class);
        if (clazzAnn != null && StringUtils.isNotEmpty(clazzAnn.description())) {
            sb.append("/* ").append(clazzAnn.description()).append("*/").append("\r\n");
        }
        sb.append("/* =========  Methods =========*/\r\n");
        for (Method m : Utils.sort(cl.getDeclaredMethods())) {
            if (!Modifier.isPublic(m.getModifiers())) {
                // do not list non public methods
                continue;
            } else if ("hashCode".equals(m.getName()) && m.getParameterTypes().length == 0) {
                // do not list hashCode() method
                continue;
            } else if ("equals".equals(m.getName()) && m.getParameterTypes().length == 1 && m.getParameterTypes()[0] == Object.class) {
                // do not list equals(Object) method
                continue;
            }
            final ScriptAPI ann = m.getAnnotation(ScriptAPI.class);
            if (cl == ScriptEnvironment.class && ann == null) {
                continue;
            }
            final boolean isDeprecated = m.getAnnotation(Deprecated.class) != null;
            if (!Clazz.isVoid(m.getReturnType())) {
                String simpleName = m.getReturnType().getSimpleName();
                final boolean isArray = m.getReturnType().isArray() || List.class.isAssignableFrom(m.getReturnType());
                if (isArray) {
                    simpleName = simpleName.replaceFirst("\\[\\]$", "");
                }
                sb.append("var ").append(Utils.toMy(Utils.cleanUpClass(simpleName))).append(" = ");
                if (isArray) {
                    sb.append(" [] = ");
                }
            }
            if (cl == ScriptEnvironment.class) {
                sb.append(m.getName());
            } else {
                sb.append(Utils.toMy(Utils.cleanUpClass(cl.getSimpleName()))).append(".").append(m.getName());
            }
            sb.append("(");
            boolean first = true;
            int i = 0;
            for (Class<?> p : m.getParameterTypes()) {
                if (!first) {
                    sb.append(", ");
                }
                first = false;
                sb.append(Utils.toMy(p.getSimpleName()));
                if (ann != null && ann.parameters().length == m.getParameterTypes().length && !StringUtils.isEmpty(ann.parameters()[i])) {
                    sb.append("/*").append(ann.parameters()[i]).append("*/");
                }
                i++;
            }
            sb.append(");");
            if (ann != null && StringUtils.isNotEmpty(ann.description())) {
                sb.append("/*").append(ann.description()).append("*/");
            }
            if (isDeprecated) {
                sb.append("/*DEPRECATED, this method may be removed in future version*/");
            }
            sb.append("\r\n");
            if (ann != null && StringUtils.isNotEmpty(ann.example())) {
                sb.append("/* Example: */");
                sb.append(ann.example());
                sb.append("\r\n");
            }
        }
    }

    @ScriptAPI(description = "Set the Speedlimit in bytes/second. Values<=0 -> Disable Limiter", parameters = { "speedlimit in bytes/second" })
    public static void setSpeedlimit(int bps) throws EnvironmentException {
        if (bps > 0) {
            CFG_GENERAL.DOWNLOAD_SPEED_LIMIT.setValue(bps);
            CFG_GENERAL.DOWNLOAD_SPEED_LIMIT_ENABLED.setValue(true);
        } else {
            CFG_GENERAL.DOWNLOAD_SPEED_LIMIT_ENABLED.setValue(false);
        }
    }

    @ScriptAPI(description = "Get a DownloadList Link by it's uuid", parameters = { "uuid" })
    public static DownloadLinkSandBox getDownloadLinkByUUID(long uuid) throws EnvironmentException {
        try {
            final DownloadLink link = DownloadController.getInstance().getLinkByID(uuid);
            if (link != null) {
                return new DownloadLinkSandBox(link);
            } else {
                return null;
            }
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Get a CrawledLink Link by it's uuid", parameters = { "uuid" })
    public static CrawledLinkSandbox getCrawledLinkByUUID(long uuid) throws EnvironmentException {
        try {
            final CrawledLink link = LinkCollector.getInstance().getLinkByID(uuid);
            if (link != null) {
                return new CrawledLinkSandbox(link);
            } else {
                return null;
            }
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Get a DownloadList Package by it's uuid", parameters = { "uuid" })
    public static FilePackageSandBox getDownloadPackageByUUID(long uuid) throws EnvironmentException {
        try {
            final FilePackage pkg = DownloadController.getInstance().getPackageByID(uuid);
            if (pkg != null) {
                return new FilePackageSandBox(pkg);
            } else {
                return null;
            }
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Get a CrawledLink Package by it's uuid", parameters = { "uuid" })
    public static CrawledPackageSandbox getCrawledPackageByUUID(long uuid) throws EnvironmentException {
        try {
            final CrawledPackage pkg = LinkCollector.getInstance().getPackageByID(uuid);
            if (pkg != null) {
                return new CrawledPackageSandbox(pkg);
            } else {
                return null;
            }
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Get a FilePath Object", parameters = { "Path to a file or folder" })
    public static FilePathSandbox getPath(String fileOrUrl) {
        if (Application.getJavaVersion() >= Application.JAVA17) {
            return new FilePathSandbox17(fileOrUrl);
        } else {
            return new FilePathSandbox(fileOrUrl);
        }
    }

    @ScriptAPI(description = "Get the current path separator / or \\")
    public static String getPathSeparator() {
        return File.separator;
    }

    @ScriptAPI(description = "Gets the value of the specified environment variable", parameters = { "environment variable" })
    public static String getEnv(final String variable) throws EnvironmentException {
        try {
            return System.getenv(variable);
        } catch (SecurityException e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Gets the system property indicated by the specified key.", parameters = { "system property key", "default value" })
    public static String getProperty(final String key, final String defaultValue) throws EnvironmentException {
        try {
            return System.getProperty(key, defaultValue);
        } catch (SecurityException e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Get an Environment Object")
    public static EnvironmentSandbox getEnvironment() throws EnvironmentException {
        return new EnvironmentSandbox();
    }

    @ScriptAPI(description = "Get an Browser Object")
    public static BrowserSandBox getBrowser() throws EnvironmentException {
        askForPermission("load resources from the internet");
        return new BrowserSandBox();
    }

    @ScriptAPI(description = "Open a website or path in your default browser/file explorer", parameters = { "URL" }, example = "openURL(\"https://jdownloader.org\");")
    public static void openURL(String url) throws EnvironmentException {
        askForPermission("open resource in your default browser/file explorer");
        try {
            CrossSystem.openURL(url);
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Loads a website (Method: GET) and returns the source code", parameters = { "URL" }, example = "var myhtmlSourceString=getPage(\"https://jdownloader.org\");")
    public static String getPage(String fileOrUrl) throws EnvironmentException {
        final BrowserSandBox browser = getBrowser();
        try {
            return browser.getPage(fileOrUrl);
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Get a Property. Set global to true if you want to access a global property", parameters = { "\"key\"", "global(boolean)" }, example = "var value=getProperty(\"myobject\", false);")
    public static Object getProperty(String key, boolean global) throws EnvironmentException {
        try {
            if (global) {
                return PackagizerController.getGlobalProperty(key);
            } else {
                synchronized (SCRIPT_PROPERTIES) {
                    final HashMap<String, Object> store = SCRIPT_PROPERTIES.get(getScriptThread().getScript());
                    if (store == null) {
                        return null;
                    }
                    return store.get(key);
                }
            }
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    private final static WeakHashMap<ReentrantReadWriteLock, String> LOCKS = new WeakHashMap<ReentrantReadWriteLock, String>();

    @ScriptAPI(description = "Get a ModifyLock.", parameters = { "\"key\"" }, example = "var lock=getModifyLock(\"lockID\");")
    public static ModifyLockSandBox getModifyLock(final String id) {
        synchronized (LOCKS) {
            final ScriptThread thread = (ScriptThread) Thread.currentThread();
            for (Entry<ReentrantReadWriteLock, String> lockEntry : LOCKS.entrySet()) {
                if (StringUtils.equals(lockEntry.getValue(), id)) {
                    final ReentrantReadWriteLock lock = lockEntry.getKey();
                    thread.addLock(lock);
                    return new ModifyLockSandBox(new ModifyLock(lock));
                }
            }
            final ReentrantReadWriteLock lock = new ReentrantReadWriteLock();
            thread.addLock(lock);
            LOCKS.put(lock, id);
            return new ModifyLockSandBox(new ModifyLock(lock));
        }
    }

    public static Collection<Class<?>> getRequiredClasses() {
        final ArraySet<Class<?>> clazzes = new ArraySet<Class<?>>();
        collectClasses(ScriptEnvironment.class, clazzes);
        if (Application.getJavaVersion() >= Application.JAVA17) {
            clazzes.add(FilePathSandbox17.class);
            collectClasses(FilePathSandbox17.class, clazzes);
        }
        return clazzes;
    }

    @ScriptAPI(description = "Get current timestamp in ms")
    public static long getCurrentTimeStamp() {
        return System.currentTimeMillis();
    }

    @ScriptAPI(description = "Get a list of all running downloadlinks")
    public static DownloadLinkSandBox[] getRunningDownloadLinks() {
        final Set<SingleDownloadController> list = DownloadWatchDog.getInstance().getRunningDownloadLinks();
        final DownloadLinkSandBox[] ret = new DownloadLinkSandBox[list.size()];
        int i = 0;
        for (final SingleDownloadController dlc : list) {
            ret[i++] = new DownloadLinkSandBox(dlc.getDownloadLink());
        }
        return ret;
    }

    @ScriptAPI(description = "Get a list of all packages")
    public static FilePackageSandBox[] getAllFilePackages() {
        final List<FilePackage> list = DownloadController.getInstance().getPackagesCopy();
        final FilePackageSandBox[] ret = new FilePackageSandBox[list.size()];
        int i = 0;
        for (final FilePackage pkg : list) {
            ret[i++] = new FilePackageSandBox(pkg);
        }
        return ret;
    }

    @ScriptAPI(description = "Create a Checksum for a file. Types: e.g. CRC32, MD5, SHA-1, SHA-256")
    public static String getChecksum(String type, String path) throws EnvironmentException {
        askForPermission("Create Checksum of local file");
        try {
            File rel = new File(path);
            if (!rel.isAbsolute()) {
                rel = Application.getResource(path);
            }
            if (StringUtils.equalsIgnoreCase("CRC32", type)) {
                return String.valueOf(Hash.getCRC32(rel));
            } else {
                return Hash.getFileHash(rel, type);
            }
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Create a Checksum for a String. Types: e.g. CRC32, MD5, SHA-1, SHA-256")
    public static String getStringChecksum(String type, String string) throws EnvironmentException {
        try {
            if (string == null) {
                return null;
            } else {
                final byte[] bytes = string.getBytes("UTF-8");
                if (StringUtils.equalsIgnoreCase("CRC32", type)) {
                    return String.valueOf(Hash.getCRC32(bytes));
                } else {
                    return Hash.getHash(new ByteArrayInputStream(bytes), type, -1, true);
                }
            }
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Get a list of all crawledpackages")
    public static CrawledPackageSandbox[] getAllCrawledPackages() {
        final List<CrawledPackage> list = LinkCollector.getInstance().getPackagesCopy();
        final CrawledPackageSandbox[] ret = new CrawledPackageSandbox[list.size()];
        int i = 0;
        for (final CrawledPackage pkg : list) {
            ret[i++] = new CrawledPackageSandbox(pkg);
        }
        return ret;
    }

    @ScriptAPI(description = "Remove a downloadlink by uuid")
    public static boolean removeDownloadLinkByUUID(final String uuid) {
        final DownloadLink link = DownloadController.getInstance().getLinkByID(Long.parseLong(uuid));
        return link != null && new DownloadLinkSandBox(link).remove();
    }

    @ScriptAPI(description = "Remove a package by uuid")
    public static boolean removeFilePackageByUUID(final String uuid) {
        final FilePackage pkg = DownloadController.getInstance().getPackageByID(Long.parseLong(uuid));
        return pkg != null && new FilePackageSandBox(pkg).remove();
    }

    @ScriptAPI(description = "Remove a crawledlink by uuid")
    public static boolean removeCrawledLinkByUUID(final String uuid) {
        final CrawledLink link = LinkCollector.getInstance().getLinkByID(Long.parseLong(uuid));
        return link != null && new CrawledLinkSandbox(link).remove();
    }

    @ScriptAPI(description = "Remove a crawledpackage by uuid")
    public static boolean removeCrawledPackageByUUID(final String uuid) {
        final CrawledPackage pkg = LinkCollector.getInstance().getPackageByID(Long.parseLong(uuid));
        return pkg != null && new CrawledPackageSandbox(pkg).remove();
    }

    @ScriptAPI(description = "Get a list of all downloadlinks")
    public static DownloadLinkSandBox[] getAllDownloadLinks() {
        final List<DownloadLink> links = DownloadController.getInstance().getAllChildren();
        final DownloadLinkSandBox[] ret = new DownloadLinkSandBox[links.size()];
        int i = 0;
        for (final DownloadLink link : links) {
            ret[i++] = new DownloadLinkSandBox(link);
        }
        return ret;
    }

    @ScriptAPI(description = "Get a list of all crawledlinks")
    public static CrawledLinkSandbox[] getAllCrawledLinks() {
        final List<CrawledLink> links = LinkCollector.getInstance().getAllChildren();
        final CrawledLinkSandbox[] ret = new CrawledLinkSandbox[links.size()];
        int i = 0;
        for (final CrawledLink link : links) {
            ret[i++] = new CrawledLinkSandbox(link);
        }
        return ret;
    }

    @ScriptAPI(description = "Get a list of all running packages")
    public static FilePackageSandBox[] getRunningDownloadPackages() {
        final Set<FilePackage> list = DownloadWatchDog.getInstance().getRunningFilePackages();
        final FilePackageSandBox[] ret = new FilePackageSandBox[list.size()];
        int i = 0;
        for (final FilePackage dlc : list) {
            ret[i++] = new FilePackageSandBox(dlc);
        }
        return ret;
    }

    private static ScriptThread getScriptThread() {
        final Thread ct = Thread.currentThread();
        if (ct instanceof ScriptThread) {
            return (ScriptThread) ct;
        } else if (ct instanceof ScriptReferenceThread) {
            return ((ScriptReferenceThread) ct).getScriptThread();
        } else {
            throw new IllegalStateException();
        }
    }

    @ScriptAPI(description = "Get current total Download Speed in bytes/second")
    public static long getTotalSpeed() {
        return DownloadWatchDog.getInstance().getDownloadSpeedManager().getSpeed();
    }

    @ScriptAPI(description = "Get current average Download Speed in bytes/second")
    public static long getAverageSpeed() {
        return DownloadWatchDog.getInstance().getDownloadSpeedManager().getSpeedMeter().getValue(Resolution.SECONDS);
    }

    @ScriptAPI(description = "Stop Downloads")
    public static void stopDownloads() {
        DownloadWatchDog.getInstance().stopDownloads();
    }

    @ScriptAPI(description = "Start Downloads")
    public static void startDownloads() {
        DownloadWatchDog.getInstance().startDownloads();
    }

    @ScriptAPI(description = "Pause/Unpause Downloads")
    public static void setDownloadsPaused(boolean paused) {
        DownloadWatchDog.getInstance().pauseDownloadWatchDog(paused);
    }

    @ScriptAPI(description = "Check if Download Controller is in IDLE State")
    public static boolean isDownloadControllerIdle() {
        return DownloadWatchDog.getInstance().isIdle();
    }

    @ScriptAPI(description = "Check if Download Controller is in PAUSE State")
    public static boolean isDownloadControllerPaused() {
        return DownloadWatchDog.getInstance().isPaused();
    }

    @ScriptAPI(description = "Check if Download Controller is in RUNNING State")
    public static boolean isDownloadControllerRunning() {
        return DownloadWatchDog.getInstance().isRunning();
    }

    @ScriptAPI(description = "Check if Download Controller is in STOPPING State (Still running, but stop has been pressed)")
    public static boolean isDownloadControllerStopping() {
        return DownloadWatchDog.getInstance().isStopping();
    }

    @ScriptAPI(description = "Log to stderr and to JDownloader Log Files")
    public static void log(Object... objects) {
        if (objects != null && objects.length == 1) {
            if (Clazz.isPrimitiveWrapper(objects[0].getClass()) || Clazz.isString(objects[0].getClass())) {
                LOGGER.info(objects[0] + "");
                return;
            } else if (objects.length > 0) {
                if (objects[0] != null && objects[0].getClass().getPackage().getName().equals(DownloadLinkSandBox.class.getPackage().getName())) {
                    LOGGER.info(objects[0].toString());
                    return;
                }
                try {
                    try {
                        LOGGER.info(JSonStorage.serializeToJson(objects[0]));
                    } catch (Throwable e) {
                        LOGGER.info(format(toJson(objects[0])));
                    }
                } catch (Throwable e) {
                    LOGGER.info(objects[0] + "");
                }
                return;
            }
        }
        try {
            try {
                LOGGER.info(JSonStorage.serializeToJson(objects));
            } catch (Throwable e) {
                LOGGER.info(format(toJson(objects)));
            }
        } catch (Throwable e) {
            LOGGER.info(objects + "");
        }
        return;
    }

    @ScriptAPI(description = "Play a Wav Audio file", parameters = { "myFilePathOrUrl" }, example = "playWavAudio(JD_HOME+\"/themes/standard/org/jdownloader/sounds/captcha.wav\");")
    public static void playWavAudio(String fileOrUrl) throws EnvironmentException {
        try {
            AudioInputStream stream = null;
            Clip clip = null;
            try {
                stream = AudioSystem.getAudioInputStream(new File(fileOrUrl));
                final AudioFormat format = stream.getFormat();
                final DataLine.Info info = new DataLine.Info(Clip.class, format);
                if (AudioSystem.isLineSupported(info)) {
                    clip = (Clip) AudioSystem.getLine(info);
                    clip.open(stream);
                    try {
                        final FloatControl gainControl = (FloatControl) clip.getControl(FloatControl.Type.MASTER_GAIN);
                        float db = (20f * (float) Math.log(JsonConfig.create(SoundSettings.class).getCaptchaSoundVolume() / 100f));
                        gainControl.setValue(Math.max(-80f, db));
                        BooleanControl muteControl = (BooleanControl) clip.getControl(BooleanControl.Type.MUTE);
                        muteControl.setValue(true);
                        muteControl.setValue(false);
                    } catch (Exception e) {
                        org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
                    }
                    final AtomicBoolean runningFlag = new AtomicBoolean(true);
                    clip.addLineListener(new LineListener() {
                        @Override
                        public void update(LineEvent event) {
                            if (event.getType() == Type.STOP) {
                                runningFlag.set(false);
                            }
                        }
                    });
                    clip.start();
                    Thread.sleep(1000);
                    while (clip.isRunning() && runningFlag.get()) {
                        Thread.sleep(100);
                    }
                }
            } finally {
                try {
                    if (clip != null) {
                        final Clip finalClip = clip;
                        Thread thread = new Thread() {
                            public void run() {
                                finalClip.close();
                            };
                        };
                        thread.setName("AudioStop");
                        thread.setDaemon(true);
                        thread.start();
                        thread.join(2000);
                    }
                } catch (Throwable e) {
                }
                try {
                    if (stream != null) {
                        stream.close();
                    }
                } catch (Throwable e) {
                }
            }
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Loads a website (METHOD: POST) and returns the source code", parameters = { "URL", "PostData" }, example = "var myhtmlSourceString=postPage(\"https://support.jdownloader.org/index.php\",\"searchquery=captcha\");")
    public static String postPage(String url, String post) throws EnvironmentException {
        final BrowserSandBox browser = getBrowser();
        try {
            return browser.postPage(url, post);
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Read a text file", parameters = { "filepath" }, example = "var myString=readFile(JD_HOME+\"/license.txt\");")
    public static String readFile(String filepath) throws EnvironmentException {
        askForPermission("read a local file");
        try {
            final File file = new File(filepath);
            final Object lock = requestLock(file);
            try {
                synchronized (lock) {
                    return IO.readFileToString(file);
                }
            } finally {
                unLock(file);
            }
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Request a reconnect", parameters = {}, example = "requestReconnect();")
    public static void requestReconnect() throws EnvironmentException {
        try {
            DownloadWatchDog.getInstance().requestReconnect(false);
        } catch (InterruptedException e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Return current reconnect status", example = "var reconnectStatus=getReconnectState();")
    public static String getReconnectState() {
        final DownloadSession session = DownloadWatchDog.getInstance().getSession();
        if (session == null) {
            return null;
        }
        final SessionState state = session.getSessionState();
        switch (state) {
        case RECONNECT_REQUESTED:
        case RECONNECT_RUNNING:
            return state.name();
        default:
            return null;
        }
    }

    @ScriptAPI(description = "Refresh all premium accounts", parameters = { "true|false (Wait for account checks)", "true|false (Force Check)" }, example = "refreshAccounts(true,true);")
    public static void refreshAccounts(final boolean wait, final boolean force) throws EnvironmentException {
        final QueueAction<Void, InterruptedException> action = new QueueAction<Void, InterruptedException>() {
            @Override
            protected Void run() throws InterruptedException {
                final List<AccountCheckJob> jobs = new ArrayList<AccountCheckJob>();
                for (final Account acc : AccountController.getInstance().list()) {
                    if (acc.getPlugin() != null && acc.isEnabled() && acc.isValid()) {
                        final AccountCheckJob job = AccountChecker.getInstance().check(acc, force);
                        if (wait && job != null) {
                            jobs.add(job);
                        }
                    }
                }
                for (final AccountCheckJob job : jobs) {
                    while (!job.isChecked()) {
                        Thread.sleep(100);
                    }
                }
                return null;
            }
        };
        try {
            if (wait) {
                TaskQueue.getQueue().addWait(action);
            } else {
                TaskQueue.getQueue().add(action);
            }
        } catch (InterruptedException e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Perform a reconnect and wait for it", parameters = {}, example = "var success= doReconnect();")
    public static boolean doReconnect() throws EnvironmentException {
        try {
            return DownloadWatchDog.getInstance().requestReconnect(true) == Reconnecter.ReconnectResult.SUCCESSFUL;
        } catch (InterruptedException e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Perform a fake reconnect and optionally wait for it", parameters = { "true|false (Wait for fake reconnect)" }, example = "fakeReconnect(false);")
    public static void fakeReconnect(final boolean wait) throws EnvironmentException {
        final AtomicBoolean executedFlag = new AtomicBoolean(false);
        DownloadWatchDog.getInstance().enqueueJob(new DownloadWatchDogJob() {
            @Override
            public boolean isHighPriority() {
                return true;
            }

            @Override
            public void interrupt() {
            }

            @Override
            public void execute(DownloadSession currentSession) {
                try {
                    final ProxyInfoHistory proxyInfoHistory = currentSession.getProxyInfoHistory();
                    proxyInfoHistory.validate();
                    final List<WaitingSkipReasonContainer> reconnects = proxyInfoHistory.list(WaitingSkipReason.CAUSE.IP_BLOCKED, null);
                    if (reconnects != null) {
                        for (WaitingSkipReasonContainer reconnect : reconnects) {
                            if (reconnect.getProxySelector().isReconnectSupported()) {
                                reconnect.invalidate();
                            }
                        }
                    }
                } finally {
                    synchronized (executedFlag) {
                        executedFlag.set(true);
                        executedFlag.notifyAll();
                    }
                }
            }
        });
        if (wait) {
            try {
                synchronized (executedFlag) {
                    if (!executedFlag.get()) {
                        executedFlag.wait();
                    }
                }
            } catch (InterruptedException e) {
                throw new EnvironmentException(e);
            }
        }
    }

    @ScriptAPI(description = "Perform a sleep and wait for x milliseconds", parameters = { "milliseconds" }, example = "sleep(1000);")
    public static void sleep(int millis) throws EnvironmentException {
        try {
            if (millis > 0) {
                Thread.sleep(millis);
            }
        } catch (InterruptedException e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "(experimental) Get proxy list", parameters = { "" }, example = "experimental_proxylist();")
    /**
     * TODO: this implementation does only use toExportString method in ugle string result, no list, just plain string
     *
     * please rewrite to return proper json/sandbox ojects(preferred) in a list
     *
     * @return
     * @throws EnvironmentException
     */
    public static String experimental_proxylist() throws EnvironmentException {
        java.util.List<AbstractProxySelectorImpl> selectedObjects = jd.controlling.proxy.ProxyController.getInstance().getList();
        StringBuilder sb = new StringBuilder();
        for (AbstractProxySelectorImpl pi : selectedObjects) {
            String str = pi.toExportString();
            if (str == null) {
                continue;
            }
            if (sb.length() > 0) {
                sb.append("\r\n");
            }
            sb.append(str);
        }
        return sb.toString();
    }

    @ScriptAPI(description = "(experimental) Get proxy banlist", parameters = { "" }, example = "experimental_proxybanlist();")
    /**
     * TODO: this implementation does use getBanList().toString() which is neither stable nor anything usefull as the result contains
     * localized strings.
     *
     * please rewrite to return proper json/sandbox ojects(preferred) in a list
     *
     * @return
     * @throws EnvironmentException
     */
    public static String experimental_proxybanlist() throws EnvironmentException {
        java.util.List<AbstractProxySelectorImpl> selectedObjects = jd.controlling.proxy.ProxyController.getInstance().getList();
        StringBuilder sb = new StringBuilder();
        for (AbstractProxySelectorImpl pi : selectedObjects) {
            String str = pi.getBanList().toString();
            if (str == null || str == "" || str == "[]") {
                continue;
            }
            String str_temp = pi.toExportString();
            if (str_temp == null) {
                continue;
            }
            str = str_temp + ": " + str;
            if (sb.length() > 0) {
                sb.append("\r\n");
            }
            sb.append(str);
        }
        return sb.toString();
    }

    @ScriptAPI(description = "Loads a Javascript file or url. ATTENTION. The loaded script can access the API as well.", parameters = { "myFilePathOrUrl" }, example = "require(\"https://raw.githubusercontent.com/douglascrockford/JSON-js/master/json.js\");")
    public static void require(String fileOrUrl) throws EnvironmentException {
        askForPermission("load external JavaScript");
        try {
            getScriptThread().requireJavascript(fileOrUrl);
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Set a Property. This property will be available until JD-exit or a script overwrites it. if global is true, the property will be available for al scripts", parameters = { "\"key\"", "anyValue", "global(boolean)" }, example = "var oldValue=setProperty(\"myobject\", { \"name\": true}, false);")
    public static Object setProperty(String key, Object value, boolean global) throws EnvironmentException {
        try {
            if (global) {
                return PackagizerController.putGlobalProperty(key, value);
            } else {
                synchronized (SCRIPT_PROPERTIES) {
                    HashMap<String, Object> store = SCRIPT_PROPERTIES.get(getScriptThread().getScript());
                    if (store == null) {
                        store = new HashMap<String, Object>();
                        SCRIPT_PROPERTIES.put(getScriptThread().getScript(), store);
                    }
                    return store.put(key, value);
                }
            }

        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }

    @ScriptAPI(description = "Show a Confirm Dialog", parameters = { "message", "okOption", "cancelOption" }, example = "showConfirmDialog(\"Do you like this method?\",\"yes\",\"no\"")
    public static int showConfirmDialog(final String message, final String okOption, final String cancelOption) {
        final ScriptThread env = getScriptThread();
        final String id = T.T.showConfirmDialog_title(env.getScript().getName(), env.getScript().getEventTrigger().getLabel());
        final AtomicReference<Object> dialogReference = new AtomicReference<Object>();
        new Thread(id) {
            {
                setDaemon(true);
            }

            public void run() {
                try {
                    final ConfirmDialog confirmDialog = new ConfirmDialog(Dialog.STYLE_LARGE, id, message, new AbstractIcon(IconKey.ICON_QUESTION, 32), okOption, cancelOption) {
                        @Override
                        protected int getPreferredWidth() {
                            return 600;
                        }

                        @Override
                        public String getDontShowAgainKey() {
                            return null;
                        };

                        @Override
                        public boolean isRemoteAPIEnabled() {
                            return true;
                        }

                        @Override
                        protected void modifyTextPane(JTextPane textField) {
                        }

                        @Override
                        public ModalityType getModalityType() {
                            return ModalityType.MODELESS;
                        }

                        @Override
                        protected JTextComponent addMessageComponent(final MigPanel p) {
                            JTextPane textField = new JTextPane();
                            modifyTextPane(textField);
                            final Font font = textField.getFont();
                            if (BinaryLogic.containsAll(flagMask, Dialog.STYLE_HTML)) {
                                textField.setContentType("text/html");
                                textField.addHyperlinkListener(new HyperlinkListener() {
                                    public void hyperlinkUpdate(final HyperlinkEvent e) {
                                        if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
                                            CrossSystem.openURL(e.getURL());
                                        }
                                    }
                                });
                            } else {
                                textField.setContentType("text/plain");
                            }
                            textField.setFont(font);
                            textField.setText(getMessage());
                            if (env.isAdvancedAlert()) {
                                textField.setEditable(true);
                            } else {
                                textField.setEditable(false);
                            }
                            textField.setBackground(null);
                            textField.setOpaque(false);
                            if (env.isAdvancedAlert()) {
                                textField.setFocusable(true);
                            } else {
                                textField.setFocusable(false);
                            }
                            textField.setForeground(new JLabel().getForeground());
                            textField.putClientProperty("Synthetica.opaque", Boolean.FALSE);
                            textField.setCaretPosition(0);
                            if (BinaryLogic.containsAll(flagMask, Dialog.STYLE_LARGE)) {
                                p.add(new JScrollPane(textField), "pushx,growx");
                            } else {
                                p.add(textField);
                            }
                            return textField;
                        }

                        @Override
                        public void pack() {
                            getDialog().pack();
                        }
                    };
                    final ConfirmDialogInterface dialog = UIOManager.I().show(ConfirmDialogInterface.class, confirmDialog);
                    dialogReference.set(dialog);
                } finally {
                    synchronized (dialogReference) {
                        dialogReference.compareAndSet(null, Boolean.FALSE);
                        dialogReference.notifyAll();
                    }
                }
            };
        }.start();
        synchronized (dialogReference) {
            while (dialogReference.get() == null) {
                try {
                    dialogReference.wait();
                } catch (InterruptedException e) {
                    break;
                }
            }
        }
        if (dialogReference.get() instanceof ConfirmDialogInterface) {
            try {
                ((ConfirmDialogInterface) dialogReference.get()).throwCloseExceptions();
                return 1;
            } catch (DialogNoAnswerException e) {
            }
        }
        return 0;
    }

    private static void showMessageDialog(final String string) {
        final ScriptThread env = getScriptThread();
        final int flags;
        if (string != null && string.matches("<html>.+</html>")) {
            flags = UIOManager.BUTTONS_HIDE_CANCEL | Dialog.STYLE_LARGE | Dialog.STYLE_HTML;
        } else {
            flags = UIOManager.BUTTONS_HIDE_CANCEL | Dialog.STYLE_LARGE;
        }
        final String id = T.T.showMessageDialog_title(env.getScript().getName(), env.getScript().getEventTrigger().getLabel());
        new Thread(id) {
            {
                setDaemon(true);
            }

            public void run() {
                UIOManager.I().show(ConfirmDialogInterface.class, new ConfirmDialog(flags, id, string, new AbstractIcon(IconKey.ICON_INFO, 32), null, null) {
                    @Override
                    protected int getPreferredWidth() {
                        return 600;
                    }

                    @Override
                    public boolean isRemoteAPIEnabled() {
                        return false;
                    }

                    @Override
                    protected void modifyTextPane(JTextPane textField) {
                    }

                    @Override
                    public ModalityType getModalityType() {
                        return ModalityType.MODELESS;
                    }

                    @Override
                    protected JTextComponent addMessageComponent(final MigPanel p) {
                        JTextPane textField = new JTextPane();
                        modifyTextPane(textField);
                        final Font font = textField.getFont();
                        if (BinaryLogic.containsAll(flagMask, Dialog.STYLE_HTML)) {
                            textField.setContentType("text/html");
                            textField.addHyperlinkListener(new HyperlinkListener() {
                                public void hyperlinkUpdate(final HyperlinkEvent e) {
                                    if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
                                        CrossSystem.openURL(e.getURL());
                                    }
                                }
                            });
                        } else {
                            textField.setContentType("text/plain");
                        }
                        textField.setFont(font);
                        textField.setText(getMessage());
                        if (env.isAdvancedAlert()) {
                            textField.setEditable(true);
                        } else {
                            textField.setEditable(false);
                        }
                        textField.setBackground(null);
                        textField.setOpaque(false);
                        if (env.isAdvancedAlert()) {
                            textField.setFocusable(true);
                        } else {
                            textField.setFocusable(false);
                        }
                        textField.setForeground(new JLabel().getForeground());
                        textField.putClientProperty("Synthetica.opaque", Boolean.FALSE);
                        textField.setCaretPosition(0);
                        if (BinaryLogic.containsAll(flagMask, Dialog.STYLE_LARGE)) {
                            p.add(new JScrollPane(textField), "pushx,growx");
                        } else {
                            p.add(textField);
                        }
                        return textField;
                    }

                    @Override
                    public void pack() {
                        getDialog().pack();
                    }
                });
            };
        }.start();
    }

    public static Object toJSObject(Object ret) {
        final ScriptThread env = getScriptThread();
        // convert to javascript object
        final String js = "(function(){ return " + JSonStorage.serializeToJson(ret) + ";}());";
        return env.evalTrusted(js);
    }

    public static String toJson(Object ret) {
        final ScriptThread env = getScriptThread();
        // convert to javascript object
        final String js = "(function(){ return JSON.stringify(" + JSonStorage.serializeToJson(ret) + ");}());";
        return (String) env.evalTrusted(js);
    }

    private static final HashMap<File, AtomicInteger> FILE_LOCKS = new HashMap<File, AtomicInteger>();

    private static Object requestLock(File name) {
        synchronized (FILE_LOCKS) {
            final AtomicInteger existingLock = FILE_LOCKS.get(name);
            if (existingLock == null) {
                final AtomicInteger newLock = new AtomicInteger(1);
                FILE_LOCKS.put(name, newLock);
                return newLock;
            } else {
                existingLock.incrementAndGet();
                return existingLock;
            }
        }
    }

    private static void unLock(File name) {
        synchronized (FILE_LOCKS) {
            final AtomicInteger existingLock = FILE_LOCKS.get(name);
            if (existingLock != null && existingLock.decrementAndGet() == 0) {
                FILE_LOCKS.remove(name);
            }
        }
    }

    @ScriptAPI(description = "Write a text file", parameters = { "filepath", "myText", "append" }, example = "writeFile(JD_HOME+\"/log.txt\",JSON.stringify(this)+\"\\r\\n\",true);")
    public static void writeFile(String filepath, String string, boolean append) throws EnvironmentException {
        askForPermission("create a local file and write to it");
        try {
            final File file = new File(filepath);
            final Object lock = requestLock(file);
            try {
                synchronized (lock) {
                    IO.writeStringToFile(file, string, append);
                }
            } finally {
                unLock(file);
            }
        } catch (Throwable e) {
            throw new EnvironmentException(e);
        }
    }
}