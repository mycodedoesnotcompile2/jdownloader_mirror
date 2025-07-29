package org.jdownloader.scripting;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArraySet;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.StringUtils;
import org.jdownloader.logging.LogController;
import org.mozilla.javascript.Callable;
import org.mozilla.javascript.ClassShutter;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.ContextFactory;
import org.mozilla.javascript.EcmaError;
import org.mozilla.javascript.NativeJavaObject;
import org.mozilla.javascript.Script;
import org.mozilla.javascript.ScriptRuntime;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.WrapFactory;
import org.mozilla.javascript.tools.shell.Global;

/**
 * from http://codeutopia.net/blog/2009/01/02/sandboxing-rhino-in-java/
 *
 * ============================================================================ =========
 *
 * Sandboxing Rhino in Java January 2, 2009 – 12:07 am Tags: Java, JavaScript, Security I’ve been working on a Java app which needed Rhino
 * for scripting. The app would need to run untrusted JavaScript code from 3rd parties, so I had to find a way to block access to all Java
 * methods, except the ones I wanted. This would not be a problem if there was an easy way to disable LiveConnect - the feature of Rhino
 * which provides java access to scripts - but there is no such thing.
 *
 * However, after a lot of digging around, I finally found a way to do this without too much hacking. In fact, it can be done by just
 * extending a few of the Rhino classes, and using the setters provided to override some of the default ones.
 *
 *
 * ClassShutter
 *
 * Let’s first look at the ClassShutter, which can be used to restrict access to Java packages and classes.
 *
 * //cx is the Context instance you're using to run scripts cx.setClassShutter(new ClassShutter() { public boolean visibleToScripts(String
 * className) { if(className.startsWith("adapter")) return true;
 *
 * return false; } }); The above will effectively disable access to all Java classes onwards from the point where the shutter was set.
 * However, if you run any scripts before setting the shutter, classes accessed there can still be used! You can use this to your advantage,
 * for example to provide specific classes in the scripts under different names or such.
 *
 * You probably noticed the comparison to “adapter” in the shutter. This is for when you implement interfaces or extend Java classes. Rhino
 * will create new classes based on those interfaces/classes, and they will be called adapterN, where N is a number. If you block access to
 * classes starting with adapter, you can’t implement or extend, and my use-case required that.
 *
 * However, there is a limitation in the ClassShutter…
 *
 * Reflection
 *
 * As you may know, you can use someInstance.getClass().forName(”some.package.Class”).newInstance() to get a new instance of
 * some.package.Class.
 *
 * This will not get blocked by the ClassShutter! We need to disable access to getClass() to block this.
 *
 * While the ClassShutter is relatively well documented, doing this required more research. A post in the Rhino mailing list finally pushed
 * me to the right direction: Overriding certain NativeJavaObject methods and creating a custom
 * sun.org.mozilla.javascript.internal.ContextFactory and WrapFactory for that.
 *
 * Here is an extended NativeJavaObject, which blocks access to getClass. You could use this approach to block access to other methods too:
 *
 * public static class SandboxNativeJavaObject extends NativeJavaObject { public SandboxNativeJavaObject(Scriptable scope, Object
 * javaObject, Class staticType) { super(scope, javaObject, staticType); }
 *
 * @Override public Object get(String name, Scriptable start) { if (name.equals("getClass")) { return NOT_FOUND; }
 *
 *           return super.get(name, start); } } To make the above class work, you need two more classes:
 *
 *           A WrapFactory which returns our SandboxNativeJavaObject’s
 *
 *           public static class SandboxWrapFactory extends WrapFactory {
 * @Override public Scriptable wrapAsJavaObject(Context cx, Scriptable scope, Object javaObject, Class staticType) { return new
 *           SandboxNativeJavaObject(scope, javaObject, staticType); } } And a sun.org.mozilla.javascript.internal.ContextFactory, which
 *           returns Context’s which use SandboxWrapFactory:
 *
 *           public class SandboxContextFactory extends sun.org.mozilla.javascript.internal.ContextFactory {
 * @Override protected Context makeContext() { Context cx = super.makeContext(); cx.setWrapFactory(new SandboxWrapFactory()); return cx; } }
 *           Finally, to make all this work, we need to tell Rhino the global sun.org.mozilla.javascript.internal.ContextFactory:
 *
 *           sun.org.mozilla.javascript.internal.ContextFactory.initGlobal(new SandboxContextFactory()); With this, we are done. Now, when
 *           you use sun.org.mozilla.javascript.internal.ContextFactory.getGlobal().enterContext(), you will get sandboxing contexts. But
 *           why did we need to set it globally? This is because it would appear that certain things, such as the adapter classes, use the
 *           global context factory to get some context for themselves, and without setting the global factory, they would get unlimited
 *           access.
 *
 *           In closing
 *
 *           I hope this is useful for someone. It took me a long time to figure it all out, so here it is now, all documented in one place.
 *           =)
 *
 *           The mailing list post where I found the direction for blocking getClass can be found here. Thanks to Charles Lowell.
 *
 *           There is also the SecurityController, which may be useful in further securing the class.
 *
 *           And as a final warning, while this approach works for me, and I haven’t yet found any way to get past the sandboxing and into
 *           Java-land… but there may be a way, and if you find one, do let me know.
 *
 *
 *
 *           ================================================================== == =================
 * @author thomas
 *
 */
public class JSRhinoPermissionRestricter {
    public static class SandboxException extends RuntimeException {
        /**
         *
         */
        private static final long serialVersionUID = 1L;
        private final Context     cx;

        public Context getContext() {
            return cx;
        }

        public Thread getThread() {
            return thread;
        }

        public String getClassName() {
            return className;
        }

        private final Thread thread;
        private final String className;

        public SandboxException(Context cx, Thread thread, String className, String message) {
            super(message);
            this.cx = cx;
            this.thread = thread;
            this.className = className;
        }
    }

    static public class SandboxContextFactory extends ContextFactory {
        static public class MyContext extends Context {
            private MyContext(ContextFactory factory) {
                super(factory);
            }
        }

        protected void observeInstructionCount(Context cx, int instructionCount) {
            // final MyContext mcx = (MyContext) cx;
            // final long currentTime = System.currentTimeMillis();
            // if (currentTime - mcx.startTime > 10 * 1000) {
            // // More then 10 seconds from Context creation time:
            // // it is time to stop the script.
            // // Throw Error instance to ensure that script will never
            // // get control back through catch or finally.
            // throw new Error();
            // }
        }

        protected Object doTopCall(Callable callable, Context cx, Scriptable scope, Scriptable thisObj, Object[] args) {
            // final MyContext mcx = (MyContext) cx;
            // mcx.startTime = System.currentTimeMillis();
            return super.doTopCall(callable, cx, scope, thisObj, args);
        }

        @Override
        protected Context makeContext() {
            final MyContext cx = new MyContext(SandboxContextFactory.this);
            try {
                final Field field = Context.class.getDeclaredField("factory");
                field.setAccessible(true);
                if (field.get(cx) != SandboxContextFactory.this) {
                    field.set(cx, SandboxContextFactory.this);
                    field.setAccessible(false);
                }
            } catch (Throwable e) {
                throw new WTFException(e);
            }
            // cx.setInstructionObserverThreshold(10000);
            cx.setWrapFactory(new SandboxWrapFactory());
            cx.setClassShutter(new ClassShutter() {
                public boolean visibleToScripts(final String className) {
                    final Thread thread = Thread.currentThread();
                    final boolean threadTrusted = TRUSTED_THREAD.containsKey(thread);
                    Boolean jsShutterResult = null;
                    if (thread instanceof JSShutterDelegate) {
                        if (((JSShutterDelegate) thread).isClassVisibleToScript(threadTrusted, className)) {
                            jsShutterResult = true;
                        } else {
                            jsShutterResult = false;
                        }
                    }
                    final JSShutterDelegate jsShutter = THREAD_JSSHUTTER.get(thread);
                    if (jsShutter != null) {
                        if (jsShutter.isClassVisibleToScript(threadTrusted, className)) {
                            jsShutterResult = true;
                        } else {
                            jsShutterResult = false;
                        }
                    }
                    if (jsShutterResult != null) {
                        if (jsShutterResult.booleanValue()) {
                            LOADED.add(className);
                        }
                        return jsShutterResult;
                    }
                    if (threadTrusted) {
                        LOADED.add(className);
                        LogController.CL().severe("Trusted Thread Loads: " + className + "|Thread:" + thread);
                        return true;
                    } else if (className.startsWith("adapter")) {
                        LOADED.add(className);
                        return true;
                    } else if (className.startsWith("org.mozilla.javascript.ConsString")) {
                        LOADED.add(className);
                        return true;
                    } else if (className.startsWith("org.mozilla.javascript.JavaScriptException")) {
                        LOADED.add(className);
                        return true;
                    } else if (className.startsWith("org.mozilla.javascript.EvaluatorException")) {
                        LOADED.add(className);
                        return true;
                    } else if (className.equals("org.mozilla.javascript.EcmaError")) {
                        LOADED.add(className);
                        LogController.CL().severe("Javascript error occured");
                        return true;
                    } else {
                        if (false) {
                            final EcmaError ret = ScriptRuntime.constructError("Security Violation", "Security Violation " + className);
                            throw ret;
                        } else {
                            throw new SandboxException(cx, thread, className, "Security Violation:" + className + "|Thread:" + thread);
                        }
                    }
                }
            });
            return cx;
        }
    }

    public static class SandboxWrapFactory extends WrapFactory {
        @SuppressWarnings("rawtypes")
        @Override
        public Scriptable wrapAsJavaObject(Context cx, Scriptable scope, Object javaObject, Class staticType) {
            if (javaObject instanceof Map) {
                return new SandboxNativeJavaMapWrapper(scope, javaObject);
            }
            if (javaObject instanceof EcmaError) {
                LogController.CL().log((Exception) javaObject);
            }
            return new SandboxNativeJavaObject(scope, javaObject, staticType);
        }
    }

    public static class SandboxNativeJavaMapWrapper extends NativeJavaObject {

        private static final long         serialVersionUID = -3786257752907047381L;

        private final Map<Object, Object> map;

        @SuppressWarnings("unchecked")
        public SandboxNativeJavaMapWrapper(Scriptable scope, Object map) {
            super(scope, map, map.getClass());
            assert map instanceof Map;
            this.map = (Map<Object, Object>) map;
        }

        @Override
        public String getClassName() {
            return "JavaMap";
        }

        @Override
        public boolean has(String name, Scriptable start) {
            final Context cx = Context.getCurrentContext();
            if (cx != null) {
                if (map.containsKey(name)) {
                    return true;
                }
            }
            return super.has(name, start);
        }

        @Override
        public boolean has(int index, Scriptable start) {
            final Context cx = Context.getCurrentContext();
            if (cx != null) {
                if (map.containsKey(Integer.valueOf(index))) {
                    return true;
                }
            }
            return super.has(index, start);
        }

        @Override
        public Object get(String name, Scriptable start) {
            if (name.equals("getClass")) {
                LogController.CL().severe("JS Security Exception:" + name + "|" + start);
                return NOT_FOUND;
            }
            final Context cx = Context.getCurrentContext();
            if (cx != null) {
                if (map.containsKey(name)) {
                    final Object obj = map.get(name);
                    return cx.getWrapFactory().wrap(cx, this, obj, obj == null ? null : obj.getClass());
                }
            }
            return super.get(name, start);
        }

        @Override
        public Object get(int index, Scriptable start) {
            final Context cx = Context.getCurrentContext();
            if (cx != null) {
                if (map.containsKey(Integer.valueOf(index))) {
                    final Object obj = map.get(Integer.valueOf(index));
                    return cx.getWrapFactory().wrap(cx, this, obj, obj == null ? null : obj.getClass());
                }
            }
            return super.get(index, start);
        }

        @Override
        public void put(String name, Scriptable start, Object value) {
            final Context cx = Context.getCurrentContext();
            if (cx != null) {
                map.put(name, Context.jsToJava(value, Object.class));
            } else {
                super.put(name, start, value);
            }
        }

        @Override
        public void put(int index, Scriptable start, Object value) {
            final Context cx = Context.getCurrentContext();
            if (cx != null) {
                map.put(Integer.valueOf(index), Context.jsToJava(value, Object.class));
            } else {
                super.put(index, start, value);
            }
        }

        @Override
        public Object[] getIds() {
            final Context cx = Context.getCurrentContext();
            if (cx != null) {
                final List<Object> ids = new ArrayList<Object>(map.size());
                for (Object key : map.keySet()) {
                    if (key instanceof Integer) {
                        ids.add(key);
                    } else {
                        ids.add(ScriptRuntime.toString(key));
                    }
                }
                return ids.toArray();
            }
            return super.getIds();
        }

        @Override
        public boolean equals(Object obj) {
            return super.equals(obj);
        }

        @Override
        public int hashCode() {
            return super.hashCode();
        }

    }

    public static class SandboxNativeJavaObject extends NativeJavaObject {
        private static final long serialVersionUID = -2783084485265910840L;

        public SandboxNativeJavaObject(Scriptable scope, Object javaObject, Class<?> staticType) {
            super(scope, javaObject, staticType);
        }

        @Override
        public Object get(String name, Scriptable start) {
            if (name.equals("getClass")) {
                LogController.CL().severe("JS Security Exception:" + name + "|" + start);
                return NOT_FOUND;
            } else {
                return super.get(name, start);
            }
        }
    }

    public static final ConcurrentHashMap<Thread, Boolean>           TRUSTED_THREAD   = new ConcurrentHashMap<Thread, Boolean>();
    public static final ConcurrentHashMap<Thread, JSShutterDelegate> THREAD_JSSHUTTER = new ConcurrentHashMap<Thread, JSShutterDelegate>();

    public static Object evaluateTrustedString(Context cx, Global scope, String source, String sourceName, int lineno, Object securityDomain) {
        final Thread thread = Thread.currentThread();
        try {
            TRUSTED_THREAD.put(thread, true);
            return cx.evaluateString(scope, source, sourceName, lineno, securityDomain);
        } finally {
            TRUSTED_THREAD.remove(thread);
        }
    }

    public static Script compileTrustedString(Context cx, Global scope, String source, String sourceName, int lineno, Object securityDomain) {
        final Thread thread = Thread.currentThread();
        try {
            TRUSTED_THREAD.put(thread, true);
            return cx.compileString(source, sourceName, lineno, securityDomain);
        } finally {
            TRUSTED_THREAD.remove(thread);
        }
    }

    private static final CopyOnWriteArraySet<String> LOADED = new CopyOnWriteArraySet<String>();

    public static List<String> getLoaded() {
        final List<String> ret = new ArrayList<String>(LOADED);
        Collections.sort(ret, new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return o2.length() - o1.length();
            }
        });
        return ret;
    }

    public static void init() {
        try {
            ContextFactory.initGlobal(new SandboxContextFactory());
            // let's do a test
            try {
                try {
                    final Context cx = ContextFactory.getGlobal().enterContext();
                    cx.evaluateString(cx.initStandardObjects(), "java.lang.System.out.println('TEST')", "<cmd>", 1, null);
                } finally {
                    Context.exit();
                }
                throw new SecurityException("Could not install the sun.org.mozilla.javascript.internal Sandbox!");
            } catch (SandboxException e) {
                if (!StringUtils.startsWithCaseInsensitive(e.getMessage(), "Security Violation:org")) {
                    throw e;
                } else {
                    // test successfull. Security Sandbox successfully initialized
                }
            }
        } catch (Throwable e) {
            LogController.CL().log(e);
        }
    }
}
