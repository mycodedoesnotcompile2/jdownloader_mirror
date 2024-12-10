/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2024, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils;

import java.awt.GraphicsEnvironment;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileDescriptor;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.lang.management.LockInfo;
import java.lang.management.ManagementFactory;
import java.lang.management.MonitorInfo;
import java.lang.management.ThreadInfo;
import java.lang.management.ThreadMXBean;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.Charset;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicReference;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.appwork.builddecision.BuildDecisions;
import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.os.CrossSystem;

/**
 * Application utils provide status helper functions concerning the applications System integration
 *
 * @author $Author: unknown$
 *
 */
public class Application {
    /**
     *
     */
    private static Boolean              IS_JARED   = null;
    private static String               APP_FOLDER = ".appwork";
    private static String               ROOT;
    public final static long            JAVA16     = JVMVersion.JAVA16;
    public final static long            JAVA17     = JVMVersion.JAVA17;
    public final static long            JAVA18     = JVMVersion.JAVA18;
    public final static long            JAVA19     = JVMVersion.JAVA19;
    private static Boolean              JVM64BIT   = null;
    private static boolean              REDIRECTED = false;
    public static PauseableOutputStream STD_OUT;
    public static PauseableOutputStream ERR_OUT;
    private static boolean              DID_INIT   = false;
    private static PrintStream          ORG_STD_OUT;

    public static PrintStream getSystemStdOut() {
        return ORG_STD_OUT;
    }

    public static PrintStream getSystemStdErr() {
        return ORG_STD_ERR;
    }

    public static PrintStream getWrappedStdOut() {
        return REDIRECTED_STD_OUT;
    }

    public static PrintStream getWrappedStdErr() {
        return REDIRECTED_STD_ERR;
    }

    private static PrintStream ORG_STD_ERR;
    private static PrintStream REDIRECTED_STD_OUT;
    private static PrintStream REDIRECTED_STD_ERR;
    static {
        // its important to do this AFTER the variables init. else statics like REDIRECTED will get overwritten
        if (System.getProperty("NO_SYSOUT_REDIRECT") == null) {
            Application.redirectOutputStreams();
        }
        System.setProperty("com.sun.jndi.rmi.object.trustURLCodebase", "false");
        System.setProperty("com.sun.jndi.ldap.object.trustURLCodebase", "false");
        System.setProperty("com.sun.jndi.cosnaming.object.trustURLCodebase", "false");
        System.setProperty("java.rmi.server.useCodebaseOnly", "true");
    }

    public static void addStreamCopy(File file, final org.appwork.utils.Application.PauseableOutputStream stream) {
        int i = 0;
        final File orgFile = file;
        while (true) {
            try {
                if (file.exists()) {
                    throw new FileNotFoundException("Exists");
                }
                stream.addBranch(new BufferedOutputStream(new FileOutputStream(file)));
                break;
            } catch (final FileNotFoundException e1) {
                i++;
                e1.printStackTrace();
                final String extension = org.appwork.utils.Files.getExtension(orgFile.getName());
                if (extension != null) {
                    file = new File(orgFile.getParentFile(), orgFile.getName().substring(0, orgFile.getName().length() - extension.length() - 1) + "." + i + "." + extension);
                } else {
                    file = new File(orgFile.getParentFile(), orgFile.getName() + "." + i);
                }
            }
        }
    }

    /**
     *
     * WARNING: this does no longer work for Java >=1.9
     *
     *
     * Adds a url to the classloader classpath this might fail if there is a security manager
     *
     * @param file
     * @throws IOException
     */
    @Deprecated
    public static void addUrlToClassPath(final URL url, final ClassLoader cl) throws IOException {
        try {
            if (url == null) {
                throw new IllegalArgumentException("file==null");
            }
            // hack to add an url to the system classpath
            Method method = null;
            try {
                method = cl.getClass().getMethod("addURL", new Class[] { URL.class });
            } catch (final NoSuchMethodException e) {
                method = URLClassLoader.class.getDeclaredMethod("addURL", new Class[] { URL.class });
            }
            method.setAccessible(true);
            method.invoke(cl, new Object[] { url });
        } catch (final Throwable t) {
            throw new IOException("Failed to add URL to system classloader: URL:" + url + "ClassLoader:" + cl, t);
        }
    }

    public static String getApplication() {
        return Application.APP_FOLDER;
    }

    /**
     * @return
     */
    public static File getApplicationRoot() {
        return Application.getRootByClass(Application.class, null);
    }

    /**
     * Returns the Path of appworkutils.jar
     *
     * @return
     */
    public static String getHome() {
        return Application.getRoot(Application.class);
    }

    /**
     * @return
     */
    public static URL getHomeURL() {
        try {
            return new File(Application.getHome()).toURI().toURL();
        } catch (final MalformedURLException e) {
            throw new WTFException(e);
        }
    }

    // returns the jar filename of clazz
    public static File getJarFile(final Class<?> clazz) {
        final String name = clazz.getName().replaceAll("\\.", "/") + ".class";
        final URL url = Application.getRessourceURL(name);
        final String prot = url.getProtocol();
        final String path = url.getPath();
        if (!"jar".equals(prot)) {
            throw new WTFException("Works in Jared mode only");
        }
        final int index = path.indexOf(".jar!");
        if (index < 0) {
            throw new WTFException("Works in Jared mode only");
        }
        try {
            return new File(new URL(path.substring(0, index + 4)).toURI());
        } catch (final MalformedURLException e) {
        } catch (final URISyntaxException e) {
        }
        return null;
    }

    public static long getJavaVersion() {
        return JVMVersion.get();
    }

    public static String getJVMVersion() {
        return JVMVersion.getJVMVersion();
    }

    /**
     * @param version
     * @return
     */
    public static long parseJavaVersionString(final String version) {
        return JVMVersion.parseJavaVersionString(version);
    }

    /**
     * @param class1
     * @return
     */
    public static String getPackagePath(final Class<?> class1) {
        return class1.getPackage().getName().replace('.', '/') + "/";
    }

    public static interface ResourceLookup {
        public File getResource(final String relative);

        public URL getRessourceURL(final String relative, final boolean preferClasspath);
    }

    private static final ResourceLookup resourceLookup = initResourceLookup();

    private static ResourceLookup initResourceLookup() {
        final String resourceLookupClass = System.getProperty("org.appwork.utils.Application.ResourceLookup", null);
        if (resourceLookupClass != null) {
            try {
                final Class<?> loadClass = Class.forName(resourceLookupClass);
                final ResourceLookup ret = (ResourceLookup) loadClass.newInstance();
                return ret;
            } catch (final ClassNotFoundException e) {
                throw new WTFException("ResourceLookupClass:" + resourceLookupClass, e);
            } catch (final InstantiationException e) {
                throw new WTFException("ResourceLookupClass:" + resourceLookupClass, e);
            } catch (final IllegalAccessException e) {
                throw new WTFException("ResourceLookupClass:" + resourceLookupClass, e);
            }
        }
        return null;
    }

    /**
     * Returns a resource file relative to the install directory
     *
     * @param relative
     * @return
     */
    public static File getResource(final String relative) {
        warnInit();
        final ResourceLookup resourceLookup = Application.resourceLookup;
        final File ret = resourceLookup != null ? resourceLookup.getResource(relative) : getHomeResource(relative);
        return ret;
    }

    public static File getHomeResource(final String relative) {
        return new File(Application.getHome(), relative);
    }

    /**
     *
     */
    public static void warnInit() {
        if (!isFrameWorkInitDone()) {
            System.out.println(" !!!!!!! FrameWork Init is not done. Call Application.ensureFrameWorkInit() as very first action in your application");
            System.out.println(" !!!!!!! FrameWork Init is not done. Call Application.ensureFrameWorkInit() as very first action in your application");
            System.out.println(" !!!!!!! FrameWork Init is not done. Call Application.ensureFrameWorkInit() as very first action in your application");
            System.out.println(" !!!!!!! FrameWork Init is not done. Call Application.ensureFrameWorkInit() as very first action in your application");
            new Exception(" !!!!!!! FrameWork Init is not done. Call Application.ensureFrameWorkInit() as very first action in your application").printStackTrace();
        }
    }

    /**
     * returns the url for the resource. if The resource can be found in classpath, it will be returned. otherwise the function will return
     * the fileurl to current working directory
     *
     * @param string
     * @return
     */
    public static URL getRessourceURL(final String relative) {
        return Application.getRessourceURL(relative, true);
    }

    public static URL getRessourceURL(final String relative, final boolean preferClasspath) {
        final ResourceLookup resourceLookup = Application.resourceLookup;
        final URL ret = resourceLookup != null ? resourceLookup.getRessourceURL(relative, preferClasspath) : getHomeRessourceURL(relative, preferClasspath);
        return ret;
    }

    /**
     * Returns the Resource url for relative.
     *
     * NOTE:this function only returns URL's that really exists!
     *
     * if preferClassPath is true:
     *
     * we first check if there is a resource available inside current classpath, for example inside the jar itself. if no such URL exists we
     * check for file in local filesystem
     *
     * if preferClassPath if false:
     *
     * first check local filesystem, then inside classpath
     *
     *
     *
     * @param string
     * @param b
     */
    public static URL getHomeRessourceURL(final String relative, final boolean preferClasspath) {
        try {
            if (relative == null) {
                return null;
            } else if (relative.startsWith("/") || relative.startsWith("\\")) {
                throw new WTFException("getRessourceURL only works with relative paths.");
            } else if (preferClasspath) {
                final URL res = Application.class.getClassLoader().getResource(relative);
                if (res != null) {
                    return res;
                }
                final File file = getHomeResource(relative);
                if (file.exists()) {
                    return file.toURI().toURL();
                }
            } else {
                final File file = getHomeResource(relative);
                if (file.exists()) {
                    return file.toURI().toURL();
                }
                final URL res = Application.class.getClassLoader().getResource(relative);
                if (res != null) {
                    return res;
                }
            }
        } catch (final MalformedURLException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * @param object
     * @return
     */
    public static String getJarName(Class<?> clazz) {
        if (clazz == null) {
            clazz = Application.class;
        }
        final String name = clazz.getName().replaceAll("\\.", "/") + ".class";
        final String url = Application.getRessourceURL(name).toString();
        final int index = url.indexOf(".jar!");
        if (index < 0) {
            throw new IllegalStateException("No JarName Found");
        } else {
            try {
                return new File(new URL(url.substring(4, index + 4)).toURI()).getName();
            } catch (final Exception e) {
            }
            throw new IllegalStateException("No JarName Found");
        }
    }

    /**
     * Detects the applications home directory. it is either the pass of the appworkutils.jar or HOME/
     */
    public static String getRoot(final Class<?> rootOfClazz) {
        if (rootOfClazz == null) {
            throw new IllegalArgumentException("rootOfClazz is null");
        } else if (Application.ROOT != null) {
            return Application.ROOT;
        }
        final String system = System.getProperty("ROOT");
        if (system != null) {
            ROOT = system;
            return system;
        }
        final String key = "awuhome" + Application.APP_FOLDER;
        final String sysProp = System.getProperty(key);
        if (sysProp != null) {
            Application.ROOT = sysProp;
            return sysProp;
        }
        if (Application.isJared(rootOfClazz)) {
            // this is the jar file
            final URL loc = rootOfClazz.getProtectionDomain().getCodeSource().getLocation();
            java.io.File appRoot = null;
            try {
                appRoot = urlToFile(loc);
                Application.ROOT = appRoot.getAbsolutePath();
                // System.out.println("Application Root: " + Application.ROOT + " (jared) " + rootOfClazz);
            } catch (final URISyntaxException e) {
                Application.ROOT = System.getProperty("user.home") + System.getProperty("file.separator") + Application.APP_FOLDER + System.getProperty("file.separator");
                // System.out.println("Application Root: " + Application.ROOT + " (jared but error) " + rootOfClazz);
            }
        } else {
            Application.ROOT = System.getProperty("user.home") + System.getProperty("file.separator") + Application.APP_FOLDER;
            // System.out.println("Application Root: " + Application.ROOT + " (DEV) " + rootOfClazz);
        }
        // do not use Log.L here. this might be null
        return Application.ROOT;
    }

    @Deprecated
    public static java.io.File urlToFile(final URL loc) throws URISyntaxException {
        if (loc == null) {
            throw new IllegalArgumentException("loc is null");
        }
        final String path = loc.getPath();
        File appRoot = null;
        // loc may be a
        try {
            appRoot = new File(java.net.URLDecoder.decode(path, "UTF-8"));
            if (!appRoot.exists()) {
                appRoot = null;
            }
        } catch (final java.io.UnsupportedEncodingException e) {
        }
        if (appRoot == null) {
            appRoot = new File(path);
            if (!appRoot.exists()) {
                appRoot = null;
            }
        }
        if (appRoot == null) {
            appRoot = new File(loc.toURI());
            if (!appRoot.exists()) {
                appRoot = null;
            }
        }
        if (appRoot == null) {
            throw new java.net.URISyntaxException(loc + "", "Bad URI");
        }
        if (appRoot.isFile()) {
            appRoot = appRoot.getParentFile();
        }
        return appRoot;
    }

    /**
     * @param class1
     * @param subPaths
     * @return
     */
    public static File getRootByClass(final Class<?> class1, final String subPaths) {
        if (class1 == null) {
            throw new IllegalArgumentException("class is null");
        }
        // this is the jar file
        final URL loc = class1.getProtectionDomain().getCodeSource().getLocation();
        try {
            File appRoot = new File(loc.toURI());
            if (appRoot.isFile()) {
                appRoot = appRoot.getParentFile();
            }
            if (subPaths != null) {
                return new File(appRoot, subPaths);
            }
            return appRoot;
        } catch (final URISyntaxException e) {
            return null;
        }
    }

    /**
     * @param class1
     * @param subPaths
     *            TODO
     * @return
     */
    public static URL getRootUrlByClass(final Class<?> class1, final String subPaths) {
        try {
            return Application.getRootByClass(class1, subPaths).toURI().toURL();
        } catch (final MalformedURLException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * @return
     */
    public static File getTemp() {
        final File ret = Application.getResource("tmp");
        if (!ret.exists()) {
            ret.mkdirs();
        }
        return ret;
    }

    public static File getTempFile(final String prefix, final String suffix) throws IOException {
        final String tmpDirProperty = System.getProperty("java.io.tmpdir");
        final String random = Long.toString(UniqueAlltimeID.next());
        final File tmp;
        if (StringUtils.isEmpty(tmpDirProperty) || tmpDirProperty.contains("~") || !new File(tmpDirProperty).isDirectory()) {
            tmp = Application.getTemp();
            tmp.mkdirs();
        } else {
            tmp = null;
        }
        final String tmpPrefix = prefix + random;
        try {
            return File.createTempFile(tmpPrefix, suffix, tmp);
        } catch (final IOException e) {
            throw new IOException("failed to create tmpFile!prefix:" + tmpPrefix + "|suffix:" + suffix + "|tmp:" + tmp, e);
        }
    }

    /**
     * @param cache
     * @return
     */
    public static File getTempResource(final String cache) {
        return new File(Application.getTemp(), cache);
    }

    public static boolean is64BitJvm() {
        if (Application.JVM64BIT != null) {
            return Application.JVM64BIT;
        }
        final String archDataModel = System.getProperty("sun.arch.data.model");
        try {
            if (archDataModel != null) {
                if (Integer.parseInt(archDataModel) == 64) {
                    Application.JVM64BIT = true;
                    return true;
                } else {
                    Application.JVM64BIT = false;
                    return false;
                }
            }
        } catch (final Throwable e) {
        }
        final boolean is64BitJVM = CrossSystem.is64BitArch();
        Application.JVM64BIT = is64BitJVM;
        return is64BitJVM;
    }

    /**
     * Detects if the Application runs out of a jar or not.
     *
     * @param rootOfClazz
     *
     * @return
     */
    public static boolean isJared(Class<?> rootOfClazz) {
        if (Application.IS_JARED != null) {
            return Application.IS_JARED == Boolean.TRUE;
        }
        if (rootOfClazz == null) {
            rootOfClazz = Application.class;
        }
        final String name = rootOfClazz.getName().replaceAll("\\.", "/") + ".class";
        final ClassLoader cll = Application.class.getClassLoader();
        if (cll == null) {
            Application.IS_JARED = Boolean.TRUE;
            return true;
        }
        // System.out.println(name);
        final URL caller = cll.getResource(name);
        // System.out.println(caller);
        /*
         * caller is null in case the resource is not found or not enough rights, in that case we assume its not jared
         */
        if (caller == null) {
            Application.IS_JARED = false;
            return false;
        } else {
            final boolean ret = caller.toString().matches("jar\\:.*\\.(jar|exe)\\!.*");
            Application.IS_JARED = ret;
            return ret;
        }
    }

    public static void printSystemProperties(final LogInterface logger) {
        printSystemProperties(logger, null);
    }

    /**
     * @param logger
     */
    public static void printSystemProperties(final LogInterface logger, final Collection<String> blackList) {
        final Properties properties = System.getProperties();
        final Enumeration propertiesKey = properties.keys();
        final StringBuilder sb = new StringBuilder();
        try {
            final List<String> lst = ManagementFactory.getRuntimeMXBean().getInputArguments();
            for (final String key : lst) {
                if (blackList != null && blackList.contains("jvm_" + key)) {
                    continue;
                } else {
                    sb.append("JVM: ").append(key);
                    logger.info(sb.toString());
                    sb.setLength(0);
                }
            }
        } catch (final Throwable e) {
            logger.log(e);
        }
        while (propertiesKey.hasMoreElements()) {
            final String key = String.valueOf(propertiesKey.nextElement());
            if (blackList != null && blackList.contains("prop_" + key)) {
                continue;
            } else {
                sb.append("SysProp: ").append(key).append(": ").append(properties.get(key));
                logger.info(sb.toString());
                sb.setLength(0);
            }
        }
        for (final Entry<String, String> e : System.getenv().entrySet()) {
            final String key = e.getKey();
            if (blackList != null && blackList.contains("env_" + key)) {
                continue;
            } else {
                sb.append("SysEnv: ").append(key).append(": ").append(e.getValue());
                logger.info(sb.toString());
                sb.setLength(0);
            }
        }
        URL url = Application.getRessourceURL("version.nfo");
        if (url != null) {
            try {
                logger.info(url + ":\r\n" + IO.readURLToString(url));
            } catch (final IOException e1) {
                logger.log(e1);
            }
        }
        url = Application.getRessourceURL("build.json");
        if (url != null) {
            try {
                logger.info(url + ":\r\n" + IO.readURLToString(url));
            } catch (final IOException e1) {
                logger.log(e1);
            }
        }
        url = Application.getRessourceURL("version.txt");
        if (url != null) {
            try {
                logger.info(url + ":\r\n" + IO.readURLToString(url));
            } catch (final IOException e1) {
                logger.log(e1);
            }
        }
        try {
            final Map<String, String> mani = Application.getManifest();
            if (mani != null && mani.size() > 0) {
                for (final Entry<String, String> es : mani.entrySet()) {
                    logger.info(es.getKey() + ": " + es.getValue());
                }
            }
            try {
                final String jarName = Application.getJarName(Application.class);
                if (jarName != null) {
                    final File jarFile = Application.getResource(jarName);
                    if (jarFile.isFile()) {
                        logger.info(jarFile + "- lastModified: " + DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.MEDIUM).format(new Date(jarFile.lastModified())));
                    }
                }
            } catch (final Exception ignore) {
                // ide
            }
            final String moduleName = System.getProperty("exe4j.moduleName");
            if (StringUtils.isNotEmpty(moduleName)) {
                File exe4jFile = new File(moduleName);
                if (!exe4jFile.isAbsolute() || !exe4jFile.isFile() || exe4jFile.length() == 0) {
                    exe4jFile = Application.getResource(moduleName);
                }
                if (exe4jFile.isFile()) {
                    logger.info(exe4jFile + "- lastModified: " + DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.MEDIUM).format(new Date(exe4jFile.lastModified())));
                }
            }
        } catch (final Throwable e1) {
            logger.log(e1);
        }
    }

    public static class PauseableOutputStream extends OutputStream {
        private final OutputStream                           _out;
        private final AtomicReference<ByteArrayOutputStream> buffer = new AtomicReference<ByteArrayOutputStream>();

        /**
         * @param out
         */
        public PauseableOutputStream(final OutputStream out) {
            this._out = out;
        }

        /*
         * (non-Javadoc)
         *
         * @see java.io.OutputStream#write(int)
         */
        @Override
        public void write(final int paramInt) throws IOException {
            if (this.branches.size() > 0) {
                for (final OutputStream os : this.branches) {
                    try {
                        os.write(paramInt);
                    } catch (final Throwable e) {
                    }
                }
            }
            final ByteArrayOutputStream buffer = this.buffer.get();
            if (buffer != null) {
                buffer.write(paramInt);
            } else {
                this._out.write(paramInt);
            }
        }

        /*
         * (non-Javadoc)
         *
         * @see java.io.OutputStream#write(byte[])
         */
        @Override
        public void write(final byte[] b) throws IOException {
            if (this.branches.size() > 0) {
                for (final OutputStream os : this.branches) {
                    try {
                        os.write(b);
                    } catch (final Throwable e) {
                    }
                }
            }
            final ByteArrayOutputStream buffer = this.buffer.get();
            if (buffer != null) {
                buffer.write(b);
            } else {
                this._out.write(b);
            }
        }

        /*
         * (non-Javadoc)
         *
         * @see java.io.OutputStream#write(byte[], int, int)
         */
        @Override
        public void write(final byte[] buff, final int off, final int len) throws IOException {
            if (this.branches.size() > 0) {
                for (final OutputStream os : this.branches) {
                    try {
                        os.write(buff, off, len);
                    } catch (final Throwable e) {
                    }
                }
            }
            final ByteArrayOutputStream buffer = this.buffer.get();
            if (buffer != null) {
                buffer.write(buff, off, len);
            } else {
                this._out.write(buff, off, len);
            }
        }

        /*
         * (non-Javadoc)
         *
         * @see java.io.OutputStream#flush()
         */
        @Override
        public void flush() throws IOException {
            for (final OutputStream os : this.branches) {
                try {
                    os.flush();
                } catch (final Throwable e) {
                }
            }
            final ByteArrayOutputStream buffer = this.buffer.get();
            if (buffer != null) {
                buffer.flush();
            } else {
                this._out.flush();
            }
        }

        /*
         * (non-Javadoc)
         *
         * @see java.io.OutputStream#close()
         */
        @Override
        public void close() throws IOException {
            try {
                for (final OutputStream os : this.branches) {
                    try {
                        os.close();
                    } catch (final Throwable e) {
                    }
                }
                final ByteArrayOutputStream buffer = this.buffer.get();
                if (buffer != null) {
                    buffer.close();
                    this.setBufferEnabled(false);
                }
            } finally {
                this._out.close();
            }
        }

        public boolean enableBuffer() throws IOException {
            return this.setBufferEnabled(true);
        }

        public boolean disableBuffer(final boolean dropBufferedData) throws IOException {
            if (dropBufferedData) {
                this.buffer.set(null);
            }
            return this.setBufferEnabled(false);
        }

        /**
         * returns current enabled state
         *
         * @param b
         * @return
         * @throws IOException
         */
        public boolean setBufferEnabled(final boolean b) throws IOException {
            if (b) {
                if (this.buffer.compareAndSet(null, new ByteArrayOutputStream())) {
                    return false;
                } else {
                    return true;
                }
            } else {
                final ByteArrayOutputStream buffer = this.buffer.getAndSet(null);
                if (buffer != null) {
                    buffer.writeTo(this._out);
                    return true;
                } else {
                    return false;
                }
            }
        }

        private final CopyOnWriteArrayList<OutputStream> branches = new CopyOnWriteArrayList<OutputStream>();

        /**
         * @param bufferedOutputStream
         */
        public boolean addBranch(final OutputStream os) {
            return os != null && this.branches.addIfAbsent(os);
        }

        public boolean removeBranch(final OutputStream os) {
            return os != null && this.branches.remove(os);
        }

        /**
         * @param branches2
         */
        public void setBranches(final List<OutputStream> branches) {
            if (branches == null || branches.size() == 0) {
                this.branches.clear();
            } else {
                for (final OutputStream os : branches) {
                    this.addBranch(os);
                }
                this.branches.retainAll(branches);
            }
        }

        /**
         * @param outputStream
         */
        public void setBranch(final OutputStream outputStream) {
            if (outputStream == null) {
                this.branches.clear();
            } else {
                this.setBranches(Arrays.asList(new OutputStream[] { outputStream }));
            }
        }
    }

    private static String getCharSet(final PrintStream ps) {
        try {
            final Field field = ReflectionUtils.getField(ps.getClass(), "charOut", ps, Writer.class);
            final Writer writer = (Writer) field.get(ps);
            if (writer instanceof OutputStreamWriter) {
                final String charSet = ((OutputStreamWriter) writer).getEncoding();
                if (!StringUtils.isEmpty(charSet)) {
                    return Charset.forName(charSet).name();
                }
            }
        } catch (final Throwable ignore) {
        }
        return Charset.defaultCharset().name();
    }

    /**
     *
     */
    public static void redirectOutputStreams() {
        if (Application.REDIRECTED) {
            return;
        }
        ORG_STD_OUT = System.out;
        ORG_STD_ERR = System.err;
        try {
            if (System.getProperty("sun.stdout.encoding") == null && Charset.defaultCharset() == Charset.forName("cp1252")) {
                // workaround.
                // even 1252 is default codepage, windows console expects cp850 codepage input
                try {
                    System.setOut(new PrintStream(Application.STD_OUT = new PauseableOutputStream(new FileOutputStream(FileDescriptor.out)), true, "CP850"));
                    System.setErr(new PrintStream(Application.ERR_OUT = new PauseableOutputStream(new FileOutputStream(FileDescriptor.err)), true, "CP850"));
                } catch (final UnsupportedEncodingException e) {
                    System.setOut(new PrintStream(Application.STD_OUT = new PauseableOutputStream(System.out)));
                    System.setErr(new PrintStream(Application.ERR_OUT = new PauseableOutputStream(System.err)));
                    e.printStackTrace();
                }
            } else {
                final PrintStream stdOut = System.out;
                final PrintStream stdErr = System.err;
                try {
                    System.setOut(new PrintStream(Application.STD_OUT = new PauseableOutputStream(stdOut), true, System.getProperty("sun.stdout.encoding", getCharSet(stdOut))));
                    System.setErr(new PrintStream(Application.ERR_OUT = new PauseableOutputStream(stdErr), true, System.getProperty("sun.stderr.encoding", getCharSet(stdErr))));
                } catch (final UnsupportedEncodingException e) {
                    System.setOut(new PrintStream(Application.STD_OUT = new PauseableOutputStream(stdOut)));
                    System.setErr(new PrintStream(Application.ERR_OUT = new PauseableOutputStream(stdErr)));
                    e.printStackTrace();
                }
            }
        } finally {
            REDIRECTED_STD_OUT = System.out;
            REDIRECTED_STD_ERR = System.err;
            Application.REDIRECTED = true;
        }
    }

    private static Class<?> MAINCLASS = null;

    public static Class<?> getMainClass() {
        return MAINCLASS;
    }

    /**
     * sets current Application Folder and Jar ID. MUST BE SET at startup! Can only be set once!
     *
     * @param newAppFolder
     * @param newJar
     */
    public synchronized static void setApplication(final String newAppFolder) {
        Application.ROOT = null;
        Application.APP_FOLDER = newAppFolder;
        if (MAINCLASS == null) {
            Application.MAINCLASS = findMainClass();
        }
        Application.ROOT = getRoot(Application.class);
        Application.ensureFrameWorkInit();
    }

    private static Class<?> findMainClass() {
        final StackTraceElement[] stackTrace = new Exception().getStackTrace();
        if (stackTrace == null || stackTrace.length == 0) {
            return null;
        }
        final StackTraceElement first = stackTrace[stackTrace.length - 1];
        final String className = first.getClassName();
        try {
            final Class<?> clz = Class.forName(className);
            clz.getMethod("main", String[].class);
            return clz;
        } catch (Exception ignore) {
            return null;
        }
    }

    /**
     * @return
     */
    public static boolean isHeadless() {
        return GraphicsEnvironment.isHeadless();
    }

    /**
     * returns a file that does not exists. thus it ads a counter to the path until the resulting file does not exist
     *
     * @param string
     * @return
     */
    public static File generateNumberedTempResource(final String string) {
        return Application.generateNumbered(Application.getTempResource(string));
    }

    /**
     * returns a file that does not exists. thus it ads a counter to the path until the resulting file does not exist
     *
     * @param string
     * @return
     */
    public static File generateNumberedResource(final String string) {
        return Application.generateNumbered(Application.getResource(string));
    }

    /**
     * @param resource
     */
    private static File generateNumbered(final File orgFile) {
        int i = 0;
        final String extension = Files.getExtension(orgFile.getName());
        File file = null;
        while (file == null || file.exists()) {
            i++;
            if (extension != null) {
                file = new File(orgFile.getParentFile(), orgFile.getName().substring(0, orgFile.getName().length() - extension.length() - 1) + "." + i + "." + extension);
            } else {
                file = new File(orgFile.getParentFile(), orgFile.getName() + "." + i);
            }
        }
        return file;
    }

    /**
     * This should always run as very first action in an application
     */
    private synchronized static void ensureFrameWorkInit() {
        if (DID_INIT) {
            return;
        } else {
            DID_INIT = true;
            BuildDecisions.status(LogV3.defaultLogger());
            // do not run in Tests
            if (BuildDecisions.isEnabled() && !isJared(null)) {
                if (System.getProperty("BUILD_DECISIONS_DIRECT") != null || BuildDecisions.isEmpty()) {
                    BuildDecisions.validate();
                } else {
                    new Thread("BuildDecisions Check") {
                        {
                            setDaemon(true);
                        }

                        public void run() {
                            try {
                                Thread.sleep(10000);
                            } catch (InterruptedException e) {
                                // cannot happen
                                LogV3.log(e);
                            }
                            BuildDecisions.validate();
                        };
                    }.start();
                }
            }
            org.appwork.shutdown.ShutdownController.getInstance();
        }
    }

    public static boolean isFrameWorkInitDone() {
        return DID_INIT;
    }

    /**
     * @param class1
     * @return
     */
    public static long getBuildTimeStamp() {
        try {
            final String value = getManifestEntry("build-timestamp");
            if (value == null) {
                return -1;
            } else {
                return Long.parseLong(value);
            }
        } catch (final NumberFormatException e) {
            return -1;
        }
    }

    public static String getBuildHotFix() {
        return getManifestEntry("hotfix-base");
    }

    public static String getManifestEntry(final String key) {
        final Map<String, String> mf = getManifest();
        if (mf == null) {
            return null;
        } else {
            final String ts = mf.get(key.toLowerCase(Locale.ROOT));
            if (ts == null) {
                return null;
            } else {
                return ts;
            }
        }
    }

    public static int getBuildID() {
        try {
            final String value = getManifestEntry("build-id");
            if (value == null) {
                return -1;
            } else {
                return Integer.parseInt(value);
            }
        } catch (final NumberFormatException e) {
            return -1;
        }
    }

    /**
     * @return
     */
    private static Map<String, String> MANIFEST;

    private static Map<String, String> getManifest() {
        Map<String, String> ret = MANIFEST;
        if (ret != null) {
            return ret;
        }
        final List<File> jars = new ArrayList<File>();
        try {
            final File applicationJar = Application.getJarFile(Application.class);
            if (applicationJar.isFile()) {
                jars.add(applicationJar);
            }
        } catch (final Exception e) {
            // IDE
        }
        if (jars.size() == 0) {
            // IDE workaround
            final File[] files = Application.getResource("").listFiles();
            if (files != null && files.length > 0) {
                for (final File file : files) {
                    if (file.isFile() && file.getName().endsWith(".jar")) {
                        jars.add(file);
                    }
                }
            }
        }
        ret = readManifests(jars);
        MANIFEST = ret;
        return ret;
    }

    public static HashMap<String, String> readManifests(final List<File> jars) {
        final HashMap<String, String> manifestMap = new HashMap<String, String>();
        NEXT_FILE: for (final File jar : jars) {
            try {
                final ZipFile zip = new ZipFile(jar);
                try {
                    final ZipEntry manifestEntry = zip.getEntry("META-INF/MANIFEST.MF");
                    if (manifestEntry != null) {
                        final String manifest = IO.readStreamToString(zip.getInputStream(manifestEntry), -1, true);
                        manifestMap.clear();
                        for (final String line : manifest.split("[\r\n]+")) {
                            if (StringUtils.isNotEmpty(line)) {
                                final int index = line.indexOf(":");
                                if (index > 0) {
                                    final String key = line.substring(0, index).trim();
                                    final String value = line.substring(index + 1).trim();
                                    manifestMap.put(key.toLowerCase(Locale.ROOT), value);
                                }
                            }
                        }
                        if (!StringUtils.equals("AppWork GmbH", manifestMap.get("created-by"))) {
                            continue NEXT_FILE;
                        } else {
                            break NEXT_FILE;
                        }
                    }
                } finally {
                    try {
                        zip.close();
                    } catch (final IOException e) {
                    }
                }
            } catch (final Exception e) {
            }
        }
        return manifestMap;
    }

    /**
     * returns a non-existing unique tmp file/path. This method will not return the same path twice, so it is ensured, that the application
     * will not use it in another thread.
     *
     * @param string
     * @return
     */
    public static File getTempUniqueResource(final String postFix) {
        File file = null;
        while (file == null || file.exists()) {
            file = Application.getResource("tmp/path_" + UniqueAlltimeID.next() + postFix);
        }
        return file;
    }

    /**
     * @return
     */
    public synchronized static String getThreadDump() {
        final StringBuilder sb = new StringBuilder();
        enhancedThreadDump: try {
            // Access the ThreadMXBean instance
            final ThreadMXBean threadMXBean = ManagementFactory.getThreadMXBean();
            if (threadMXBean == null) {
                break enhancedThreadDump;
            }
            final Boolean originalContentionMonitoring = threadMXBean.isThreadContentionMonitoringSupported() ? threadMXBean.isThreadContentionMonitoringEnabled() : null;
            try {
                if (originalContentionMonitoring != null) {
                    threadMXBean.setThreadContentionMonitoringEnabled(true); // Enable contention monitoring
                }
                // Retrieve all thread info, including lock information
                final ThreadInfo[] threadInfos = threadMXBean.dumpAllThreads(threadMXBean.isObjectMonitorUsageSupported(), threadMXBean.isSynchronizerUsageSupported());
                // Use StringBuilder for efficient string building
                sb.append("=== Enhanced Thread Dump ===\r\n\r\n");
                for (final ThreadInfo threadInfo : threadInfos) {
                    sb.append("Thread Name: ").append(threadInfo.getThreadName()).append("\r\n");
                    sb.append("Thread ID: ").append(threadInfo.getThreadId()).append("\r\n");
                    sb.append("Thread State: ").append(threadInfo.getThreadState()).append("\r\n");
                    if (JavaVersion.getVersion().isMinimum(JavaVersion.JVM_9_0)) {
                        try {
                            sb.append("Thread Daemon: ").append(ReflectionUtils.invoke(ThreadInfo.class, "isDaemon", threadInfo, boolean.class)).append("\r\n");
                            sb.append("Thread Priority: ").append(ReflectionUtils.invoke(ThreadInfo.class, "getPriority", threadInfo, int.class)).append("\r\n");
                        } catch (final InvocationTargetException e) {
                        }
                    }
                    // Blocked and waited time
                    if (originalContentionMonitoring != null && threadMXBean.isThreadContentionMonitoringEnabled()) {
                        final long blockedTime = threadInfo.getBlockedTime();
                        if (blockedTime > 0) {
                            sb.append("Blocked Time (ms/count): ").append(blockedTime).append("/").append(threadInfo.getBlockedCount()).append("\r\n");
                        }
                        final long waitedTime = threadInfo.getWaitedTime();
                        if (waitedTime > 0) {
                            sb.append("Waited Time (ms/count): ").append(waitedTime).append("/").append(threadInfo.getWaitedCount()).append("\r\n");
                        }
                    }
                    // Lock that this thread is waiting to acquire
                    final LockInfo lockInfo = threadInfo.getLockInfo();
                    if (lockInfo != null && threadInfo.getLockOwnerId() != -1) {
                        sb.append("  Waiting for Lock: ").append(lockInfo).append("\r\n");
                        sb.append("  Owned by Thread: ").append(threadInfo.getLockOwnerName()).append(" (ID: ").append(threadInfo.getLockOwnerId()).append(")\r\n");
                    }
                    // All monitors held by this thread
                    final MonitorInfo[] lockedMonitors = threadInfo.getLockedMonitors();
                    if (lockedMonitors.length > 0) {
                        sb.append("  Locked Monitors:\r\n");
                        for (final MonitorInfo monitor : lockedMonitors) {
                            sb.append("    - ").append(monitor).append(" at ").append(monitor.getLockedStackFrame()).append("\r\n");
                        }
                    }
                    // Synchronization locks held by this thread
                    final LockInfo[] lockedSynchronizers = threadInfo.getLockedSynchronizers();
                    if (lockedSynchronizers.length > 0) {
                        sb.append("  Locked Synchronizers:\r\n");
                        for (final LockInfo synchronizer : lockedSynchronizers) {
                            sb.append("    - ").append(synchronizer).append("\r\n");
                        }
                    }
                    sb.append("  Stack Trace:\r\n");
                    for (final StackTraceElement element : threadInfo.getStackTrace()) {
                        sb.append("    at ").append(element).append("\r\n");
                    }
                    sb.append("\r\n");
                }
                // Deadlock detection
                long[] deadlockedThreads = null;
                try {
                    if (threadMXBean.isSynchronizerUsageSupported()) {
                        deadlockedThreads = threadMXBean.findDeadlockedThreads();
                    }
                } catch (final UnsupportedOperationException e) {
                }
                if (deadlockedThreads != null) {
                    sb.append("=== Deadlock Detected ===\r\n\r\n");
                    final ThreadInfo[] deadlockedThreadInfos = threadMXBean.getThreadInfo(deadlockedThreads);
                    for (final ThreadInfo deadlockThreadInfo : deadlockedThreadInfos) {
                        sb.append("Thread Name: ").append(deadlockThreadInfo.getThreadName()).append("\r\n");
                        sb.append("Thread ID: ").append(deadlockThreadInfo.getThreadId()).append("\r\n");
                        sb.append("Thread State: ").append(deadlockThreadInfo.getThreadState()).append("\r\n");
                        final LockInfo lockInfo = deadlockThreadInfo.getLockInfo();
                        if (lockInfo != null) {
                            sb.append("  Waiting for Lock: ").append(lockInfo).append("\r\n");
                            sb.append("  Owned by Thread: ").append(deadlockThreadInfo.getLockOwnerName()).append(" (ID: ").append(deadlockThreadInfo.getLockOwnerId()).append(")\r\n");
                        }
                        sb.append("  Stack Trace:\r\n");
                        for (final StackTraceElement element : deadlockThreadInfo.getStackTrace()) {
                            sb.append("    at ").append(element).append("\r\n");
                        }
                        sb.append("\r\n");
                    }
                } else {
                    sb.append("No deadlocks detected.\r\n");
                }
                return sb.toString();
            } finally {
                // Restore original contention monitoring state
                if (originalContentionMonitoring != null) {
                    threadMXBean.setThreadContentionMonitoringEnabled(originalContentionMonitoring.booleanValue());
                }
            }
        } catch (final RuntimeException e) {
            sb.append("\r\nException during ThreadDump: \r\n" + Exceptions.getStackTrace(e));
        }
        sb.append("=== Basic Thread Dump ===\r\n\r\n");
        final Iterator<Entry<Thread, StackTraceElement[]>> it = Thread.getAllStackTraces().entrySet().iterator();
        while (it.hasNext()) {
            final Entry<Thread, StackTraceElement[]> next = it.next();
            final Thread thread = next.getKey();
            sb.append("Thread Name: ").append(thread.getName()).append("\r\n");
            sb.append("Thread ID: ").append(thread.getId()).append("\r\n");
            sb.append("Thread State: ").append(thread.getState()).append("\r\n");
            sb.append("Thread Daemon: ").append(thread.isDaemon()).append("\r\n");
            sb.append("Thread Priority: ").append(thread.getPriority()).append("\r\n");
            sb.append("  Stack Trace:\r\n");
            for (final StackTraceElement stackTraceElement : next.getValue()) {
                sb.append("\tat " + stackTraceElement + "\r\n");
            }
            sb.append("\r\n");
        }
        return sb.toString();
    }
}
