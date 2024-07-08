/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
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
package org.appwork.txtresource;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.lang.reflect.Proxy;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map.Entry;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;

import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Files;
import org.appwork.utils.StringUtils;

public class TranslationFactory {
    private static final HashMap<String, TranslateInterface> CACHE    = new HashMap<String, TranslateInterface>();
    private static String                                    LANGUAGE = "en";
    static {
        try {
            String l = System.getProperty("user.language");
            if (StringUtils.isEmpty(l)) {
                l = Locale.getDefault().getLanguage();
                if (StringUtils.isEmpty(l)) {
                    l = "en";
                }
            }
            String c = System.getProperty("user.country");
            if (StringUtils.isEmpty(c)) {
                c = Locale.getDefault().getCountry();
            }
            String v = System.getProperty("user.variant");
            if (StringUtils.isEmpty(v)) {
                v = Locale.getDefault().getVariant();
            }
            setDesiredLanguage(localeToString(l, c, v));
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }

    /**
     * @param string
     * @param ret
     */
    private static void collectByPath(final File path, final HashSet<String> ret) {
        final java.util.List<File> files = Files.getFiles(new FileFilter() {
            @Override
            public boolean accept(final File pathname) {
                return pathname.getName().endsWith(".lng");
            }
        }, path);
        String name;
        if (files != null) {
            for (final File file : files) {
                try {
                    name = file.getName();
                    final int index = name.indexOf(".");
                    if (index < 0 || index >= name.length() - 4) {
                        continue;
                    }
                    name = name.substring(index + 1, name.length() - 4);
                    if (ret.add(name)) {
                        org.appwork.loggingv3.LogV3.info(name + " found in " + file);
                    }
                } catch (final Throwable e) {
                    // Invalid LanguageFile nameing
                }
            }
        }
    }

    public static <T extends TranslateInterface> T create(final Class<T> class1) {
        return TranslationFactory.create(class1, getDesiredLanguage());
    }

    /**
     * do not call this directly for each translationrequest. use a static cache instead!
     */
    @SuppressWarnings("unchecked")
    public static <T extends TranslateInterface> T create(final Class<T> class1, final String... lookup) {
        synchronized (TranslationFactory.CACHE) {
            final StringBuilder sb = new StringBuilder();
            sb.append(class1.getName());
            for (final String c : lookup) {
                sb.append(c + ";");
            }
            final String id = sb.toString();
            T ret = (T) TranslationFactory.CACHE.get(id);
            if (ret == null) {
                ret = (T) Proxy.newProxyInstance(class1.getClassLoader(), new Class[] { class1 }, new TranslationHandler(class1, lookup));
                TranslationFactory.CACHE.put(id, ret);
            }
            return ret;
        }
    }

    /**
     * @param ret2
     * @param string
     * @return
     */
    private static void findInClassPath(final String path, final HashSet<String> ret) {
        // Search in jar:
        try {
            Enumeration<URL> resources;
            resources = Thread.currentThread().getContextClassLoader().getResources(path);
            String name, p, jarPath, internPath;
            while (resources.hasMoreElements()) {
                final URL url = resources.nextElement();
                if (url.getProtocol().equalsIgnoreCase("jar")) {
                    p = url.getPath();
                    int index = p.lastIndexOf('!');
                    jarPath = p.substring(0, index);
                    internPath = p.substring(index + 2);
                    final JarInputStream jarFile = new JarInputStream(new FileInputStream(new File(new URL(jarPath).toURI())));
                    JarEntry e;
                    String jarName;
                    while ((e = jarFile.getNextJarEntry()) != null) {
                        jarName = e.getName();
                        if (jarName.startsWith(internPath) && jarName.endsWith(".lng")) {
                            name = new File(jarName).getName();
                            index = name.indexOf(".");
                            if (index < 0 || index >= name.length() - 4) {
                                continue;
                            }
                            name = name.substring(index + 1, name.length() - 4);
                            if (ret.add(name)) {
                                org.appwork.loggingv3.LogV3.finer(name + " found in " + new File(jarName));
                            }
                        }
                    }
                } else {
                    TranslationFactory.collectByPath(new File(url.toURI()), ret);
                }
            }
        } catch (final Exception e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
    }

    /**
     *
     */
    public static List<TranslateInterface> getCachedInterfaces() {
        final LinkedHashSet<TranslateInterface> ret = new LinkedHashSet<TranslateInterface>();
        synchronized (TranslationFactory.CACHE) {
            for (final TranslateInterface intf : TranslationFactory.CACHE.values()) {
                if (intf != null) {
                    ret.add(intf);
                }
            }
        }
        return new ArrayList<TranslateInterface>(ret);
    }

    public static String getDesiredLanguage() {
        return TranslationFactory.LANGUAGE;
    }

    public static Locale getDesiredLocale() {
        final String lng = TranslationFactory.getDesiredLanguage();
        return TranslationFactory.stringToLocale(lng);
    }

    public static String handleLocaleMap(LocaleMap message) {
        if (message == null) {
            return null;
        }
        return handleLocaleMap(message, (Class<TranslateInterface>[]) null);
    }

    /**
     * @param message
     * @return
     */
    public static String handleLocaleMap(LocaleMap message, final Class<? extends TranslateInterface>... classes) {
        if (message == null) {
            return null;
        }
        String bestmatch = LocaleMap.getBestMatch(message, getDesiredLanguage());
        if (bestmatch == null) {
            DebugMode.logInfoInIDEOnly("No Value in LocaleMap");
            return "";
        }
        if (!bestmatch.contains("%")) {
            return bestmatch;
        }
        if (classes != null && classes.length > 0 && classes[0] != null) {
            for (Class<? extends TranslateInterface> t : classes) {
                if (t != null) {
                    TranslateInterface handler = TranslationFactory.create(t);
                    bestmatch = handler._getHandler().replaceWildCards(bestmatch);
                }
            }
        } else {
            synchronized (TranslationFactory.CACHE) {
                for (Entry<String, TranslateInterface> es : TranslationFactory.CACHE.entrySet()) {
                    bestmatch = es.getValue()._getHandler().replaceWildCards(bestmatch);
                }
            }
        }
        return bestmatch;
    }

    public static List<String> listAvailableTranslations(final Class<? extends TranslateInterface>... classes) {
        final LinkedHashSet<String> ret = new LinkedHashSet<String>();
        TranslationFactory.collectByPath(Application.getResource("translations"), ret);
        TranslationFactory.findInClassPath("translations", ret);
        for (final Class<? extends TranslateInterface> clazz : classes) {
            TranslationFactory.collectByPath(Application.getResource(clazz.getPackage().getName().replace(".", "/")), ret);
            TranslationFactory.findInClassPath(clazz.getPackage().getName().replace(".", "/"), ret);
            final Defaults defs = clazz.getAnnotation(Defaults.class);
            if (defs != null) {
                for (final String s : defs.lngs()) {
                    if (ret.add(s)) {
                        org.appwork.loggingv3.LogV3.finer(s + " src: " + clazz + " Defaults");
                    }
                }
            }
        }
        return new ArrayList<String>(ret);
    }

    /**
     * @return
     */
    public static String localeToString(final Locale l) {
        return localeToString(l.getLanguage(), l.getCountry(), l.getVariant());
    }

    public static String localeToString(final String language, String country, String variant) {
        final StringBuilder sb = new StringBuilder();
        sb.append(language);
        String c = country;
        boolean hasCountry = false;
        if (c != null && c.trim().length() > 0) {
            sb.append("_");
            sb.append(c.toUpperCase(Locale.ENGLISH));
            hasCountry = true;
        }
        c = variant;
        if (c != null && c.trim().length() > 0) {
            if (!hasCountry) {
                sb.append("_");
            }
            sb.append("_");
            sb.append(c);
        }
        return sb.toString();
    }

    public static boolean setDesiredLanguage(final String loc) {
        if (TranslationFactory.getDesiredLanguage().equals(loc)) {
            return false;
        }
        TranslationFactory.LANGUAGE = loc;
        ArrayList<String> lst = new ArrayList<String>();
        HashSet<String> dupe = new HashSet<String>();
        if (loc != null) {
            if (dupe.add(loc)) {
                lst.add(loc);
            }
        }
        synchronized (TranslationFactory.CACHE) {
            for (final TranslateInterface i : TranslationFactory.CACHE.values()) {
                i._getHandler().setLanguage(loc);
            }
        }
        return true;
    }

    public static Locale stringToLocale(final String lng) {
        final String[] split = lng.split("[\\-\\_]");
        final Locale def = Locale.getDefault();
        final Locale ret;
        switch (split.length) {
        case 1:
            if (split[0].equals(def.getLanguage())) {
                // HostLocaleProviderAdapterImpl only supports matching OS Locale
                ret = def;
            } else {
                ret = new Locale(split[0]);
            }
            break;
        case 2:
            if (split[0].equals(def.getLanguage()) && split[1].equals(def.getCountry())) {
                // avoid new Locale instance
                ret = def;
            } else {
                ret = new Locale(split[0], split[1]);
            }
            break;
        default:
            if (split[0].equals(def.getLanguage()) && split[1].equals(def.getCountry()) && split[2].equals(def.getVariant())) {
                // avoid new Locale instance
                ret = def;
            } else {
                ret = new Locale(split[0], split[1], split[2]);
            }
            break;
        }
        return ret;
    }

    public static List<String> getVariantsOf(String lng) {
        final LinkedHashSet<String> ret = new LinkedHashSet<String>();
        final Locale loc = TranslationFactory.stringToLocale(lng);
        ret.add(localeToString(loc.getLanguage(), loc.getCountry(), loc.getVariant()));
        ret.add(localeToString(loc.getLanguage(), loc.getCountry(), null));
        ret.add(localeToString(loc.getLanguage(), null, loc.getVariant()));
        ret.add(localeToString(loc.getLanguage(), null, null));
        // TODO: update TranslationHandler.createTranslationResource to use java.util.Locale.getLanguage.equals, see javadoc of
        // Locale.getLanguage
        // - ISO 639 is not a stable standard; some of the language codes it defines (specifically "iw",
        // "ji", and "in") have changed. This constructor accepts both the old codes ("iw", "ji", and "in") and
        // the new codes ("he", "yi", and "id"), but all other API on Locale will return only the OLD codes.
        if (StringUtils.equalsIgnoreCase("he", lng) || StringUtils.equalsIgnoreCase("yi", lng) || StringUtils.equalsIgnoreCase("id", lng)) {
            ret.add(lng);
        } else if (StringUtils.equalsIgnoreCase("iw", lng)) {
            ret.add("he");
        } else if (StringUtils.equalsIgnoreCase("ji", lng)) {
            ret.add("yi");
        } else if (StringUtils.equalsIgnoreCase("in", lng)) {
            ret.add("id");
        }
        return new ArrayList<String>(ret);
    }

    public static ArrayList<String> localesFromAcceptLanguageHeader(String value) {
        final ArrayList<String> ret = new ArrayList<String>();
        value = value.replace("-", "_");
        final String[] values = value.split("[,;]+");
        // String language = null;
        // String country = null;
        for (final String v : values) {
            if (v.startsWith("q=")) {
                continue;
            }
            final Locale l = stringToLocale(v);
            final String lS = localeToString(l);
            if (lS.matches("^[a-zA-Z_]+$")) {
                ret.add(lS);
            }
            ret.add(l.getLanguage());
        }
        return ret;
    }
}
