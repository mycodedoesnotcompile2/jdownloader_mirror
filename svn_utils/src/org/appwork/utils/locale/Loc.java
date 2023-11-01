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
package org.appwork.utils.locale;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Locale;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.Storage;
import org.appwork.utils.Application;
import org.appwork.utils.URLStream;


/**
 * This class provides functions to return translated strings
 *
 * @author Christian
 */
@Deprecated
public class Loc {
    /**
     * The directory, where all localization files are located. A_ because this the order is important.
     */
    // public static final File A_LOCALIZATION_DIR =
    // Application.getResource("languages/");

    public static final Storage             CFG             = JSonStorage.getStorage("Locale");
    /**
     * The HashMap which contains all hashcodes of the keys and their translated values.
     *
     * @see Loc#parseLocalization(RFSFile)
     */
    private static HashMap<Integer, String> DATA            = null;

    private static String                   DEFAULT_LOCALE_CACHE;

    /**
     * The name of the default localization file. This is the english language.
     */
    private static final String             FALLBACK_LOCALE = "en_GB";

    private static String                   locale;

    /**
     * The key (String) under which the saved localization-name is stored.
     */
    public static final String              PROPERTY_LOCALE = "PROPERTY_LOCALE2";

    public static String T(final Translate t) {
        return t.s();
    }

    public static String T(final Translate t, final Object... parameter) {
        return t.s(parameter);
    }

    /**
     * @return
     */
    private static String getDefaultLocale() {
        if (Loc.DEFAULT_LOCALE_CACHE != null) {
            return Loc.DEFAULT_LOCALE_CACHE;
        }
        final String sys = System.getProperty("user.language").toLowerCase();
        final String cou = System.getProperty("user.country").toUpperCase();

        final String[] locs = Loc.getLocales();
        if (locs.length == 0) {
            Loc.DEFAULT_LOCALE_CACHE = Loc.FALLBACK_LOCALE;

        }
        if (Loc.DEFAULT_LOCALE_CACHE == null) {
            for (final String l : locs) {

                if (l.equals(sys + "_" + cou)) {
                    Loc.DEFAULT_LOCALE_CACHE = l;
                    break;
                }
            }
        }
        if (Loc.DEFAULT_LOCALE_CACHE == null) {
            for (final String l : locs) {

                if (l.equals(sys)) {
                    Loc.DEFAULT_LOCALE_CACHE = l;
                    break;
                }
            }
        }
        if (Loc.DEFAULT_LOCALE_CACHE == null) {
            for (final String l : locs) {
                if (l.startsWith(sys + "_")) {
                    Loc.DEFAULT_LOCALE_CACHE = l;
                    break;
                }
            }
        }

        if (Loc.DEFAULT_LOCALE_CACHE == null) {
            for (final String l : locs) {
                if (l.equals(Loc.FALLBACK_LOCALE)) {
                    Loc.DEFAULT_LOCALE_CACHE = l;
                    break;
                }
            }
        }
        if (Loc.DEFAULT_LOCALE_CACHE == null) {
            Loc.DEFAULT_LOCALE_CACHE = locs[0];
        }
        return Loc.DEFAULT_LOCALE_CACHE;
    }

    /**
     * Returns a localized regular expression for words that usualy ar present in an error message
     *
     * @return
     */
    public static String getErrorRegex() {
        return Loc.L("system.error", ".*(error|failed).*");
    }

    /**
     * @return
     */
    public static String getLocale() {

        return Loc.locale;
    }

    /**
     * @return
     */
    public static String[] getLocales() {
        final java.util.List<String> ret = new ArrayList<String>();

        // first look out for all translations in filesystem
        String[] files;

        files = Application.getResource("languages/").list(new FilenameFilter() {

            public boolean accept(final File dir, final String name) {
                return name.endsWith(".loc");

            }

        });

        if (files != null) {
            for (final String file : files) {
                ret.add(file.substring(0, file.length() - 4));
            }
        }

        // Search in jar:
        try {
            URL url = Application.getRessourceURL("languages/");
            if (url != null) {
                Enumeration<URL> resources;

                resources = Thread.currentThread().getContextClassLoader().getResources("languages/");

                while (resources.hasMoreElements()) {

                    url = resources.nextElement();
                    if (url.getProtocol().equalsIgnoreCase("jar")) {
                        final String path = url.getPath();
                        final int index = path.lastIndexOf('!');

                        final String jarPath = path.substring(0, index);
                        final String internPath = path.substring(index + 2);

                        final JarInputStream jarFile = new JarInputStream(new FileInputStream(new File(new URL(jarPath).toURI())));
                        JarEntry e;

                        String jarName;
                        while ((e = jarFile.getNextJarEntry()) != null) {
                            jarName = e.getName();
                            if (jarName.startsWith(internPath) && jarName.endsWith(".loc")) {
                                String filename = new File(jarName).getName();
                                filename = filename.substring(0, filename.length() - 4);
                                ret.remove(filename);
                                ret.add(filename);
                            }
                        }
                    } else {
                        files = new File(url.toURI()).list(new FilenameFilter() {

                            public boolean accept(final File dir, final String name) {
                                return name.endsWith(".loc");

                            }

                        });

                        if (files != null) {
                            for (final String file : files) {
                                ret.add(file.substring(0, file.length() - 4));
                            }
                        }
                    }
                }
            }
        } catch (final Exception e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
        return ret.toArray(new String[] {});

    }

    public static URL getResourceURL(final String loc) throws MalformedURLException {
        final File singleFile = Application.getResource("languages/" + loc + ".loc");
        URL file = Application.getRessourceURL("languages/" + loc + ".loc");
        if (singleFile.exists() && singleFile.length() > 0 || file == null) {
            file = singleFile.toURI().toURL();
        }
        return file;
    }

    /**
     * Returns the translated value for the translation-key. If the current language file doesn't contain the translated value, the default
     * value will be returned.
     *
     * @param key
     *            key for the translation in the language file. the key should <b>always</b> have the following structure
     *            <i>PACKAGE_NAME_FROM_CALLER.CLASS_NAME_FROM_CALLER.key</i>
     * @param def
     *            default value which will be returned if there is no mapping for the key
     * @return translated value or the def parameter
     * @see Loc#LF(String, String, Object...)
     * @throws IllegalArgumentException
     *             if the key is null or is empty
     */
    public static String L(String key, final String def) {
        if (key == null || (key = key.trim()).length() == 0) {
            throw new IllegalArgumentException();
        }
        if (Loc.DATA == null) {
                  org.appwork.loggingv3.LogV3.warning("No parsed localization found! Loading now from saved localization file!");
            try {
                Loc.setLocale(Loc.CFG.get(Loc.PROPERTY_LOCALE, Loc.FALLBACK_LOCALE));
            } catch (final Exception e) {

                      org.appwork.loggingv3.LogV3.severe("Error while loading the stored localization name!");
                Loc.setLocale(Loc.FALLBACK_LOCALE);
            }
            if (Loc.DATA == null) {
                return def == null ? "Error in Loc! No loaded data!" : def;
            }
        }

        final String loc = Loc.DATA.get(key.toLowerCase().hashCode());
        if (loc == null) {
            Loc.DATA.put(key.toLowerCase().hashCode(), def);
            return def;
        }
        return loc;
    }

    /**
     * Returns the translated value for the translation-key filled with the parameters.
     *
     * @param key
     *            key for the translation in the language file. the key should <b>always</b> have the following structure
     *            <i>PACKAGE_NAME_FROM_CALLER.CLASS_NAME_FROM_CALLER.key</i>
     * @param def
     *            default value which will be returned if there is no mapping for the key
     * @param args
     *            parameters which should be inserted in the translated string
     * @return translated value or the def parameter filled with the parameters
     * @see Loc#L(String, String)
     */
    public static String LF(final String key, final String def, final Object... args) {
        try {
            return String.format(Loc.L(key, def), args);
        } catch (final Exception e) {
            return "Error: " + key;
        }
    }

    /**
     * Creates a HashMap with the data obtained from the localization file. <br>
     * <b>Warning:</b> Overwrites any previously created HashMap
     *
     * @param file
     *            {@link RFSFile} object to the localization file
     * @throws IllegalArgumentException
     *             if the parameter is null or doesn't exist
     * @see Loc#DATA
     */
    public static void parseLocalization(final URL file) throws IllegalArgumentException {
        if (file == null) {
            throw new IllegalArgumentException();
        }

        if (Loc.DATA != null) {
                  org.appwork.loggingv3.LogV3.finer("Previous HashMap will be overwritten!");
        }
        Loc.DATA = new HashMap<Integer, String>();

        BufferedReader reader = null;
        InputStreamReader isr = null;
        InputStream fis = null;
        try {
            fis = URLStream.openStream(file);
            reader = new BufferedReader(isr = new InputStreamReader(fis, "UTF8"));

            String line;
            String key;
            String value;
            int split;
            while ((line = reader.readLine()) != null) {
                if (line.startsWith("#")) {
                    continue;
                }

                if ((split = line.indexOf('=')) <= 0) {
                    continue;
                }

                key = line.substring(0, split).toLowerCase().trim();
                value = line.substring(split + 1).trim();
                value = value.replace("\\n", "\n").replace("\\r", "\r");

                Loc.DATA.put(key.hashCode(), value);
            }
        } catch (final FileNotFoundException e) {
            throw new IllegalArgumentException(e);
        } catch (final Exception e) {
            org.appwork.loggingv3.LogV3.log(e);
        } finally {
            try {
                reader.close();
            } catch (final Throwable e) {
            }
            try {
                isr.close();
            } catch (final Throwable e) {
            }
            try {
                fis.close();
            } catch (final Throwable e) {
            }
        }
    }

    /**
     * Set-up this class by creating the HashMap for the key-string-pairs.
     *
     * @param loc
     *            name of the localization file
     * @see Loc#parseLocalization(RFSFile)
     */
    public static void setLocale(String loc) {
        try {
            if (loc == null) {

                loc = Loc.CFG.get(Loc.PROPERTY_LOCALE, Loc.getDefaultLocale());
            }
            // first check filesystem
            final URL file = Loc.getResourceURL(loc);

            Loc.locale = loc;
            if (file != null) {
                // TODO

                Loc.CFG.put(Loc.PROPERTY_LOCALE, loc);
                Loc.parseLocalization(file);
            } else {
                      org.appwork.loggingv3.LogV3.info("The language " + loc + " isn't available! Parsing default (" + Loc.FALLBACK_LOCALE + ".loc) one!");
                Loc.locale = Loc.getDefaultLocale();
                final String[] locs = Loc.locale.split("_");
                Locale.setDefault(new Locale(locs[0], locs[1]));
                Loc.parseLocalization(Loc.getResourceURL(Loc.FALLBACK_LOCALE));
            }
        } catch (final Exception e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
    }
}
