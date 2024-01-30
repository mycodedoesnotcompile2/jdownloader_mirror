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
import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Map.Entry;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Application;
import org.appwork.utils.IO;

public class TranslationUtils {
    /**
     * This function reads all given TranslateInterfaces and writes lng files for all their Defaults Languages.
     *
     * @param addComments
     * @param classes
     * @throws URISyntaxException
     * @throws IOException
     */
    public static void createFiles(final boolean addComments, final Class<? extends TranslateInterface>... classes) throws URISyntaxException, IOException {
        for (final Class<? extends TranslateInterface> class1 : classes) {
            final String rel = class1.getName().replace(".", "/") + ".class";
            final String file = new File(Application.getRessourceURL(rel).toURI()).getParentFile().getAbsolutePath().replace("\\bin\\", "\\src\\");
            for (final String lng : class1.getAnnotation(Defaults.class).lngs()) {
                final File f = new File(file + "/" + class1.getSimpleName() + "." + lng + ".lng");
                final String txt = TranslationFactory.create(class1)._getHandler().createFile(lng, null, addComments);
                f.delete();
                IO.writeStringToFile(f, txt);
                System.out.println("Wrote " + f);
            }
        }
    }

    public static class Translated {
        /**
         *
         */
        public Translated(String value, String comment) {
            this.value = value;
            this.comment = comment.trim();
        }

        final public String value;
        final public String comment;
    }

    public static interface TranslationProviderInterface {
        /**
         * @param m
         * @param enDefault
         *            TODO
         * @param string
         * @return
         */
        Translated get(Method m, String tl, String currentTranslation, String enDefault);
    }

    public static void createFiles(final boolean addComments, File dist, String[] lngs, TranslationProviderInterface prov, final Class<? extends TranslateInterface>... classes) throws URISyntaxException, IOException {
        for (final Class<? extends TranslateInterface> class1 : classes) {
            final String rel = class1.getName().replace(".", "/") + ".class";
            String file;
            if (dist != null) {
                file = new File(dist, rel).getParentFile().getAbsolutePath();
            } else {
                file = new File(Application.getRessourceURL(rel).toURI()).getParentFile().getAbsolutePath().replace("\\bin\\", "\\src\\");
            }
            for (final String lng : lngs) {
                final File f = new File(file + "/" + class1.getSimpleName() + "." + lng + ".lng");
                final String txt = TranslationFactory.create(class1, "en")._getHandler().createFile(lng, prov, addComments);
                f.delete();
                IO.writeStringToFile(f, txt);
                System.out.println("Wrote " + f);
                System.out.println(txt);
            }
        }
    }

    /**
     * @param txt
     * @param class1
     * @return
     */
    public static TranslateData restoreFromString(String txt, Class<TranslateData> class1) {
        if (txt.startsWith("{")) {
            return TranslateData.convertFromMap(JSonStorage.restoreFromString(txt, TypeRef.HASHMAP_STRING));
        } else {
            // key==value
            TranslateData ret = new TranslateData();
            int index = 0;
            int found = 0;
            int found2 = 0;
            int found3 = 0;
            String key;
            String value;
            while (true) {
                found = txt.indexOf("=", index);
                if (found < 0) {
                    break;
                }
                key = txt.substring(index, found).trim();
                found2 = txt.indexOf("\r", found + 1);
                found3 = txt.indexOf("\n", found + 1);
                if (found2 < 0 && found3 < 0) {
                    value = txt.substring(found + 1);
                } else if ((found2 < found3 && found2 >= 0) || found3 < 0) {
                    value = txt.substring(found + 1, found2);
                } else {
                    value = txt.substring(found + 1, found3);
                }
                // Slow!!
                ret.put(key, new TranslatedEntry(clean(value)));
                index = Math.max(found2, found3) + 1;
                if (index <= 0) {
                    break;
                }
            }
            return ret;
        }
    }

    /**
     * this method does a .trim() and replace("\\r","\r").replace("\\n","\n").replace("\\t","\t") TODO:Speed this up!
     *
     * @param value
     * @return
     */
    private static String clean(String value) {
        StringBuilder sb = new StringBuilder();
        char c, c2;
        value = value.trim();
        for (int i = 0; i < value.length(); i++) {
            switch (c = value.charAt(i)) {
            case '\\':
                i++;
                if (i == value.length()) {
                    sb.append(c);
                    continue;
                }
                switch (c2 = value.charAt(i)) {
                case 'n':
                    sb.append('\n');
                    break;
                case 'r':
                    sb.append('\r');
                    break;
                case 't':
                    sb.append('\t');
                    break;
                default:
                    sb.append(c);
                    sb.append(c2);
                    break;
                }
                break;
            default:
                sb.append(c);
            }
        }
        return sb.toString();
    }

    /**
     * @param map
     * @return
     */
    public static String serialize(TranslateData map) {
		StringBuilder ret = new StringBuilder();
        java.util.List<String> keys = new ArrayList<String>();
        for (Entry<String, TranslatedEntry> entry : map.entrySet()) {
            keys.add(entry.getKey());
        }
        Collections.sort(keys);
        for (String key : keys) {
            ret.append(key);
            ret.append("=");
            ret.append(map.get(key).getRaw().replace("\r", "\\r").replace("\n", "\\n"));
            ret.append("\r\n");
        }
        return ret.toString();
    }
}
