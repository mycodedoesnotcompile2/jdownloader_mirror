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
package org.appwork.storage.config.utils;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.config.ConfigInterface;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.storage.config.handler.ListHandler;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;

public class ConfigUtils {

    /**
     * @param class1
     */
    /**
     * @param class1
     * @param resource
     */
    public static void printStaticMappings(final Class<? extends ConfigInterface> configInterface) {
        printStaticMappings(configInterface, null);
    }

    public static void printStaticMappings(final Class<? extends ConfigInterface> configInterface, final String resource) {
        printStaticMappings(configInterface, resource, "JsonConfig.create");
    }

    public static void printStaticMappings(final Class<? extends ConfigInterface> configInterface, final String resource, String factory) {
		final StringBuilder strBuild = new StringBuilder();
        System.err.println(configInterface);
        System.err.flush();
        strBuild.append("\r\n");
        strBuild.append("//Static Mappings for " + configInterface);
        strBuild.append("\r\n");
        if (resource == null) {
            strBuild.append("public static final " + configInterface.getSimpleName() + "                 CFG                               = " + factory + "(" + configInterface.getSimpleName() + ".class);");
        } else {
            strBuild.append("public static final " + configInterface.getSimpleName() + "                 CFG                               = " + factory + "(" + resource + ", " + configInterface.getSimpleName() + ".class);");

        }
        strBuild.append("\r\n");
        strBuild.append("public static final StorageHandler<" + configInterface.getSimpleName() + ">                 SH                               = (StorageHandler<" + configInterface.getSimpleName() + ">) CFG._getStorageHandler();");
        strBuild.append("\r\n");
        strBuild.append("//let's do this mapping here. If we map all methods to static handlers, access is faster, and we get an error on init if mappings are wrong.");

        // public static final BooleanKeyHandler LINK_FILTER_ENABLED =
        // SH.getKeyHandler("LinkFilterEnabled", BooleanKeyHandler.class);

        for (final KeyHandler<?> kh : JsonConfig.create(configInterface)._getStorageHandler().getKeyHandler()) {
            strBuild.append("\r\n");

            // String key = kh.getKey();
            final String methodname = kh.getGetMethod().getName().startsWith("is") ? kh.getGetMethod().getName().substring(2) : kh.getGetMethod().getName().substring(3);
            final StringBuilder sb = new StringBuilder();
            char c, lastc;
            lastc = ' ';
            for (int i = 0; i < methodname.length(); i++) {
                c = methodname.charAt(i);
                if (sb.length() > 0) {

                    if (Character.isUpperCase(c) && Character.isLowerCase(lastc)) {
                        sb.append('_');

                    }
                }

                sb.append(Character.toUpperCase(c));
                lastc = c;
            }
            /**
             *
             */

            if (kh.getAnnotation(DescriptionForConfigEntry.class) != null) {
                strBuild.append("\r\n");
                strBuild.append("/**");
                strBuild.append("\r\n");
                strBuild.append(" * " + kh.getAnnotation(DescriptionForConfigEntry.class).value());
                strBuild.append("\r\n");
                strBuild.append("**/");
            }
            strBuild.append("\r\n");
            if (kh.getClass().getName().contains("$")) {
                if (ListHandler.class.isAssignableFrom(kh.getClass())) {
                    final ParameterizedType sc = (ParameterizedType) kh.getClass().getGenericSuperclass();
                    final Class type = (Class) sc.getActualTypeArguments()[0];
                    final String sn = type.getSimpleName();
                    final Type raw = sc.getRawType();
                    if (raw instanceof Class) {
                        strBuild.append("public static final " + ((Class) raw).getSimpleName() + "<" + sn + ">" + " " + sb + " = (" + ((Class) raw).getSimpleName() + "<" + sn + ">" + ")SH.getKeyHandler(\"" + methodname + "\", " + ((Class) raw).getSimpleName() + ".class);");

                    } else {
                        throw new WTFException(raw + " - isNoClass");
                    }
                    continue;
                }
                throw new WTFException("Unsupported Keyhanlder");
            } else {
                strBuild.append("public static final " + kh.getClass().getSimpleName() + " " + sb + " = SH.getKeyHandler(\"" + methodname + "\", " + kh.getClass().getSimpleName() + ".class);");
            }

        }

        System.err.println("=======================");
        System.err.flush();
        try {
            Dialog.getInstance().showInputDialog(Dialog.STYLE_LARGE, configInterface.toString(), strBuild.toString());
        } catch (final DialogClosedException e) {           
            e.printStackTrace();
        } catch (final DialogCanceledException e) {            
            e.printStackTrace();
        }

    }
}
