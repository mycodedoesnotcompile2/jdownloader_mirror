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
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.storage.flexijson.config.ide;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.net.URISyntaxException;
import java.security.InvalidParameterException;
import java.util.HashSet;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.TypeRef;
import org.appwork.storage.flexijson.config.FlexiConfigBuilder;
import org.appwork.storage.flexijson.config.FlexiConfigFromJsonConfigBuilder;
import org.appwork.storage.flexijson.mapper.interfacestorage.InterfaceStorage;
import org.appwork.storage.flexijson.mapper.interfacestorage.PropertyHandler;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.reflection.CompiledType;
import org.appwork.utils.reflection.CompiledType.AbstractNameScheme;
import org.appwork.utils.reflection.CompiledType.PrimitiveWrapperStrategy;
import org.appwork.utils.reflection.JavaSyntax;

/**
 * @author thomas
 * @date 15.09.2022
 *
 */
public class STATIC_CFG_CLASS_GENERATOR {
    private static StringBuilder     imports;
    private static HashSet<Class<?>> importedClasses;

    /**
     * @param b
     * @param class1
     * @param class2
     * @return
     * @return
     * @throws ClassNotFoundException
     * @throws URISyntaxException
     * @throws NoSuchMethodException
     * @throws SecurityException
     * @throws IOException
     */
    public static File overwrite(String container, Class<?> inter, boolean createIfNotExists, boolean swallowCFGInitException, boolean swallowPropertyInitExceptions) throws ClassNotFoundException, URISyntaxException, SecurityException, NoSuchMethodException, IOException {
        String sourcE = createSource(container, inter, swallowCFGInitException, swallowPropertyInitExceptions);
        String bin = new File(inter.getClassLoader().getResource("").toURI()).getAbsolutePath();
        File root = new File(bin.replace("/bin", "/src").replace("\\bin", "\\src"));
        File target = new File(root, container.replace(".", "/") + ".java");
        if (!createIfNotExists && !target.isFile()) {
            return target;
        }
        System.out.println("Updated file at " + target);
        System.out.println(sourcE);
        IO.secureWrite(target, sourcE, SYNC.META_AND_DATA);
        return target;
    }

    public static String createSource(String container, Class<?> inter, boolean swallowCFGInitException, boolean swallowPropertyInitExceptions) throws ClassNotFoundException, URISyntaxException, SecurityException, NoSuchMethodException, IOException {
        String bin = new File(inter.getClassLoader().getResource("").toURI()).getAbsolutePath();
        File root = new File(bin.replace("/bin", "/src").replace("\\bin", "\\src"));
        File target = new File(root, container.replace(".", "/") + ".java");
        StringBuilder sb = new StringBuilder();
        imports = new StringBuilder();
        importedClasses = new HashSet<Class<?>>();
        if (!StringUtils.equals(inter.getName().replaceAll("\\.[^\\.]+$", ""), container.replaceAll("\\.[^\\.]+$", ""))) {
            addImport(inter);
        }
        addImport(InterfaceStorage.class);
        addImport(PropertyHandler.class);
        addImport(Application.class);
        StringBuilder props = new StringBuilder();
        boolean builderAdded = false;
        if (target.isFile()) {
            String exists = IO.readFileToString(target);
            String builder = new Regex(exists, "(public\\s+static\\s+[^;]*\\s+CFG\\s+[^;]*=[^;]+;)").getMatch(0);
            String buildConfig = new Regex(exists, "(private\\s+static\\s+[^;]*buildConfig.*\\}//\\s*buildConfig)").getMatch(0);
            if (StringUtils.isNotEmpty(buildConfig)) {
                for (String imp : new Regex(exists, "import ([^\\;]+);").getColumn(0)) {
                    try {
                        Class<?> cls = Class.forName(imp);
                        if (buildConfig.contains(cls.getSimpleName())) {
                            addImport(cls);
                        }
                    } catch (Exception e) {
                    }
                }
            }
            if (builder != null) {
                builderAdded = true;
                props.append("     " + builder).append("\r\n\r\n");
                if (buildConfig != null) {
                    props.append("     " + buildConfig).append("\r\n\r\n");
                    if (buildConfig.contains(FlexiConfigBuilder.class.getSimpleName() + "<")) {
                        addImport(FlexiConfigBuilder.class);
                    }
                    if (buildConfig.contains(FlexiConfigFromJsonConfigBuilder.class.getSimpleName() + "<")) {
                        addImport(FlexiConfigFromJsonConfigBuilder.class);
                    }
                    addImport(LogV3.class);
                    addImport(InterruptedException.class);
                    addImport(Exception.class);
                }
                if (builder.contains(FlexiConfigBuilder.class.getSimpleName() + "<")) {
                    addImport(FlexiConfigBuilder.class);
                }
                if (builder.contains(FlexiConfigFromJsonConfigBuilder.class.getSimpleName() + "<")) {
                    addImport(FlexiConfigFromJsonConfigBuilder.class);
                }
            }
        }
        addImport(LogV3.class);
        addImport(InterruptedException.class);
        addImport(Exception.class);
        addImport(WTFException.class);
        if (!builderAdded) {
            props.append("    private static " + inter.getSimpleName() + " buildConfig() {\r\n");
            props.append("        try {\r\n");
            props.append("            final " + inter.getSimpleName() + " ret = new " + FlexiConfigBuilder.class.getSimpleName() + "<" + inter.getSimpleName() + ">(" + inter.getSimpleName() + ".class, Application.getResource(\"cfg/" + inter.getSimpleName() + ".json\")).getStorageOrDefault(null);\r\n");
            props.append("            if (ret == null) {\r\n");
            props.append("                throw new WTFException();\r\n");
            props.append("            }\r\n");
            props.append("            return ret;\r\n");
            props.append("        } catch (final Exception e) {\r\n");
            props.append("            LogV3.log(e);\r\n");
            props.append("            if (e instanceof InterruptedException) {\r\n");
            props.append("                // restore interrupt flag\r\n");
            props.append("                Thread.currentThread().interrupt();\r\n");
            props.append("            }\r\n");
            if (!swallowCFGInitException) {
                props.append("            throw e instanceof RuntimeException ? (RuntimeException) e : new WTFException(e);\r\n");
            }
            props.append("        }\r\n");
            if (swallowCFGInitException) {
                props.append("        return null;\r\n");
            }
            props.append("    }// buildConfig\r\n");
            props.append("     " + StringUtils.fillPost("public static final " + inter.getSimpleName(), " ", 100) + " " + StringUtils.fillPost("CFG", " ", 50) + " = buildConfig();").append("\r\n\r\n");
            addImport(FlexiConfigBuilder.class);
        }
        ClassCache cc = ClassCache.getClassCache(inter);
        for (String key : cc.getKeys()) {
            final String staticKey = StringUtils.camelCaseToUnderscore(key, true);
            // public static final PropertyHandler<CommitClientConfig, HashMap<String, String>> PATH_MAP =
            // InterfaceStorage.getPropertyHandler(CFG, "PathMap", TypeRef.HASHMAP_STRING);
            addImport(cc.getType(key));
            AbstractNameScheme rules = new JavaSyntax();
            CompiledType ct = CompiledType.create(cc.getType(key));
            addImport(ct.raw);
            for (CompiledType c : ct.componentTypes) {
                addImport(c.raw);
            }
            String typeRef = "new TypeRef<" + ct.toString(rules) + ">(){}";
            if (cc.getType(key) instanceof Class) {
                typeRef = ((Class) cc.getType(key)).getSimpleName() + ".class";
            }
            if (ct.type.equals(TypeRef.HASHMAP_STRING.getType())) {
                typeRef = "TypeRef.HASHMAP_STRING";
            }
            if (typeRef.contains("TypeRef")) {
                addImport(TypeRef.class);
            }
            rules.setPrimitiveWrapperStrategy(PrimitiveWrapperStrategy.WRAPPER_NAMES_FOR_BOTH);
            props.append("     " + StringUtils.fillPost("public static final PropertyHandler<" + inter.getSimpleName() + "," + ct.toString(rules) + "> ", " ", 100) + " " + StringUtils.fillPost(staticKey, " ", 50) + " = InterfaceStorage.getPropertyHandler(CFG, \"" + key + "\", " + typeRef + ", " + swallowPropertyInitExceptions + ");").append("\r\n");
        }
        String simple = new Regex(container, "([^\\.]+$)").getMatch(0);
        String packageName = new Regex(container, "(.*)(\\.[^\\.]+$)").getMatch(0);
        String ret = "package " + packageName + ";\r\n\r\n";
        ret += imports.toString() + "\r\n";
        ret += "public class " + simple + " {\r\n";
        ret += props.toString();
        ret += "\r\n";
        ret += "}";
        return ret;
    }

    /**
     * @param imports
     * @param type
     */
    private static void addImport(Type type) {
        if (type == null) {
            return;
        }
        if (type instanceof Class) {
            if (Clazz.isPrimitive(type)) {
                return;
            }
            if (((Class<?>) type).getName().startsWith("java.lang.")) {
                return;
            }
            if (!importedClasses.add((Class<?>) type)) {
                return;
            }
            imports.append("import " + ((Class<?>) type).getName().replace("$", ".") + ";").append("\r\n");
            return;
        } else if (type instanceof ParameterizedType) {
            addImport(ReflectionUtils.getRaw(type));
            Type[] actual = ((ParameterizedType) type).getActualTypeArguments();
            for (int i = 0; i < actual.length; i++) {
                addImport(actual[i]);
            }
            return;
        }
        throw new InvalidParameterException("type not supported: " + type);
    }
}
