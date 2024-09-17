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
package org.appwork.resources.ide;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;

import org.appwork.exceptions.WTFException;
import org.appwork.resources.IconRef;
import org.appwork.utils.FileHandler;
import org.appwork.utils.Files;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.ide.IDEUtils;

/**
 * @author thomas
 * @date 14.10.2016
 *
 */
public class AWIconCleanUP {
    /**
     * @param string
     * @param clazz
     * @param class1
     * @param string
     * @throws ClassNotFoundException
     */
    public static void cleanup(final String setup, final String global, Class<? extends IconRef>... classes) throws ClassNotFoundException {
        File project = IDEUtils.getProjectFolder(Class.forName(new Exception().getStackTrace()[1].getClassName()));
        final HashSet<String> icons = new HashSet<String>();
        HashMap<String, Collection<Class<? extends IconRef>>> clsMap = new HashMap<String, Collection<Class<? extends IconRef>>>();
        for (Class<? extends IconRef> cl : classes) {
            for (IconRef e : cl.getEnumConstants()) {
                icons.add(e.path());
                System.out.println(cl.getSimpleName() + "." + e + " - " + e.path());
                Collection<Class<? extends IconRef>> ls = clsMap.get(e.path());
                if (ls == null) {
                    ls = new HashSet<Class<? extends IconRef>>();
                    clsMap.put(e.path(), ls);
                }
                ls.add(cl);
            }
        }
        for (String icon : icons) {
            File pngThemes = new File(new File(project, setup), icon + ".png");
            File svgThemes = new File(new File(project, setup), icon + ".svg");
            File pngglobal = new File(new File(project, global), icon + ".png");
            File svgglobal = new File(new File(project, global), icon + ".svg");
            if (!pngglobal.exists() && !svgglobal.exists() && !pngThemes.exists() && !svgThemes.exists()) {
                System.err.println("Missing Icon!: " + icon + " " + clsMap.get(icon));
            } else {
                System.out.println("OK icon: " + icon);
            }
        }
        findUnused(icons, new File(project, setup));
    }

    /**
     * @param icons
     * @param file
     */
    private static void findUnused(final HashSet<String> icons, final File root) {
        org.appwork.utils.Files.walkThroughStructure(new FileHandler<RuntimeException>() {
            @Override
            public void intro(File f) throws RuntimeException {
            }

            @Override
            public boolean onFile(File f, int depths) throws RuntimeException {
                String rel = Files.getRelativePath(root, f);
                rel = rel.replace("\\", "/");
                if (StringUtils.isNotEmpty(rel) && (rel.endsWith(".svg") || rel.endsWith(".png"))) {
                    rel = rel.replaceAll("\\.(png|svg)$", "");
                    if (!icons.contains(rel)) {
                        System.err.println("Useless Icon: " + f + " ( " + rel + " ) ");
                    }
                }
                return true;
            }

            @Override
            public void outro(File f) throws RuntimeException {
            }
        }, root);
    }

    /**
     *
     */
    public static void printIconRefClassesInClassPath() {
        final Enumeration<URL> roots;
        final StringBuilder sb = new StringBuilder();
        sb.append("\r\n").append("  ArrayList<Class<? extends IconRef>> required = new ArrayList<Class<? extends IconRef>>();");
        try {
            roots = AWIconCleanUP.class.getClassLoader().getResources("");
            while (roots.hasMoreElements()) {
                final URL u = roots.nextElement();
                if (u.getProtocol().equals("file")) {
                    final File folder = new File(u.getPath());
                    if (folder.isDirectory()) {
                        sb.append("\r\n").append("// in " + folder.getParentFile().getName());
                        Files.walkThroughStructure(new FileHandler<RuntimeException>() {
                            @Override
                            public void intro(File f) throws RuntimeException {
                            }

                            @Override
                            public boolean onFile(File f, int depths) throws RuntimeException {
                                if ("class".equals(Files.getExtension(f.getName()))) {
                                    final String path = new Regex(Files.getRelativePath(folder, f), "(.*)\\.class$").getMatch(0).replace("/", ".");
                                    if (!path.contains("$") && path.toLowerCase(Locale.ENGLISH).contains("icon")) {
                                        try {
                                            final Class<?> cls = Class.forName(path);
                                            if (IconRef.class.isAssignableFrom(cls) && cls.isEnum()) {
                                                sb.append("\r\n").append("\trequired.add(" + path + ".class);");
                                            }
                                        } catch (ClassNotFoundException e) {
                                            e.printStackTrace();
                                        }
                                    }
                                }
                                return true;
                            }

                            @Override
                            public void outro(File f) throws RuntimeException {
                            }
                        }, folder);
                    }
                }
            }
            System.out.println(sb);
        } catch (IOException e1) {
            throw new WTFException(e1);
        }
    }
}
