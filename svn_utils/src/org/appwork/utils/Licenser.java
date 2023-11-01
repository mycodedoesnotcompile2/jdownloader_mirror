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
package org.appwork.utils;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;

/**
 * @author thomas
 *
 */
public class Licenser {
    public static String[] getLines(final String arg) {
        if (arg == null) {
            return new String[] {};
        } else {
            final String[] temp = arg.split("[\r\n]{1,2}");
            final int tempLength = temp.length;
            final String[] output = new String[tempLength];
            for (int i = 0; i < tempLength; i++) {
                output[i] = temp[i];
            }
            return output;
        }
    }

    public static void main(String[] args) throws URISyntaxException, IOException {
        Application.setApplication(".licenser");
        File file = new File(new File(Application.class.getResource("Application.class").toURI()).getParentFile().getParentFile().getParentFile().getParentFile().getParentFile().getParentFile(), "/AppWorkUtils/src");
        String license = IO.readFileToString(new File(file.getParentFile(), "/License AppWork Utilities.txt"));
        doIt(file, license);
        file = new File(new File(Application.class.getResource("Application.class").toURI()).getParentFile().getParentFile().getParentFile().getParentFile().getParentFile().getParentFile(), "/MyJDownloaderClient/src");
        String newName;
        license = license.replace("AppWork Utilities", newName = "My JDownloader Client");
        File newFile = new File(file.getParentFile(), "/License " + newName + ".txt");
        newFile.delete();
        IO.writeStringToFile(newFile, license);
        doIt(file, license);
    }

    /**
     * @param file
     * @param license
     */
    protected static void doIt(File file, final String license) {
        final StringBuilder sb = new StringBuilder();
        sb.append("/**");
        for (String line : getLines(license)) {
            sb.append("\r\n");
            sb.append(" * ");
            sb.append(line);
        }
        sb.append(" */\r\n");
        System.out.println(file);
        Files.walkThroughStructure(new org.appwork.utils.Files.AbstractHandler<RuntimeException>() {
            @Override
            public void onFile(File f) throws RuntimeException {
                if (f.isFile() && f.getName().endsWith(".java")) {
                    try {
                        String src = IO.readFileToString(f);
                        int index = src.indexOf("package ");
                        if (index >= 0) {
                            String header = src.substring(0, index);
                            if (header.contains("AppWork GmbH")) {
                                write(sb, f, src, index);
                                return;
                            }
                            if (header.contains("AppWork UG")) {
                            } else {
                                header = header.replaceAll("[*/\\s]*", "");
                                if (header.trim().length() == 0) {
                                    if (src.toLowerCase().contains("license") || src.toLowerCase().contains("m.i.t") || src.toLowerCase().contains("gpl") || src.toLowerCase().contains("bsd") || src.toLowerCase().contains("apache")) {
                                        try {
                                            Dialog.getInstance().showConfirmDialog(0, "Write " + f);
                                        } catch (DialogClosedException e) {
                                            return;
                                        } catch (DialogCanceledException e) {
                                            return;
                                        }
                                    }
                                    write(sb, f, src, index);
                                } else {
                                    if (header.contains("GNUGeneralPublicLicense") && header.contains("JD-Team")) {
                                        write(sb, f, src, index);
                                    } else {
                                        System.out.println("??");
                                    }
                                }
                            }
                        } else {
                            System.out.println("??");
                        }
                    } catch (IOException e) {                        
                        throw new WTFException(e);
                    }
                }
            }

            /**
             * @param sb
             * @param f
             * @param src
             * @param index
             * @throws IOException
             */
            protected void write(final StringBuilder sb, File f, String src, int index) throws IOException {
                f.delete();
                IO.writeStringToFile(f, sb + clean(src.substring(index, src.length())));
            }

            private String clean(String src) {
                // TODO Auto-generated method stub
                /**
                 * Copyright (c) 2009 - 2014 AppWork UG(haftungsbeschränkt) <e-mail@appwork.org>
                 *
                 * This file is part of org.appwork.utils.os
                 *
                 * This software is licensed under the Artistic License 2.0, see the LICENSE file or
                 * http://www.opensource.org/licenses/artistic-license-2.0.php for details
                 */
                src = src.replaceAll("/\\*\\*\\s*\\*\\s*Copyright.*AppWork UG.*\\* for details.*\\*\\/", "");
                if (src.contains("AppWork UG")) {
                    System.out.println(src);
                }
                return src;
            }
        }, file);
    }
}
