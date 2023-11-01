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
import java.net.URL;
import java.net.URLConnection;
import java.util.jar.JarEntry;

/**
 * @author daniel
 *
 */
public class OracleWorkaroundJarHandler extends sun.net.www.protocol.jar.Handler {

    @Override
    protected URLConnection openConnection(final URL url) throws IOException {
        try {
            // System.out.println("openConnection:" + url);
            final String path = url.getFile();
            URL jarFileURL = null;
            int lastIndex = 0;
            if (StringUtils.startsWithCaseInsensitive(path, "file:")) {
                while (true) {
                    final int indexOf = path.indexOf(".jar", lastIndex);
                    if (indexOf > 0) {
                        final int index = indexOf + 4;
                        lastIndex = index;
                        final String jarFileName = path.substring(0, index);
                        if (jarFileName.contains("!/")) {
                            final File jarFile;
                            final URL jarURL;
                            try {
                                jarURL = new URL(jarFileName);
                                jarFile = new File(jarURL.toURI());
                            } catch (Exception e) {
                                e.printStackTrace();
                                continue;
                            }
                            if (jarFile.exists() && jarFile.isFile()) {
                                jarFileURL = jarURL;
                                break;
                            }
                        }
                    } else {
                        break;
                    }
                }
            }
            if (jarFileURL != null) {
                lastIndex++;// skip !
                final URL finalJarFileURL = jarFileURL;
                final String entryName;
                if (++lastIndex != path.length()) {
                    final String tempEntryName = path.substring(lastIndex, path.length());
                    entryName = sun.net.www.ParseUtil.decode(tempEntryName);
                } else {
                    entryName = null;
                }
                System.out.println("Workaround for URL.openConnection:" + finalJarFileURL + " Entry:" + entryName);
                return new sun.net.www.protocol.jar.JarURLConnection(url, this) {
                    @Override
                    public URL getJarFileURL() {
                        return finalJarFileURL;
                    }

                    @Override
                    public JarEntry getJarEntry() throws IOException {
                        return getJarFile().getJarEntry(getEntryName());
                    }

                    @Override
                    public String getEntryName() {
                        return entryName;
                    }
                };
            }
        } catch (final Throwable e) {
            e.printStackTrace();
        }
        return super.openConnection(url);

    }
}
