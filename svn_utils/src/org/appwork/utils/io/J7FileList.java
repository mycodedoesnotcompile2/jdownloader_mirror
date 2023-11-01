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
package org.appwork.utils.io;

import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.FileSystem;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * @author daniel
 *
 */
public class J7FileList {
    public static List<File> findFiles(final Pattern pattern, final File directory, final boolean filesOnly) throws IOException {
        return findFiles(pattern, directory, filesOnly, true);
    }

    public static List<File> findFiles(final Pattern pattern, final File directory, final boolean filesOnly, final boolean patternOnFileNameOnly) throws IOException {
        final ArrayList<File> ret = new ArrayList<File>();
        if (directory != null && directory.exists()) {
            DirectoryStream<Path> stream = null;
            try {
                final Path directoryPath = directory.toPath();
                final FileSystem fs = directoryPath.getFileSystem();
                if (pattern != null) {
                    final PathMatcher matcher = fs.getPathMatcher("regex:" + pattern.pattern());
                    final DirectoryStream.Filter<Path> filter;
                    if (patternOnFileNameOnly) {
                        filter = new DirectoryStream.Filter<Path>() {
                            @Override
                            public boolean accept(Path entry) {
                                return matcher.matches(entry.getFileName());
                            }
                        };
                    } else {
                        filter = new DirectoryStream.Filter<Path>() {
                            @Override
                            public boolean accept(Path entry) {
                                return matcher.matches(entry.toAbsolutePath());
                            }
                        };
                    }
                    stream = fs.provider().newDirectoryStream(directoryPath, filter);
                } else {
                    final DirectoryStream.Filter<Path> filter = new DirectoryStream.Filter<Path>() {
                        @Override
                        public boolean accept(Path entry) {
                            return true;
                        }
                    };
                    stream = fs.provider().newDirectoryStream(directoryPath, filter);
                }
                for (final Path path : stream) {
                    final BasicFileAttributes pathAttr = Files.readAttributes(path, BasicFileAttributes.class);
                    if (filesOnly == false || pathAttr.isRegularFile()) {
                        ret.add(path.toFile());
                    }
                }
            } catch (final Throwable e) {
                throw new IOException(e);
            } finally {
                if (stream != null) {
                    try {
                        stream.close();
                    } catch (Throwable e) {
                    }
                }
            }
        }
        return ret;
    }
}
