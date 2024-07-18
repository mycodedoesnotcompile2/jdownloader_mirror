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
package org.appwork.utils;

import java.io.File;
import java.io.IOException;
import java.nio.channels.ClosedByInterruptException;
import java.nio.file.FileStore;
import java.nio.file.FileSystem;
import java.nio.file.FileSystemException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Stream;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.os.CrossSystem;

public class Files17 {
    private final static HashMap<String, List<FileStore>> FILESTORECACHE = new HashMap<String, List<FileStore>>();

    public static Iterable<FileStore> getFileStores() {
        final FileSystem fileSystem = FileSystems.getDefault();
        if (fileSystem.getClass().getName().endsWith("LinuxFileSystem")) {
            // LinuxFileSystem.getFileStores returns iterator which parses mtab file in real time and can block/take long time
            final File mtab = new File("/etc/mtab");
            if (mtab.isFile()) {
                final String cacheID = Hash.getSHA256(mtab);
                synchronized (FILESTORECACHE) {
                    final List<FileStore> cache = FILESTORECACHE.get(cacheID);
                    if (cache != null) {
                        LogV3.logger(Files17.class).info("getFileStores|cached|size:" + cache.size());
                        return cache;
                    } else {
                        final List<FileStore> fileStores = new ArrayList<FileStore>();
                        final long startTimeStamp = Time.systemIndependentCurrentJVMTimeMillis();
                        try {
                            for (final FileStore fileStore : fileSystem.getFileStores()) {
                                fileStores.add(fileStore);
                            }
                        } finally {
                            LogV3.logger(Files17.class).info("getFileStores|duration:" + (Time.systemIndependentCurrentJVMTimeMillis() - startTimeStamp) + "|size:" + fileStores.size());
                        }
                        FILESTORECACHE.clear();
                        FILESTORECACHE.put(cacheID, fileStores);
                        return fileStores;
                    }
                }
            }
        }
        return fileSystem.getFileStores();
    }

    public static FileStore getFileStore(final Path path) throws IOException {
        try {
            return Files.getFileStore(path);
        } catch (final FileSystemException fse) {
            // https://bugs.openjdk.java.net/browse/JDK-8165852
            // https://bugs.openjdk.java.net/browse/JDK-8166162
            if (CrossSystem.isWindows() && (path.getNameCount() == 0 || (path.getNameCount() == 1 && !Files.isDirectory(path)))) {
                // workaround for subst drive
                // FileStore doesn't work on subst root drive or file in subst root drive
                try {
                    Stream<Path> pathStream = null;
                    try {
                        if (!Files.isDirectory(path)) {
                            pathStream = Files.list(path.getParent());
                        } else {
                            pathStream = Files.list(path);
                        }
                        final Iterator<Path> it = pathStream.iterator();
                        while (it.hasNext()) {
                            final Path next = it.next();
                            if (Files.isDirectory(next)) {
                                final FileStore ret = Files.getFileStore(next);
                                return ret;
                            }
                        }
                    } finally {
                        if (pathStream != null) {
                            pathStream.close();
                        }
                    }
                } catch (final IOException io) {
                    throw Exceptions.addSuppressed(fse, io);
                }
                return null;
            }
            throw fse;
        }
    }

    public static Path guessRoot(final Path path) throws IOException {
        FileStore lastFileStore = null;
        Path lastPath = null;
        Path currentPath = path;
        while (currentPath != null) {
            if (Files.exists(currentPath)) {
                final FileStore fileStore = getFileStore(currentPath);
                if (lastFileStore == null || lastFileStore.equals(fileStore)) {
                    lastPath = currentPath;
                    lastFileStore = fileStore;
                } else {
                    // FileStore has changed -> different drive/mountpoint -> lastFile/lastFileStore are what we were looking
                    // for
                    break;
                }
            }
            currentPath = currentPath.getParent();
        }
        if (lastFileStore != null) {
            return lastPath;
        } else {
            return null;
        }
    }

    protected static boolean deleteIfExists(final File file) throws IOException {
        return deleteIfExists(file.toPath());
    }

    public static boolean deleteIfExists(final Path path) throws IOException {
        try {
            return java.nio.file.Files.deleteIfExists(path);
        } catch (ClosedByInterruptException e) {
            throw e;
        } catch (final IOException e) {
            if (path.toFile().setWritable(true)) {
                return java.nio.file.Files.deleteIfExists(path);
            } else {
                throw e;
            }
        }
    }

    public static Long getUsableSpace(final Path path) throws IOException {
        Path currentPath = path;
        while (currentPath != null) {
            if (Files.exists(currentPath)) {
                final FileStore fileStore = getFileStore(currentPath);
                if (fileStore != null) {
                    return fileStore.getUsableSpace();
                } else {
                    break;
                }
            } else {
                currentPath = currentPath.getParent();
            }
        }
        return null;
    }
}
