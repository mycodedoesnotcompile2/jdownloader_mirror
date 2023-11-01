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
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.ArrayList;

import org.appwork.utils.Application;
import org.appwork.utils.Hash;

import com.sun.nio.file.ExtendedWatchEventModifier;

/**
 * @author $Author: unknown$
 * 
 */
public abstract class J7FileObserver implements Runnable {

    public static void main(final String s[]) throws IOException, InterruptedException {

        final J7FileObserver o = new J7FileObserver("file.txt", null) {

            @Override
            public void onFound(final File file) {
                System.out.println("Found " + file);
            }
        };

        o.start();

        Thread.sleep(150000);
        o.stop();

    }

    private Thread              runner;
    private final String        filename;
    private final String        hash;
    private java.util.List<WatchKey> keys;

    /**
     * @param string
     * @param object
     */
    public J7FileObserver(final String name, final String hash) {
        filename = name;
        this.hash = hash;
        if (Application.getJavaVersion() < Application.JAVA17 /*
                                                     * ||
                                                     * !CrossSystem.isWindows()
                                                     */) { throw new IllegalStateException("This Class is Java 1.7 and Windows only"); }
    }

    public abstract void onFound(File file);

    @Override
    public void run() {
        WatchService watcher;
        keys = new ArrayList<WatchKey>();
        try {
            watcher = FileSystems.getDefault().newWatchService();

            for (final Path next : FileSystems.getDefault().getRootDirectories()) {
                try {
                    keys.add(next.register(watcher, new WatchEvent.Kind[] { java.nio.file.StandardWatchEventKinds.ENTRY_CREATE }, ExtendedWatchEventModifier.FILE_TREE));

                } catch (final Throwable e) {

                }
            }

            for (;;) {

                // wait for key to be signaled
                WatchKey key;
                try {
                    key = watcher.take();
                } catch (final InterruptedException x) {
                    return;
                }

                for (final WatchEvent<?> event : key.pollEvents()) {
                    final WatchEvent.Kind<?> kind = event.kind();

                    // This key is registered only for ENTRY_CREATE events,
                    // but an OVERFLOW event can occur regardless if events are
                    // lost or discarded.
                    if (kind == StandardWatchEventKinds.OVERFLOW) {
                        continue;
                    }

                    // The filename is the context of the event.
                    final WatchEvent<Path> ev = (WatchEvent<Path>) event;
                    final Path filename = ev.context();
                    final Path abp = ((Path) key.watchable()).resolve(filename);
                    // System.out.println(this.filename + " Created abp " +
                    // abp);
                    if (abp.getFileName().toString().equals(this.filename)) {
                        for (int i = 0; i < 5; i++) {

                            // avoid java.io.FileNotFoundException:
                            // C:\test\Bilder\1312445939619.tmp (Der Prozess
                            // kann nicht auf die Datei zugreifen, da sie
                            // von
                            // einem anderen Prozess verwendet wird)

                            final String localHash = Hash.getMD5(abp.toFile());
                            if (localHash == null && hash != null) {
                                try {
                                    Thread.sleep(200);
                                } catch (final InterruptedException e) {                                    
                                    e.printStackTrace();
                                }
                                continue;
                            }
                            System.out.println(hash + " - " + localHash);
                            if (hash == null || hash.equals(localHash)) {
                                onFound(abp.toFile());
                                return;
                            }

                        }
                    }
                    if (!key.reset()) {
                        System.out.println("Key " + key + " is invalid" + key.watchable());
                    }

                }

            }
        } catch (final IOException e) {
            org.appwork.loggingv3.LogV3.log(e);

        }

    }

    /**
     * 
     */
    public void start() {
        if (runner != null) { throw new IllegalStateException("Already running"); }
        runner = new Thread(this);
        runner.start();
    }

    /**
     * 
     */
    public void stop() {
        if (runner != null) {
            runner.interrupt();
            runner = null;
        }
        for (final WatchKey w : keys) {
            w.cancel();
        }

    }
}
