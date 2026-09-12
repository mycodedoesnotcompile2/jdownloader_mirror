package org.appwork.utils.os;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.utils.processes.ProcessBuilderFactory;

public class DesktopSupportWindowsViaJNA extends DesktopSupportWindows {
    /** Windows MAX_PATH limit (drive letter + path + terminating null). Paths of this length or longer cannot be handled by {@code explorer.exe /select}. */
    private static final int MAX_PATH          = 260;
    /** Maximum time (ms) the calling thread waits for the reveal worker before falling back to a simple open. */
    private static final int REVEAL_TIMEOUT_MS = 1000;

    public DesktopSupportWindowsViaJNA() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @see org.appwork.utils.os.DesktopSupportWindows#openFile(java.io.File, boolean)
     */
    @Override
    public void openFile(File file, boolean tryToReuseWindows) throws IOException {
        if (!file.exists()) {
            throw new IOException("File does not exist " + file.getAbsolutePath());
        } else if (!tryToReuseWindows) {
            super.openFile(file, tryToReuseWindows);
            return;
        }
        try {
            final AtomicReference<Object> resultReference = new AtomicReference<Object>(null);
            final Thread openThread = new Thread("openFile:" + file) {
                {
                    setDaemon(true);
                }

                // desktop.open might freeze in WDesktopPeer.open....bla on win7 java 1.7u25
                @Override
                public void run() {
                    try {
                        // we need to go this cmd /c way, because explorer.exe seems to
                        // do some strange parameter parsing.
                        String selectPath = file.getAbsolutePath();
                        if (selectPath.length() >= MAX_PATH) {
                            if (WindowsUtils.openFolderAndSelectItem(file)) {
                                /*
                                 * MAX_PATH-safe reveal via Shell COM API (SHParseDisplayName + SHOpenFolderAndSelectItems). Works without an
                                 * 8.3 short name, which is why it is preferred on Windows 11. Done - no explorer.exe call needed.
                                 */
                                synchronized (resultReference) {
                                    resultReference.set(Boolean.TRUE);
                                }
                                return;
                            }
                            /*
                             * Shell COM reveal failed, and explorer.exe /select cannot select files whose full path exceeds MAX_PATH (260)
                             * and does not accept the "\\?\" prefix. -> open the parent folder without selecting.
                             */
                            selectPath = null;
                        }
                        if (selectPath != null) {
                            new ProcessBuilder("cmd", "/c", "explorer /select,\"" + selectPath + "\"").start();
                        } else {
                            /* Long path without a working shell reveal -> Fallback: open the parent folder without selecting. */
                            final File parent = file.getAbsoluteFile().getParentFile();
                            if (parent != null) {
                                new ProcessBuilder("cmd", "/c", "explorer \"" + parent.getAbsolutePath() + "\"").start();
                            } else {
                                /* No parent (e.g. a drive root) -> open the file's own location directly. */
                                new ProcessBuilder("cmd", "/c", "explorer \"" + file.getAbsolutePath() + "\"").start();
                            }
                        }
                        synchronized (resultReference) {
                            resultReference.set(Boolean.TRUE);
                        }
                    } catch (IOException e) {
                        synchronized (resultReference) {
                            resultReference.set(e);
                        }
                    } finally {
                        synchronized (resultReference) {
                            resultReference.notifyAll();
                        }
                    }
                }
            };
            openThread.start();
            Object result = null;
            synchronized (resultReference) {
                result = resultReference.get();
                if (result == null) {
                    resultReference.wait(REVEAL_TIMEOUT_MS);
                    result = resultReference.get();
                }
            }
            if (result instanceof Exception) {
                throw (Exception) result;
            } else if (Boolean.TRUE.equals(result)) {
                return;
            } else {
                ProcessBuilderFactory.create("cmd", "/c", "start", "/B", " ", file.getCanonicalPath()).start();
            }
        } catch (final Exception e) {
            ProcessBuilderFactory.create("rundll32.exe", "url.dll,FileProtocolHandler", file.getCanonicalPath()).start();
        }
    }
}
