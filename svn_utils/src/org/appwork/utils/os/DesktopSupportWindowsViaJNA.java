package org.appwork.utils.os;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.utils.processes.ProcessBuilderFactory;

public class DesktopSupportWindowsViaJNA extends DesktopSupportWindows {
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
                        if (selectPath.length() > 259) {
                            /*
                             * explorer.exe /select cannot select files whose full path exceeds MAX_PATH (260) and does not accept the
                             * "\\?\" prefix. Fall back to the 8.3 short path (below MAX_PATH), which explorer can select.
                             */
                            final String shortPath = WindowsUtils.getWindowsShortPath(file);
                            if (shortPath != null && shortPath.length() <= 259) {
                                selectPath = shortPath;
                            } else {
                                /*
                                 * No usable short path (e.g. 8.3 name generation disabled) -> open the parent folder without selecting.
                                 */
                                selectPath = null;
                            }
                        }
                        if (selectPath != null) {
                            new ProcessBuilder("cmd", "/c", "explorer /select,\"" + selectPath + "\"").start();
                        } else {
                            /* No short path available -> Fallback */
                            new ProcessBuilder("cmd", "/c", "explorer \"" + file.getParent() + "\"").start();
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
                    resultReference.wait(1000);
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
