package org.appwork.utils.os;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.utils.processes.ProcessBuilderFactory;

public class DesktopSupportWindowsViaJNA extends DesktopSupportWindows {
    /**
     * Windows MAX_PATH limit (drive letter + path + terminating null). Paths of this length or longer cannot be handled by
     * {@code explorer.exe /select}.
     */
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
    public void openFile(final File file, final boolean tryToReuseWindows) throws IOException {
        if (!file.exists()) {
            throw new IOException("File does not exist " + file.getAbsolutePath());
        }
        if (!tryToReuseWindows) {
            super.openFile(file, tryToReuseWindows);
            return;
        }
        final String absolutePath = file.getAbsolutePath();
        try {
            /*
             * desktop.open might freeze in WDesktopPeer.open....bla on win7 java 1.7u25, so run the reveal on a separate daemon thread
             * and only wait REVEAL_TIMEOUT_MS for it before falling back below.
             */
            final AtomicReference<Object> resultReference = new AtomicReference<Object>(null);
            final Thread openThread = new Thread("openFile:" + file) {
                {
                    setDaemon(true);
                }

                @Override
                public void run() {
                    try {
                        revealInExplorer(file, absolutePath);
                        synchronized (resultReference) {
                            resultReference.set(Boolean.TRUE);
                        }
                    } catch (final IOException e) {
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
            Object result;
            synchronized (resultReference) {
                if (resultReference.get() == null) {
                    resultReference.wait(REVEAL_TIMEOUT_MS);
                }
                result = resultReference.get();
            }
            if (result instanceof Exception) {
                throw (Exception) result;
            }
            if (Boolean.TRUE.equals(result)) {
                return;
            }
            /* Fallback 1: the reveal worker timed out (no result within REVEAL_TIMEOUT_MS) -> at least try to open the file via "start". */
            ProcessBuilderFactory.create("cmd", "/c", "start", "/B", " ", canonicalPathWithinMaxPathOrThrow(file, null)).start();
        } catch (final Exception e) {
            /* Last-resort fallback: open the file itself via the shell's FileProtocolHandler. */
            ProcessBuilderFactory.create("rundll32.exe", "url.dll,FileProtocolHandler", canonicalPathWithinMaxPathOrThrow(file, e)).start();
        }
    }

    /**
     * Reveals the given file in a new Explorer window and selects it. </br>
     * Uses the {@code cmd /c} indirection because explorer.exe seems to do some strange parameter parsing. Must be run off the calling
     * thread by the caller because the underlying calls may freeze (see {@link #openFile(File, boolean)}).
     *
     * @param file
     *            the file to reveal
     * @param absolutePath
     *            {@code file.getAbsolutePath()}, passed in so it is computed only once
     */
    private static void revealInExplorer(final File file, final String absolutePath) throws IOException {
        if (absolutePath.length() < MAX_PATH) {
            /* Path within MAX_PATH -> explorer.exe /select can select the file directly. */
            new ProcessBuilder("cmd", "/c", "explorer /select,\"" + absolutePath + "\"").start();
            return;
        }
        if (WindowsUtils.openFolderAndSelectItem(file)) {
            /*
             * Path exceeds MAX_PATH: explorer.exe /select cannot select such files and does not accept the "\\?\" prefix. MAX_PATH-safe
             * reveal via the Shell COM API (SHParseDisplayName + SHOpenFolderAndSelectItems) instead. Works without an 8.3 short name,
             * which is why it is preferred on Windows 11. Done - no explorer.exe call needed.
             */
            return;
        }
        /*
         * Shell COM reveal failed -> fall back to opening the parent folder without selecting the file. A drive root has no parent -> open
         * the file's own location directly.
         */
        final File parent = file.getAbsoluteFile().getParentFile();
        final String folderToOpen = parent != null ? parent.getAbsolutePath() : absolutePath;
        new ProcessBuilder("cmd", "/c", "explorer \"" + folderToOpen + "\"").start();
    }

    /**
     * Returns {@code file.getCanonicalPath()} if it stays within MAX_PATH. </br>
     * The path-based shell fallbacks ({@code start} / FileProtocolHandler) go through the classic Win32/ShellExecute layer, which is
     * limited to MAX_PATH and does not accept the "\\?\" prefix. Beyond the limit they would launch the associated application only for it
     * to report an error like "File not found", so callers must not use them for such paths.
     *
     * @param cause
     *            the failure that led here, or {@code null}; used as the cause / rethrown when the path is too long
     * @throws IOException
     *             if the canonical path reaches or exceeds MAX_PATH (rethrows {@code cause} if it already is an {@link IOException})
     */
    private static String canonicalPathWithinMaxPathOrThrow(final File file, final Exception cause) throws IOException {
        final String canonicalPath = file.getCanonicalPath();
        if (canonicalPath.length() < MAX_PATH) {
            return canonicalPath;
        }
        /* Path too long -> the MAX_PATH-bound shell fallback cannot help; report the failure instead of opening the wrong thing. */
        if (cause instanceof IOException) {
            throw (IOException) cause;
        }
        throw new IOException("File path exceeds MAX_PATH, cannot open: " + file.getAbsolutePath(), cause);
    }
}
