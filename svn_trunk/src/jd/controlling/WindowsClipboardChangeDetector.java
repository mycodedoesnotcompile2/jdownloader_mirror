package jd.controlling;

import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

import org.appwork.jna.windows.JNAOptions;
import org.appwork.utils.logging2.LogSource;
import org.jdownloader.settings.staticreferences.CFG_GUI;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.win32.StdCallLibrary;

import jd.controlling.ClipboardMonitoring.ClipboardHash;

public class WindowsClipboardChangeDetector extends ClipboardMonitoring.ClipboardChangeDetector {
    private interface User32 extends StdCallLibrary {
        // https://msdn.microsoft.com/en-us/library/windows/desktop/ms649042%28v=vs.85%29.aspx
        int GetClipboardSequenceNumber();

        // https://msdn.microsoft.com/en-us/library/windows/desktop/ms649041%28v=vs.85%29.aspx
        HWND GetClipboardOwner();

        // https://msdn.microsoft.com/de-de/library/windows/desktop/ms633522%28v=vs.85%29.aspx
        int GetWindowThreadProcessId(HWND hwnd, IntByReference pid);
    }

    private interface Kernel32 extends StdCallLibrary {
        public Pointer OpenProcess(int dwDesiredAccess, boolean bInheritHandle, int dwProcessId);

        boolean CloseHandle(Pointer handle);
    };

    private interface psapi extends StdCallLibrary {
        // https://msdn.microsoft.com/en-us/library/windows/desktop/ms683198%28v=vs.85%29.aspx
        int GetModuleFileNameExA(Pointer process, Pointer hModule, byte[] lpString, int nMaxCount);
    };

    private final User32         user32;
    private volatile Integer     lastClipboardSequenceNumber   = null;
    private final psapi          psapi;
    private final Kernel32       kernel32;
    private final Pattern[]      blackListPatterns;
    private final boolean        isProcessBlacklisted;
    private final LogSource      logger;
    /* Firefox and its forks update the clipboard with unchanged content (e.g. on exit) */
    private static final Pattern FIREFOX_PATTERN               = Pattern.compile(".*[\\\\/](firefox|librewolf|waterfox)\\.exe", Pattern.CASE_INSENSITIVE);
    /* true as long as the last detected change is from Firefox, from an unknown owner or was detected after a reset() */
    private volatile boolean     lastChangeNeedsContentCompare = true;

    protected WindowsClipboardChangeDetector(final AtomicReference<AtomicBoolean> skipChangeFlag, final LogSource logger) {
        super(skipChangeFlag);
        this.logger = logger;
        user32 = com.sun.jna.Native.load("user32", User32.class, JNAOptions.SYSTEM_DLLS_ONLY);
        psapi = com.sun.jna.Native.load("psapi", psapi.class, JNAOptions.SYSTEM_DLLS_ONLY);
        kernel32 = com.sun.jna.Native.load("kernel32", Kernel32.class, JNAOptions.SYSTEM_DLLS_ONLY);
        final String[] blackList = CFG_GUI.CFG.getClipboardProcessBlacklist();
        final ArrayList<Pattern> blackListPatterns = new ArrayList<Pattern>();
        if (blackList != null) {
            for (final String entry : blackList) {
                try {
                    blackListPatterns.add(Pattern.compile(entry));
                } catch (final Throwable th) {
                    logger.log(th);
                }
            }
        }
        this.blackListPatterns = blackListPatterns.toArray(new Pattern[0]);
        this.isProcessBlacklisted = this.blackListPatterns.length > 0;
    }

    // http://stackoverflow.com/questions/7521693/converting-c-sharp-to-java-jna-getmodulefilename-from-hwnd
    private final String getClipboardOwnerProcess() {
        final HWND hWnd = user32.GetClipboardOwner();
        if (hWnd == null) {
            return null;
        }
        final IntByReference pid = new IntByReference();
        user32.GetWindowThreadProcessId(hWnd, pid);
        final Pointer process = kernel32.OpenProcess(1040, false, pid.getValue());
        if (process == null) {
            return null;
        }
        try {
            final Pointer zero = new Pointer(0);
            final byte[] exePathname = new byte[1024];
            final int result = psapi.GetModuleFileNameExA(process, zero, exePathname, 512);
            if (result <= 0) {
                return null;
            }
            /* Native.toString stops at the terminating NUL, no need to cut by result (decoded length may differ from byte count) */
            return Native.toString(exePathname).trim();
        } catch (final Throwable th) {
            /* must never propagate, it would terminate the clipboard monitoring thread */
            logger.log(th);
            return null;
        } finally {
            kernel32.CloseHandle(process);
        }
    }

    @Override
    protected boolean equals(ClipboardHash clipboardHash, Object content) {
        if (lastChangeNeedsContentCompare) {
            /*
             * Due to unknown reasons, Firefox updates the clipboard with the same content on exit, so for Firefox (or an unknown owner) we
             * still have to check/compare content.
             */
            return super.equals(clipboardHash, content);
        }
        /* we use GetClipboardSequenceNumber to detect changes in Clipboard */
        return false;
    }

    /**
     * Returns true if the clipboard owner is Firefox (or a fork) or unknown. In both cases a changed sequence number alone is not
     * trustworthy and the content has to be compared. <br>
     * This is possibly only neded for Windows 11 users. <br>
     * Background: https://github.com/mozilla-firefox/firefox/blob/main/widget/windows/nsClipboard.cpp#L773
     */
    private final boolean needsContentCompare(final String ownerProcess) {
        if (ownerProcess == null || ownerProcess.length() == 0) {
            return true;
        }
        return FIREFOX_PATTERN.matcher(ownerProcess).matches();
    }

    private final boolean isProcessBlacklisted(final String process) {
        for (final Pattern blackListPattern : blackListPatterns) {
            if (blackListPattern.matcher(process).matches()) {
                return true;
            }
        }
        return false;
    }

    @Override
    protected void slowDown(Throwable e) {
        if (e instanceof IllegalStateException) {
            // java.lang.IllegalStateException: cannot open system clipboard
            reset();
        }
    }

    @Override
    protected int getCurrentWaitTimeout() {
        return 100;
    }

    @Override
    protected void restart() {
        super.restart();
    }

    @Override
    protected void reset() {
        lastClipboardSequenceNumber = null;
    }

    public void syncClipboardSequenceNumber() {
        lastClipboardSequenceNumber = user32.GetClipboardSequenceNumber();
    }

    private final boolean isChangeBlacklisted(final String process) {
        if (!isProcessBlacklisted) {
            return false;
        }
        return process != null && isProcessBlacklisted(process);
    }

    @Override
    protected CHANGE_FLAG hasChanges() {
        if (lastClipboardSequenceNumber == null) {
            try {
                synchronized (this) {
                    this.wait(5);
                }
            } catch (InterruptedException e) {
                return CHANGE_FLAG.INTERRUPTED;
            }
        }
        final int currentClipboardSequenceNumber = user32.GetClipboardSequenceNumber();
        final boolean changed = currentClipboardSequenceNumber != 0 && (lastClipboardSequenceNumber == null || currentClipboardSequenceNumber != lastClipboardSequenceNumber.intValue());
        if (changed) {
            /* detection after reset(): the sequence number is unknown, so the content may be unchanged */
            final boolean resyncDetection = lastClipboardSequenceNumber == null;
            lastClipboardSequenceNumber = currentClipboardSequenceNumber;
            /* read the owner right at detection time, it may already have changed when equals(...) is called later */
            final String ownerProcess = getClipboardOwnerProcess();
            lastChangeNeedsContentCompare = resyncDetection || needsContentCompare(ownerProcess);
            if (isChangeBlacklisted(ownerProcess)) {
                return CHANGE_FLAG.BLACKLISTED;
            }
            if (isSkipFlagSet()) {
                return CHANGE_FLAG.SKIP;
            }
            return CHANGE_FLAG.DETECTED;
        }
        try {
            synchronized (this) {
                this.wait(getCurrentWaitTimeout());
            }
            return CHANGE_FLAG.FALSE;
        } catch (InterruptedException e) {
            return CHANGE_FLAG.INTERRUPTED;
        }
    }
}
