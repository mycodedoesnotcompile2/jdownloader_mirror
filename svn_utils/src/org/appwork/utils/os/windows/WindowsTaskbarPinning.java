/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
 *         Germany
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.os.windows;

import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.JNAHelper;
import org.appwork.jna.windows.JNAOptions;
import org.appwork.jna.windows.Shell32Ext;
import org.appwork.jna.windows.interfaces.IPropertyStore;
import org.appwork.jna.windows.structs.PROPERTYKEY;
import org.appwork.jna.windows.structs.PROPVARIANT;
import org.appwork.loggingv3.LogV3;
import org.appwork.processes.ProcessHandlerFactory;
import org.appwork.processes.ProcessInfo;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.images.JNAImageHelper;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.windows.execute.WindowsJobObject;

import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.Guid;
import com.sun.jna.platform.win32.Ole32;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WTypes;
import com.sun.jna.platform.win32.COM.Unknown;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinDef.HICON;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.platform.win32.WinError;
import com.sun.jna.platform.win32.WinNT.HRESULT;
import com.sun.jna.platform.win32.WinUser;
import com.sun.jna.platform.win32.COM.COMUtils;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

/**
 * Sets Windows AppUserModel relaunch properties on a window HWND so taskbar pinning starts a defined command (not the raw exe).
 * <p>
 * All public entry points swallow {@link Throwable} and only log — must never crash the host application.
 */
public class WindowsTaskbarPinning {
    private static final String LOG                                = "WindowsTaskbarPinning: ";
    private static final int    PID_RELAUNCH_COMMAND               = 2;
    private static final int    PID_RELAUNCH_ICON_RESOURCE         = 3;
    private static final int    PID_RELAUNCH_DISPLAY_NAME_RESOURCE = 4;
    private static final int    PID_APP_USER_MODEL_ID              = 5;
    private static final String PKEY_APP_USER_MODEL_FMTID          = "{9F4C2855-9F79-4B39-A8D0-E1D42DE1D5F3}";

    private WindowsTaskbarPinning() {
    }

    /**
     * Sets relaunch command / display name / optional icon on {@code hwnd}. Does not change AppUserModelID.
     */
    public static void configureTaskbarPinning(final HWND hwnd, final String relaunchCommand, final String displayName, final String iconResource) {
        configureTaskbarPinning(hwnd, relaunchCommand, displayName, iconResource, null);
    }

    /**
     * @param appUserModelId
     *            when non-empty, sets {@code System.AppUserModel.ID}; otherwise leaves any existing ID alone
     */
    public static void configureTaskbarPinning(final HWND hwnd, final String relaunchCommand, final String displayName, final String iconResource, final String appUserModelId) {
        try {
            configureTaskbarPinningImpl(hwnd, relaunchCommand, displayName, iconResource, appUserModelId, true);
        } catch (final Throwable e) {
            LogV3.info(LOG + "configureTaskbarPinning failed (swallowed)");
            LogV3.log(e);
        }
    }

    private static void configureTaskbarPinningImpl(final HWND hwnd, final String relaunchCommand, final String displayName, final String iconResource, final String appUserModelId, final boolean applyWindowIcon) {
        if (!CrossSystem.isWindows() || !JNAHelper.isJNAAvailable()) {
            LogV3.info(LOG + "configure skipped (not Windows/JNA)");
            return;
        }
        if (hwnd == null || StringUtils.isEmpty(relaunchCommand)) {
            LogV3.info(LOG + "configure skipped (hwnd=" + hwnd + " relaunchEmpty=" + StringUtils.isEmpty(relaunchCommand) + ")");
            return;
        }
        long hwndVal = 0L;
        try {
            hwndVal = Pointer.nativeValue(hwnd.getPointer());
        } catch (final Throwable ignored) {
        }
        final String title = readWindowTitle(hwnd);
        LogV3.info(LOG + "configure HWND=0x" + Long.toHexString(hwndVal) + " title=\"" + title + "\" displayName=\"" + displayName + "\" icon=\"" + iconResource + "\" relaunch=" + relaunchCommand);
        boolean coUninit = false;
        HRESULT initHr = null;
        try {
            initHr = Ole32.INSTANCE.CoInitializeEx(null, Ole32.COINIT_APARTMENTTHREADED);
        } catch (final Throwable e) {
            LogV3.info(LOG + "CoInitializeEx threw");
            LogV3.log(e);
            return;
        }
        final int initCode = initHr != null ? initHr.intValue() : WinError.E_FAIL;
        if (initCode == COMUtils.S_OK || initCode == COMUtils.S_FALSE) {
            coUninit = true;
            LogV3.info(LOG + "CoInitializeEx ok (0x" + Integer.toHexString(initCode) + ")");
        } else if (initCode != WinError.RPC_E_CHANGED_MODE) {
            LogV3.info(LOG + "CoInitializeEx failed: 0x" + Integer.toHexString(initCode));
            return;
        } else {
            LogV3.info(LOG + "CoInitializeEx RPC_E_CHANGED_MODE (continue without CoUninitialize)");
        }
        final PointerByReference ppv = new PointerByReference();
        IPropertyStore store = null;
        try {
            final Guid.IID iid = IPropertyStore.IID;
            final HRESULT getHr = Shell32Ext.INSTANCE.SHGetPropertyStoreForWindow(hwnd, new Guid.REFIID(iid), ppv);
            if (!COMUtils.SUCCEEDED(getHr) || ppv.getValue() == null) {
                LogV3.info(LOG + "SHGetPropertyStoreForWindow failed: 0x" + Integer.toHexString(getHr != null ? getHr.intValue() : -1) + " for HWND=0x" + Long.toHexString(hwndVal));
                return;
            }
            store = new IPropertyStore(ppv.getValue());
            LogV3.info(LOG + "got IPropertyStore for HWND=0x" + Long.toHexString(hwndVal));
            final PROPVARIANT relaunchPv = PROPVARIANT.ofLpWStr(relaunchCommand);
            setLpWStr(store, PID_RELAUNCH_COMMAND, "RelaunchCommand", relaunchPv);
            final PROPVARIANT displayPv = StringUtils.isNotEmpty(displayName) ? PROPVARIANT.ofLpWStr(displayName) : null;
            if (displayPv != null) {
                setLpWStr(store, PID_RELAUNCH_DISPLAY_NAME_RESOURCE, "RelaunchDisplayNameResource", displayPv);
            } else {
                LogV3.info(LOG + "skip RelaunchDisplayNameResource (empty)");
            }
            // The job watcher sets the icon later. The first HICON is often the class default (a small window glyph), not the icon the app
            // applies after show.
            final String windowIcon = applyWindowIcon ? windowIconResource(hwnd, appUserModelId) : null;
            final String effectiveIcon = StringUtils.isNotEmpty(windowIcon) ? windowIcon : (applyWindowIcon ? iconResource : null);
            if (!applyWindowIcon) {
                LogV3.info(LOG + "RelaunchIconResource deferred until the target window icon is stable");
            } else if (StringUtils.isNotEmpty(windowIcon)) {
                LogV3.info(LOG + "RelaunchIconResource from target window icon: " + windowIcon);
            }
            final PROPVARIANT iconPv = StringUtils.isNotEmpty(effectiveIcon) ? PROPVARIANT.ofLpWStr(effectiveIcon) : null;
            if (iconPv != null) {
                setLpWStr(store, PID_RELAUNCH_ICON_RESOURCE, "RelaunchIconResource", iconPv);
            } else if (applyWindowIcon) {
                LogV3.info(LOG + "skip RelaunchIconResource (no window icon and no fallback)");
            }
            final PROPVARIANT aumidPv = StringUtils.isNotEmpty(appUserModelId) ? PROPVARIANT.ofLpWStr(appUserModelId) : null;
            if (aumidPv != null) {
                // Without an explicit AppUserModelID, Windows often ignores RelaunchCommand and pins the process image (e.g. electron.exe).
                setLpWStr(store, PID_APP_USER_MODEL_ID, "AppUserModelID", aumidPv);
            } else {
                LogV3.info(LOG + "WARNING: no AppUserModelID provided — RelaunchCommand may be ignored for pinning");
            }
            final HRESULT commitHr = store.Commit();
            if (!COMUtils.SUCCEEDED(commitHr)) {
                LogV3.info(LOG + "Commit FAILED: 0x" + Integer.toHexString(commitHr != null ? commitHr.intValue() : -1) + " HWND=0x" + Long.toHexString(hwndVal));
            } else {
                LogV3.info(LOG + "Commit OK HWND=0x" + Long.toHexString(hwndVal) + " title=\"" + title + "\"");
                verifyProperty(store, PID_APP_USER_MODEL_ID, "AppUserModelID", appUserModelId);
                verifyProperty(store, PID_RELAUNCH_COMMAND, "RelaunchCommand", relaunchCommand);
                verifyProperty(store, PID_RELAUNCH_DISPLAY_NAME_RESOURCE, "RelaunchDisplayNameResource", displayName);
                verifyProperty(store, PID_RELAUNCH_ICON_RESOURCE, "RelaunchIconResource", effectiveIcon);
            }
        } catch (final Throwable e) {
            LogV3.info(LOG + "configure exception for HWND=0x" + Long.toHexString(hwndVal));
            LogV3.log(e);
        } finally {
            if (store != null) {
                try {
                    store.Release();
                } catch (final Throwable ignored) {
                }
            }
            if (coUninit) {
                try {
                    Ole32.INSTANCE.CoUninitialize();
                } catch (final Throwable ignored) {
                }
            }
        }
    }

    private static void setLpWStr(final IPropertyStore store, final int propertyId, final String name, final PROPVARIANT pv) {
        try {
            final PROPERTYKEY key = new PROPERTYKEY(new Guid.GUID(PKEY_APP_USER_MODEL_FMTID), propertyId);
            final HRESULT hr = store.SetValue(key, pv);
            if (!COMUtils.SUCCEEDED(hr)) {
                LogV3.info(LOG + "SetValue " + name + " (pid=" + propertyId + ") FAILED: 0x" + Integer.toHexString(hr != null ? hr.intValue() : -1));
            } else {
                LogV3.info(LOG + "SetValue " + name + " (pid=" + propertyId + ") OK");
            }
        } catch (final Throwable e) {
            LogV3.info(LOG + "SetValue " + name + " threw");
            LogV3.log(e);
        }
    }

    private static void verifyProperty(final IPropertyStore store, final int propertyId, final String name, final String expected) {
        if (StringUtils.isEmpty(expected)) {
            return;
        }
        try {
            final PROPERTYKEY key = new PROPERTYKEY(new Guid.GUID(PKEY_APP_USER_MODEL_FMTID), propertyId);
            final PROPVARIANT out = new PROPVARIANT();
            final HRESULT hr = store.GetValue(key, out);
            if (!COMUtils.SUCCEEDED(hr)) {
                LogV3.info(LOG + "GetValue " + name + " FAILED: 0x" + Integer.toHexString(hr != null ? hr.intValue() : -1));
                return;
            }
            final String got = out.readLpWStr();
            final boolean match = expected.equals(got);
            LogV3.info(LOG + "GetValue " + name + " match=" + match + " vt=" + out.vt + " value=\"" + got + "\"");
        } catch (final Throwable e) {
            LogV3.info(LOG + "GetValue " + name + " exception");
            LogV3.log(e);
        }
    }

    public static Thread startWatchJobAsync(final WindowsJobObject job, final long timeoutMs, final long pollIntervalMs, final String relaunchCommand, final String displayName, final String iconResource) {
        return startWatchJobAsync(job, timeoutMs, pollIntervalMs, relaunchCommand, displayName, iconResource, null);
    }

    /**
     * @param appUserModelId
     *            explicit AppUserModelID to set on windows (required for Windows to honor RelaunchCommand when pinning). Prefer a
     *            Connect-owned ID so the pin is not bound to electron.exe / the host process image.
     */
    public static Thread startWatchJobAsync(final WindowsJobObject job, final long timeoutMs, final long pollIntervalMs, final String relaunchCommand, final String displayName, final String iconResource, final String appUserModelId) {
        try {
            return startWatchJobAsyncImpl(job, timeoutMs, pollIntervalMs, relaunchCommand, displayName, iconResource, appUserModelId);
        } catch (final Throwable e) {
            LogV3.info(LOG + "startWatchJobAsync failed (swallowed)");
            LogV3.log(e);
            closeJobQuietly(job);
            return null;
        }
    }

    private static Thread startWatchJobAsyncImpl(final WindowsJobObject job, final long timeoutMs, final long pollIntervalMs, final String relaunchCommand, final String displayName, final String iconResource, final String appUserModelId) {
        if (!CrossSystem.isWindows() || !JNAHelper.isJNAAvailable()) {
            LogV3.info(LOG + "startWatchJobAsync skipped (not Windows/JNA)");
            closeJobQuietly(job);
            return null;
        }
        if (job == null || !job.isOpen() || StringUtils.isEmpty(relaunchCommand) || timeoutMs <= 0) {
            LogV3.info(LOG + "startWatchJobAsync skipped (jobOpen=" + (job != null && job.isOpen()) + " relaunchEmpty=" + StringUtils.isEmpty(relaunchCommand) + " timeoutMs=" + timeoutMs + ")");
            closeJobQuietly(job);
            return null;
        }
        final long poll = pollIntervalMs > 0 ? pollIntervalMs : 500L;
        LogV3.info(LOG + "startWatchJobAsync timeoutMs=" + timeoutMs + " pollMs=" + poll + " aumid=\"" + appUserModelId + "\" displayName=\"" + displayName + "\" icon=\"" + iconResource + "\" relaunch=" + relaunchCommand);
        final Thread t = new Thread("WindowsTaskbarPinning-job") {
            @Override
            public void run() {
                try {
                    runWatchLoop(job, timeoutMs, poll, relaunchCommand, displayName, iconResource, appUserModelId);
                } catch (final Throwable e) {
                    LogV3.info(LOG + "watch thread crashed (swallowed)");
                    LogV3.log(e);
                } finally {
                    closeJobQuietly(job);
                }
            }
        };
        t.setDaemon(true);
        try {
            t.start();
        } catch (final Throwable e) {
            LogV3.info(LOG + "failed to start watch thread");
            LogV3.log(e);
            closeJobQuietly(job);
            return null;
        }
        return t;
    }

    /** Per-PID icon sample. Relaunch command is set once; the icon is published only after it stops changing. */
    private static final class IconWatch {
        HWND    hwnd;
        int     hash;
        int     stable;
        int     defaultPolls;
        boolean settled;
        boolean loggedDefault;
    }

    private static void runWatchLoop(final WindowsJobObject job, final long timeoutMs, final long poll, final String relaunchCommand, final String displayName, final String iconResource, final String appUserModelId) {
        final long deadline = System.currentTimeMillis() + timeoutMs;
        final Set<Integer> configuredPids = new HashSet<Integer>();
        final Map<Integer, IconWatch> icons = new HashMap<Integer, IconWatch>();
        // Stop once every configured window has a stable icon and nothing new showed up. The 30s deadline stays the upper bound.
        final int idleStopAfterPolls = 6;
        int idlePolls = 0;
        int pollIndex = 0;
        LogV3.info(LOG + "watch thread started (relaunch once per PID, icon when stable)");
        // The taskbar button paints the implicit shortcut, not RelaunchIconResource. Retarget that .lnk before the first AppUserModelID commit.
        final File preparedIcon = Application.getResource("tmp/taskbar-pin/" + sanitizeIconFileName(appUserModelId) + ".ico");
        if (preparedIcon.isFile()) {
            updateImplicitShortcutIcons(relaunchCommand, preparedIcon.getAbsolutePath());
        }
        while (System.currentTimeMillis() < deadline && !Thread.currentThread().isInterrupted() && job.isOpen()) {
            pollIndex++;
            final int before = configuredPids.size();
            try {
                applyJobOnce(job, relaunchCommand, displayName, appUserModelId, configuredPids, icons, pollIndex);
            } catch (final Throwable e) {
                LogV3.info(LOG + "applyJobOnce exception poll#" + pollIndex);
                LogV3.log(e);
            }
            if (configuredPids.size() > before || !iconsSettled(configuredPids, icons)) {
                idlePolls = 0;
            } else if (configuredPids.size() > 0) {
                idlePolls++;
                if (idlePolls >= idleStopAfterPolls) {
                    LogV3.info(LOG + "early stop: icons stable, no new PID for " + idlePolls + " polls; donePids=" + configuredPids);
                    break;
                }
            }
            try {
                Thread.sleep(poll);
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            } catch (final Throwable e) {
                LogV3.log(e);
                break;
            }
        }
        LogV3.info(LOG + "watch done polls=" + pollIndex + " configuredPids=" + configuredPids + " launcherIcon=\"" + iconResource + "\"");
    }

    private static boolean iconsSettled(final Set<Integer> configuredPids, final Map<Integer, IconWatch> icons) {
        for (final Integer pid : configuredPids) {
            final IconWatch watch = icons.get(pid);
            if (watch == null || !watch.settled) {
                return false;
            }
        }
        return true;
    }

    private static void closeJobQuietly(final WindowsJobObject job) {
        if (job == null) {
            return;
        }
        try {
            job.close();
        } catch (final Throwable e) {
            LogV3.log(e);
        }
    }

    /**
     * Configures each job member PID at most once (first visible ownerless top-level HWND).
     */
    private static void applyJobOnce(final WindowsJobObject job, final String relaunchCommand, final String displayName, final String appUserModelId, final Set<Integer> configuredPids, final Map<Integer, IconWatch> icons, final int pollIndex) {
        final List<Integer> pids = job.listProcessIds();
        if (pids == null || pids.isEmpty()) {
            if (pollIndex <= 3 || pollIndex % 10 == 0) {
                LogV3.info(LOG + "poll#" + pollIndex + " job has 0 PIDs");
            }
            return;
        }
        int pendingNoHwnd = 0;
        int skippedDone = 0;
        for (final Integer pidObj : pids) {
            if (pidObj == null) {
                continue;
            }
            try {
                final int pid = pidObj.intValue();
                final List<HWND> hwnds = findMainWindows(pid);
                if (hwnds.isEmpty()) {
                    pendingNoHwnd++;
                    continue;
                }
                if (!configuredPids.contains(Integer.valueOf(pid))) {
                    final HWND hwnd = hwnds.get(0);
                    final long hwndVal = Pointer.nativeValue(hwnd.getPointer());
                    LogV3.info(LOG + "poll#" + pollIndex + " configure once pid=" + pid + " HWND=0x" + Long.toHexString(hwndVal) + " title=\"" + readWindowTitle(hwnd) + "\" exeDetail=" + describePids(Collections.singletonList(Integer.valueOf(pid))));
                    configureTaskbarPinningImpl(hwnd, relaunchCommand, displayName, null, appUserModelId, false);
                    configuredPids.add(Integer.valueOf(pid));
                    // Setting AppUserModelID can recreate the implicit shortcut from the exe icon. Point it back at the window .ico.
                    final File preparedIcon = Application.getResource("tmp/taskbar-pin/" + sanitizeIconFileName(appUserModelId) + ".ico");
                    if (preparedIcon.isFile()) {
                        updateImplicitShortcutIcons(relaunchCommand, preparedIcon.getAbsolutePath());
                    }
                } else {
                    skippedDone++;
                }
                observeWindowIcon(pid, hwnds, appUserModelId, relaunchCommand, icons);
            } catch (final Throwable e) {
                LogV3.info(LOG + "poll#" + pollIndex + " pid handling failed");
                LogV3.log(e);
            }
        }
        if (pollIndex <= 2 || configuredPids.size() > 0 && pendingNoHwnd > 0 && pollIndex % 5 == 0) {
            LogV3.info(LOG + "poll#" + pollIndex + " jobPids=" + pids.size() + " done=" + configuredPids.size() + " pendingNoHwnd=" + pendingNoHwnd + " skippedDone=" + skippedDone);
        }
    }

    private static String describePids(final List<Integer> pids) {
        final StringBuilder sb = new StringBuilder();
        try {
            final int[] arr = new int[pids.size()];
            for (int i = 0; i < pids.size(); i++) {
                arr[i] = pids.get(i).intValue();
            }
            final List<ProcessInfo> infos = ProcessHandlerFactory.getProcessHandler().listByPids(arr);
            if (infos != null) {
                for (final ProcessInfo pi : infos) {
                    if (sb.length() > 0) {
                        sb.append("; ");
                    }
                    sb.append("pid=").append(pi.getPid()).append(" exe=").append(pi.getExecutableName()).append(" path=").append(pi.getExecutablePath());
                }
            }
        } catch (final Throwable e) {
            sb.append("listByPids failed: ").append(e.getMessage());
        }
        return sb.toString();
    }

    /**
     * Visible top-level window without owner for {@code targetPID}, or null.
     */
    public static HWND findMainWindow(final int targetPID) {
        try {
            final List<HWND> all = findMainWindows(targetPID);
            return all.isEmpty() ? null : all.get(0);
        } catch (final Throwable e) {
            LogV3.log(e);
            return null;
        }
    }

    /**
     * All visible top-level windows without owner for {@code targetPID}.
     */
    public static List<HWND> findMainWindows(final int targetPID) {
        final ArrayList<HWND> found = new ArrayList<HWND>();
        try {
            if (targetPID <= 0 || !CrossSystem.isWindows() || !JNAHelper.isJNAAvailable()) {
                return found;
            }
            User32.INSTANCE.EnumWindows(new WinUser.WNDENUMPROC() {
                @Override
                public boolean callback(final HWND hWnd, final Pointer arg1) {
                    try {
                        final IntByReference processId = new IntByReference();
                        User32.INSTANCE.GetWindowThreadProcessId(hWnd, processId);
                        if (processId.getValue() != targetPID) {
                            return true;
                        }
                        final boolean visible = User32.INSTANCE.IsWindowVisible(hWnd);
                        final HWND owner = User32.INSTANCE.GetWindow(hWnd, new DWORD(WinUser.GW_OWNER));
                        final boolean noOwner = owner == null || owner.getPointer() == null || Pointer.nativeValue(owner.getPointer()) == 0L;
                        if (visible && noOwner) {
                            found.add(hWnd);
                        }
                    } catch (final Throwable e) {
                        // Never break EnumWindows / never crash the JVM from a native callback
                        LogV3.log(e);
                    }
                    return true;
                }
            }, null);
        } catch (final Throwable e) {
            LogV3.info(LOG + "findMainWindows failed for pid=" + targetPID);
            LogV3.log(e);
        }
        return found;
    }

    private static String readWindowTitle(final HWND hwnd) {
        try {
            final int len = User32.INSTANCE.GetWindowTextLength(hwnd);
            if (len <= 0) {
                return "";
            }
            final char[] buf = new char[len + 1];
            User32.INSTANCE.GetWindowText(hwnd, buf, len + 1);
            return Native.toString(buf);
        } catch (final Throwable e) {
            return "?";
        }
    }

    private static final int ICON_SMALL  = 0;
    private static final int ICON_BIG    = 1;
    private static final int ICON_SMALL2 = 2;

    /**
     * Undocumented user32 export, present since Windows 7. Returns an icon handle valid in this process. Do not destroy it: on some Windows
     * versions it is the window's live icon. Copy it before {@code JNAImageHelper.toImage}.
     */
    private interface WindowIconApi extends User32 {
        WindowIconApi INSTANCE = Native.load("user32", WindowIconApi.class, org.appwork.jna.windows.JNAOptions.SYSTEM_DLLS_ONLY);

        HICON InternalGetWindowIcon(HWND hwnd, int iconType);
    }

    /** Separate load so a missing {@code LoadIconW} binding cannot break {@link WindowIconApi}. */
    private interface StockIconApi extends com.sun.jna.win32.StdCallLibrary {
        StockIconApi INSTANCE = Native.load("user32", StockIconApi.class, org.appwork.jna.windows.JNAOptions.SYSTEM_DLLS_ONLY);

        /** {@code 32512} is {@code IDI_APPLICATION}. */
        HICON LoadIconW(Pointer hInstance, Pointer lpIconName);
    }

    private static int systemIconCoarseHash = Integer.MIN_VALUE;

    /**
     * Publishes {@code RelaunchIconResource} once the window icon is no longer the system default and has stayed the same for a few polls.
     * Electron often shows the class default first and applies the real icon shortly after.
     */
    private static void observeWindowIcon(final int pid, final List<HWND> hwnds, final String appUserModelId, final String relaunchCommand, final Map<Integer, IconWatch> icons) {
        try {
            IconWatch watch = icons.get(Integer.valueOf(pid));
            if (watch == null) {
                watch = new IconWatch();
                icons.put(Integer.valueOf(pid), watch);
            }
            if (!hwnds.isEmpty()) {
                watch.hwnd = hwnds.get(0);
            }
            BufferedImage best = null;
            int bestScore = 0;
            for (int i = 0; i < hwnds.size(); i++) {
                final BufferedImage image = customWindowImage(hwnds.get(i));
                final int score = customIconScore(image);
                if (score > bestScore) {
                    best = image;
                    bestScore = score;
                    watch.hwnd = hwnds.get(i);
                }
            }
            if (best == null) {
                watch.defaultPolls++;
                if (!watch.loggedDefault) {
                    LogV3.info(LOG + "pid=" + pid + " window icon is still the default glyph, waiting for the app icon");
                    watch.loggedDefault = true;
                }
                watch.hash = 0;
                watch.stable = 0;
                // Do not write the default glyph or the launcher icon over the window. After ~5s, leave whatever the window shows.
                if (watch.defaultPolls >= 10) {
                    watch.settled = true;
                    LogV3.info(LOG + "pid=" + pid + " leaving the window icon unchanged");
                }
                return;
            }
            final int hash = hashImage(best);
            if (hash == watch.hash) {
                watch.stable++;
            } else {
                LogV3.info(LOG + "pid=" + pid + " window icon changed " + best.getWidth() + "x" + best.getHeight());
                watch.hash = hash;
                watch.stable = 1;
                watch.settled = false;
            }
            if (!watch.settled && watch.stable >= 3) {
                final String path = writeWindowIcon(best, appUserModelId);
                if (StringUtils.isNotEmpty(path)) {
                    applyIconResource(watch.hwnd, path, relaunchCommand);
                    watch.settled = true;
                    LogV3.info(LOG + "pid=" + pid + " published stable window icon " + best.getWidth() + "x" + best.getHeight() + " " + path);
                }
            }
        } catch (final Throwable e) {
            LogV3.info(LOG + "observeWindowIcon failed pid=" + pid);
            LogV3.log(e);
        }
    }

    private static void applyIconResource(final HWND hwnd, final String iconPath, final String relaunchCommand) {
        if (hwnd == null || StringUtils.isEmpty(iconPath)) {
            return;
        }
        boolean coUninit = false;
        IPropertyStore store = null;
        try {
            final HRESULT initHr = Ole32.INSTANCE.CoInitializeEx(null, Ole32.COINIT_APARTMENTTHREADED);
            final int initCode = initHr != null ? initHr.intValue() : WinError.E_FAIL;
            if (initCode == COMUtils.S_OK || initCode == COMUtils.S_FALSE) {
                coUninit = true;
            } else if (initCode != WinError.RPC_E_CHANGED_MODE) {
                LogV3.info(LOG + "icon CoInitializeEx failed: 0x" + Integer.toHexString(initCode));
                return;
            }
            final PointerByReference ppv = new PointerByReference();
            final HRESULT getHr = Shell32Ext.INSTANCE.SHGetPropertyStoreForWindow(hwnd, new Guid.REFIID(IPropertyStore.IID), ppv);
            if (!COMUtils.SUCCEEDED(getHr) || ppv.getValue() == null) {
                LogV3.info(LOG + "icon SHGetPropertyStoreForWindow failed: 0x" + Integer.toHexString(getHr != null ? getHr.intValue() : -1));
                return;
            }
            store = new IPropertyStore(ppv.getValue());
            // Shell icon resources are "path,index". A bare .ico path is stored but does not replace an existing shortcut icon.
            final String iconResource = iconPath.indexOf(',') >= 0 ? iconPath : iconPath + ",0";
            final PROPVARIANT iconPv = PROPVARIANT.ofLpWStr(iconResource);
            setLpWStr(store, PID_RELAUNCH_ICON_RESOURCE, "RelaunchIconResource", iconPv);
            final HRESULT commitHr = store.Commit();
            if (!COMUtils.SUCCEEDED(commitHr)) {
                LogV3.info(LOG + "icon Commit FAILED: 0x" + Integer.toHexString(commitHr != null ? commitHr.intValue() : -1));
            } else {
                verifyProperty(store, PID_RELAUNCH_ICON_RESOURCE, "RelaunchIconResource", iconResource);
            }
        } catch (final Throwable e) {
            LogV3.info(LOG + "applyIconResource failed");
            LogV3.log(e);
        } finally {
            if (store != null) {
                try {
                    store.Release();
                } catch (final Throwable ignored) {
                }
            }
            // HWND property is not what the running button paints. The implicit shortcut still had vega.exe,0 after GetValue matched the .ico.
            updateImplicitShortcutIcons(relaunchCommand, iconPath);
            if (coUninit) {
                try {
                    Ole32.INSTANCE.CoUninitialize();
                } catch (final Throwable ignored) {
                }
            }
        }
    }

    /**
     * Points matching taskbar shortcuts at the window .ico. The running button uses that shortcut, not {@code RelaunchIconResource}.
     */
    private static void updateImplicitShortcutIcons(final String relaunchCommand, final String iconFile) {
        if (StringUtils.isEmpty(relaunchCommand) || StringUtils.isEmpty(iconFile)) {
            return;
        }
        final String appData = System.getenv("APPDATA");
        if (StringUtils.isEmpty(appData)) {
            return;
        }
        boolean coUninit = false;
        try {
            final HRESULT initHr = Ole32.INSTANCE.CoInitializeEx(null, Ole32.COINIT_APARTMENTTHREADED);
            final int initCode = initHr != null ? initHr.intValue() : WinError.E_FAIL;
            if (initCode == COMUtils.S_OK) {
                coUninit = true;
            } else if (initCode != COMUtils.S_FALSE && initCode != WinError.RPC_E_CHANGED_MODE) {
                LogV3.info(LOG + "shortcut CoInitializeEx failed: 0x" + Integer.toHexString(initCode));
                return;
            }
            final File pinned = new File(appData, "Microsoft/Internet Explorer/Quick Launch/User Pinned");
            final int[] counts = new int[2];
            updateShortcutIconsIn(new File(pinned, "ImplicitAppShortcuts"), relaunchCommand, iconFile, counts);
            updateShortcutIconsIn(new File(pinned, "TaskBar"), relaunchCommand, iconFile, counts);
            if (counts[0] == 0 && counts[1] == 0) {
                LogV3.info(LOG + "shortcut icon scan: no matching lnk for " + iconFile);
            }
        } catch (final Throwable e) {
            LogV3.info(LOG + "shortcut icon scan failed");
            LogV3.log(e);
        } finally {
            if (coUninit) {
                try {
                    Ole32.INSTANCE.CoUninitialize();
                } catch (final Throwable ignored) {
                }
            }
        }
    }

    /** {@code counts[0]} = icon rewritten, {@code counts[1]} = already the window .ico. */
    private static void updateShortcutIconsIn(final File dir, final String relaunchCommand, final String iconFile, final int[] counts) {
        final File[] children = dir.listFiles();
        if (children == null) {
            return;
        }
        for (int i = 0; i < children.length; i++) {
            final File child = children[i];
            if (child.isDirectory()) {
                updateShortcutIconsIn(child, relaunchCommand, iconFile, counts);
            } else if (child.isFile() && child.getName().length() > 4 && child.getName().regionMatches(true, child.getName().length() - 4, ".lnk", 0, 4)) {
                final int status = updateShortcutIconIfMatch(child, relaunchCommand, iconFile);
                if (status > 0) {
                    counts[status - 1]++;
                }
            }
        }
    }

    /** @return 0 not our shortcut, 1 icon rewritten, 2 already the window .ico */
    private static int updateShortcutIconIfMatch(final File lnk, final String relaunchCommand, final String iconFile) {
        ShellLinkW link = null;
        PersistFile persist = null;
        try {
            final PointerByReference ppv = new PointerByReference();
            final HRESULT createHr = Ole32.INSTANCE.CoCreateInstance(ShellLinkW.CLSID, null, WTypes.CLSCTX_INPROC_SERVER, ShellLinkW.IID, ppv);
            if (!COMUtils.SUCCEEDED(createHr) || ppv.getValue() == null) {
                LogV3.info(LOG + "CoCreateInstance(ShellLink) failed: 0x" + Integer.toHexString(createHr != null ? createHr.intValue() : -1));
                return 0;
            }
            link = new ShellLinkW(ppv.getValue());
            final PointerByReference persistRef = new PointerByReference();
            final HRESULT qi = link.QueryInterface(new Guid.REFIID(PersistFile.IID), persistRef);
            if (!COMUtils.SUCCEEDED(qi) || persistRef.getValue() == null) {
                LogV3.info(LOG + "IShellLink QueryInterface(IPersistFile) failed: 0x" + Integer.toHexString(qi != null ? qi.intValue() : -1));
                return 0;
            }
            persist = new PersistFile(persistRef.getValue());
            final HRESULT loadHr = persist.Load(new WString(lnk.getAbsolutePath()), 2);
            if (!COMUtils.SUCCEEDED(loadHr)) {
                return 0;
            }
            final Memory args = new Memory(2048L * 2L);
            if (!COMUtils.SUCCEEDED(link.GetArguments(args, 2048)) || !sameLaunch(relaunchCommand, args.getWideString(0))) {
                return 0;
            }
            final Memory oldIcon = new Memory(1024L * 2L);
            final IntByReference oldIndex = new IntByReference();
            String previousPath = "";
            String previous = "";
            if (COMUtils.SUCCEEDED(link.getIconLocation(oldIcon, 1024, oldIndex))) {
                previousPath = oldIcon.getWideString(0);
                previous = previousPath + "," + oldIndex.getValue();
            }
            if (iconFile.equalsIgnoreCase(previousPath) && oldIndex.getValue() == 0) {
                // Path already matches, but the shell may still be showing a failed extract of the previous file bytes.
                notifyShellItem(lnk.getAbsolutePath());
                notifyShellItem(iconFile);
                return 2;
            }
            final HRESULT setHr = link.setIconLocation(new WString(iconFile), 0);
            if (!COMUtils.SUCCEEDED(setHr)) {
                LogV3.info(LOG + "SetIconLocation failed: 0x" + Integer.toHexString(setHr != null ? setHr.intValue() : -1) + " " + lnk.getAbsolutePath());
                return 0;
            }
            final HRESULT saveHr = persist.Save(new WString(lnk.getAbsolutePath()), 1);
            if (!COMUtils.SUCCEEDED(saveHr)) {
                LogV3.info(LOG + "shortcut Save failed: 0x" + Integer.toHexString(saveHr != null ? saveHr.intValue() : -1) + " " + lnk.getAbsolutePath());
                return 0;
            }
            notifyShellItem(lnk.getAbsolutePath());
            notifyShellItem(iconFile);
            LogV3.info(LOG + "taskbar shortcut icon " + lnk.getAbsolutePath() + " " + previous + " -> " + iconFile + ",0");
            return 1;
        } catch (final Throwable e) {
            LogV3.info(LOG + "shortcut icon update failed " + lnk.getAbsolutePath());
            LogV3.log(e);
            return 0;
        } finally {
            if (persist != null) {
                try {
                    persist.Release();
                } catch (final Throwable ignored) {
                }
            }
            if (link != null) {
                try {
                    link.Release();
                } catch (final Throwable ignored) {
                }
            }
        }
    }

    private static boolean sameLaunch(final String relaunchCommand, final String shortcutArgs) {
        if (StringUtils.isEmpty(relaunchCommand) || StringUtils.isEmpty(shortcutArgs)) {
            return false;
        }
        if (relaunchCommand.indexOf(shortcutArgs) >= 0) {
            return true;
        }
        final String marker = "-launcherID ";
        final int relaunchAt = relaunchCommand.lastIndexOf(marker);
        final int shortcutAt = shortcutArgs.lastIndexOf(marker);
        if (relaunchAt < 0 || shortcutAt < 0) {
            return false;
        }
        return relaunchCommand.substring(relaunchAt).trim().equalsIgnoreCase(shortcutArgs.substring(shortcutAt).trim());
    }

    /** {@code IShellLinkW}. Vtable indexes are after {@code IUnknown}. */
    private static class ShellLinkW extends Unknown {
        static final Guid.GUID CLSID = new Guid.GUID("{00021401-0000-0000-C000-000000000046}");
        static final Guid.IID  IID   = new Guid.IID("{000214F9-0000-0000-C000-000000000046}");

        ShellLinkW(final Pointer pvInstance) {
            super(pvInstance);
        }

        /** Vtable index 10 */
        HRESULT GetArguments(final Memory buffer, final int cch) {
            return (HRESULT) this._invokeNativeObject(10, new Object[] { this.getPointer(), buffer, Integer.valueOf(cch) }, HRESULT.class);
        }

        /** Vtable index 16 */
        HRESULT getIconLocation(final Memory buffer, final int cch, final IntByReference index) {
            return (HRESULT) this._invokeNativeObject(16, new Object[] { this.getPointer(), buffer, Integer.valueOf(cch), index }, HRESULT.class);
        }

        /** Vtable index 17 */
        HRESULT setIconLocation(final WString path, final int index) {
            return (HRESULT) this._invokeNativeObject(17, new Object[] { this.getPointer(), path, Integer.valueOf(index) }, HRESULT.class);
        }
    }

    /** {@code IPersistFile}. */
    private static class PersistFile extends Unknown {
        static final Guid.IID IID = new Guid.IID("{0000010B-0000-0000-C000-000000000046}");

        PersistFile(final Pointer pvInstance) {
            super(pvInstance);
        }

        /** Vtable index 5. {@code mode} is {@code STGM_READWRITE} (2). */
        HRESULT Load(final WString path, final int mode) {
            return (HRESULT) this._invokeNativeObject(5, new Object[] { this.getPointer(), path, Integer.valueOf(mode) }, HRESULT.class);
        }

        /** Vtable index 6. {@code remember} is a COM {@code BOOL} (4 bytes), not a Java boolean. */
        HRESULT Save(final WString path, final int remember) {
            return (HRESULT) this._invokeNativeObject(6, new Object[] { this.getPointer(), path, Integer.valueOf(remember) }, HRESULT.class);
        }
    }

    private interface Shell32Notify extends StdCallLibrary {
        Shell32Notify INSTANCE = Native.load("shell32", Shell32Notify.class, JNAOptions.UNICODE_SYSTEM_DLLS_ONLY);

        int SHChangeNotify(int wEventId, int uFlags, WString item1, Pointer item2);

        int SHChangeNotify(int wEventId, int uFlags, Pointer item1, Pointer item2);
    }

    private static void notifyShellItem(final String path) {
        if (StringUtils.isEmpty(path)) {
            return;
        }
        try {
            Shell32Notify.INSTANCE.SHChangeNotify(0x00002000, 0x0005 | 0x1000, new WString(path), null);
        } catch (final Throwable e) {
            LogV3.info(LOG + "SHChangeNotify failed for " + path);
        }
    }

    /** Drops a failed extract of this .ico. The taskbar keeps that bitmap until the association cache is flushed. */
    private static void notifyIconCache() {
        try {
            Shell32Notify.INSTANCE.SHChangeNotify(0x08000000, 0x1000, (Pointer) null, (Pointer) null);
        } catch (final Throwable e) {
            LogV3.info(LOG + "icon cache notify failed");
        }
    }

    /**
     * Exports the target window's icon to a stable .ico. {@code RelaunchIconResource} can only store a path, so the pin keeps using this
     * file. Returns null when the window has no custom icon.
     * <p>
     * Do not use {@code WM_GETICON} or {@code GetClassLongPtr} here. Those return an {@code HICON} that belongs to the other process. The
     * same numeric value in Connect refers to a local icon, typically Connect's own window icon.
     */
    private static String windowIconResource(final HWND hwnd, final String appUserModelId) {
        try {
            final BufferedImage image = customWindowImage(hwnd);
            if (image == null) {
                LogV3.info(LOG + "target window has no custom icon; fallback icon resource is used");
                return null;
            }
            LogV3.info(LOG + "copied target window icon " + image.getWidth() + "x" + image.getHeight());
            return writeWindowIcon(image, appUserModelId);
        } catch (final Throwable e) {
            LogV3.info(LOG + "target window icon export failed; fallback icon resource is used");
            LogV3.log(e);
            return null;
        }
    }

    private static String writeWindowIcon(final BufferedImage image, final String appUserModelId) throws java.io.IOException {
        final String name = sanitizeIconFileName(appUserModelId);
        final File file = Application.getResource("tmp/taskbar-pin/" + name + ".ico");
        file.getParentFile().mkdirs();
        // Classic DIB, not PNG-in-ICO. The taskbar icon loader parses the directory as a bitmap and rejects the PNG payload.
        IO.secureWrite(file, dibIco(image));
        notifyShellItem(file.getAbsolutePath());
        notifyIconCache();
        return file.getAbsolutePath();
    }

    /** Largest icon that is not the system application glyph. Null when every candidate is that default. */
    private static BufferedImage customWindowImage(final HWND hwnd) {
        BufferedImage best = null;
        int bestScore = 0;
        final int[] kinds = new int[] { ICON_BIG, ICON_SMALL2, ICON_SMALL };
        for (int i = 0; i < kinds.length; i++) {
            final BufferedImage image = imageFromWindowIcon(hwnd, kinds[i]);
            final int score = customIconScore(image);
            if (score > bestScore) {
                best = image;
                bestScore = score;
            }
        }
        return best;
    }

    private static BufferedImage imageFromWindowIcon(final HWND hwnd, final int kind) {
        final HICON icon = WindowIconApi.INSTANCE.InternalGetWindowIcon(hwnd, kind);
        final Pointer pointer = icon == null ? null : icon.getPointer();
        if (pointer == null || Pointer.nativeValue(pointer) == 0L) {
            return null;
        }
        final HICON copy = User32.INSTANCE.CopyIcon(icon);
        final Pointer copyPointer = copy == null ? null : copy.getPointer();
        if (copyPointer == null || Pointer.nativeValue(copyPointer) == 0L) {
            return null;
        }
        return JNAImageHelper.toImage(copy);
    }

    private static int customIconScore(final BufferedImage image) {
        if (image == null || image.getWidth() <= 0 || image.getHeight() <= 0) {
            return 0;
        }
        final int systemHash = systemIconCoarseHash();
        if (systemHash != 0 && coarseHash(image) == systemHash) {
            return 0;
        }
        return image.getWidth() * image.getHeight();
    }

    private static int systemIconCoarseHash() {
        if (systemIconCoarseHash != Integer.MIN_VALUE) {
            return systemIconCoarseHash;
        }
        systemIconCoarseHash = 0;
        try {
            final HICON stock = StockIconApi.INSTANCE.LoadIconW(null, Pointer.createConstant(32512));
            final Pointer handle = stock == null ? null : stock.getPointer();
            if (handle == null || Pointer.nativeValue(handle) == 0L) {
                return 0;
            }
            // Shared stock icon. CopyIcon, then toImage consumes the copy. Do not DestroyIcon the stock handle.
            final HICON copy = User32.INSTANCE.CopyIcon(new HICON(handle));
            final Pointer copyPointer = copy == null ? null : copy.getPointer();
            if (copyPointer == null || Pointer.nativeValue(copyPointer) == 0L) {
                return 0;
            }
            final BufferedImage image = JNAImageHelper.toImage(copy);
            if (image != null) {
                systemIconCoarseHash = coarseHash(image);
            }
        } catch (final Throwable e) {
            LogV3.info(LOG + "system icon hash failed");
            LogV3.log(e);
        }
        return systemIconCoarseHash;
    }

    private static int coarseHash(final BufferedImage image) {
        final BufferedImage small = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB);
        final Graphics2D g = small.createGraphics();
        g.drawImage(image, 0, 0, 16, 16, null);
        g.dispose();
        final int[] px = new int[16 * 16];
        small.getRGB(0, 0, 16, 16, px, 0, 16);
        return Arrays.hashCode(px);
    }

    private static int hashImage(final BufferedImage image) {
        final int width = image.getWidth();
        final int height = image.getHeight();
        final int[] px = new int[width * height];
        image.getRGB(0, 0, width, height, px, 0, width);
        return Arrays.hashCode(px) * 31 + width * 17 + height;
    }

    private static String sanitizeIconFileName(final String raw) {
        if (raw == null || raw.length() == 0) {
            return "window";
        }
        final StringBuilder sb = new StringBuilder(raw.length());
        for (int i = 0; i < raw.length(); i++) {
            final char c = raw.charAt(i);
            if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '.' || c == '_' || c == '-') {
                sb.append(c);
            } else {
                sb.append('_');
            }
        }
        return sb.length() == 0 ? "window" : sb.toString();
    }

    /**
     * Several 32-bit BMP sizes in one ICO. A single 48x48 image decoded in GDI, but the taskbar asked for another size, failed, and showed the generic window icon.
     * The AND mask stays empty: with mask bits set, the shell ignored the alpha channel and drew that same broken icon.
     */
    private static byte[] dibIco(final BufferedImage image) {
        final int[] sizes = new int[] { 16, 24, 32, 48, 256 };
        final byte[][] blobs = new byte[sizes.length][];
        for (int i = 0; i < sizes.length; i++) {
            blobs[i] = dibImage(scaleIcon(image, sizes[i]));
        }
        final int count = sizes.length;
        int offset = 6 + (16 * count);
        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        out.write(0);
        out.write(0);
        out.write(1);
        out.write(0);
        out.write(count);
        out.write(0);
        for (int i = 0; i < count; i++) {
            final int size = sizes[i];
            out.write(size >= 256 ? 0 : size);
            out.write(size >= 256 ? 0 : size);
            out.write(0);
            out.write(0);
            out.write(1);
            out.write(0);
            out.write(32);
            out.write(0);
            writeIntLE(out, blobs[i].length);
            writeIntLE(out, offset);
            offset += blobs[i].length;
        }
        for (int i = 0; i < count; i++) {
            out.write(blobs[i], 0, blobs[i].length);
        }
        return out.toByteArray();
    }

    private static BufferedImage scaleIcon(final BufferedImage src, final int size) {
        if (src.getWidth() == size && src.getHeight() == size) {
            return src;
        }
        final BufferedImage out = new BufferedImage(size, size, BufferedImage.TYPE_INT_ARGB);
        final Graphics2D g = out.createGraphics();
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
        final float scale = Math.min(size / (float) src.getWidth(), size / (float) src.getHeight());
        final int w = Math.max(1, Math.round(src.getWidth() * scale));
        final int h = Math.max(1, Math.round(src.getHeight() * scale));
        g.drawImage(src, (size - w) / 2, (size - h) / 2, w, h, null);
        g.dispose();
        return out;
    }

    /** One bottom-up 32-bit DIB plus an empty AND mask. Alpha in the pixel bytes is the only transparency. */
    private static byte[] dibImage(final BufferedImage image) {
        final int width = image.getWidth();
        final int height = image.getHeight();
        final int xorSize = width * 4 * height;
        final int maskStride = ((width + 31) / 32) * 4;
        final int maskSize = maskStride * height;
        final ByteArrayOutputStream out = new ByteArrayOutputStream(40 + xorSize + maskSize);
        writeIntLE(out, 40);
        writeIntLE(out, width);
        writeIntLE(out, height * 2);
        out.write(1);
        out.write(0);
        out.write(32);
        out.write(0);
        writeIntLE(out, 0);
        writeIntLE(out, xorSize + maskSize);
        writeIntLE(out, 0);
        writeIntLE(out, 0);
        writeIntLE(out, 0);
        writeIntLE(out, 0);
        for (int y = height - 1; y >= 0; y--) {
            for (int x = 0; x < width; x++) {
                final int argb = image.getRGB(x, y);
                out.write(argb & 0xff);
                out.write((argb >> 8) & 0xff);
                out.write((argb >> 16) & 0xff);
                out.write((argb >> 24) & 0xff);
            }
        }
        for (int i = 0; i < maskSize; i++) {
            out.write(0);
        }
        return out.toByteArray();
    }

    private static void writeIntLE(final ByteArrayOutputStream out, final int value) {
        out.write(value & 0xff);
        out.write((value >> 8) & 0xff);
        out.write((value >> 16) & 0xff);
        out.write((value >> 24) & 0xff);
    }
}
