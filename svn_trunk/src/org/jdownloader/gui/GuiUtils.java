package org.jdownloader.gui;

import java.awt.Window;
import java.lang.reflect.InvocationTargetException;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.CrossSystem.OperatingSystem;
import org.appwork.utils.swing.EDTHelper;

import com.sun.jna.Native;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef.HWND;

public class GuiUtils {
    private static final User32 lib;
    static {
        User32 loadedLib = null;
        try {
            if (CrossSystem.isWindows()) {
                loadedLib = Native.loadLibrary("user32", User32.class);
            } else {
                loadedLib = null;
            }
        } catch (final Throwable e) {
            e.printStackTrace();
        }
        lib = loadedLib;
    }

    public static void flashWindow(final Window window, boolean flashTray) {
        taskBarJava9: if (JavaVersion.getVersion().isMinimum(JavaVersion.JVM_9_0)) {
            try {
                final String className = "java.awt.Taskbar";
                final boolean isTaskbarSupported = ReflectionUtils.invoke(className, "isTaskbarSupported", null, boolean.class);
                if (!isTaskbarSupported) {
                    break taskBarJava9;
                }
                final Object taskBar = ReflectionUtils.invoke(className, "getTaskbar", null, Class.forName(className));
                user_attention_window: {
                    final Object FEATURE_USER_ATTENTION_WINDOW = Enum.valueOf((Class<Enum>) Class.forName("java.awt.Taskbar$Feature"), "USER_ATTENTION_WINDOW");
                    final boolean isUserAttentionWindowSupported = ReflectionUtils.invoke(className, "isSupported", taskBar, boolean.class, FEATURE_USER_ATTENTION_WINDOW);
                    if (!isUserAttentionWindowSupported) {
                        break user_attention_window;
                    }
                    try {
                        ReflectionUtils.invoke(taskBar.getClass(), "requestWindowUserAttention", taskBar, void.class, window);
                    } catch (InvocationTargetException e) {
                        throw e.getTargetException();
                    }
                    return;
                }
                user_attention: {
                    final Object FEATURE_USER_ATTENTION = Enum.valueOf((Class<Enum>) Class.forName("java.awt.Taskbar$Feature"), "USER_ATTENTION");
                    final boolean isUserAttentionSupported = ReflectionUtils.invoke(className, "isSupported", taskBar, boolean.class, FEATURE_USER_ATTENTION);
                    if (!isUserAttentionSupported) {
                        break user_attention;
                    }
                    try {
                        ReflectionUtils.invoke(taskBar.getClass(), "requestUserAttention", taskBar, void.class, true, false);
                    } catch (InvocationTargetException e) {
                        throw e.getTargetException();
                    }
                    return;
                }
            } catch (Throwable e) {
                LogV3.log(e);
            }
        }
        if (CrossSystem.getOS().isMinimum(OperatingSystem.WINDOWS_XP)) {
            if (lib == null) {
                return;
            }
            final User32.FLASHWINFO flash = new User32.FLASHWINFO();
            final HWND hwnd = new HWND(Native.getComponentPointer(window));
            flash.hWnd = hwnd;
            flash.uCount = 100;
            flash.dwTimeout = 1000;
            if (flashTray) {
                flash.dwFlags = User32.FLASHW_TIMERNOFG | User32.FLASHW_ALL;
            } else {
                flash.dwFlags = User32.FLASHW_STOP;
            }
            flash.cbSize = flash.size();
            lib.FlashWindowEx(flash);
        } else if (CrossSystem.isMac()) {
            try {
                final com.apple.eawt.Application application = com.apple.eawt.Application.getApplication();
                if (application != null) {
                    application.requestUserAttention(flashTray);
                }
            } catch (Throwable th) {
                LogV3.log(th);
            }
        }
    }

    /**
     * Checks whether mainFrame or its children are active or not.
     *
     * @param mainFrame
     * @return
     */
    public static boolean isActiveWindow(final Window mainFrame) {
        if (mainFrame == null) {
            return false;
        }
        return new EDTHelper<Boolean>() {
            @Override
            public Boolean edtRun() {
                // this methods does not work under windows if the jdownloader frame is flashing in task. in this case it is not really
                // active, but mainFRame.isActive returns true
                if (!mainFrame.isVisible()) {
                    System.out.println("Mainframe is invisible");
                    return false;
                }
                // frames can be active, but not visible... at least under win7 java 1.7
                if (mainFrame.isActive() && mainFrame.isVisible()) {
                    System.out.println("Mainframe is active");
                    return true;
                }
                for (final Window w : mainFrame.getOwnedWindows()) {
                    // frames can be active, but not visible... at least under win7 java 1.7
                    if (w.isActive() && w.isVisible()) {
                        System.out.println(w + " is active");
                        return true;
                    }
                }
                return false;
            }
        }.getReturnValue();
    }
}
