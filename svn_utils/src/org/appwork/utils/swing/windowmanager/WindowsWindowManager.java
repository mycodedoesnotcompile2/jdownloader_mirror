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
package org.appwork.utils.swing.windowmanager;

import java.awt.AWTException;
import java.awt.Component;
import java.awt.Frame;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.Window;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.HierarchyBoundsListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.awt.event.WindowListener;
import java.awt.event.WindowStateListener;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.Timer;

import org.appwork.swing.ExtJFrame;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;

/**
 * @author Thomas
 *
 */
public class WindowsWindowManager extends WindowManager {
    private final AtomicReference<Robot>     robot                = new AtomicReference<Robot>(null);
    private volatile String                  blocker;
    protected HashMap<Window, ResetRunnable> runnerMap            = new HashMap<Window, ResetRunnable>();
    private int                              foregroundLock       = -1;
    private volatile boolean                 altWorkaroundEnabled = false;
    private volatile int[]                   altWorkaroundKeys    = new int[] { KeyEvent.VK_CONTROL, KeyEvent.VK_ALT, KeyEvent.VK_SHIFT };

    public boolean isAltWorkaroundEnabled() {
        return altWorkaroundEnabled;
    }

    public void setAltWorkaroundEnabled(final boolean altWorkaroundEnabled) {
        this.altWorkaroundEnabled = altWorkaroundEnabled;
    }

    public int[] getAltWorkaroundKeys() {
        return altWorkaroundKeys;
    }

    public void setAltWorkaroundKeys(final int[] altWorkaroundKeys) {
        this.altWorkaroundKeys = altWorkaroundKeys;
    }

    public String getBlocker() {
        return blocker;
    }

    public static void writeForegroundLockTimeout(final int foregroundLockTimeout) {
        try {
            final Process p = Runtime.getRuntime().exec("reg add \"HKEY_CURRENT_USER\\Control Panel\\Desktop\" /v \"ForegroundLockTimeout\" /t REG_DWORD /d 0x" + Integer.toHexString(foregroundLockTimeout) + " /f");
            IO.readInputStreamToString(p.getInputStream());
            final int exitCode = p.exitValue();
            if (exitCode == 0) {
            } else {
                throw new IOException("Reg add execution failed");
            }
        } catch (final UnsupportedEncodingException e) {            
            e.printStackTrace();
        } catch (final IOException e) {            
            e.printStackTrace();
        }
    }

    public WindowsWindowManager() {
        initForegroundLock();
    }

    protected void initForegroundLock() {
        try {
            foregroundLock = readForegroundLockTimeout();
        } catch (final Exception e) {
            // LogV3.log(e);
        }
    }

    /**
     * @return
     * @throws IOException
     * @throws UnsupportedEncodingException
     */
    public static int readForegroundLockTimeout() throws UnsupportedEncodingException, IOException {
        final String iconResult = IO.readInputStreamToString(Runtime.getRuntime().exec("reg query \"HKEY_CURRENT_USER\\Control Panel\\Desktop\" /v \"ForegroundLockTimeout\"").getInputStream());
        final Matcher matcher = Pattern.compile("ForegroundLockTimeout\\s+REG_DWORD\\s+0x(.*)").matcher(iconResult);
        matcher.find();
        final String value = matcher.group(1);
        return Integer.parseInt(value, 16);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.utils.swing.WindowManager#toFront(java.awt.Window)
     */
    @Override
    public void setZState(final Window w, final FrameState state) {
        switch (state) {
        case OS_DEFAULT:
            // do nothing
            return;
        case TO_FRONT_FOCUSED:
            // setAutoRequestFocus status seems to be not important because we
            // requestFocus below. we prefer to request focus, because
            // setAutoRequestFocus is java 1.7 only
            // setFocusableWindowState is important. if it would be false, the
            // window would not even go to front.
            if (w.hasFocus()) {
                return;
            }
            // if (true) { return; }
            WindowResetListener hasListener = findListener(w);
            ResetRunnable runner = runnerMap.get(w);
            if (runner == null) {
                runner = new ResetRunnable(this, w, hasListener);
                runnerMap.put(w, runner);
                executeAfterASecond(runner);
            }
            runner.setState(state);
            setFocusableWindowState(w, true);
            setFocusable(w, true);
            toFrontAltWorkaround(w, true);
            requestFocus(w);
            break;
        case TO_BACK:
            setAlwaysOnTop(w, false);
            toBack(w);
            break;
        default:
            hasListener = findListener(w);
            runner = runnerMap.get(w);
            if (runner == null) {
                runner = new ResetRunnable(this, w, hasListener);
                runnerMap.put(w, runner);
                executeAfterASecond(runner);
            }
            runner.setState(state);
            setFocusableWindowState(w, false);
            setAlwaysOnTop(w, true);
            toFrontAltWorkaround(w, false);
        }
    }

    /**
     * @param actionListener
     */
    protected void executeAfterASecond(final ActionListener actionListener) {
        // System.out.println("Launch timer");
        final Timer timer = new Timer(1000, actionListener);
        timer.setRepeats(false);
        timer.restart();
    }

    protected void toFrontAltWorkaround(final Window w, final boolean requestFocus) {
        try {
            // Workaround...
            // try {
            // Robot r = null;
            // windows can prevent windows from getting to front or getting
            // the focus. @see writeForegroundLockTimeout
            // we can write the foregroundLockTimeout, but this would need a
            // windows reboot.
            // how foregroundLockTimeout works:
            // if application A has the focus, and gets user input(mouse,
            // keyboard), windows considers this application as active. it
            // gets "Inactive" after the foregroundLockTimeout in ms.
            // when no application is considered as active, windows allows
            // other applications to get focus.
            // on xp this seems to be enabled by default.
            // on win 7 foregroundLockTimeout seems to be on 0 by default.
            // WORKAROUND:
            // windows uses alt+tab to switch between windows. so there must
            // be an windows API exception for this shortcut
            // it seems that pressing alt, while trying to switch the focus
            // works pretty well
            // Tested: WIN7
            // org.appwork.utils.swing.WindowsWindowManager.setVisible(Window,
            // boolean, boolean, boolean) method
            if (requestFocus && foregroundLock > 0 && isAltWorkaroundEnabled()) {
                try {
                    pressAlt();
                    toFront(w);
                    // this may flicker on linux?
                    repaint(w);
                } finally {
                    releaseAlt();
                }
            } else {
                toFront(w);
                // this may flicker on linux?
                repaint(w);
            }
        } catch (final Exception e) {
            e.printStackTrace();
            toFront(w);
            // this may flicker on linux?
            repaint(w);
        }
    }

    /**
     *
     */
    private void releaseAlt() {
        if (isAltWorkaroundEnabled()) {
            final Robot robot = this.robot.getAndSet(null);
            if (robot != null) {
                // System.out.println("key: Alt released");
                // actually, we only need to asure that alt is pressed during
                // tofront
                // this would activte the window context menu.
                // alt + shift is actually not a shortcut. only modifiers. so
                // this
                // one should not create problems
                // we probably need this workaround only if
                // foregroundtimeoutlock
                // is>0
                for (final int key : getAltWorkaroundKeys()) {
                    robot.keyRelease(key);
                }
            }
        }
    }

    /**
     * @return
     * @throws AWTException
     */
    private void pressAlt() throws AWTException {
        if (isAltWorkaroundEnabled()) {
            Robot robot = this.robot.get();
            if (robot == null) {
                robot = new Robot();
                this.robot.getAndSet(robot);
            }
            // System.out.println("key: Alt+sh pressed");
            // actually, we only need to asure that alt is pressed during
            // tofront
            // this would activte the window context menu.
            // alt + shift is actually not a shortcut. only modifiers. so this
            // one
            // should not create problems
            // we probably need this workaround only if foregroundtimeoutlock
            // is>0
            for (final int key : getAltWorkaroundKeys()) {
                robot.keyPress(key);
            }
        }
    }

    protected void repaint(final Window w) {
        // System.out.println("Call repaint ");
        // This repaint may cause a kind of flickering on some systems. we have
        // reports from windows and linux users
        // w.repaint();
    }

    protected void toFront(final Window w) {
        // System.out.println("Call toFront ");
        w.toFront();
    }

    /**
     * @param w
     * @param b
     * @return
     */
    protected boolean setAlwaysOnTop(final Window w, final boolean b) {
        final boolean ret = w.isAlwaysOnTop();
        if (b == ret) {
            return ret;
        }
        blocker = ExtJFrame.PROPERTY_ALWAYS_ON_TOP;
        try {
            // System.out.println("Call setAlwaysOnTop " + b);
            w.setAlwaysOnTop(b);
        } finally {
            blocker = null;
        }
        return ret;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.utils.swing.WindowManager#setVisible(java.awt.Window, boolean, boolean, boolean)
     */
    @Override
    public void setVisible(final Window w, final boolean visible, final FrameState state) {
        if (visible) {
            getMetaInfo(w).setShown(System.currentTimeMillis());
        }
        w.removeWindowFocusListener(windowFocusListener);
        w.addWindowFocusListener(windowFocusListener);
        if (w.isVisible() && visible) {
            setZState(w, state);
            return;
        }
        // LogV3.info(w + ".SetVisible " + visible + " " + state);
        addDebugListener(w);
        if (visible) {
            if (state == FrameState.OS_DEFAULT) {
                setVisibleInternal(w, visible);
                return;
            }
            assignWindowOpenListener(w, state);
            switch (state) {
            case TO_FRONT_FOCUSED:
                setFocusableWindowState(w, true);
                break;
            default:
                // avoid that the dialog get's focues
                setFocusableWindowState(w, false);
                setFocusable(w, false);
                putOffscreen(w, state);
            }
            setVisibleInternal(w, visible);
        } else {
            setVisibleInternal(w, false);
        }
    }

    protected void putOffscreen(final Window w, final FrameState state) {
        if (state != FrameState.TO_FRONT) {
            // on some systems, the window comes to front, even of
            // focusable and focusablewindowstate are false
            final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
            final GraphicsDevice[] screens = ge.getScreenDevices();
            final Point p = new Point(Integer.MIN_VALUE, Integer.MIN_VALUE);
            // search offscreen position
            for (final GraphicsDevice screen : screens) {
                final Rectangle bounds = screen.getDefaultConfiguration().getBounds();
                p.x = Math.max(bounds.x + bounds.width, p.x);
                p.y = Math.max(bounds.y + bounds.height, p.y);
            }
            p.x++;
            p.y++;
            setLocation(w, p);
            //
        }
    }

    /**
     * @param w
     * @param minValue
     * @param minValue2
     * @return
     */
    private Point setLocation(final Window w, final Point offscreen) {
        blocker = ExtJFrame.PROPERTY_LOCATION;
        try {
            // System.out.println("call setLocation " + offscreen);
            w.setLocation(offscreen);
        } finally {
            blocker = null;
        }
        try {
            return w.getLocationOnScreen();
        } catch (java.awt.IllegalComponentStateException e) {
            return w.getLocation();
        }
    }

    /**
     * @param w
     * @param frameExtendedState
     */
    protected void setExtendedState(final Frame w, final int frameExtendedState) {
        blocker = ExtJFrame.PROPERTY_EXTENDED_STATE;
        try {
            if (frameExtendedState == w.getExtendedState()) {
                return;
            }
            // System.out.println("Call setExtendedState " +
            // frameExtendedState);
            w.setExtendedState(frameExtendedState);
        } finally {
            blocker = null;
        }
    }

    /**
     * @param w
     * @param flags
     * @return
     */
    public WindowResetListener assignWindowOpenListener(final Window w, final FrameState state) {
        WindowResetListener hasListener = findListener(w);
        if (hasListener != null) {
            hasListener.setState(state);
        } else {
            hasListener = new WindowResetListener(this, w, state);
            hasListener.add();
        }
        return hasListener;
    }

    protected WindowResetListener findListener(final Window w) {
        WindowResetListener hasListener = null;
        for (final WindowListener wl : w.getWindowListeners()) {
            if (wl != null && wl instanceof WindowResetListener) {
                hasListener = (WindowResetListener) wl;
                break;
            }
        }
        return hasListener;
    }

    protected void addDebugListener(final Window w) {
        if (Application.isJared(WindowsWindowManager.class) || true) {
            return;
        }
        w.addWindowFocusListener(new WindowFocusListener() {
            @Override
            public void windowLostFocus(final WindowEvent windowevent) {                
                log(windowevent);
            }

            @Override
            public void windowGainedFocus(final WindowEvent windowevent) {                
                log(windowevent);
            }
        });
        w.addWindowListener(new WindowListener() {
            @Override
            public void windowOpened(final WindowEvent windowevent) {
                log(windowevent);
            }

            @Override
            public void windowIconified(final WindowEvent windowevent) {
                log(windowevent);
            }

            @Override
            public void windowDeiconified(final WindowEvent windowevent) {
                log(windowevent);
            }

            @Override
            public void windowDeactivated(final WindowEvent windowevent) {
                log(windowevent);
            }

            @Override
            public void windowClosing(final WindowEvent windowevent) {
                log(windowevent);
            }

            @Override
            public void windowClosed(final WindowEvent windowevent) {
                log(windowevent);
            }

            @Override
            public void windowActivated(final WindowEvent windowevent) {
                log(windowevent);
            }
        });
        w.addWindowStateListener(new WindowStateListener() {
            @Override
            public void windowStateChanged(final WindowEvent windowevent) {
                log(windowevent);
            }
        });
        w.addComponentListener(new ComponentListener() {
            @Override
            public void componentShown(final ComponentEvent e) {
            }

            @Override
            public void componentResized(final ComponentEvent e) {
            }

            @Override
            public void componentMoved(final ComponentEvent e) {
            }

            @Override
            public void componentHidden(final ComponentEvent e) {
            }
        });
        w.addHierarchyBoundsListener(new HierarchyBoundsListener() {
            @Override
            public void ancestorResized(final HierarchyEvent e) {
            }

            @Override
            public void ancestorMoved(final HierarchyEvent e) {
            }
        });
        w.addHierarchyListener(new HierarchyListener() {
            @Override
            public void hierarchyChanged(final HierarchyEvent e) {
            }
        });
    }

    /**
     * @param windowevent
     */
    protected void log(final WindowEvent windowevent) {
        // System.out.println(name(windowevent.getWindow()) + "." +
        // name(windowevent.getComponent()) + " " + type(windowevent.getID()) +
        // " to " + name(windowevent.getOppositeWindow()));
    }

    /**
     * @param window
     * @return
     */
    private String name(final Component window) {
        if (window == null) {
            return null;
        }
        String ret = window.getName();
        if (StringUtils.isEmpty(ret)) {
            ret = window.getClass().getName();
        }
        return ret;
    }

    /**
     * @param id
     * @return
     */
    private String type(final int id) {
        /* 389 */switch (id) {
        /* 391 */case 200: /* 391 */
            return "WINDOW_OPENED";
            /* 394 */case 201: /* 394 */
            return "WINDOW_CLOSING";
            /* 397 */case 202: /* 397 */
            return "WINDOW_CLOSED";
            /* 400 */case 203: /* 400 */
            return "WINDOW_ICONIFIED";
            /* 403 */case 204: /* 403 */
            return "WINDOW_DEICONIFIED";
            /* 406 */case 205: /* 406 */
            return "WINDOW_ACTIVATED";
            /* 409 */case 206: /* 409 */
            return "WINDOW_DEACTIVATED";
            /* 412 */case 207: /* 412 */
            return "WINDOW_GAINED_FOCUS";
            /* 415 */case 208: /* 415 */
            return "WINDOW_LOST_FOCUS";
            /* 418 */case 209: /* 418 */
            return "WINDOW_STATE_CHANGED";
            /* 421 */default:/* 421 */
            return "unknown type";
        }
    }

    /**
     * @param w
     * @param b
     */
    protected void setFocusable(final Window w, final boolean b) {
        if (w.isFocusable() == b) {
            return;
        }
        blocker = ExtJFrame.PROPERTY_FOCUSABLE;
        try {
            // LogV3.info("Call setFocusable " + b);
            w.setFocusable(b);
        } finally {
            blocker = null;
        }
    }

    /**
     * @param w
     * @param b
     * @return
     */
    protected boolean setFocusableWindowState(final Window w, final boolean b) {
        final boolean ret = w.getFocusableWindowState();
        if (ret == b) {
            return ret;
        }
        // LogV3.info("Call setFocusableWindowState " + b);
        blocker = ExtJFrame.PROPERTY_FOCUSABLE_WINDOW_STATE;
        try {
            w.setFocusableWindowState(b);
        } finally {
            blocker = null;
        }
        return ret;
    }

    /**
     * @param w
     */
    protected void toBack(final Window w) {
        // System.out.println("Call toBack ");
        w.toBack();
    }

    /**
     * @param w
     * @param b
     */
    private void setVisibleInternal(final Window w, final boolean b) {
        blocker = ExtJFrame.PROPERTY_VISIBLE;
        try {
            // WindowExtendedState ext = getExtendedState(((JFrame) w));
            // //System.out.println("Call setVisible " + b);
            // w.setVisible(false);
            // setExtendedState((JFrame) w, WindowExtendedState.MAXIMIZED_BOTH);
            // ;
            w.setVisible(b);
        } finally {
            blocker = null;
        }
    }

    /**
     * @param w
     */
    protected void requestFocus(final Window w) {
        // if a frame is active, one of its components has the focus, if we da a
        // requestfocus now, these components will lose the focus again
        if (w.getFocusOwner() != null || w.isFocusOwner() || w.isFocused()) {
            return;
        }
        // System.out.println("Call requestFocus");
        w.requestFocus();
    }

    /**
     * @param resetRunnable
     */
    public void removeTimer(final ResetRunnable resetRunnable) {
        runnerMap.remove(resetRunnable.getWindow());
    }
}
