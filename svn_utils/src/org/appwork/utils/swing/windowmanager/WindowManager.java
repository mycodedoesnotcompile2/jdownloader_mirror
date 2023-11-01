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

import java.awt.Dialog;
import java.awt.Frame;
import java.awt.Window;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.util.Map.Entry;
import java.util.WeakHashMap;

import javax.swing.JFrame;

import org.appwork.utils.os.CrossSystem;

public abstract class WindowManager {
    public static enum FrameState {
        OS_DEFAULT,
        TO_FRONT,
        TO_BACK,
        TO_FRONT_FOCUSED;
    }

    public static enum WindowExtendedState {
        NORMAL(JFrame.NORMAL),
        MAXIMIZED_BOTH(JFrame.MAXIMIZED_BOTH),
        ICONIFIED(JFrame.ICONIFIED);

        private int id;

        public int getId() {
            return id;
        }

        private WindowExtendedState(int i) {
            this.id = i;
        }

        public static WindowExtendedState get(final int state) {
            if ((state & Frame.MAXIMIZED_BOTH) != 0) {
                return MAXIMIZED_BOTH;
            } else if ((state & Frame.ICONIFIED) != 0) {
                return ICONIFIED;
            } else {
                return NORMAL;
            }
        }
    }

    static WindowManager INSTANCE = WindowManager.createOsWindowManager();

    public static void setCustom(WindowManager instance) {
        INSTANCE = instance;
    }

    private static WindowManager createOsWindowManager() {
        if (CrossSystem.isWindows()) {
            return new WindowsWindowManager();
        } else if (CrossSystem.isUnix()) {
            return new LinuxWindowManager();
        } else if (CrossSystem.isMac()) {
            // return new WindowsWindowManager();
            return new MacWindowManager();
        } else {
            return new DefaultWindowManager();
        }
    }

    public static WindowManager getInstance() {
        return WindowManager.INSTANCE;
    }

    protected final WeakHashMap<Window, WindowMetaInfo> windowMetaMap;
    protected final WindowFocusListener                 windowFocusListener;

    public WindowExtendedState getExtendedState(final Frame w) {
        return WindowExtendedState.get(w.getExtendedState());
    }

    public boolean hasFocus(final Window window) {
        if (window != null) {
            if (window.isFocusOwner()) {
                return true;
            } else if (window.getFocusOwner() != null) {
                return true;
            } else if (window.isFocused()) {
                return true;
            } else if (window.hasFocus()) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    public void hide(final Window w) {
        this.setVisible(w, false, FrameState.OS_DEFAULT);
    }

    public void hide(final Window w, final FrameState state) {
        this.setVisible(w, false, state);
    }

    /**
     *
     */
    public static class WindowMetaInfo {
        private long lostFocus;
        private long gainedFocus;
        private long shown;

        public long getShown() {
            return shown;
        }

        public void setShown(long shown) {
            this.shown = shown;
        }

        public long getLostFocus() {
            return lostFocus;
        }

        public void setLostFocus(long lostFocus) {
            this.lostFocus = lostFocus;
        }

        public long getGainedFocus() {
            return gainedFocus;
        }

        public void setGainedFocus(long gainedFocus) {
            this.gainedFocus = gainedFocus;
        }
    }

    public Window getLatestWorkingFrameOrDialog() {
        WindowMetaInfo meta = null;
        Window window = null;
        for (final Entry<Window, WindowMetaInfo> es : windowMetaMap.entrySet()) {
            final Window w = es.getKey();
            // we are only interested in Frame or Dialog for this method, because Dialog.class only accepts Frame or dialog as parent
            if (w != null && w.isDisplayable() && (w instanceof Frame || w instanceof Dialog)) {
                final WindowMetaInfo metaInfo = es.getValue();
                if (meta == null || (Math.max(meta.getGainedFocus(), meta.getShown()) < Math.max(metaInfo.getGainedFocus(), metaInfo.getShown()))) {
                    meta = metaInfo;
                    window = w;
                }
            }
        }
        return window;
    }

    public WindowMetaInfo getMetaInfo(Window window) {
        WindowMetaInfo ret = windowMetaMap.get(window);
        if (ret == null) {
            ret = new WindowMetaInfo();
            windowMetaMap.put(window, ret);
        }
        return ret;
    }

    public WindowManager() {
        windowMetaMap = new WeakHashMap<Window, WindowMetaInfo>();
        windowFocusListener = new WindowFocusListener() {
            @Override
            public void windowLostFocus(WindowEvent e) {
                getMetaInfo(e.getWindow()).setLostFocus(System.currentTimeMillis());
            }

            @Override
            public void windowGainedFocus(WindowEvent e) {
                getMetaInfo(e.getWindow()).setGainedFocus(System.currentTimeMillis());
            }
        };
    }

    public void setExtendedState(final Frame w, final WindowExtendedState state) {
        if (state == null) {
            throw new NullPointerException("State is null");
        } else {
            switch (state) {
            case NORMAL:
                w.setExtendedState(JFrame.NORMAL);
                break;
            case ICONIFIED:
                w.setExtendedState(JFrame.ICONIFIED);
                break;
            case MAXIMIZED_BOTH:
                w.setExtendedState(JFrame.MAXIMIZED_BOTH);
                break;
            }
        }
    }

    public void setVisible(final Window w, final boolean visible) {
        this.setVisible(w, visible, FrameState.OS_DEFAULT);
    }

    abstract public void setVisible(Window w, boolean visible, FrameState state);

    abstract public void setZState(Window w, FrameState state);

    public void show(final Window w) {
        this.setVisible(w, true, FrameState.OS_DEFAULT);
    }

    public void show(final Window w, final FrameState state) {
        this.setVisible(w, true, state);
    }

    public boolean hasFocus() {
        for (final Window w : Window.getWindows()) {
            if (hasFocus(w)) {
                return true;
            }
        }
        return false;
    }
}
