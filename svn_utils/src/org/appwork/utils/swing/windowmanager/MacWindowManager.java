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

import java.awt.Window;
import java.lang.reflect.InvocationTargetException;

import org.appwork.utils.ReflectionUtils;

/**
 * @author Thomas
 *
 */
public class MacWindowManager extends WindowsWindowManager {

    protected static boolean REQUESTFOREGROUND_SUPPORTED = true;

    protected void initForegroundLock() {
    }

    // @Override
    protected boolean setFocusableWindowState(Window w, boolean b) {
        // creates strange assertions in mac native NSWIndow _changejustmain code.-->thus overridden
        // tested in mac 10.12.2 and the "do not focus new windows" code works fine anyway
        return true;
    }

    //
    // /*
    // * (non-Javadoc)
    // *
    // * @see org.appwork.utils.swing.windowmanager.WindowsWindowManager#putOffscreen(java.awt.Window,
    // * org.appwork.utils.swing.windowmanager.WindowManager.FrameState)
    // */
    @Override
    protected void putOffscreen(Window w, FrameState state) {
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
            setAlwaysOnTop(w, true);
            toFrontAltWorkaround(w, true);
            if (REQUESTFOREGROUND_SUPPORTED) {
                try {
                    final Object application = ReflectionUtils.invoke("com.apple.eawt.Application", "getApplication", null, Class.forName("com.apple.eawt.Application"), new Class[] {}, new Object[] {});
                    ReflectionUtils.invoke("com.apple.eawt.Application", "requestForeground", application, void.class, new Class[] { boolean.class }, new Object[] { true });
                } catch (NoSuchMethodError ignore) {
                    // (very)old MacOS
                    REQUESTFOREGROUND_SUPPORTED = false;
                } catch (IllegalAccessError ignore) {
                    // TODO: Taskbar.requestUserAttention, JDK9
                    // JDK16, --illegal-access=deny
                    REQUESTFOREGROUND_SUPPORTED = false;
                } catch (ClassNotFoundException e) {
                    REQUESTFOREGROUND_SUPPORTED = false;
                } catch (SecurityException e) {
                    REQUESTFOREGROUND_SUPPORTED = false;
                } catch (InvocationTargetException e) {
                    REQUESTFOREGROUND_SUPPORTED = false;
                }
            }
            // requestFocus(w);
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
}
