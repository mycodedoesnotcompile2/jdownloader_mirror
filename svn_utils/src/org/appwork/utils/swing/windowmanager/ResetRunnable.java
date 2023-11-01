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

import java.awt.Component;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import org.appwork.swing.ExtJFrame;
import org.appwork.swing.PropertyStateEventProviderInterface;
import org.appwork.swing.event.PropertySetListener;
import org.appwork.utils.swing.windowmanager.WindowManager.FrameState;

/**
 * @author Thomas
 * 
 */
public class ResetRunnable implements ActionListener, PropertySetListener {

    private WindowsWindowManager windowsWindowManager;
    private Window               w;
    private FrameState           state;
    private boolean              oldFocusableWindowState;
    private boolean              oldAlwaysOnTop;
    private boolean              oldFocusable;

    public FrameState getState() {
        return state;
    }

    /**
     * @param windowsWindowManager
     * @param w
     * @param hasListener
     */
    public ResetRunnable(final WindowsWindowManager windowsWindowManager, final Window w, final WindowResetListener hasListener) {

        this.windowsWindowManager = windowsWindowManager;
        this.w = w;
        if (hasListener != null) {
            hasListener.resetProperties();
        }
        oldFocusableWindowState = w.getFocusableWindowState();
        oldAlwaysOnTop = w.isAlwaysOnTop();
        oldFocusable = w.isFocusable();

        if (w instanceof PropertyStateEventProviderInterface) {
            ((PropertyStateEventProviderInterface) w).getPropertySetEventSender().addListener(this, true);
        }

    }

    public Window getWindow() {
        return w;
    }

    /**
     * @param flags
     */
    public void setState(final FrameState flags) {
        state = flags;

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.appwork.swing.event.PropertySetListener#onPropertySet(java.awt.Component
     * , java.lang.String, java.lang.Object, java.lang.Object)
     */
    @Override
    public void onPropertySet(final Component caller, final String propertyName, final Object oldValue, final Object newValue) {
        if (propertyName == null || propertyName.equals(windowsWindowManager.getBlocker())) { return; }
        //System.out.println("Property Update: " + propertyName + " - " + newValue);
        if (ExtJFrame.PROPERTY_FOCUSABLE_WINDOW_STATE.equals(propertyName)) {
            oldFocusableWindowState = (Boolean) newValue;
        } else if (ExtJFrame.PROPERTY_FOCUSABLE.equals(propertyName)) {
            oldFocusable = (Boolean) newValue;
        } else if (ExtJFrame.PROPERTY_ALWAYS_ON_TOP.equals(propertyName)) {
            oldAlwaysOnTop = (Boolean) newValue;
        }

    };

    /*
     * (non-Javadoc)
     * 
     * @see
     * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(final ActionEvent actionevent) {
        //System.out.println("Reset After Timeout");
        if (w instanceof PropertyStateEventProviderInterface) {
            ((PropertyStateEventProviderInterface) w).getPropertySetEventSender().removeListener(ResetRunnable.this);
        }
        windowsWindowManager.removeTimer(ResetRunnable.this);


        // it is important that we
        // 1. setAlwaysOnTop back
        // 2. setFocusableWindowState back

        // else setAlwaysOnTop would fire a WINDOW_ACTIVATED and a
        // WINDOW_GAINED_FOCUS even if the window does not get
        // active or focused
        windowsWindowManager.setAlwaysOnTop(w, oldAlwaysOnTop);
        switch (getState()) {
        case TO_FRONT_FOCUSED:
        case TO_FRONT:

            // it is important to reset focus states before calling
            // toFront
            windowsWindowManager.setFocusableWindowState(w, oldFocusableWindowState);
            windowsWindowManager.setFocusable(w, oldFocusable);
            break;

        default:

            // // it's important to call toBack first. else we see a flicker
            // // (window appears and disappears)
            // windowsWindowManager.toBack(w);

            windowsWindowManager.setFocusableWindowState(w, oldFocusableWindowState);
            windowsWindowManager.setFocusable(w, oldFocusable);

        }

    }

}
