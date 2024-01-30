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
package org.appwork.utils.swing.dialog.locator;

import java.awt.Point;
import java.awt.Window;
import java.awt.event.ComponentEvent;

import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.InternDialog;
import org.appwork.utils.swing.locator.AbstractLocator;
import org.appwork.utils.swing.locator.RememberRelativeLocator;

public class RememberRelativeDialogLocator implements DialogLocator {

    protected final RememberRelativeLocator delegate;
    private String                          id;

    /**
     * @param jFrame
     * @param string
     */
    public RememberRelativeDialogLocator(final String id, final Window jFrame) {
        this.id = id;
        if (id == null) {
            throw new IllegalArgumentException("id ==null");
        }
        delegate = new RememberRelativeLocator(id, jFrame) {
            /*
             * (non-Javadoc)
             *
             * @see org.appwork.utils.swing.locator.RememberRelativeLocator#getID (java.awt.Window)
             */
            @Override
            protected String getID(final Window frame) {                
                return RememberRelativeDialogLocator.this.getID(frame);
            }
        };

    }

    /**
     * @param frame
     * @return
     */
    protected String getID(final Window frame) {
        return id;
    }

    @Override
    public Point getLocationOnScreen(final AbstractDialog<?> dialog) {
        return delegate.getLocationOnScreen(dialog.getDialog());

    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.Locator#onClose(org.appwork.utils.swing .dialog.AbstractDialog)
     */
    @Override
    public void onClose(final AbstractDialog<?> dialog, final ComponentEvent event) {
        delegate.onClose(dialog.getDialog(), event);
    }

    /**
     * @param rememberAbsoluteDialogLocator
     */
    public void setFallbackLocator(final DialogLocator fallback) {
        delegate.setFallbackLocator(new AbstractLocator() {

            @Override
            public Point getLocationOnScreen(final Window frame) {
                return fallback.getLocationOnScreen(((InternDialog) frame).getDialogModel());
            }

            @Override
            public void onClose(final Window frame, final ComponentEvent event) {
                try {
                    fallback.onClose(((InternDialog) frame).getDialogModel(), event);
                } catch (final Exception e) {
                    e.printStackTrace();
                }
            }
        });

    }

}
