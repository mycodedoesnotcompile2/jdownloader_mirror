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
package org.appwork.utils.swing.dialog.test;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.appwork.utils.swing.dialog.InputDialog;

/**
 * @author thomas
 */
public class DialogOrder {

    /**
     * Close Order: 0 1 2 3 4 5 6 7 8 9 A B
     */
    public static void main(final String[] args) {
        for (int i = 0; i < 10; i++) {
            DialogOrder.startDialogInThread(i);
        }
        try {
            Thread.sleep(11000);
        } catch (final InterruptedException e) {
            e.printStackTrace();
        }
        DialogOrder.test2();
    }

    /**
     * Close Order: 0 1 2 3 4 5 6 7 8 9
     */
    private static void startDialogInThread(final int i) {
        new Thread(i + "") {
            @Override
            public void run() {
                try {
                    Thread.sleep(1000 * i);
                } catch (final InterruptedException e) {
                    e.printStackTrace();
                }
                try {
                    Dialog.getInstance().showInputDialog("Dialog " + i);
                } catch (final DialogClosedException e) {                    
                    e.printStackTrace();
                } catch (final DialogCanceledException e) {                    
                    e.printStackTrace();
                }
                System.out.println("Closed " + i);
            }
        }.start();
    }

    /**
     * Close Order: A B
     */
    private static void test2() {
        final InputDialog dialog = new InputDialog(0, "title", "message", "defaultMessage", null, null, null);

        dialog.setLeftActions(new AbstractAction("CLICK HERE!!!") {

            private static final long serialVersionUID = 3916626551625222343L;

            public void actionPerformed(final ActionEvent e) {

                Dialog.getInstance().showMessageDialog("INTERNAL");

                System.out.println("Closed A");
            }

        });
        try {
            Dialog.getInstance().showDialog(dialog);
        } catch (final DialogClosedException e1) {            
            e1.printStackTrace();
        } catch (final DialogCanceledException e1) {            
            e1.printStackTrace();
        }
        System.out.println("Closed B");
    }

}
