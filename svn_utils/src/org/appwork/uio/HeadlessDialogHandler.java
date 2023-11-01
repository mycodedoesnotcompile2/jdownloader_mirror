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
package org.appwork.uio;

import javax.swing.Icon;

import org.appwork.console.AbstractConsole;
import org.appwork.console.ConsoleDialog;
import org.appwork.utils.BinaryLogic;
import org.appwork.utils.Exceptions;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;

/**
 * @author Thomas
 *
 */
public class HeadlessDialogHandler implements UserIOHandlerInterface {

    @SuppressWarnings("unchecked")
    @Override
    public <T extends UserIODefinition> T show(Class<T> class1, T impl) {
        if (impl instanceof ConfirmDialog) {
            final CloseReason cr;
            if (BinaryLogic.containsAll(impl.getFlags(), UIOManager.BUTTONS_HIDE_CANCEL)) {
                showMessageDialog(((ConfirmDialog) impl).getMessage());
                cr = CloseReason.OK;
            } else {
                if (showConfirmDialog(impl.getFlags(), impl.getTitle(), ((ConfirmDialog) impl).getMessage(), null, ((ConfirmDialog) impl).getOKButtonText(), ((ConfirmDialog) impl).getCancelButtonText())) {
                    cr = CloseReason.OK;
                } else {
                    cr = CloseReason.CANCEL;
                }
            }
            return (T) new ConfirmDialogInterface() {

                @Override
                public void throwCloseExceptions() throws DialogClosedException, DialogCanceledException {

                    switch (getCloseReason()) {
                    case CANCEL:
                        throw new DialogCanceledException(Dialog.RETURN_CANCEL);
                    case CLOSE:
                        throw new DialogCanceledException(Dialog.RETURN_CLOSED);
                    case TIMEOUT:
                        throw new DialogCanceledException(Dialog.RETURN_CLOSED | Dialog.RETURN_TIMEOUT);
                    case INTERRUPT:
                        throw new DialogCanceledException(Dialog.RETURN_CLOSED | Dialog.RETURN_INTERRUPT);
                    default:
                        return;
                    }
                }

                @Override
                public void setCloseReason(CloseReason closeReason) {
                }

                @Override
                public boolean isRemoteAPIEnabled() {
                    return false;
                }

                @Override
                public boolean isDontShowAgainSelected() {
                    return false;
                }

                @Override
                public String getTitle() {
                    return null;
                }

                @Override
                public int getTimeout() {
                    return 0;
                }

                @Override
                public int getFlags() {
                    return 0;
                }

                @Override
                public CloseReason getCloseReason() {
                    return cr;
                }

                @Override
                public String getOKButtonText() {
                    return null;
                }

                @Override
                public String getMessage() {
                    return null;
                }

                @Override
                public String getCancelButtonText() {
                    return null;
                }
            };
        }
        return impl;
    }

    @Override
    public boolean showConfirmDialog(int flag, String title, String message) {
        synchronized (AbstractConsole.LOCK) {
            final ConsoleDialog cd = new ConsoleDialog(title);
            cd.start();
            cd.printLines(message);
            try {
                cd.waitYesOrNo(flag, "ok", "cancel");
                return true;
            } catch (DialogNoAnswerException e) {
            } finally {
                cd.end();
            }
            return false;
        }
    }

    @Override
    public boolean showConfirmDialog(int flags, String title, String message, Icon icon, String ok, String cancel, String dontShowAgainKey) {
        synchronized (AbstractConsole.LOCK) {
            final ConsoleDialog cd = new ConsoleDialog(title);
            cd.start();
            cd.printLines(message);
            try {
                cd.waitYesOrNo(flags, ok, cancel);
                return true;
            } catch (DialogNoAnswerException e) {
            } finally {
                cd.end();
            }
        }
        return false;
    }

    @Override
    public boolean showConfirmDialog(int flags, String title, String message, Icon icon, String ok, String cancel) {
        return showConfirmDialog(flags, title, message, icon, ok, cancel, null);
    }

    @Override
    public void showErrorMessage(String message) {
        synchronized (AbstractConsole.LOCK) {
            final ConsoleDialog cd = new ConsoleDialog("Error!");
            cd.printLines(message);
            cd.start();
            try {
                cd.waitYesOrNo(UIOManager.BUTTONS_HIDE_OK, null, null);
            } catch (DialogNoAnswerException e) {
            } finally {
                cd.end();
            }
        }
    }

    @Override
    public void showMessageDialog(String message) {
        synchronized (AbstractConsole.LOCK) {
            final ConsoleDialog cd = new ConsoleDialog("Message");
            cd.printLines(message);
            try {
                cd.waitYesOrNo(UIOManager.BUTTONS_HIDE_OK, null, null);
            } catch (DialogNoAnswerException e) {
            } finally {
                cd.end();
            }
        }
    }

    @Override
    public void showException(String message, Throwable e1) {
        synchronized (AbstractConsole.LOCK) {
            final ConsoleDialog cd = new ConsoleDialog(message);
            cd.printLines(message);
            cd.printLines(Exceptions.getStackTrace(e1));
            cd.start();
            try {
                cd.waitYesOrNo(UIOManager.BUTTONS_HIDE_OK, null, null);
            } catch (DialogNoAnswerException e) {
            } finally {
                cd.end();
            }
        }
    }

}
