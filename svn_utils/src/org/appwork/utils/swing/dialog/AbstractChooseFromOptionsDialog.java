/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
package org.appwork.utils.swing.dialog;

import javax.swing.Icon;

import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.uio.ComboBoxDialogInterface;
import org.appwork.uio.UIOManager;

/**
 * Shared base for {@link ChooseFromTilesDialog} and {@link ChooseFromListDialog}: pick one of several options by index (same contract as
 * {@link ComboBoxDialog} / {@link ComboBoxDialogInterface}).
 */
abstract class AbstractChooseFromOptionsDialog extends AbstractDialog<Integer> implements ComboBoxDialogInterface {
    protected final String   message;
    protected final Object[] options;
    protected final int      defaultAnswer;
    /**
     * When true, a single activation (tile click or list row click) confirms the dialog; OK is hidden ({@link UIOManager#BUTTONS_HIDE_OK}).
     */
    protected final boolean  closeOnOptionActivated;
    /**
     * If &gt;= 0, selection was set without a normal OK (e.g. console / automation); {@link #getSelectedIndex()} and {@link #throwCloseExceptions()} honor this.
     */
    protected int            headlessCommittedIndex = -1;
    /**
     * Current choice; kept in sync with the UI so {@link #createReturnValue()} is safe after dispose.
     */
    protected int            selectionIndex;

    protected AbstractChooseFromOptionsDialog(final int flag, final String title, final String question, final Object[] options, final int defaultSelection, final Icon icon, final String okText, final String cancelText) {
        this(flag, title, question, options, defaultSelection, icon, okText, cancelText, false);
    }

    /**
     * @param icon
     *            ignored; choose dialogs never show a frame icon ({@link Dialog#STYLE_HIDE_ICON}).
     */
    protected AbstractChooseFromOptionsDialog(final int flag, final String title, final String question, final Object[] options, final int defaultSelection, final Icon icon, final String okText, final String cancelText, final boolean closeOnOptionActivated) {
        super((closeOnOptionActivated ? flag | UIOManager.BUTTONS_HIDE_OK : flag) | Dialog.STYLE_HIDE_ICON, title, null, okText, cancelText);
        this.closeOnOptionActivated = closeOnOptionActivated;
        this.message = question;
        this.options = options == null ? new Object[0] : options;
        this.defaultAnswer = defaultSelection < 0 ? 0 : defaultSelection;
        if (this.options.length == 0) {
            throw new IllegalArgumentException("options must not be empty");
        }
        this.selectionIndex = Math.min(this.defaultAnswer, this.options.length - 1);
        this.selectionIndex = Math.max(0, this.selectionIndex);
    }

    protected String elementToString(final Object value) {
        if (value instanceof LabelInterface) {
            return ((LabelInterface) value).getLabel();
        }
        return String.valueOf(value);
    }

    /**
     * Optional icon per option; tiles and list use it when non-null.
     */
    protected Icon elementToIcon(final Object value) {
        return null;
    }

    /**
     * Optional tooltip per list/tile row (e.g. full URL when the label is shortened). Default: none.
     */
    protected String elementToTooltip(final Object value) {
        return null;
    }

    protected void applyHeadlessCommit(final int index) {
        if (index < 0 || index >= this.options.length) {
            throw new IllegalArgumentException("index out of range: " + index);
        }
        this.headlessCommittedIndex = index;
        this.selectionIndex = index;
    }

    @Override
    public boolean isRemoteAPIEnabled() {
        return true;
    }

    @Override
    public int getSelectedIndex() {
        if (this.headlessCommittedIndex >= 0) {
            return this.headlessCommittedIndex;
        }
        if ((getReturnmask() & Dialog.RETURN_OK) == 0) {
            return -1;
        }
        return this.selectionIndex;
    }

    @Override
    public void throwCloseExceptions() throws DialogClosedException, DialogCanceledException {
        if (this.headlessCommittedIndex >= 0) {
            return;
        }
        super.throwCloseExceptions();
    }

    @Override
    protected Integer createReturnValue() {
        if (this.headlessCommittedIndex >= 0) {
            return Integer.valueOf(this.selectionIndex);
        }
        if ((getReturnmask() & Dialog.RETURN_OK) == 0) {
            return Integer.valueOf(-1);
        }
        return Integer.valueOf(this.selectionIndex);
    }

    @Override
    public String getMessage() {
        return this.message;
    }

    @Override
    public String[] getLabels() {
        final String[] ret = new String[this.options.length];
        for (int i = 0; i < ret.length; i++) {
            ret[i] = this.elementToString(this.options[i]);
        }
        return ret;
    }

    @Override
    public int getPreSelectedIndex() {
        return this.defaultAnswer;
    }
}
