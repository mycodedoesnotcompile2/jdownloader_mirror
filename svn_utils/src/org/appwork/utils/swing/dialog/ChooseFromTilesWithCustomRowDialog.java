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
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.info) file that contains a reference to this license.
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

import java.awt.Color;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTextField;

import org.appwork.swing.MigPanel;

import net.miginfocom.swing.MigLayout;

/**
 * {@link ChooseFromTilesDialog} with an extra row under the tile grid: optional label, {@link JTextField}, and {@link JButton} (e.g. other URL + OK).
 * <p>
 * For embedding in a parent dialog, call {@link #createContentForEmbedding()} without showing this dialog instance (tile actions that auto-OK guard {@code okButton == null}).
 */
public class ChooseFromTilesWithCustomRowDialog extends ChooseFromTilesDialog {
    private final JTextField customRowField;
    private final JButton    customRowApplyButton;
    private final String     customRowLabelText;

    public ChooseFromTilesWithCustomRowDialog(final int flag, final String title, final String question, final Object[] options, final int defaultSelection, final Icon icon, final String okText, final String cancelText, final boolean closeOnOptionActivated, final JTextField customRowField, final JButton customRowApplyButton, final String customRowLabelText) {
        super(flag, title, question, options, defaultSelection, icon, okText, cancelText, closeOnOptionActivated);
        this.customRowField = customRowField != null ? customRowField : new JTextField();
        this.customRowApplyButton = customRowApplyButton;
        this.customRowLabelText = customRowLabelText;
    }

    @Override
    public JComponent layoutDialogContent() {
        final MigPanel contentpane = new MigPanel("ins 0,wrap 1", "[fill,grow]", "[][][]");
        contentpane.add(this.createQuestionTextComponent(), "growx");
        contentpane.add(this.createTileGridPanel(), "pushx,pushy,growx,growy,wmin 0,hmin 0");
        final JLabel lbl = new JLabel(this.customRowLabelText != null ? this.customRowLabelText : "");
        lbl.setForeground(Color.DARK_GRAY);
        final MigPanel customRow = new MigPanel(new MigLayout("ins 0", "[align leading]10[grow,fill][]", ""));
        customRow.add(lbl);
        customRow.add(this.customRowField, "growx, split 2");
        if (this.customRowApplyButton != null) {
            customRow.add(this.customRowApplyButton, "");
        }
        contentpane.add(customRow, "growx");
        return contentpane;
    }

    /**
     * Same body as {@link #layoutDialogContent()} for use inside another dialog (e.g. login). Do not show this dialog if you only embed the returned component.
     */
    public JComponent createContentForEmbedding() {
        return this.layoutDialogContent();
    }
}
