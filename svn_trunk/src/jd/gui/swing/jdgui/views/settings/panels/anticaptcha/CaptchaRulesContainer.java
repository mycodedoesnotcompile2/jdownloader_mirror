package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtButton;
import org.appwork.swing.exttable.utils.MinimumSelectionObserver;
import org.appwork.utils.swing.SwingUtils;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter;
import org.jdownloader.gui.translate._GUI;

import jd.gui.swing.jdgui.views.settings.components.SettingsComponent;

/**
 * Settings component that shows the {@link CaptchaChallengeFilter} rules in an editable table. Modelled after the packagizer filter
 * table ({@link jd.gui.swing.jdgui.views.settings.panels.packagizer.PackagizerFilter}): the table fills the area, a button bar at the
 * bottom carries Add/Remove on the left and Import/Export on the right, and the buttons are grayed out consistently with that table
 * (Remove needs a non-static selection, Export needs at least one row).
 */
public class CaptchaRulesContainer extends MigPanel implements SettingsComponent {
    private final CaptchaRulesTableModel model;
    private final CaptchaRulesTable      table;
    private final JButton                addButton;
    private final JButton                removeButton;
    private final ExtButton              importButton;
    private final ExtButton              exportButton;

    public CaptchaRulesContainer() {
        super("ins 0, wrap 1", "[grow,fill]", "[][grow,fill][]");
        this.model = new CaptchaRulesTableModel();
        this.table = new CaptchaRulesTable(model);
        final JTextArea hint = new JTextArea();
        SwingUtils.setOpaque(hint, false);
        hint.setEditable(false);
        hint.setLineWrap(true);
        hint.setWrapStyleWord(true);
        hint.setFocusable(false);
        hint.setText(_GUI.T.CaptchaRules_hint());
        add(hint, "gaptop 0,growx,pushx,gapbottom 5,wmin 10");
        add(new JScrollPane(table), "grow");
        final MigPanel buttonBar = new MigPanel("ins 0", "[][][grow,fill][][]", "[]");
        addButton = new JButton(new CaptchaRulesAddAction(table));
        final CaptchaRulesRemoveAction removeAction = new CaptchaRulesRemoveAction(table);
        removeButton = new JButton(removeAction);
        importButton = new ExtButton(new CaptchaRulesImportAction(table));
        exportButton = new ExtButton(new CaptchaRulesExportAction(table, null));
        buttonBar.add(addButton, "height 26!,sg 1");
        buttonBar.add(removeButton, "height 26!,sg 1");
        buttonBar.add(Box.createHorizontalGlue());
        buttonBar.add(importButton, "height 26!,sg 2");
        buttonBar.add(exportButton, "height 26!,sg 2");
        add(buttonBar, "growx");
        /* Export needs at least one persisted rule; row 0 is always the non-persisted static example rule. */
        table.getModel().addTableModelListener(new TableModelListener() {
            @Override
            public void tableChanged(final TableModelEvent e) {
                exportButton.setEnabled(table.getRowCount() > 1);
            }
        });
        /* Remove is only enabled while the selection contains at least one non-static rule. */
        table.getSelectionModel().addListSelectionListener(new MinimumSelectionObserver(table, removeAction, 1) {
            @Override
            public void valueChanged(final ListSelectionEvent e) {
                boolean removable = true;
                int count = 0;
                for (final CaptchaChallengeFilter rule : CaptchaRulesContainer.this.table.getModel().getSelectedObjects()) {
                    removable &= !rule.isStaticRule();
                    count++;
                }
                if (!removable) {
                    removeButton.setToolTipText(_GUI.T.CaptchaRules_remove_disabled_static());
                    action.setEnabled(false);
                    return;
                }
                removeButton.setToolTipText(null);
                action.setEnabled(count >= minSelections);
            }
        });
    }

    public CaptchaRulesTable getTable() {
        return table;
    }

    @Override
    public String getConstraints() {
        return "height 60:n:n,pushy,growy";
    }

    @Override
    public boolean isMultiline() {
        return true;
    }
}
