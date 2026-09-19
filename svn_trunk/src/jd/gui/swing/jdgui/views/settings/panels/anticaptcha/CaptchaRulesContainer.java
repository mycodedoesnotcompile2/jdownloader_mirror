package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JScrollPane;

import org.appwork.swing.MigPanel;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter.CaptchaFilterType;
import org.jdownloader.captcha.v2.CaptchaChallengeFilterController;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.translate._GUI;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.gui.swing.jdgui.views.settings.components.SettingsComponent;

/**
 * Settings component that shows the {@link CaptchaChallengeFilter} rules in an editable table. The layout follows the Linkgrabber filter
 * panel: the table fills the area and a button bar at the bottom carries Add/Remove on the left and Import/Export on the right.
 */
public class CaptchaRulesContainer extends MigPanel implements SettingsComponent {
    private final CaptchaRulesTableModel               model;
    private final BasicJDTable<CaptchaChallengeFilter> table;

    public CaptchaRulesContainer() {
        super("ins 0, wrap 1", "[grow,fill]", "[grow,fill][]");
        this.model = new CaptchaRulesTableModel();
        this.table = new BasicJDTable<CaptchaChallengeFilter>(model);
        add(new JScrollPane(table), "grow");
        final MigPanel buttonBar = new MigPanel("ins 0", "[][][grow,fill][][]", "[]");
        final JButton addButton = new JButton(_GUI.T.CaptchaRules_add_button());
        addButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                onAddRule();
            }
        });
        final JButton removeButton = new JButton(_GUI.T.CaptchaRules_remove_button());
        removeButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                onRemoveSelected();
            }
        });
        final JButton importButton = new JButton(_GUI.T.CaptchaRules_import_button());
        importButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                onImport();
            }
        });
        final JButton exportButton = new JButton(_GUI.T.CaptchaRules_export_button());
        exportButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                onExport();
            }
        });
        buttonBar.add(addButton, "height 26!");
        buttonBar.add(removeButton, "height 26!");
        buttonBar.add(Box.createHorizontalGlue());
        buttonBar.add(importButton, "height 26!");
        buttonBar.add(exportButton, "height 26!");
        add(buttonBar, "growx");
    }

    /** Returns the solvers a rule can be assigned to. */
    private List<SolverService> getSelectableSolvers() {
        return new ArrayList<SolverService>(ChallengeResponseController.getInstance().listServices());
    }

    private void onAddRule() {
        final List<SolverService> services = getSelectableSolvers();
        if (services.isEmpty()) {
            Dialog.getInstance().showErrorDialog(_GUI.T.CaptchaRules_add_no_solver());
            return;
        }
        final SolverService[] options = services.toArray(new SolverService[services.size()]);
        final int selectedIndex;
        try {
            selectedIndex = Dialog.getInstance().showComboDialog(0, _GUI.T.CaptchaRules_add_title(), _GUI.T.CaptchaRules_add_message(), options, 0, null, null, null, new DefaultListCellRenderer() {
                private static final long serialVersionUID = 1L;

                @Override
                public Component getListCellRendererComponent(final JList list, final Object value, final int index, final boolean isSelected, final boolean cellHasFocus) {
                    final JLabel label = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                    if (value instanceof SolverService) {
                        final SolverService service = (SolverService) value;
                        label.setText(service.getName());
                        label.setIcon(service.getIcon(18));
                    }
                    return label;
                }
            });
        } catch (final DialogNoAnswerException e) {
            /* User cancelled or closed the dialog */
            return;
        }
        if (selectedIndex < 0) {
            return;
        }
        final SolverService chosen = options[selectedIndex];
        final CaptchaChallengeFilter rule = new CaptchaChallengeFilter();
        rule.setSolver(chosen.getID());
        rule.setName(_GUI.T.CaptchaRules_new_rule_name(chosen.getName()));
        rule.setFilterType(CaptchaFilterType.BLACKLIST);
        rule.setDomain("example.com");
        rule.setEnabled(true);
        rule.setRegex(false);
        rule.setPosition(CaptchaChallengeFilterController.getInstance().list().size());
        CaptchaChallengeFilterController.getInstance().add(rule);
        model.refresh();
    }

    private void onRemoveSelected() {
        final List<CaptchaChallengeFilter> selected = model.getSelectedObjects();
        if (selected == null || selected.isEmpty()) {
            return;
        }
        for (int i = 0; i < selected.size(); i++) {
            final CaptchaChallengeFilter rule = selected.get(i);
            if (rule.isStaticRule()) {
                /* Static default rules (e.g. the example rule) cannot be removed. */
                continue;
            }
            CaptchaChallengeFilterController.getInstance().remove(rule);
        }
        model.refresh();
    }

    private void onImport() {
        final JFileChooser chooser = new JFileChooser();
        if (chooser.showOpenDialog(this) != JFileChooser.APPROVE_OPTION) {
            return;
        }
        final File file = chooser.getSelectedFile();
        if (file == null) {
            return;
        }
        CaptchaChallengeFilterController.getInstance().importList(file);
        model.refresh();
    }

    private void onExport() {
        final JFileChooser chooser = new JFileChooser();
        if (chooser.showSaveDialog(this) != JFileChooser.APPROVE_OPTION) {
            return;
        }
        File file = chooser.getSelectedFile();
        if (file == null) {
            return;
        }
        if (!file.getName().toLowerCase().endsWith(".json")) {
            file = new File(file.getAbsolutePath() + ".json");
        }
        CaptchaChallengeFilterController.getInstance().exportList(file, CaptchaChallengeFilterController.getInstance().list());
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
