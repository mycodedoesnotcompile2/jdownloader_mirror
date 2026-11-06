package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Action;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.dimensor.RememberLastDialogDimension;
import org.appwork.utils.swing.dialog.locator.RememberAbsoluteDialogLocator;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.actions.AppAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.staticreferences.CFG_CAPTCHA;
import org.jdownloader.updatev2.gui.LAFOptions;

import jd.gui.swing.jdgui.views.settings.panels.anticaptcha.TimingTableModel.TimingChangeListener;
import jd.gui.swing.jdgui.views.settings.panels.anticaptcha.TimingTableModel.TimingRow;
import jd.plugins.CaptchaType;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import net.miginfocom.swing.MigLayout;

/**
 * Shows how a captcha runs through the solvers (see {@link TimingTableModel}) and lets the user change the seconds after which a solver
 * starts the next one. Above the table a captcha type can be chosen: Then only the solvers which support this captcha type are shown and
 * the changes apply to this captcha type only. Without a captcha type, the changes apply to the solver as a whole. <br>
 * The changes are only stored in the config of the solvers when the user clicks Save, the other button closes the dialog and discards them.
 */
public class SolverTimingDialog extends AbstractDialog<Object> {
    /** The preselected timings: a shortcut to set all shown solvers at once. Changing something in the table switches to CUSTOM. */
    private enum Preset {
        IMMEDIATELY,
        DEFAULT,
        CUSTOM;
        public String getLabel() {
            switch (this) {
            case IMMEDIATELY:
                return _GUI.T.SolverTimingDialog_preselected_immediately();
            case DEFAULT:
                return _GUI.T.SolverTimingDialog_preselected_default();
            case CUSTOM:
            default:
                return _GUI.T.SolverTimingDialog_preselected_custom();
            }
        }
    }

    /* Number of table rows visible without scrolling. */
    private static final int      MAX_VISIBLE_ROWS = 12;
    /* True while the dialog changes the preselection or the table itself: This is no change by the user. */
    private boolean               adjusting;
    /* True as soon as the user changed something: ESC does not close the dialog then, so the changes are not lost by accident. */
    private boolean               changed;
    private JComboBox             presetBox;
    private JLabel                immediatelyInfo;
    /* Entry of the captcha type box: No captcha type, all solvers are shown. */
    private static final Object   ALL_TYPES = new Object();
    private final SolverService   selectedService;
    /* The captcha type which is preselected when the dialog opens, null = all captcha types. */
    private final CAPTCHA_TYPE    initialCaptchaType;
    private final TimingTableModel model;
    private final TimingTable     table;
    private JComboBox             typeBox;
    private JTextArea             appliesToLabel;
    private JTextArea             warningLabel;
    private JPanel                warningPanel;
    private JLabel                presetLabel;
    /* Removes the values of the selected captcha type, only shown if a captcha type is selected. */
    private JButton               resetCaptchaTypeButton;
    private AppAction             resetCaptchaTypeAction;
    /* Sets the value of the solver of all shown solvers back to the default. */
    private AppAction             resetSolverAction;
    /* Removes all values (solver and captcha types), only shown if any shown solver has a value which is not the default. */
    private JButton               resetAllButton;
    private AppAction             resetAllAction;

    /**
     * @param selectedService
     *            the solver whose row is selected when the dialog opens, may be null
     */
    public SolverTimingDialog(final SolverService selectedService) {
        this(selectedService, null);
    }

    /**
     * @param selectedService
     *            the solver whose row is selected when the dialog opens, may be null
     * @param initialCaptchaType
     *            the captcha type which is preselected in the dropdown, null = all captcha types
     */
    public SolverTimingDialog(final SolverService selectedService, final CAPTCHA_TYPE initialCaptchaType) {
        super(Dialog.STYLE_HIDE_ICON, _GUI.T.SolverTimingDialog_title(), null, _GUI.T.SolverTimingDialog_save(), _GUI.T.SolverTimingDialog_discard());
        this.initialCaptchaType = initialCaptchaType;
        this.selectedService = selectedService;
        setLocator(new RememberAbsoluteDialogLocator(getClass().getSimpleName()));
        setDimensor(new RememberLastDialogDimension(getClass().getSimpleName()));
        model = new TimingTableModel();
        table = new TimingTable(model);
    }

    @Override
    public JComponent layoutDialogContent() {
        final JPanel panel = new JPanel(new MigLayout("ins 0,wrap 1", "[grow,fill]", "[][][][][grow,fill][][]"));
        /* Wrapping text areas: The text follows the width of the dialog when it is resized. */
        panel.add(createWrappingText(_GUI.T.SolverTimingDialog_intro()), "wmin 0");
        /* Nothing was changed yet: Saving is only possible after the user changed something. */
        getOkButton().setEnabled(false);
        final JPanel typePanel = new JPanel(new MigLayout("ins 0,wrap 2", "[][grow,fill]", "[][][]"));
        typePanel.setOpaque(false);
        typePanel.add(new JLabel(_GUI.T.SolverTimingDialog_captchaType()));
        final List<Object> items = new ArrayList<Object>();
        items.add(ALL_TYPES);
        for (final CAPTCHA_TYPE type : CaptchaType.getProcessableCaptchaTypes()) {
            items.add(type);
        }
        typeBox = new JComboBox(items.toArray());
        typeBox.setMaximumRowCount(15);
        typeBox.setRenderer(new DefaultListCellRenderer() {
            private static final long serialVersionUID = 1L;

            @Override
            public Component getListCellRendererComponent(final JList list, final Object value, final int index, final boolean isSelected, final boolean cellHasFocus) {
                if (value == ALL_TYPES) {
                    final JLabel all = (JLabel) super.getListCellRendererComponent(list, _GUI.T.SolverTimingDialog_allCaptchaTypes(), index, isSelected, cellHasFocus);
                    /* The "A" icon, the same as the one of the JAntiCaptcha solver (SolverType.JD_LOCAL). */
                    all.setIcon(NewTheme.I().getIcon(IconKey.ICON_OCR, 16));
                    return all;
                }
                final JLabel label = (JLabel) super.getListCellRendererComponent(list, ((CAPTCHA_TYPE) value).getDisplayName(), index, isSelected, cellHasFocus);
                label.setIcon(((CAPTCHA_TYPE) value).getIcon());
                return label;
            }
        });
        typeBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                final Object selected = typeBox.getSelectedItem();
                model.setCaptchaType(selected instanceof CAPTCHA_TYPE ? (CAPTCHA_TYPE) selected : null);
                updateHints();
                selectDetectedPreset();
            }
        });
        typePanel.add(typeBox);
        presetLabel = new JLabel(_GUI.T.SolverTimingDialog_preselected());
        typePanel.add(presetLabel);
        immediatelyInfo = new JLabel(_GUI.T.SolverTimingDialog_preselected_immediatelyInfo());
        immediatelyInfo.setIcon(NewTheme.I().getIcon(IconKey.ICON_WARNING, 16));
        immediatelyInfo.setVisible(false);
        presetBox = new JComboBox(Preset.values());
        presetBox.setRenderer(new DefaultListCellRenderer() {
            private static final long serialVersionUID = 1L;

            @Override
            public Component getListCellRendererComponent(final JList list, final Object value, final int index, final boolean isSelected, final boolean cellHasFocus) {
                return super.getListCellRendererComponent(list, ((Preset) value).getLabel(), index, isSelected, cellHasFocus);
            }
        });
        presetBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                /* The info is shown whenever "immediately" is selected, also if the selection was detected and not chosen by the user. */
                immediatelyInfo.setVisible(presetBox.getSelectedItem() == Preset.IMMEDIATELY);
                if (adjusting) {
                    return;
                }
                adjusting = true;
                try {
                    switch ((Preset) presetBox.getSelectedItem()) {
                    case IMMEDIATELY:
                        model.setAllImmediately();
                        break;
                    case DEFAULT:
                        model.resetAll();
                        break;
                    case CUSTOM:
                    default:
                        break;
                    }
                } finally {
                    adjusting = false;
                }
            }
        });
        typePanel.add(presetBox);
        typePanel.add(immediatelyInfo, "skip 1");
        /* Every change of the user unlocks the save button and switches the preselection to custom. */
        model.setChangeListener(new TimingChangeListener() {
            @Override
            public void onTimingChanged() {
                if (!adjusting) {
                    adjusting = true;
                    try {
                        presetBox.setSelectedItem(Preset.CUSTOM);
                    } finally {
                        adjusting = false;
                    }
                }
                changed = true;
                final JButton save = getOkButton();
                save.setEnabled(true);
                /* The save button is the default button as soon as it can be used. */
                getDialog().getRootPane().setDefaultButton(save);
                updateResetEnabled();
            }
        });
        panel.add(typePanel);
        /* Tells for what the changes in the table apply: The user can not see that in the table. */
        appliesToLabel = createWrappingText(null);
        panel.add(appliesToLabel, "wmin 0");
        warningLabel = createWrappingText(null);
        warningLabel.setForeground(LAFOptions.getInstance().getColorForErrorForeground());
        /* The warning has a warning icon in front of it, it is only shown if there is a warning. */
        warningPanel = new JPanel(new MigLayout("ins 0", "[][grow,fill]", "[]"));
        warningPanel.setOpaque(false);
        warningPanel.add(new JLabel(NewTheme.I().getIcon(IconKey.ICON_WARNING_RED, 18)), "aligny top");
        warningPanel.add(warningLabel, "wmin 0");
        warningPanel.setVisible(false);
        panel.add(warningPanel, "wmin 0,hidemode 3");
        /*
         * At most MAX_VISIBLE_ROWS rows are visible at once, further rows are reached with the scrollbar. The preferred height is fixed, so the
         * buttons below stay visible by default.
         */
        final JScrollPane scrollPane = table.createScrollPane();
        final int tableHeight = table.getTableHeader().getPreferredSize().height + MAX_VISIBLE_ROWS * table.getRowHeight() + 2;
        scrollPane.setPreferredSize(new Dimension(860, tableHeight));
        panel.add(scrollPane, "hmin 100");
        /* The overall time a captcha may take: after it the challenge times out, no matter how many solvers are still waiting to be started. */
        final String timeout = TimeFormatter.formatMilliSeconds(CFG_CAPTCHA.CFG.getDefaultChallengeTimeout(), 0);
        panel.add(createWrappingText(_GUI.T.SolverTimingDialog_challengeTimeout(timeout)), "wmin 0");
        resetCaptchaTypeAction = table.getResetCaptchaTypeAction();
        resetCaptchaTypeButton = new JButton(resetCaptchaTypeAction);
        resetSolverAction = table.getResetSolverAction();
        resetAllAction = table.getResetAllAction();
        resetAllButton = new JButton(resetAllAction);
        /* The reset buttons depend on the values which are not the default, see updateResetEnabled. */
        model.addTableModelListener(new TableModelListener() {
            @Override
            public void tableChanged(final TableModelEvent e) {
                updateResetEnabled();
            }
        });
        final JPanel resetPanel = new JPanel(new MigLayout("ins 0", "[][][]", "[]"));
        resetPanel.setOpaque(false);
        resetPanel.add(resetCaptchaTypeButton);
        resetPanel.add(new JButton(resetSolverAction));
        resetPanel.add(resetAllButton);
        panel.add(resetPanel, "gaptop 5,growx 0,alignx left");
        if (initialCaptchaType != null) {
            /* Fires the listener of the box, which applies the captcha type to the model and updates the hints and the preselection. */
            typeBox.setSelectedItem(initialCaptchaType);
        }
        selectSolverRow();
        updateHints();
        selectDetectedPreset();
        return panel;
    }

    /** A read-only text which looks like a label but wraps at the current width of the dialog. */
    private static JTextArea createWrappingText(final String text) {
        final JTextArea area = new JTextArea(text);
        area.setEditable(false);
        area.setFocusable(false);
        area.setLineWrap(true);
        area.setWrapStyleWord(true);
        /* Same handling as the other read-only texts of the settings (e.g. the error text of the MyJDownloader panel). */
        SwingUtils.setOpaque(area, false);
        area.setBorder(BorderFactory.createEmptyBorder());
        area.setMargin(new Insets(0, 0, 0, 0));
        /* Look like a label: font, colors and the cursor of a label, not the ones of a text field. */
        final JLabel template = new JLabel();
        area.setFont(template.getFont());
        area.setForeground(template.getForeground());
        area.setDisabledTextColor(template.getForeground());
        area.setCursor(Cursor.getDefaultCursor());
        area.setHighlighter(null);
        return area;
    }

    /**
     * Selects the preselection which matches the shown values (e.g. after the captcha type changed): All default, all 0 seconds, or custom.
     * This is no change by the user.
     */
    private void selectDetectedPreset() {
        final Preset preset;
        if (model.isAllDefault()) {
            preset = Preset.DEFAULT;
        } else if (model.isAllImmediately()) {
            preset = Preset.IMMEDIATELY;
        } else {
            preset = Preset.CUSTOM;
        }
        adjusting = true;
        try {
            presetBox.setSelectedItem(preset);
        } finally {
            adjusting = false;
        }
    }

    /** ESC closes the dialog only as long as the user did not change anything. After a change the user has to use one of the buttons. */
    @Override
    protected void registerEscape() {
        super.registerEscape();
        final JComponent focus = getFocusButton();
        if (focus == null) {
            return;
        }
        final Action original = focus.getActionMap().get("ESCAPE");
        if (original == null) {
            return;
        }
        final Action guarded = new AbstractAction() {
            private static final long serialVersionUID = 1L;

            @Override
            public void actionPerformed(final ActionEvent e) {
                if (!changed) {
                    original.actionPerformed(e);
                }
            }
        };
        focus.getActionMap().put("ESCAPE", guarded);
        /*
         * The focus button is the save button, which is disabled until the user changed something. Swing does not run the key bindings of a
         * disabled component, so ESC would do nothing. The binding is therefore also registered on the root pane of the dialog.
         */
        final JRootPane rootPane = getDialog().getRootPane();
        rootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "SOLVER_TIMING_ESCAPE");
        rootPane.getActionMap().put("SOLVER_TIMING_ESCAPE", guarded);
    }

    /** The timings are stored in the config of the solvers only now, when the user saves. */
    @Override
    public void actionPerformed(final ActionEvent e) {
        if (e.getSource() == getOkButton()) {
            model.save();
        }
        super.actionPerformed(e);
    }

    /**
     * Updates the reset buttons, which are always shown but greyed out if there is nothing to reset: The one for the captcha type needs a
     * selected captcha type with a value. The one for the solvers needs a solver with a value which is not its default. The one for
     * everything needs any shown solver with such a value, the value of the solver or the value of any of its captcha types.
     */
    private void updateResetEnabled() {
        if (resetAllAction == null) {
            return;
        }
        resetCaptchaTypeAction.setEnabled(model.getCaptchaType() != null && !model.isAllDefault());
        resetSolverAction.setEnabled(model.isAnySolverValueSet());
        resetAllAction.setEnabled(model.isAnyValueSet());
    }

    /** Updates the hints above the table and the tooltip of the reset button for the chosen captcha type. */
    private void updateHints() {
        updateResetEnabled();
        final CAPTCHA_TYPE type = model.getCaptchaType();
        if (type == null) {
            appliesToLabel.setText(_GUI.T.SolverTimingDialog_appliesToSolver());
        } else {
            appliesToLabel.setText(_GUI.T.SolverTimingDialog_appliesToCaptchaType(type.getDisplayName()));
        }
        if (model.getSolverCount() > 0) {
            warningLabel.setText(null);
        } else if (type == null) {
            warningLabel.setText(_GUI.T.SolverTimingDialog_noSolver());
        } else {
            warningLabel.setText(_GUI.T.SolverTimingDialog_noSolverForCaptchaType(type.getDisplayName()));
        }
        warningPanel.setVisible(model.getSolverCount() == 0);
        /* Without a solver there is nothing to preselect: The preselection is greyed out. */
        presetLabel.setEnabled(model.getSolverCount() > 0);
        presetBox.setEnabled(model.getSolverCount() > 0);
        /* The warning for a selected captcha type without solvers is bold. */
        final Font font = warningLabel.getFont();
        final int style = type != null && model.getSolverCount() == 0 ? Font.BOLD : Font.PLAIN;
        if (font.getStyle() != style) {
            warningLabel.setFont(font.deriveFont(style));
        }
    }

    /** Selects the row of the solver that was chosen when the dialog was opened. */
    private void selectSolverRow() {
        if (selectedService == null) {
            return;
        }
        final List<TimingRow> rows = model.getTableData();
        for (int i = 0; i < rows.size(); i++) {
            final TimingRow row = rows.get(i);
            if (row.isSolverRow() && row.getService() == selectedService) {
                table.getSelectionModel().setSelectionInterval(i, i);
                return;
            }
        }
    }

    @Override
    protected Object createReturnValue() {
        return null;
    }
}
