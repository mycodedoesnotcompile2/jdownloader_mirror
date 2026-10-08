package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Icon;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;

import org.appwork.swing.exttable.ExtTableHeaderRenderer;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtComponentColumn;
import org.appwork.swing.exttable.columns.ExtSpinnerColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.renderer.RenderLabel;
import org.appwork.utils.swing.renderer.RendererMigPanel;
import org.jdownloader.captcha.v2.CaptchaSolverLimitRule;
import org.jdownloader.captcha.v2.CaptchaSolverLimitRule.IntervalUnit;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfig;

/**
 * Table model for editing the {@link CaptchaSolverLimitRule}s of one external captcha solver (stored in
 * {@link CaptchaSolverPluginConfig#getLimitRules()}). Columns: enabled, name, max captchas, interval, unit. Like the captcha rules table,
 * every column reports {@code isEnabled(rule) == rule.isEnabled()}, so disabled rules are rendered grayed out. Every edit is persisted
 * immediately.
 */
public class CaptchaSolverLimitsTableModel extends ExtTableModel<CaptchaSolverLimitRule> {
    private final CaptchaSolverPluginConfig         cfg;
    /** The working list: edited in place and written back to the config on every change (see {@link #persist()}). */
    private final ArrayList<CaptchaSolverLimitRule> rules;
    /** The "Name" column, kept to start inline editing on it right after a rule was added, see {@link #startEditingName}. */
    private ExtTextColumn<CaptchaSolverLimitRule>   nameColumn;

    public CaptchaSolverLimitsTableModel(final CaptchaSolverPluginConfig cfg) {
        super("CaptchaSolverLimitsTableModel");
        this.cfg = cfg;
        final ArrayList<CaptchaSolverLimitRule> stored = cfg.getLimitRules();
        this.rules = stored != null ? new ArrayList<CaptchaSolverLimitRule>(stored) : new ArrayList<CaptchaSolverLimitRule>();
        refresh();
    }

    private void refresh() {
        _fireTableStructureChanged(new ArrayList<CaptchaSolverLimitRule>(rules), true);
    }

    /** Writes the working list back to the config. Needed after every edit, because rules are edited in place. */
    private void persist() {
        cfg.setLimitRules(rules);
    }

    /**
     * Appends a new rule and shows it. The new rule continues the existing ones: it gets the unit of the rule with the longest interval
     * and that rule's interval value + 1 (capped at {@link CaptchaSolverLimitRule#MAX_INTERVAL}), e.g. after "1 day" comes "2 days". Without
     * any existing rule the default values of a new rule apply.
     */
    public CaptchaSolverLimitRule addRule() {
        final CaptchaSolverLimitRule rule = new CaptchaSolverLimitRule();
        rule.setName(_GUI.T.CaptchaSolverLimits_new_name());
        CaptchaSolverLimitRule longest = null;
        for (final CaptchaSolverLimitRule existing : rules) {
            if (longest == null || existing._getIntervalMillis() > longest._getIntervalMillis()) {
                longest = existing;
            }
        }
        if (longest != null) {
            rule.setUnit(longest.getUnit());
            rule.setInterval(Math.min(CaptchaSolverLimitRule.MAX_INTERVAL, longest.getInterval() + 1));
        }
        rules.add(rule);
        persist();
        refresh();
        return rule;
    }

    /** Adds the example rules again (offered while the table is empty, after the user deleted all rules). */
    public void restoreExampleRules() {
        rules.addAll(CaptchaSolverLimitRule.createExampleRules());
        persist();
        refresh();
    }

    /**
     * Starts inline editing of the given rule's "Name" cell, with its current text pre-selected (like a rename-on-create). Called right
     * after a new rule was added. {@link ExtTextColumn} already selects all text once its editor field gains focus, so only starting the
     * edit and moving focus there is needed.
     */
    public void startEditingName(final CaptchaSolverLimitRule rule) {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                if (getTable() == null) {
                    return;
                }
                final int row = getRowforObject(rule);
                if (row < 0) {
                    return;
                }
                getTable().getSelectionModel().setSelectionInterval(row, row);
                if (!getTable().editCellAt(row, nameColumn.getIndex())) {
                    return;
                }
                final Component editor = getTable().getEditorComponent();
                if (editor != null) {
                    /*
                     * requestFocus (not requestFocusInWindow): the editor panel of ExtTextColumn forwards exactly this call to its text
                     * field. Just starting the edit is not enough here, because the focus is still on the "Add" button that was clicked.
                     */
                    editor.requestFocus();
                }
            }
        };
    }

    public void removeRules(final List<CaptchaSolverLimitRule> toRemove) {
        if (toRemove == null || toRemove.isEmpty()) {
            return;
        }
        rules.removeAll(toRemove);
        persist();
        refresh();
    }

    private static String getUnitLabel(final IntervalUnit unit) {
        switch (unit) {
        case MINUTES:
            return _GUI.T.CaptchaSolverLimits_unit_minutes();
        case DAYS:
            return _GUI.T.CaptchaSolverLimits_unit_days();
        case HOURS:
        default:
            return _GUI.T.CaptchaSolverLimits_unit_hours();
        }
    }

    private static String[] getUnitLabels() {
        final IntervalUnit[] units = IntervalUnit.values();
        final String[] ret = new String[units.length];
        for (int i = 0; i < units.length; i++) {
            ret[i] = getUnitLabel(units[i]);
        }
        return ret;
    }

    @Override
    protected void initColumns() {
        addColumn(new ExtCheckColumn<CaptchaSolverLimitRule>(_GUI.T.premiumaccounttablemodel_column_enabled()) {
            @Override
            public ExtTableHeaderRenderer getHeaderRenderer(final JTableHeader jTableHeader) {
                final ExtTableHeaderRenderer ret = new ExtTableHeaderRenderer(this, jTableHeader) {
                    private final Icon ok = NewTheme.I().getIcon(IconKey.ICON_OK, 14);

                    @Override
                    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
                        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                        setIcon(ok);
                        setHorizontalAlignment(CENTER);
                        setText(null);
                        return this;
                    }
                };
                return ret;
            }

            @Override
            public int getMaxWidth() {
                return 40;
            }

            @Override
            public boolean isHidable() {
                return false;
            }

            @Override
            protected boolean getBooleanValue(final CaptchaSolverLimitRule rule) {
                return rule.isEnabled();
            }

            @Override
            public boolean isEditable(final CaptchaSolverLimitRule rule) {
                return true;
            }

            @Override
            protected void setBooleanValue(final boolean enabled, final CaptchaSolverLimitRule rule) {
                rule.setEnabled(enabled);
                persist();
            }
        });
        /* The name is optional, so an empty text is a valid value. */
        nameColumn = new ExtTextColumn<CaptchaSolverLimitRule>(_GUI.T.lit_name()) {
            @Override
            public boolean isEnabled(final CaptchaSolverLimitRule rule) {
                return rule.isEnabled();
            }

            @Override
            public String getStringValue(final CaptchaSolverLimitRule rule) {
                return StringUtils.isEmpty(rule.getName()) ? "" : rule.getName();
            }

            @Override
            public boolean isEditable(final CaptchaSolverLimitRule rule) {
                return true;
            }

            @Override
            protected void setStringValue(final String value, final CaptchaSolverLimitRule rule) {
                rule.setName(value);
                persist();
            }
        };
        addColumn(nameColumn);
        addColumn(new ExtSpinnerColumn<CaptchaSolverLimitRule>(_GUI.T.CaptchaSolverLimits_column_maxCaptchas()) {
            {
                getIntModel().setMinimum(Integer.valueOf(CaptchaSolverLimitRule.MIN_VALUE));
                getIntModel().setMaximum(Integer.valueOf(CaptchaSolverLimitRule.MAX_MAX_CAPTCHAS));
            }

            @Override
            public boolean isEnabled(final CaptchaSolverLimitRule rule) {
                return rule.isEnabled();
            }

            /*
             * The super implementation must not be called: it registers a focus listener on the table which stops the editing as soon as
             * the table loses focus, and that happens right when the focus moves into the spinner, so the spinner would close immediately.
             * Every other table with spinner columns (e.g. proxies, solver timings) overrides this method without calling super, too.
             */
            @Override
            public boolean isEditable(final CaptchaSolverLimitRule rule) {
                return true;
            }

            @Override
            protected Number getNumber(final CaptchaSolverLimitRule rule) {
                return Integer.valueOf(rule.getMaxCaptchas());
            }

            @Override
            public String getStringValue(final CaptchaSolverLimitRule rule) {
                return String.valueOf(rule.getMaxCaptchas());
            }

            @Override
            protected void setNumberValue(final Number value, final CaptchaSolverLimitRule rule) {
                rule.setMaxCaptchas(Math.max(CaptchaSolverLimitRule.MIN_VALUE, Math.min(CaptchaSolverLimitRule.MAX_MAX_CAPTCHAS, value.intValue())));
                persist();
            }
        });
        addColumn(new ExtSpinnerColumn<CaptchaSolverLimitRule>(_GUI.T.CaptchaSolverLimits_column_interval()) {
            {
                getIntModel().setMinimum(Integer.valueOf(CaptchaSolverLimitRule.MIN_VALUE));
                getIntModel().setMaximum(Integer.valueOf(CaptchaSolverLimitRule.MAX_INTERVAL));
            }

            @Override
            public boolean isEnabled(final CaptchaSolverLimitRule rule) {
                return rule.isEnabled();
            }

            /* Super is not called on purpose, see the "max captchas" column. */
            @Override
            public boolean isEditable(final CaptchaSolverLimitRule rule) {
                return true;
            }

            /* The number alone is meaningless without the unit, so sorting by it would be misleading. */
            @Override
            public boolean isSortable(final CaptchaSolverLimitRule rule) {
                return false;
            }

            @Override
            protected Number getNumber(final CaptchaSolverLimitRule rule) {
                return Integer.valueOf(rule.getInterval());
            }

            @Override
            public String getStringValue(final CaptchaSolverLimitRule rule) {
                return String.valueOf(rule.getInterval());
            }

            @Override
            protected void setNumberValue(final Number value, final CaptchaSolverLimitRule rule) {
                rule.setInterval(Math.max(CaptchaSolverLimitRule.MIN_VALUE, Math.min(CaptchaSolverLimitRule.MAX_INTERVAL, value.intValue())));
                persist();
            }
        });
        /* The unit of the interval, changeable in place via a dropdown (same behavior as the "Rule Type" column of the captcha rules). */
        addColumn(new ExtComponentColumn<CaptchaSolverLimitRule>(_GUI.T.CaptchaSolverLimits_column_unit()) {
            private CaptchaSolverLimitRule editing;
            private final JComboBox        editorBox;
            private final RendererMigPanel renderer;
            private final RenderLabel      rendererLabel;
            {
                editorBox = new JComboBox(getUnitLabels());
                editorBox.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent e) {
                        if (editing == null || editorBox.getSelectedIndex() < 0) {
                            return;
                        }
                        final IntervalUnit newUnit = IntervalUnit.values()[editorBox.getSelectedIndex()];
                        if (editing.getUnit() != newUnit) {
                            editing.setUnit(newUnit);
                            persist();
                        }
                    }
                });
                rendererLabel = new RenderLabel();
                renderer = new RendererMigPanel("ins 0", "[grow,fill]", "[grow,fill]");
                renderer.add(rendererLabel);
                setClickcount(1);
            }

            @Override
            public boolean isEnabled(final CaptchaSolverLimitRule rule) {
                return rule.isEnabled();
            }

            @Override
            public boolean isSortable(final CaptchaSolverLimitRule rule) {
                return false;
            }

            @Override
            public boolean isEditable(final CaptchaSolverLimitRule rule) {
                return true;
            }

            @Override
            protected JComponent getInternalEditorComponent(final CaptchaSolverLimitRule value, final boolean isSelected, final int row, final int column) {
                return editorBox;
            }

            @Override
            protected JComponent getInternalRendererComponent(final CaptchaSolverLimitRule value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                return renderer;
            }

            @Override
            public void configureEditorComponent(final CaptchaSolverLimitRule value, final boolean isSelected, final int row, final int column) {
                editing = value;
                editorBox.setSelectedIndex(value.getUnit().ordinal());
            }

            @Override
            public void configureRendererComponent(final CaptchaSolverLimitRule value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                rendererLabel.setText(getUnitLabel(value.getUnit()));
            }

            @Override
            public void resetEditor() {
            }

            @Override
            public void resetRenderer() {
            }
        });
    }
}
