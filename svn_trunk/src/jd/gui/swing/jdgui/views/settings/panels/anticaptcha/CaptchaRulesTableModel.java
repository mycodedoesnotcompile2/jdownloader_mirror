package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;

import org.appwork.swing.MigPanel;
import org.appwork.swing.exttable.ExtTableHeaderRenderer;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtComponentColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.renderer.RenderLabel;
import org.appwork.utils.swing.renderer.RendererMigPanel;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter.CaptchaFilterType;
import org.jdownloader.captcha.v2.CaptchaChallengeFilterController;
import org.jdownloader.captcha.v2.Challenge.CaptchaRequestType;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.NewTheme;

/**
 * Table model for editing the {@link CaptchaChallengeFilter} rules managed by {@link CaptchaChallengeFilterController}. The table itself is
 * the editor: enabled/regex are toggle columns, name/domain are inline-editable text columns and the type is a dropdown. A non-editable,
 * disabled "Example rule" ({@link CaptchaChallengeFilter#isStaticRule()}) is always shown as the first row for guidance.
 */
public class CaptchaRulesTableModel extends ExtTableModel<CaptchaChallengeFilter> {
    /** Fixed example rule shown at the top of the table. It is never persisted and cannot be edited or removed. */
    private final CaptchaChallengeFilter exampleRule;

    public CaptchaRulesTableModel() {
        super("CaptchaRulesTableModel");
        exampleRule = new CaptchaChallengeFilter();
        exampleRule.setName("Example rule");
        exampleRule.setDomain("example.com");
        exampleRule.setFilterType(CaptchaFilterType.BLACKLIST);
        exampleRule.setEnabled(false);
        exampleRule.setRegex(false);
        exampleRule.setStaticRule(true);
        refresh();
    }

    /** Reloads the table content from the controller, always keeping the static example rule as the first row. */
    public void refresh() {
        final List<CaptchaChallengeFilter> rules = new ArrayList<CaptchaChallengeFilter>();
        rules.add(exampleRule);
        rules.addAll(CaptchaChallengeFilterController.getInstance().list());
        _fireTableStructureChanged(rules, true);
    }

    /** Resolves the display name of the solver a rule is assigned to. */
    private String getSolverDisplayName(final CaptchaChallengeFilter rule) {
        final String solverId = rule.getSolver();
        if (StringUtils.isEmpty(solverId)) {
            return "All solvers";
        }
        final SolverService service = ChallengeResponseController.getInstance().getServiceByID(solverId);
        if (service != null) {
            return service.getName();
        }
        return solverId;
    }

    @Override
    protected void initColumns() {
        addColumn(new ExtCheckColumn<CaptchaChallengeFilter>("Enabled") {
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
            protected boolean getBooleanValue(final CaptchaChallengeFilter rule) {
                return rule.isEnabled();
            }

            @Override
            public boolean isEditable(final CaptchaChallengeFilter rule) {
                return !rule.isStaticRule();
            }

            @Override
            protected void setBooleanValue(final boolean enabled, final CaptchaChallengeFilter rule) {
                rule.setEnabled(enabled);
                CaptchaChallengeFilterController.getInstance().persist();
            }
        });
        addColumn(new ExtTextColumn<CaptchaChallengeFilter>("Name") {
            @Override
            public String getStringValue(final CaptchaChallengeFilter rule) {
                return rule.getName();
            }

            @Override
            public boolean isEditable(final CaptchaChallengeFilter rule) {
                return !rule.isStaticRule();
            }

            @Override
            protected void setStringValue(final String value, final CaptchaChallengeFilter rule) {
                rule.setName(value);
                CaptchaChallengeFilterController.getInstance().persist();
            }
        });
        addColumn(new ExtTextColumn<CaptchaChallengeFilter>("Solver") {
            @Override
            public String getStringValue(final CaptchaChallengeFilter rule) {
                return getSolverDisplayName(rule);
            }
        });
        addColumn(new ExtComponentColumn<CaptchaChallengeFilter>("Type") {
            private CaptchaChallengeFilter editing;
            private final JComboBox        editorBox;
            private final RendererMigPanel renderer;
            private final RenderLabel      rendererLabel;

            {
                editorBox = new JComboBox(new String[] { "Blacklist", "Whitelist" });
                editorBox.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent e) {
                        if (editing == null || editing.isStaticRule()) {
                            return;
                        }
                        final CaptchaFilterType newType = editorBox.getSelectedIndex() == 1 ? CaptchaFilterType.WHITELIST : CaptchaFilterType.BLACKLIST;
                        if (editing.getFilterType() != newType) {
                            editing.setFilterType(newType);
                            CaptchaChallengeFilterController.getInstance().persist();
                        }
                    }
                });
                rendererLabel = new RenderLabel();
                renderer = new RendererMigPanel("ins 0", "[grow,fill]", "[grow,fill]");
                renderer.add(rendererLabel);
                setClickcount(1);
            }

            @Override
            public boolean isSortable(final CaptchaChallengeFilter obj) {
                return false;
            }

            @Override
            public boolean isEditable(final CaptchaChallengeFilter rule) {
                return !rule.isStaticRule();
            }

            @Override
            protected JComponent getInternalEditorComponent(final CaptchaChallengeFilter value, final boolean isSelected, final int row, final int column) {
                return editorBox;
            }

            @Override
            protected JComponent getInternalRendererComponent(final CaptchaChallengeFilter value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                return renderer;
            }

            @Override
            public void configureEditorComponent(final CaptchaChallengeFilter value, final boolean isSelected, final int row, final int column) {
                editing = value;
                editorBox.setSelectedIndex(value.getFilterType() == CaptchaFilterType.WHITELIST ? 1 : 0);
            }

            @Override
            public void configureRendererComponent(final CaptchaChallengeFilter value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                rendererLabel.setText(value.getFilterType() == CaptchaFilterType.WHITELIST ? "Whitelist" : "Blacklist");
            }

            @Override
            public void resetEditor() {
            }

            @Override
            public void resetRenderer() {
            }
        });
        addColumn(new ExtComponentColumn<CaptchaChallengeFilter>("Captcha request types") {
            private CaptchaChallengeFilter editing;
            private final JButton          editorBtn;
            private final MigPanel         editor;
            private final RendererMigPanel renderer;
            private final RenderLabel      rendererLabel;
            private final JPopupMenu       popup;
            {
                rendererLabel = new RenderLabel();
                renderer = new RendererMigPanel("ins 0", "[grow,fill]", "[grow,fill]");
                renderer.add(rendererLabel);
                popup = new JPopupMenu();
                editorBtn = new JButton();
                editorBtn.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent e) {
                        if (editing == null || editing.isStaticRule()) {
                            return;
                        }
                        showPopup(editing);
                    }
                });
                editor = new MigPanel("ins 0", "[grow,fill]", "[grow,fill]");
                editor.add(editorBtn, "height 18!");
                setClickcount(1);
            }

            /** Empty set means "all request types"; renders as [selected/total]. */
            private String labelFor(final CaptchaChallengeFilter rule) {
                final Set<CaptchaRequestType> set = rule.getCaptchaRequestTypes();
                final int total = CaptchaRequestType.values().length;
                final int sel = (set == null || set.isEmpty()) ? total : set.size();
                return "[" + sel + "/" + total + "]";
            }

            private void showPopup(final CaptchaChallengeFilter rule) {
                popup.removeAll();
                final Set<CaptchaRequestType> current = rule.getCaptchaRequestTypes();
                final CaptchaRequestType[] all = CaptchaRequestType.values();
                for (int i = 0; i < all.length; i++) {
                    final CaptchaRequestType type = all[i];
                    final boolean selected = current == null || current.isEmpty() || current.contains(type);
                    final JCheckBoxMenuItem item = new JCheckBoxMenuItem(type.getLabel(), selected);
                    item.addActionListener(new ActionListener() {
                        @Override
                        public void actionPerformed(final ActionEvent e) {
                            toggle(rule, type, item.isSelected());
                        }
                    });
                    popup.add(item);
                }
                popup.show(editorBtn, 0, editorBtn.getHeight());
            }

            private void toggle(final CaptchaChallengeFilter rule, final CaptchaRequestType type, final boolean selected) {
                Set<CaptchaRequestType> set = rule.getCaptchaRequestTypes();
                if (set == null) {
                    set = new HashSet<CaptchaRequestType>();
                }
                if (set.isEmpty()) {
                    /* Empty means "all"; materialize the full set before removing one. */
                    final CaptchaRequestType[] all = CaptchaRequestType.values();
                    for (int i = 0; i < all.length; i++) {
                        set.add(all[i]);
                    }
                }
                if (selected) {
                    set.add(type);
                } else {
                    set.remove(type);
                }
                rule.setCaptchaRequestTypes(set);
                CaptchaChallengeFilterController.getInstance().persist();
                if (getModel() != null && getModel().getTable() != null) {
                    getModel().getTable().repaint();
                }
            }

            @Override
            public boolean isSortable(final CaptchaChallengeFilter obj) {
                return false;
            }

            @Override
            public boolean isEditable(final CaptchaChallengeFilter rule) {
                return !rule.isStaticRule();
            }

            @Override
            protected JComponent getInternalEditorComponent(final CaptchaChallengeFilter value, final boolean isSelected, final int row, final int column) {
                return editor;
            }

            @Override
            protected JComponent getInternalRendererComponent(final CaptchaChallengeFilter value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                return renderer;
            }

            @Override
            public void configureEditorComponent(final CaptchaChallengeFilter value, final boolean isSelected, final int row, final int column) {
                editing = value;
                editorBtn.setText(labelFor(value));
            }

            @Override
            public void configureRendererComponent(final CaptchaChallengeFilter value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                rendererLabel.setText(labelFor(value));
            }

            @Override
            public void resetEditor() {
            }

            @Override
            public void resetRenderer() {
            }
        });
        addColumn(new ExtCheckColumn<CaptchaChallengeFilter>("Regex") {
            @Override
            public int getMaxWidth() {
                return 50;
            }

            @Override
            protected boolean getBooleanValue(final CaptchaChallengeFilter rule) {
                return rule.isRegex();
            }

            @Override
            public boolean isEditable(final CaptchaChallengeFilter rule) {
                return !rule.isStaticRule();
            }

            @Override
            protected void setBooleanValue(final boolean regex, final CaptchaChallengeFilter rule) {
                rule.setRegex(regex);
                CaptchaChallengeFilterController.getInstance().persist();
            }
        });
        addColumn(new ExtTextColumn<CaptchaChallengeFilter>("Domains comma separated / Regex") {
            @Override
            public String getStringValue(final CaptchaChallengeFilter rule) {
                return rule.getDomain();
            }

            @Override
            public boolean isEditable(final CaptchaChallengeFilter rule) {
                return !rule.isStaticRule();
            }

            @Override
            protected void setStringValue(final String value, final CaptchaChallengeFilter rule) {
                rule.setDomain(value);
                CaptchaChallengeFilterController.getInstance().persist();
            }
        });
    }
}
