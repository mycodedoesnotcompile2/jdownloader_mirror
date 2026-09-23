package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;

import org.appwork.swing.MigPanel;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtComponentRowHighlighter;
import org.appwork.swing.exttable.ExtTableHeaderRenderer;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtComponentColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.renderer.RenderLabel;
import org.appwork.utils.swing.renderer.RendererMigPanel;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter.CaptchaFilterType;
import org.jdownloader.captcha.v2.CaptchaChallengeFilterController;
import org.jdownloader.captcha.v2.Challenge.CaptchaRequestType;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.logging.LogController;

import jd.gui.swing.jdgui.views.settings.components.MultiComboBox;
import jd.plugins.CaptchaType;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * Table model for editing the {@link CaptchaChallengeFilter} rules managed by {@link CaptchaChallengeFilterController}. The table itself is
 * the editor: enabled/regex are toggle columns, name/domain are inline-editable text columns and solver/type are dropdowns. Static rules
 * ({@link CaptchaChallengeFilter#isStaticRule()}) stay supported by every column (never editable/removable/draggable), but nothing
 * currently creates one. Rows can be reordered by drag & drop (see {@link #move(List, int)}, used by {@link CaptchaRulesTable}); disabled
 * rules are rendered grayed out (every column reports {@code isEnabled(rule) == rule.isEnabled()}); rules that cannot ever match anything
 * (see {@link #isBroken(CaptchaChallengeFilter)}) are additionally rendered with red text (see the highlighter registered in the
 * constructor).
 */
public class CaptchaRulesTableModel extends ExtTableModel<CaptchaChallengeFilter> {
    /** The "Name" column, kept to start inline editing on it right after a rule was added, see {@link #startEditingName(CaptchaChallengeFilter)}. */
    private ExtTextColumn<CaptchaChallengeFilter> nameColumn;

    public CaptchaRulesTableModel() {
        super("CaptchaRulesTableModel");
        addExtComponentRowHighlighter(new ExtComponentRowHighlighter<CaptchaChallengeFilter>(Color.RED, null, null) {
            @Override
            public boolean accept(final ExtColumn<CaptchaChallengeFilter> column, final CaptchaChallengeFilter rule, final boolean selected, final boolean focus, final int row) {
                return isBroken(rule);
            }
        });
        refresh();
    }

    /**
     * The request types this rule applies to, i.e. all types minus the rule's excluded ones (see
     * {@link CaptchaChallengeFilter#getExcludedCaptchaRequestTypes()}). The GUI always shows/edits this "included" set; only the
     * complement is persisted, so that request types added in the future are included by default without migrating existing rules.
     */
    private Set<CaptchaRequestType> getIncludedRequestTypes(final CaptchaChallengeFilter rule) {
        final Set<CaptchaRequestType> excluded = rule.getExcludedCaptchaRequestTypes();
        final Set<CaptchaRequestType> included = new HashSet<CaptchaRequestType>(Arrays.asList(CaptchaRequestType.values()));
        included.removeAll(excluded);
        return included;
    }

    /** Persists the GUI's "included" request type selection as its complement, see {@link #getIncludedRequestTypes(CaptchaChallengeFilter)}. */
    private void setIncludedRequestTypes(final CaptchaChallengeFilter rule, final Set<CaptchaRequestType> included) {
        final Set<CaptchaRequestType> excluded = new HashSet<CaptchaRequestType>(Arrays.asList(CaptchaRequestType.values()));
        excluded.removeAll(included);
        rule.setExcludedCaptchaRequestTypes(excluded);
    }

    /**
     * The captcha types this rule applies to, i.e. all (processable) types minus the rule's excluded ones (see
     * {@link CaptchaChallengeFilter#getExcludedCaptchaTypes()}). Same "included in the GUI, excluded in storage" principle as
     * {@link #getIncludedRequestTypes(CaptchaChallengeFilter)}.
     */
    private Set<CAPTCHA_TYPE> getIncludedCaptchaTypes(final CaptchaChallengeFilter rule) {
        final Set<CAPTCHA_TYPE> excluded = rule.getExcludedCaptchaTypes();
        final Set<CAPTCHA_TYPE> included = new HashSet<CAPTCHA_TYPE>(CaptchaType.getProcessableCaptchaTypes());
        included.removeAll(excluded);
        return included;
    }

    /** Persists the GUI's "included" captcha type selection as its complement, see {@link #getIncludedCaptchaTypes(CaptchaChallengeFilter)}. */
    private void setIncludedCaptchaTypes(final CaptchaChallengeFilter rule, final Set<CAPTCHA_TYPE> included) {
        final Set<CAPTCHA_TYPE> excluded = new HashSet<CAPTCHA_TYPE>(CaptchaType.getProcessableCaptchaTypes());
        excluded.removeAll(included);
        rule.setExcludedCaptchaTypes(excluded);
    }

    /**
     * A rule that can never match anything is broken: no request type is included, no captcha type is included, the domain is empty,
     * (with regex enabled) the domain is not a compilable regex, or the assigned solver no longer exists (a non-empty id that does not
     * resolve to a currently existing solver; an empty id, "applies to all solvers", is not broken). Static rules are never considered
     * broken.
     */
    private boolean isBroken(final CaptchaChallengeFilter rule) {
        if (rule.isStaticRule()) {
            return false;
        }
        if (getIncludedRequestTypes(rule).isEmpty()) {
            return true;
        }
        if (getIncludedCaptchaTypes(rule).isEmpty()) {
            return true;
        }
        if (StringUtils.isEmpty(rule.getDomain())) {
            return true;
        }
        if (rule.isRegex()) {
            try {
                Pattern.compile(rule.getDomain(), Pattern.CASE_INSENSITIVE);
            } catch (final PatternSyntaxException e) {
                return true;
            }
        }
        if (!StringUtils.isEmpty(rule.getSolver()) && getSolverService(rule) == null) {
            return true;
        }
        return false;
    }

    /**
     * Reloads the table content from the controller. Nothing is hidden or auto-removed: every persisted rule is shown, including ones
     * with a broken solver reference or an empty solver id ("applies to all solvers", not offered by the solver dropdown but still a
     * legitimate, supported value at the filter-evaluation level) -- both are flagged via {@link #isBroken(CaptchaChallengeFilter)}
     * instead.
     */
    public void refresh() {
        _fireTableStructureChanged(CaptchaChallengeFilterController.getInstance().list(), true);
    }

    /**
     * Starts inline editing of the given rule's "Name" cell, with its current text pre-selected (like a rename-on-create). Called right
     * after a new rule was added. {@link org.appwork.swing.exttable.columns.ExtTextColumn} already selects all text once its editor field
     * gains focus, so only starting the edit and moving focus there is needed.
     */
    public void startEditingName(final CaptchaChallengeFilter rule) {
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
                getTable().editCellAt(row, nameColumn.getIndex());
                final Component editor = getTable().getEditorComponent();
                if (editor != null) {
                    editor.requestFocusInWindow();
                }
            }
        };
    }

    /** Resolves the solver a rule is assigned to, or null if the id is empty or no longer exists. */
    private SolverService getSolverService(final CaptchaChallengeFilter rule) {
        final String solverId = rule.getSolver();
        if (StringUtils.isEmpty(solverId)) {
            return null;
        }
        return ChallengeResponseController.getInstance().getServiceByID(solverId);
    }

    /**
     * Moves the dragged rules to the given drop row (an index into the currently displayed, possibly filtered rows) and persists the new
     * order. Rules hidden by {@link #refresh()} (broken solver reference) are left untouched at their original position in the persisted
     * list; only the relative order of the displayed rules changes.
     */
    @Override
    public boolean move(final List<CaptchaChallengeFilter> transferData, final int dropRow) {
        try {
            final List<CaptchaChallengeFilter> draggedRules = new ArrayList<CaptchaChallengeFilter>();
            for (final CaptchaChallengeFilter rule : transferData) {
                if (!rule.isStaticRule()) {
                    draggedRules.add(rule);
                }
            }
            if (draggedRules.isEmpty()) {
                return false;
            }
            final List<CaptchaChallengeFilter> displayed = new ArrayList<CaptchaChallengeFilter>(getTableData());
            int targetIndex = dropRow;
            if (targetIndex < 0) {
                targetIndex = 0;
            } else if (targetIndex > displayed.size()) {
                targetIndex = displayed.size();
            }
            final List<CaptchaChallengeFilter> before = new ArrayList<CaptchaChallengeFilter>(displayed.subList(0, targetIndex));
            final List<CaptchaChallengeFilter> after = new ArrayList<CaptchaChallengeFilter>(displayed.subList(targetIndex, displayed.size()));
            before.removeAll(draggedRules);
            after.removeAll(draggedRules);
            final List<CaptchaChallengeFilter> newDisplayOrder = new ArrayList<CaptchaChallengeFilter>();
            newDisplayOrder.addAll(before);
            newDisplayOrder.addAll(draggedRules);
            newDisplayOrder.addAll(after);
            /* Merge the new order of the displayed rows back into the full persisted list; hidden rules keep their original slot. */
            final Set<CaptchaChallengeFilter> displayedSet = new HashSet<CaptchaChallengeFilter>(displayed);
            final java.util.Iterator<CaptchaChallengeFilter> newOrderIterator = newDisplayOrder.iterator();
            final List<CaptchaChallengeFilter> newPersisted = new ArrayList<CaptchaChallengeFilter>();
            for (final CaptchaChallengeFilter rule : CaptchaChallengeFilterController.getInstance().list()) {
                if (displayedSet.contains(rule)) {
                    if (newOrderIterator.hasNext()) {
                        newPersisted.add(newOrderIterator.next());
                    }
                } else {
                    newPersisted.add(rule);
                }
            }
            while (newOrderIterator.hasNext()) {
                newPersisted.add(newOrderIterator.next());
            }
            for (int i = 0; i < newPersisted.size(); i++) {
                newPersisted.get(i).setPosition(i);
            }
            CaptchaChallengeFilterController.getInstance().set(newPersisted);
            refresh();
            return true;
        } catch (final Throwable t) {
            LogController.CL().log(t);
        }
        return false;
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
        nameColumn = new ExtTextColumn<CaptchaChallengeFilter>("Name") {
            @Override
            public boolean isEnabled(final CaptchaChallengeFilter rule) {
                return rule.isEnabled();
            }

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
        };
        addColumn(nameColumn);
        /* The solver a rule applies to, changeable in place via a dropdown (rebuilt on every edit so newly added solvers show up). */
        addColumn(new ExtComponentColumn<CaptchaChallengeFilter>(_GUI.T.CaptchaSolverComparison_column_solver()) {
            private CaptchaChallengeFilter editing;
            /*
             * True while configureEditorComponent (re-)populates the combo box. JComboBox fires a selection-changed action event as a side
             * effect of removeAllItems()/addItem()/setSelectedItem() (e.g. the first added item auto-selects), which must NOT be mistaken for
             * a real user choice -- that previously reset the rule's solver to "All solvers" whenever the editor was merely (re-)configured.
             */
            private boolean                adjusting = false;
            private final JComboBox        editorBox;
            private final RendererMigPanel renderer;
            private final RenderLabel      rendererLabel;
            {
                editorBox = new JComboBox();
                editorBox.setRenderer(new SolverListCellRenderer());
                editorBox.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent e) {
                        if (adjusting || editing == null || editing.isStaticRule()) {
                            return;
                        }
                        final Object selected = editorBox.getSelectedItem();
                        if (!(selected instanceof SolverService)) {
                            return;
                        }
                        final String newSolverId = ((SolverService) selected).getID();
                        if (!StringUtils.equals(editing.getSolver(), newSolverId)) {
                            editing.setSolver(newSolverId);
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
            public boolean isEnabled(final CaptchaChallengeFilter rule) {
                return rule.isEnabled();
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
                adjusting = true;
                try {
                    editing = value;
                    editorBox.removeAllItems();
                    /* "All solvers" (empty/broken solver id) is no longer offered: a rule must always name one concrete, existing solver. */
                    for (final SolverService service : ChallengeResponseController.getInstance().listServices()) {
                        editorBox.addItem(service);
                    }
                    editorBox.setSelectedItem(getSolverService(value));
                } finally {
                    adjusting = false;
                }
            }

            @Override
            public void configureRendererComponent(final CaptchaChallengeFilter value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                final SolverService service = getSolverService(value);
                /*
                 * Nothing is hidden or auto-removed here anymore (see isBroken(CaptchaChallengeFilter)): an empty solver id ("applies to
                 * all solvers", no longer assignable via the dropdown, but still a legitimate value) and a broken one (a non-empty id that
                 * no longer resolves) are both shown as-is; the row is additionally flagged red by the broken-row highlighter.
                 */
                if (service != null) {
                    rendererLabel.setText(service.getName());
                    rendererLabel.setIcon(service.getIcon(16));
                } else if (StringUtils.isEmpty(value.getSolver())) {
                    rendererLabel.setText(_GUI.T.CaptchaRules_allSolvers());
                    rendererLabel.setIcon(null);
                } else {
                    rendererLabel.setText(value.getSolver());
                    rendererLabel.setIcon(null);
                }
            }

            @Override
            public void resetEditor() {
            }

            @Override
            public void resetRenderer() {
            }
        });
        addColumn(new ExtComponentColumn<CaptchaChallengeFilter>("Rule Type") {
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
            public boolean isEnabled(final CaptchaChallengeFilter rule) {
                return rule.isEnabled();
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
            {
                rendererLabel = new RenderLabel();
                renderer = new RendererMigPanel("ins 0", "[grow,fill]", "[grow,fill]");
                renderer.add(rendererLabel);
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

            /**
             * Same "[selected/total] [Label1, Label2, ...]" format as the multi-select ENUM columns in the Advanced Settings (see
             * {@code AdvancedValueColumn}).
             */
            private String labelFor(final CaptchaChallengeFilter rule) {
                final Set<CaptchaRequestType> selected = getIncludedRequestTypes(rule);
                final List<String> labels = new ArrayList<String>();
                for (final CaptchaRequestType type : CaptchaRequestType.values()) {
                    if (selected.contains(type)) {
                        labels.add(type.getLabel());
                    }
                }
                return "[" + labels.size() + "/" + CaptchaRequestType.values().length + "] " + labels.toString();
            }

            /** Same multi-select popup control used by the Advanced Settings' multi-select ENUM columns. */
            private void showPopup(final CaptchaChallengeFilter rule) {
                /*
                 * Captured here (ExtComponentColumn's own getModel()), not called from inside the MultiComboBox subclass below: there
                 * "getModel()" would resolve to the unrelated, inherited AbstractButton.getModel() (a ButtonModel) instead.
                 */
                final ExtTableModel<CaptchaChallengeFilter> outerModel = getModel();
                final MultiComboBox<CaptchaRequestType> comp = new MultiComboBox<CaptchaRequestType>(CaptchaRequestType.values()) {
                    {
                        setSelectedItems(new ArrayList<CaptchaRequestType>(getIncludedRequestTypes(rule)));
                    }

                    @Override
                    public void onChanged() {
                        super.onChanged();
                        final Set<CaptchaRequestType> newIncluded = new HashSet<CaptchaRequestType>(getSelectedItems());
                        if (newIncluded.equals(getIncludedRequestTypes(rule))) {
                            /* setSelectedItems() (called once from the instance initializer above to reflect the current value into the
                             * popup) also triggers this method; skip persisting when nothing actually changed. */
                            return;
                        }
                        setIncludedRequestTypes(rule, newIncluded);
                        CaptchaChallengeFilterController.getInstance().persist();
                        if (outerModel != null && outerModel.getTable() != null) {
                            outerModel.getTable().repaint();
                        }
                    }
                };
                final JPopupMenu popup = comp.getPopup();
                popup.setPreferredSize(new Dimension(Math.max(editorBtn.getWidth(), popup.getPreferredSize().width), popup.getPreferredSize().height));
                popup.show(editorBtn, 0, editorBtn.getHeight());
            }

            @Override
            public boolean isEnabled(final CaptchaChallengeFilter rule) {
                return rule.isEnabled();
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
        addColumn(new ExtComponentColumn<CaptchaChallengeFilter>("Captcha types") {
            private CaptchaChallengeFilter editing;
            private final JButton          editorBtn;
            private final MigPanel         editor;
            private final RendererMigPanel renderer;
            private final RenderLabel      rendererLabel;
            {
                rendererLabel = new RenderLabel();
                renderer = new RendererMigPanel("ins 0", "[grow,fill]", "[grow,fill]");
                renderer.add(rendererLabel);
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

            /** Same "[selected/total] [Label1, Label2, ...]" format as the "Captcha request types" column. */
            private String labelFor(final CaptchaChallengeFilter rule) {
                final Set<CAPTCHA_TYPE> selected = getIncludedCaptchaTypes(rule);
                final List<CAPTCHA_TYPE> allTypes = CaptchaType.getProcessableCaptchaTypes();
                final List<String> labels = new ArrayList<String>();
                for (final CAPTCHA_TYPE type : allTypes) {
                    if (selected.contains(type)) {
                        labels.add(type.getDisplayName());
                    }
                }
                return "[" + labels.size() + "/" + allTypes.size() + "] " + labels.toString();
            }

            /** Same multi-select popup control as the "Captcha request types" column. */
            private void showPopup(final CaptchaChallengeFilter rule) {
                /*
                 * Captured here (ExtComponentColumn's own getModel()), not called from inside the MultiComboBox subclass below: there
                 * "getModel()" would resolve to the unrelated, inherited AbstractButton.getModel() (a ButtonModel) instead.
                 */
                final ExtTableModel<CaptchaChallengeFilter> outerModel = getModel();
                /* CAPTCHA_TYPE has no LabelInterface, so the per-item label comes from getDisplayName() (same as SolverComparisonContainer). */
                final MultiComboBox<CAPTCHA_TYPE> comp = new MultiComboBox<CAPTCHA_TYPE>(CaptchaType.getProcessableCaptchaTypes()) {
                    {
                        setSelectedItems(new ArrayList<CAPTCHA_TYPE>(getIncludedCaptchaTypes(rule)));
                    }

                    @Override
                    protected String getLabel(final CAPTCHA_TYPE type) {
                        return type.getDisplayName();
                    }

                    @Override
                    public void onChanged() {
                        super.onChanged();
                        final Set<CAPTCHA_TYPE> newIncluded = new HashSet<CAPTCHA_TYPE>(getSelectedItems());
                        if (newIncluded.equals(getIncludedCaptchaTypes(rule))) {
                            /* setSelectedItems() (called once from the instance initializer above to reflect the current value into the
                             * popup) also triggers this method; skip persisting when nothing actually changed. */
                            return;
                        }
                        setIncludedCaptchaTypes(rule, newIncluded);
                        CaptchaChallengeFilterController.getInstance().persist();
                        if (outerModel != null && outerModel.getTable() != null) {
                            outerModel.getTable().repaint();
                        }
                    }
                };
                final JPopupMenu popup = comp.getPopup();
                popup.setPreferredSize(new Dimension(Math.max(editorBtn.getWidth(), popup.getPreferredSize().width), popup.getPreferredSize().height));
                popup.show(editorBtn, 0, editorBtn.getHeight());
            }

            @Override
            public boolean isEnabled(final CaptchaChallengeFilter rule) {
                return rule.isEnabled();
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
        addColumn(new ExtCheckColumn<CaptchaChallengeFilter>(_GUI.T.CaptchaRules_column_regex()) {
            @Override
            public boolean isEnabled(final CaptchaChallengeFilter rule) {
                return rule.isEnabled();
            }

            /* Tooltip for the column HEADER only (this explains the column itself), not for every checkbox cell/row. */
            @Override
            public String getHeaderTooltip() {
                return _GUI.T.CaptchaRules_regex_tooltip();
            }

            /*
             * No fixed cap: the previous 50px cap was narrower than the "Regex" header text itself. The header text now decides the default
             * width (via getDefaultWidth), and the column stays freely resizable like any other (isDefaultResizable defaults to true).
             */
            @Override
            public int getDefaultWidth() {
                return 70;
            }

            @Override
            public int getMaxWidth() {
                return 120;
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
            public boolean isEnabled(final CaptchaChallengeFilter rule) {
                return rule.isEnabled();
            }

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
