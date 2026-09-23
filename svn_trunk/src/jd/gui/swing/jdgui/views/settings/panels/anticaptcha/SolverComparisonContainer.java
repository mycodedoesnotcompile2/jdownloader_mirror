package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;

import org.appwork.swing.MigPanel;
import org.appwork.swing.exttable.ExtColumn;
import org.jdownloader.captcha.v2.CaptchaHistoryEntry;
import org.jdownloader.captcha.v2.CaptchaHistoryManager;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.captchasolver.PluginForCaptchaSolverSolverService;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.gui.swing.jdgui.views.settings.components.MultiComboBox;
import jd.gui.swing.jdgui.views.settings.components.SettingsComponent;
import jd.plugins.CaptchaType;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * "External Solver comparison Table" tab: shows which external captcha solver supports which captcha type (rows = solvers, columns =
 * captcha types). Above the table the user picks which captcha types are shown: a dropdown preselects a set of types (all / manual
 * selection / the types the user ever needed according to the {@link CaptchaHistoryManager}) and a multi-select popup allows changing
 * that selection freely afterwards.
 */
public class SolverComparisonContainer extends MigPanel implements SettingsComponent {
    /** Sets of captcha types the "preselected captcha types" dropdown can select. */
    private enum Preselect {
        /** All captcha types. */
        ALL,
        /** All captcha types, meant as starting point for a manual selection. */
        SELECTION,
        /** The captcha types the user ever needed (captcha history). Only offered if the history contains any entry. */
        USED
    }

    private final SolverComparisonTableModel     model;
    private final BasicJDTable<SolverService>    table;
    private final JComboBox                      presetCombo;
    private final JCheckBox                      onlyFullSupportCheckBox;
    private final JLabel                         emptyLabel;
    /**
     * Multi-select "[x/y] [Type1, Type2, ...]" control (same widget/label format as the multi-select ENUM columns in the Advanced
     * Settings, see {@code AdvancedValueColumn}), doubling as its own button and popup.
     */
    private final MultiComboBox<CAPTCHA_TYPE>    typesCombo;
    /** All captcha types JDownloader can process, i.e. all possible columns. */
    private final List<CAPTCHA_TYPE>             allTypes;
    /** Captcha types whose columns are currently shown. */
    private final Set<CAPTCHA_TYPE>              selectedTypes = new LinkedHashSet<CAPTCHA_TYPE>();
    /** True while the dropdown/multi-select is modified programmatically so that its listener does not reset the user's selection. */
    private boolean                              adjusting     = false;
    private final JScrollPane                    scrollPane;
    /**
     * The captcha types of the "Selection" preset, remembered for the running session only (not persisted). null = the user never
     * populated it, the preset then selects all types.
     */
    private Set<CAPTCHA_TYPE>                    rememberedSelection = null;

    public SolverComparisonContainer() {
        super("ins 0, wrap 1", "[grow,fill]", "[][][grow,fill][]");
        allTypes = CaptchaType.getProcessableCaptchaTypes();
        model = new SolverComparisonTableModel();
        emptyLabel = new JLabel();
        emptyLabel.setForeground(Color.RED);
        emptyLabel.setFont(emptyLabel.getFont().deriveFont(Font.BOLD, 20f));
        table = new BasicJDTable<SolverService>(model) {
            private static final long serialVersionUID = 1L;

            /* Large red hint inside the table if the "supports all selected types" filter removed every solver. */
            @Override
            public void paintComponent(final Graphics g) {
                super.paintComponent(g);
                if (SolverComparisonContainer.this.model.isEmptyDueToFilter()) {
                    final int width = Math.max(100, getWidth() - 40);
                    emptyLabel.setText("<html><body style='width:" + width + "px;text-align:center'>" + _GUI.T.CaptchaSolverComparison_noSolverSupportsAll() + "</body></html>");
                    final Dimension size = emptyLabel.getPreferredSize();
                    emptyLabel.setSize(size);
                    final Graphics2D g2 = (Graphics2D) g.create();
                    try {
                        g2.translate((getWidth() - size.width) / 2, getTableHeader().getHeight() + 30);
                        emptyLabel.paint(g2);
                    } finally {
                        g2.dispose();
                    }
                }
            }

            /* The shown columns are controlled by the selection above the table only, so the table's own column menu/button is disabled. */
            @Override
            protected JPopupMenu columnControlMenu(final ExtColumn<SolverService> extColumn) {
                return null;
            }

            @Override
            public boolean isColumnButtonVisible() {
                return false;
            }
        };
        /* Many captcha type columns: keep their width and scroll horizontally instead of squeezing them. */
        table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        /* Multi-select "[x/y] [Type1, Type2]" control; CAPTCHA_TYPE has no LabelInterface, so the per-item label comes from getDisplayName(). */
        typesCombo = new MultiComboBox<CAPTCHA_TYPE>(allTypes) {
            private static final long serialVersionUID = 1L;

            @Override
            protected String getLabel(final CAPTCHA_TYPE type) {
                return type.getDisplayName();
            }

            @Override
            protected String getLabel(final List<CAPTCHA_TYPE> list) {
                final List<String> labels = new ArrayList<String>();
                for (final CAPTCHA_TYPE type : list) {
                    labels.add(getLabel(type));
                }
                return "[" + list.size() + "/" + getValues().size() + "] " + labels.toString();
            }

            @Override
            public void onChanged() {
                super.onChanged();
                if (adjusting) {
                    return;
                }
                selectedTypes.clear();
                selectedTypes.addAll(getSelectedItems());
                onManualSelectionChange();
            }
        };
        presetCombo = new JComboBox();
        presetCombo.setRenderer(new DefaultListCellRenderer() {
            private static final long serialVersionUID = 1L;

            @Override
            public Component getListCellRendererComponent(final JList list, final Object value, final int index, final boolean isSelected, final boolean cellHasFocus) {
                final JLabel label = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (value instanceof Preselect) {
                    label.setText(getPresetName((Preselect) value));
                }
                return label;
            }
        });
        presetCombo.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                if (!adjusting && presetCombo.getSelectedItem() instanceof Preselect) {
                    applyPreset((Preselect) presetCombo.getSelectedItem());
                }
            }
        });
        final MigPanel selectionBar = new MigPanel("ins 0", "[][][grow,fill]", "[]");
        selectionBar.add(new JLabel(_GUI.T.CaptchaSolverComparison_preselect_label()));
        selectionBar.add(presetCombo, "height 26!");
        selectionBar.add(typesCombo, "height 26!");
        add(selectionBar, "growx");
        onlyFullSupportCheckBox = new JCheckBox(_GUI.T.CaptchaSolverComparison_onlyFullSupport());
        onlyFullSupportCheckBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                model.setOnlyFullSupport(onlyFullSupportCheckBox.isSelected());
            }
        });
        add(onlyFullSupportCheckBox, "growx");
        scrollPane = new JScrollPane(table);
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        /* Re-evaluate whether the columns fit whenever the available width changes. */
        scrollPane.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(final ComponentEvent e) {
                updateResizeMode();
            }
        });
        add(scrollPane, "grow");
        add(new JLabel(_GUI.T.CaptchaSolverComparison_disclaimer()), "growx, wmin 10");
        /* Default: all captcha types selected. */
        selectedTypes.addAll(allTypes);
        syncTypesCombo();
        onSelectionChanged();
        refresh();
    }

    private static String getPresetName(final Preselect preset) {
        switch (preset) {
        case SELECTION:
            return _GUI.T.CaptchaSolverComparison_preselect_selection();
        case USED:
            return _GUI.T.CaptchaSolverComparison_preselect_used();
        case ALL:
        default:
            return _GUI.T.CaptchaSolverComparison_preselect_all();
        }
    }

    /**
     * Reloads the rows (external solvers) and the offered presets. Call this whenever the tab becomes visible again, since solvers and the
     * captcha history change while the application is running.
     */
    public void refresh() {
        final List<SolverService> externalSolvers = new ArrayList<SolverService>();
        for (final SolverService service : ChallengeResponseController.getInstance().listServices()) {
            if (service instanceof PluginForCaptchaSolverSolverService) {
                externalSolvers.add(service);
            }
        }
        model.setSolvers(externalSolvers);
        rebuildPresetCombo();
    }

    /** "Used by you" is only offered if the captcha history contains at least one captcha type. */
    private void rebuildPresetCombo() {
        final Object current = presetCombo.getSelectedItem();
        final boolean usedAvailable = !getUsedTypes().isEmpty();
        Preselect newSelection = Preselect.ALL;
        boolean presetNotAvailableAnymore = false;
        if (current instanceof Preselect) {
            switch ((Preselect) current) {
            case USED:
                if (usedAvailable) {
                    newSelection = Preselect.USED;
                } else {
                    /* The previously chosen preset is not available anymore (history got cleared). */
                    presetNotAvailableAnymore = true;
                }
                break;
            case ALL:
            case SELECTION:
            default:
                newSelection = (Preselect) current;
                break;
            }
        }
        adjusting = true;
        try {
            presetCombo.removeAllItems();
            presetCombo.addItem(Preselect.ALL);
            presetCombo.addItem(Preselect.SELECTION);
            if (usedAvailable) {
                presetCombo.addItem(Preselect.USED);
            }
            presetCombo.setSelectedItem(newSelection);
        } finally {
            adjusting = false;
        }
        if (presetNotAvailableAnymore) {
            applyPreset(newSelection);
        }
    }

    /** Returns all captcha types the user ever needed according to the captcha history (only types JDownloader can process). */
    private Set<CAPTCHA_TYPE> getUsedTypes() {
        final Set<CAPTCHA_TYPE> usedTypes = new LinkedHashSet<CAPTCHA_TYPE>();
        for (final CaptchaHistoryEntry entry : CaptchaHistoryManager.getInstance().getAllEntries()) {
            final CAPTCHA_TYPE type = entry.getCaptcha_type();
            if (type != null && allTypes.contains(type)) {
                usedTypes.add(type);
            }
        }
        return usedTypes;
    }

    /** Returns the captcha types the given preset selects. */
    private Set<CAPTCHA_TYPE> getPresetTypes(final Preselect preset) {
        switch (preset) {
        case USED:
            return getUsedTypes();
        case SELECTION:
            if (rememberedSelection != null) {
                return new LinkedHashSet<CAPTCHA_TYPE>(rememberedSelection);
            }
            return new LinkedHashSet<CAPTCHA_TYPE>(allTypes);
        case ALL:
        default:
            return new LinkedHashSet<CAPTCHA_TYPE>(allTypes);
        }
    }

    private void applyPreset(final Preselect preset) {
        selectedTypes.clear();
        selectedTypes.addAll(getPresetTypes(preset));
        syncTypesCombo();
        onSelectionChanged();
    }

    /**
     * Called when the user changed the captcha type selection manually: as soon as it differs from what the current preset selects, the
     * preselect dropdown switches to "Selection".
     */
    private void onManualSelectionChange() {
        final Object currentPreset = presetCombo.getSelectedItem();
        if (currentPreset instanceof Preselect) {
            switch ((Preselect) currentPreset) {
            case SELECTION:
                /* The user edits the selection itself -> remember it. */
                rememberedSelection = new LinkedHashSet<CAPTCHA_TYPE>(selectedTypes);
                break;
            case USED:
            case ALL:
            default:
                if (!selectedTypes.equals(getPresetTypes((Preselect) currentPreset))) {
                    /* Jumping to "Selection" always overwrites the remembered selection, whether there was one before or not. */
                    rememberedSelection = new LinkedHashSet<CAPTCHA_TYPE>(selectedTypes);
                    adjusting = true;
                    try {
                        presetCombo.setSelectedItem(Preselect.SELECTION);
                    } finally {
                        adjusting = false;
                    }
                }
                break;
            }
        }
        onSelectionChanged();
    }

    /** Reflects {@link #selectedTypes} in the multi-select control without triggering its own change handling (see {@link #adjusting}). */
    private void syncTypesCombo() {
        adjusting = true;
        try {
            typesCombo.setSelectedItems(new ArrayList<CAPTCHA_TYPE>(selectedTypes));
        } finally {
            adjusting = false;
        }
    }

    private void onSelectionChanged() {
        /* "Used by you": list solvers first that support the most (ideally all) of the captcha types the user needs. */
        model.setVisibleTypes(selectedTypes, presetCombo.getSelectedItem() == Preselect.USED);
        updateResizeMode();
    }

    /**
     * Few columns: stretch them over the full table width. So many columns that they do not fit with their default width: keep the
     * default width and scroll horizontally.
     */
    private void updateResizeMode() {
        int neededWidth = model.getSolverColumnWidth();
        for (final CAPTCHA_TYPE type : selectedTypes) {
            neededWidth += SolverComparisonTableModel.getTypeColumnDefaultWidth(type);
        }
        final int available = scrollPane.getViewport().getWidth();
        final int mode = neededWidth <= available ? JTable.AUTO_RESIZE_ALL_COLUMNS : JTable.AUTO_RESIZE_OFF;
        if (table.getAutoResizeMode() != mode) {
            table.setAutoResizeMode(mode);
        }
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
