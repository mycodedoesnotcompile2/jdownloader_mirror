package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.Icon;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtIconColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;

import jd.plugins.CaptchaType;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * Table model of the "External Solver comparison Table": one row per external captcha solver and one column per captcha type. Each cell
 * shows whether the solver supports the captcha type (check mark) or not (red X). The columns are always created for every captcha type
 * JDownloader can process (see {@link CaptchaType#getProcessableCaptchaTypes()}); which of them are actually shown is controlled via
 * {@link #setVisibleTypes(Set)}.
 */
public class SolverComparisonTableModel extends ExtTableModel<SolverService> {
    /** Default widths of the solver column and of each captcha type column. */
    public static final int            SOLVER_COLUMN_WIDTH = 170;
    public static final int            TYPE_COLUMN_WIDTH   = 135;
    /* null = all columns visible. Only read after the constructor has finished (columns are created inside the super constructor). */
    private volatile Set<CAPTCHA_TYPE> visibleTypes         = null;
    /* All external solvers, before filtering. */
    private volatile List<SolverService> allSolvers          = new ArrayList<SolverService>();
    /* True: only show solvers that support every selected (visible) captcha type. */
    private volatile boolean             onlyFullSupport     = false;
    /* True: sort by the number of supported types among the selected ones instead of the total number of supported types. */
    private volatile boolean             sortBySelectedTypes = false;

    public SolverComparisonTableModel() {
        super("SolverComparisonTableModel");
    }

    /** Returns the tooltip shared by all solver tables (services table and comparison table) for the solver name cell. */
    public static String getSolverTooltip(final SolverService solver) {
        final String supportedCount = String.valueOf(getSupportedCount(solver));
        if (solver.getBuyURL() == null) {
            return _GUI.T.CaptchaSolverComparison_solver_tooltip_noBuyPage(supportedCount);
        }
        return _GUI.T.CaptchaSolverComparison_solver_tooltip(supportedCount);
    }

    /** Opens the page where an account for the given solver can be bought. Returns true if a page was opened. */
    public static boolean openBuyPage(final SolverService solver) {
        final String buyURL = solver.getBuyURL();
        if (buyURL == null || !CrossSystem.isOpenBrowserSupported()) {
            return false;
        }
        CrossSystem.openURL(buyURL);
        return true;
    }

    /** True if the table is empty only because "only services supporting all selected types" filtered out every solver. */
    public boolean isEmptyDueToFilter() {
        return onlyFullSupport && !allSolvers.isEmpty() && getRowCount() == 0;
    }

    /** Number of selected (visible) captcha types the solver supports. */
    private int getSupportedSelectedCount(final SolverService solver) {
        final Set<CAPTCHA_TYPE> visible = visibleTypes;
        if (visible == null) {
            return getSupportedCount(solver);
        }
        int count = 0;
        for (final CAPTCHA_TYPE type : visible) {
            if (isSupported(solver, type)) {
                count++;
            }
        }
        return count;
    }

    private boolean supportsAllSelected(final SolverService solver) {
        final Set<CAPTCHA_TYPE> visible = visibleTypes;
        final int selected = visible == null ? CaptchaType.getProcessableCaptchaTypes().size() : visible.size();
        return getSupportedSelectedCount(solver) == selected;
    }

    /** Returns the number of captcha types the solver supports. Used as default sort criterion. */
    private static int getSupportedCount(final SolverService solver) {
        final List<CAPTCHA_TYPE> supportedTypes = solver.getSupportedCaptchaTypes();
        return supportedTypes == null ? 0 : supportedTypes.size();
    }

    private static boolean isSupported(final SolverService solver, final CAPTCHA_TYPE type) {
        final List<CAPTCHA_TYPE> supportedTypes = solver.getSupportedCaptchaTypes();
        return supportedTypes != null && supportedTypes.contains(type);
    }

    /**
     * Replaces the rows. Default order: the solver supporting the most captcha types first. If the user selected a sort column, that sort
     * is applied on top.
     */
    public void setSolvers(final List<SolverService> solvers) {
        this.allSolvers = new ArrayList<SolverService>(solvers);
        rebuildRows();
    }

    /** Filters (optional) and sorts the solvers and replaces the rows. */
    private void rebuildRows() {
        final boolean fullSupportOnly = onlyFullSupport;
        final boolean bySelected = sortBySelectedTypes;
        final List<SolverService> rows = new ArrayList<SolverService>();
        for (final SolverService solver : allSolvers) {
            if (!fullSupportOnly || supportsAllSelected(solver)) {
                rows.add(solver);
            }
        }
        Collections.sort(rows, new Comparator<SolverService>() {
            @Override
            public int compare(final SolverService a, final SolverService b) {
                if (bySelected) {
                    final int selectedDiff = getSupportedSelectedCount(b) - getSupportedSelectedCount(a);
                    if (selectedDiff != 0) {
                        return selectedDiff;
                    }
                }
                final int diff = getSupportedCount(b) - getSupportedCount(a);
                if (diff != 0) {
                    return diff;
                }
                return a.getName().compareToIgnoreCase(b.getName());
            }
        });
        _fireTableStructureChanged(rows, true);
    }

    /** Enables/disables hiding of all solvers that do not support every selected captcha type. */
    public void setOnlyFullSupport(final boolean onlyFullSupport) {
        this.onlyFullSupport = onlyFullSupport;
        rebuildRows();
    }

    /**
     * Selects the captcha types whose columns are shown.
     *
     * @param sortBySelectedTypes
     *            true: solvers supporting the most of the selected types (ideally all) are listed first
     */
    public void setVisibleTypes(final Set<CAPTCHA_TYPE> types, final boolean sortBySelectedTypes) {
        this.visibleTypes = new HashSet<CAPTCHA_TYPE>(types);
        this.sortBySelectedTypes = sortBySelectedTypes;
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                /* The selection influences filtering and sorting, so the rows need to be rebuilt as well. */
                rebuildRows();
                if (getTable() != null) {
                    getTable().updateColumns();
                }
            }
        };
    }

    @Override
    protected void initColumns() {
        addColumn(new ExtTextColumn<SolverService>(_GUI.T.CaptchaSolverComparison_column_solver()) {
            @Override
            protected Icon getIcon(final SolverService value) {
                return value.getIcon(18);
            }

            @Override
            public String getStringValue(final SolverService value) {
                return value.getName();
            }

            @Override
            protected String getTooltipText(final SolverService value) {
                return getSolverTooltip(value);
            }

            /** Double click opens the page where an account for this solver can be bought. */
            @Override
            public boolean onDoubleClick(final MouseEvent e, final SolverService value) {
                return openBuyPage(value);
            }

            @Override
            public int getDefaultWidth() {
                return SOLVER_COLUMN_WIDTH;
            }

            @Override
            public boolean isHidable() {
                return false;
            }
        });
        for (final CAPTCHA_TYPE type : CaptchaType.getProcessableCaptchaTypes()) {
            addColumn(new TypeColumn(type));
        }
    }

    /** Column showing whether the solver supports one specific captcha type. */
    private class TypeColumn extends ExtIconColumn<SolverService> {
        private static final long serialVersionUID = 1L;
        private final CAPTCHA_TYPE type;

        public TypeColumn(final CAPTCHA_TYPE type) {
            super(type.getDisplayName());
            this.type = type;
            /* Supported solvers first when sorting ascending, unsupported first when sorting descending. */
            setRowSorter(new ExtDefaultRowSorter<SolverService>() {
                @Override
                public int compare(final SolverService o1, final SolverService o2) {
                    final int s1 = isSupported(o1, TypeColumn.this.type) ? 1 : 0;
                    final int s2 = isSupported(o2, TypeColumn.this.type) ? 1 : 0;
                    if (this.getSortOrderIdentifier() == ExtColumn.SORT_ASC) {
                        return s2 - s1;
                    } else {
                        return s1 - s2;
                    }
                }
            });
        }

        /*
         * All type columns are instances of the same class, so the default id (derived from the class) would be identical for all of them.
         * The id is needed to store/restore width, order and sort state per column. It is generated inside the super constructor, i.e. before
         * the type field is set, so the (unique) column name is used.
         */
        @Override
        protected String generateID() {
            return "SolverComparisonTable.type." + getName();
        }

        @Override
        protected Icon getIcon(final SolverService value) {
            if (isSupported(value, type)) {
                return NewTheme.I().getIcon(IconKey.ICON_TRUE, 16);
            } else {
                return NewTheme.I().getIcon(IconKey.ICON_FALSE, 16);
            }
        }

        @Override
        protected String getTooltipText(final SolverService obj) {
            final String state = isSupported(obj, type) ? _GUI.T.CaptchaSolverComparison_supported() : _GUI.T.CaptchaSolverComparison_notSupported();
            return obj.getName() + " - " + type.getDisplayName() + ": " + state;
        }

        @Override
        public boolean isVisible(final boolean savedValue) {
            final Set<CAPTCHA_TYPE> visible = visibleTypes;
            return visible == null || visible.contains(type);
        }

        /*
         * Must stay hidable (default): ExtTable only removes invisible columns from the table if they are hidable. The visibility itself is
         * controlled by the selection above the table only (see isVisible), the table's own column menu is disabled.
         */
        @Override
        public int getDefaultWidth() {
            return TYPE_COLUMN_WIDTH;
        }

        @Override
        public int getMinWidth() {
            return 90;
        }

        /* No tight limit: with few selected types the columns are stretched over the full table width. */
        @Override
        public int getMaxWidth() {
            return 2000;
        }
    }
}
