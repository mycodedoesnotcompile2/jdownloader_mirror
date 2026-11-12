package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.Icon;

import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtIconColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * Solver table of the {@link CaptchaWizardDialog}: lists the solvers which can solve at least one of the captcha types the user needs
 * and tells whether a solver solves all of them. The rows are few, so the table is always displayed completely (no scrollbar).
 */
public class CaptchaWizardSolverTable extends BasicJDTable<SolverService> {
    private static final long serialVersionUID = 1L;

    public CaptchaWizardSolverTable() {
        super(new Model());
        setSearchEnabled(true);
        setFocusable(true);
    }

    @Override
    protected boolean isDisplayFullsizeAlways() {
        return true;
    }

    /**
     * Sets the solvers to show and the captcha types the user needs. Solvers which solve most of the needed types come first. Solvers
     * which are already usable are not relevant for the comparison table by default (the user can tick them).
     * <p>
     * Rows which are not relevant (not ticked) are NOT greyed out in this table, on purpose: Not ticked does not mean disabled, it is only
     * a recommendation, and the solvers must stay easy to read.
     *
     * @param deselectAll
     *            true: no solver is relevant by default (the user already has usable solvers for all needed captcha types)
     * @return the number of already usable solvers in the table, they are deselected
     */
    public int setSolvers(final Collection<SolverService> solvers, final Set<CAPTCHA_TYPE> requiredTypes, final boolean deselectAll) {
        return ((Model) getModel()).setSolvers(solvers, requiredTypes, deselectAll);
    }

    /** The solvers whose check box is ticked, i.e. the ones to mark in the comparison table. */
    public List<SolverService> getRelevantSolvers() {
        final Model model = (Model) getModel();
        final List<SolverService> ret = new ArrayList<SolverService>();
        for (final SolverService solver : model.getTableData()) {
            if (model.relevant.contains(solver)) {
                ret.add(solver);
            }
        }
        return ret;
    }

    /** Number of the needed captcha types the solver can solve. */
    static int getSolvedCount(final SolverService solver, final Set<CAPTCHA_TYPE> requiredTypes) {
        final Set<CAPTCHA_TYPE> supported = solver.getSupportedCaptchaTypes();
        int count = 0;
        if (supported != null) {
            for (final CAPTCHA_TYPE type : requiredTypes) {
                if (supported.contains(type)) {
                    count++;
                }
            }
        }
        return count;
    }

    private static class Model extends ExtTableModel<SolverService> {
        private static final long   serialVersionUID = 1L;
        private volatile Set<CAPTCHA_TYPE> requiredTypes = Collections.emptySet();

        private Model() {
            super("CaptchaWizardSolverTable");
        }

        /* The solvers which are relevant for the comparison table (ticked). */
        private final Set<SolverService> relevant = new HashSet<SolverService>();

        /* True if at least one solver in the table is already usable: Then the "Already usable" column is shown by default. */
        private volatile boolean anyUsable = false;

        private int setSolvers(final Collection<SolverService> solvers, final Set<CAPTCHA_TYPE> requiredTypes, final boolean deselectAll) {
            this.requiredTypes = requiredTypes;
            relevant.clear();
            int usableCount = 0;
            for (final SolverService solver : solvers) {
                if (solver.isUsable()) {
                    usableCount++;
                } else if (!deselectAll) {
                    relevant.add(solver);
                }
            }
            anyUsable = usableCount > 0;
            final int deselected = usableCount;
            final List<SolverService> rows = new ArrayList<SolverService>(solvers);
            Collections.sort(rows, new Comparator<SolverService>() {
                @Override
                public int compare(final SolverService a, final SolverService b) {
                    final int diff = getSolvedCount(b, requiredTypes) - getSolvedCount(a, requiredTypes);
                    if (diff != 0) {
                        return diff;
                    }
                    return a.getName().compareToIgnoreCase(b.getName());
                }
            });
            _fireTableStructureChanged(rows, true);
            if (getTable() != null) {
                /* The default visibility of the "Already usable" column depends on anyUsable. */
                getTable().updateColumns();
            }
            return deselected;
        }

        @Override
        protected void initColumns() {
            addColumn(new ExtCheckColumn<SolverService>(_GUI.T.CaptchaWizard_column_relevant()) {
                private static final long serialVersionUID = 1L;

                @Override
                public boolean isEditable(final SolverService obj) {
                    return true;
                }

                @Override
                protected boolean getBooleanValue(final SolverService value) {
                    return relevant.contains(value);
                }

                @Override
                protected void setBooleanValue(final boolean value, final SolverService object) {
                    if (value) {
                        relevant.add(object);
                    } else {
                        relevant.remove(object);
                    }
                }

                @Override
                public boolean isHidable() {
                    return false;
                }
            });
            addColumn(new ExtTextColumn<SolverService>(_GUI.T.CaptchaWizard_column_solver()) {
                private static final long serialVersionUID = 1L;

                @Override
                public String getStringValue(final SolverService value) {
                    return value.getName();
                }

                @Override
                protected Icon getIcon(final SolverService value) {
                    return value.getIcon(18);
                }

                @Override
                protected String getTooltipText(final SolverService value) {
                    return value.getDescription();
                }
            });
            /* Green check mark if the solver is usable right now, red X if not. Shown by default only if there is a usable solver. */
            addColumn(new ExtIconColumn<SolverService>(_GUI.T.CaptchaWizard_column_alreadyUsable()) {
                private static final long serialVersionUID = 1L;

                {
                    /* Usable solvers first when sorting ascending. */
                    setRowSorter(new ExtDefaultRowSorter<SolverService>() {
                        @Override
                        public int compare(final SolverService o1, final SolverService o2) {
                            final int s1 = o1.isUsable() ? 1 : 0;
                            final int s2 = o2.isUsable() ? 1 : 0;
                            if (this.getSortOrderIdentifier() == ExtColumn.SORT_ASC) {
                                return s2 - s1;
                            } else {
                                return s1 - s2;
                            }
                        }
                    });
                }

                @Override
                public boolean isDefaultVisible() {
                    return anyUsable;
                }

                @Override
                protected Icon getIcon(final SolverService value) {
                    return NewTheme.I().getIcon(value.isUsable() ? IconKey.ICON_TRUE : IconKey.ICON_FALSE, 16);
                }
            });
            addColumn(new ExtTextColumn<SolverService>(_GUI.T.CaptchaWizard_column_solvesAll()) {
                private static final long serialVersionUID = 1L;

                private boolean solvesAll(final SolverService value) {
                    return getSolvedCount(value, requiredTypes) == requiredTypes.size();
                }

                @Override
                public String getStringValue(final SolverService value) {
                    final int solved = getSolvedCount(value, requiredTypes);
                    return solvesAll(value) ? _GUI.T.CaptchaWizard_solvesAll_yes(solved, requiredTypes.size()) : _GUI.T.CaptchaWizard_solvesAll_no(solved, requiredTypes.size());
                }

                @Override
                protected Icon getIcon(final SolverService value) {
                    return NewTheme.I().getIcon(solvesAll(value) ? IconKey.ICON_TRUE : IconKey.ICON_FALSE, 16);
                }
            });
        }
    }
}
