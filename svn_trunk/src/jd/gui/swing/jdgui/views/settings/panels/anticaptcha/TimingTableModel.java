package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Font;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.Icon;

import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtSpinnerColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.uio.UIOManager;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.ChallengeSolver.SolverType;
import org.jdownloader.captcha.v2.JobRunnable;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;

import jd.gui.swing.jdgui.views.settings.panels.anticaptcha.TimingTableModel.TimingRow;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * Simulation of how a captcha runs through the solvers: The rows are the enabled and ready solvers in the order of the solver table, each
 * followed by a row with the maximum time this solver works on the captcha without a solution before the next solver is started in
 * parallel (the last solver has no such row). <br>
 * If a captcha type is set (see {@link #setCaptchaType(CAPTCHA_TYPE)}), only the solvers which support it are shown and the seconds apply
 * to this captcha type only, otherwise to the solver as a whole (all captcha types). <br>
 * The table can neither be sorted nor can the order be changed, see {@link #move(List, int)}. Only the seconds can be edited. <br>
 * The edited seconds are kept in this model only (a draft). They are written to the config of the solvers by {@link #save()}.
 */
public class TimingTableModel extends ExtTableModel<TimingRow> {
    /** Informed whenever the user changes the seconds (by editing, resetting or a preselection). */
    public interface TimingChangeListener {
        void onTimingChanged();
    }

    private enum RowType {
        SOLVER,
        WAIT,
        FINAL_INFO
    }

    /** Either a solver, the wait time after a solver, or the final info row after the last solver. */
    public static final class TimingRow {
        private final SolverService service;
        private final SolverService nextService;
        private final RowType       type;

        private TimingRow(final SolverService service, final SolverService nextService, final RowType type) {
            this.service = service;
            this.nextService = nextService;
            this.type = type;
        }

        /** True for the row of a solver. */
        public boolean isSolverRow() {
            return type == RowType.SOLVER;
        }

        /** True for the info row after the last solver: It says what happens if no solver solved the captcha. */
        public boolean isFinalRow() {
            return type == RowType.FINAL_INFO;
        }

        public SolverService getService() {
            return service;
        }

        /** The solver which is started when the wait time of a wait row has passed, null for solver rows. */
        public SolverService getNextService() {
            return nextService;
        }

        /** True for the row with the wait time after the solver, false for the row of the solver. */
        public boolean isWaitRow() {
            return type == RowType.WAIT;
        }
    }

    /** The values of a solver as they are edited in the dialog, a copy of the config until {@link TimingTableModel#save()}. */
    private static final class Draft {
        /* The value of the solver (all captcha types). */
        private int                     seconds;
        /* The values for single captcha types, key: name of the captcha type. */
        private final Map<String, Integer> byType;
        private boolean                 dirty;

        private Draft(final int seconds, final Map<String, Integer> byType) {
            this.seconds = seconds;
            this.byType = byType == null ? new HashMap<String, Integer>() : new HashMap<String, Integer>(byType);
        }

        private static boolean isValid(final int seconds) {
            return seconds >= 0 && seconds <= SolverType.DEFAULT_START_NEXT_AFTER_SECONDS_MAX;
        }
    }

    /* null = all captcha types. Assigned only after the super constructor, which already builds the columns. */
    private volatile CAPTCHA_TYPE                captchaType;
    private volatile int                         solverCount;
    private final Map<SolverService, Draft>      drafts = new HashMap<SolverService, Draft>();
    private volatile TimingChangeListener        changeListener;

    public TimingTableModel() {
        super("TimingTableModel2");
        update();
    }

    public void setChangeListener(final TimingChangeListener changeListener) {
        this.changeListener = changeListener;
    }

    /** The captcha type the seconds apply to, null = the solver as a whole (all captcha types). */
    public CAPTCHA_TYPE getCaptchaType() {
        return captchaType;
    }

    /** Sets the captcha type for the simulation, null = no captcha type (all solvers, the seconds apply to the solver as a whole). */
    public void setCaptchaType(final CAPTCHA_TYPE captchaType) {
        this.captchaType = captchaType;
        update();
    }

    /** The number of solvers (not rows) that are shown. */
    public int getSolverCount() {
        return solverCount;
    }

    private Draft getDraft(final SolverService service) {
        Draft draft = drafts.get(service);
        if (draft == null) {
            draft = new Draft(service.getConfigV3().getStartNextSolverAfterSeconds(), service.getConfigV3().getStartNextSolverAfterSecondsByCaptchaType());
            /*
             * Invalid values in the config (e.g. -1 stored by an old version) are repaired right here: The value of the solver is set to its
             * default, invalid values of single captcha types are removed. The repair is stored when the user saves.
             */
            if (!Draft.isValid(draft.seconds)) {
                draft.seconds = JobRunnable.getDefaultStartNextSolverAfterSeconds(service);
                draft.dirty = true;
            }
            final Iterator<Map.Entry<String, Integer>> it = draft.byType.entrySet().iterator();
            while (it.hasNext()) {
                final Integer seconds = it.next().getValue();
                if (seconds == null || !Draft.isValid(seconds.intValue())) {
                    it.remove();
                    draft.dirty = true;
                }
            }
            drafts.put(service, draft);
        }
        return draft;
    }

    /**
     * The seconds after which the given solver starts the next one. The value set for the given captcha type wins. Without such a value (or
     * without a captcha type) the value of the solver is used, and if there is none either, the default of the solver type.
     */
    public int getSeconds(final SolverService service, final CAPTCHA_TYPE type) {
        final Draft draft = getDraft(service);
        if (type != null) {
            final Integer seconds = draft.byType.get(type.name());
            if (seconds != null) {
                return seconds.intValue();
            }
        }
        return draft.seconds;
    }

    /**
     * True if the user set a value: for the given captcha type, or for the solver (all captcha types) if the type is null. For the solver
     * that is a value which is not the default of the solver. False means that the value is inherited: the value of the solver for a
     * captcha type, the default of the solver for the solver.
     */
    private boolean isSet(final SolverService service, final CAPTCHA_TYPE type) {
        final Draft draft = getDraft(service);
        if (type == null) {
            return draft.seconds != JobRunnable.getDefaultStartNextSolverAfterSeconds(service);
        }
        return draft.byType.containsKey(type.name());
    }

    /**
     * True if the value is the default: Either no value is set, or the value set is the same as the one that would be used anyway (the
     * value of the solver for a captcha type, the default of the solver for the solver).
     */
    private boolean isDefault(final SolverService service, final CAPTCHA_TYPE type) {
        if (!isSet(service, type)) {
            return true;
        } else if (type == null) {
            return false;
        }
        return getSeconds(service, type) == getSeconds(service, null);
    }

    /** Sets the seconds in the draft. A value for a captcha type that is the same as the value of the solver is removed instead. */
    private void putSeconds(final SolverService service, final CAPTCHA_TYPE type, final int seconds) {
        final Draft draft = getDraft(service);
        final int value = Math.max(0, Math.min(SolverType.DEFAULT_START_NEXT_AFTER_SECONDS_MAX, seconds));
        if (type == null) {
            draft.seconds = value;
        } else if (value == getSeconds(service, null)) {
            draft.byType.remove(type.name());
        } else {
            draft.byType.put(type.name(), Integer.valueOf(value));
        }
        draft.dirty = true;
    }

    /** Removes the value in the draft: for the captcha type, or for the solver (all captcha types, default again) if the type is null. */
    private void removeSeconds(final SolverService service, final CAPTCHA_TYPE type) {
        final Draft draft = getDraft(service);
        if (type == null) {
            draft.seconds = JobRunnable.getDefaultStartNextSolverAfterSeconds(service);
        } else {
            draft.byType.remove(type.name());
        }
        draft.dirty = true;
    }

    /** Sets the seconds for the current captcha type (or the solver if there is none). This is a change by the user. */
    public void setSeconds(final SolverService service, final int seconds) {
        putSeconds(service, captchaType, seconds);
        fireChanged();
    }

    private void fireChanged() {
        refreshRows();
        final TimingChangeListener listener = changeListener;
        if (listener != null) {
            listener.onTimingChanged();
        }
    }

    /** Writes the edited seconds to the config of the solvers. Until this is called, nothing is changed in the config. */
    public void save() {
        for (final Map.Entry<SolverService, Draft> entry : drafts.entrySet()) {
            final Draft draft = entry.getValue();
            if (draft.dirty) {
                JobRunnable.storeStartNextSolverAfterSeconds(entry.getKey(), draft.seconds, draft.byType);
                draft.dirty = false;
            }
        }
    }

    /** True if the solver can be used in the simulation: enabled, ready and, with a captcha type, supporting this captcha type. */
    private boolean isShown(final SolverService service, final CAPTCHA_TYPE type) {
        if (!service.isUsable()) {
            return false;
        } else if (type == null) {
            return true;
        }
        final Set<CAPTCHA_TYPE> supported = service.getSupportedCaptchaTypes();
        final Set<CAPTCHA_TYPE> disabled = service.getConfigV3().getDisabledCaptchaTypes();
        return supported != null && supported.contains(type) && (disabled == null || !disabled.contains(type));
    }

    public void update() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                final CAPTCHA_TYPE type = captchaType;
                final List<SolverService> solvers = new ArrayList<SolverService>();
                for (final SolverService service : ChallengeResponseController.getInstance().getOrderedServices()) {
                    if (isShown(service, type)) {
                        solvers.add(service);
                    }
                }
                final List<TimingRow> rows = new ArrayList<TimingRow>();
                for (int i = 0; i < solvers.size(); i++) {
                    rows.add(new TimingRow(solvers.get(i), null, RowType.SOLVER));
                    if (i < solvers.size() - 1) {
                        rows.add(new TimingRow(solvers.get(i), solvers.get(i + 1), RowType.WAIT));
                    }
                }
                if (!solvers.isEmpty()) {
                    /* What happens if even the last solver has no solution. */
                    rows.add(new TimingRow(solvers.get(solvers.size() - 1), null, RowType.FINAL_INFO));
                }
                solverCount = solvers.size();
                _fireTableStructureChanged(rows, true);
            }
        };
    }

    /** Redraws the rows (e.g. after a value was changed) without changing them. */
    private void refreshRows() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                if (getRowCount() > 0) {
                    fireTableRowsUpdated(0, getRowCount() - 1);
                }
            }
        };
    }

    /**
     * Drag and drop of rows is allowed visually, but it has no effect: The order of the solvers can only be changed in the solver order
     * table.
     */
    @Override
    public boolean move(final List<TimingRow> transferData, final int dropRow) {
        UIOManager.I().showMessageDialog(_GUI.T.TimingTable_dragDrop_hint());
        return false;
    }

    /**
     * Resets the seconds of the solvers of the given rows: For a selected captcha type the value set for this captcha type is removed (the
     * solver's value applies again), without a captcha type the solver is set to its default again.
     */
    public void reset(final List<TimingRow> rows) {
        final Set<SolverService> services = new LinkedHashSet<SolverService>();
        for (final TimingRow row : rows) {
            services.add(row.getService());
        }
        final CAPTCHA_TYPE type = captchaType;
        for (final SolverService service : services) {
            removeSeconds(service, type);
        }
        fireChanged();
    }

    /** Resets the seconds of all shown solvers, see {@link #reset(List)}. */
    public void resetAll() {
        reset(getTableData());
    }

    /**
     * Sets the value of the solver (all captcha types) of all shown solvers back to the default of the solver. The values of single captcha
     * types stay as they are.
     */
    public void resetAllSolverValues() {
        for (final SolverService service : getShownServices()) {
            removeSeconds(service, null);
        }
        fireChanged();
    }

    /** Removes everything the user set for all shown solvers: the value of the solver and the values of all of its captcha types. */
    public void resetAllValues() {
        for (final SolverService service : getShownServices()) {
            removeSeconds(service, null);
            getDraft(service).byType.clear();
        }
        fireChanged();
    }

    /** True if at least one shown solver has a value of the solver (all captcha types) which is not its default. */
    public boolean isAnySolverValueSet() {
        for (final SolverService service : getShownServices()) {
            if (isSet(service, null)) {
                return true;
            }
        }
        return false;
    }

    /**
     * True if at least one shown solver has a value which is not the default: the value of the solver, or the value of any of its captcha
     * types (not only of the selected one).
     */
    public boolean isAnyValueSet() {
        for (final SolverService service : getShownServices()) {
            if (isSet(service, null)) {
                return true;
            }
            if (!getDraft(service).byType.isEmpty()) {
                return true;
            }
        }
        return false;
    }

    /** The solvers of the table, each one once. */
    private Set<SolverService> getShownServices() {
        final Set<SolverService> services = new LinkedHashSet<SolverService>();
        for (final TimingRow row : getTableData()) {
            services.add(row.getService());
        }
        return services;
    }

    /** Sets the seconds of all shown wait rows to 0: All solvers are started in parallel. */
    public void setAllImmediately() {
        final CAPTCHA_TYPE type = captchaType;
        for (final TimingRow row : getTableData()) {
            if (row.isWaitRow()) {
                putSeconds(row.getService(), type, 0);
            }
        }
        fireChanged();
    }

    /**
     * True if all shown solvers have their default seconds for the current scope (no value, or the same value as the default): Then there
     * is nothing to reset.
     */
    public boolean isAllDefault() {
        final CAPTCHA_TYPE type = captchaType;
        for (final TimingRow row : getTableData()) {
            if (!isDefault(row.getService(), type)) {
                return false;
            }
        }
        return true;
    }

    /**
     * True if at least one solver of the given rows has a value which is not the default for the current scope (the solver, or the selected
     * captcha type): Only then there is something to reset.
     */
    public boolean isAnyNotDefault(final List<TimingRow> rows) {
        final CAPTCHA_TYPE type = captchaType;
        for (final TimingRow row : rows) {
            if (!isDefault(row.getService(), type)) {
                return true;
            }
        }
        return false;
    }

    /** True if there is at least one wait row and all of them are 0 seconds: All solvers are started in parallel. */
    public boolean isAllImmediately() {
        boolean any = false;
        for (final TimingRow row : getTableData()) {
            if (row.isWaitRow()) {
                any = true;
                if (getSeconds(row.getService(), captchaType) != 0) {
                    return false;
                }
            }
        }
        return any;
    }

    /** True if the seconds of the row are not set by the user for the current scope (captcha type or solver), but inherited. */
    private boolean isInherited(final TimingRow row) {
        return !isSet(row.getService(), captchaType);
    }

    @Override
    protected void initColumns() {
        addColumn(new ExtTextColumn<TimingRow>(_GUI.T.SolverOrderTableModel_initColumns_service()) {
            /* The service column is the only one which can not be hidden. */
            @Override
            public boolean isHidable() {
                return false;
            }

            @Override
            public boolean isSortable(final TimingRow obj) {
                return false;
            }

            @Override
            protected Icon getIcon(final TimingRow value) {
                if (value.isWaitRow()) {
                    return new AbstractIcon(IconKey.ICON_WAIT, 16);
                } else if (value.isFinalRow()) {
                    return new AbstractIcon(IconKey.ICON_WARNING, 16);
                }
                return value.getService().getIcon(18);
            }

            @Override
            public int getDefaultWidth() {
                return 260;
            }

            @Override
            public boolean isEnabled(final TimingRow obj) {
                return true;
            }

            @Override
            public String getStringValue(final TimingRow value) {
                if (value.isWaitRow()) {
                    return _GUI.T.TimingTableModel_waitRow(value.getService().getName(), value.getNextService().getName());
                } else if (value.isFinalRow()) {
                    return _GUI.T.TimingTableModel_finalRow(value.getService().getName());
                }
                return value.getService().getName();
            }
        });
        addColumn(new ExtTextColumn<TimingRow>(_GUI.T.SolverOrderTableModel_initColumns_type_()) {
            @Override
            public boolean isSortable(final TimingRow obj) {
                return false;
            }

            @Override
            public boolean isEnabled(final TimingRow obj) {
                return true;
            }

            @Override
            public int getDefaultWidth() {
                return 80;
            }

            @Override
            public String getStringValue(final TimingRow value) {
                return value.isSolverRow() ? value.getService().getType().getLabel() : "";
            }
        });
        addColumn(new ExtSpinnerColumn<TimingRow>(_GUI.T.TimingTableModel_column_seconds()) {
            {
                /*
                 * The spinner of the column: no negative values (neither with the arrow buttons nor typed in), steps of 5 seconds. The
                 * limits are read from this model by the text field of the spinner, too.
                 */
                getIntModel().setMinimum(Integer.valueOf(0));
                getIntModel().setMaximum(Integer.valueOf(SolverType.DEFAULT_START_NEXT_AFTER_SECONDS_MAX));
                getIntModel().setStepSize(Integer.valueOf(5));
            }

            @Override
            public boolean isSortable(final TimingRow obj) {
                return false;
            }

            @Override
            public boolean isHidable() {
                return false;
            }

            /* The renderer is reused for all rows, so color and font have to be set (or reset) for every row. */
            private final Font defaultFont = rendererField.getFont();
            private final Font changedFont = defaultFont.deriveFont(Font.BOLD);

            /* Values which are not the default (set by the user and different from the inherited value) are shown bold. */
            @Override
            public void configureRendererComponent(final TimingRow value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                super.configureRendererComponent(value, isSelected, hasFocus, row, column);
                final boolean changed = value.isWaitRow() && !isDefault(value.getService(), captchaType);
                rendererField.setFont(changed ? changedFont : defaultFont);
            }

            /* Only the seconds of the wait rows can be edited. */
            @Override
            public boolean isEditable(final TimingRow obj) {
                return obj.isWaitRow();
            }

            @Override
            protected boolean isEditable(final TimingRow obj, final boolean enabled) {
                return obj.isWaitRow();
            }

            @Override
            protected String getTooltipText(final TimingRow obj) {
                if (!obj.isWaitRow()) {
                    return null;
                }
                final CAPTCHA_TYPE type = captchaType;
                if (type == null) {
                    return _GUI.T.TimingTableModel_tooltip_solver();
                }
                return _GUI.T.TimingTableModel_tooltip_captchaType(type.getDisplayName());
            }

            @Override
            public boolean isEnabled(final TimingRow obj) {
                return true;
            }

            @Override
            public int getDefaultWidth() {
                return 180;
            }

            @Override
            protected Number getNumber(final TimingRow value) {
                if (!value.isWaitRow()) {
                    return Integer.valueOf(0);
                }
                return Integer.valueOf(getSeconds(value.getService(), captchaType));
            }

            @Override
            protected void setNumberValue(final Number value, final TimingRow object) {
                setSeconds(object.getService(), value.intValue());
            }

            @Override
            public String getStringValue(final TimingRow value) {
                if (!value.isWaitRow()) {
                    return "";
                }
                final int amount = getNumber(value).intValue();
                /* 0 seconds: the next solver is started immediately. */
                final String text = amount == 0 ? _GUI.T.TimingTableModel_immediately() : _GUI.T.TimingTableModel_seconds(String.valueOf(amount));
                final CAPTCHA_TYPE type = captchaType;
                if (!isInherited(value)) {
                    /* With a captcha type, the value only applies when this solver solves this captcha type, otherwise for any captcha type. */
                    if (type == null) {
                        return _GUI.T.TimingTableModel_secondsAnyCaptchaType(text, value.getService().getName());
                    }
                    return _GUI.T.TimingTableModel_secondsCaptchaType(text, value.getService().getName(), type.getDisplayName());
                } else if (type == null) {
                    return _GUI.T.TimingTableModel_secondsSolverDefault(text);
                } else {
                    return _GUI.T.TimingTableModel_secondsInherited(text);
                }
            }
        });
    }
}
