package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Set;

import javax.swing.Icon;

import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * Table of the {@link CaptchaWizardDialog} (captcha types step): lists the captcha types the user needs and tells for each of them
 * whether JDownloader can solve it with any method, automatically or manually. The rows are few, so the table is always displayed
 * completely (no scrollbar). Which of the three yes/no columns is shown by default depends on the solving method the user chose in step 1,
 * the user can show and hide all of them.
 */
public class CaptchaWizardTypeTable extends BasicJDTable<CAPTCHA_TYPE> {
    private static final long serialVersionUID = 1L;

    /** The yes/no column which is shown by default. */
    public enum DefaultColumn {
        /** Solvable with any method. */
        ANY,
        /** Solvable automatically (paid solvers). */
        AUTOMATIC,
        /** Solvable manually (the user solves it). */
        MANUAL
    }

    public CaptchaWizardTypeTable() {
        super(new Model());
        setSearchEnabled(true);
        setFocusable(true);
    }

    @Override
    protected boolean isDisplayFullsizeAlways() {
        return true;
    }

    /**
     * Sets the captcha types to show and the types which can be solved automatically / manually by the solvers of JDownloader (usable or
     * not, this table is about what is possible at all).
     */
    public void setTypes(final Set<CAPTCHA_TYPE> types, final Set<CAPTCHA_TYPE> automaticTypes, final Set<CAPTCHA_TYPE> manualTypes, final DefaultColumn defaultColumn) {
        ((Model) getModel()).setTypes(types, automaticTypes, manualTypes, defaultColumn);
    }

    private static class Model extends ExtTableModel<CAPTCHA_TYPE> {
        private static final long          serialVersionUID = 1L;
        private volatile Set<CAPTCHA_TYPE> automaticTypes   = Collections.emptySet();
        private volatile Set<CAPTCHA_TYPE> manualTypes      = Collections.emptySet();
        private volatile DefaultColumn     defaultColumn    = DefaultColumn.ANY;

        private Model() {
            super("CaptchaWizardTypeTable");
        }

        private void setTypes(final Set<CAPTCHA_TYPE> types, final Set<CAPTCHA_TYPE> automaticTypes, final Set<CAPTCHA_TYPE> manualTypes, final DefaultColumn defaultColumn) {
            this.automaticTypes = automaticTypes;
            this.manualTypes = manualTypes;
            this.defaultColumn = defaultColumn;
            _fireTableStructureChanged(new ArrayList<CAPTCHA_TYPE>(types), true);
            if (getTable() != null) {
                /* The default visibility of the yes/no columns depends on defaultColumn. */
                getTable().updateColumns();
            }
        }

        private boolean isSolvableAutomatically(final CAPTCHA_TYPE type) {
            return automaticTypes.contains(type);
        }

        private boolean isSolvableManually(final CAPTCHA_TYPE type) {
            return manualTypes.contains(type);
        }

        @Override
        protected void initColumns() {
            addColumn(new ExtTextColumn<CAPTCHA_TYPE>(_GUI.T.CaptchaWizard_column_captchaType()) {
                private static final long serialVersionUID = 1L;

                @Override
                public String getStringValue(final CAPTCHA_TYPE value) {
                    return value.getDisplayName();
                }

                @Override
                protected Icon getIcon(final CAPTCHA_TYPE value) {
                    return value.getIcon();
                }

                @Override
                public boolean isHidable() {
                    return false;
                }
            });
            addColumn(new YesNoColumn(_GUI.T.CaptchaWizard_column_solvableAny()) {
                private static final long serialVersionUID = 1L;

                @Override
                protected boolean isYes(final CAPTCHA_TYPE value) {
                    return isSolvableAutomatically(value) || isSolvableManually(value);
                }

                @Override
                protected String getTooltipText(final CAPTCHA_TYPE value) {
                    /* Tells how it can be solved: by both methods, or by only one of them. */
                    final boolean automatic = isSolvableAutomatically(value);
                    final boolean manual = isSolvableManually(value);
                    if (automatic && manual) {
                        return _GUI.T.CaptchaWizard_tooltip_solvableBoth();
                    } else if (automatic) {
                        return _GUI.T.CaptchaWizard_tooltip_solvableAutomaticOnly();
                    } else if (manual) {
                        return _GUI.T.CaptchaWizard_tooltip_solvableManualOnly();
                    } else {
                        return _GUI.T.CaptchaWizard_tooltip_solvableNone();
                    }
                }

                @Override
                public boolean isDefaultVisible() {
                    return isDefault(DefaultColumn.ANY);
                }
            });
            addColumn(new YesNoColumn(_GUI.T.CaptchaWizard_column_solvableAutomatically()) {
                private static final long serialVersionUID = 1L;

                @Override
                protected boolean isYes(final CAPTCHA_TYPE value) {
                    return isSolvableAutomatically(value);
                }

                @Override
                protected String getTooltipText(final CAPTCHA_TYPE value) {
                    return isSolvableAutomatically(value) ? _GUI.T.CaptchaWizard_tooltip_automaticYes() : _GUI.T.CaptchaWizard_tooltip_automaticNo();
                }

                @Override
                public boolean isDefaultVisible() {
                    return isDefault(DefaultColumn.AUTOMATIC);
                }
            });
            addColumn(new YesNoColumn(_GUI.T.CaptchaWizard_column_solvableManually()) {
                private static final long serialVersionUID = 1L;

                @Override
                protected boolean isYes(final CAPTCHA_TYPE value) {
                    return isSolvableManually(value);
                }

                @Override
                protected String getTooltipText(final CAPTCHA_TYPE value) {
                    return isSolvableManually(value) ? _GUI.T.CaptchaWizard_tooltip_manualYes() : _GUI.T.CaptchaWizard_tooltip_manualNo();
                }

                @Override
                public boolean isDefaultVisible() {
                    return isDefault(DefaultColumn.MANUAL);
                }
            });
        }

        private boolean isDefault(final DefaultColumn column) {
            switch (defaultColumn) {
            case AUTOMATIC:
                return column == DefaultColumn.AUTOMATIC;
            case MANUAL:
                return column == DefaultColumn.MANUAL;
            case ANY:
            default:
                return column == DefaultColumn.ANY;
            }
        }
    }

    /** A sortable column which shows "Yes" with a green check mark or "No" with a red X. */
    private abstract static class YesNoColumn extends ExtTextColumn<CAPTCHA_TYPE> {
        private static final long serialVersionUID = 1L;

        private YesNoColumn(final String name) {
            super(name);
        }

        protected abstract boolean isYes(CAPTCHA_TYPE value);

        @Override
        public String getStringValue(final CAPTCHA_TYPE value) {
            /* Sorted by this text: "No" comes before "Yes". */
            return isYes(value) ? _GUI.T.lit_yes() : _GUI.T.lit_no();
        }

        @Override
        protected Icon getIcon(final CAPTCHA_TYPE value) {
            return NewTheme.I().getIcon(isYes(value) ? IconKey.ICON_TRUE : IconKey.ICON_FALSE, 16);
        }
    }
}
