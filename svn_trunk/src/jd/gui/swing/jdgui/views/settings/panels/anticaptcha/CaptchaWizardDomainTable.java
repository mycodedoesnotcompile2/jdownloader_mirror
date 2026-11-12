package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import java.awt.Component;

import javax.swing.Icon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtTableHeaderRenderer;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.jdownloader.DomainInfo;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.gui.swing.jdgui.views.settings.components.ToggleEnabledSelectionAction;

/**
 * Domain table of the {@link CaptchaWizardDialog}: one row per (domain, captcha type) with a check box to select it. The rows are sorted by
 * captcha type (then by domain), unless the user chooses another sort column. The table scrolls itself, as the captcha history can contain
 * many domains.
 */
public class CaptchaWizardDomainTable extends BasicJDTable<CaptchaWizardEntry> {
    private static final long serialVersionUID = 1L;

    public CaptchaWizardDomainTable(final Runnable onSelectionChanged) {
        super(new Model(onSelectionChanged));
        setSearchEnabled(true);
        /* Focusable: Without focus the table never receives keyboard shortcuts like Ctrl+A (select all) or Delete. */
        setFocusable(true);
    }

    private Model getWizardModel() {
        return (Model) getModel();
    }

    /** Replaces all rows. */
    public void setEntries(final Collection<CaptchaWizardEntry> entries) {
        getWizardModel().setEntries(entries);
    }

    /** Adds rows (already existing ones are ignored), sorts them in and selects the new rows. */
    public void addEntries(final Collection<CaptchaWizardEntry> entries) {
        final List<CaptchaWizardEntry> added = getWizardModel().addEntries(entries);
        getModel().setSelectedObjects(added);
        if (!added.isEmpty()) {
            final int row = getModel().getRowforObject(added.get(0));
            if (row >= 0) {
                scrollRectToVisible(getCellRect(row, 0, true));
            }
        }
    }

    /** Enables the given rows (which are already in the table), selects them and scrolls to the first one. */
    public void enableAndSelect(final List<CaptchaWizardEntry> entries) {
        for (final CaptchaWizardEntry entry : entries) {
            entry.setEnabled(true);
        }
        getWizardModel().fireChanged();
        getModel().setSelectedObjects(entries);
        final int row = getModel().getRowforObject(entries.get(0));
        if (row >= 0) {
            scrollRectToVisible(getCellRect(row, 0, true));
        }
        repaint();
    }

    public boolean contains(final CaptchaWizardEntry entry) {
        return getModel().getTableData().contains(entry);
    }

    public List<CaptchaWizardEntry> getAllEntries() {
        return new ArrayList<CaptchaWizardEntry>(getModel().getTableData());
    }

    /** The entries whose check box is ticked. */
    public List<CaptchaWizardEntry> getEnabledEntries() {
        final List<CaptchaWizardEntry> ret = new ArrayList<CaptchaWizardEntry>();
        for (final CaptchaWizardEntry entry : getModel().getTableData()) {
            if (entry.isEnabled()) {
                ret.add(entry);
            }
        }
        return ret;
    }

    @Override
    protected JPopupMenu onContextMenu(final JPopupMenu popup, final CaptchaWizardEntry contextObject, final List<CaptchaWizardEntry> selection, final ExtColumn<CaptchaWizardEntry> column, final MouseEvent mouseEvent) {
        if (selection == null || selection.isEmpty()) {
            return null;
        }
        popup.add(new ToggleEnabledSelectionAction<CaptchaWizardEntry>(contextObject, selection) {
            private static final long serialVersionUID = 1L;

            @Override
            protected boolean isEntryEnabled(final CaptchaWizardEntry entry) {
                return entry.isEnabled();
            }

            @Override
            protected void setEntryEnabled(final CaptchaWizardEntry entry, final boolean enabled) {
                entry.setEnabled(enabled);
            }

            @Override
            protected void onFinished() {
                getWizardModel().fireChanged();
                CaptchaWizardDomainTable.this.repaint();
            }
        });
        final JMenuItem remove = new JMenuItem(_GUI.T.literally_remove(), NewTheme.I().getIcon(IconKey.ICON_REMOVE, 18));
        remove.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                getWizardModel().removeEntries(selection);
            }
        });
        popup.add(remove);
        return popup;
    }

    /*
     * Delete removes the selected rows from this list only (nothing else is deleted). They can be added again with "Add", so there is no
     * confirmation.
     */
    @Override
    protected boolean onShortcutDelete(final List<CaptchaWizardEntry> selectedObjects, final KeyEvent evt, final boolean direct) {
        getWizardModel().removeEntries(selectedObjects);
        return true;
    }

    private static class Model extends ExtTableModel<CaptchaWizardEntry> {
        private static final long serialVersionUID = 1L;
        private final Runnable    onChange;

        private Model(final Runnable onChange) {
            super("CaptchaWizardDomainTable");
            this.onChange = onChange;
        }

        private void fireChanged() {
            if (onChange != null) {
                onChange.run();
            }
        }

        /** Sorted by captcha type, then by domain. */
        private static List<CaptchaWizardEntry> sorted(final Collection<CaptchaWizardEntry> entries) {
            final List<CaptchaWizardEntry> ret = new ArrayList<CaptchaWizardEntry>(entries);
            Collections.sort(ret, new Comparator<CaptchaWizardEntry>() {
                @Override
                public int compare(final CaptchaWizardEntry a, final CaptchaWizardEntry b) {
                    final int diff = a.getCaptchaType().ordinal() - b.getCaptchaType().ordinal();
                    if (diff != 0) {
                        return diff;
                    }
                    return a.getDomain().compareToIgnoreCase(b.getDomain());
                }
            });
            return ret;
        }

        private void setEntries(final Collection<CaptchaWizardEntry> entries) {
            _fireTableStructureChanged(sorted(new LinkedHashSet<CaptchaWizardEntry>(entries)), true);
            fireChanged();
        }

        /** @return the entries which were really added, i.e. which were not in the table yet */
        private List<CaptchaWizardEntry> addEntries(final Collection<CaptchaWizardEntry> entries) {
            final Set<CaptchaWizardEntry> all = new LinkedHashSet<CaptchaWizardEntry>(getTableData());
            final List<CaptchaWizardEntry> added = new ArrayList<CaptchaWizardEntry>();
            for (final CaptchaWizardEntry entry : entries) {
                if (all.add(entry)) {
                    added.add(entry);
                }
            }
            _fireTableStructureChanged(sorted(all), true);
            fireChanged();
            return added;
        }

        private void removeEntries(final Collection<CaptchaWizardEntry> entries) {
            final List<CaptchaWizardEntry> rest = new ArrayList<CaptchaWizardEntry>(getTableData());
            rest.removeAll(entries);
            _fireTableStructureChanged(rest, true);
            fireChanged();
        }

        @Override
        protected void initColumns() {
            addColumn(new ExtCheckColumn<CaptchaWizardEntry>(_GUI.T.CaptchaWizard_column_enabled()) {
                private static final long serialVersionUID = 1L;

                /* Same green check mark in the header as the "Enabled" columns of the other tables. */
                @Override
                public ExtTableHeaderRenderer getHeaderRenderer(final JTableHeader jTableHeader) {
                    return new ExtTableHeaderRenderer(this, jTableHeader) {
                        private static final long serialVersionUID = 1L;
                        private final Icon        ok               = NewTheme.I().getIcon(IconKey.ICON_OK, 14);

                        @Override
                        public Component getTableCellRendererComponent(final JTable table, final Object value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                            setIcon(ok);
                            setHorizontalAlignment(CENTER);
                            setText(null);
                            return this;
                        }
                    };
                }

                /* Without this the check boxes cannot be clicked. */
                @Override
                public boolean isEditable(final CaptchaWizardEntry obj) {
                    return true;
                }

                @Override
                public int getMaxWidth() {
                    return 30;
                }

                @Override
                protected boolean getBooleanValue(final CaptchaWizardEntry value) {
                    return value.isEnabled();
                }

                @Override
                protected void setBooleanValue(final boolean value, final CaptchaWizardEntry object) {
                    object.setEnabled(value);
                    fireChanged();
                }
            });
            addColumn(new ExtTextColumn<CaptchaWizardEntry>(_GUI.T.CaptchaWizard_column_domain()) {
                private static final long serialVersionUID = 1L;

                /* Disabled rows are greyed out. */
                @Override
                public boolean isEnabled(final CaptchaWizardEntry obj) {
                    return obj.isEnabled();
                }

                @Override
                public String getStringValue(final CaptchaWizardEntry value) {
                    return value.getDomain();
                }

                @Override
                protected Icon getIcon(final CaptchaWizardEntry value) {
                    return DomainInfo.getInstance(value.getDomain()).getFavIcon();
                }
            });
            addColumn(new ExtTextColumn<CaptchaWizardEntry>(_GUI.T.CaptchaWizard_column_captchaType()) {
                private static final long serialVersionUID = 1L;

                @Override
                public boolean isEnabled(final CaptchaWizardEntry obj) {
                    return obj.isEnabled();
                }

                @Override
                public String getStringValue(final CaptchaWizardEntry value) {
                    return value.getCaptchaType().getDisplayName();
                }

                @Override
                protected Icon getIcon(final CaptchaWizardEntry value) {
                    return value.getCaptchaType().getIcon();
                }
            });
            /* Information only, hidden by default. */
            addColumn(new ExtTextColumn<CaptchaWizardEntry>(_GUI.T.CaptchaWizard_column_requestType()) {
                private static final long serialVersionUID = 1L;

                @Override
                public boolean isEnabled(final CaptchaWizardEntry obj) {
                    return obj.isEnabled();
                }

                @Override
                public String getStringValue(final CaptchaWizardEntry value) {
                    return value.getRequestType() == null ? "" : value.getRequestType().getLabel();
                }

                @Override
                public boolean isDefaultVisible() {
                    return false;
                }
            });
        }
    }
}
