package org.jdownloader.gui.views.downloads.bottombar;

import java.lang.reflect.InvocationTargetException;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import javax.swing.Icon;
import javax.swing.JComponent;

import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.NullsafeAtomicReference;
import org.appwork.utils.swing.EDTHelper;
import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.contextmenu.MenuItemData;
import org.jdownloader.controlling.contextmenu.MenuLink;
import org.jdownloader.controlling.contextmenu.gui.MenuBuilder;
import org.jdownloader.extensions.ExtensionNotLoadedException;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.components.PseudoCombo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTableModelFilter;
import org.jdownloader.gui.views.components.packagetable.TableStateSnapshot;
import org.jdownloader.gui.views.downloads.View;
import org.jdownloader.gui.views.downloads.table.DownloadsTable;
import org.jdownloader.gui.views.downloads.table.DownloadsTableModel;
import org.jdownloader.plugins.FinalLinkState;
import org.jdownloader.plugins.SkipReason;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;
import org.jdownloader.settings.staticreferences.CFG_GUI;

public class QuickFilterMenuItem extends MenuItemData implements MenuLink {
    @Override
    public JComponent createSettingsPanel() {
        return null;
    }

    @Override
    public List<AppAction> createActionsToLink() {
        return null;
    }

    public static final class FilterCombo extends PseudoCombo<View> {
        private final DownloadsTable                                                                  table;
        protected static FilterCombo                                                                  INSTANCE;
        private NullsafeAtomicReference<PackageControllerTableModelFilter<FilePackage, DownloadLink>> appliedFilter = new NullsafeAtomicReference<PackageControllerTableModelFilter<FilePackage, DownloadLink>>(null);
        /*
         * Selection and scroll position of every view, remembered when the view is left. Restored when it gets selected again, unless the
         * current selection can be taken over into it. Only accessed in the EDT. Not persisted: After a restart no view has a remembered
         * state.
         */
        private final Map<View, TableStateSnapshot>                                                    viewStates    = new EnumMap<View, TableStateSnapshot>(View.class);
        /* View whose filter is currently applied, null before the first one has been applied */
        private View                                                                                  currentView   = null;

        public static FilterCombo getInstance() {
            return new EDTHelper<FilterCombo>() {
                @Override
                public FilterCombo edtRun() {
                    if (INSTANCE != null) {
                        return INSTANCE;
                    }
                    INSTANCE = new FilterCombo();
                    return INSTANCE;
                }
            }.getReturnValue();
        }

        @Override
        protected boolean isHideSelf() {
            return false;
        }

        @Override
        protected Icon getIcon(View v, boolean closed) {
            return v.getIcon();
        }

        @Override
        protected String getLabel(View v, boolean closed) {
            return v.getLabel();
        }

        private FilterCombo() {
            super(new View[] { View.ALL, View.RUNNING, View.FAILED, View.EXISTS, View.OFFLINE, View.SKIPPED, View.SUCCESSFUL, View.TODO });
            this.setToolTipText(_GUI.T.PseudoCombo_PseudoCombo_tt_());
            this.table = (DownloadsTable) DownloadsTableModel.getInstance().getTable();
            View view = (View) CFG_GUI.DOWNLOAD_VIEW.getValue();
            if (view == null || !JsonConfig.create(GraphicalUserInterfaceSettings.class).isSaveDownloadViewCrossSessionEnabled()) {
                view = View.ALL;
            }
            setSelectedItem(view);
        }

        @Override
        public void onChanged(final View value) {
            final View previous = currentView;
            currentView = value;
            /* State of the view which is left: Its selection is taken over into the new view if possible */
            final TableStateSnapshot leftState = previous != null && previous != value ? TableStateSnapshot.create(table) : null;
            if (leftState != null) {
                /* Also remembered, so it can be restored when the user returns to that view */
                viewStates.put(previous, leftState);
            }
            PackageControllerTableModelFilter<FilePackage, DownloadLink> newFilter = getFilter(value);
            PackageControllerTableModelFilter<FilePackage, DownloadLink> oldFilter = appliedFilter.getAndSet(newFilter);
            if (oldFilter != null) {
                table.getModel().removeFilter(oldFilter);
            }
            if (newFilter != null) {
                table.getModel().addFilter(newFilter);
            }
            final TableStateSnapshot targetState = previous != value ? viewStates.remove(value) : null;
            if (leftState != null || targetState != null) {
                /* Registered after the filter change: only a rebuild with the new filter triggers it */
                TableStateSnapshot.runAfterRebuild(table, new Runnable() {
                    @Override
                    public void run() {
                        if (currentView != value) {
                            /* User switched to another view in the meantime */
                            return;
                        }
                        /*
                         * Selected nodes which are present in the new view stay selected. Only if none of them is present, the remembered
                         * state of the new view is restored. Neither (view not visited yet, e.g. first switch after a restart): The table
                         * stays as it is after the rebuild.
                         */
                        if (leftState != null && leftState.carryOverSelection(table)) {
                            return;
                        }
                        if (targetState != null) {
                            targetState.restore(table);
                        }
                    }
                });
            }
            table.getModel().recreateModel(true);
            CFG_GUI.DOWNLOAD_VIEW.setValue(value);
        }

        private PackageControllerTableModelFilter<FilePackage, DownloadLink> getFilter(View value) {
            if (value == null) {
                return null;
            }
            switch (value) {
            case ALL:
                return null;
            case SKIPPED:
                return new PackageControllerTableModelFilter<FilePackage, DownloadLink>() {
                    @Override
                    public boolean isFilteringPackageNodes() {
                        return false;
                    }

                    @Override
                    public boolean isFilteringChildrenNodes() {
                        return true;
                    }

                    @Override
                    public boolean isFiltered(DownloadLink v) {
                        return v.getSkipReason() == null;
                    }

                    @Override
                    public boolean isFiltered(FilePackage e) {
                        return false;
                    }

                    @Override
                    public int getComplexity() {
                        return 0;
                    }
                };
            case FAILED:
                return new PackageControllerTableModelFilter<FilePackage, DownloadLink>() {
                    @Override
                    public boolean isFilteringPackageNodes() {
                        return false;
                    }

                    @Override
                    public boolean isFilteringChildrenNodes() {
                        return true;
                    }

                    @Override
                    public boolean isFiltered(DownloadLink v) {
                        return !(FinalLinkState.CheckFailed(v.getFinalLinkState()));
                    }

                    @Override
                    public boolean isFiltered(FilePackage e) {
                        return false;
                    }

                    @Override
                    public int getComplexity() {
                        return 0;
                    }
                };
            case EXISTS:
                return new PackageControllerTableModelFilter<FilePackage, DownloadLink>() {
                    @Override
                    public boolean isFilteringPackageNodes() {
                        return false;
                    }

                    @Override
                    public boolean isFilteringChildrenNodes() {
                        return true;
                    }

                    @Override
                    public boolean isFiltered(DownloadLink v) {
                        return !FinalLinkState.FAILED_EXISTS.equals(v.getFinalLinkState()) && !SkipReason.FILE_EXISTS.equals(v.getSkipReason());
                    }

                    @Override
                    public boolean isFiltered(FilePackage e) {
                        return false;
                    }

                    @Override
                    public int getComplexity() {
                        return 0;
                    }
                };
            case OFFLINE:
                return new PackageControllerTableModelFilter<FilePackage, DownloadLink>() {
                    @Override
                    public boolean isFilteringPackageNodes() {
                        return false;
                    }

                    @Override
                    public boolean isFilteringChildrenNodes() {
                        return true;
                    }

                    @Override
                    public boolean isFiltered(DownloadLink v) {
                        return FinalLinkState.OFFLINE != v.getFinalLinkState();
                    }

                    @Override
                    public boolean isFiltered(FilePackage e) {
                        return false;
                    }

                    @Override
                    public int getComplexity() {
                        return 0;
                    }
                };
            case RUNNING:
                return new PackageControllerTableModelFilter<FilePackage, DownloadLink>() {
                    @Override
                    public boolean isFilteringPackageNodes() {
                        return false;
                    }

                    @Override
                    public boolean isFilteringChildrenNodes() {
                        return true;
                    }

                    @Override
                    public boolean isFiltered(DownloadLink v) {
                        return !v.isEnabled() || v.getDownloadLinkController() == null;
                    }

                    @Override
                    public boolean isFiltered(FilePackage e) {
                        return false;
                    }

                    @Override
                    public int getComplexity() {
                        return 0;
                    }
                };
            case SUCCESSFUL:
                return new PackageControllerTableModelFilter<FilePackage, DownloadLink>() {
                    @Override
                    public boolean isFilteringPackageNodes() {
                        return false;
                    }

                    @Override
                    public boolean isFilteringChildrenNodes() {
                        return true;
                    }

                    @Override
                    public boolean isFiltered(DownloadLink v) {
                        return !(FinalLinkState.CheckFinished(v.getFinalLinkState()));
                    }

                    @Override
                    public boolean isFiltered(FilePackage e) {
                        return false;
                    }

                    @Override
                    public int getComplexity() {
                        return 0;
                    }
                };
            case TODO:
                return new PackageControllerTableModelFilter<FilePackage, DownloadLink>() {
                    @Override
                    public boolean isFilteringPackageNodes() {
                        return false;
                    }

                    @Override
                    public boolean isFilteringChildrenNodes() {
                        return true;
                    }

                    @Override
                    public boolean isFiltered(DownloadLink v) {
                        return v.getFinalLinkState() != null || v.getDownloadLinkController() != null;
                    }

                    @Override
                    public boolean isFiltered(FilePackage e) {
                        return false;
                    }

                    @Override
                    public int getComplexity() {
                        return 0;
                    }
                };
            }
            return null;
        }
    }

    public QuickFilterMenuItem() {
        super();
        setName(_GUI.T.QuickFilterMenuItem_QuickFilterMenuItem());
        setIconKey(IconKey.ICON_FILTER);
        setVisible(true);
        //
    }

    @Override
    public JComponent createItem(MenuBuilder menuBuilder) throws InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, ClassNotFoundException, NoSuchMethodException, SecurityException, ExtensionNotLoadedException {
        return FilterCombo.getInstance();
    }
}
