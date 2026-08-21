package org.jdownloader.gui.views.linkgrabber.actions;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.event.queue.QueueAction;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.SelectionInfo.PackageView;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.settings.staticreferences.CFG_LINKGRABBER;

import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.controlling.packagecontroller.PackageController;
import jd.controlling.packagecontroller.PackageController.PackageSettings;

public abstract class AbstractMergeSameNamedPackagesAction<PackageType extends AbstractPackageNode<ChildrenType, PackageType>, ChildrenType extends AbstractPackageChildrenNode<PackageType>> extends CustomizableTableContextAppAction<PackageType, ChildrenType> implements ActionContext {
    public static enum CaseInsensitiveMergeOptions implements LabelInterface {
        GLOBAL {
            @Override
            public String getLabel() {
                return _GUI.T.MergeSameNamedPackagesAction_CaseInsensitiveMergeOptions_GLOBAL(isEnabled() ? _GUI.T.MergeSameNamedPackagesAction_CaseInsensitiveMergeOptions_ENABLED() : _GUI.T.MergeSameNamedPackagesAction_CaseInsensitiveMergeOptions_DISABLED());
            }

            @Override
            public boolean isEnabled() {
                return CFG_LINKGRABBER.CFG.isMergeSameNamedPackagesInDownloadlistInExistingPackagesCaseInsensitiveDefaultEnabled();
            }
        },
        ENABLED {
            @Override
            public String getLabel() {
                return _GUI.T.MergeSameNamedPackagesAction_CaseInsensitiveMergeOptions_ENABLED();
            }

            @Override
            public boolean isEnabled() {
                return true;
            }
        },
        DISABLED {
            @Override
            public String getLabel() {
                return _GUI.T.MergeSameNamedPackagesAction_CaseInsensitiveMergeOptions_DISABLED();
            }

            @Override
            public boolean isEnabled() {
                return false;
            }
        };

        public abstract boolean isEnabled();
    }

    private CaseInsensitiveMergeOptions caseInsensitive = CaseInsensitiveMergeOptions.GLOBAL;
    private boolean                     mergeAll        = false;

    public static String getTranslationForMatchPackageNamesCaseInsensitive() {
        return _GUI.T.MergeSameNamedPackagesAction_Case_Insensitive();
    }

    @Customizer(link = "#getTranslationForMatchPackageNamesCaseInsensitive")
    @Order(100)
    public CaseInsensitiveMergeOptions getMatchPackageNamesCaseInsensitive() {
        return caseInsensitive;
    }

    public void setMatchPackageNamesCaseInsensitive(CaseInsensitiveMergeOptions val) {
        if (val == null) {
            val = CaseInsensitiveMergeOptions.GLOBAL;
        }
        this.caseInsensitive = val;
    }

    public static String getTranslationForMergeAll() {
        return _GUI.T.MergeSameNamedPackagesAction_Merge_All();
    }

    @Customizer(link = "#getTranslationForMergeAll")
    @Order(200)
    public boolean isMergeAll() {
        return mergeAll;
    }

    public void setMergeAll(boolean val) {
        this.mergeAll = val;
    }

    /**
     * @param selection
     *
     */
    public AbstractMergeSameNamedPackagesAction() {
        super(true, true);
        setName(_GUI.T.MergeSameNamedPackagesAction_());
        setIconKey(IconKey.ICON_REMOVE_DUPES);
    }

    private static final long serialVersionUID = -1758454550263991987L;

    public void actionPerformed(ActionEvent e) {
        if (!isEnabled()) {
            return;
        }
        final SelectionInfo<PackageType, ChildrenType> sel = getSelection();
        if (sel == null) {
            return;
        }
        final PackageController<PackageType, ChildrenType> controller = sel.getController();
        controller.getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                final PackageSettings settings = new PackageSettings();
                settings.setMergeSameNamedPackages(true);
                settings.setMergeSameNamedPackagesCaseInsensitive(getMatchPackageNamesCaseInsensitive().isEnabled());
                /* If user has selected package(s), only collect duplicates within selection. */
                final List<PackageView<PackageType, ChildrenType>> selPackageViews = sel.getPackageViews();
                if (isMergeAll() || selPackageViews == null || selPackageViews.size() == 0) {
                    /* Merge duplicates in whole list */
                    controller.merge(null, null, null, settings);
                } else {
                    /* Merge duplicates within users' selection */
                    final List<PackageType> selectedPackages = new ArrayList<PackageType>();
                    for (final PackageView<PackageType, ChildrenType> pv : selPackageViews) {
                        final PackageType selectedpackage = pv.getPackage();
                        selectedPackages.add(selectedpackage);
                    }
                    if (selectedPackages.isEmpty()) {
                        /* User has only selected items we can't work with -> Do nothing */
                        return null;
                    }
                    controller.merge(null, null, selectedPackages, settings);
                }
                return null;
            }
        });
    }

    @Override
    public boolean isEnabled() {
        final SelectionInfo<PackageType, ChildrenType> sel = getSelection();
        if (sel == null) {
            return false;
        }
        final PackageController<PackageType, ChildrenType> controller = sel.getController();
        if (controller == null || controller.getPackages() == null || controller.getPackages().isEmpty()) {
            /* Zero items in linkgrabberlist/downloadlist -> Nothing that can be merged. */
            return false;
        }
        return super.isEnabled();
    }
}
