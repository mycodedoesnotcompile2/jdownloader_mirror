package org.jdownloader.gui.views.linkgrabber.contextmenu;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.controlling.packagizer.PackagizerController;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.packagehistorycontroller.DownloadPathHistoryManager;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.SelectionInfo.PackageView;
import org.jdownloader.gui.views.components.LocationInList;
import org.jdownloader.gui.views.components.packagetable.LinkTreeUtils;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.translate._JDT;

import jd.controlling.linkcollector.LinknameCleaner;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.controlling.packagecontroller.PackageController;
import jd.controlling.packagecontroller.PackageController.PackageSettings;

public abstract class AbstractMergeToPackageAction<PackageType extends AbstractPackageNode<ChildrenType, PackageType>, ChildrenType extends AbstractPackageChildrenNode<PackageType>> extends CustomizableTableContextAppAction<PackageType, ChildrenType> implements ActionContext {
    /**
     *
     */
    private static final long serialVersionUID        = -4468197802870765463L;
    private boolean           displayNewPackageDialog = true;
    private PackageExpandMode packageExpandMode       = PackageExpandMode.AUTO;
    private LocationInList    location                = LocationInList.END_OF_LIST;
    private DownloadPath      downloadpath            = DownloadPath.GLOBAL_DEFAULT;
    private boolean           mergeSameNamedPackages  = false;

    public AbstractMergeToPackageAction() {
        setName(_GUI.T.MergeToPackageAction_MergeToPackageAction_());
        setIconKey(IconKey.ICON_PACKAGE_NEW);
    }

    public static String getTranslationForDisplayNewPackageDialog() {
        return "Display new package dialog";
    }

    @Customizer(link = "#getTranslationForDisplayNewPackageDialog")
    @Order(100)
    public boolean isDisplayNewPackageDialog() {
        return displayNewPackageDialog;
    }

    public void setDisplayNewPackageDialog(boolean displayNewPackageDialog) {
        this.displayNewPackageDialog = displayNewPackageDialog;
    }

    public static String getTranslationPackageExpandMode() {
        return "Package expand mode";
    }

    @Customizer(link = "#getTranslationPackageExpandMode")
    @Order(201)
    public PackageExpandMode getPackageExpandMode() {
        return packageExpandMode;
    }

    public void setPackageExpandMode(PackageExpandMode mode) {
        this.packageExpandMode = mode;
    }

    public static enum PackageExpandMode implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return _JDT.FIX_ME("Auto");
            }
        },
        EXPANDED {
            @Override
            public String getLabel() {
                return _JDT.T.PackageExpandBehavior_EXPANDED();
            }
        },
        COLLAPSED {
            @Override
            public String getLabel() {
                return _JDT.T.PackageExpandBehavior_COLLAPSED();
            }
        };
    }

    public static String getTranslationForDownloadPath() {
        return _JDT.FIX_ME("Download path");
    }

    @Customizer(link = "#getTranslationForDownloadPath")
    @Order(201)
    public DownloadPath getDownloadPath() {
        return downloadpath;
    }

    public void setDownloadPath(DownloadPath path) {
        this.downloadpath = path;
    }

    public enum DownloadPath implements LabelInterface {
        PATH_OF_FIRST_SELECTED_PACKAGE {
            @Override
            public String getLabel() {
                return _JDT.FIX_ME("Path of first selected package");
            }
        },
        LAST_DOWNLOAD_PATH {
            @Override
            public String getLabel() {
                return _JDT.FIX_ME("Last used download path");
            }
        },
        GLOBAL_DEFAULT {
            @Override
            public String getLabel() {
                return _JDT.FIX_ME("Global default download path");
            }
        };
    }

    private String getDownloadPathString() {
        final List<String> history_paths;
        if (downloadpath == DownloadPath.GLOBAL_DEFAULT) {
            final String path = PackagizerController.replaceDynamicTags(org.appwork.storage.config.JsonConfig.create(GeneralSettings.class).getDefaultDownloadFolder(), null, null);
            return path;
        } else if (downloadpath == DownloadPath.LAST_DOWNLOAD_PATH && (history_paths = DownloadPathHistoryManager.getInstance().listPaths((String[]) null)) != null && history_paths.size() > 0) {
            return history_paths.get(0);
        } else {
            /* Path of first selected package */
            final SelectionInfo<PackageType, ChildrenType> sel = getSelection();
            return LinkTreeUtils.getRawDownloadDirectory(sel.getFirstPackage()).getAbsolutePath();
        }
    }

    public static String getTranslationForLocation() {
        return _JDT.T.MergeToPackageAction_getTranslationForLocation();
    }

    @Customizer(link = "#getTranslationForLocation")
    @Order(201)
    public LocationInList getLocation() {
        return location;
    }

    public void setLocation(LocationInList location) {
        this.location = location;
    }

    public static String getTranslationForMergeSameNamedPackages() {
        return _GUI.T.MergeSameNamedPackagesAction_();
    }

    @Customizer(link = "#getTranslationForMergeSameNamedPackages")
    @Order(300)
    public boolean isMergeSameNamedPackages() {
        return this.mergeSameNamedPackages;
    }

    public void setMergeSameNamedPackages(final boolean bool) {
        this.mergeSameNamedPackages = bool;
    }

    @Override
    protected void onActionPerformed(ActionEvent e, final SelectionType selectionType, final SelectionInfo<PackageType, ChildrenType> sel) {
        if (!isEnabled()) {
            return;
        }
        int numberofExpandedPackages = 0;
        int numberofSelectedPackages = 0;
        String singleFileName = null;
        final List<PackageView<PackageType, ChildrenType>> pvlist = sel.getPackageViews();
        for (final PackageView<PackageType, ChildrenType> pv : pvlist) {
            if (pv.getPackage().isEnabled()) {
                numberofExpandedPackages++;
            }
            if (pv.isPackageSelected()) {
                numberofSelectedPackages++;
            }
            final List<ChildrenType> children = pv.getSelectedChildren();
            if (children.size() == 1) {
                singleFileName = children.get(0).getName();
            }
        }
        AbstractPackageNode<?, ?> selectedpackage = null;
        final Object rawContext = sel.getRawContext();
        if (rawContext instanceof AbstractPackageNode) {
            selectedpackage = (AbstractPackageNode<?, ?>) rawContext;
        } else {
            /* No package selected */
        }
        final String suggestedNewPackageName;
        if (numberofSelectedPackages == 0 && singleFileName != null) {
            suggestedNewPackageName = LinknameCleaner.derivePackagenameFromFilename(singleFileName);
        } else {
            suggestedNewPackageName = sel.getFirstPackage().getName();
        }
        final String downloadFolder = getDownloadPathString();
        final boolean final_mergeSameNamedPackages;
        final String final_newPackageName;
        final String final_downloadFolder;
        final boolean final_expandPackage;
        final boolean expandPackagePreSetState;
        final PackageExpandMode packageExpandMode = this.getPackageExpandMode();
        if (packageExpandMode == PackageExpandMode.AUTO) {
            final boolean autoPackageExpandedResult;
            if (selectedpackage != null) {
                autoPackageExpandedResult = selectedpackage.isExpanded();
            } else if (numberofSelectedPackages > 0 && numberofExpandedPackages >= numberofSelectedPackages / 2) {
                /* Most selected packages are expanded -> Expand target package too */
                autoPackageExpandedResult = true;
            } else {
                autoPackageExpandedResult = false;
            }
            expandPackagePreSetState = autoPackageExpandedResult;
        } else if (packageExpandMode == PackageExpandMode.EXPANDED) {
            expandPackagePreSetState = true;
        } else {
            expandPackagePreSetState = false;
        }
        if (this.isDisplayNewPackageDialog()) {
            NewPackageDialog d = null;
            try {
                d = new NewPackageDialog(sel) {
                    @Override
                    public String getDontShowAgainKey() {
                        return "ABSTRACTDIALOG_DONT_SHOW_AGAIN_" + AbstractMergeToPackageAction.this.getClass().getSimpleName();
                    }
                };
                /* Set dialog default values */
                d.setDownloadFolder(downloadFolder);
                d.setMergeCheckboxDefaultValue(Boolean.TRUE.equals(this.isMergeSameNamedPackages()));
                d.setExpandPackage(expandPackagePreSetState);
                d.setPreSetPackageName(suggestedNewPackageName);
                Dialog.getInstance().showDialog(d);
            } catch (final DialogNoAnswerException e1) {
                return;
            }
            final_mergeSameNamedPackages = d.isMergeWithSameNamedPackages();
            final_newPackageName = d.getName();
            final_downloadFolder = d.getDownloadFolder();
            final_expandPackage = d.isExpandPackage();
        } else {
            final_mergeSameNamedPackages = Boolean.TRUE.equals(this.isMergeSameNamedPackages());
            final_newPackageName = suggestedNewPackageName;
            final_downloadFolder = downloadFolder;
            final_expandPackage = expandPackagePreSetState;
        }
        if (StringUtils.isEmpty(final_newPackageName)) {
            /* New package name cannot be used -> Do nothing */
            return;
        }
        final PackageController<PackageType, ChildrenType> controller = sel.getController();
        controller.getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                final PackageType newPackage = createNewPackage(final_newPackageName, final_downloadFolder);
                newPackage.setExpanded(final_expandPackage);
                final List<PackageType> packages = new ArrayList<PackageType>();
                for (PackageView<PackageType, ChildrenType> pv : sel.getPackageViews()) {
                    packages.add(pv.getPackage());
                }
                final String packageComment = controller.mergePackageComments(packages);
                if (!StringUtils.isEmpty(packageComment)) {
                    newPackage.setComment(packageComment);
                }
                int index = -1;
                switch (getLocation()) {
                case AFTER_SELECTION:
                    for (PackageView<PackageType, ChildrenType> pv : sel.getPackageViews()) {
                        index = Math.max(index, controller.indexOf(pv.getPackage()) + 1);
                    }
                    break;
                case BEFORE_SELECTION:
                    index = Integer.MAX_VALUE;
                    for (PackageView<PackageType, ChildrenType> pv : sel.getPackageViews()) {
                        index = Math.min(index, controller.indexOf(pv.getPackage()));
                    }
                    if (index == Integer.MAX_VALUE) {
                        index = 0;
                    }
                    break;
                case END_OF_LIST:
                    index = Integer.MAX_VALUE;
                    break;
                case TOP_OF_LIST:
                    index = 0;
                    break;
                }
                final PackageSettings mergesettings = new PackageSettings();
                mergesettings.setExpandPackage(final_expandPackage);
                mergesettings.setPackagePosition(index);
                mergesettings.setMergeSameNamedPackages(final_mergeSameNamedPackages);
                controller.merge(newPackage, sel.getChildren(), null, mergesettings);
                return null;
            }
        });
    }

    protected abstract PackageType createNewPackage(final String name, final String downloadFolder);
}
