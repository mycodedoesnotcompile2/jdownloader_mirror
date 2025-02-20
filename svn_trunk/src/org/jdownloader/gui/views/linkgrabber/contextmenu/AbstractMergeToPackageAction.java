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
import org.jdownloader.plugins.config.Order;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.translate._JDT;

import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.controlling.packagecontroller.PackageController;
import jd.controlling.packagecontroller.PackageController.MergePackageSettings;

public abstract class AbstractMergeToPackageAction<PackageType extends AbstractPackageNode<ChildrenType, PackageType>, ChildrenType extends AbstractPackageChildrenNode<PackageType>> extends CustomizableTableContextAppAction<PackageType, ChildrenType> implements ActionContext {
    /**
     *
     */
    private static final long serialVersionUID        = -4468197802870765463L;
    private boolean           displayNewPackageDialog = true;
    private boolean           expandNewPackage        = false;
    private LocationInList    location                = LocationInList.END_OF_LIST;
    private DownloadPath      downloadpath            = DownloadPath.GLOBAL_DEFAULT;
    private boolean           lastPathDefault         = false;
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

    public static String getTranslationForExpandNewPackage() {
        return _JDT.T.MergeToPackageAction_getTranslationForExpandNewPackage();
    }

    @Customizer(link = "#getTranslationForExpandNewPackage")
    @Order(150)
    public boolean isExpandNewPackage() {
        return expandNewPackage;
    }

    public void setExpandNewPackage(boolean expandNewPackage) {
        this.expandNewPackage = expandNewPackage;
    }

    public static String getTranslationForLastPathDefault() {
        return _JDT.T.MergeToPackageAction_getTranslationForLastPathDefault();
    }

    @Customizer(link = "#getTranslationForLastPathDefault")
    @Order(200)
    public boolean isLastPathDefault() {
        return lastPathDefault;
    }

    public void setLastPathDefault(boolean lastPathDefault) {
        this.lastPathDefault = lastPathDefault;
    }

    public static String getTranslationForDownloadPath() {
        return "Download path";
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
                return "Path of first selected package";
            }
        },
        LAST_DOWNLOAD_PATH {
            @Override
            public String getLabel() {
                return "Last used download path";
            }
        },
        GLOBAL_DEFAULT {
            @Override
            public String getLabel() {
                return "Global default download path";
            }
        };
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

    public void actionPerformed(ActionEvent e) {
        if (!isEnabled()) {
            return;
        }
        final SelectionInfo<PackageType, ChildrenType> sel = getSelection();
        boolean userHasSelectedPackage = false;
        final List<AbstractNode> selecteditems = sel.getRawSelection();
        for (final AbstractNode selecteditem : selecteditems) {
            if (selecteditem instanceof AbstractPackageNode) {
                userHasSelectedPackage = true;
                break;
            }
        }
        if (!userHasSelectedPackage) {
            /*
             * TODO: Add more 'intelligent' package name suggestion if selection contains only a single item or only multipart archives that
             * belong together RE ticket https://svn.jdownloader.org/issues/81815
             */
        }
        final String downloadFolder = getDownloadPathNew();
        final boolean final_mergeSameNamedPackages;
        final String final_newPackageName;
        final String final_downloadFolder;
        final boolean final_expandPackage;
        if (this.isDisplayNewPackageDialog()) {
            /*
             * TODO: Maybe collect list of selected package names in beforehand and hide "merge same named packages" checkbox if packages
             * already have the same names.
             */
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
                d.setExpandPackage(isExpandNewPackage());
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
            final_newPackageName = sel.getFirstPackage().getName();
            final_downloadFolder = downloadFolder;
            final_expandPackage = isExpandNewPackage();
        }
        if (StringUtils.isEmpty(final_newPackageName)) {
            /* New package name cannot be used -> Do nothing */
            return;
        }
        final PackageController<PackageType, ChildrenType> controller = sel.getController();
        controller.getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                final MergePackageSettings mergesettings = new MergePackageSettings();
                mergesettings.setExpandPackage(final_expandPackage);
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
                    controller.moveOrAddAt(newPackage, sel.getChildren(), 0, index);
                    break;
                case BEFORE_SELECTION:
                    index = Integer.MAX_VALUE;
                    for (PackageView<PackageType, ChildrenType> pv : sel.getPackageViews()) {
                        index = Math.min(index, controller.indexOf(pv.getPackage()));
                    }
                    if (index == Integer.MAX_VALUE) {
                        index = 0;
                    }
                    controller.moveOrAddAt(newPackage, sel.getChildren(), 0, index);
                    break;
                case END_OF_LIST:
                    index = -1;
                    controller.moveOrAddAt(newPackage, sel.getChildren(), 0, -1);
                    break;
                case TOP_OF_LIST:
                    index = 0;
                    controller.moveOrAddAt(newPackage, sel.getChildren(), 0, 0);
                    break;
                }
                mergesettings.setMergePosition(index);
                if (final_mergeSameNamedPackages) {
                    mergesettings.setMergeSameNamedPackages(true);
                    controller.merge(newPackage, null, null, mergesettings);
                }
                return null;
            }
        });
    }

    private String getDownloadPathNew() {
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

    protected abstract PackageType createNewPackage(final String name, final String downloadFolder);

    @Override
    public boolean isEnabled() {
        final SelectionInfo<PackageType, ChildrenType> sel = getSelection();
        if (sel == null || sel.isEmpty()) {
            return false;
        }
        return super.isEnabled();
    }
}
