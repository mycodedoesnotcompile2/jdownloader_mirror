package org.jdownloader.gui.views.linkgrabber.contextmenu;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.packagehistorycontroller.DownloadPathHistoryManager;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.SelectionInfo.PackageView;
import org.jdownloader.gui.views.components.LocationInList;
import org.jdownloader.gui.views.components.packagetable.LinkTreeUtils;
import org.jdownloader.gui.views.downloads.action.MergeSameNamedPackagesAction;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.translate._JDT;

import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.controlling.packagecontroller.PackageController;

public abstract class AbstractMergeToPackageAction<PackageType extends AbstractPackageNode<ChildrenType, PackageType>, ChildrenType extends AbstractPackageChildrenNode<PackageType>> extends CustomizableTableContextAppAction<PackageType, ChildrenType> implements ActionContext {
    /**
     *
     */
    private static final long serialVersionUID        = -4468197802870765463L;
    private boolean           displayNewPackageDialog = true;
    private boolean           expandNewPackage        = false;
    private LocationInList    location                = LocationInList.END_OF_LIST;
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
    @Order(100)
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
    public void addContextSetup(ActionContext contextSetup) {
        super.addContextSetup(contextSetup);
    }

    public void actionPerformed(ActionEvent e) {
        if (!isEnabled()) {
            return;
        }
        final SelectionInfo<PackageType, ChildrenType> sel = getSelection();
        String downloadFolder = null;
        final List<String> history_paths;
        if (isLastPathDefault() && (history_paths = DownloadPathHistoryManager.getInstance().listPaths((String[]) null)) != null && history_paths.size() > 0) {
            downloadFolder = history_paths.get(0);
        } else {
            downloadFolder = LinkTreeUtils.getRawDownloadDirectory(sel.getFirstPackage()).getAbsolutePath();
        }
        final boolean final_mergeSameNamedPackages;
        final String final_newPackageName;
        final String final_downloadFolder;
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
                if (downloadFolder != null) {
                    d.setDownloadFolder(downloadFolder);
                }
                d.setMergeWithSameNamedPackages(Boolean.TRUE.equals(this.isMergeSameNamedPackages()));
                Dialog.getInstance().showDialog(d);
            } catch (final DialogNoAnswerException e1) {
                return;
            }
            final_mergeSameNamedPackages = d.isMergeWithSameNamedPackages();
            final_newPackageName = d.getName();
            final_downloadFolder = d.getDownloadFolder();
        } else {
            final_mergeSameNamedPackages = Boolean.TRUE.equals(this.isMergeSameNamedPackages());
            final_newPackageName = sel.getFirstPackage().getName();
            final_downloadFolder = downloadFolder;
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
                newPackage.setExpanded(isExpandNewPackage());
                final String packageComment = mergePackageViewListComments(sel.getPackageViews());
                if (!StringUtils.isEmpty(packageComment)) {
                    newPackage.setComment(packageComment);
                }
                switch (getLocation()) {
                case AFTER_SELECTION:
                    int index = -1;
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
                    controller.moveOrAddAt(newPackage, sel.getChildren(), 0, -1);
                    break;
                case TOP_OF_LIST:
                    controller.moveOrAddAt(newPackage, sel.getChildren(), 0, 0);
                    break;
                }
                if (final_mergeSameNamedPackages) {
                    final MergeSameNamedPackagesAction mp = new MergeSameNamedPackagesAction();
                    mp.setMergeAll(true);
                    mp.actionPerformed(e);
                }
                return null;
            }
        });
    }

    protected abstract PackageType createNewPackage(final String name, final String downloadFolder);

    /** Merges comments of multiple packages into one string. */
    protected String mergePackageViewListComments(final List<PackageView<PackageType, ChildrenType>> packages) {
        final List<PackageType> crawledpackagelist = new ArrayList<PackageType>();
        for (PackageView<PackageType, ChildrenType> pv : packages) {
            crawledpackagelist.add(pv.getPackage());
        }
        return mergePackageComments(crawledpackagelist);
    }

    /** Merges comments of given packages into one string. */
    protected String mergePackageComments(final List<PackageType> packages) {
        final StringBuilder sb = new StringBuilder();
        final HashSet<String> commentDups = new HashSet<String>();
        for (final PackageType cp : packages) {
            final String comment = cp.getComment();
            if (StringUtils.isNotEmpty(comment)) {
                final String[] commentLines = Regex.getLines(comment);
                for (final String commentLine : commentLines) {
                    if (StringUtils.isNotEmpty(commentLine) && commentDups.add(commentLine)) {
                        if (sb.length() > 0) {
                            sb.append("\r\n");
                        }
                        sb.append(commentLine);
                    }
                }
            }
        }
        return sb.toString();
    }
}
