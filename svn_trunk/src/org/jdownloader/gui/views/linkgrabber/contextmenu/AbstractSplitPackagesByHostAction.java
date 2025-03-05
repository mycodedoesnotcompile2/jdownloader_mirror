package org.jdownloader.gui.views.linkgrabber.contextmenu;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.controlling.packagizer.PackagizerController;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.LocationInList;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.linkgrabber.addlinksdialog.LinkgrabberSettings;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.settings.staticreferences.CFG_LINKCOLLECTOR;
import org.jdownloader.translate._JDT;

import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.controlling.packagecontroller.PackageController;
import jd.controlling.packagecontroller.PackageController.PackageSettings;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

public abstract class AbstractSplitPackagesByHostAction<PackageType extends AbstractPackageNode<ChildrenType, PackageType>, ChildrenType extends AbstractPackageChildrenNode<PackageType>> extends CustomizableTableContextAppAction<PackageType, ChildrenType> implements ActionContext {
    private static final long   serialVersionUID        = 2636706677433058055L;
    private boolean             mergePackages           = false;
    private final static String NAME                    = _GUI.T.SplitPackagesByHost_SplitPackagesByHost_object_();
    private boolean             displayNewPackageDialog = true;
    private LocationInList      location                = LocationInList.AFTER_SELECTION;

    public AbstractSplitPackagesByHostAction() {
        super();
        setName(NAME);
        setIconKey(IconKey.ICON_SPLIT_PACKAGES);
    }

    public static String getTranslationForAskForNewDownloadFolderAndPackageName() {
        return _JDT.T.SplitPackagesByHost_getTranslationForAskForNewDownloadFolderAndPackageName();
    }

    @Customizer(link = "#getTranslationForAskForNewDownloadFolderAndPackageName")
    @Order(100)
    public boolean isDisplayNewPackageDialog() {
        return displayNewPackageDialog;
    }

    public void setDisplayNewPackageDialog(boolean b) {
        this.displayNewPackageDialog = b;
    }

    public static String getTranslationForMergePackages() {
        return _JDT.T.SplitPackagesByHost_getTranslationForMergePackages();
    }

    @Customizer(link = "#getTranslationForMergePackages")
    @Order(200)
    public boolean isMergePackages() {
        return mergePackages;
    }

    public void setMergePackages(boolean mergePackages) {
        this.mergePackages = mergePackages;
    }

    public static String getTranslationForLocation() {
        return _JDT.T.SplitPackagesByHost_getTranslationForLocation();
    }

    @Customizer(link = "#getTranslationForLocation")
    @Order(300)
    public LocationInList getLocation() {
        return location;
    }

    public void setLocation(LocationInList location) {
        this.location = location;
    }

    @Override
    protected void onActionPerformed(final ActionEvent e, final SelectionType selectionType, final SelectionInfo<PackageType, ChildrenType> sel) {
        if (!isEnabled()) {
            return;
        }
        final boolean isLinkgrabber = sel.getController() instanceof LinkCollector;
        final String newName;
        final String newDownloadFolder;
        boolean packageExpandState = CFG_LINKCOLLECTOR.CFG.isPackageAutoExpanded();
        final boolean final_mergePackages = isMergePackages();
        if (final_mergePackages && sel.getPackageViews().size() > 1) {
            if (isDisplayNewPackageDialog()) {
                NewPackageDialog d = null;
                try {
                    d = new NewPackageDialog(sel) {
                        @Override
                        public String getDontShowAgainKey() {
                            return "ABSTRACTDIALOG_DONT_SHOW_AGAIN_" + AbstractSplitPackagesByHostAction.this.getClass().getSimpleName();
                        }
                    };
                    d.setDisplayCheckboxMergeWithSameNamedPackages(false);
                    d.setExpandPackage(packageExpandState);
                    Dialog.getInstance().showDialog(d);
                    newName = d.getName();
                    newDownloadFolder = d.getDownloadFolder();
                    if (StringUtils.isEmpty(newName)) {
                        return;
                    }
                } catch (Throwable ignore) {
                    return;
                }
                packageExpandState = d.isExpandPackage();
            } else {
                newName = "";
                newDownloadFolder = sel.getFirstPackage().getDownloadDirectory();
            }
        } else {
            newName = null;
            newDownloadFolder = null;
        }
        final boolean final_packageExpandState = packageExpandState;
        final PackageController<PackageType, ChildrenType> controller = sel.getController();
        controller.getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                final Map<AbstractPackageNode, Map<String, List<ChildrenType>>> splitMap = new HashMap<AbstractPackageNode, Map<String, List<ChildrenType>>>();
                int insertAt = -1;
                switch (getLocation()) {
                case BEFORE_SELECTION:
                    insertAt = Integer.MAX_VALUE;
                }
                for (final AbstractNode child : sel.getChildren()) {
                    if (!(child instanceof DownloadLink) && !(child instanceof CrawledLink)) {
                        continue;
                    }
                    final AbstractPackageChildrenNode childnode = (AbstractPackageChildrenNode) child;
                    // final DownloadLink cL = (DownloadLink) child;
                    final AbstractPackageNode parent = final_mergePackages ? null : (AbstractPackageNode) childnode.getParentNode();
                    Map<String, List<ChildrenType>> parentMap = splitMap.get(parent);
                    if (parentMap == null) {
                        parentMap = new HashMap<String, List<ChildrenType>>();
                        splitMap.put(parent, parentMap);
                    }
                    final String host = childnode.getDomainInfo().getTld();
                    List<ChildrenType> hostList = parentMap.get(host);
                    if (hostList == null) {
                        hostList = new ArrayList<ChildrenType>();
                        parentMap.put(host, hostList);
                    }
                    hostList.add((ChildrenType) childnode);
                    switch (getLocation()) {
                    case AFTER_SELECTION:
                        insertAt = Math.max(insertAt, controller.indexOf((PackageType) ((AbstractPackageChildrenNode) child).getParentNode()) + 1);
                        break;
                    case BEFORE_SELECTION:
                        insertAt = Math.min(insertAt, controller.indexOf((PackageType) ((AbstractPackageChildrenNode) child).getParentNode()));
                        break;
                    case END_OF_LIST:
                        insertAt = -1;
                        break;
                    case TOP_OF_LIST:
                        insertAt = 0;
                        break;
                    }
                }
                if (insertAt == Integer.MAX_VALUE) {
                    insertAt = 0;
                }
                final String nameFactory = JsonConfig.create(LinkgrabberSettings.class).getSplitPackageNameFactoryPattern();
                final boolean merge = JsonConfig.create(LinkgrabberSettings.class).isSplitPackageMergeEnabled();
                final PackageSettings ps = new PackageSettings();
                ps.setExpandPackage(final_packageExpandState);
                // TODO
                // ps.setPackagePosition(insertAt);
                final Map<String, AbstractPackageNode> mergedPackages = new HashMap<String, AbstractPackageNode>();
                final Iterator<Entry<AbstractPackageNode, Map<String, List<ChildrenType>>>> it = splitMap.entrySet().iterator();
                while (it.hasNext()) {
                    final Entry<AbstractPackageNode, Map<String, List<ChildrenType>>> next = it.next();
                    final AbstractPackageNode sourcePackage = next.getKey();
                    final Map<String, List<ChildrenType>> items = next.getValue();
                    final Iterator<Entry<String, List<ChildrenType>>> it2 = items.entrySet().iterator();
                    while (it2.hasNext()) {
                        final Entry<String, List<ChildrenType>> next2 = it2.next();
                        final String host = next2.getKey();
                        final String newPackageName = getNewPackageName(nameFactory, sourcePackage == null ? newName : sourcePackage.getName(), host);
                        final PackageType newPkg;
                        if (isLinkgrabber) {
                            if (merge) {
                                CrawledPackage destPackage = (CrawledPackage) mergedPackages.get(newPackageName);
                                if (destPackage == null) {
                                    destPackage = new CrawledPackage();
                                    destPackage.setExpanded(final_packageExpandState);
                                    if (sourcePackage != null) {
                                        sourcePackage.copyPropertiesTo(destPackage);
                                    } else {
                                        destPackage.setDownloadFolder(newDownloadFolder);
                                    }
                                    destPackage.setName(newPackageName);
                                    mergedPackages.put(newPackageName, destPackage);
                                }
                                newPkg = (PackageType) destPackage;
                            } else {
                                newPkg = (PackageType) new CrawledPackage();
                                newPkg.setExpanded(final_packageExpandState);
                                if (sourcePackage != null) {
                                    sourcePackage.copyPropertiesTo(newPkg);
                                } else {
                                    newPkg.setDownloadFolder(newDownloadFolder);
                                }
                                newPkg.setName(newPackageName);
                            }
                        } else {
                            if (merge) {
                                FilePackage destPackage = (FilePackage) mergedPackages.get(newPackageName);
                                if (destPackage == null) {
                                    destPackage = FilePackage.getInstance();
                                    if (sourcePackage != null) {
                                        sourcePackage.copyPropertiesTo(destPackage);
                                    } else {
                                        final String downloadFolder = PackagizerController.replaceDynamicTags(newDownloadFolder, newPackageName, destPackage);
                                        destPackage.setDownloadDirectory(downloadFolder);
                                    }
                                    destPackage.setName(newPackageName);
                                    mergedPackages.put(newPackageName, destPackage);
                                }
                                newPkg = (PackageType) destPackage;
                            } else {
                                newPkg = (PackageType) FilePackage.getInstance();
                                if (sourcePackage != null) {
                                    sourcePackage.copyPropertiesTo(newPkg);
                                } else {
                                    final String downloadFolder = PackagizerController.replaceDynamicTags(newDownloadFolder, newPackageName, newPkg);
                                    newPkg.setDownloadFolder(downloadFolder);
                                }
                                newPkg.setName(newPackageName);
                            }
                        }
                        controller.moveOrAddAt(newPkg, next2.getValue(), 0, insertAt);
                        insertAt++;
                    }
                }
                return null;
            }
        });
    }

    public String getNewPackageName(String nameFactory, String oldPackageName, String host) {
        if (StringUtils.isEmpty(nameFactory)) {
            if (!StringUtils.isEmpty(oldPackageName)) {
                return oldPackageName;
            }
            return host;
        }
        if (!StringUtils.isEmpty(oldPackageName)) {
            nameFactory = nameFactory.replaceAll("\\{PACKAGENAME\\}", oldPackageName);
        } else {
            nameFactory = nameFactory.replaceAll("\\{PACKAGENAME\\}", _JDT.T.LinkCollector_addCrawledLink_variouspackage());
        }
        nameFactory = nameFactory.replaceAll("\\{HOSTNAME\\}", host);
        return nameFactory;
    }
}
