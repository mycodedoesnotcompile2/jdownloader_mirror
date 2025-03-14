package org.jdownloader.gui.toolbar;

import java.awt.event.ActionEvent;
import java.lang.ref.WeakReference;

import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.toolbar.action.SelectionBasedToolbarAction;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.LocationInList;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.EDTSelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.linkgrabber.contextmenu.AbstractMergeToPackageAction.DownloadPath;
import org.jdownloader.gui.views.linkgrabber.contextmenu.AbstractMergeToPackageAction.PackageExpandMode;
import org.jdownloader.gui.views.linkgrabber.contextmenu.MergeToPackageAction;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.translate._JDT;

import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.JDGui.Panels;
import jd.gui.swing.jdgui.interfaces.View;

public class MoveToNewFolderToolbarAction extends SelectionBasedToolbarAction {
    public MoveToNewFolderToolbarAction() {
        setName(_GUI.T.MergeToPackageAction_MergeToPackageAction_());
        setIconKey(IconKey.ICON_PACKAGE_NEW);
    }

    @Override
    public void onGuiMainTabSwitch(View oldView, View newView) {
        super.onGuiMainTabSwitch(oldView, newView);
    }

    private boolean           displayNewPackageDialog = true;
    private PackageExpandMode packageExpandMode       = PackageExpandMode.AUTO;
    private LocationInList    location                = LocationInList.END_OF_LIST;
    private DownloadPath      downloadpath            = DownloadPath.GLOBAL_DEFAULT;
    private boolean           mergeSameNamedPackages  = false;

    public static String getTranslationForDisplayNewPackageDialog() {
        return _GUI.T.MergeToPackageAction_setting_SettingDisplayDialog();
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
        return _GUI.T.MergeToPackageAction_setting_PackageExpandMode();
    }

    @Customizer(link = "#getTranslationPackageExpandMode")
    @Order(201)
    public PackageExpandMode getPackageExpandMode() {
        return packageExpandMode;
    }

    public void setPackageExpandMode(PackageExpandMode mode) {
        this.packageExpandMode = mode;
    }

    public static String getTranslationForDownloadPath() {
        return _GUI.T.gui_config_general_downloaddirectory();
    }

    @Customizer(link = "#getTranslationForDownloadPath")
    @Order(201)
    public DownloadPath getDownloadPath() {
        return downloadpath;
    }

    public void setDownloadPath(DownloadPath path) {
        this.downloadpath = path;
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
    public void actionPerformed(ActionEvent event) {
        if (JDGui.getInstance().isCurrentPanel(Panels.LINKGRABBER)) {
            final MergeToPackageAction action = new MergeToPackageAction();
            action.setDisplayNewPackageDialog(this.isDisplayNewPackageDialog());
            action.setPackageExpandMode(this.getPackageExpandMode());
            action.setDownloadPath(this.getDownloadPath());
            action.setLocation(getLocation());
            action.setMergeSameNamedPackages(this.isMergeSameNamedPackages());
            action.actionPerformed(event);
        } else if (JDGui.getInstance().isCurrentPanel(Panels.DOWNLOADLIST)) {
            final org.jdownloader.gui.views.downloads.action.MergeToPackageAction action = new org.jdownloader.gui.views.downloads.action.MergeToPackageAction();
            action.setDisplayNewPackageDialog(this.isDisplayNewPackageDialog());
            action.setPackageExpandMode(this.getPackageExpandMode());
            action.setDownloadPath(this.getDownloadPath());
            action.setLocation(getLocation());
            action.setMergeSameNamedPackages(this.isMergeSameNamedPackages());
            action.actionPerformed(event);
        }
    }

    @Override
    protected String createTooltip() {
        return null;
    }

    protected volatile WeakReference<SelectionInfoCallback> lastCallBack = new WeakReference<SelectionInfoCallback>(null);

    @Override
    protected void onSelectionUpdate(final PackageControllerTable<?, ?> table) {
        if (table == null) {
            setEnabled(false);
            return;
        } else {
            table.getSelectionInfo(new EDTSelectionInfoCallback() {
                {
                    lastCallBack = new WeakReference<SelectionInfoCallback>(this);
                }

                @Override
                public boolean isCancelled() {
                    final WeakReference<SelectionInfoCallback> lastCallBack = MoveToNewFolderToolbarAction.this.lastCallBack;
                    return lastCallBack.get() != this;
                }

                @Override
                public void onSelectionInfo(SelectionInfo selectionInfo) {
                    setEnabled(!selectionInfo.isEmpty());
                }
            }, SelectionType.SELECTED);
        }
    }
}
