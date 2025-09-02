package org.jdownloader.gui.views.linkgrabber.addlinksdialog;

import java.util.List;

import org.appwork.storage.config.ConfigInterface;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultJsonObject;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.DefaultStringValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.MultiLineString;
import org.appwork.storage.config.annotations.RequiresRestart;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.controlling.Priority;
import org.jdownloader.controlling.packagizer.SubFolderByPackageRule;
import org.jdownloader.gui.packagehistorycontroller.DownloadPath;
import org.jdownloader.gui.packagehistorycontroller.PackageHistoryEntry;
import org.jdownloader.gui.views.linkgrabber.contextmenu.ConfirmLinksContextAction.AutoStartOptions;
import org.jdownloader.gui.views.linkgrabber.contextmenu.ConfirmLinksContextAction.OnDupesLinksAction;
import org.jdownloader.gui.views.linkgrabber.contextmenu.ConfirmLinksContextAction.OnOfflineLinksAction;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.ConfirmIncompleteArchiveAction;

public interface LinkgrabberSettings extends ConfigInterface {
    @AboutConfig
    @DescriptionForConfigEntry("Threshold for missing parent directories")
    @DefaultIntValue(1)
    @SpinnerValidator(min = 0, max = Integer.MAX_VALUE)
    int getMissingParentDirectoryThreshold();

    void setMissingParentDirectoryThreshold(int threshold);

    @AboutConfig
    @DefaultBooleanValue(false)
    boolean isVariantsColumnAlwaysVisible();

    void setVariantsColumnAlwaysVisible(boolean b);

    @DefaultJsonObject("[]")
    @DefaultOnNull
    @AboutConfig
    List<DownloadPath> getDownloadDestinationHistory();

    void setDownloadDestinationHistory(List<DownloadPath> value);

    @DefaultJsonObject("[]")
    @DefaultOnNull
    @AboutConfig
    List<PackageHistoryEntry> getPackageNameHistory();

    void setPackageNameHistory(List<PackageHistoryEntry> value);

    @AboutConfig
    @DefaultBooleanValue(true)
    boolean isAutoExtractionEnabled();

    void setAutoExtractionEnabled(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    boolean isVariousPackageEnabled();

    void setVariousPackageEnabled(boolean b);

    void setLatestDownloadDestinationFolder(String absolutePath);

    String getLatestDownloadDestinationFolder();

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("If true, AddLinks Dialogs will use the last used downloadfolder as defaultvalue. IF False, the Default Download Paath (settings) will be used")
    boolean isUseLastDownloadDestinationAsDefault();

    void setUseLastDownloadDestinationAsDefault(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("If true, JD will switch to the Download Tab after confirming Links in Linkgrabber")
    boolean isAutoSwitchToDownloadTableOnConfirmDefaultEnabled();

    void setAutoSwitchToDownloadTableOnConfirmDefaultEnabled(boolean b);

    @AboutConfig
    @RequiresRestart("A JDownloader Restart is Required")
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("If false, The 'Add Links' dialog in Linkgrabber works on the pasted text, and does not prefilter URLS anymore")
    boolean isAddLinksPreParserEnabled();

    void setAddLinksPreParserEnabled(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("Automatically search for possible extraction password in pasted text and show them in 'Add Links' dialog")
    @DefaultBooleanValue(true)
    boolean isAddLinksPreParserAutoExtractionPasswordSearchEnabled();

    void setAddLinksPreParserAutoExtractionPasswordSearchEnabled(boolean b);

    @AboutConfig
    @RequiresRestart("A JDownloader Restart is Required")
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("Selecting Views in Linkgrabber Sidebar autoselects the matching links in the table. Set this to false to avoid this.")
    boolean isQuickViewSelectionEnabled();

    void setQuickViewSelectionEnabled(boolean b);

    @DescriptionForConfigEntry("If set, the addlinks dialog has this text. Use it for debug reasons.")
    @AboutConfig
    @MultiLineString
    String getPresetDebugLinks();

    void setPresetDebugLinks(String text);

    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry("Set to false to hide the 'Add Downloads' Context Menu Action in Linkgrabber")
    boolean isContextMenuAddLinksActionAlwaysVisible();

    void setContextMenuAddLinksActionAlwaysVisible(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("If true, Offline Links, that do not fit in a existing package, will be moved to a offline package.")
    boolean isOfflinePackageEnabled();

    void setOfflinePackageEnabled(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("If true, Linkcollector will create an extra package for each multipart or *.rar  archive")
    boolean isArchivePackagizerEnabled();

    void setArchivePackagizerEnabled(boolean b);

    @AboutConfig
    @DefaultIntValue(1)
    @DescriptionForConfigEntry("If >0, there will be no packages with * or less links")
    @SpinnerValidator(min = 0, max = Integer.MAX_VALUE)
    int getVariousPackageLimit();

    void setVariousPackageLimit(int b);

    @AboutConfig
    @RequiresRestart("A JDownloader Restart is Required")
    @DescriptionForConfigEntry("AutoConfirm waits a delay before confirming the links. Default is 15000ms")
    @DefaultIntValue(15000)
    @SpinnerValidator(min = 1, max = Integer.MAX_VALUE)
    int getAutoConfirmDelay();

    void setAutoConfirmDelay(int delay);

    @AboutConfig
    @RequiresRestart("A JDownloader Restart is Required")
    @DescriptionForConfigEntry("AutoConfirm waits max delay in ms after first link has been added before confirming the links. Default is -1 = wait for min delay")
    @DefaultIntValue(-1)
    @SpinnerValidator(min = -1, max = Integer.MAX_VALUE)
    int getAutoConfirmMaxDelay();

    void setAutoConfirmMaxDelay(int delay);

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("If true, Plugins will try to correct filenames to match to others. For example in split archives.")
    boolean isAutoFilenameCorrectionEnabled();

    void setAutoFilenameCorrectionEnabled(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("If true, the Linkcollector asks the Hosterplugins to filter the packageidentifier. This helps to map corrupt filenames into the correct packages.")
    boolean isAutoPackageMatchingCorrectionEnabled();

    void setAutoPackageMatchingCorrectionEnabled(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("Define the Pattern that is used to create Packagename created by SplitPackages! {PACKAGENAME,HOSTNAME}")
    @DefaultStringValue("{PACKAGENAME}-{HOSTNAME}")
    String getSplitPackageNameFactoryPattern();

    void setSplitPackageNameFactoryPattern(String b);

    @AboutConfig
    @DefaultBooleanValue(false)
    boolean isSplitPackageMergeEnabled();

    void setSplitPackageMergeEnabled(boolean b);

    @AboutConfig
    @DefaultBooleanValue(false)
    boolean isLinkgrabberAddAtTop();

    void setLinkgrabberAddAtTop(boolean selected);

    @AboutConfig
    @DefaultBooleanValue(true)
    boolean isLinkgrabberAutoStartEnabled();

    void setLinkgrabberAutoStartEnabled(boolean selected);

    @AboutConfig
    @DefaultBooleanValue(false)
    boolean isLinkgrabberAutoConfirmEnabled();

    void setLinkgrabberAutoConfirmEnabled(boolean selected);

    @DefaultBooleanValue(true)
    @AboutConfig
    @RequiresRestart("A JDownloader Restart is Required")
    @org.appwork.storage.config.annotations.DescriptionForConfigEntry("Retain filtered links and provide a restore button for them. When disabled, filtered links will not be stored.")
    boolean isRestoreButtonEnabled();

    void setRestoreButtonEnabled(boolean b);

    @DefaultBooleanValue(true)
    @AboutConfig
    @DescriptionForConfigEntry("If true, the 'Add Links' dialog in Linkgrabber will be filled with current clipboard content")
    boolean isAutoFillAddLinksDialogWithClipboardContentEnabled();

    void setAutoFillAddLinksDialogWithClipboardContentEnabled(boolean b);

    @DefaultBooleanValue(true)
    @AboutConfig
    @DescriptionForConfigEntry("If true, the 'Add Links' dialog in Linkgrabber will come up with the 'Overwrite Packagizer rules' checkbox checked.")
    boolean isAddLinksDialogOverwritesPackagizerRulesEnabled();

    void setAddLinksDialogOverwritesPackagizerRulesEnabled(boolean b);

    @DefaultBooleanValue(true)
    @AboutConfig
    boolean isAutoStartConfirmSidebarFilterEnabled();

    void setAutoStartConfirmSidebarFilterEnabled(boolean b);

    @AboutConfig
    @DefaultEnumValue("ASK")
    OnOfflineLinksAction getDefaultOnAddedOfflineLinksAction();

    void setDefaultOnAddedOfflineLinksAction(OnOfflineLinksAction value);

    @AboutConfig
    @DefaultEnumValue("ASK")
    @DescriptionForConfigEntry("Defines what to do when user tries to add items from linkgrabber to downloadlist that are already in downloadlist.")
    OnDupesLinksAction getDefaultOnAddedDupesLinksAction();

    void setDefaultOnAddedDupesLinksAction(OnDupesLinksAction value);

    @AboutConfig
    @DefaultIntValue(25)
    @RequiresRestart("A JDownloader Restart is Required")
    @DescriptionForConfigEntry("How many entries will be in the Packagename quick selection")
    @SpinnerValidator(min = 0, max = 250)
    int getPackageNameHistoryLength();

    void setPackageNameHistoryLength(int i);

    @AboutConfig
    @DefaultEnumValue("AUTO")
    void setAutoConfirmManagerAutoStart(AutoStartOptions selectedItem);

    AutoStartOptions getAutoConfirmManagerAutoStart();

    @AboutConfig
    @DefaultEnumValue("GLOBAL")
    void setAutoConfirmManagerHandleOffline(OnOfflineLinksAction selectedItem);

    OnOfflineLinksAction getAutoConfirmManagerHandleOffline();

    @AboutConfig
    @DefaultBooleanValue(false)
    void setAutoConfirmManagerForceDownloads(boolean selected);

    boolean isAutoConfirmManagerForceDownloads();

    @AboutConfig
    @DefaultBooleanValue(false)
    void setAutoConfirmManagerAssignPriorityEnabled(boolean selected);

    boolean isAutoConfirmManagerAssignPriorityEnabled();

    @AboutConfig
    @DefaultEnumValue("DEFAULT")
    void setAutoConfirmManagerPriority(Priority selectedItem);

    Priority getAutoConfirmManagerPriority();

    @AboutConfig
    @DefaultBooleanValue(false)
    void setAutoConfirmManagerClearListAfterConfirm(boolean selected);

    boolean isAutoConfirmManagerClearListAfterConfirm();

    @AboutConfig
    void setHandleOfflineOnConfirmLatestSelection(OnOfflineLinksAction handleOfflineLoc);

    OnOfflineLinksAction getHandleOfflineOnConfirmLatestSelection();

    @AboutConfig
    void setHandleIncompleteArchiveOnConfirmLatestSelection(ConfirmIncompleteArchiveAction handleOfflineLoc);

    ConfirmIncompleteArchiveAction getHandleIncompleteArchiveOnConfirmLatestSelection();

    @AboutConfig
    @DescriptionForConfigEntry("Defines pre-selected default action in dialog that shows up when user tries to add items from linkgrabber to downloadlist that are already in downloadlist.")
    void setHandleDupesOnConfirmLatestSelection(OnDupesLinksAction handleDupesLoc);

    OnDupesLinksAction getHandleDupesOnConfirmLatestSelection();

    @AboutConfig
    @DescriptionForConfigEntry("Analyze content in the links field 'Add Links' dialog before button \"continue\" is enabled.")
    @DefaultBooleanValue(true)
    boolean getAddLinksDialogCheck();

    void setAddLinksDialogCheck(boolean i);

    @AboutConfig
    @DescriptionForConfigEntry("if this value is >0, the subfolder option only will be used if the package contains more than subfolderThreshold value links")
    @RequiresRestart("A JDownloader Restart is Required")
    @DefaultIntValue(0)
    int getSubfolderThreshold();

    void setSubfolderThreshold(int i);

    @AboutConfig
    @DefaultEnumValue("NAMES")
    @RequiresRestart("A JDownloader Restart is Required")
    void setSubfolderCount(SubFolderByPackageRule.COUNT count);

    SubFolderByPackageRule.COUNT getSubfolderCount();
}
