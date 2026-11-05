package jd.gui.swing.jdgui.components.toolbar.actions;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.Icon;

import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.downloadcontroller.DownloadLinkCandidate;
import jd.controlling.downloadcontroller.DownloadLinkCandidateResult;
import jd.controlling.downloadcontroller.DownloadSession;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.DownloadWatchDogJob;
import jd.controlling.downloadcontroller.DownloadWatchDogProperty;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.controlling.downloadcontroller.event.DownloadWatchdogListener;
import jd.controlling.linkcollector.DefaultLinkCollectorListener;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcollector.LinkCollector.ConfirmLinksSettings;
import jd.controlling.linkcollector.LinkCollector.ConfirmLinksSettings.SwitchToDownloadlistBehavior;
import jd.controlling.linkcollector.LinkCollector.MoveLinksMode;
import jd.controlling.linkcollector.LinkCollectorEvent;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.JDGui.Panels;
import jd.gui.swing.jdgui.MainTabbedPane;
import jd.gui.swing.jdgui.interfaces.View;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.swing.components.ExtButton;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.controlling.download.DefaultDownloadControllerListener;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.event.GUIEventSender;
import org.jdownloader.gui.event.GUIListener;
import org.jdownloader.gui.toolbar.action.AbstractToolBarAction;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberView;
import org.jdownloader.gui.views.linkgrabber.contextmenu.ConfirmLinksContextAction;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.StartButtonAction;
import org.jdownloader.settings.staticreferences.CFG_GUI;
import org.jdownloader.translate._JDT;

public class StartDownloadsAction extends AbstractToolBarAction implements DownloadWatchdogListener, GUIListener, GenericConfigEventListener<Enum>, ActionContext {
    private final DelayedRunnable delayedUpdateEnableState;

    /**
     * Create a new instance of StartDownloadsAction. This is a singleton class. Access the only existing instance by using
     * {@link #getInstance()}.
     */
    public StartDownloadsAction() {
        setName(_JDT.T.StartDownloadsAction_createTooltip_());
        if (!isValidationMode()) {
            delayedUpdateEnableState = new DelayedRunnable(100) {
                final AtomicLong jobID = new AtomicLong(0);

                @Override
                public void delayedrun() {
                    final long finalJobID = jobID.incrementAndGet();
                    System.out.println("update " + finalJobID + " " + StartDownloadsAction.this);
                    DownloadWatchDog.getInstance().enqueueJob(new DownloadWatchDogJob() {
                        @Override
                        public boolean isHighPriority() {
                            return true;
                        }

                        @Override
                        public void interrupt() {
                        }

                        @Override
                        public void execute(final DownloadSession session) {
                            if (finalJobID != jobID.get()) {
                                return;
                            }
                            System.out.println("update 1:" + finalJobID + "\t" + jobID.get() + " " + StartDownloadsAction.this);
                            final boolean downloadListHasContent = DownloadController.getInstance().size() > 0;
                            final boolean linkgrabberHasContent = LinkCollector.getInstance().size() > 0;
                            final boolean isRunning = DownloadWatchDog.getInstance().isRunning();
                            /*
                             * Size lookups take read locks, so they are done here and not in the EDT
                             */
                            new EDTRunner() {
                                @Override
                                protected void runInEDT() {
                                    if (finalJobID != jobID.get()) {
                                        return;
                                    }
                                    System.out.println("update 2 :" + finalJobID + "\t" + jobID.get() + " " + StartDownloadsAction.this);
                                    boolean enable = (!isRunning);
                                    Icon newSmall = normalSmall;
                                    if (session != null && isRunning) {
                                        enable |= session.isForcedOnlyModeEnabled();
                                        if (session.isForcedOnlyModeEnabled()) {
                                            newSmall = forcedSmall;
                                        }
                                    }
                                    if (newSmall != smallIcon) {
                                        if (newSmall == forcedSmall) {
                                            smallIcon = forcedSmall;
                                            largeIcon = forcedLarge;
                                        } else {
                                            smallIcon = normalSmall;
                                            largeIcon = normalLarge;
                                        }
                                        firePropertyChange(Action.SMALL_ICON, new Object(), new Object());
                                        firePropertyChange(Action.LARGE_ICON_KEY, new Object(), new Object());
                                    }
                                    if (smallIcon == forcedSmall) {
                                        setTooltipText(_JDT.T.StartDownloadsAction_forced_createTooltip_());
                                    } else {
                                        setTooltipText(_JDT.T.StartDownloadsAction_createTooltip_());
                                    }
                                    if (enable) {
                                        enable = isStartPossible(JDGui.getInstance().getMainTabbedPane().getSelectedView(), downloadListHasContent, linkgrabberHasContent);
                                    }
                                    setEnabled(enable);
                                    if (isHideIfDownloadsAreRunning()) {
                                        setVisible(enable);
                                    }
                                }
                            };
                        }
                    });
                };
            };
            DownloadWatchDog.getInstance().getEventSender().addListener(this, true);
            DownloadWatchDog.getInstance().notifyCurrentState(this);
            CFG_GUI.START_BUTTON_ACTION_IN_LINKGRABBER_CONTEXT.getEventSender().addListener(this, true);
            GUIEventSender.getInstance().addListener(this, true);
            /* Needed to update the enabled state when the downloadlist/linkgrabber becomes empty or gets filled */
            DownloadController.getInstance().getEventSender().addListener(downloadListListener, true);
            LinkCollector.getInstance().getEventsender().addListener(linkgrabberListener, true);
            onGuiMainTabSwitch(null, MainTabbedPane.getInstance().getSelectedView());
        } else {
            delayedUpdateEnableState = null;
        }
        setAccelerator(KeyEvent.VK_S);
    }

    /**
     * Re-evaluates the enabled state when packages are added to or removed from the downloadlist, because an empty downloadlist disables
     * the button. All other events are irrelevant here.<br>
     * Held in a field because the listeners are registered weakly and would otherwise be garbage collected.
     */
    private final DefaultDownloadControllerListener downloadListListener = new DefaultDownloadControllerListener() {

        @Override
        public void onDownloadControllerAddedPackage(FilePackage pkg) {
            updateEnableState(true, false);
        }

        @Override
        public void onDownloadControllerStructureRefresh() {
            /*
             * The initial load of the saved downloadlist only fires a
             * structure refresh, no "package added" events
             */
             updateEnableState(null, null);
        }

        @Override
        public void onDownloadControllerRemovedPackage(FilePackage pkg) {
            updateEnableState(false, null);
        }

        @Override
        public void onDownloadControllerRemovedLinklist(List<DownloadLink> list) {
            updateEnableState(false, null);
        }

    };
    /**
     * Re-evaluates the enabled state when content is added to or removed from the linkgrabber, because in the linkgrabber context the
     * button is only disabled if both the linkgrabber and the downloadlist are empty. All other events are irrelevant here.<br>
     * Held in a field because the listeners are registered weakly and would otherwise be garbage collected.
     */
    private final DefaultLinkCollectorListener      linkgrabberListener  = new DefaultLinkCollectorListener() {

        @Override
        public void onLinkCollectorStructureRefresh(LinkCollectorEvent event) {
            /*
             * Covers structure changes that are not reported as
             * content added/removed, e.g. when the linkgrabber list
             * gets loaded
             */
            updateEnableState(null, null);
        }

        @Override
        public void onLinkCollectorContentRemoved(LinkCollectorEvent event) {
            updateEnableState(null, false);
        }

        @Override
        public void onLinkCollectorContentAdded(LinkCollectorEvent event) {
            updateEnableState(null, true);
        }

    };

    @Override
    public void initContextDefaults() {
        setHideIfDownloadsAreRunning(false);
    }

    @Override
    public void onKeyModifier(int parameter) {
    }

    public void actionPerformed(final ActionEvent e) {
        if (JDGui.getInstance().isCurrentPanel(Panels.LINKGRABBER)) {
            switch (CFG_GUI.CFG.getStartButtonActionInLinkgrabberContext()) {
            case ADD_ALL_LINKS_AND_START_DOWNLOADS:
                LinkGrabberTable.getInstance().getSelectionInfo(new SelectionInfoCallback<CrawledPackage, CrawledLink>() {

                    @Override
                    public void onSelectionInfo(SelectionInfo<CrawledPackage, CrawledLink> selectionInfo) {
                        final ConfirmLinksSettings cls = new ConfirmLinksSettings(MoveLinksMode.MANUAL);
                        cls.setAutoStartDownloads(true);
                        cls.setClearLinkgrabberlistOnConfirm(false);
                        cls.setSwitchToDownloadlistBehavior(SwitchToDownloadlistBehavior.ALWAYS);
                        cls.setForceDownloads(false);
                        ConfirmLinksContextAction.confirmSelection(selectionInfo, cls);
                    }

                    @Override
                    public boolean isCancelled() {
                        return false;
                    }
                }, SelectionType.ALL);

                break;
            case START_DOWNLOADS_ONLY:
                DownloadWatchDog.getInstance().startDownloads();
                break;
            case DISABLED:
                return;
            }
        } else {
            DownloadWatchDog.getInstance().startDownloads();
        }
        final DownloadSession session = DownloadWatchDog.getInstance().getSession();
        if (session != null && session.isForcedOnlyModeEnabled()) {
            DownloadWatchDog.getInstance().enqueueJob(new DownloadWatchDogJob() {
                @Override
                public void interrupt() {
                }

                @Override
                public void execute(DownloadSession currentSession) {
                    currentSession.setForcedOnlyModeEnabled(false);
                    updateEnableState(null, null);
                }

                @Override
                public boolean isHighPriority() {
                    return true;
                }
            });
        }
    }

    @Override
    public String createTooltip() {
        final DownloadSession session = DownloadWatchDog.getInstance().getSession();
        if (session != null && session.isForcedOnlyModeEnabled()) {
            return _JDT.T.StartDownloadsAction_forced_createTooltip_();
        }
        return _JDT.T.StartDownloadsAction_createTooltip_();
    }

    private boolean            hideIfDownloadsAreRunning     = false;
    public static final String HIDE_IF_DOWNLOADS_ARE_RUNNING = "HideIfDownloadsAreRunning";

    public static String getHideIfDownloadsAreRunningTranslation() {
        return _JDT.T.StartDownloadsAction_getHideIfDownloadsAreRunningTranslation_();
    }

    @Customizer(link = "#getHideIfDownloadsAreRunningTranslation")
    public boolean isHideIfDownloadsAreRunning() {
        return hideIfDownloadsAreRunning;
    }

    public void setHideIfDownloadsAreRunning(boolean showIfDownloadsAreRunning) {
        this.hideIfDownloadsAreRunning = showIfDownloadsAreRunning;
        updateEnableState(null, null);
    }

    @Override
    public void onDownloadWatchdogDataUpdate() {
    }

    @Override
    public void onDownloadWatchdogStateIsIdle() {
        updateEnableState(null, null);
    }

    @Override
    public String getIconKey() {
        return null;
    }

    /**
     * Returns whether the button has anything to do in the given view. The list contents are passed in so that the (lock-guarded) size
     * lookups can be done outside the EDT.<br>
     * The button has a different function depending on the view:
     * <ul>
     * <li>Downloadlist (and every other view): it starts the downloads, so it is only useful if the downloadlist is not empty.</li>
     * <li>Linkgrabber: its function is configurable ({@link StartButtonAction}). It is never useful if that function is disabled. Otherwise
     * it can either start the downloads or first add the linkgrabber links to the downloadlist and start them, so it is useful if the
     * downloadlist or the linkgrabber has content.</li>
     * </ul>
     * Note: A list that only contains disabled links/packages still counts as having content.
     */
    private boolean isStartPossible(final View view, final boolean downloadListHasContent, final boolean linkgrabberHasContent) {
        if (!(view instanceof LinkGrabberView)) {
            /* Downloadlist or any other view: the button can only start the existing downloads */
            return downloadListHasContent;
        }
        switch (CFG_GUI.CFG.getStartButtonActionInLinkgrabberContext()) {
        case DISABLED:
            /* The user disabled the start button function in the linkgrabber, regardless of any content */
            return false;
        default:
            /*
             * The button can still add the linkgrabber links to the downloadlist and start them, so linkgrabber content is enough even if
             * the downloadlist is empty
             */
            return downloadListHasContent || linkgrabberHasContent;
        }
    }

    private void updateEnableState(final Boolean downloadListEvent, final Boolean linkCollectorEvent) {
        final DelayedRunnable delayedUpdateEnableState = this.delayedUpdateEnableState;
        if (delayedUpdateEnableState == null) {
            return;
        }
        delayedUpdateEnableState.resetAndStart();
    }

    private final Icon normalSmall = new AbstractIcon(IconKey.ICON_MEDIA_PLAYBACK_START, 18);
    private final Icon forcedSmall = new AbstractIcon(IconKey.ICON_PLAY_BREAKUP_FORCED_ONLY, 18); ;
    private final Icon normalLarge = new AbstractIcon(IconKey.ICON_MEDIA_PLAYBACK_START, 24);
    private final Icon forcedLarge = new AbstractIcon(IconKey.ICON_PLAY_BREAKUP_FORCED_ONLY, 24); ;
    private Icon       smallIcon   = normalSmall;
    private Icon       largeIcon   = normalLarge;

    /**
     * @return
     */
    protected Icon getSmallIconForToolbar() {
        return smallIcon;
    }

    /**
     * @return
     */
    protected Icon getLargeIconForToolbar() {
        return largeIcon;
    }

    @Override
    public Object getValue(String key) {
        return super.getValue(key);
    }

    @Override
    public AbstractButton createButton() {
        ExtButton bt = new ExtButton(this);
        bt.setHideActionText(true);
        return bt;
    }

    @Override
    public void onDownloadWatchdogStateIsPause() {
    }

    @Override
    public void onDownloadWatchdogStateIsRunning() {
        updateEnableState(null, null);
    }

    @Override
    public void onDownloadWatchdogStateIsStopped() {
        updateEnableState(null, null);
    }

    @Override
    public void onDownloadWatchdogStateIsStopping() {
    }

    @Override
    public void onGuiMainTabSwitch(View oldView, final View newView) {
        DownloadWatchDog.getInstance().notifyCurrentState(this);
    }

    @Override
    public void onConfigValidatorError(KeyHandler<Enum> keyHandler, Enum invalidValue, ValidationException validateException) {
    }

    @Override
    public void onConfigValueModified(KeyHandler<Enum> keyHandler, Enum newValue) {
        onGuiMainTabSwitch(null, MainTabbedPane.getInstance().getSelectedView());
    }

    @Override
    public void onDownloadControllerStart(SingleDownloadController downloadController, DownloadLinkCandidate candidate) {
    }

    @Override
    public void onDownloadControllerStopped(SingleDownloadController downloadController, DownloadLinkCandidate candidate, DownloadLinkCandidateResult result) {
    }

    @Override
    public void onDownloadWatchDogPropertyChange(DownloadWatchDogProperty propertyChange) {
    }
}
