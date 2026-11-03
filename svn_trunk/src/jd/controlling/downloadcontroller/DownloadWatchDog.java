//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.controlling.downloadcontroller;

import java.io.File;
import java.io.IOException;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import jd.controlling.AccountController;
import jd.controlling.AccountControllerEvent;
import jd.controlling.AccountControllerListener;
import jd.controlling.TaskQueue;
import jd.controlling.captcha.CaptchaSettings;
import jd.controlling.downloadcontroller.AccountCache.CachedAccount;
import jd.controlling.downloadcontroller.BadFilePathException.PathFailureReason;
import jd.controlling.downloadcontroller.DiskSpaceManager.DISKSPACERESERVATIONRESULT;
import jd.controlling.downloadcontroller.DownloadLinkCandidateResult.RESULT;
import jd.controlling.downloadcontroller.DownloadLinkCandidateSelector.CachedAccountPermission;
import jd.controlling.downloadcontroller.DownloadLinkCandidateSelector.DownloadLinkCandidatePermission;
import jd.controlling.downloadcontroller.DownloadSession.STOPMARK;
import jd.controlling.downloadcontroller.DownloadSession.SessionState;
import jd.controlling.downloadcontroller.FilePathChecker.CheckFlag;
import jd.controlling.downloadcontroller.IfFilenameTooLongDialog.IfFilenameTooLongAction;
import jd.controlling.downloadcontroller.ProxyInfoHistory.WaitingSkipReasonContainer;
import jd.controlling.downloadcontroller.event.DownloadWatchdogEvent;
import jd.controlling.downloadcontroller.event.DownloadWatchdogEventSender;
import jd.controlling.downloadcontroller.event.DownloadWatchdogListener;
import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcollector.LinkOrigin;
import jd.controlling.linkcollector.LinkOriginDetails;
import jd.controlling.linkcollector.LinknameCleaner;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNodeFilter;
import jd.controlling.proxy.AbstractProxySelectorImpl;
import jd.controlling.proxy.ProxyController;
import jd.controlling.proxy.ProxyEvent;
import jd.controlling.reconnect.Reconnecter;
import jd.controlling.reconnect.Reconnecter.ReconnectResult;
import jd.controlling.reconnect.ReconnecterEvent;
import jd.controlling.reconnect.ReconnecterListener;
import jd.controlling.reconnect.ipcheck.IPController;
import jd.gui.UserIO;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.WarnLevel;
import jd.http.Browser.BlockedByException;
import jd.http.NoGateWayException;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.CandidateResultProvider;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.DownloadLinkProperty;
import jd.plugins.FilePackage;
import jd.plugins.FilePackageProperty;
import jd.plugins.LinkStatus;
import jd.plugins.MultiHostHost;
import jd.plugins.ParsedFilename;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.PluginsC;
import jd.plugins.download.DownloadInterface;
import jd.plugins.download.Downloadable;
import jd.plugins.download.HashInfo;
import jd.plugins.download.HashResult;
import jd.plugins.download.raf.FileBytesCache;

import org.appwork.controlling.State;
import org.appwork.controlling.StateEvent;
import org.appwork.controlling.StateEventListener;
import org.appwork.controlling.StateMachine;
import org.appwork.controlling.StateMachineInterface;
import org.appwork.exceptions.WTFException;
import org.appwork.scheduler.DelayedRunnable;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.shutdown.ShutdownVetoException;
import org.appwork.shutdown.ShutdownVetoListener;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.uio.ExceptionDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.ConcatIterator;
import org.appwork.utils.DebugMode;
import org.appwork.utils.NullsafeAtomicReference;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.event.DefaultEventListener;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.net.httpconnection.ProxyAuthException;
import org.appwork.utils.net.httpconnection.ProxyConnectException;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.speedmeter.SpeedMeterInterface.Resolution;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.ExceptionDialog;
import org.jdownloader.captcha.blacklist.CaptchaBlackList;
import org.jdownloader.controlling.DownloadLinkWalker;
import org.jdownloader.controlling.FileCreationEvent;
import org.jdownloader.controlling.FileCreationListener;
import org.jdownloader.controlling.FileCreationManager;
import org.jdownloader.controlling.FileCreationManager.DeleteOption;
import org.jdownloader.controlling.Priority;
import org.jdownloader.controlling.domainrules.DomainRuleController;
import org.jdownloader.controlling.domainrules.event.DomainRuleControllerListener;
import org.jdownloader.controlling.download.DownloadControllerListener;
import org.jdownloader.controlling.hosterrule.AccountUsageRule;
import org.jdownloader.controlling.hosterrule.HosterRuleController;
import org.jdownloader.controlling.hosterrule.HosterRuleControllerListener;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.downloads.action.ResetSettings;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.ConditionalSkipReason;
import org.jdownloader.plugins.ConditionalSkipReasonException;
import org.jdownloader.plugins.FinalLinkState;
import org.jdownloader.plugins.IgnorableConditionalSkipReason;
import org.jdownloader.plugins.MirrorLoading;
import org.jdownloader.plugins.SkipReason;
import org.jdownloader.plugins.SkipReasonException;
import org.jdownloader.plugins.TimeOutCondition;
import org.jdownloader.plugins.ValidatableConditionalSkipReason;
import org.jdownloader.plugins.WaitForAccountSkipReason;
import org.jdownloader.plugins.WaitWhileWaitingSkipReasonIsSet;
import org.jdownloader.plugins.WaitingSkipReason;
import org.jdownloader.plugins.WaitingSkipReason.CAUSE;
import org.jdownloader.plugins.controller.container.ContainerPluginController;
import org.jdownloader.settings.CleanAfterDownloadAction;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.settings.GeneralSettings.OnSkipDueToAlreadyExistsAction;
import org.jdownloader.settings.IfFileExistsAction;
import org.jdownloader.settings.MirrorDetectionDecision;
import org.jdownloader.settings.staticreferences.CFG_CAPTCHA;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;
import org.jdownloader.settings.staticreferences.CFG_RECONNECT;
import org.jdownloader.translate._JDT;
import org.jdownloader.utils.JDFileUtils;

public class DownloadWatchDog implements DownloadControllerListener, StateMachineInterface, ShutdownVetoListener, FileCreationListener {
    private class ReconnectThread extends Thread {
        private AtomicBoolean                        finished = new AtomicBoolean(false);
        private volatile Reconnecter.ReconnectResult result   = null;

        public Reconnecter.ReconnectResult waitForResult() throws InterruptedException {
            return waitForResult(-1);
        }

        public Reconnecter.ReconnectResult waitForResult(long timeout) throws InterruptedException {
            synchronized (finished) {
                if (finished.get()) {
                    return result;
                }
                if (timeout > 0) {
                    finished.wait(timeout);
                } else {
                    finished.wait();
                }
                if (!finished.get()) {
                    return Reconnecter.ReconnectResult.RUNNING;
                }
            }
            return result;
        }

        @Override
        public void run() {
            try {
                IPController.getInstance().invalidate();
                result = Reconnecter.getInstance().doReconnect();
            } finally {
                final Thread thread = Thread.currentThread();
                enqueueJob(new DownloadWatchDogJob() {
                    @Override
                    public boolean isHighPriority() {
                        return true;
                    }

                    @Override
                    public void interrupt() {
                    }

                    @Override
                    public void execute(DownloadSession currentSession) {
                        if (reconnectThread.compareAndSet(thread, null)) {
                            currentSession.compareAndSetSessionState(SessionState.RECONNECT_RUNNING, SessionState.NORMAL);
                        }
                    }
                });
                synchronized (finished) {
                    finished.set(true);
                    finished.notifyAll();
                }
            }
        }
    }

    public static final State                              IDLE_STATE            = new State("IDLE");
    public static final State                              RUNNING_STATE         = new State("RUNNING");
    public static final State                              PAUSE_STATE           = new State("PAUSE");
    public static final State                              STOPPING_STATE        = new State("STOPPING");
    public static final State                              STOPPED_STATE         = new State("STOPPED_STATE");
    static {
        IDLE_STATE.addChildren(RUNNING_STATE);
        RUNNING_STATE.addChildren(STOPPING_STATE, PAUSE_STATE);
        PAUSE_STATE.addChildren(RUNNING_STATE, STOPPING_STATE);
        STOPPING_STATE.addChildren(STOPPED_STATE);
    }
    protected final NullsafeAtomicReference<Thread>        currentWatchDogThread = new NullsafeAtomicReference<Thread>(null);
    protected final NullsafeAtomicReference<Thread>        reconnectThread       = new NullsafeAtomicReference<Thread>(null);
    protected final NullsafeAtomicReference<Thread>        tempWatchDogJobThread = new NullsafeAtomicReference<Thread>(null);
    protected NullsafeAtomicReference<DownloadWatchDogJob> currentWatchDogJob    = new NullsafeAtomicReference<DownloadWatchDogJob>(null);
    private final LinkedList<DownloadWatchDogJob>          watchDogJobs          = new LinkedList<DownloadWatchDogJob>();
    private final StateMachine                             stateMachine;
    private final DownloadSpeedManager                     dsm;
    private final GeneralSettings                          config;
    private final static DownloadWatchDog                  INSTANCE              = new DownloadWatchDog();
    private final LogSource                                logger;
    private final Object                                   WATCHDOGLOCK          = new Object();
    private final AtomicBoolean                            autoReconnectEnabled  = new AtomicBoolean(false);
    private final DownloadWatchdogEventSender              eventSender;
    private final boolean                                  isForceMirrorDetectionCaseInsensitive;

    public boolean isForceMirrorDetectionCaseInsensitive() {
        return isForceMirrorDetectionCaseInsensitive;
    }

    public DownloadWatchdogEventSender getEventSender() {
        return eventSender;
    }

    public static DownloadWatchDog getInstance() {
        return INSTANCE;
    }

    private DownloadWatchDog() {
        logger = LogController.CL();
        config = JsonConfig.create(GeneralSettings.class);
        isForceMirrorDetectionCaseInsensitive = config.isForceMirrorDetectionCaseInsensitive();
        eventSender = new DownloadWatchdogEventSender();
        DownloadSession initSession = new DownloadSession();
        session.set(initSession);
        this.dsm = new DownloadSpeedManager();
        /* speed limit */
        org.jdownloader.settings.staticreferences.CFG_GENERAL.DOWNLOAD_SPEED_LIMIT.getEventSender().addListener(new GenericConfigEventListener<Integer>() {
            public void onConfigValueModified(KeyHandler<Integer> keyHandler, Integer newValue) {
                updateDownloadSpeedManagerLimit();
            }

            public void onConfigValidatorError(KeyHandler<Integer> keyHandler, Integer invalidValue, ValidationException validateException) {
            }
        }, false);
        updateDownloadSpeedManagerLimit();
        /* speed limiter enabled? */
        org.jdownloader.settings.staticreferences.CFG_GENERAL.DOWNLOAD_SPEED_LIMIT_ENABLED.getEventSender().addListener(new GenericConfigEventListener<Boolean>() {
            public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
            }

            public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
                updateDownloadSpeedManagerLimit();
            }
        }, false);
        /* pause speed: while paused the effective limit follows the pause speed, so react to changes as well */
        org.jdownloader.settings.staticreferences.CFG_GENERAL.PAUSE_SPEED.getEventSender().addListener(new GenericConfigEventListener<Integer>() {
            public void onConfigValueModified(KeyHandler<Integer> keyHandler, Integer newValue) {
                updateDownloadSpeedManagerLimit();
            }

            public void onConfigValidatorError(KeyHandler<Integer> keyHandler, Integer invalidValue, ValidationException validateException) {
            }
        }, false);
        /* changes in max simultaneous downloads */
        org.jdownloader.settings.staticreferences.CFG_GENERAL.MAX_SIMULTANE_DOWNLOADS.getEventSender().addListener(new GenericConfigEventListener<Integer>() {
            @Override
            public void onConfigValueModified(KeyHandler<Integer> keyHandler, Integer newValue) {
                enqueueJob(new DownloadWatchDogJob() {
                    @Override
                    public void execute(DownloadSession currentSession) {
                        currentSession.refreshCandidates();
                    }

                    @Override
                    public void interrupt() {
                    }

                    @Override
                    public boolean isHighPriority() {
                        return false;
                    }
                });
            }

            @Override
            public void onConfigValidatorError(KeyHandler<Integer> keyHandler, Integer invalidValue, ValidationException validateException) {
            }
        }, false);
        CFG_CAPTCHA.CAPTCHA_MODE.getEventSender().addListener(new GenericConfigEventListener<Enum>() {
            @Override
            public void onConfigValueModified(KeyHandler<Enum> keyHandler, final Enum newValue) {
                enqueueJob(new DownloadWatchDogJob() {
                    @Override
                    public void execute(DownloadSession currentSession) {
                        currentSession.setCaptchaMode((CaptchaSettings.MODE) newValue);
                    }

                    @Override
                    public void interrupt() {
                    }

                    @Override
                    public boolean isHighPriority() {
                        return true;
                    }
                });
            }

            @Override
            public void onConfigValidatorError(KeyHandler<Enum> keyHandler, Enum invalidValue, ValidationException validateException) {
            }
        });
        initSession.setCaptchaMode(CFG_CAPTCHA.CFG.getCaptchaMode());
        /* changes in max simultaneous downloads per host */
        org.jdownloader.settings.staticreferences.CFG_GENERAL.MAX_SIMULTANE_DOWNLOADS_PER_HOST.getEventSender().addListener(new GenericConfigEventListener<Integer>() {
            @Override
            public void onConfigValueModified(KeyHandler<Integer> keyHandler, final Integer newValue) {
                if (!org.jdownloader.settings.staticreferences.CFG_GENERAL.MAX_DOWNLOADS_PER_HOST_ENABLED.isEnabled()) {
                    return;
                }
                enqueueJob(new DownloadWatchDogJob() {
                    @Override
                    public void execute(DownloadSession currentSession) {
                        if (newValue != null && org.jdownloader.settings.staticreferences.CFG_GENERAL.MAX_DOWNLOADS_PER_HOST_ENABLED.isEnabled()) {
                            currentSession.setMaxConcurrentDownloadsPerHost(newValue);
                        } else {
                            currentSession.setMaxConcurrentDownloadsPerHost(-1);
                        }
                        currentSession.refreshCandidates();
                    }

                    @Override
                    public void interrupt() {
                    }

                    @Override
                    public boolean isHighPriority() {
                        return false;
                    }
                });
            }

            @Override
            public void onConfigValidatorError(KeyHandler<Integer> keyHandler, Integer invalidValue, ValidationException validateException) {
            }
        }, false);
        DomainRuleController.getInstance().getEventSender().addListener(new DomainRuleControllerListener() {
            @Override
            public void onDomainRulesUpdated() {
                enqueueJob(new DownloadWatchDogJob() {
                    @Override
                    public void execute(DownloadSession currentSession) {
                        currentSession.refreshCandidates();
                    }

                    @Override
                    public void interrupt() {
                    }

                    @Override
                    public boolean isHighPriority() {
                        return false;
                    }
                });
            }
        });
        /* max simultaneous downloads per host enabled? */
        org.jdownloader.settings.staticreferences.CFG_GENERAL.MAX_DOWNLOADS_PER_HOST_ENABLED.getEventSender().addListener(new GenericConfigEventListener<Boolean>() {
            @Override
            public void onConfigValueModified(KeyHandler<Boolean> keyHandler, final Boolean newValue) {
                enqueueJob(new DownloadWatchDogJob() {
                    @Override
                    public void execute(DownloadSession currentSession) {
                        if (newValue != null && Boolean.TRUE.equals(newValue)) {
                            currentSession.setMaxConcurrentDownloadsPerHost(org.jdownloader.settings.staticreferences.CFG_GENERAL.MAX_SIMULTANE_DOWNLOADS_PER_HOST.getValue());
                        } else {
                            currentSession.setMaxConcurrentDownloadsPerHost(-1);
                        }
                        currentSession.refreshCandidates();
                    }

                    @Override
                    public void interrupt() {
                    }

                    @Override
                    public boolean isHighPriority() {
                        return false;
                    }
                });
            }

            @Override
            public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
            }
        }, false);
        if (org.jdownloader.settings.staticreferences.CFG_GENERAL.MAX_DOWNLOADS_PER_HOST_ENABLED.isEnabled()) {
            initSession.setMaxConcurrentDownloadsPerHost(org.jdownloader.settings.staticreferences.CFG_GENERAL.MAX_SIMULTANE_DOWNLOADS_PER_HOST.getValue());
        }
        /* auto reconnect enabled? */
        CFG_RECONNECT.AUTO_RECONNECT_ENABLED.getEventSender().addListener(new GenericConfigEventListener<Boolean>() {
            @Override
            public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
                autoReconnectEnabled.set(Boolean.TRUE.equals(newValue));
            }

            @Override
            public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
            }
        }, false);
        autoReconnectEnabled.set(CFG_RECONNECT.AUTO_RECONNECT_ENABLED.isEnabled());
        /* use accounts enabled? */
        CFG_GENERAL.USE_AVAILABLE_ACCOUNTS.getEventSender().addListener(new GenericConfigEventListener<Boolean>() {
            @Override
            public void onConfigValueModified(KeyHandler<Boolean> keyHandler, final Boolean newValue) {
                enqueueJob(new DownloadWatchDogJob() {
                    @Override
                    public void execute(DownloadSession currentSession) {
                        final boolean useAccounts = Boolean.TRUE.equals(newValue);
                        logger.info("USE_AVAILABLE_ACCOUNTS: " + useAccounts);
                        currentSession.setUseAccountsEnabled(useAccounts);
                        currentSession.removeAccountCache(null);
                    }

                    @Override
                    public void interrupt() {
                    }

                    @Override
                    public boolean isHighPriority() {
                        return true;
                    }
                });
            }

            @Override
            public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
            }
        }, false);
        final boolean useAccounts = CFG_GENERAL.USE_AVAILABLE_ACCOUNTS.isEnabled();
        logger.info("USE_AVAILABLE_ACCOUNTS: " + useAccounts);
        initSession.setUseAccountsEnabled(useAccounts);
        Reconnecter.getInstance().getEventSender().addListener(new ReconnecterListener() {
            @Override
            public void onBeforeReconnect(ReconnecterEvent event) {
            }

            @Override
            public void onAfterReconnect(final ReconnecterEvent event) {
                enqueueJob(new DownloadWatchDogJob() {
                    @Override
                    public void execute(DownloadSession currentSession) {
                        if (event.getResult() != ReconnectResult.SUCCESSFUL) {
                            return;
                        }
                        final ProxyInfoHistory proxyInfoHistory = currentSession.getProxyInfoHistory();
                        proxyInfoHistory.validate();
                        final List<WaitingSkipReasonContainer> reconnects = proxyInfoHistory.list(WaitingSkipReason.CAUSE.IP_BLOCKED, null);
                        if (reconnects == null) {
                            return;
                        }
                        for (WaitingSkipReasonContainer reconnect : reconnects) {
                            if (reconnect.getProxySelector().isReconnectSupported()) {
                                reconnect.invalidate();
                            }
                        }
                    }

                    @Override
                    public void interrupt() {
                    }

                    @Override
                    public boolean isHighPriority() {
                        return true;
                    }
                });
                if (isAutoReconnectEnabled() && Reconnecter.getFailedCounter() > 5) {
                    switch (event.getResult()) {
                    case FAILED:
                        CFG_RECONNECT.AUTO_RECONNECT_ENABLED.setValue(false);
                        UserIO.getInstance().requestMessageDialog(UserIO.DONT_SHOW_AGAIN | UserIO.DONT_SHOW_AGAIN_IGNORES_CANCEL, _JDT.T.jd_controlling_reconnect_Reconnector_progress_failed2());
                        break;
                    }
                }
            }
        }, false);
        stateMachine = new StateMachine(this, IDLE_STATE, STOPPED_STATE);
        startDownloadJobExecuter();
        stateMachine.addListener(new StateEventListener() {
            @Override
            public void onStateUpdate(StateEvent event) {
            }

            @Override
            public void onStateChange(StateEvent event) {
                /*
                 * recalculate the effective speed manager limit on every state change so that entering/leaving pause (through any path:
                 * pause toggle, stop, start) applies/removes the pause throttling
                 */
                updateDownloadSpeedManagerLimit();
                if (event.getNewState() == RUNNING_STATE) {
                    eventSender.fireEvent(new DownloadWatchdogEvent(this, DownloadWatchdogEvent.Type.STATE_RUNNING));
                } else if (event.getNewState() == STOPPED_STATE) {
                    eventSender.fireEvent(new DownloadWatchdogEvent(this, DownloadWatchdogEvent.Type.STATE_STOPPED));
                } else if (event.getNewState() == IDLE_STATE) {
                    eventSender.fireEvent(new DownloadWatchdogEvent(this, DownloadWatchdogEvent.Type.STATE_IDLE));
                } else if (event.getNewState() == PAUSE_STATE) {
                    eventSender.fireEvent(new DownloadWatchdogEvent(this, DownloadWatchdogEvent.Type.STATE_PAUSE));
                } else if (event.getNewState() == STOPPING_STATE) {
                    eventSender.fireEvent(new DownloadWatchdogEvent(this, DownloadWatchdogEvent.Type.STATE_STOPPING));
                }
            };
        });
        DownloadController.getInstance().addListener(this);
        ShutdownController.getInstance().addShutdownVetoListener(this);
        FileCreationManager.getInstance().getEventSender().addListener(this);
    }

    public final boolean isWatchDogThread() {
        final Thread current = Thread.currentThread();
        return current == getWatchDogThread();
    }

    private final Thread getWatchDogThread() {
        final Thread current = tempWatchDogJobThread.get();
        if (current != null) {
            return current;
        }
        return currentWatchDogThread.get();
    }

    /**
     * reset LinkStatus for files where it is useful and needed
     */
    private void clearDownloadListStatus() {
        /* reset skipReasons */
        DownloadController.getInstance().getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                DownloadController.getInstance().set(new DownloadLinkWalker() {
                    @Override
                    public void handle(DownloadLink link) {
                        link.setConditionalSkipReason(null);
                    }

                    @Override
                    public boolean accept(FilePackage fp) {
                        return true;
                    }

                    @Override
                    public boolean accept(DownloadLink link) {
                        return true;
                    }
                });
                return null;
            }
        });
    }

    /**
     * try to force a downloadstart, will ignore maxperhost and maxdownloads limits
     */
    public void forceDownload(final List<DownloadLink> linksForce) {
        if (linksForce == null || linksForce.size() == 0) {
            return;
        }
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                if (DownloadWatchDog.this.stateMachine.isState(STOPPING_STATE)) {
                    /*
                     * controller will shutdown soon or is paused, so no sense in forcing downloads now
                     */
                }
                unSkip(linksForce);
                if (DownloadWatchDog.this.stateMachine.isStartState() || DownloadWatchDog.this.stateMachine.isFinal()) {
                    /*
                     * no downloads are running, so we will force only the selected links to get started by setting stopmark to first forced
                     * link
                     */
                    // DownloadWatchDog.this.setStopMark(linksForce.get(0));
                    DownloadWatchDog.this.startDownloads(new Runnable() {
                        @Override
                        public void run() {
                            getSession().setForcedOnlyModeEnabled(true);
                            enqueueForcedDownloads(linksForce);
                        }
                    });
                } else {
                    enqueueForcedDownloads(linksForce);
                }
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return true;
            }
        });
    }

    private void enqueueForcedDownloads(final List<DownloadLink> linksForce) {
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                final Set<DownloadLink> oldList = new LinkedHashSet<DownloadLink>(currentSession.getForcedLinks());
                oldList.addAll(linksForce);
                currentSession.setForcedLinks(new CopyOnWriteArrayList<DownloadLink>(sortActivationRequests(oldList)));
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return true;
            }
        });
    }

    private List<DownloadLink> sortActivationRequests(Iterable<DownloadLink> links) {
        HashMap<Priority, java.util.List<DownloadLink>> optimizedList = new HashMap<Priority, java.util.List<DownloadLink>>();
        int count = 0;
        for (DownloadLink link : links) {
            count++;
            Priority prio = link.getPriorityEnum();
            if (Priority.DEFAULT.equals(prio)) {
                prio = link.getFilePackage().getPriorityEnum();
            }
            java.util.List<DownloadLink> list = optimizedList.get(prio);
            if (list == null) {
                list = new ArrayList<DownloadLink>();
                optimizedList.put(prio, list);
            }
            list.add(link);
        }
        List<DownloadLink> newList = new ArrayList<DownloadLink>(count);
        for (Priority prio : Priority.values()) {
            List<DownloadLink> ret = optimizedList.remove(prio);
            if (ret != null) {
                newList.addAll(ret);
            }
        }
        return newList;
    }

    /**
     * returns how many downloads are currently watched by this DownloadWatchDog
     *
     * @return
     */
    public int getActiveDownloads() {
        return getSession().getControllers().size();
    }

    /**
     * returns the ThrottledConnectionManager of this DownloadWatchDog
     *
     * @return
     */
    public DownloadSpeedManager getDownloadSpeedManager() {
        return dsm;
    }

    /** Result of the validation of a download destination. {@link #skipReason} is null if the destination is valid. */
    private static final class DestinationValidation {
        private final long       timestamp;
        private final SkipReason skipReason;

        private DestinationValidation(final long timestamp, final SkipReason skipReason) {
            this.timestamp = timestamp;
            this.skipReason = skipReason;
        }
    }

    private final HashMap<String, DestinationValidation> destinationValidationCache     = new HashMap<String, DestinationValidation>();
    /** How long the validation result of a download destination is reused, e.g. while many downloads are started in a row. */
    private static final long                            DESTINATION_VALIDATION_MAX_AGE = TimeUnit.SECONDS.toMillis(10);

    /**
     * Checks (without writing anything) if the given download destination is usable. The result is cached for a short time because this is
     * done for every started download and most of them share the same destination.
     *
     * @return null if the destination is valid, otherwise the reason why it is not
     */
    private SkipReason validateDestination(final String destination) {
        final long now = Time.systemIndependentCurrentJVMTimeMillis();
        synchronized (destinationValidationCache) {
            final DestinationValidation cached = destinationValidationCache.get(destination);
            if (cached != null && now - cached.timestamp <= DESTINATION_VALIDATION_MAX_AGE) {
                return cached.skipReason;
            }
        }
        SkipReason skipReason = null;
        try {
            FilePathChecker.validatePath(new File(destination));
        } catch (final BadFilePathException e) {
            switch (e.getReason()) {
            case PATH_TOO_LONG:
                skipReason = SkipReason.INVALID_DESTINATION_TOO_LONG_PATH;
                break;
            default:
                skipReason = SkipReason.INVALID_DESTINATION;
                break;
            }
        } catch (final IOException e) {
            /* e.g. canonicalization failure -> treat as invalid destination. */
            skipReason = SkipReason.INVALID_DESTINATION;
        }
        synchronized (destinationValidationCache) {
            if (destinationValidationCache.size() > 50) {
                /* Keep the cache small, it is only a short term optimization. */
                destinationValidationCache.clear();
            }
            destinationValidationCache.put(destination, new DestinationValidation(now, skipReason));
        }
        return skipReason;
    }

    private DownloadLinkCandidate next(final DownloadLinkCandidateSelector selector) {
        final DownloadSession currentSession = selector.getSession();
        while (newDLStartAllowed(currentSession)) {
            final List<DownloadLinkCandidate> nextCandidates = nextDownloadLinkCandidates(selector);
            if (nextCandidates == null || nextCandidates.size() == 0) {
                break;
            }
            DownloadLinkCandidate nextCandidate = findFinalCandidate(selector, nextCandidates);
            if (nextCandidate == null) {
                continue;
            }
            final String destination = nextCandidate.getLink().getFilePackage().getDownloadDirectory();
            final SkipReason invalidDestinationReason = validateDestination(destination);
            if (invalidDestinationReason != null) {
                for (final DownloadLinkCandidate candidate : nextCandidates) {
                    selector.addExcluded(candidate, new DownloadLinkCandidateResult(invalidDestinationReason, null, null));
                }
                continue;
            }
            if (DISKSPACERESERVATIONRESULT.FAILED.equals(validateDiskFree(nextCandidates))) {
                for (final DownloadLinkCandidate candidate : nextCandidates) {
                    selector.addExcluded(candidate, new DownloadLinkCandidateResult(SkipReason.DISK_FULL, null, null));
                }
                continue;
            }
            try {
                /*
                 * Let the plugin decide which of the (safe) mirrors should be downloaded instead of the selected candidate, e.g. to prefer
                 * the mirror with the best quality or the one that is available on the current account.
                 */
                if (PluginForHost.implementsSortDownloadLink(nextCandidate.getCachedAccount().getPlugin())) {
                    /* The originally selected link is always part of the list so the plugin may also decide to keep it. */
                    final ArrayList<DownloadLink> mirrors = new ArrayList<DownloadLink>();
                    mirrors.add(0, nextCandidate.getLink());
                    /* Only use 100% safe mirror detection here because we swap the link that is going to be downloaded. */
                    for (final DownloadLink mirror : findDownloadLinkMirrors(nextCandidate.getLink(), MirrorDetectionDecision.SAFE, false)) {
                        if (!nextCandidate.getCachedAccount().canHandle(mirror)) {
                            /* The account of the selected candidate cannot download this mirror -> it can never be the replacement. */
                            continue;
                        }
                        if (FinalLinkState.OFFLINE.equals(mirror.getFinalLinkState())) {
                            /* Mirror is known to be offline -> downloading it would fail. */
                            continue;
                        }
                        /* The mirror itself needs to be allowed to start (limits, captcha mode, ...), not the original link. */
                        final DownloadLinkCandidate candidate = new DownloadLinkCandidate(mirror, nextCandidate.isForced(), nextCandidate.getCachedAccount(), nextCandidate.getProxySelector(), nextCandidate.isCustomizedAccount());
                        switch (selector.getDownloadLinkCandidatePermission(candidate)) {
                        case OK:
                        case OK_FORCED:
                        case OK_SPEED_EXTENSION:
                            break;
                        default:
                            /* Mirror is currently not allowed to start (e.g. limit reached) -> do not offer it to the plugin. */
                            continue;
                        }
                        if (!selector.validateDownloadLinkCandidate(candidate)) {
                            /* Mirror does not pass the selector's validation -> do not offer it to the plugin. */
                            continue;
                        }
                        mirrors.add(mirror);
                    }
                    final List<DownloadLink> sortedMirrors = nextCandidate.getCachedAccount().getPlugin().sortDownloadLinks(nextCandidate.getCachedAccount().getAccount(), mirrors);
                    if (sortedMirrors != null && sortedMirrors.size() > 0 && sortedMirrors.get(0) != nextCandidate.getLink()) {
                        /* Plugin prefers another mirror -> download that one instead. */
                        nextCandidate = new DownloadLinkCandidate(sortedMirrors.get(0), nextCandidate.isForced(), nextCandidate.getCachedAccount(), nextCandidate.getProxySelector(), nextCandidate.isCustomizedAccount());
                    }
                }
            } catch (final Throwable e) {
                /* Mirror sorting is optional -> on any plugin error continue with the originally selected candidate. */
                logger.log(e);
            }
            selector.setExcluded(nextCandidate.getLink());
            /*
             * Every mirror (according to the configured mirror detection, including disabled links) must not be started on its own while
             * the chosen link is loading. Mark them with a MirrorLoading condition unless they already finished successfully.
             */
            final MirrorLoading condition = new MirrorLoading(nextCandidate.getLink());
            for (DownloadLink mirror : findDownloadLinkMirrors(nextCandidate.getLink(), config.getMirrorDetectionDecision(), true)) {
                selector.setExcluded(mirror);
                if (mirror.getFinalLinkState() == null || FinalLinkState.CheckFailed(mirror.getFinalLinkState())) {
                    mirror.setConditionalSkipReason(condition);
                }
            }
            return nextCandidate;
        }
        return null;
    }

    private DownloadLinkCandidate findFinalCandidate(DownloadLinkCandidateSelector selector, List<DownloadLinkCandidate> candidates) {
        if (candidates == null || candidates.size() == 0) {
            return null;
        }
        final LinkedHashMap<DownloadLink, LinkedHashMap<String, ArrayList<DownloadLinkCandidate>>> preferredFiltered = new LinkedHashMap<DownloadLink, LinkedHashMap<String, ArrayList<DownloadLinkCandidate>>>();
        for (DownloadLinkCandidate nextCandidate : candidates) {
            final DownloadLink candidateLink = nextCandidate.getLink();
            LinkedHashMap<String, ArrayList<DownloadLinkCandidate>> map = preferredFiltered.get(candidateLink);
            if (map == null) {
                map = new LinkedHashMap<String, ArrayList<DownloadLinkCandidate>>();
                preferredFiltered.put(candidateLink, map);
            }
            final String host = nextCandidate.getCachedAccount().getPlugin().getHost();
            ArrayList<DownloadLinkCandidate> list = map.get(host);
            if (list == null) {
                list = new ArrayList<DownloadLinkCandidate>();
                map.put(host, list);
            }
            if (list.isEmpty() || !list.get(0).isCustomizedAccount()) {
                list.add(nextCandidate);
            }
        }
        candidates = new ArrayList<DownloadLinkCandidate>();
        final Iterator<Entry<DownloadLink, LinkedHashMap<String, ArrayList<DownloadLinkCandidate>>>> it = preferredFiltered.entrySet().iterator();
        while (it.hasNext()) {
            final Entry<DownloadLink, LinkedHashMap<String, ArrayList<DownloadLinkCandidate>>> next = it.next();
            final DownloadLink downloadLink = next.getKey();
            final LinkedHashMap<String, ArrayList<DownloadLinkCandidate>> map = next.getValue();
            final Iterator<Entry<String, ArrayList<DownloadLinkCandidate>>> it2 = map.entrySet().iterator();
            while (it2.hasNext()) {
                final Entry<String, ArrayList<DownloadLinkCandidate>> next2 = it2.next();
                final ArrayList<DownloadLinkCandidate> list = next2.getValue();
                if (list.size() <= 1) {
                    /* nothing to sort */
                    candidates.addAll(list);
                    continue;
                }
                final ArrayList<DownloadLinkCandidate> newList = new ArrayList<DownloadLinkCandidate>();
                try {
                    final ArrayList<Account> accList = new ArrayList<Account>();
                    final LinkedHashMap<Account, DownloadLinkCandidate> accMap = new LinkedHashMap<Account, DownloadLinkCandidate>();
                    for (final DownloadLinkCandidate candidate : list) {
                        accMap.put(candidate.getCachedAccount().getAccount(), candidate);
                        accList.add(candidate.getCachedAccount().getAccount());
                    }
                    for (Account account : list.get(0).getCachedAccount().getPlugin().sortAccounts(downloadLink, accList)) {
                        final DownloadLinkCandidate candidate = accMap.remove(account);
                        if (candidate != null) {
                            newList.add(candidate);
                        }
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                }
                if (newList.size() == list.size()) {
                    candidates.addAll(newList);
                    continue;
                }
                candidates.addAll(list);
            }
        }
        final LinkedHashMap<DownloadLink, DownloadLinkCandidate> bestCandidates = new LinkedHashMap<DownloadLink, DownloadLinkCandidate>();
        for (final DownloadLinkCandidate nextCandidate : candidates) {
            final DownloadLink candidateLink = nextCandidate.getLink();
            final DownloadLinkCandidate bestCandidate = bestCandidates.get(candidateLink);
            if (bestCandidate == null) {
                /* no bestCandidate yet */
                bestCandidates.put(candidateLink, nextCandidate);
                continue;
            }
            if (bestCandidate.isCustomizedAccount()) {
                /* a customized account always wins, keep it */
                continue;
            }
            /* we have a bestCandidate, check if nextCandidate would be better */
            final boolean bestHasCaptcha = bestCandidate.getCachedAccount().hasCaptcha(candidateLink);
            final boolean nextHasCaptcha = nextCandidate.getCachedAccount().hasCaptcha(candidateLink);
            switch (bestCandidate.getCachedAccount().getType()) {
            case MULTI:
                /* our bestCandidate is a multihost one */
                switch (nextCandidate.getCachedAccount().getType()) {
                case ORIGINAL:
                    if (nextHasCaptcha == false) {
                        /* we always prefer originalAccount if it does not have a captcha */
                        bestCandidates.put(candidateLink, nextCandidate);
                    }
                    break;
                case MULTI:
                    if (bestHasCaptcha && nextHasCaptcha == false) {
                        /* we replace our bestCandidate because nextCandidate does not have a captcha */
                        bestCandidates.put(candidateLink, nextCandidate);
                    }
                    break;
                case NONE:
                    if (false && bestHasCaptcha && nextHasCaptcha == false) {
                        /* TODO */
                        /* disabled because needs to be discussed if we prefer captchaless none over captcha original/multihost */
                        /* we replace our bestCandidate with NONE because nextCandidate does not have a captcha */
                        bestCandidates.put(candidateLink, nextCandidate);
                    }
                    break;
                }
                break;
            case ORIGINAL:
                /* our bestCandidate is an original one */
                switch (nextCandidate.getCachedAccount().getType()) {
                case MULTI:
                case ORIGINAL:
                    if (bestHasCaptcha && nextHasCaptcha == false) {
                        /* we only replace originalAccount in case bestCandidate does have a captcha and nextCandidate does not */
                        bestCandidates.put(candidateLink, nextCandidate);
                    }
                    break;
                case NONE:
                    if (false && bestHasCaptcha && nextHasCaptcha == false) {
                        /* TODO */
                        /* disabled because needs to be discussed if we prefer captchaless none over captcha original/multihost */
                        /* we replace our bestCandidate with NONE because nextCandidate does not have a captcha */
                        bestCandidates.put(candidateLink, nextCandidate);
                    }
                    break;
                }
                break;
            case NONE:
                /* our bestCandidate is without an account */
                switch (nextCandidate.getCachedAccount().getType()) {
                case NONE:
                    if (bestHasCaptcha && nextHasCaptcha == false) {
                        /* we replace our bestCandidate because nextCandidate does not have a captcha */
                        bestCandidates.put(candidateLink, nextCandidate);
                    }
                    break;
                case MULTI:
                case ORIGINAL:
                    /* we always replace our account-less bestCandidate by original/multihost, no matter whether there is a captcha */
                    bestCandidates.put(candidateLink, nextCandidate);
                    break;
                }
                break;
            }
        }
        ArrayList<DownloadLinkCandidate> finalCandidates = new ArrayList<DownloadLinkCandidate>(bestCandidates.values());
        try {
            Collections.sort(finalCandidates, new Comparator<DownloadLinkCandidate>() {
                public int compareDown(boolean x, boolean y) {
                    return (x == y) ? 0 : (x ? 1 : -1);
                }

                @Override
                public int compare(DownloadLinkCandidate x, DownloadLinkCandidate y) {
                    final int ret = x.getCachedAccount().getType().compareTo(y.getCachedAccount().getType());
                    if (ret != 0) {
                        return ret;
                    }
                    final boolean xCaptcha = x.getCachedAccount().hasCaptcha(x.getLink());
                    final boolean yCaptcha = y.getCachedAccount().hasCaptcha(y.getLink());
                    return compareDown(xCaptcha, yCaptcha);
                }
            });
        } catch (final Throwable e) {
            logger.log(e);
        }
        return finalCandidates.get(0);
    }

    /**
     * Decides whether {@code mirrorCandidate} is a mirror of {@code linkCandidate}, i.e. both would result in the same file. </br> Decision
     * order: <br>
     * 1. A mirror ID provided by both plugins is authoritative. <br>
     * 2. Otherwise file name, file size and hash are combined depending on the given {@link MirrorDetectionDecision}.
     *
     * @param cachedLinkCandidateName
     *            Display name of linkCandidate to avoid recomputing it for every comparison; null to look it up here.
     */
    private boolean isMirrorCandidate(DownloadLink linkCandidate, String cachedLinkCandidateName, DownloadLink mirrorCandidate, MirrorDetectionDecision mirrorDetectionDecision) {
        if (MirrorDetectionDecision.DISABLED.equals(mirrorDetectionDecision)) {
            return false;
        }
        /* Plugins can tell us exactly which links are mirrors -> no further heuristics needed. */
        String cachedLinkMirrorID = linkCandidate.getDefaultPlugin().getMirrorID(linkCandidate);
        String mirrorCandidateMirrorID = mirrorCandidate.getDefaultPlugin().getMirrorID(mirrorCandidate);
        if (cachedLinkMirrorID != null && mirrorCandidateMirrorID != null) {
            return StringUtils.equals(cachedLinkMirrorID, mirrorCandidateMirrorID);
        }
        if (cachedLinkCandidateName == null) {
            cachedLinkCandidateName = linkCandidate.getView().getDisplayName();
        }
        final boolean sameName;
        String mirrorCandidateName = mirrorCandidate.getView().getDisplayName();
        /* Windows file names are case insensitive, on other systems this is optional. */
        if (CrossSystem.isWindows() || isForceMirrorDetectionCaseInsensitive()) {
            sameName = cachedLinkCandidateName.equalsIgnoreCase(mirrorCandidateName);
        } else {
            sameName = cachedLinkCandidateName.equals(mirrorCandidateName);
        }
        switch (mirrorDetectionDecision) {
        case DISABLED:
            return false;
        case SAFE: {
            /* Different (verified) size or different hash always rules out a mirror, otherwise the file name decides. */
            final Boolean sameSizeResult = hasSameSize(linkCandidate, mirrorCandidate, mirrorDetectionDecision);
            if (sameSizeResult != null && Boolean.FALSE.equals(sameSizeResult)) {
                return false;
            }
            final Boolean sameHashResult = hasSameHash(linkCandidate, mirrorCandidate);
            if (sameHashResult != null && Boolean.FALSE.equals(sameHashResult)) {
                return sameHashResult;
            }
            return sameName;
        }
        case AUTO: {
            /* Different size rules out a mirror. If both have a comparable hash, the hash decides, otherwise the file name. */
            final Boolean sameSizeResult = hasSameSize(linkCandidate, mirrorCandidate, mirrorDetectionDecision);
            if (sameSizeResult != null && Boolean.FALSE.equals(sameSizeResult)) {
                return false;
            }
            final Boolean sameHashResult = hasSameHash(linkCandidate, mirrorCandidate);
            if (sameHashResult != null) {
                return sameHashResult;
            }
            return sameName;
        }
        case FILENAME:
            return sameName;
        case FILENAME_FILESIZE: {
            /* Same name AND a positively confirmed same size; unknown sizes do not count as equal. */
            final Boolean sameSizeResult = hasSameSize(linkCandidate, mirrorCandidate, mirrorDetectionDecision);
            return sameName && sameSizeResult != null && Boolean.TRUE.equals(sameSizeResult);
        }
        }
        return false;
    }

    /**
     * Compares the file sizes of two links for mirror detection. </br> With {@link MirrorDetectionDecision#SAFE} or a configured equality
     * of 100% (10000) only verified sizes are compared. Otherwise the known sizes may differ by the configured tolerance (equality in
     * 1/10000, 10000 = identical).
     *
     * @return true/false if the sizes could be compared; null if at least one size is unknown and unknown sizes are not treated as a
     *         mismatch (config: isForceMirrorDetectionFileSizeCheck).
     */
    private Boolean hasSameSize(DownloadLink linkCandidate, DownloadLink mirrorCandidate, MirrorDetectionDecision mirrorDetectionDecision) {
        if (MirrorDetectionDecision.DISABLED.equals(mirrorDetectionDecision)) {
            return false;
        }
        final int fileSizeEquality = config.getMirrorDetectionFileSizeEquality();
        final long verifiedFileSizeA = linkCandidate.getView().getBytesTotalVerified();
        final long verifiedFileSizeB = mirrorCandidate.getView().getBytesTotalVerified();
        if (fileSizeEquality == 10000 || MirrorDetectionDecision.SAFE.equals(mirrorDetectionDecision)) {
            /* 100 percent sure, only use verifiedFileSizes */
            if (verifiedFileSizeA >= 0 && verifiedFileSizeB >= 0) {
                return verifiedFileSizeA == verifiedFileSizeB;
            }
        } else {
            /* we use knownDownloadSize for check */
            final long sizeA = linkCandidate.getView().getBytesTotal();
            final long sizeB = mirrorCandidate.getView().getBytesTotal();
            if (sizeA >= 0 && sizeB >= 0) {
                final long diff = Math.abs(sizeA - sizeB);
                final int maxDiffPercent = 10000 - Math.max(1, Math.min(9999, fileSizeEquality));
                final long maxDiff = (sizeA * maxDiffPercent) / 10000;
                return diff <= maxDiff;
            }
        }
        /* At least one size is unknown: Treat as mismatch if a size check is enforced, otherwise leave the decision to the caller. */
        if (config.isForceMirrorDetectionFileSizeCheck() || MirrorDetectionDecision.SAFE.equals(mirrorDetectionDecision)) {
            return false;
        }
        return null;
    }

    /**
     * @return true/false if both links have a hash of the same type that can be compared; null if no comparison is possible.
     */
    private Boolean hasSameHash(final DownloadLink linkCandidate, final DownloadLink mirrorCandidate) {
        final HashInfo hashInfoA = linkCandidate.getHashInfo();
        final HashInfo hashInfoB = hashInfoA != null ? mirrorCandidate.getHashInfo() : null;
        if (hashInfoA == null || hashInfoB == null || hashInfoA.getType() != hashInfoB.getType()) {
            return null;
        }
        return hashInfoA.equals(hashInfoB);
    }

    /**
     * Returns all mirrors of the given link. At the moment mirrors are only searched inside the package of the link.
     *
     * @param includeDisabledLinks
     *            true to also return disabled links
     */
    private List<DownloadLink> findDownloadLinkMirrors(final DownloadLink link, final MirrorDetectionDecision mirrorDetectionDecision, final boolean includeDisabledLinks) {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (MirrorDetectionDecision.DISABLED.equals(mirrorDetectionDecision)) {
            return ret;
        }
        final FilePackage fp = link.getFilePackage();
        final String name = link.getView().getDisplayName();
        fp.getModifyLock().runReadLock(new Runnable() {
            @Override
            public void run() {
                for (final DownloadLink mirror : fp.getChildren()) {
                    if (mirror != link && (includeDisabledLinks || mirror.isEnabled()) && isMirrorCandidate(link, name, mirror, mirrorDetectionDecision)) {
                        ret.add(mirror);
                    }
                }
            }
        });
        return ret;
    }

    private void printDownloadLinkCandidateHistory(DownloadLinkCandidate candidate) {
        if (candidate == null || candidate.getLink() == null || candidate.getCachedAccount() == null) {
            return;
        }
        try {
            final DownloadSession currentSession = getSession();
            final DownloadLinkCandidateHistory history = currentSession.getHistory(candidate.getLink());
            if (history == null) {
                return;
            }
            final List<DownloadLinkCandidateResult> results = history.getResults(candidate);
            if (results == null) {
                return;
            }
            for (final DownloadLinkCandidateResult result : results) {
                switch (result.getResult()) {
                case CONDITIONAL_SKIPPED:
                    logger.info(candidate + "->" + result.getResult() + "|" + result.getConditionalSkip());
                    break;
                case SKIPPED:
                    logger.info(candidate + "->" + result.getResult() + "|" + result.getSkipReason());
                    break;
                default:
                    logger.info(candidate + "->" + result.getResult());
                    break;
                }
            }
        } catch (final Throwable e) {
            logger.log(e);
        }
    }

    private void setFinalLinkStatus(DownloadLinkCandidate candidate, DownloadLinkCandidateResult value, SingleDownloadController singleDownloadController) {
        final DownloadLink link = candidate.getLink();
        final DownloadSession currentSession = getSession();
        final boolean onDetach = singleDownloadController != null;
        HashResult hashResult = null;
        if (onDetach) {
            hashResult = singleDownloadController.getHashResult();
        }
        switch (value.getResult()) {
        case CONDITIONAL_SKIPPED:
            logger.info(candidate + "->" + value.getResult() + "|" + value.getConditionalSkip());
            break;
        case SKIPPED:
            logger.info(candidate + "->" + value.getResult() + "|" + value.getSkipReason());
            break;
        default:
            logger.info(candidate + "->" + value.getResult());
            break;
        }
        ProxyInfoHistory proxyHistory = currentSession.getProxyInfoHistory();
        switch (value.getResult()) {
        case PROXY_UNAVAILABLE:
            if (!onDetach) {
                ConditionalSkipReason conditionalSkipReason = proxyHistory.getConditionalSkipReason(candidate);
                if (conditionalSkipReason != null) {
                    link.setConditionalSkipReason(conditionalSkipReason);
                }
                return;
            }
            break;
        case CONDITIONAL_SKIPPED:
            ConditionalSkipReason conditionalSkipReason = value.getConditionalSkip();
            if (conditionalSkipReason != null) {
                link.setConditionalSkipReason(conditionalSkipReason);
            }
            return;
        case IP_BLOCKED:
            if (onDetach) {
                long remainingTime = value.getRemainingTime();
                if (remainingTime > 0) {
                    proxyHistory.putIntoHistory(candidate, new WaitingSkipReason(CAUSE.IP_BLOCKED, remainingTime, value.getMessage()));
                }
                return;
            }
            break;
        case HOSTER_UNAVAILABLE:
            if (onDetach) {
                long remainingTime = value.getRemainingTime();
                if (remainingTime > 0) {
                    proxyHistory.putIntoHistory(candidate, new WaitingSkipReason(CAUSE.HOST_TEMP_UNAVAILABLE, remainingTime, value.getMessage()));
                }
                return;
            }
            break;
        case FILE_UNAVAILABLE:
            if (!onDetach) {
                long remaining = value.getRemainingTime();
                if (remaining > 0) {
                    link.setConditionalSkipReason(new WaitingSkipReason(CAUSE.FILE_TEMP_UNAVAILABLE, remaining, value.getMessage()));
                }
            }
            return;
        case CONNECTION_ISSUES:
            if (!onDetach) {
                long remaining = value.getRemainingTime();
                if (remaining > 0) {
                    link.setConditionalSkipReason(new WaitingSkipReason(CAUSE.CONNECTION_TEMP_UNAVAILABLE, remaining, value.getMessage()));
                }
            }
            return;
        case SKIPPED:
            if (SkipReason.NO_ACCOUNT.equals(value.getSkipReason())) {
                printDownloadLinkCandidateHistory(candidate);
            }
            currentSession.removeHistory(link);
            candidate.getLink().setSkipReason(value.getSkipReason());
            return;
        case PLUGIN_DEFECT:
            if (!onDetach) {
                currentSession.removeHistory(link);
                candidate.getLink().setFinalLinkState(FinalLinkState.PLUGIN_DEFECT);
            }
            return;
        case OFFLINE_TRUSTED:
            if (onDetach) {
                currentSession.removeHistory(link);
                candidate.getLink().setFinalLinkState(FinalLinkState.OFFLINE);
                return;
            }
            break;
        case FINISHED_EXISTS:
            if (onDetach) {
                currentSession.removeHistory(link);
                candidate.getLink().setFinishedDate(value.getFinishTime());
                candidate.getLink().setFinalLinkState(FinalLinkState.FINISHED_MIRROR);
                return;
            }
            break;
        case FINISHED:
            if (!onDetach) {
                break;
            }
            currentSession.removeHistory(link);
            candidate.getLink().setFinishedDate(value.getFinishTime());
            if (hashResult == null || !hashResult.match()) {
                /* No (matching) hash result -> plain finished state. */
                candidate.getLink().setFinalLinkState(FinalLinkState.FINISHED);
                return;
            }
            switch (hashResult.getHashInfo().getType()) {
            case WHIRLPOOL:
                candidate.getLink().setFinalLinkState(FinalLinkState.FINISHED_WHIRLPOOL);
                break;
            case CRC32:
                candidate.getLink().setFinalLinkState(FinalLinkState.FINISHED_CRC32);
                break;
            case CRC32C:
                candidate.getLink().setFinalLinkState(FinalLinkState.FINISHED_CRC32C);
                break;
            case MD5:
                candidate.getLink().setFinalLinkState(FinalLinkState.FINISHED_MD5);
                break;
            case SHA1:
                candidate.getLink().setFinalLinkState(FinalLinkState.FINISHED_SHA1);
                break;
            case SHA224:
                candidate.getLink().setFinalLinkState(FinalLinkState.FINISHED_SHA224);
                break;
            case SHA256:
                candidate.getLink().setFinalLinkState(FinalLinkState.FINISHED_SHA256);
                break;
            case SHA384:
                candidate.getLink().setFinalLinkState(FinalLinkState.FINISHED_SHA384);
                break;
            case SHA512:
                candidate.getLink().setFinalLinkState(FinalLinkState.FINISHED_SHA512);
                break;
            default:
                candidate.getLink().setFinalLinkState(FinalLinkState.FINISHED);
                break;
            }
            return;
        case FAILED_EXISTS:
            if (onDetach) {
                currentSession.removeHistory(link);
                candidate.getLink().setFinalLinkState(FinalLinkState.FAILED_EXISTS);
                return;
            }
            break;
        case FAILED:
            if (!onDetach) {
                break;
            }
            currentSession.removeHistory(link);
            if (hashResult == null || hashResult.match()) {
                /* No hash result or hash is fine -> plain failed state. */
                candidate.getLink().setFinalLinkState(FinalLinkState.FAILED);
                return;
            }
            switch (hashResult.getHashInfo().getType()) {
            case CRC32C:
                candidate.getLink().setFinalLinkState(FinalLinkState.FAILED_CRC32C);
                break;
            case CRC32:
                candidate.getLink().setFinalLinkState(FinalLinkState.FAILED_CRC32);
                break;
            case MD5:
                candidate.getLink().setFinalLinkState(FinalLinkState.FAILED_MD5);
                break;
            case SHA1:
                candidate.getLink().setFinalLinkState(FinalLinkState.FAILED_SHA1);
                break;
            case SHA224:
                candidate.getLink().setFinalLinkState(FinalLinkState.FAILED_SHA224);
                break;
            case SHA256:
                candidate.getLink().setFinalLinkState(FinalLinkState.FAILED_SHA256);
                break;
            case SHA384:
                candidate.getLink().setFinalLinkState(FinalLinkState.FAILED_SHA384);
                break;
            case SHA512:
                candidate.getLink().setFinalLinkState(FinalLinkState.FAILED_SHA512);
                break;
            case WHIRLPOOL:
                candidate.getLink().setFinalLinkState(FinalLinkState.FAILED_WHIRLPOOL);
                break;
            default:
                candidate.getLink().setFinalLinkState(FinalLinkState.FAILED);
                break;
            }
            return;
        case STOPPED:
            if (onDetach) {
                currentSession.removeHistory(link);
                return;
            }
            break;
        case ACCOUNT_ERROR:
        case ACCOUNT_INVALID:
        case ACCOUNT_UNAVAILABLE:
            if (onDetach) {
                final Account account = candidate.getCachedAccount().getAccount();
                candidate.getCachedAccount().getPlugin().handleAccountException(account, null, value.getThrowable());
                return;
            }
            break;
        case ACCOUNT_REQUIRED:
            if (!onDetach) {
                printDownloadLinkCandidateHistory(candidate);
                currentSession.removeHistory(link);
                candidate.getLink().setSkipReason(SkipReason.NO_ACCOUNT);
            }
            return;
        case CAPTCHA:
            if (!onDetach) {
                currentSession.removeHistory(link);
                candidate.getLink().setSkipReason(SkipReason.CAPTCHA);
            }
            return;
        case FATAL_ERROR:
            if (!onDetach) {
                currentSession.removeHistory(link);
                candidate.getLink().setFinalLinkState(FinalLinkState.FAILED_FATAL);
            }
            return;
        default:
            if (onDetach) {
                /* todo */
                logger.info("TODO: " + value.getResult());
                return;
            }
            break;
        }
        throw new WTFException("This should never happen!? " + value.getResult());
    }

    protected DISKSPACERESERVATIONRESULT validateDiskFree(final List<DownloadLinkCandidate> downloadLinkCandidates) {
        DownloadLink downloadLink = null;
        for (DownloadLinkCandidate downloadLinkCandidate : downloadLinkCandidates) {
            if (downloadLinkCandidate.getLink().getVerifiedFileSize() >= 0) {
                downloadLink = downloadLinkCandidate.getLink();
                break;
            }
        }
        if (downloadLink == null) {
            downloadLink = downloadLinkCandidates.get(0).getLink();
        }
        final File partFile = new File(downloadLink.getFileOutput() + ".part");
        /* File#length() returns 0 for a non-existing file, no extra exists() call needed. */
        final long doneSize = Math.max(partFile.length(), downloadLink.getView().getBytesLoaded());
        final long remainingSize = downloadLink.getView().getBytesTotal() - Math.max(0, doneSize);
        final DownloadLink finalDownloadLink = downloadLink;
        return getSession().getDiskSpaceManager().check(new DiskSpaceReservation() {
            @Override
            public File getDestination() {
                return partFile;
            }

            @Override
            public long getSize() {
                final PluginForHost plugin = finalDownloadLink.getDefaultPlugin();
                if (plugin != null) {
                    return remainingSize + Math.max(0, plugin.calculateAdditionalRequiredDiskSpace(finalDownloadLink));
                }
                return remainingSize;
            }

            @Override
            public Object getOwner() {
                return DownloadWatchDog.this;
            }

            @Override
            public LogInterface getLogger() {
                return DownloadWatchDog.this.logger;
            }
        });
    }

    protected DISKSPACERESERVATIONRESULT validateDiskFree(DiskSpaceReservation reservation) {
        return getSession().getDiskSpaceManager().check(reservation);
    }

    private List<DownloadLinkCandidate> nextDownloadLinkCandidates(DownloadLinkCandidateSelector selector) {
        final DownloadSession currentSession = selector.getSession();
        final boolean skipAllCaptchas = CaptchaSettings.MODE.SKIP_ALL.equals(selector.getSession().getCaptchaMode());
        while (!selector.isStopMarkReached() && newDLStartAllowed(currentSession)) {
            final DownloadLinkCandidate nextSelectedCandidate = nextDownloadLinkCandidate(selector);
            if (nextSelectedCandidate == null) {
                break;
            }
            final List<DownloadLinkCandidate> nextSelectedCandidates = new ArrayList<DownloadLinkCandidate>();
            nextSelectedCandidates.add(nextSelectedCandidate);
            if (!nextSelectedCandidate.isForced() && selector.isMirrorManagement()) {
                nextSelectedCandidates.addAll(findDownloadLinkCandidateMirrors(selector, nextSelectedCandidate));
            }
            /**
             * filter captcha stuff and canHandle==false
             */
            final List<DownloadLinkCandidate> checkNextCandidatesStage1 = new ArrayList<DownloadLinkCandidate>();
            for (final DownloadLinkCandidate candidate : nextSelectedCandidates) {
                final DownloadLink link = candidate.getLink();
                selector.addExcluded(link);
                final AccountCache accountCache = currentSession.getAccountCache(link);
                boolean ok = false;
                CachedAccount waitForAccount = null;
                ConditionalSkipReason conditionalSkipReason = null;
                for (final CachedAccount cachedAccount : accountCache) {
                    final CachedAccountPermission permission = selector.getCachedAccountPermission(cachedAccount);
                    try {
                        switch (permission) {
                        case TEMP_DISABLED:
                            /* Remember the account that is available again first. */
                            if (cachedAccount.canHandle(link)) {
                                if (waitForAccount == null || waitForAccount.getAccount().getTmpDisabledTimeout() > cachedAccount.getAccount().getTmpDisabledTimeout()) {
                                    waitForAccount = cachedAccount;
                                }
                            }
                            continue;
                        case OK:
                            break;
                        default:
                            /* Account cannot be used right now. */
                            continue;
                        }
                        if (!cachedAccount.canHandle(link)) {
                            continue;
                        }
                        final DownloadLinkCandidate checkNextCandidate = new DownloadLinkCandidate(candidate, cachedAccount, accountCache.isCustomizedCache());
                        ok = true;
                        if (cachedAccount.hasCaptcha(link) && (skipAllCaptchas || CaptchaBlackList.getInstance().matches(new PrePluginCheckDummyChallenge(link)) != null)) {
                            if (selector.validateDownloadLinkCandidate(checkNextCandidate)) {
                                selector.addExcluded(candidate, new DownloadLinkCandidateResult(SkipReason.CAPTCHA, null, null));
                            }
                            continue;
                        }
                        checkNextCandidatesStage1.add(checkNextCandidate);
                    } catch (final ConditionalSkipReasonException e) {
                        final ConditionalSkipReason skipReason = e.getConditionalSkipReason();
                        if (skipReason == null) {
                            logger.log(e);
                            continue;
                        }
                        if (conditionalSkipReason == null) {
                            conditionalSkipReason = skipReason;
                            continue;
                        }
                        /* Keep the skip reason with the longest timeout. */
                        if (skipReason instanceof TimeOutCondition && conditionalSkipReason instanceof TimeOutCondition) {
                            if (((TimeOutCondition) conditionalSkipReason).getTimeOutLeft() < ((TimeOutCondition) skipReason).getTimeOutLeft()) {
                                conditionalSkipReason = skipReason;
                            }
                        }
                    } catch (final Exception e) {
                        logger.log(e);
                    }
                }
                if (ok) {
                    continue;
                }
                if (conditionalSkipReason != null) {
                    selector.addExcluded(candidate, new DownloadLinkCandidateResult(conditionalSkipReason, null, null));
                    continue;
                }
                if (waitForAccount != null) {
                    selector.addExcluded(candidate, new DownloadLinkCandidateResult(new WaitForAccountSkipReason(waitForAccount.getAccount()), null, null));
                    continue;
                }
                selector.addExcluded(candidate, new DownloadLinkCandidateResult(RESULT.ACCOUNT_REQUIRED, null, null));
            }
            if (checkNextCandidatesStage1.size() == 0) {
                continue;
            }
            /**
             * find all possible proxySelectors
             */
            final List<DownloadLinkCandidate> checkNextCandidatesStage2 = new ArrayList<DownloadLinkCandidate>();
            final boolean[][] flags = new boolean[][] { { false, false }, { true, false }, { true, true } };
            for (boolean[] flag : flags) {
                final Iterator<DownloadLinkCandidate> it = checkNextCandidatesStage1.iterator();
                while (it.hasNext()) {
                    final DownloadLinkCandidate candidate = it.next();
                    final List<AbstractProxySelectorImpl> proxySelectors = selector.getProxies(candidate, flag[0], flag[1]);
                    if (proxySelectors.size() == 0) {
                        continue;
                    }
                    it.remove();
                    for (AbstractProxySelectorImpl proxySelector : proxySelectors) {
                        checkNextCandidatesStage2.add(new DownloadLinkCandidate(candidate, proxySelector));
                    }
                }
                if (checkNextCandidatesStage2.size() > 0) {
                    /**
                     * skip next flagset because we have already found possible proxySelectors
                     */
                    break;
                }
            }
            /**
             * handle all candidates without a proxySelector
             */
            for (final DownloadLinkCandidate notPossibleCandidate : checkNextCandidatesStage1) {
                try {
                    if (selector.validateDownloadLinkCandidate(notPossibleCandidate)) {
                        selector.addExcluded(notPossibleCandidate, new DownloadLinkCandidateResult(SkipReason.CONNECTION_UNAVAILABLE, null, null));
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                    selector.addExcluded(notPossibleCandidate, new DownloadLinkCandidateResult(RESULT.PLUGIN_DEFECT, null, null));
                }
            }
            final List<DownloadLinkCandidate> nextCandidates = new ArrayList<DownloadLinkCandidate>();
            final Iterator<DownloadLinkCandidate> it = checkNextCandidatesStage2.iterator();
            while (it.hasNext()) {
                final DownloadLinkCandidate candidate = it.next();
                try {
                    final DownloadLinkCandidatePermission permission = selector.getDownloadLinkCandidatePermission(candidate);
                    switch (permission) {
                    case OK:
                    case OK_FORCED:
                    case OK_SPEED_EXTENSION:
                        if (selector.validateDownloadLinkCandidate(candidate)) {
                            nextCandidates.add(candidate);
                        }
                        break;
                    case CONCURRENCY_FORBIDDEN:
                    case CONCURRENCY_LIMIT:
                        if (selector.validateDownloadLinkCandidate(candidate)) {
                            selector.addExcluded(candidate, new DownloadLinkCandidateResult(RESULT.CONNECTION_TEMP_UNAVAILABLE, null, null));
                        }
                        break;
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                    selector.addExcluded(candidate, new DownloadLinkCandidateResult(RESULT.PLUGIN_DEFECT, null, null));
                }
            }
            if (nextCandidates.size() > 0) {
                return nextCandidates;
            }
        }
        return null;
    }

    /**
     * Collects the waiting download requests (activation requests) that are mirrors of the given candidate, so they can be evaluated
     * together with it. Forced candidates have no mirrors. Requests that can no longer be handled are removed from the session on the way.
     */
    private List<DownloadLinkCandidate> findDownloadLinkCandidateMirrors(DownloadLinkCandidateSelector selector, DownloadLinkCandidate candidate) {
        final List<DownloadLinkCandidate> ret = new ArrayList<DownloadLinkCandidate>();
        if (candidate.isForced()) {
            return ret;
        }
        final DownloadLink link = candidate.getLink();
        final FilePackage filePackage = link.getFilePackage();
        final String name = link.getView().getDisplayName();
        final ArrayList<DownloadLink> removeList = new ArrayList<DownloadLink>();
        final MirrorDetectionDecision mirrorDetectionDecision = config.getMirrorDetectionDecision();
        for (DownloadLink next : getSession().getActivationRequests()) {
            if (next == link) {
                continue;
            }
            if (next.getFilePackage() != filePackage) {
                /* AT THE MOMENT the mirror must be in the same package */
                continue;
            }
            if (canIgnore(selector, next, false)) {
                continue;
            }
            if (canRemove(next, false)) {
                next.setConditionalSkipReason(null);
                removeList.add(next);
                continue;
            }
            if (isMirrorCandidate(link, name, next, mirrorDetectionDecision)) {
                ret.add(new DownloadLinkCandidate(next, false));
            }
        }
        getSession().removeActivationRequests(removeList);
        return ret;
    }

    private boolean canRemove(DownloadLink next, boolean isForced) {
        if (next.getDefaultPlugin() == null) {
            /* download has no plugin, we can remove it from downloadLinks because we will never be able to handle it */
            return true;
        }
        if (FilePackage.isDefaultFilePackage(next.getFilePackage())) {
            /* download has no longer a valid FilePackage because it got removed, we can remove it from downloadLinks as well */
            return true;
        }
        if (next.getAvailableStatus() == AvailableStatus.FALSE) {
            return true;
        }
        if (next.getFinalLinkState() != null) {
            return true;
        }
        if (next.isSkipped()) {
            /* download is skipped, remove it from downloadLinks until it is no longer skipped */
            return true;
        }
        if (!isForced && !next.isEnabled()) {
            /* ONLY when not forced */
            /* link is disabled, lets skip it */
            return true;
        }
        return false;
    }

    private boolean canIgnore(DownloadLinkCandidateSelector selector, DownloadLink next, boolean isForced) {
        if (next.getDownloadLinkController() != null) {
            /* download is in progress */
            return true;
        }
        ConditionalSkipReason conditionalSkipReason = next.getConditionalSkipReason();
        if (conditionalSkipReason == null) {
            return false;
        }
        if (conditionalSkipReason instanceof IgnorableConditionalSkipReason && ((IgnorableConditionalSkipReason) conditionalSkipReason).canIgnore()) {
            return true;
        }
        if (!(conditionalSkipReason instanceof ValidatableConditionalSkipReason) || ((ValidatableConditionalSkipReason) conditionalSkipReason).isValid()) {
            return true;
        }
        return false;
    }

    private DownloadLinkCandidate nextDownloadLinkCandidate(DownloadLinkCandidateSelector selector) {
        ArrayList<DownloadLink> removeList = new ArrayList<DownloadLink>();
        try {
            DownloadSession currentSession = selector.getSession();
            HashSet<DownloadLink> avoidReHandle = new HashSet<DownloadLink>();
            List<List<DownloadLink>> activationRequests = new ArrayList<List<DownloadLink>>(2);
            if (currentSession.isForcedLinksWaiting()) {
                activationRequests.add(currentSession.getForcedLinks());
            }
            if (selector.isForcedOnly() == false && currentSession.isForcedOnlyModeEnabled() == false) {
                activationRequests.add(currentSession.getActivationRequests());
            }
            for (List<DownloadLink> activationRequest : activationRequests) {
                activationRequest.removeAll(removeList);
                boolean isForced = activationRequest == currentSession.getForcedLinks();
                try {
                    Iterator<DownloadLink> it = activationRequest.iterator();
                    while (it.hasNext()) {
                        DownloadLink next = it.next();
                        if (selector.isExcluded(next)) {
                            continue;
                        }
                        if (!avoidReHandle.add(next)) {
                            continue;
                        }
                        if (canIgnore(selector, next, isForced)) {
                            selector.addExcluded(next);
                            continue;
                        }
                        if (canRemove(next, isForced)) {
                            next.setConditionalSkipReason(null);
                            removeList.add(next);
                            continue;
                        }
                        return new DownloadLinkCandidate(next, isForced);
                    }
                } finally {
                    activationRequest.removeAll(removeList);
                }
            }
        } catch (final Throwable e) {
            logger.log(e);
        }
        return null;
    }

    /**
     * returns current pause state
     *
     * @return
     */
    public boolean isPaused() {
        return DownloadWatchDog.this.stateMachine.isState(PAUSE_STATE);
    }

    private boolean isAutoReconnectEnabled() {
        return autoReconnectEnabled.get();
    }

    /**
     * may the DownloadWatchDog start new Downloads?
     *
     * @return
     */
    private boolean newDLStartAllowed(DownloadSession session) {
        if (ShutdownController.getInstance().isShutDownRequested()) {
            /* shutdown is requested, we do not start new downloads */
            return false;
        }
        final DownloadWatchDogJob job;
        synchronized (watchDogJobs) {
            job = watchDogJobs.peek();
        }
        if (job != null && job.isHighPriority()) {
            return false;
        }
        final SessionState sessionState = session.getSessionState();
        if (sessionState == DownloadSession.SessionState.RECONNECT_RUNNING) {
            /* reconnect in progress */
            return false;
        }
        if (session.isForcedLinksWaiting()) {
            /* forced links are allowed to start regardless of the state and a requested reconnect */
            return true;
        }
        if (!DownloadWatchDog.this.stateMachine.isState(DownloadWatchDog.RUNNING_STATE)) {
            /*
             * only allow new downloads in running state
             */
            return false;
        }
        if (sessionState == DownloadSession.SessionState.RECONNECT_REQUESTED && isAutoReconnectEnabled() && CFG_RECONNECT.CFG.isDownloadControllerPrefersReconnectEnabled()) {
            /*
             * auto reconnect is enabled and downloads are waiting for reconnect and user set to wait for reconnect
             */
            return false;
        }
        return true;
    }

    protected void setTempWatchDogJobThread(Thread thread) {
        tempWatchDogJobThread.set(thread);
    }

    /**
     * pauses the DownloadWatchDog
     *
     * @param value
     */
    private Object                                     shutdownLock = new Object();
    protected NullsafeAtomicReference<DownloadSession> session      = new NullsafeAtomicReference<DownloadSession>(null);

    /**
     * Updates the effective limit of the {@link DownloadSpeedManager}.
     *
     * While paused, the download speed is throttled to the (pause) speed limit even though the persistent DownloadSpeedLimitEnabled flag is
     * NOT set: pause mode must never enable the speed limit in the config, otherwise a crash/restart while paused would leave the user with
     * an active speed limit they never enabled. The pause throttling is therefore derived from the pause state here instead of from the
     * enabled flag.
     */
    protected void updateDownloadSpeedManagerLimit() {
        /* stateMachine is initialized later in the constructor than the speed limit listeners, so guard against null */
        final boolean paused = stateMachine != null && stateMachine.isState(PAUSE_STATE);
        final int limit;
        if (paused) {
            /*
             * While paused we throttle to the (separate) pause speed. Pause mode never touches the persistent download speed limit config
             * (neither value nor enabled flag), so a crash/restart while paused can not leave any speed limit behind, and a regular limit
             * changed during pause (e.g. via advanced settings) is preserved.
             */
            limit = config.getPauseSpeed();
        } else if (config.isDownloadSpeedLimitEnabled()) {
            limit = config.getDownloadSpeedLimit();
        } else {
            limit = 0;
        }
        dsm.setLimit(limit);
    }

    public void pauseDownloadWatchDog(final boolean value) {
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                /* the effective speed manager limit is (re)applied centrally by the state machine listener */
                if (!value) {
                    if (!stateMachine.isState(DownloadWatchDog.PAUSE_STATE)) {
                        /* not paused -> nothing to revert */
                        return;
                    }
                    logger.info("Pause disabled: Switch back to old downloadspeed");
                    /* we revert pause to running state */
                    stateMachine.setStatus(RUNNING_STATE);
                    return;
                }
                if (!stateMachine.isState(DownloadWatchDog.RUNNING_STATE)) {
                    /* we can only pause downloads when downloads are running */
                    return;
                }
                /*
                 * Pause mode is a pure runtime state: it does NOT touch the persistent download speed limit config (neither the value nor
                 * the enabled flag). The pause throttling is derived from the pause state in updateDownloadSpeedManagerLimit (throttling to
                 * the separate pause speed). This way a crash/restart while paused can not leave any speed limit behind, and a regular
                 * limit changed during pause is kept.
                 */
                logger.info("Pause enabled: Reducing downloadspeed to " + config.getPauseSpeed() + " KiB/s");
                /* pause downloads */
                stateMachine.setStatus(PAUSE_STATE);
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return true;
            }
        });
    }

    public void notifyCurrentState(final DownloadWatchdogListener listener) {
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                if (DownloadWatchDog.this.stateMachine.isState(RUNNING_STATE)) {
                    listener.onDownloadWatchdogStateIsRunning();
                } else if (DownloadWatchDog.this.stateMachine.isState(STOPPED_STATE)) {
                    listener.onDownloadWatchdogStateIsStopped();
                } else if (DownloadWatchDog.this.stateMachine.isState(IDLE_STATE)) {
                    listener.onDownloadWatchdogStateIsIdle();
                } else if (DownloadWatchDog.this.stateMachine.isState(PAUSE_STATE)) {
                    listener.onDownloadWatchdogStateIsPause();
                } else if (DownloadWatchDog.this.stateMachine.isState(STOPPING_STATE)) {
                    listener.onDownloadWatchdogStateIsStopping();
                }
                listener.onDownloadWatchDogPropertyChange(new DownloadWatchDogProperty(DownloadWatchDogProperty.Property.STOPSIGN, currentSession.getStopMark()));
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return true;
            }
        });
    }

    public long getDownloadSpeedbyFilePackage(FilePackage pkg) {
        long speed = -1;
        for (SingleDownloadController con : getSession().getControllers()) {
            if (con.getDownloadLink().getFilePackage() != pkg) {
                continue;
            }
            DownloadInterface dli = con.getDownloadInstance();
            if (dli == null) {
                continue;
            }
            if (speed < 0) {
                /* first matching controller: start summing from 0 instead of the -1 "no data" marker */
                speed = 0;
            }
            speed += dli.getManagedConnetionHandler().getSpeed();
        }
        return speed;
    }

    public int getDownloadsbyFilePackage(FilePackage pkg) {
        int ret = 0;
        for (SingleDownloadController con : getSession().getControllers()) {
            if (con.getDownloadLink().getFilePackage() != pkg) {
                continue;
            }
            ret++;
        }
        return ret;
    }

    public Set<SingleDownloadController> getRunningDownloadLinks() {
        return getSession().getControllers();
    }

    public Set<FilePackage> getRunningFilePackages() {
        HashSet<FilePackage> ret = new HashSet<FilePackage>();
        for (SingleDownloadController con : getSession().getControllers()) {
            ret.add(con.getDownloadLink().getParentNode());
        }
        return ret;
    }

    public boolean hasRunningDownloads(FilePackage pkg) {
        for (SingleDownloadController con : getSession().getControllers()) {
            if (con.getDownloadLink().getFilePackage() == pkg) {
                return true;
            }
        }
        return false;
    }

    /**
     * aborts all running SingleDownloadControllers, NOTE: DownloadWatchDog is still running, new Downloads will can started after this call
     */
    public void abortAllSingleDownloadControllers() {
        if (isWatchDogThread()) {
            throw new WTFException("it is not possible to use this method inside WatchDogThread!");
        }
        while (true) {
            for (SingleDownloadController con : getSession().getControllers()) {
                if (con.isAlive() && !con.isAborting()) {
                    con.abort();
                }
            }
            synchronized (WATCHDOGLOCK) {
                if (getSession().getControllers().size() == 0) {
                    return;
                }
                try {
                    WATCHDOGLOCK.wait(1000);
                } catch (InterruptedException e) {
                    logger.log(e);
                    return;
                }
            }
        }
    }

    public void enqueueJob(final DownloadWatchDogJob job) {
        synchronized (watchDogJobs) {
            final DownloadWatchDogJob first = watchDogJobs.peekFirst();
            final DownloadWatchDogJob last = watchDogJobs.peekLast();
            if (!job.isHighPriority()) {
                watchDogJobs.offerLast(job);
            } else if (first == null || !first.isHighPriority()) {
                /* offerFirst because list is empty or first one is non high priority */
                watchDogJobs.offerFirst(job);
            } else if (last.isHighPriority()) {
                /* offerLast because last one is high priority */
                watchDogJobs.offerLast(job);
            } else {
                boolean jobOffered = false;
                final ListIterator<DownloadWatchDogJob> it = watchDogJobs.listIterator();
                while (it.hasNext()) {
                    final DownloadWatchDogJob next = it.next();
                    if (next.isHighPriority()) {
                        continue;
                    }
                    /* add after last(it.previous) high priority */
                    it.previous();
                    it.add(job);
                    jobOffered = true;
                    break;
                }
                if (!jobOffered) {
                    watchDogJobs.offerLast(job);
                }
            }
        }
        if (!isWatchDogThread()) {
            synchronized (WATCHDOGLOCK) {
                WATCHDOGLOCK.notifyAll();
            }
        }
    }

    public void resume(final List<DownloadLink> resetLinks) {
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                for (DownloadLink link : resetLinks) {
                    if (link.getDownloadLinkController() == null && (FinalLinkState.CheckFailed(link.getFinalLinkState()) || link.isSkipped())) {
                        resumeLink(link, currentSession);
                    }
                }
                return;
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return true;
            }
        });
    }

    public void unSkip(final List<DownloadLink> resetLinks) {
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                for (DownloadLink link : resetLinks) {
                    if (link.getDownloadLinkController() == null && link.isSkipped()) {
                        unSkipLink(link, currentSession);
                    }
                }
                return;
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return true;
            }
        });
    }

    public void reset(final List<DownloadLink> resetLinks) {
        final ResetSettings settings = new ResetSettings();
        reset(resetLinks, settings);
    }

    public static abstract class DownloadLinkJob implements DownloadWatchDogJob {

        private final DownloadLink link;

        public DownloadLinkJob(final DownloadLink link) {
            this.link = link;
        }

        @Override
        public void execute(DownloadSession currentSession) {
            final SingleDownloadController con = link.getDownloadLinkController();
            if (con != null && con.isAlive()) {
                /* link has a running singleDownloadController, abort it and reset it after */
                con.getJobsAfterDetach().add(this);
                if (stopDownload(link)) {
                    con.abort();
                }
                return;
            }
            execute(link, currentSession);
        }

        protected abstract boolean stopDownload(final DownloadLink link);

        public abstract void execute(final DownloadLink link, DownloadSession currentSession);

        @Override
        public void interrupt() {
        }

        @Override
        public boolean isHighPriority() {
            return false;
        }

    }

    public void reset(final List<DownloadLink> resetLinks, final ResetSettings settings) {
        if (resetLinks == null || resetLinks.size() == 0) {
            return;
        }
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                if (currentSession != null) {
                    currentSession.getActivationRequests().removeAll(resetLinks);
                    currentSession.getForcedLinks().removeAll(resetLinks);
                }
                for (final DownloadLink link : resetLinks) {
                    enqueueJob(new DownloadLinkJob(link) {

                        @Override
                        public void execute(DownloadLink link, DownloadSession currentSession) {
                            resetLink(link, currentSession, settings);
                        }

                        @Override
                        protected boolean stopDownload(DownloadLink link) {
                            return true;
                        }

                    });
                }
                return;
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return true;
            }
        });
    }

    private void resumeLink(DownloadLink link, DownloadSession session) {
        if (link.getDownloadLinkController() != null) {
            throw new IllegalStateException("Link is in progress! cannot resume!");
        }
        final FinalLinkState finalLinkState = link.getFinalLinkState();
        if (!FinalLinkState.CheckFinished(finalLinkState)) {
            if (FinalLinkState.CheckFailed(finalLinkState)) {
                link.setFinalLinkState(null);
            }
            unSkipLink(link, session);
            if (!link.isEnabled()) {
                link.setEnabled(true);
            }
        }
        final DownloadLinkCandidateHistory history = session.getHistory(link);
        final List<PluginForHost> plugins = getPluginsFromHistory(link, history);
        link.resume(new ArrayList<PluginForHost>(plugins));
    }

    private void unSkipLink(DownloadLink link, DownloadSession session) {
        if (link.isSkipped()) {
            if (SkipReason.FFMPEG_MISSING.equals(link.getSkipReason()) || SkipReason.UPDATE_RESTART_REQUIRED.equals(link.getSkipReason())) {
                getSession().removeProperty(org.jdownloader.controlling.ffmpeg.FFmpegProvider.FFMPEG_INSTALL_CHECK);
            }
            link.setSkipReason(null);
        }
        CaptchaBlackList.getInstance().addWhitelist(link);
    }

    private List<PluginForHost> getPluginsFromHistory(DownloadLink link, DownloadLinkCandidateHistory history) {
        final HashSet<PluginForHost> plugins = new HashSet<PluginForHost>();
        if (history != null) {
            for (DownloadLinkCandidate candidate : history.getHistory().keySet()) {
                plugins.add(candidate.getCachedAccount().getPlugin());
            }
        }
        final PluginForHost defaultPlugin = link.getDefaultPlugin();
        if (defaultPlugin != null) {
            plugins.add(defaultPlugin);
        }
        return new ArrayList<PluginForHost>(plugins);
    }

    private void resetLink(DownloadLink link, DownloadSession session, ResetSettings settings) {
        if (link.getDownloadLinkController() != null) {
            throw new IllegalStateException("Link is in progress! cannot reset!");
        }
        final DownloadLinkCandidateHistory history = session.removeHistory(link);
        final List<WaitingSkipReasonContainer> list = session.getProxyInfoHistory().list(link.getHost());
        if (list != null) {
            for (WaitingSkipReasonContainer container : list) {
                container.invalidate();
            }
        }
        deleteFile(link, settings.getDeleteMode());
        unSkipLink(link, session);
        final List<PluginForHost> plugins = getPluginsFromHistory(link, history);
        link.reset(plugins);
    }

    public void delete(final List<DownloadLink> deleteFiles, final DeleteOption deleteTo) {
        delete(deleteFiles, deleteTo, false);
    }

    public Map<DownloadLink, Map<File, Boolean>> delete(final List<DownloadLink> deleteFiles, final DeleteOption deleteTo, final boolean waitForDeletion) {
        if (deleteFiles == null || deleteFiles.size() == 0) {
            return null;
        }
        final HashSet<DownloadLink> todo = new HashSet<DownloadLink>(deleteFiles);
        final Map<DownloadLink, Map<File, Boolean>> ret = new HashMap<DownloadLink, Map<File, Boolean>>();
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                if (currentSession != null) {
                    currentSession.getActivationRequests().removeAll(todo);
                    currentSession.getForcedLinks().removeAll(todo);
                }
                for (final DownloadLink link : todo) {
                    enqueueJob(new DownloadLinkJob(link) {

                        @Override
                        public void execute(DownloadLink link, DownloadSession currentSession) {
                            Map<File, Boolean> result = null;
                            try {
                                result = deleteFile(link, deleteTo);
                            } finally {
                                synchronized (ret) {
                                    ret.put(link, result);
                                    ret.notifyAll();
                                }
                            }
                        }

                        @Override
                        protected boolean stopDownload(DownloadLink link) {
                            return true;
                        }
                    });
                }
                return;
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return false;
            }
        });
        if (!waitForDeletion) {
            return null;
        }
        while (true) {
            synchronized (ret) {
                if (ret.size() == todo.size()) {
                    break;
                }
                try {
                    ret.wait();
                } catch (InterruptedException e) {
                    logger.log(e);
                    return new HashMap<DownloadLink, Map<File, Boolean>>(ret);
                }
            }
        }
        return ret;
    }

    private Map<File, Boolean> deleteFile(DownloadLink link, DeleteOption deleteTo) {
        if (deleteTo == null) {
            deleteTo = DeleteOption.NULL;
        }
        if (DeleteOption.NO_DELETE == deleteTo) {
            return null;
        }
        final Map<File, Boolean> deleteFiles = new HashMap<File, Boolean>();
        try {
            for (final File deleteFile : link.getDefaultPlugin().listProcessFiles(link)) {
                /* isFile() is only true for existing files. */
                if (!deleteFile.isFile()) {
                    continue;
                }
                try {
                    getSession().getFileAccessManager().lock(deleteFile, this);
                    deleteFiles.put(deleteFile, null);
                } catch (FileIsLockedException e) {
                    logger.log(e);
                }
            }
            for (final File deleteFile : new ArrayList<File>(deleteFiles.keySet())) {
                switch (deleteTo) {
                case NULL:
                    if (deleteFile.delete() == false) {
                        deleteFiles.put(deleteFile, Boolean.FALSE);
                        logger.info("Could not delete file: " + deleteFile + " for " + link + " exists: " + deleteFile.exists());
                    } else {
                        deleteFiles.put(deleteFile, Boolean.TRUE);
                        logger.info("Deleted file: " + deleteFile + " for " + link);
                    }
                    break;
                case RECYCLE:
                    try {
                        JDFileUtils.moveToTrash(deleteFile);
                        deleteFiles.put(deleteFile, Boolean.TRUE);
                        logger.info("Recycled file: " + deleteFile + " for " + link);
                    } catch (IOException e) {
                        deleteFiles.put(deleteFile, Boolean.FALSE);
                        logger.log(e);
                        logger.info("Could not recycle file: " + deleteFile + " for " + link + " exists: " + deleteFile.exists());
                    }
                    break;
                default:
                    /* NO_DELETE has already been handled at the top of this method */
                    break;
                }
            }
            /* try to delete folder (if its empty and NOT the default downloadfolder */
            final File dlFolder = new File(link.getDownloadDirectory());
            final File dfFolder = new File(org.appwork.storage.config.JsonConfig.create(GeneralSettings.class).getDefaultDownloadFolder());
            if (!dfFolder.equals(dlFolder) && dlFolder.isDirectory()) {
                /*
                 * File#delete() only removes an empty directory, so there is no need to list the content of the folder first. The
                 * isDirectory() check above makes sure that we never delete a regular file with that name.
                 */
                if (dlFolder.delete()) {
                    logger.info("Deleted folder: " + dlFolder + " for " + link);
                } else {
                    logger.info("Could not delete folder(not empty or no permission): " + dlFolder + " for " + link);
                }
            }
            return deleteFiles;
        } finally {
            for (File deleteFile : deleteFiles.keySet()) {
                getSession().getFileAccessManager().unlock(deleteFile, this);
            }
        }
    }

    public void abort(final List<DownloadLink> abortLinks) {
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                for (final DownloadLink link : abortLinks) {
                    final SingleDownloadController con = link.getDownloadLinkController();
                    if (con != null) {
                        con.abort();
                    }
                }
                return;
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return true;
            }
        });
    }

    /**
     * activates new Downloads as long as possible and returns how many got activated
     *
     * @param downloadLinksWithConditionalSkipReasons
     *
     * @return
     **/
    private List<SingleDownloadController> activateDownloads(final List<DownloadLink> downloadLinksWithConditionalSkipReasons) throws Exception {
        final ArrayList<SingleDownloadController> ret = new ArrayList<SingleDownloadController>();
        final DownloadSession session = getSession();
        final DownloadLinkCandidateSelector selector = new DownloadLinkCandidateSelector(session);
        int maxConcurrentNormal = Math.max(1, config.getMaxSimultaneDownloads());
        maxConcurrentNormal = Math.max(maxConcurrentNormal, DomainRuleController.getInstance().getMaxSimultaneousDownloads());
        int maxConcurrentForced = maxConcurrentNormal + Math.max(0, config.getMaxForcedDownloads());
        int loopCounter = 0;
        final boolean invalidate = session.setCandidatesRefreshRequired(false);
        try {
            if (invalidate && downloadLinksWithConditionalSkipReasons != null) {
                for (final DownloadLink candidate : downloadLinksWithConditionalSkipReasons) {
                    final ConditionalSkipReason con = candidate.getConditionalSkipReason();
                    if (con != null && con instanceof ValidatableConditionalSkipReason) {
                        ((ValidatableConditionalSkipReason) con).invalidate();
                    }
                }
            }
            if (session.isForcedLinksWaiting()) {
                /* first process forcedLinks */
                selector.setForcedOnly(true);
                loopCounter = 0;
                while (this.newDLStartAllowed(session) && session.isForcedLinksWaiting() && (getActiveDownloads() < maxConcurrentForced) && loopCounter < maxConcurrentForced) {
                    try {
                        final DownloadLinkCandidate candidate = this.next(selector);
                        if (candidate == null) {
                            break;
                        }
                        ret.add(attach(candidate));
                    } finally {
                        loopCounter++;
                    }
                }
            }
            if (!session.isStopMarkReached()) {
                selector.setForcedOnly(false);
                /* then process normal activationRequests */
                final boolean canExceed = DomainRuleController.getInstance().getMaxSimultaneousDownloads() > 0;
                // heckForAdditionalDownloadSlots(session)
                while (this.newDLStartAllowed(session)) {
                    if (!canExceed) {
                        // no rules that may exceed the global download limits
                        if (getActiveDownloads() >= maxConcurrentNormal && !checkForAdditionalDownloadSlots(session)) {
                            break;
                        }
                    }
                    final DownloadLinkCandidate candidate = this.next(selector);
                    if (candidate == null) {
                        break;
                    }
                    ret.add(attach(candidate));
                }
            }
        } finally {
            finalize(selector, downloadLinksWithConditionalSkipReasons);
        }
        return ret;
    }

    public boolean checkForAdditionalDownloadSlots(final DownloadSession session) {
        final long autoMaxDownloadSpeedLimit = config.getAutoMaxDownloadsSpeedLimit();
        final int autoMaxDownloadsMaxDownloads = config.getAutoMaxDownloadsSpeedLimitMaxDownloads();
        final int autoMaxDownloadsMinDelay = config.getAutoMaxDownloadsSpeedLimitMinDelay();
        final Set<SingleDownloadController> controllers = session.getControllers();
        if (autoMaxDownloadSpeedLimit <= 0 || (autoMaxDownloadsMaxDownloads != 0 && controllers.size() >= autoMaxDownloadsMaxDownloads)) {
            /* auto download slots are disabled or the maximum number of downloads is reached */
            return false;
        }
        try {
            final long currentDownloadSpeed = getDownloadSpeedManager().getSpeedMeter().getValue(Resolution.SECONDS);
            if (currentDownloadSpeed >= autoMaxDownloadSpeedLimit) {
                /* the current speed is already high enough */
                return false;
            }
            /* use the effective limit applied by the speed manager (pause speed while paused, 0 = no limit) */
            final int speedlimit = getDownloadSpeedManager().getLimit();
            if (speedlimit > 0 && currentDownloadSpeed > speedlimit * 0.8) {
                // do not start a new download: speedlimit is set, and the speed is almost at the limit
                return false;
            }
            long latestStart = 0;
            for (final SingleDownloadController s : controllers) {
                latestStart = Math.max(latestStart, s.getStartTimestamp());
                long left = s.getDownloadLink().getView().getBytesTotal() - s.getDownloadLink().getView().getBytesLoaded();
                if (left <= 0) {
                    // download done - like mega.
                    continue;
                }
                if (s.getDownloadLink().getView().getSpeedBps() <= 0) {
                    // do not start a new download: not all downloads are running.
                    return false;
                }
            }
            if (System.currentTimeMillis() - latestStart < autoMaxDownloadsMinDelay) {
                // do not start a new download: latest start is less then 5 secs ago
                return false;
            }
            return true;
        } catch (Throwable e) {
            logger.log(e);
        }
        // do not start a new download.
        return false;
    }

    private void finalize(final DownloadLinkCandidateSelector selector, final List<DownloadLink> downloadLinksWithConditionalSkipReasons) {
        if (downloadLinksWithConditionalSkipReasons != null) {
            for (final DownloadLink link : downloadLinksWithConditionalSkipReasons) {
                final ConditionalSkipReason conditionalSkipReason = link.getConditionalSkipReason();
                if (conditionalSkipReason == null) {
                    continue;
                }
                if (conditionalSkipReason instanceof ValidatableConditionalSkipReason && !((ValidatableConditionalSkipReason) conditionalSkipReason).isValid()) {
                    link.setConditionalSkipReason(null);
                }
                if (conditionalSkipReason.isConditionReached()) {
                    link.setConditionalSkipReason(null);
                    conditionalSkipReason.finalize(link);
                }
            }
        }
        LinkedHashMap<DownloadLinkCandidate, DownloadLinkCandidateResult> results = selector.finalizeDownloadLinkCandidatesResults();
        Iterator<Entry<DownloadLinkCandidate, DownloadLinkCandidateResult>> it = results.entrySet().iterator();
        while (it.hasNext()) {
            Entry<DownloadLinkCandidate, DownloadLinkCandidateResult> next = it.next();
            DownloadLinkCandidate candidate = next.getKey();
            DownloadLinkCandidateResult result = next.getValue();
            setFinalLinkStatus(candidate, result, null);
        }
    }

    public void startDownloads() {
        startDownloads(null);
    }

    /**
     * start the DownloadWatchDog
     */
    public void startDownloads(final Runnable runBeforeStartingDownloadWatchDog) {
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                if (DownloadWatchDog.this.stateMachine.isFinal()) {
                    /* downloadwatchdog was in stopped state, so reset it */
                    DownloadWatchDog.this.stateMachine.reset(false);
                }
                if (!DownloadWatchDog.this.stateMachine.isStartState()) {
                    /* only allow to start when in FinalState(NOT_RUNNING) */
                    return;
                }
                /*
                 * setNewSession
                 */
                session.set(new DownloadSession(currentSession));
                if (runBeforeStartingDownloadWatchDog != null) {
                    try {
                        runBeforeStartingDownloadWatchDog.run();
                    } catch (final Throwable e) {
                        logger.log(e);
                    }
                }
                /* start watchdogthread */
                startDownloadWatchDog();
                /* throw start event */
                logger.info("DownloadWatchDog: start");
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return true;
            }
        });
    }

    public DownloadSession getSession() {
        return session.get();
    }

    /**
     * activates a new SingleDownloadController for the given SingleDownloadControllerActivator
     *
     * @param activator
     */
    private SingleDownloadController attach(final DownloadLinkCandidate candidate) {
        logger.info("Start new Download: Use Premium: " + getSession().isUseAccountsEnabled() + " Host:" + candidate);
        DownloadLinkCandidateHistory history = getSession().buildHistory(candidate.getLink());
        if (history == null || !history.attach(candidate)) {
            logger.severe("Could not attach to History: " + candidate);
        }
        final SingleDownloadController con = new SingleDownloadController(candidate, this);
        DownloadController.getInstance().getQueue().addWait(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                boolean ignoreUnsafe = true;
                String downloadTo = candidate.getLink().getFileOutput(ignoreUnsafe, false);
                if (StringUtils.isEmpty(downloadTo)) {
                    ignoreUnsafe = false;
                    downloadTo = candidate.getLink().getFileOutput(ignoreUnsafe, false);
                }
                String customDownloadTo = candidate.getLink().getFileOutput(ignoreUnsafe, true);
                if (ignoreUnsafe) {
                    logger.info("Download To: " + downloadTo);
                } else {
                    logger.info("Download To(Unsafe): " + downloadTo);
                }
                if (!StringUtils.equalsIgnoreCase(downloadTo, customDownloadTo)) {
                    logger.info("Download To(custom): " + customDownloadTo);
                }
                con.setSessionDownloadDirectory(candidate.getLink().getParentNode().getDownloadDirectory());
                return null;
            }
        });
        con.setSessionDownloadFilename(candidate.getLink().getForcedFileName());
        if (candidate.getProxySelector() != null) {
            candidate.getProxySelector().add(con);
        }
        candidate.getLink().setEnabled(true);
        getSession().getControllers().add(con);
        con.start();
        return con;
    }

    protected void detach(final SingleDownloadController singleDownloadController, final SingleDownloadReturnState returnState) {
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                LogSource logger = null;
                try {
                    if (singleDownloadController.getLogger() instanceof LogSource) {
                        logger = (LogSource) singleDownloadController.getLogger();
                    }
                    if (logger == null) {
                        logger = new LogSource("Dummy") {
                            @Override
                            public synchronized void clear() {
                            }

                            public synchronized void log(java.util.logging.LogRecord record) {
                                DownloadWatchDog.this.logger.log(record);
                            };
                        };
                    }
                    final DownloadLinkCandidate candidate = singleDownloadController.getDownloadLinkCandidate();
                    final DownloadLink link = candidate.getLink();
                    if (candidate.getProxySelector() != null) {
                        candidate.getProxySelector().remove(singleDownloadController);
                    }
                    DownloadLinkCandidateResult result = null;
                    try {
                        try {
                            result = handleReturnState(logger, singleDownloadController, returnState);
                            result.setStartTime(singleDownloadController.getStartTimestamp());
                            result.setFinishTime(returnState.getTimeStamp());
                            final DownloadLinkCandidateHistory existingHistory = currentSession.getHistory(link);
                            setFinalLinkStatus(candidate, result, singleDownloadController);
                            if (existingHistory != null && !existingHistory.dettach(candidate, result)) {
                                DownloadWatchDog.this.logger.severe("Could not detach from History: " + candidate);
                            }
                        } catch (final Throwable e) {
                            DownloadWatchDog.this.logger.log(e);
                        }
                        currentSession.getControllers().remove(singleDownloadController);
                        for (final DownloadWatchDogJob job : singleDownloadController.getJobsAfterDetach()) {
                            try {
                                job.execute(currentSession);
                            } catch (Throwable e) {
                                DownloadWatchDog.this.logger.log(e);
                            }
                        }
                        try {
                            logger.info("Rename after Download?");
                            final File desiredPath = new File(link.getFileOutput(false, false));
                            logger.info("Desired Path: " + desiredPath);
                            final File usedPath = singleDownloadController.getFileOutput(false, false);
                            logger.info("Actually Used path: " + usedPath);
                            if (!desiredPath.equals(usedPath)) {
                                logger.info("Move");
                                move(link, usedPath.getParent(), usedPath.getName(), desiredPath.getParent(), desiredPath.getName());
                            }
                        } catch (final Throwable e) {
                            DownloadWatchDog.this.logger.log(e);
                        }
                        /* after each download, the order/position of next downloadCandidate could have changed */
                        currentSession.refreshCandidates();
                        final FilePackage fp = link.getLastValidFilePackage();
                        try {
                            fp.getView().requestUpdate();
                        } catch (final Throwable e) {
                            /* link can already be removed->nullpointer exception */
                        }
                        final Set<DownloadLink> finalizedLinks;
                        if (fp != null && !FilePackage.isDefaultFilePackage(fp)) {
                            final boolean readL = fp.getModifyLock().readLock();
                            try {
                                finalizedLinks = finalizeConditionalSkipReasons(fp.getChildren(), null);
                            } finally {
                                fp.getModifyLock().readUnlock(readL);
                            }
                        } else {
                            finalizedLinks = null;
                        }
                        try {
                            eventSender.fireEvent(new DownloadWatchdogEvent(this, DownloadWatchdogEvent.Type.LINK_STOPPED, singleDownloadController, candidate, result));
                        } catch (final Throwable e) {
                            DownloadWatchDog.this.logger.log(e);
                        }
                        handleFinalLinkStates(finalizedLinks, currentSession, DownloadWatchDog.this.logger, singleDownloadController);
                    } finally {
                        if (result != null) {
                            // cleanup
                            //
                            // result.setThrowable(null);
                        }
                    }
                } catch (final Throwable e) {
                    DownloadWatchDog.this.logger.log(e);
                    if (logger != null) {
                        logger.log(e);
                    }
                } finally {
                    if (logger != null) {
                        logger.close();
                    }
                }
                return;
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return false;
            }
        });
    }

    private void handleFinalLinkStates(Set<DownloadLink> links, DownloadSession session, final LogSource logger, final SingleDownloadController singleDownloadController) {
        if ((links == null || links.size() == 0) && singleDownloadController == null) {
            return;
        }
        if (links == null) {
            links = new HashSet<DownloadLink>();
        }
        final DownloadLink singleDownloadControllerLink;
        if (singleDownloadController != null) {
            singleDownloadControllerLink = singleDownloadController.getDownloadLinkCandidate().getLink();
            links.add(singleDownloadControllerLink);
        } else {
            singleDownloadControllerLink = null;
        }
        final Iterator<DownloadLink> it = links.iterator();
        final CleanAfterDownloadAction cleanupAction = org.jdownloader.settings.staticreferences.CFG_GENERAL.CFG.getCleanupAfterDownloadAction();
        final boolean cleanupFileExists = JsonConfig.create(GeneralSettings.class).getCleanupFileExists();
        final ArrayList<DownloadLink> cleanupLinks = new ArrayList<DownloadLink>();
        final HashSet<FilePackage> cleanupPackages = new HashSet<FilePackage>();
        while (it.hasNext()) {
            final DownloadLink next = it.next();
            final FinalLinkState state = next.getFinalLinkState();
            final boolean failedExists = FinalLinkState.FAILED_EXISTS.equals(state);
            if (!failedExists && !FinalLinkState.CheckFinished(state)) {
                /* link is neither finished nor failed because the file exists -> nothing to do */
                continue;
            }
            session.removeActivationRequest(next);
            session.removeHistory(next);
            it.remove();
            if (!failedExists || cleanupFileExists) {
                switch (cleanupAction) {
                case CLEANUP_IMMEDIATELY:
                    cleanupLinks.add(next);
                    break;
                case CLEANUP_AFTER_PACKAGE_HAS_FINISHED:
                    cleanupPackages.add(next.getFilePackage());
                    break;
                }
            }
            if (failedExists || next != singleDownloadControllerLink) {
                /* only the link of the detached controller created a new file */
                continue;
            }
            TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
                @Override
                protected Void run() throws RuntimeException {
                    FileCreationManager.getInstance().getEventSender().fireEvent(new FileCreationEvent(singleDownloadController, FileCreationEvent.Type.NEW_FILES, new File[] { new File(next.getFileOutput()) }));
                    return null;
                }
            });
        }
        if (cleanupLinks.size() > 0 || cleanupPackages.size() > 0) {
            TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
                @Override
                protected Void run() throws RuntimeException {
                    switch (cleanupAction) {
                    case CLEANUP_IMMEDIATELY:
                        if (cleanupLinks.size() > 0) {
                            DownloadController.getInstance().getQueue().add(new QueueAction<Void, RuntimeException>() {
                                @Override
                                protected Void run() throws RuntimeException {
                                    for (DownloadLink cleanupLink : cleanupLinks) {
                                        if (DownloadController.getInstance() == cleanupLink.getFilePackage().getControlledBy()) {
                                            String name = cleanupLink.getView().getDisplayName();
                                            logger.info("Remove Link " + name + " because " + cleanupLink.getFinalLinkState() + " and CleanupImmediately!");
                                            List<DownloadLink> remove = new ArrayList<DownloadLink>();
                                            remove.add(cleanupLink);
                                            remove = DownloadController.getInstance().askForRemoveVetos(singleDownloadController, remove);
                                            if (remove.size() > 0) {
                                                DownloadController.getInstance().removeChildren(remove);
                                            } else {
                                                logger.info("Remove Link " + name + " failed because of removeVetos!");
                                            }
                                        }
                                    }
                                    return null;
                                }
                            });
                        }
                        break;
                    case CLEANUP_AFTER_PACKAGE_HAS_FINISHED:
                        if (cleanupPackages.size() > 0) {
                            for (FilePackage filePackage : cleanupPackages) {
                                DownloadController.removePackageIfFinished(singleDownloadController, logger, filePackage);
                            }
                        }
                        break;
                    }
                    return null;
                }
            });
        }
    }

    private DownloadLinkCandidateResult handleReturnState(LogSource logger, SingleDownloadController singleDownloadController, SingleDownloadReturnState result) {
        final Throwable throwable = result.getCaughtThrowable();
        final DownloadLinkCandidate candidate = singleDownloadController.getDownloadLinkCandidate();
        final DownloadLink link = candidate.getLink();
        final long sizeChange = Math.max(0, link.getView().getBytesLoaded() - singleDownloadController.getSizeBefore());
        final Account account = singleDownloadController.getAccount();
        if (account != null && sizeChange > 0) {
            candidate.getCachedAccount().getPlugin().update(link, account, sizeChange);
        }

        PluginException pluginException = null;
        PluginForHost latestPlugin = null;
        final String pluginHost;
        if ((latestPlugin = result.getLatestPlugin()) != null) {
            pluginHost = latestPlugin.getLazyP().getHost();
        } else {
            pluginHost = candidate.getCachedAccount().getPlugin().getLazyP().getHost();
        }
        if (throwable instanceof PluginException) {
            pluginException = (PluginException) throwable;
            if (pluginException instanceof CandidateResultProvider) {
                final DownloadLinkCandidateResult ret = ((CandidateResultProvider) pluginException).createCandidateResult(candidate, pluginHost);
                if (ret != null) {
                    return ret;
                }
            }
        } else if (throwable instanceof SkipReasonException) {
            final SkipReason skipReason = ((SkipReasonException) throwable).getSkipReason();
            switch (skipReason) {
            case NO_ACCOUNT:
                return new DownloadLinkCandidateResult(RESULT.ACCOUNT_REQUIRED, throwable, pluginHost);
            default:
                return new DownloadLinkCandidateResult(skipReason, throwable, pluginHost);
            }
        } else if (throwable instanceof ConditionalSkipReasonException) {
            final ConditionalSkipReason conditionalSkipReason = ((ConditionalSkipReasonException) throwable).getConditionalSkipReason();
            return new DownloadLinkCandidateResult(conditionalSkipReason, throwable, pluginHost);
        } else if (throwable != null) {
            /*
             * Connection related throwables. The order matters because some of the exceptions extend others (e.g. SocketException is an
             * IOException).
             */
            if (throwable instanceof NoInternetConnection || throwable instanceof UnknownHostException) {
                final DownloadLinkCandidateResult ret = new DownloadLinkCandidateResult(RESULT.CONNECTION_ISSUES, throwable, pluginHost);
                ret.setWaitTime(JsonConfig.create(GeneralSettings.class).getNetworkIssuesTimeout());
                ret.setMessage(_JDT.T.plugins_errors_nointernetconn());
                return ret;
            }
            if (throwable instanceof SocketTimeoutException) {
                final DownloadLinkCandidateResult ret = new DownloadLinkCandidateResult(RESULT.CONNECTION_ISSUES, throwable, pluginHost);
                ret.setWaitTime(JsonConfig.create(GeneralSettings.class).getNetworkIssuesTimeout());
                ret.setMessage(_JDT.T.plugins_errors_hosteroffline());
                return ret;
            }
            if (throwable instanceof SocketException) {
                final DownloadLinkCandidateResult ret = new DownloadLinkCandidateResult(RESULT.CONNECTION_ISSUES, throwable, pluginHost);
                ret.setWaitTime(JsonConfig.create(GeneralSettings.class).getNetworkIssuesTimeout());
                ret.setMessage(_JDT.T.plugins_errors_disconnect());
                return ret;
            }
            if (throwable instanceof NoGateWayException || throwable instanceof ProxyConnectException) {
                final DownloadLinkCandidateResult ret = new DownloadLinkCandidateResult(RESULT.CONNECTION_ISSUES, throwable, pluginHost);
                ret.setWaitTime(TimeUnit.SECONDS.toMillis(10));
                ret.setMessage(_JDT.T.plugins_errors_proxy_connection());
                return ret;
            }
            if (throwable instanceof ProxyAuthException) {
                final DownloadLinkCandidateResult ret = new DownloadLinkCandidateResult(RESULT.CONNECTION_ISSUES, throwable, pluginHost);
                ret.setWaitTime(TimeUnit.SECONDS.toMillis(30));
                ret.setMessage(_JDT.T.plugins_errors_proxy_auth());
                return ret;
            }
            if (throwable instanceof BlockedByException) {
                final BlockedByException bbe = (BlockedByException) throwable;
                final DownloadLinkCandidateResult ret = new DownloadLinkCandidateResult(RESULT.CONNECTION_ISSUES, throwable, pluginHost);
                ret.setWaitTime(JsonConfig.create(GeneralSettings.class).getDownloadUnknownIOExceptionWaittime());
                ret.setMessage(_JDT.T.downloadlink_status_error_blocked_by(bbe.getBlockedBy().getLabel()));
                return ret;
            }
            if (throwable instanceof IOException) {
                final DownloadLinkCandidateResult ret = new DownloadLinkCandidateResult(RESULT.CONNECTION_ISSUES, throwable, pluginHost);
                ret.setWaitTime(JsonConfig.create(GeneralSettings.class).getDownloadUnknownIOExceptionWaittime());
                ret.setMessage(_JDT.T.plugins_errors_hosterproblem());
                return ret;
            }
            final boolean interrupted = throwable instanceof InterruptedException;
            if (interrupted && result.getController().isAborting()) {
                /* interrupted because we aborted the download on purpose */
                return new DownloadLinkCandidateResult(RESULT.STOPPED, throwable, pluginHost);
            }
            pluginException = new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, _JDT.T.plugins_errors_error() + (interrupted ? "Interrupted" : "Throwable"));
        }
        if (pluginException != null) {
            DownloadLinkCandidateResult ret = null;
            String message = null;
            long waitTime = -1;
            switch (pluginException.getLinkStatus()) {
            case LinkStatus.ERROR_RETRY:
                ret = new DownloadLinkCandidateResult(RESULT.RETRY, throwable, pluginHost);
                message = pluginException.getLocalizedMessage();
                break;
            case LinkStatus.ERROR_CAPTCHA:
                ret = new DownloadLinkCandidateResult(RESULT.CAPTCHA, throwable, pluginHost);
                break;
            case LinkStatus.FINISHED:
                ret = new DownloadLinkCandidateResult(RESULT.FINISHED_EXISTS, throwable, pluginHost);
                break;
            case LinkStatus.ERROR_FILE_NOT_FOUND:
                ret = new DownloadLinkCandidateResult(RESULT.OFFLINE_TRUSTED, throwable, pluginHost);
                break;
            case LinkStatus.ERROR_PLUGIN_DEFECT:
                if (latestPlugin != null) {
                    latestPlugin.errLog(throwable, null, link);
                }
                logger.info("Plugin Defect.1");
                ret = new DownloadLinkCandidateResult(RESULT.PLUGIN_DEFECT, throwable, pluginHost);
                break;
            case LinkStatus.ERROR_FATAL:
                ret = new DownloadLinkCandidateResult(RESULT.FATAL_ERROR, throwable, pluginHost);
                message = pluginException.getLocalizedMessage();
                break;
            case LinkStatus.ERROR_IP_BLOCKED:
                ret = new DownloadLinkCandidateResult(RESULT.IP_BLOCKED, throwable, pluginHost);
                message = pluginException.getLocalizedMessage();
                waitTime = pluginException.getValue() > 0 ? pluginException.getValue() : TimeUnit.HOURS.toMillis(1);
                break;
            case LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE:
                ret = new DownloadLinkCandidateResult(RESULT.FILE_UNAVAILABLE, throwable, pluginHost);
                message = pluginException.getLocalizedMessage();
                waitTime = pluginException.getValue() > 0 ? pluginException.getValue() : JsonConfig.create(GeneralSettings.class).getDownloadTempUnavailableRetryWaittime();
                break;
            case LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE:
                ret = new DownloadLinkCandidateResult(RESULT.HOSTER_UNAVAILABLE, throwable, pluginHost);
                message = pluginException.getLocalizedMessage();
                waitTime = pluginException.getValue() > 0 ? pluginException.getValue() : JsonConfig.create(GeneralSettings.class).getDownloadHostUnavailableRetryWaittime();
                break;
            case LinkStatus.ERROR_PREMIUM:
                if (pluginException.getValue() == PluginException.VALUE_ID_PREMIUM_ONLY) {
                    ret = new DownloadLinkCandidateResult(RESULT.ACCOUNT_REQUIRED, throwable, pluginHost);
                    message = pluginException.getLocalizedMessage();
                    break;
                }
                if (pluginException.getValue() == PluginException.VALUE_ID_PREMIUM_DISABLE) {
                    ret = new DownloadLinkCandidateResult(RESULT.ACCOUNT_INVALID, throwable, pluginHost);
                    break;
                }
                if (pluginException.getValue() == PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE) {
                    ret = new DownloadLinkCandidateResult(RESULT.ACCOUNT_UNAVAILABLE, throwable, pluginHost);
                    break;
                }
                ret = new DownloadLinkCandidateResult(RESULT.ACCOUNT_ERROR, throwable, pluginHost);
                break;
            case LinkStatus.ERROR_DOWNLOAD_FAILED:
                if (pluginException.getValue() == LinkStatus.VALUE_LOCAL_IO_ERROR) {
                    ret = new DownloadLinkCandidateResult(SkipReason.INVALID_DESTINATION, throwable, pluginHost);
                    break;
                }
                if (pluginException.getValue() == LinkStatus.VALUE_NETWORK_IO_ERROR) {
                    ret = new DownloadLinkCandidateResult(RESULT.CONNECTION_ISSUES, throwable, pluginHost);
                    break;
                }
                ret = new DownloadLinkCandidateResult(RESULT.FAILED, throwable, pluginHost);
                break;
            case LinkStatus.ERROR_DOWNLOAD_INCOMPLETE:
                if (pluginException.getValue() == LinkStatus.VALUE_NETWORK_IO_ERROR) {
                    ret = new DownloadLinkCandidateResult(RESULT.CONNECTION_ISSUES, throwable, pluginHost);
                    break;
                }
                ret = new DownloadLinkCandidateResult(RESULT.FAILED_INCOMPLETE, throwable, pluginHost);
                break;
            case LinkStatus.ERROR_ALREADYEXISTS:
                ret = new DownloadLinkCandidateResult(RESULT.FAILED_EXISTS, throwable, pluginHost);
                break;
            }
            if (ret == null) {
                logger.info("Plugin Defect.2");
                ret = new DownloadLinkCandidateResult(RESULT.PLUGIN_DEFECT, throwable, pluginHost);
            }
            ret.setWaitTime(waitTime);
            ret.setMessage(message);
            return ret;
        }
        if (result.getController().getLinkStatus().getStatus() == LinkStatus.FINISHED) {
            /* finished, no matter whether we aborted afterwards */
            return new DownloadLinkCandidateResult(RESULT.FINISHED, throwable, pluginHost);
        }
        if (result.getController().isAborting()) {
            return new DownloadLinkCandidateResult(RESULT.STOPPED, throwable, pluginHost);
        }
        logger.info("Plugin Defect.3");
        return new DownloadLinkCandidateResult(RESULT.PLUGIN_DEFECT, throwable, pluginHost);
    }

    private Set<DownloadLink> finalizeConditionalSkipReasons(final DownloadSession currentSession, final List<DownloadLink> downloadLinksWithConditionalSkipReasons) {
        return finalizeConditionalSkipReasons(new ConcatIterator<DownloadLink>(currentSession.getForcedLinks().iterator(), currentSession.getActivationRequests().iterator()), downloadLinksWithConditionalSkipReasons);
    }

    private Set<DownloadLink> finalizeConditionalSkipReasons(final Iterable<DownloadLink> links, final List<DownloadLink> downloadLinksWithConditionalSkipReasons) {
        final HashSet<DownloadLink> ret = new HashSet<DownloadLink>();
        final ArrayList<DownloadLink> again = new ArrayList<DownloadLink>();
        for (final DownloadLink link : links) {
            final ConditionalSkipReason conditionalSkipReason = link.getConditionalSkipReason();
            if (conditionalSkipReason == null) {
                continue;
            }
            if (!conditionalSkipReason.isConditionReached()) {
                again.add(link);
                continue;
            }
            link.setConditionalSkipReason(null);
            conditionalSkipReason.finalize(link);
            if (link.getFinalLinkState() != null) {
                ret.add(link);
            }
        }
        final int size = again.size();
        while (size > 0) {
            int finalized = 0;
            for (int index = 0; index < size; index++) {
                final DownloadLink link = again.get(index);
                final ConditionalSkipReason conditionalSkipReason = link.getConditionalSkipReason();
                if (conditionalSkipReason == null || !conditionalSkipReason.isConditionReached()) {
                    continue;
                }
                link.setConditionalSkipReason(null);
                conditionalSkipReason.finalize(link);
                finalized++;
                if (link.getFinalLinkState() != null) {
                    ret.add(link);
                }
            }
            if (finalized == 0) {
                break;
            }
        }
        if (downloadLinksWithConditionalSkipReasons == null) {
            return ret;
        }
        for (int index = 0; index < size; index++) {
            final DownloadLink link = again.get(index);
            if (link.getConditionalSkipReason() != null) {
                downloadLinksWithConditionalSkipReasons.add(link);
            }
        }
        return ret;
    }

    private synchronized void startDownloadJobExecuter() {
        Thread thread = new Thread() {
            @Override
            public void interrupt() {
                logger.log(new RuntimeException("WatchDog(" + getName() + ") interrupted!"));
                super.interrupt();
            }

            @Override
            public void run() {
                this.setName("WatchDog: jobExecuter");
                try {
                    while (true) {
                        synchronized (WATCHDOGLOCK) {
                            if (!isWatchDogThread()) {
                                return;
                            }
                            /* clear interrupted flag in case someone interrupted a DownloadWatchDogJob */
                            interrupted();
                            if (!hasWaitingJobs()) {
                                WATCHDOGLOCK.wait();
                            }
                            if (!isWatchDogThread()) {
                                return;
                            }
                        }
                        final DownloadWatchDogJob job;
                        synchronized (watchDogJobs) {
                            job = watchDogJobs.poll();
                        }
                        if (job == null) {
                            continue;
                        }
                        try {
                            currentWatchDogJob.set(job);
                            job.execute(getSession());
                        } catch (final Throwable e) {
                            logger.log(e);
                        } finally {
                            currentWatchDogJob.set(null);
                        }
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                } finally {
                    currentWatchDogThread.compareAndSet(Thread.currentThread(), null);
                }
            }
        };
        thread.setDaemon(true);
        currentWatchDogThread.set(thread);
        thread.start();
    }

    private boolean isReconnectPossible(List<WaitingSkipReasonContainer> reconnectRequests) {
        if (!CFG_RECONNECT.CFG.isReconnectAllowedToInterruptResumableDownloads()) {
            return getSession().getControllers().size() == 0;
        }
        for (final SingleDownloadController con : getSession().getControllers()) {
            if (!con.getDownloadLink().isResumeable()) {
                /*
                 * running downloadLink is not resumable
                 */
                return false;
            }
            for (WaitingSkipReasonContainer reconnectRequest : reconnectRequests) {
                if (StringUtils.equals(reconnectRequest.getDlHost(), con.getDownloadLink().getHost())) {
                    /*
                     * running downloadLink is from same host as reconnectRequest (avoid loops from free/free registered)
                     */
                    return false;
                }
            }
        }
        return true;
    }

    private boolean isReconnectRequired(DownloadSession currentSession, WaitingSkipReasonContainer reconnectRequest) {
        if (!reconnectRequest.getProxySelector().isReconnectSupported()) {
            return false;
        }
        HashSet<DownloadLink> alreadyChecked = new HashSet<DownloadLink>();
        for (DownloadLink link : currentSession.getForcedLinks()) {
            if (alreadyChecked.add(link) == false || canRemove(link, true)) {
                continue;
            }
            ConditionalSkipReason conditionalSkipReason = link.getConditionalSkipReason();
            if (conditionalSkipReason == null) {
                continue;
            }
            if (conditionalSkipReason instanceof WaitWhileWaitingSkipReasonIsSet) {
                conditionalSkipReason = ((WaitWhileWaitingSkipReasonIsSet) conditionalSkipReason).getConditionalSkipReason();
            }
            if (!(conditionalSkipReason instanceof WaitingSkipReason)) {
                continue;
            }
            if (StringUtils.equals(reconnectRequest.getDlHost(), link.getHost())) {
                return true;
            }
        }
        if (currentSession.isForcedOnlyModeEnabled()) {
            /* only forced links are processed, the other activation requests are irrelevant */
            return false;
        }
        for (DownloadLink link : currentSession.getActivationRequests()) {
            if (alreadyChecked.add(link) == false || canRemove(link, false)) {
                continue;
            }
            ConditionalSkipReason conditionalSkipReason = link.getConditionalSkipReason();
            if (conditionalSkipReason == null) {
                continue;
            }
            if (conditionalSkipReason instanceof WaitWhileWaitingSkipReasonIsSet) {
                conditionalSkipReason = ((WaitWhileWaitingSkipReasonIsSet) conditionalSkipReason).getConditionalSkipReason();
            }
            if (!(conditionalSkipReason instanceof WaitingSkipReason)) {
                continue;
            }
            if (StringUtils.equals(reconnectRequest.getDlHost(), link.getHost())) {
                return true;
            }
        }
        return false;
    }

    private void validateProxyInfoHistory() {
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                checkReconnectRequests: {
                    if (!isAutoReconnectEnabled()) {
                        break checkReconnectRequests;
                    }
                    final ProxyInfoHistory proxyInfoHistory = currentSession.getProxyInfoHistory();
                    proxyInfoHistory.validate();
                    final List<WaitingSkipReasonContainer> reconnectRequests = proxyInfoHistory.list(WaitingSkipReason.CAUSE.IP_BLOCKED, null);
                    if (reconnectRequests == null) {
                        break checkReconnectRequests;
                    }
                    for (final WaitingSkipReasonContainer reconnectRequest : reconnectRequests) {
                        if (!isReconnectRequired(currentSession, reconnectRequest)) {
                            continue;
                        }
                        currentSession.compareAndSetSessionState(DownloadSession.SessionState.NORMAL, DownloadSession.SessionState.RECONNECT_REQUESTED);
                        if (currentSession.getSessionState() == DownloadSession.SessionState.RECONNECT_REQUESTED && isReconnectPossible(reconnectRequests)) {
                            invokeReconnect(currentSession);
                        }
                        /* a reconnect is required -> keep the requested state */
                        return;
                    }
                }
                currentSession.compareAndSetSessionState(DownloadSession.SessionState.RECONNECT_REQUESTED, DownloadSession.SessionState.NORMAL);
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return false;
            }
        });
    }

    private ReconnectThread invokeReconnect(final DownloadSession currentSession) {
        while (true) {
            Thread ret = reconnectThread.get();
            if (ret != null) {
                return (ReconnectThread) ret;
            }
            ret = new ReconnectThread();
            if (!reconnectThread.compareAndSet(null, ret)) {
                /* somebody else was faster -> the next loop returns his thread */
                continue;
            }
            if (isWatchDogThread() && currentSession != null) {
                currentSession.setSessionState(DownloadSession.SessionState.RECONNECT_RUNNING);
            }
            final DownloadWatchDogJob job = new DownloadWatchDogJob() {
                @Override
                public void execute(DownloadSession currentSession) {
                    currentSession.setSessionState(DownloadSession.SessionState.RECONNECT_RUNNING);
                    if (currentSession.getControllers().size() == 0) {
                        reconnectThread.get().start();
                        return;
                    }
                    /* abort the running downloads and check again later */
                    for (SingleDownloadController con : getSession().getControllers()) {
                        if (con.isAlive() && !con.isAborting()) {
                            con.abort();
                        }
                    }
                    enqueueJob(this);
                }

                @Override
                public void interrupt() {
                }

                @Override
                public boolean isHighPriority() {
                    return false;
                }
            };
            enqueueJob(job);
        }
    }

    public Reconnecter.ReconnectResult requestReconnect(boolean waitForResult) throws InterruptedException {
        final ReconnectThread thread = invokeReconnect(null);
        if (!waitForResult) {
            return null;
        }
        return thread.waitForResult();
    }

    protected DownloadWatchDogJob getCurrentDownloadWatchDogJob() {
        return currentWatchDogJob.get();
    }

    public void unSkipAllSkipped() {
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void interrupt() {
            }

            @Override
            public void execute(DownloadSession currentSession) {
                /* reset skipReasons */
                List<DownloadLink> unSkip = DownloadController.getInstance().getChildrenByFilter(new AbstractPackageChildrenNodeFilter<DownloadLink>() {
                    @Override
                    public int returnMaxResults() {
                        return 0;
                    }

                    @Override
                    public boolean acceptNode(DownloadLink node) {
                        return node.isSkipped();
                    }
                });
                unSkip(unSkip);
            }

            @Override
            public boolean isHighPriority() {
                return true;
            }
        });
    }

    protected boolean hasWaitingJobs() {
        synchronized (watchDogJobs) {
            return watchDogJobs.peek() != null;
        }
    }

    private synchronized void startDownloadWatchDog() {
        stateMachine.setStatus(RUNNING_STATE);
        Thread thread = new Thread() {
            @Override
            public void interrupt() {
                logger.log(new RuntimeException("WatchDog(" + getName() + ") interrupted!"));
                super.interrupt();
            }

            protected final void processJobs() {
                try {
                    setTempWatchDogJobThread(Thread.currentThread());
                    DownloadWatchDogJob peekLast = null;
                    while (true) {
                        final DownloadWatchDogJob job;
                        synchronized (watchDogJobs) {
                            if (peekLast == null) {
                                peekLast = watchDogJobs.peekLast();
                            }
                            job = watchDogJobs.poll();
                        }
                        if (job == null) {
                            /* no more jobs */
                            break;
                        }
                        try {
                            currentWatchDogJob.set(job);
                            job.execute(getSession());
                        } catch (final Throwable e) {
                            logger.log(e);
                        } finally {
                            currentWatchDogJob.set(null);
                        }
                        if (job == peekLast) {
                            // avoid loops for enqueueJob->enqueueJob->enqueueJob...
                            break;
                        }
                    }
                } finally {
                    setTempWatchDogJobThread(null);
                }
            }

            protected final int maxWaitTimeout = 5000;

            private void removeLink(final DownloadLink link, DownloadSession session) {
                session.removeActivationRequest(link);
                SingleDownloadController con = link.getDownloadLinkController();
                if (con == null) {
                    link.setConditionalSkipReason(null);
                    session.removeHistory(link);
                    return;
                }
                /* the link is running -> clean up after the download got detached */
                con.getJobsAfterDetach().add(new DownloadWatchDogJob() {
                    @Override
                    public void execute(DownloadSession currentSession) {
                        link.setConditionalSkipReason(null);
                        currentSession.removeHistory(link);
                    }

                    @Override
                    public void interrupt() {
                    }

                    @Override
                    public boolean isHighPriority() {
                        return false;
                    }
                });
                con.abort();
            }

            @Override
            public void run() {
                this.setName("WatchDog: downloadWatchDog");
                boolean jobExecuterStarted = false;
                DownloadControllerListener listener = null;
                AccountControllerListener accListener = null;
                HosterRuleControllerListener hrcListener = null;
                DefaultEventListener<ProxyEvent<AbstractProxySelectorImpl>> proxyListener = null;
                final AtomicLong lastStructureChange = new AtomicLong(-1l);
                final AtomicLong lastActivatorRequestRebuild = new AtomicLong(-1l);
                long waitedForNewActivationRequests = 0;
                @SuppressWarnings("unused")
                final FileBytesCache hardReferenceFileBytesCache = DownloadSession.getDownloadWriteCache();
                try {
                    unSkipAllSkipped();
                    ProxyController.getInstance().getEventSender().addListener(proxyListener = new DefaultEventListener<ProxyEvent<AbstractProxySelectorImpl>>() {
                        final DelayedRunnable delayer = new DelayedRunnable(1000, 5000) {
                                                          @Override
                                                          public void delayedrun() {
                                                              enqueueJob(new DownloadWatchDogJob() {
                                                                  @Override
                                                                  public void interrupt() {
                                                                  }

                                                                  @Override
                                                                  public void execute(DownloadSession currentSession) {
                                                                      /* reset CONNECTION_UNAVAILABLE */
                                                                      final List<DownloadLink> unSkip = DownloadController.getInstance().getChildrenByFilter(new AbstractPackageChildrenNodeFilter<DownloadLink>() {
                                                                          @Override
                                                                          public int returnMaxResults() {
                                                                              return 0;
                                                                          }

                                                                          @Override
                                                                          public boolean acceptNode(DownloadLink node) {
                                                                              return SkipReason.CONNECTION_UNAVAILABLE.equals(node.getSkipReason());
                                                                          }
                                                                      });
                                                                      unSkip(unSkip);
                                                                  }

                                                                  @Override
                                                                  public boolean isHighPriority() {
                                                                      return false;
                                                                  }
                                                              });
                                                          }
                                                      };

                        @Override
                        public void onEvent(ProxyEvent<AbstractProxySelectorImpl> event) {
                            delayer.delayedrun();
                        }
                    }, true);
                    HosterRuleController.getInstance().getEventSender().addListener(hrcListener = new HosterRuleControllerListener() {
                        private void removeAccountCache(final String host) {
                            if (host == null) {
                                return;
                            }
                            enqueueJob(new DownloadWatchDogJob() {
                                @Override
                                public void execute(DownloadSession currentSession) {
                                    currentSession.removeAccountCache(host);
                                }

                                @Override
                                public void interrupt() {
                                }

                                @Override
                                public boolean isHighPriority() {
                                    return false;
                                }
                            });
                        }

                        @Override
                        public void onRuleAdded(AccountUsageRule parameter) {
                            if (parameter != null) {
                                removeAccountCache(parameter.getHoster());
                            }
                        }

                        @Override
                        public void onRuleDataUpdate(AccountUsageRule parameter) {
                            if (parameter != null) {
                                removeAccountCache(parameter.getHoster());
                            }
                        }

                        @Override
                        public void onRuleRemoved(final AccountUsageRule parameter) {
                            if (parameter != null) {
                                removeAccountCache(parameter.getHoster());
                            }
                        }

                        @Override
                        public void onRuleStructureUpdate() {
                            // nothing internal changed. just order update
                        }
                    }, true);
                    AccountController.getInstance().getEventSender().addListener(accListener = new AccountControllerListener() {
                        @Override
                        public void onAccountControllerEvent(final AccountControllerEvent event) {
                            enqueueJob(new DownloadWatchDogJob() {
                                @Override
                                public void execute(DownloadSession currentSession) {
                                    currentSession.removeAccountCache(event.getAccount().getHoster());
                                    final AccountInfo ai = event.getAccount().getAccountInfo();
                                    if (ai == null) {
                                        return;
                                    }
                                    final List<MultiHostHost> supportedhosts = ai.getMultiHostSupportV2();
                                    if (supportedhosts == null) {
                                        return;
                                    }
                                    for (final MultiHostHost mhost : supportedhosts) {
                                        for (final String domain : mhost.getDomains()) {
                                            currentSession.removeAccountCache(domain);
                                        }
                                    }
                                }

                                @Override
                                public void interrupt() {
                                }

                                @Override
                                public boolean isHighPriority() {
                                    return false;
                                }
                            });
                        }
                    }, true);
                    DownloadController.getInstance().addListener(listener = new DownloadControllerListener() {
                        @Override
                        public void onDownloadControllerAddedPackage(FilePackage pkg) {
                            lastStructureChange.set(-1l);
                        }

                        @Override
                        public void onDownloadControllerStructureRefresh(FilePackage pkg) {
                            lastStructureChange.set(-1l);
                        }

                        @Override
                        public void onDownloadControllerStructureRefresh() {
                            lastStructureChange.set(-1l);
                        }

                        @Override
                        public void onDownloadControllerStructureRefresh(AbstractNode node, Object param) {
                            lastStructureChange.set(-1l);
                        }

                        @Override
                        public void onDownloadControllerRemovedPackage(final FilePackage fp) {
                            lastStructureChange.set(-1l);
                            enqueueJob(new DownloadWatchDogJob() {
                                @Override
                                public void execute(DownloadSession currentSession) {
                                    currentSession.setOnFileExistsAction(fp, null);
                                }

                                @Override
                                public void interrupt() {
                                }

                                @Override
                                public boolean isHighPriority() {
                                    return false;
                                }
                            });
                        }

                        @Override
                        public void onDownloadControllerRemovedLinklist(final List<DownloadLink> list) {
                            lastStructureChange.set(-1l);
                            enqueueJob(new DownloadWatchDogJob() {
                                @Override
                                public void execute(DownloadSession currentSession) {
                                    for (DownloadLink item : list) {
                                        removeLink(item, currentSession);
                                    }
                                }

                                @Override
                                public void interrupt() {
                                }

                                @Override
                                public boolean isHighPriority() {
                                    return false;
                                }
                            });
                        }

                        @Override
                        public void onDownloadControllerUpdatedData(DownloadLink dl, DownloadLinkProperty dlProperty) {
                            lastStructureChange.set(-1l);
                            if (dl != null) {
                                final DownloadLink link = dlProperty.getDownloadLink();
                                switch (dlProperty.getProperty()) {
                                case PRIORITY:
                                    enqueueJob(new DownloadWatchDogJob() {
                                        @Override
                                        public void execute(DownloadSession currentSession) {
                                            if (link.isEnabled()) {
                                                currentSession.incrementActivatorRebuildRequest();
                                            }
                                        }

                                        @Override
                                        public void interrupt() {
                                        }

                                        @Override
                                        public boolean isHighPriority() {
                                            return false;
                                        }
                                    });
                                    break;
                                case ENABLED:
                                    enqueueJob(new DownloadWatchDogJob() {
                                        @Override
                                        public void execute(DownloadSession currentSession) {
                                            if (!link.isEnabled()) {
                                                removeLink(link, currentSession);
                                            } else {
                                                currentSession.incrementActivatorRebuildRequest();
                                            }
                                        }

                                        @Override
                                        public void interrupt() {
                                        }

                                        @Override
                                        public boolean isHighPriority() {
                                            return false;
                                        }
                                    });
                                    break;
                                case SKIPPED:
                                    enqueueJob(new DownloadWatchDogJob() {
                                        @Override
                                        public void execute(DownloadSession currentSession) {
                                            if (link.getSkipReason() != null) {
                                                removeLink(link, currentSession);
                                            } else {
                                                currentSession.incrementActivatorRebuildRequest();
                                            }
                                        }

                                        @Override
                                        public void interrupt() {
                                        }

                                        @Override
                                        public boolean isHighPriority() {
                                            return false;
                                        }
                                    });
                                    break;
                                case FINAL_STATE:
                                    enqueueJob(new DownloadWatchDogJob() {
                                        @Override
                                        public void execute(DownloadSession currentSession) {
                                            if (link.getFinalLinkState() != null) {
                                                removeLink(link, currentSession);
                                            } else {
                                                currentSession.incrementActivatorRebuildRequest();
                                            }
                                        }

                                        @Override
                                        public void interrupt() {
                                        }

                                        @Override
                                        public boolean isHighPriority() {
                                            return false;
                                        }
                                    });
                                    break;
                                }
                            }
                        }

                        @Override
                        public void onDownloadControllerUpdatedData(FilePackage pkg, FilePackageProperty property) {
                            lastStructureChange.set(-1l);
                        }

                        @Override
                        public void onDownloadControllerUpdatedData(DownloadLink downloadlink) {
                            lastStructureChange.set(-1l);
                        }

                        @Override
                        public void onDownloadControllerUpdatedData(FilePackage pkg) {
                            lastStructureChange.set(-1l);
                        }
                    }, true);
                    while (DownloadWatchDog.this.stateMachine.isState(DownloadWatchDog.RUNNING_STATE, DownloadWatchDog.PAUSE_STATE)) {
                        long currentStructure = DownloadController.getInstance().getPackageControllerChanges();
                        long currentActivatorRequestRebuild = getSession().getActivatorRebuildRequest();
                        if (lastStructureChange.getAndSet(currentStructure) != currentStructure || lastActivatorRequestRebuild.getAndSet(currentActivatorRequestRebuild) != currentActivatorRequestRebuild) {
                            final AtomicInteger skippedLinksCounterTmp = new AtomicInteger(0);
                            List<DownloadLink> links = DownloadController.getInstance().getChildrenByFilter(new AbstractPackageChildrenNodeFilter<DownloadLink>() {
                                @Override
                                public int returnMaxResults() {
                                    return 0;
                                }

                                @Override
                                public boolean acceptNode(DownloadLink node) {
                                    if (node.isSkipped()) {
                                        skippedLinksCounterTmp.incrementAndGet();
                                        return false;
                                    }
                                    if (canRemove(node, false)) {
                                        node.setConditionalSkipReason(null);
                                        return false;
                                    }
                                    return true;
                                }
                            });
                            getSession().setSkipCounter(skippedLinksCounterTmp.get());
                            getSession().setActivationRequests(new CopyOnWriteArrayList<DownloadLink>(sortActivationRequests(links)));
                            eventSender.fireEvent(new DownloadWatchdogEvent(this, DownloadWatchdogEvent.Type.DATA_UPDATE));
                        }
                        try {
                            final ArrayList<DownloadLink> downloadLinksWithConditionalSkipReasons = new ArrayList<DownloadLink>();
                            while (true) {
                                final int before = getSession().getControllers().size();
                                validateProxyInfoHistory();
                                processJobs();
                                final Set<DownloadLink> finalLinkStateLinks = finalizeConditionalSkipReasons(getSession(), downloadLinksWithConditionalSkipReasons);
                                handleFinalLinkStates(finalLinkStateLinks, getSession(), logger, null);
                                if (before == getSession().getControllers().size()) {
                                    break;
                                }
                            }
                            if (newDLStartAllowed(getSession())) {
                                DownloadWatchDog.this.activateDownloads(downloadLinksWithConditionalSkipReasons);
                            }
                            if (getSession().getControllers().size() == 0 && getSession().isStopMarkReached()) {
                                logger.info("Wait at least " + Math.max(0, (maxWaitTimeout - waitedForNewActivationRequests)) + " for new change in StopMark(reached)");
                            } else if (getSession().getControllers().size() > 0 || lastActivatorRequestRebuild.get() != getSession().getActivatorRebuildRequest() || lastStructureChange.get() != DownloadController.getInstance().getPackageControllerChanges() || getSession().isActivationRequestsWaiting()) {
                                waitedForNewActivationRequests = 0;
                            } else {
                                logger.info("Wait at least " + Math.max(0, (maxWaitTimeout - waitedForNewActivationRequests)) + " for new ActivationRequests");
                            }
                            if (waitedForNewActivationRequests > maxWaitTimeout) {
                                logger.info("Waited " + waitedForNewActivationRequests + " but no new ActivationRequests are available->Stop DownloadWatchDog");
                                break;
                            }
                        } catch (final Exception e) {
                            logger.log(e);
                        }
                        try {
                            int round = 0;
                            synchronized (WATCHDOGLOCK) {
                                while (!hasWaitingJobs() && ++round < 4 && DownloadWatchDog.this.stateMachine.isState(DownloadWatchDog.RUNNING_STATE, DownloadWatchDog.PAUSE_STATE)) {
                                    long currentTimeStamp = System.currentTimeMillis();
                                    WATCHDOGLOCK.wait(1250);
                                    waitedForNewActivationRequests += System.currentTimeMillis() - currentTimeStamp;
                                    if ((getSession().isActivationRequestsWaiting() == false && DownloadWatchDog.this.getActiveDownloads() == 0)) {
                                        /*
                                         * it's important that this if statement gets checked after wait!, else we will loop through without
                                         * waiting for new links/user interaction
                                         */
                                        break;
                                    }
                                }
                            }
                        } catch (final InterruptedException e) {
                            logger.log(e);
                        }
                    }
                    enqueueJob(new DownloadWatchDogJob() {
                        @Override
                        public void execute(DownloadSession currentSession) {
                            stateMachine.setStatus(STOPPING_STATE);
                        }

                        @Override
                        public void interrupt() {
                        }

                        @Override
                        public boolean isHighPriority() {
                            return true;
                        }
                    });
                    DownloadController.getInstance().removeListener(listener);
                    AccountController.getInstance().getEventSender().removeListener(accListener);
                    HosterRuleController.getInstance().getEventSender().removeListener(hrcListener);
                    ProxyController.getInstance().getEventSender().removeListener(proxyListener);
                    logger.info("DownloadWatchDog: stopping");
                    synchronized (DownloadWatchDog.this) {
                        startDownloadJobExecuter();
                        jobExecuterStarted = !isWatchDogThread();
                    }
                    /* stop all remaining downloads */
                    abortAllSingleDownloadControllers();
                    /* finalize links */
                    final Set<DownloadLink> finalLinkStateLinks = finalizeConditionalSkipReasons(getSession(), null);
                    handleFinalLinkStates(finalLinkStateLinks, getSession(), logger, null);
                    /* clear Status */
                    clearDownloadListStatus();
                    /* clear blocked Accounts */
                    // TODO:AccountController.getInstance().removeAccountBlocked(null);
                    /* unpause downloads */
                    pauseDownloadWatchDog(false);
                    if (getSession().isStopMarkReached()) {
                        /* remove stopsign if it has been reached */
                        getSession().setStopMark(STOPMARK.NONE);
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                    enqueueJob(new DownloadWatchDogJob() {
                        @Override
                        public void execute(DownloadSession currentSession) {
                            stateMachine.setStatus(STOPPING_STATE);
                        }

                        @Override
                        public void interrupt() {
                        }

                        @Override
                        public boolean isHighPriority() {
                            return true;
                        }
                    });
                } finally {
                    if (jobExecuterStarted == false) {
                        synchronized (DownloadWatchDog.this) {
                            startDownloadJobExecuter();
                        }
                    }
                    /* full stop reached */
                    logger.info("DownloadWatchDog: stopped");
                    /* clear session */
                    final DownloadSession latestSession = getSession();
                    enqueueJob(new DownloadWatchDogJob() {
                        @Override
                        public void execute(DownloadSession currentSession) {
                            try {
                                latestSession.removeHistory((DownloadLink) null);
                                latestSession.removeAccountCache(null);
                                latestSession.clearPluginCache();
                                latestSession.getActivationRequests().clear();
                                latestSession.getForcedLinks().clear();
                                latestSession.setOnFileExistsAction(null, null);
                            } finally {
                                stateMachine.setStatus(STOPPED_STATE);
                            }
                        }

                        @Override
                        public void interrupt() {
                        }

                        @Override
                        public boolean isHighPriority() {
                            return true;
                        }
                    });
                }
            }
        };
        thread.setDaemon(true);
        currentWatchDogThread.set(thread);
        thread.start();
    }

    /**
     * tell the DownloadWatchDog to stop all running Downloads
     */
    public void stopDownloads() {
        final DownloadWatchDogJob job = getCurrentDownloadWatchDogJob();
        if (job != null) {
            job.interrupt();
        }
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                if (DownloadWatchDog.this.stateMachine.isFinal() || DownloadWatchDog.this.stateMachine.isStartState()) {
                    /* not downloading */
                    return;
                }
                if (DownloadWatchDog.this.stateMachine.isState(STOPPING_STATE)) {
                    /* download is already in stopping, stopped state */
                    return;
                }
                /* we now want to stop all downloads */
                stateMachine.setStatus(STOPPING_STATE);
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return true;
            }
        });
    }

    /**
     * toggles between start/stop states
     */
    public void toggleStartStop() {
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                if (stateMachine.isStartState() || stateMachine.isFinal()) {
                    /* download is in idle or stopped state */
                    DownloadWatchDog.this.startDownloads();
                } else {
                    /* download can be stopped */
                    DownloadWatchDog.this.stopDownloads();
                }
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return true;
            }
        });
    }

    @Deprecated
    /**
     * @Deprecated: User getEventSender Instead
     */
    public StateMachine getStateMachine() {
        return this.stateMachine;
    }

    public void onNewFile(Object obj, final File[] fileList) {
        if (!JsonConfig.create(GeneralSettings.class).isAutoOpenContainerAfterDownload()) {
            /* Do nothing */
            return;
        }
        if (fileList == null || fileList.length <= 0) {
            return;
        }
        /* check if extracted files are container files */
        final List<String> files = new LinkedList<String>();
        for (final File file : fileList) {
            if (!file.isFile()) {
                continue;
            }
            files.add(file.toURI().toString());
        }
        if (files.size() == 0) {
            return;
        }
        final String source;
        if (obj instanceof SingleDownloadController) {
            final String url = ((SingleDownloadController) obj).getDownloadLink().getContentUrlOrPatternMatcher();
            source = url;
        } else {
            source = null;
        }
        final List<PluginsC> containerPlugins = ContainerPluginController.getInstance().list();
        final Iterator<String> it = files.iterator();
        nextFile: while (it.hasNext()) {
            final String file = it.next();
            for (final PluginsC pCon : containerPlugins) {
                if (pCon.canHandle(file)) {
                    continue nextFile;
                }
            }
            it.remove();
        }
        if (files.size() == 0) {
            return;
        }
        TaskQueue.getQueue().addAsynch(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                final StringBuilder sb = new StringBuilder();
                for (final String file : files) {
                    if (sb.length() > 0) {
                        sb.append("\r\n");
                    }
                    sb.append(file);
                }
                LinkCollector.getInstance().addCrawlerJob(new LinkCollectingJob(LinkOriginDetails.getInstance(LinkOrigin.DOWNLOADED_CONTAINER, source), sb.toString()));
                return null;
            }

            @Override
            protected boolean allowAsync() {
                return true;
            }
        });
    }

    /** Contains the file-paths we got permission to write in, together with the time of the (last) successful check. */
    private final static HashMap<String, Long> accessChecks         = new HashMap<String, Long>();
    /**
     * A confirmed-writable folder is trusted for this long. Afterwards the folder is checked again, because it may have been removed or its
     * permissions may have changed (e.g. unplugged drive).
     */
    private final static long                  ACCESS_CHECK_MAX_AGE = TimeUnit.HOURS.toMillis(24);

    /**
     * Marks the given folder as confirmed-writable in the access-check cache. </br> This is the single point that writes to
     * {@link #accessChecks}; derive the cache key only here.
     */
    private static void markFolderWritable(final File parentFolder) {
        accessChecks.put(parentFolder.getAbsolutePath(), Time.systemIndependentCurrentJVMTimeMillis());
    }

    /** @return true if we already confirmed (not longer than {@link #ACCESS_CHECK_MAX_AGE} ago) that we can write into the given folder. */
    private static boolean isFolderKnownWritable(final File parentFolder) {
        final String key = parentFolder.getAbsolutePath();
        final Long checkedTimestamp = accessChecks.get(key);
        if (checkedTimestamp == null) {
            return false;
        }
        if (Time.systemIndependentCurrentJVMTimeMillis() - checkedTimestamp.longValue() > ACCESS_CHECK_MAX_AGE) {
            /* Confirmation is too old -> check again. */
            accessChecks.remove(key);
            return false;
        }
        return true;
    }

    /**
     * Ensures that the file we are about to download can be written to the desired destination. </br> Also takes care about these cases: <br>
     * - File already exists </br> - Mirror of file is currently already being downloaded <br>
     * - [Windows] File name is too long <br>
     * - [Windows] File path is too long
     */
    public void localFileCheck(final SingleDownloadController controller, final ExceptionRunnable runOkay, final ExceptionRunnable runFailed) throws Exception {
        final NullsafeAtomicReference<Object> asyncResult = new NullsafeAtomicReference<Object>(null);
        enqueueJob(new DownloadWatchDogJob() {
            private void check(final DownloadSession session, final SingleDownloadController controller) throws Exception {
                if (controller.isAborting()) {
                    throw new InterruptedException("Controller is aborted");
                }
                /**
                 * this call may return an unsafe(not final) fileOutput, but it is the fileOutput we want to start with
                 */
                final File fileOutput = controller.getFileOutput(false, true);
                final boolean fileExists = fileOutput.exists();
                /* Only an existing file system entry can be a directory -> in the common case (file does not exist) this saves a call. */
                if (fileExists && fileOutput.isDirectory()) {
                    controller.getLogger().severe("fileOutput is a directory " + fileOutput);
                    throw new SkipReasonException(SkipReason.INVALID_DESTINATION, "Invalid download destination: Folder with name of this file already exists!");
                }
                final File safeFinalFileDestination = controller.getFileOutput(true, true);
                if (config.isAllowUnsafeFileNameForFileExistsCheck() == false && controller.getDownloadInstance() == null && safeFinalFileDestination == null) {
                    /**
                     * Don't proceed when we do not have a finalFilename yet or do not want to download _now_.
                     */
                    return;
                }
                if (fileExists) {
                    /* File already exists -> Handle that case */
                    fileAlreadyExistsHandling(session, controller, null);
                    /* If no exception is thrown above, this means that the user has selected "overwrite already existing file". */
                }
                if (controller.isAborting()) {
                    throw new InterruptedException("Controller is aborted");
                }
                /* Check if a mirror of this item is currently being downloaded. */
                fileMirrorAlreadyDownloadingDetection(session, controller);
                if (controller.isAborting()) {
                    throw new InterruptedException("Controller is aborted");
                }
                /* Ensure we can write to the destination and handle too long filenames. */
                fileDownloadDestinationWriteCheck(session, controller);
            }

            @Override
            public void execute(DownloadSession currentSession) {
                try {
                    check(currentSession, controller);
                    if (runOkay != null) {
                        runOkay.run();
                    }
                    synchronized (asyncResult) {
                        asyncResult.set(asyncResult);
                        asyncResult.notifyAll();
                    }
                } catch (final Exception e) {
                    try {
                        if (runFailed != null) {
                            runFailed.run();
                        }
                    } catch (final Throwable e2) {
                        logger.log(e2);
                    } finally {
                        synchronized (asyncResult) {
                            asyncResult.set(e);
                            asyncResult.notifyAll();
                        }
                    }
                }
            }

            @Override
            public void interrupt() {
                final Thread watchDogThread = getWatchDogThread();
                if (getCurrentDownloadWatchDogJob() == this && watchDogThread != null) {
                    watchDogThread.interrupt();
                }
            }

            @Override
            public boolean isHighPriority() {
                return true;
            }
        });
        Object ret = null;
        while (true) {
            synchronized (asyncResult) {
                ret = asyncResult.get();
                if (ret != null) {
                    break;
                }
                asyncResult.wait();
            }
        }
        if (asyncResult == ret) {
            return;
        }
        if (ret instanceof PluginException) {
            throw (PluginException) ret;
        }
        if (ret instanceof Exception) {
            throw (Exception) ret;
        }
        throw new WTFException("WTF? Result: " + ret);
    }

    public String getFilename(final SingleDownloadController controller) {
        final MirrorDetectionDecision mirrorDetectionDecision = config.getMirrorDetectionDecision();
        final DownloadLink downloadLink = controller.getDownloadLink();
        /* Check if a mirror of this item is currently being downloaded. */
        if (!MirrorDetectionDecision.SAFE.equals(mirrorDetectionDecision)) {
            /**
             * we use fileName from fileOutput (may be unsafe)
             */
            return controller.getFileOutput(false, true).getName();
        }
        /**
         * this returns a safe checkFile or null (if not available yet, eg no final/forcedFileName set)
         */
        final File safeFinalFileDestination = controller.getFileOutput(true, true);
        if (safeFinalFileDestination != null) {
            /**
             * we use fileName from checkFile
             */
            return safeFinalFileDestination.getName();
        }
        /**
         * we use final/forcedFileName from downloadLink
         */
        return downloadLink.getName(true, false);
    }

    /**
     * Builds a non-existing "jd_accessCheck_..." file inside the given folder whose name has exactly {@code targetNameLength} characters
     * (but at least "jd_accessCheck_" + 4 random digits, regardless of {@code targetNameLength}). </br> The random digits both provide
     * uniqueness and are used to pad the name to the requested length. </br> Used for the destination write-test: the test file is named
     * differently from the real download file (so a test file that cannot be deleted right away does not collide with the real file later)
     * but reproduces the same name length so that a too-long path/filename is still detected reliably.
     */
    private static File createAccessCheckFile(final File parentFolder, final int targetNameLength) {
        final String prefix = "jd_accessCheck_";
        /* At least 4 random digits, regardless of targetNameLength; otherwise use as many as needed to reach the requested length. */
        int randomDigits = targetNameLength - prefix.length();
        if (randomDigits < 4) {
            randomDigits = 4;
        }
        final java.util.Random random = new java.util.Random();
        final StringBuilder name = new StringBuilder(prefix.length() + randomDigits);
        name.append(prefix);
        for (int i = 0; i < randomDigits; i++) {
            name.append(random.nextInt(10));
        }
        return new File(parentFolder, name.toString());
    }

    /**
     * @return the maximum allowed filename length (number of characters) for a file placed directly inside the given parent folder on
     *         Windows, honouring {@link GeneralSettings#isLongWindowsPathsAllowed()}. </br> May be &lt;= 0 when the parent folder path
     *         alone already exhausts the classic MAX_PATH limit.
     */
    private static int getMaxWindowsFilenameLength(final File parentFolder) {
        final int maxPathLengthWindows = 259; // 260 but only 259 usable
        final int maxSegmentLength = 255; // NTFS per-segment limit; a hard filesystem limit that ALWAYS applies to a single filename.
        if (JsonConfig.create(GeneralSettings.class).isLongWindowsPathsAllowed()) {
            /* Long paths allowed -> the total path length is not the limit; only the per-segment limit applies to the filename. */
            return maxSegmentLength;
        }
        /*
         * Classic MAX_PATH -> the whole path must fit; -1 accounts for the path separator between the parent folder and the filename. The
         * per-segment limit still applies on top of that, so cap at it - otherwise a very short parent path could yield a budget above 255.
         */
        return Math.min(maxSegmentLength, maxPathLengthWindows - parentFolder.getAbsolutePath().length() - 1);
    }

    /**
     * Ensures that the file we are about to download can be written to the desired destination. </br> This combines the former
     * destination-write-check and too-long-filename handling into a single step: <br>
     * 1. Create the directory tree up until the download folder and perform a write-test on the target file (via
     * {@link FilePathChecker#createFilePath(File, jd.controlling.downloadcontroller.FilePathChecker.CheckFlag...)}). <br>
     * 2. Verify that we are allowed to write into that folder (result cached via {@link #accessChecks}). <br>
     * 3. [Windows] Check for- and handle too long filenames (offer auto-shortening). <br>
     * Path/permission problems detected via {@link FilePathChecker} are raised as {@link BadFilePathException} and mapped to a
     * {@link SkipReasonException} centrally in {@link SingleDownloadController}.
     */
    public void fileDownloadDestinationWriteCheck(final DownloadSession session, final SingleDownloadController controller) throws SkipReasonException, DeferredRunnableException, PluginException, InterruptedException, IOException {
        final DownloadLink downloadLink = controller.getDownloadLink();
        /**
         * this call may return an unsafe(not final) fileOutput, but it is the fileOutput we want to start with
         */
        final File fileOutput = controller.getFileOutput(false, true);
        /* Check- and prepare path and ensure we can write into the download folder. */
        /* Create directory tree up until file. */
        final File parentFolder = fileOutput.getParentFile();
        final boolean isWindows = CrossSystem.isWindows();
        if (!isWindows) {
            /* Non Windows OS -> Trust cache since we do not expect issues due to long paths. */
            if (isFolderKnownWritable(parentFolder)) {
                return;
            }
            if (fileOutput.exists()) {
                /* Cheap way: Assume that file is writable. */
                markFolderWritable(parentFolder);
                return;
            }
        }
        /**
         * We are close to the finish line! </br> We know that we can write in the directory but can we write the specific file we want to
         * write? </br> The filename could still be too long!
         */
        /**
         * Determine the length of the longest filename the plugin could write (the final file or one of its process files, e.g. the .part
         * file). We reproduce this worst-case name length in the write-test so that a too-long path/filename is detected reliably. <br>
         * In most cases the longest name will be that of our temporary .part file.
         */
        int longestNameLength = fileOutput.getName().length();
        final List<File> processFiles = downloadLink.getDefaultPlugin().listProcessFiles(downloadLink);
        if (processFiles != null) {
            /* First only compare the name lengths, this needs no filesystem access. */
            for (final File file : processFiles) {
                longestNameLength = Math.max(longestNameLength, file.getName().length());
            }
        }
        if (isWindows && isFolderKnownWritable(parentFolder) && longestNameLength <= getMaxWindowsFilenameLength(parentFolder)) {
            /*
             * Folder already confirmed writable. On Windows we would normally still run the write-test to catch a too-long filename, but
             * when the worst-case name length already fits into the current path limit there is nothing left to verify -> skip the
             * write-test just like on other operating systems.
             */
            return;
        }
        if (processFiles != null) {
            /*
             * The write-test is needed. A process file that already exists has proven that its name length can be written, so it must not
             * raise the length of the test file. Only now it is worth checking which of the files exist.
             */
            longestNameLength = fileOutput.getName().length();
            for (final File file : processFiles) {
                if (file.getName().length() > longestNameLength && !file.exists()) {
                    longestNameLength = file.getName().length();
                }
            }
        }
        /*
         * Perform the write-test with a file that is named differently from the real target file but has the same name length. This way a
         * test file that (for whatever reason) could not be deleted right away does not collide with the real download file later on.
         */
        final File writeTest = createAccessCheckFile(parentFolder, longestNameLength);
        /*
         * Perform the write-test. On success - or the deletion-failure edge case - tooLongFilename stays null. If the folder itself is
         * writable but the filename is too long on Windows, tooLongFilename holds that exception so the shortening logic below can run. Any
         * other failure means the folder is not writable and is thrown right here (so the cache is NOT updated for it).
         */
        BadFilePathException tooLongFilename = null;
        try {
            FilePathChecker.createFilePath(writeTest, new CheckFlag[] { CheckFlag.IS_FILE, CheckFlag.CHECK_FILE_WRITE, CheckFlag.CHECK_FILE_FOR_TOO_LONG_FILENAME, CheckFlag.ERROR_ON_WRITE_TEST_DELETION_FAILURE });
        } catch (final BadFilePathException e) {
            if (e.getReason() == PathFailureReason.WRITE_TEST_DELETION_FAILURE) {
                /*
                 * Edge case: Write-test succeeded but the test-file could not be deleted right away -> schedule a late deletion and treat
                 * the write-test as successful.
                 */
                controller.getJobsAfterDetach().add(new DownloadWatchDogJob() {
                    @Override
                    public void interrupt() {
                    }

                    @Override
                    public void execute(DownloadSession currentSession) {
                        writeTest.delete();
                    }

                    @Override
                    public boolean isHighPriority() {
                        return false;
                    }
                });
            } else if (e.getReason() == PathFailureReason.PATH_SEGMENT_TOO_LONG) {
                /* Folder path was created successfully and our problem is solely the (too long) file name. */
                tooLongFilename = e;
            } else {
                /* Something not related to the length of our filename happened (e.g. permission problem) -> Throw exception. */
                throw e;
            }
        }
        /*
         * INVARIANT: reaching this point always means the parent folder is confirmed writable (the write-test passed, or only the filename
         * was too long). Any other failure was thrown above. Keep this the ONLY place that updates the access-check cache.
         */
        markFolderWritable(parentFolder);
        if (tooLongFilename == null) {
            /* Destination is fully writable -> done. */
            return;
        }
        final BadFilePathException bfpe = tooLongFilename;
        if (!isWindows) {
            /*
             * Auto shortening of filenames is a Windows only feature. On other systems we do not apply any filename length limit of our own
             * -> report the error of the write-test.
             */
            throw bfpe;
        }
        /* Looks like filename might be too long -> Check if writing a shortened filename would be possible. */
        int maxFilenameLength = getMaxWindowsFilenameLength(parentFolder);
        final String filename = getFilename(controller);
        if (longestNameLength > filename.length()) {
            /*
             * The write-test was performed with the longest file the plugin could write (e.g. the .part file), which is longer than the
             * filename -> the filename needs to be smaller to compensate for that.
             */
            maxFilenameLength = maxFilenameLength - (longestNameLength - filename.length());
        }
        final ParsedFilename pfname = new jd.plugins.ParsedFilename(filename);
        if (pfname.isMultipartArchive()) {
            /* Do not offer auto filename shortening for multipart archive files. User needs to do this manually. */
            throw bfpe;
        }
        if (maxFilenameLength <= 0) {
            /*
             * Does not look like too long filename -> Write-fail must have happened for a different reason, possibly permission problem ->
             * Give up.
             */
            logger.info("Filename is too long according to Exception but it's not");
            throw bfpe;
        }
        /* Shorten filename and try again */
        final String autoShortenedFilename = LinknameCleaner.shortenFilename(pfname, maxFilenameLength);
        if (autoShortenedFilename == null) {
            /* This should never happen! */
            logger.info("Shortening this filename is not possible");
            throw bfpe;
        }
        /*
         * Edge case: the shortened filename may already exist in the download folder. The file-exists check earlier in localFileCheck ran
         * on the original (un-shortened) filename, so the shortened name was never checked. Now that the final filename is known, route
         * this through the regular file-already-exists handling so the user's configured policy (overwrite / auto-rename / skip / set to
         * successful mirror / ask) is honoured instead of always skipping. We pass maxFilenameLength so any filename generated there (e.g.
         * the auto-rename "_2" variant) stays within the length limit that made us shorten in the first place.
         */
        final File autoShortenedFile = new File(writeTest.getParent(), autoShortenedFilename);
        if (autoShortenedFile.exists()) {
            logger.info("File with auto shortened filename already exists -> assume that file was created by JD -> route to file-already-exists handling");
            /* Pass the shortened file explicitly so the handling acts on it (the link still carries the un-shortened name). */
            fileAlreadyExistsHandling(session, controller, null, maxFilenameLength, autoShortenedFile);
            return;
        }
        /* The final (possibly custom) shortened filename to use; starts with the auto-shortened suggestion. */
        String shortenedFilename = autoShortenedFilename;
        final IfFilenameTooLongAction action;
        decideAction: {
            /*
             * Per-package remembered action first, then the session-wide one: the user may pick a per-package action for one or more
             * packages before later deciding "don't ask again this session", so the more specific per-package choice must take precedence.
             * getOnFilenameTooLongAction resolves the per-package action and falls back to the global default (then ASK).
             */
            final IfFilenameTooLongAction packageAction = session.getOnFilenameTooLongAction(downloadLink.getFilePackage());
            if (packageAction != null && packageAction != IfFilenameTooLongAction.ASK_FOR_EACH_FILE) {
                /* Pre-selected per-package action (or global default) -> do not ask. */
                action = packageAction;
                break decideAction;
            }
            /* Then the session-wide "don't ask again during this session" decision. */
            final IfFilenameTooLongAction sessionWide = session.getSessionOnFilenameTooLongAction();
            if (sessionWide != null && sessionWide != IfFilenameTooLongAction.ASK_FOR_EACH_FILE) {
                action = sessionWide;
                break decideAction;
            }
            /* Ask the user. */
            final IfFilenameTooLongDialogInterface io = new IfFilenameTooLongDialog(downloadLink, pfname, shortenedFilename).show();
            switch (io.getCloseReason()) {
            case INTERRUPT:
                throw new InterruptedException("IfFilenameTooLongDialog Interrupted");
            case OK:
                break;
            default:
                /* Timeout or dialog closed/cancelled -> do not auto rename, give up (skip). */
                action = IfFilenameTooLongAction.SKIP_FILE;
                break decideAction;
            }
            IfFilenameTooLongAction dialogAction = io.getAction();
            if (dialogAction == null) {
                /* Fallback */
                dialogAction = IfFilenameTooLongAction.SKIP_FILE;
            }
            shortenedFilename = io.getNewFilename();
            /* Per-package "remember" checkbox -> apply the chosen action to the remaining items of this package. */
            if (io.isRememberForPackageSelected()) {
                session.setOnFileFilenameTooLongAction(downloadLink.getFilePackage(), dialogAction);
            } else {
                session.setOnFileFilenameTooLongAction(downloadLink.getFilePackage(), null);
            }
            /*
             * "Don't ask again during this session" checkbox -> store session-wide (in the DownloadSession). A custom filename or the
             * global "do not show again" option disables the checkbox, so it only reports true for a plain auto-shortened rename or skip.
             */
            if (io.isDontAskAgainThisSessionSelected() && dialogAction != IfFilenameTooLongAction.ASK_FOR_EACH_FILE) {
                session.setSessionOnFilenameTooLongAction(dialogAction);
            }
            /*
             * Framework "Do not ask again" checkbox -> store as the global default. Only auto shortened rename or skip may be stored; a
             * custom filename is a per-file decision (the dialog disables the checkbox in that case).
             */
            if (io.isDontShowAgainSelected()) {
                switch (dialogAction) {
                case AUTO_RENAME:
                case SKIP_FILE:
                    config.setIfFilenameTooLongAction(dialogAction);
                    break;
                default:
                    break;
                }
            }
            action = dialogAction;
        }
        switch (action) {
        case AUTO_RENAME:
            break;
        case SKIP_FILE:
        default:
            /* No rename wished -> give up (skip the too-long file). */
            throw bfpe;
        }
        if (shortenedFilename == null) {
            /* Dialog returned no filename -> do not rename. */
            throw bfpe;
        }
        /* Continue with shortened filename */
        controller.setSessionDownloadFilename(shortenedFilename);
        downloadLink.setForcedFileName(shortenedFilename);
        downloadLink.setChunksProgress(null);
    }

    /**
     * Checks whether another running download currently holds the target file. </br> If that download is a mirror from the same package,
     * this link is postponed via {@link MirrorLoading}. Otherwise the regular file-already-exists handling decides.
     */
    public void fileMirrorAlreadyDownloadingDetection(final DownloadSession session, final SingleDownloadController controller) throws ConditionalSkipReasonException, SkipReasonException, DeferredRunnableException, PluginException, InterruptedException {
        final MirrorDetectionDecision mirrorDetectionDecision = config.getMirrorDetectionDecision();
        final DownloadLink downloadLink = controller.getDownloadLink();
        /* Check if a mirror of this item is currently being downloaded. */
        final String filename = getFilename(controller);
        final File fileOutput = controller.getFileOutput(false, true);
        /* Check if a mirror is currently being downloaded or if a file with the same name is currently being downloaded. */
        for (final SingleDownloadController downloadController : session.getControllers()) {
            if (downloadController == controller) {
                continue;
            }
            final DownloadLink block = downloadController.getDownloadLink();
            if (block == downloadLink) {
                continue;
            }
            if (!session.getFileAccessManager().isLockedBy(fileOutput, downloadController)) {
                continue;
            }
            /* fileOutput is already locked */
            if (block.getFilePackage() == downloadLink.getFilePackage() && isMirrorCandidate(downloadLink, filename, block, mirrorDetectionDecision)) {
                /* File is from same package */
                throw new ConditionalSkipReasonException(new MirrorLoading(block));
            }
            final DownloadLink fileInProgress = block;
            /* Calling this function should throw an exception. */
            fileAlreadyExistsHandling(session, controller, fileInProgress);
            return;
        }
    }

    public void fileAlreadyExistsHandling(final DownloadSession session, final SingleDownloadController controller, final DownloadLink fileInProgress) throws SkipReasonException, DeferredRunnableException, PluginException, InterruptedException {
        /* No filename length restriction (-1) and no explicit target file -> classic behaviour. */
        fileAlreadyExistsHandling(session, controller, fileInProgress, -1, null);
    }

    public void fileAlreadyExistsHandling(final DownloadSession session, final SingleDownloadController controller, final DownloadLink fileInProgress, final int maxFilenameLength) throws SkipReasonException, DeferredRunnableException, PluginException, InterruptedException {
        fileAlreadyExistsHandling(session, controller, fileInProgress, maxFilenameLength, null);
    }

    /**
     * Builds a single auto-rename candidate filename of the form "&lt;base&gt;_&lt;counter&gt;&lt;extensionWithDot&gt;". When
     * {@code maxFilenameLength > 0} the base part is truncated so that the whole name still fits into that many characters. Returns null
     * when even a minimal name (empty base) would not fit.
     */
    static String buildAutoRenameCandidate(final String baseNameWithoutExtension, final String extensionWithDot, final int counter, final int maxFilenameLength) {
        final String suffix = "_" + counter;
        String baseName = baseNameWithoutExtension;
        if (maxFilenameLength > 0) {
            final int allowedBaseLength = maxFilenameLength - suffix.length() - extensionWithDot.length();
            if (allowedBaseLength <= 0) {
                return null;
            }
            if (baseName.length() > allowedBaseLength) {
                baseName = baseName.substring(0, allowedBaseLength);
            }
        }
        return baseName + suffix + extensionWithDot;
    }

    /**
     * Generates a suggested auto-rename filename for the given file by appending "_2", "_3", ... until a name is found that does not yet
     * exist on disk. Uses existence-only checks (no lock detection) and is limited to a small number of attempts - it is meant for preview
     * purposes only (e.g. as the pre-filled suggestion in the "file already exists" dialog). Returns null when no free name could be found
     * within the attempt limit or none fits {@code maxFilenameLength}.
     */
    static String generateAutoRenameSuggestion(final File fileOutput, final int maxFilenameLength) {
        return findFreeAutoRenameName(fileOutput, maxFilenameLength, null, null);
    }

    /**
     * Searches a free auto-rename filename for the given file by appending "_2", "_3", ... (continuing the counter if the name already ends
     * with "_&lt;n&gt;"). The number of attempts is limited so we never loop (near) endlessly when many variants already exist.
     *
     * @param session
     *            When not null, a candidate is only free if it does not exist on disk AND is not locked by another download controller of
     *            this session (see {@link #isAutoRenameTargetFree(DownloadSession, SingleDownloadController, File)}). When null, only the
     *            existence on disk is checked.
     * @param controller
     *            The controller to ignore when checking locks; only used if session is not null.
     * @return a free filename, or null if none was found within the attempt limit or none fits {@code maxFilenameLength}.
     */
    static String findFreeAutoRenameName(final File fileOutput, final int maxFilenameLength, final DownloadSession session, final SingleDownloadController controller) {
        final String splitName[] = CrossSystem.splitFileName(fileOutput.getName());
        String extension = splitName[1];
        if (extension == null) {
            extension = "";
        } else {
            extension = "." + extension;
        }
        String baseName = splitName[0];
        int duplicateFilenameCounter = 2;
        final String alreadyDuplicated = new Regex(baseName, ".*_(\\d{1,5})$").getMatch(0);
        if (alreadyDuplicated != null) {
            /* it seems the file already got auto renamed! */
            duplicateFilenameCounter = Integer.parseInt(alreadyDuplicated) + 1;
            baseName = new Regex(baseName, "(.*)_\\d+$").getMatch(0);
        }
        final File downloadPath = fileOutput.getParentFile();
        final int maxAutoRenameAttempts = 10;
        for (int autoRenameAttempt = 0; autoRenameAttempt < maxAutoRenameAttempts; autoRenameAttempt++) {
            final String candidate = buildAutoRenameCandidate(baseName, extension, duplicateFilenameCounter++, maxFilenameLength);
            if (candidate == null) {
                /* Not even a minimal renamed filename fits into the allowed length -> give up. */
                return null;
            }
            final File target = new File(downloadPath, candidate);
            final boolean free = session != null ? isAutoRenameTargetFree(session, controller, target) : !target.exists();
            if (free) {
                return candidate;
            }
        }
        return null;
    }

    /**
     * Returns true when the given target file neither exists on disk nor is currently locked by another download controller of the given
     * session (i.e. it is safe to use as an auto-rename target).
     */
    static boolean isAutoRenameTargetFree(final DownloadSession session, final SingleDownloadController controller, final File target) {
        /* Check the locks first, they only need memory. The filesystem is only accessed if no other download holds the file. */
        for (final SingleDownloadController downloadController : session.getControllers()) {
            if (downloadController == controller) {
                continue;
            }
            if (session.getFileAccessManager().isLockedBy(target, downloadController)) {
                return false;
            }
        }
        return !target.exists();
    }

    /**
     * @param maxFilenameLength
     *            When > 0, any filename generated by this method (currently the auto-rename "_N" variant) must not exceed this number of
     *            characters. This is used when we already had to shorten a too-long filename and the shortened name turned out to already
     *            exist: the auto-rename variant would otherwise grow the name beyond the filesystem limit again. A value <= 0 means "no
     *            restriction".
     * @param alreadyExistingFile
     *            The already existing file to handle instead of the controller's current output file; null to use
     *            {@code controller.getFileOutput(...)}. Used by the too-long handling, where the collision is on the shortened name while
     *            the download link still carries the (un-shortened) original name: passing the shortened file here lets this method act on
     *            the correct name without prematurely forcing that name onto the link. For the "continue with this name" outcome
     *            (overwrite) the name is then applied to the link here; auto-rename sets the (generated) name itself; skip throws.
     */
    public void fileAlreadyExistsHandling(final DownloadSession session, final SingleDownloadController controller, final DownloadLink fileInProgress, final int maxFilenameLength, final File alreadyExistingFile) throws SkipReasonException, DeferredRunnableException, PluginException, InterruptedException {
        /* File already exists or file is currently being downloaded. */
        final DownloadLink downloadLink = controller.getDownloadLink();
        final File fileOutput = alreadyExistingFile != null ? alreadyExistingFile : controller.getFileOutput(false, true);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && !fileOutput.exists()) {
            throw new IllegalArgumentException("Provided file does not exist");
        }
        /* A specific new filename the user entered/confirmed in the dialog (rename case); null when not applicable. */
        String userDefinedFilename = null;
        /* The auto-rename suggestion shown in the dialog. Reused for AUTO_RENAME so the search for a free name does not run twice. */
        String dialogAutoRenameSuggestion = null;
        /* The "on skip due to already exists" action chosen in the dialog (skip option); null -> fall back to the global config value. */
        OnSkipDueToAlreadyExistsAction dialogOnSkipAction = null;
        final IfFileExistsAction doAction;
        decideAction: {
            /*
             * Per-package remembered action first, then the session-wide one: the user may pick a per-package action for one or more
             * packages before later deciding "don't ask again this session", so the more specific per-package choice must take precedence.
             * getOnFileExistsAction resolves the per-package action and falls back to the global default (then ASK).
             */
            final IfFileExistsAction packageAction = session.getOnFileExistsAction(downloadLink.getFilePackage());
            if (packageAction != null && packageAction != IfFileExistsAction.ASK_FOR_EACH_FILE) {
                /* Pre-selected per-package action (or global default) -> do not ask. */
                doAction = packageAction;
                break decideAction;
            }
            /* Then the session-wide "don't ask again during this session" decision. */
            final IfFileExistsAction sessionWide = session.getSessionOnFileExistsAction();
            if (sessionWide != null && sessionWide != IfFileExistsAction.ASK_FOR_EACH_FILE) {
                doAction = sessionWide;
                break decideAction;
            }
            /* Get action from dialog */
            /* The dialog is going to be shown -> we always need an auto-rename suggestion to display and pre-fill. */
            final String autoRenameSuggestion = generateAutoRenameSuggestion(fileOutput, maxFilenameLength);
            dialogAutoRenameSuggestion = autoRenameSuggestion;
            /* Pass the actual existing file (fileOutput) so the dialog shows the correct name/size, also in the too-long case. */
            final IfFileExistsDialogInterface io = new IfFileExistsDialog(downloadLink, fileInProgress, autoRenameSuggestion, maxFilenameLength, fileOutput).show();
            switch (io.getCloseReason()) {
            case TIMEOUT:
                /* User did not react -> All we can do is display an error. */
                throw new SkipReasonException(SkipReason.FILE_EXISTS);
            case INTERRUPT:
                throw new InterruptedException("IFFileExistsDialog Interrupted");
            case OK:
                break;
            default:
                /* Dialog closed/cancelled -> skip. */
                doAction = IfFileExistsAction.SKIP_FILE;
                break decideAction;
            }
            IfFileExistsAction dialogAction = io.getAction();
            /* Only relevant for the rename case; null otherwise. */
            userDefinedFilename = io.getNewFilename();
            /* Only relevant for the skip case; null otherwise -> caller then uses the global config value. */
            dialogOnSkipAction = io.getOnSkipDueToAlreadyExistsAction();
            if (dialogAction == null) {
                /* Fallback */
                dialogAction = IfFileExistsAction.SKIP_FILE;
            }
            /* Windows file names are case insensitive, elsewhere a pure letter-case change results in a different file. */
            if (userDefinedFilename != null && (CrossSystem.isWindows() ? userDefinedFilename.equalsIgnoreCase(fileOutput.getName()) : userDefinedFilename.equals(fileOutput.getName()))) {
                logger.info("User chosen filename is the same as before");
                throw new PluginException(LinkStatus.ERROR_ALREADYEXISTS);
            }
            /* Per-package "remember" checkbox -> apply the chosen action to the remaining items of this package. */
            if (io.isRememberForPackageSelected()) {
                session.setOnFileExistsAction(downloadLink.getFilePackage(), dialogAction);
            } else {
                session.setOnFileExistsAction(downloadLink.getFilePackage(), null);
            }
            /*
             * "Don't ask again during this session" checkbox -> store session-wide (in the DownloadSession). A custom filename or the
             * global "do not show again" option disables the checkbox, so it only reports true for a plain overwrite/skip/auto-rename.
             */
            if (io.isDontAskAgainThisSessionSelected() && dialogAction != IfFileExistsAction.ASK_FOR_EACH_FILE) {
                session.setSessionOnFileExistsAction(dialogAction);
            }
            /*
             * Framework "Do not ask again" checkbox -> store as the global default. Only overwrite/skip/auto-rename may be stored; a custom
             * filename is a per-file decision (the dialog disables the checkbox in that case) and ASK must never be stored.
             */
            if (io.isDontShowAgainSelected()) {
                switch (dialogAction) {
                case OVERWRITE_FILE:
                case SKIP_FILE:
                case AUTO_RENAME:
                    config.setIfFileExistsAction(dialogAction);
                    break;
                default:
                    break;
                }
            }
            doAction = dialogAction;
        }
        switch (doAction) {
        case SKIP_FILE:
            /* Use the action chosen in the dialog's skip dropdown if present, otherwise the global config value. */
            switch (dialogOnSkipAction != null ? dialogOnSkipAction : CFG_GENERAL.CFG.getOnSkipDueToAlreadyExistsAction()) {
            case SET_FILE_TO_SUCCESSFUL_MIRROR:
                /*
                 * TODO: Maybe check this in beforehand so that in the dialog, we can already hide the
                 * "Skip & mark as finished if file is a mirror" option?
                 */
                if (fileInProgress != null) {
                    throw new PluginException(LinkStatus.ERROR_ALREADYEXISTS);
                }
                throw new DeferredRunnableException(new ExceptionRunnable() {
                    @Override
                    public void run() throws Exception {
                        final PluginForHost plugin = controller.getProcessingPlugin();
                        if (plugin == null) {
                            throw new PluginException(LinkStatus.ERROR_ALREADYEXISTS);
                        }
                        final Downloadable downloadable = plugin.newDownloadable(downloadLink, null);
                        if (downloadable == null) {
                            throw new PluginException(LinkStatus.ERROR_ALREADYEXISTS);
                        }
                        final MirrorDetectionDecision mirrorDetectionDecision = config.getMirrorDetectionDecision();
                        final long verifiedFileSize = downloadable.getVerifiedFileSize();
                        /* Size of the existing file on disk, read only once and only if needed (-1 = not read yet). */
                        long existingFileLength = -1;
                        switch (mirrorDetectionDecision) {
                        case AUTO:
                            existingFileLength = fileOutput.length();
                            if (verifiedFileSize >= 0 && verifiedFileSize != existingFileLength) {
                                /* The size differs, so it cannot be the same file -> do not read the whole file just to hash it. */
                                break;
                            }
                            final HashInfo hashInfo = downloadable.getHashInfo();
                            if (hashInfo != null) {
                                final HashResult hashResult = downloadable.getHashResult(hashInfo, fileOutput);
                                if (hashResult != null) {
                                    if (!hashResult.match()) {
                                        /*
                                         * Hash was calculated and differs -> definitely a different file, a matching file size must not
                                         * override this.
                                         */
                                        throw new PluginException(LinkStatus.ERROR_ALREADYEXISTS);
                                    }
                                    downloadable.setHashResult(hashResult);
                                    downloadLink.setDownloadCurrent(existingFileLength);
                                    throw new PluginException(LinkStatus.FINISHED);
                                }
                            }
                            /* No hash available or it could not be calculated -> fall through and compare the file size. */
                        case FILENAME_FILESIZE:
                            if (existingFileLength < 0) {
                                existingFileLength = fileOutput.length();
                            }
                            if (verifiedFileSize >= 0 && verifiedFileSize == existingFileLength) {
                                downloadLink.setDownloadCurrent(existingFileLength);
                                throw new PluginException(LinkStatus.FINISHED);
                            }
                            break;
                        case FILENAME:
                        case DISABLED:
                        default:
                            // nothing
                        }
                        throw new PluginException(LinkStatus.ERROR_ALREADYEXISTS);
                    }
                });
            case SET_FILE_TO_SUCCESSFUL:
                throw new PluginException(LinkStatus.FINISHED);
            default:
                throw new PluginException(LinkStatus.ERROR_ALREADYEXISTS);
            }
        case OVERWRITE_FILE:
            if (fileInProgress != null) {
                /* We cannot overwrite a file that is currently in progress */
                controller.getLogger().severe("Cannot overwrite file:" + fileOutput + " | Blocked by:" + fileInProgress);
                throw new PluginException(LinkStatus.ERROR_ALREADYEXISTS);
            }
            if (!fileOutput.delete()) {
                controller.getLogger().severe("Cannot overwrite file:" + fileOutput + " | Can't delete initial file");
                throw new PluginException(LinkStatus.ERROR_ALREADYEXISTS);
            }
            if (alreadyExistingFile != null) {
                /*
                 * We were given an explicit target (e.g. the shortened too-long name) that the link does not carry yet -> now that we
                 * overwrite it, continue the download with exactly that name.
                 */
                controller.setSessionDownloadFilename(fileOutput.getName());
                downloadLink.setForcedFileName(fileOutput.getName());
                downloadLink.setChunksProgress(null);
            }
            break;
        case AUTO_RENAME: {
            final String downloadPath = fileOutput.getParent();
            String newName = null;
            if (userDefinedFilename != null) {
                /*
                 * The user entered/confirmed a specific new filename in the dialog. Use it as-is as long as it is still free (not existing,
                 * not locked by another download) and within the allowed length; otherwise fall back to auto-generation.
                 */
                final boolean fitsLength = maxFilenameLength <= 0 || userDefinedFilename.length() <= maxFilenameLength;
                if (fitsLength && isAutoRenameTargetFree(session, controller, new File(downloadPath, userDefinedFilename))) {
                    newName = userDefinedFilename;
                } else {
                    controller.getLogger().info("User defined rename target not usable, falling back to auto-rename: " + userDefinedFilename);
                }
            }
            if (newName == null && dialogAutoRenameSuggestion != null && !dialogAutoRenameSuggestion.equals(userDefinedFilename) && isAutoRenameTargetFree(session, controller, new File(downloadPath, dialogAutoRenameSuggestion))) {
                /* The suggestion that was already searched for the dialog is still free (also not locked) -> no need to search again. */
                newName = dialogAutoRenameSuggestion;
            }
            if (newName == null) {
                /* Auto-generate "<name>_<n>" until we find a free (non-existing, non-locked) name. */
                newName = findFreeAutoRenameName(fileOutput, maxFilenameLength, session, controller);
                if (newName == null) {
                    /* Nothing was changed on the link so far -> a possibly existing forced filename stays untouched. */
                    controller.getLogger().severe("Auto-rename failed (no free name found or none fits the length limit) for:" + fileOutput.getName());
                    throw new PluginException(LinkStatus.ERROR_ALREADYEXISTS);
                }
            }
            // we can do this, because the localFilecheck always runs BEFORE the download
            // except for org.jdownloader.controlling.linkcrawler.GenericVariants.DEMUX_GENERIC_AUDIO
            controller.setSessionDownloadFilename(newName);
            downloadLink.setForcedFileName(newName);
            downloadLink.setChunksProgress(null);
            break;
        }
        default:
            throw new PluginException(LinkStatus.ERROR_ALREADYEXISTS);
        }
    }

    @Override
    public void onShutdownVetoRequest(ShutdownRequest request) throws ShutdownVetoException {
        if (request.getVetos().size() > 0) {
            /* we already abort shutdown, no need to ask again */
            /*
             */
            return;
        }
        String dialogTitle = null;
        synchronized (this.shutdownLock) {
            /*
             * we sync to make sure that no new downloads get started meanwhile
             */
            if (this.stateMachine.isState(RUNNING_STATE, PAUSE_STATE)) {
                for (final SingleDownloadController con : getSession().getControllers()) {
                    if (con.isAlive() == false) {
                        continue;
                    }
                    dialogTitle = _JDT.T.DownloadWatchDog_onShutdownRequest_();
                    DownloadInterface dl = con.getDownloadInstance();
                    if (dl != null && !con.getDownloadLink().isResumeable()) {
                        dialogTitle = _JDT.T.DownloadWatchDog_onShutdownRequest_nonresumable();
                        break;
                    }
                }
            }
            if (dialogTitle == null) {
                /* no running downloads that would be interrupted */
                return;
            }
            if (request.isSilent()) {
                throw new ShutdownVetoException(dialogTitle, this);
            }
            if (!JDGui.bugme(WarnLevel.NORMAL)) {
                return;
            }
            if (UIOManager.I().showConfirmDialog(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN | UIOManager.LOGIC_DONT_SHOW_AGAIN_IGNORES_CANCEL, dialogTitle, _JDT.T.DownloadWatchDog_onShutdownRequest_msg(), new AbstractIcon(IconKey.ICON_DOWNLOAD, 32), _JDT.T.literally_yes(), null)) {
                return;
            }
            throw new ShutdownVetoException(dialogTitle, this);
        }
    }

    @Override
    public long getShutdownVetoPriority() {
        return 0;
    }

    public boolean isLinkForced(DownloadLink dlLink) {
        List<DownloadLink> links = getSession().getForcedLinks();
        return links.size() > 0 && links.contains(dlLink);
    }

    public boolean isRunning() {
        return stateMachine.isState(RUNNING_STATE, PAUSE_STATE);
    }

    public boolean isIdle() {
        return stateMachine.isState(IDLE_STATE, STOPPED_STATE);
    }

    public boolean isStopping() {
        return stateMachine.isState(STOPPING_STATE);
    }

    @Override
    public void onShutdown(ShutdownRequest request) {
        config.setClosedWithRunningDownloads(isRunning());
    }

    @Override
    public void onShutdownVeto(ShutdownRequest request) {
    }

    @Override
    public void onDownloadControllerAddedPackage(FilePackage pkg) {
    }

    @Override
    public void onDownloadControllerStructureRefresh(FilePackage pkg) {
    }

    @Override
    public void onDownloadControllerStructureRefresh() {
    }

    @Override
    public void onDownloadControllerStructureRefresh(AbstractNode node, Object param) {
    }

    @Override
    public void onDownloadControllerRemovedPackage(final FilePackage pkg) {
        final DownloadSession session = getSession();
        if (session.isStopMarkSet() == false || session.getStopMark() == STOPMARK.HIDDEN) {
            return;
        }
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                final Object stopMark = currentSession.getStopMark();
                if (currentSession.isStopMarkSet() == false || stopMark == STOPMARK.HIDDEN) {
                    return;
                }
                if (stopMark == pkg) {
                    /* now the stopmark is hidden */
                    currentSession.setStopMark(STOPMARK.HIDDEN);
                }
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return false;
            }
        });
    }

    @Override
    public void onDownloadControllerRemovedLinklist(final List<DownloadLink> list) {
        final DownloadSession session = getSession();
        if (session.isStopMarkSet() == false || session.getStopMark() == STOPMARK.HIDDEN) {
            return;
        }
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                final Object stopMark = currentSession.getStopMark();
                if (currentSession.isStopMarkSet() == false || stopMark == STOPMARK.HIDDEN) {
                    return;
                }
                for (final DownloadLink l : list) {
                    if (stopMark == l) {
                        currentSession.setStopMark(STOPMARK.HIDDEN);
                        return;
                    }
                }
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return false;
            }
        });
    }

    @Override
    public void onDownloadControllerUpdatedData(DownloadLink downloadlink, DownloadLinkProperty property) {
    }

    @Override
    public void onDownloadControllerUpdatedData(FilePackage pkg, FilePackageProperty property) {
    }

    @Override
    public void onDownloadControllerUpdatedData(DownloadLink downloadlink) {
    }

    @Override
    public void onDownloadControllerUpdatedData(FilePackage pkg) {
    }

    public void renameLink(final DownloadLink downloadLink, final String value) {
        if (StringUtils.equals(downloadLink.getForcedFileName(), value)) {
            return;
        }
        // logger.log(new Exception("Rename"));
        logger.info("Requested Rename of " + downloadLink + " to " + value);
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                if (StringUtils.equals(downloadLink.getForcedFileName(), value)) {
                    return;
                }
                if (downloadLink.getDownloadLinkController() != null) {
                    logger.info("Requested Rename of " + downloadLink + " to " + value + " DELAYED");
                    downloadLink.setForcedFileName(value);
                    return;
                }
                if (downloadLink.getDefaultPlugin() == null) {
                    return;
                }
                logger.info("Requested Rename of " + downloadLink + " to " + value + " NOW");
                final String downloadDirectory = downloadLink.getParentNode().getDownloadDirectory();
                if (!move(downloadLink, downloadDirectory, downloadLink.getName(), downloadDirectory, value)) {
                    /* Renaming the file failed -> keep the old name so link and file on disk stay in sync. */
                    return;
                }
                downloadLink.setForcedFileName(value);
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return false;
            }
        });
    }

    public void setStopMark(final Object stopEntry) {
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void interrupt() {
            }

            @Override
            public void execute(DownloadSession currentSession) {
                currentSession.setStopMark(stopEntry);
            }

            @Override
            public boolean isHighPriority() {
                return true;
            }
        });
    }

    public void handleMovedDownloadLinks(final FilePackage dest, final FilePackage source, final List<DownloadLink> links) {
        if (source == dest) {
            /* Same package -> nothing changes. */
            return;
        }
        if (links == null || links.size() == 0) {
            /* No links to handle. */
            return;
        }
        if (!CFG_GENERAL.CFG.isMoveFilesIfDownloadDestinationChangesEnabled()) {
            /* Moving files is disabled -> do not even queue a job that would only skip every single link. */
            return;
        }
        if (new File(source.getDownloadDirectory()).equals(new File(dest.getDownloadDirectory()))) {
            /*
             * Both packages resolve to the same download directory -> no file needs to be moved. Skip the whole per-link iteration (and its
             * disk IO) instead of testing every single link.
             */
            return;
        }
        enqueueJob(new DownloadWatchDogJob() {
            //
            @Override
            public void execute(DownloadSession currentSession) {
                /* The directories are the same for every link -> resolve them once instead of once per link. */
                final String sourceDirectory = source.getDownloadDirectory();
                final String destinationDirectory = dest.getDownloadDirectory();
                for (DownloadLink downloadLink : links) {
                    if (downloadLink.getDownloadLinkController() != null) {
                        // running
                        // TODO
                        // if (DISKSPACERESERVATIONRESULT.FAILED.equals(validateDiskFree(nextSelectedCandidates))) {
                        // for (final DownloadLinkCandidate candidate : nextSelectedCandidates) {
                        // selector.addExcluded(candidate, new DownloadLinkCandidateResult(SkipReason.DISK_FULL, null, null));
                        // }
                        // }
                        continue;
                    }
                    if (downloadLink.getDefaultPlugin() == null) {
                        /* no plugin, so there are no files we could move */
                        continue;
                    }
                    move(downloadLink, sourceDirectory, downloadLink.getName(), destinationDirectory, null);
                }
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return false;
            }
        });
    }

    public void setDownloadDirectory(final FilePackage pkg, final String path) {
        if (new File(pkg.getDownloadDirectory()).equals(new File(path))) {
            /* Path hasn't changed -> Do nothing */
            return;
        }
        enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                final String old = pkg.getDownloadDirectory();
                if (new File(old).equals(new File(path))) {
                    /* Path hasn't changed -> Do nothing */
                    return;
                }
                pkg.setDownloadDirectory(path);
                boolean readL = pkg.getModifyLock().readLock();
                try {
                    for (DownloadLink downloadLink : pkg.getChildren()) {
                        if (downloadLink.getDownloadLinkController() != null) {
                            // running
                            continue;
                        }
                        if (downloadLink.getDefaultPlugin() == null) {
                            /* no plugin, so there are no files we could move */
                            continue;
                        }
                        move(downloadLink, old, downloadLink.getName(), path, null);
                    }
                } finally {
                    pkg.getModifyLock().readUnlock(readL);
                }
            }

            @Override
            public void interrupt() {
            }

            @Override
            public boolean isHighPriority() {
                return false;
            }
        });
    }

    /** True while the error dialog of a failed move is shown, so a failing bulk move does not open one dialog per link. */
    private final AtomicBoolean moveErrorDialogOpen = new AtomicBoolean(false);

    /**
     * Shows the error dialog of a failed move in its own thread. The dialog is modal and must neither block the DownloadWatchDog thread nor
     * keep a package lock while it is open. Only one dialog is shown at a time; further errors are only logged.
     */
    private void showMoveError(final Throwable e) {
        if (!moveErrorDialogOpen.compareAndSet(false, true)) {
            /* A dialog is already open (e.g. bulk move with many failing links) -> the error is already logged. */
            return;
        }
        final Thread thread = new Thread("DownloadWatchDog:MoveErrorDialog") {
            @Override
            public void run() {
                try {
                    UIOManager.I().show(ExceptionDialogInterface.class, new ExceptionDialog(UIOManager.BUTTONS_HIDE_CANCEL, _GUI.T.lit_error_occured(), e.getMessage(), e, _GUI.T.lit_close(), null));
                } finally {
                    moveErrorDialogOpen.set(false);
                }
            }
        };
        thread.setDaemon(true);
        thread.start();
    }

    /**
     * Moves and/or renames the files of the given link if enabled in the settings.
     *
     * @return false if moving/renaming failed (the user is informed), true otherwise - this includes the cases in which nothing had to be
     *         done or the feature is disabled
     */
    protected boolean move(DownloadLink downloadLink, String oldDir, String oldName, String newDir, String newName) {
        /*
         * Decide whether anything actually needs to happen BEFORE touching the filesystem (disk space check). Moving many links between
         * packages must not trigger per-link IO when no move/rename is going to happen.
         */
        final boolean moveEnabled = CFG_GENERAL.CFG.isMoveFilesIfDownloadDestinationChangesEnabled();
        final boolean renameEnabled = CFG_GENERAL.CFG.isRenameFilesIfDownloadLinkNameChangesEnabled();
        if (!moveEnabled && !renameEnabled) {
            /* Both features are disabled -> nothing can happen, no need to compare the paths. */
            return true;
        }
        if (!PluginForHost.hasOwnFileOnDisk(downloadLink)) {
            /* No file was downloaded for this link, so there is nothing to move or rename (PluginForHost#move would skip it as well). */
            return true;
        }
        final boolean sameDir = new File(oldDir).equals(new File(newDir));
        final boolean sameName = StringUtils.equals(oldName, newName);
        if (sameDir && (newName == null || sameName)) {
            /* Neither directory nor name changed -> nothing to move or rename. */
            return true;
        }
        if (!sameDir && !moveEnabled) {
            logger.info("Cancel isMoveFilesIfDownloadDestinationChanges is false");
            return true;
        }
        if (sameDir && !renameEnabled) {
            /* Same directory, name changed -> rename only */
            logger.info("Cancel isRenameFilesIfDownloadLinkNameChangesEnabled is false");
            return true;
        }
        try {
            if (!sameDir) {
                /*
                 * A pure rename inside the same directory needs no additional disk space, so the check is only done if the files change
                 * their directory. Moving to another volume copies the files, so the destination needs as much free space as the already
                 * loaded data. On the same volume the move only renames the file system entries and nothing is required.
                 */
                if (DISKSPACERESERVATIONRESULT.FAILED.equals(getSession().getDiskSpaceManager().checkMove(new File(oldDir), new File(newDir), downloadLink.getView().getBytesLoaded(), logger))) {
                    throw new IOException(_GUI.T.DownloadWatchDog_move_exception_disk_full(downloadLink.getFileOutput()));
                }
            }
            logger.info("Move " + downloadLink + " from " + oldDir + "/" + oldName + " to " + newDir + "/" + newName);
            downloadLink.getDefaultPlugin().move(downloadLink, oldDir, oldName, newDir, newName);
            return true;
        } catch (Throwable e) {
            logger.log(e);
            showMoveError(e);
            return false;
        }
    }

    @Override
    public void onNewFolder(Object caller, File folder) {
    }
}