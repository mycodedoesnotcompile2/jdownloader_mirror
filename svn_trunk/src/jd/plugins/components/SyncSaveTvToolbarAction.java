package jd.plugins.components;

import java.awt.event.ActionEvent;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.AbstractButton;

import org.appwork.swing.components.ExtButton;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.DomainInfo;
import org.jdownloader.controlling.contextmenu.ActionData;
import org.jdownloader.controlling.contextmenu.ContextMenuManager;
import org.jdownloader.controlling.contextmenu.MenuContainerRoot;
import org.jdownloader.controlling.contextmenu.MenuExtenderHandler;
import org.jdownloader.controlling.contextmenu.MenuItemData;
import org.jdownloader.gui.mainmenu.MenuManagerMainmenu;
import org.jdownloader.gui.toolbar.MenuManagerMainToolbar;
import org.jdownloader.gui.toolbar.action.AbstractToolBarAction;

import jd.config.SubConfiguration;
import jd.controlling.AccountController;
import jd.controlling.AccountControllerEvent;
import jd.controlling.AccountControllerListener;
import jd.controlling.AccountFilter;
import jd.controlling.TaskQueue;
import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcollector.LinkOrigin;
import jd.plugins.Account.AccountType;
import jd.plugins.hoster.SaveTv;

public class SyncSaveTvToolbarAction extends AbstractToolBarAction {
    private static final String TOOLTIP = "save.tv Videoarchiv crawlen";

    public SyncSaveTvToolbarAction() {
        setTooltipText(TOOLTIP);
        setIconKey("fav/save.tv");
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                ACTION_REFERENCES.put(SyncSaveTvToolbarAction.this, new Object());
                TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
                    @Override
                    protected Void run() throws RuntimeException {
                        updateEnabled();
                        return null;
                    }
                });
            }
        };
    }

    @Override
    public String getName() {
        return super.getName();
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (!isEnabled()) {
            return;
        }
        /*
         * This action does nothing else than SaveTvDecrypter with source=jdtoolbar --> It crawls all recordings from the users' save.tv
         * video archive. The dialog below is purely informational (auto-closes/continues on its own) and can be disabled via
         * SaveTv.SYNC_TOOLBAR_ENABLE_DIALOG.
         */
        final SubConfiguration cfg = SubConfiguration.getConfig(SaveTv.HOST_STATIC);
        if (cfg.getBooleanProperty(SaveTv.SYNC_TOOLBAR_ENABLE_DIALOG, SaveTv.defaultSYNC_TOOLBAR_ENABLE_DIALOG)) {
            final String message = "JDownloader durchsucht jetzt dein komplettes save.tv Videoarchiv nach Aufnahmen und fügt sie dem Linkgrabber hinzu.\r\n" + "Das kann etwas dauern - bitte habe etwas Geduld.\r\n" + "Die zugehörigen Crawler-Einstellungen findest du unter: Einstellungen -> Plugin Einstellungen -> save.tv\r\n" + "Dort kannst du diesen Dialog auch permanent abschalten.";
            SaveTv.showAutoCloseDialog(TOOLTIP, message, 60);
        }
        LinkCollector.getInstance().addCrawlerJob(new LinkCollectingJob(LinkOrigin.TOOLBAR.getLinkOriginDetails(), "https://www.save.tv/STV/M/obj/archive/VideoArchive.cfm?source=jdtoolbar"));
    }

    @Override
    public AbstractButton createButton() {
        final ExtButton ret = new ExtButton(this);
        ret.setHideActionText(true);
        ret.setIcon(DomainInfo.getInstance("save.tv").getFavIcon(true));
        return ret;
    }

    @Override
    protected String createTooltip() {
        return TOOLTIP;
    }

    @Override
    public boolean isEnabled() {
        return ENABLED.get();
    }

    private static final AtomicBoolean                                EXTENDER_REGISTERED = new AtomicBoolean(false);
    private static final AtomicBoolean                                ENABLED             = new AtomicBoolean(false);
    private static final WeakHashMap<SyncSaveTvToolbarAction, Object> ACTION_REFERENCES   = new WeakHashMap<SyncSaveTvToolbarAction, Object>();

    private static void updateEnabled() {
        final boolean enabled = AccountController.getInstance().listAccounts(new AccountFilter("save.tv").setEnabled(true).setValid(true).setAccountTypes(AccountType.PREMIUM, AccountType.LIFETIME).setMaxResultsNum(1)).size() > 0;
        ENABLED.set(enabled);
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                for (SyncSaveTvToolbarAction action : ACTION_REFERENCES.keySet()) {
                    action.setEnabled(enabled);
                }
            }
        };
    }

    public static void registerExtender() {
        if (!org.appwork.utils.Application.isHeadless() && EXTENDER_REGISTERED.compareAndSet(false, true)) {
            // must run in own thread to avoid deadlock during HostPluginController initialization
            new Thread("SyncSaveTvToolbarAction.registerExtender") {
                @Override
                public void run() {
                    AccountController.getInstance().getEventSender().addListener(new AccountControllerListener() {
                        @Override
                        public void onAccountControllerEvent(AccountControllerEvent event) {
                            updateEnabled();
                        }
                    });
                    final MenuExtenderHandler extenderHandler = new MenuExtenderHandler() {
                        @Override
                        public MenuItemData updateMenuModel(ContextMenuManager manager, MenuContainerRoot mr) {
                            if (manager instanceof MenuManagerMainmenu) {
                                return null;
                            } else if (manager instanceof MenuManagerMainToolbar) {
                                for (MenuItemData m : mr.getItems()) {
                                    final ActionData ad = m.getActionData();
                                    if (ad != null) {
                                        final String cl = ad.getClazzName();
                                        if (StringUtils.equals(cl, SyncSaveTvToolbarAction.class.getName())) {
                                            return null;
                                        }
                                    }
                                }
                                mr.add(SyncSaveTvToolbarAction.class);
                            }
                            return null;
                        }
                    };
                    MenuManagerMainmenu.getInstance().registerExtender(extenderHandler);
                    MenuManagerMainToolbar.getInstance().registerExtender(extenderHandler);
                }
            }.start();
        }
    }
}
