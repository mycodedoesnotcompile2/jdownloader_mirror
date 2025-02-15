package jd.plugins.components.gopro;

import java.awt.event.ActionEvent;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

import org.appwork.uio.UIOManager;
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

import jd.controlling.AccountController;
import jd.controlling.AccountControllerEvent;
import jd.controlling.AccountControllerListener;
import jd.controlling.TaskQueue;
import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcollector.LinkOrigin;

public class SyncGoProLibraryToolbarAction extends AbstractToolBarAction {
    public SyncGoProLibraryToolbarAction() {
        setName("Sync GoPro Plus Library");
        setSmallIcon(DomainInfo.getInstance("gopro.com").getFavIcon(true));
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                ACTION_REFERENCES.put(SyncGoProLibraryToolbarAction.this, new Object());
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
    public void actionPerformed(ActionEvent e) {
        if (isEnabled() && UIOManager.I().showConfirmDialog(UIOManager.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, "GoPro Plus Media Library Sync", "JDownloader will now search all available media files in your GoPro media library and add them to the Linkgrabber tab.\r\nPlease be patient - this will take a while. If you keep all links in the linkgrabber or downlist, the next sync process will be much faster.")) {
            LinkCollector.getInstance().addCrawlerJob(new LinkCollectingJob(LinkOrigin.TOOLBAR.getLinkOriginDetails(), "https://plus.gopro.com/media-library/"));
        }
    }

    @Override
    protected String createTooltip() {
        return "Sync GoPro Plus Library";
    }

    @Override
    public boolean isEnabled() {
        return ENABLED.get();
    }

    private static final AtomicBoolean                                      EXTENDER_REGISTERED = new AtomicBoolean(false);
    private static final AtomicBoolean                                      ENABLED             = new AtomicBoolean(false);
    private static final WeakHashMap<SyncGoProLibraryToolbarAction, Object> ACTION_REFERENCES   = new WeakHashMap<SyncGoProLibraryToolbarAction, Object>();

    private static void updateEnabled() {
        final boolean enabled = AccountController.getInstance().hasAccount("gopro.com", Boolean.TRUE, Boolean.TRUE, Boolean.TRUE, null);
        ENABLED.set(enabled);
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                for (SyncGoProLibraryToolbarAction action : ACTION_REFERENCES.keySet()) {
                    action.setEnabled(enabled);
                }
            }
        };
    }

    public static void registerExtender() {
        if (!org.appwork.utils.Application.isHeadless() && EXTENDER_REGISTERED.compareAndSet(false, true)) {
            // must run in own thread to avoid deadlock during HostPluginController initialization
            new Thread("SyncGoProLibraryToolbarAction.registerExtender") {
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
                                        if (StringUtils.equals(cl, SyncGoProLibraryToolbarAction.class.getName())) {
                                            return null;
                                        }
                                    }
                                }
                                mr.add(SyncGoProLibraryToolbarAction.class);
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
