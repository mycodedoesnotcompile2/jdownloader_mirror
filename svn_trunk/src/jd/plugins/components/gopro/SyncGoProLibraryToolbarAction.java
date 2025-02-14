package jd.plugins.components.gopro;

import java.awt.event.ActionEvent;
import java.util.concurrent.atomic.AtomicBoolean;

import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcollector.LinkOrigin;
import jd.controlling.linkcrawler.LinkCrawler;

import org.appwork.uio.UIOManager;
import org.appwork.utils.StringUtils;
import org.jdownloader.controlling.contextmenu.ActionData;
import org.jdownloader.controlling.contextmenu.ContextMenuManager;
import org.jdownloader.controlling.contextmenu.MenuContainerRoot;
import org.jdownloader.controlling.contextmenu.MenuExtenderHandler;
import org.jdownloader.controlling.contextmenu.MenuItemData;
import org.jdownloader.gui.mainmenu.MenuManagerMainmenu;
import org.jdownloader.gui.toolbar.MenuManagerMainToolbar;
import org.jdownloader.gui.toolbar.action.AbstractToolBarAction;

public class SyncGoProLibraryToolbarAction extends AbstractToolBarAction {
    public SyncGoProLibraryToolbarAction() {
        setName("Sync GoPro Plus Library");
        setIconKey("fav/gopro.com");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        UIOManager.I().showConfirmDialog(UIOManager.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, "GoPro Plus Media Library Sync", "JDownloader will now search all available media files in your GoPro media library and add them to the Linkgrabber tab.\r\nPlease be patient - this will take a while. If you keep all links in the linkgrabber or downlist, the next sync process will be much faster.");
        LinkCrawler job = LinkCollector.getInstance().addCrawlerJob(new LinkCollectingJob(LinkOrigin.TOOLBAR.getLinkOriginDetails(), "https://plus.gopro.com/media-library/"));
    }

    @Override
    protected String createTooltip() {
        return "Sync GoPro Plus Library";
    }

    private static final AtomicBoolean EXTENDER_REGISTERED = new AtomicBoolean(false);

    public static void registerExtender() {
        if (!org.appwork.utils.Application.isHeadless() && EXTENDER_REGISTERED.compareAndSet(false, true)) {
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

    }
}
