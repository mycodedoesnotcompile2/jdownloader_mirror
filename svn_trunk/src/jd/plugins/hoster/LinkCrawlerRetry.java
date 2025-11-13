package jd.plugins.hoster;

import java.awt.event.ActionEvent;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.JComponent;
import javax.swing.JMenuItem;

import jd.PluginWrapper;
import jd.config.ConfigContainer;
import jd.config.ConfigEntry;
import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcollector.LinkCollector.JobLinkCrawler;
import jd.controlling.linkcollector.LinkOrigin;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.LinkCrawler;
import jd.controlling.packagecontroller.AbstractNodeNotifier;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.swing.action.BasicAction;
import org.appwork.utils.event.queue.QueueAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo.PluginView;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.host.PluginFinder;

@HostPlugin(revision = "$Revision: 51816 $", interfaceVersion = 3, names = { "LinkCrawlerRetry" }, urls = { "" })
public class LinkCrawlerRetry extends PluginForHost {
    private final static String ON_LINKCHECK = "onlinkcheck";

    public LinkCrawlerRetry(PluginWrapper wrapper) {
        super(wrapper);
        setConfigElements();
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.INTERNAL, LazyPlugin.FEATURE.GENERIC, LazyPlugin.FEATURE.FAVICON };
    }

    @Override
    public String getAGBLink() {
        return null;
    }

    @Override
    public Object getFavIcon(final String host) throws IOException {
        return new AbstractIcon(IconKey.ICON_REFRESH, 16);
    }

    @Override
    public String getHost(DownloadLink link, Account account, boolean includeSubdomain) {
        if (link != null) {
            String url = LinkCrawler.cleanURL(link.getPluginPatternMatcher());
            if (url == null) {
                url = link.getPluginPatternMatcher();
            }
            return Browser.getHost(url, includeSubdomain);
        } else {
            return getHost();
        }
    }

    @Override
    public PluginForHost assignPlugin(PluginFinder pluginFinder, final DownloadLink link) {
        final PluginForHost ret = super.assignPlugin(pluginFinder, link);
        if (ret != null) {
            link.setAvailableStatus(AvailableStatus.UNCHECKED);
            return ret;
        } else {
            return null;
        }
    }

    @Override
    public AvailableStatus requestFileInformation(DownloadLink parameter) {
        final String reason = parameter.getStringProperty("reason", null);
        if ("FILE_NOT_FOUND".equals(reason)) {
            return AvailableStatus.FALSE;
        } else {
            return AvailableStatus.UNCHECKED;
        }
    }

    private PluginView<AbstractPackageChildrenNode> toPluginView(final DownloadLink... downloadLinks) {
        final PluginView<AbstractPackageChildrenNode> pv = new PluginView<AbstractPackageChildrenNode>(this);
        for (DownloadLink downloadLink : downloadLinks) {
            if (downloadLink.getTempProperties().removeProperty("checked")) {
                final AbstractNodeNotifier nodeChangeListener = downloadLink.getNodeChangeListener();
                if (nodeChangeListener instanceof CrawledLink) {
                    pv.add((CrawledLink) nodeChangeListener);
                } else {
                    pv.add(downloadLink);
                }
            } else {
                downloadLink.getTempProperties().setProperty("checked", Boolean.TRUE);
            }
        }
        return pv;
    }

    @Override
    public boolean checkLinks(DownloadLink[] urls) {
        if (urls == null || urls.length == 0) {
            return true;
        }
        for (final DownloadLink link : urls) {
            link.setAvailableStatus(requestFileInformation(link));
        }
        if (Boolean.TRUE.equals(getPluginConfig().getBooleanProperty(ON_LINKCHECK))) {
            retry(toPluginView(urls));
        }
        return true;
    }

    @Override
    public List<JComponent> extendLinkgrabberContextMenu(final AtomicBoolean isCancelled, JComponent parent, final PluginView<CrawledLink> pv, final Collection<PluginView<CrawledLink>> allPvs) {
        return extendTableContextMenu(isCancelled, parent, pv);
    }

    private void retry(final PluginView<? extends AbstractPackageChildrenNode> pv) {
        if (pv.getChildren().size() == 0) {
            return;
        }
        if (pv.getChildren().get(0) instanceof DownloadLink) {
            DownloadController.getInstance().getQueue().addAsynch(new QueueAction<Void, RuntimeException>() {
                @Override
                protected Void run() throws RuntimeException {
                    final List<DownloadLink> downloadLinks = new ArrayList<DownloadLink>((List<DownloadLink>) pv.getChildren());
                    DownloadController.getInstance().removeChildren(downloadLinks);
                    final List<CrawledLink> crawledLinks = new ArrayList<CrawledLink>();
                    for (final DownloadLink downloadLink : downloadLinks) {
                        // allow LinkCrawler to process this link again
                        downloadLink.setDefaultPlugin(null);
                        // required for LinkCrawler.breakPluginForDecryptLoop
                        downloadLink.setAvailableStatus(AvailableStatus.UNCHECKED);
                        crawledLinks.add(new CrawledLink(downloadLink));
                    }
                    final JobLinkCrawler jlc = LinkCollector.getInstance().newJobLinkCrawler(new LinkCollectingJob(LinkOrigin.ADD_LINKS_DIALOG.getLinkOriginDetails()));
                    jlc.crawl(crawledLinks);
                    return null;
                }
            });
        } else {
            LinkCollector.getInstance().getQueue().addAsynch(new QueueAction<Void, RuntimeException>() {
                @Override
                protected Void run() throws RuntimeException {
                    final List<CrawledLink> crawledLinks = new ArrayList<CrawledLink>((List<CrawledLink>) pv.getChildren());
                    LinkCollector.getInstance().removeChildren(crawledLinks);
                    for (final CrawledLink crawledLink : crawledLinks) {
                        final DownloadLink downloadLink = crawledLink.getDownloadLink();
                        // allow LinkCrawler to process this link again
                        downloadLink.setDefaultPlugin(null);
                        // required for LinkCrawler.breakPluginForDecryptLoop
                        downloadLink.setAvailableStatus(AvailableStatus.UNCHECKED);
                    }
                    final JobLinkCrawler jlc = LinkCollector.getInstance().newJobLinkCrawler(new LinkCollectingJob(LinkOrigin.ADD_LINKS_DIALOG.getLinkOriginDetails()));
                    jlc.crawl(crawledLinks);
                    return null;
                }
            });
        }
    }

    private List<JComponent> extendTableContextMenu(final AtomicBoolean isCancelled, final JComponent parent, final PluginView<? extends AbstractPackageChildrenNode> pv) {
        if (pv.size() == 0) {
            return null;
        }
        final List<JComponent> ret = new ArrayList<JComponent>();
        ret.add(new JMenuItem(new BasicAction() {
            {
                setName(_GUI.T.AddLinksDialog_AddLinksDialog_());
                setSmallIcon(new AbstractIcon(IconKey.ICON_LINKGRABBER, 16));
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                retry(pv);
            }
        }));
        return ret;
    }

    @Override
    public List<JComponent> extendDownloadsTableContextMenu(final AtomicBoolean isCancelled, final JComponent parent, final PluginView<DownloadLink> pv, final Collection<PluginView<DownloadLink>> views) {
        return extendTableContextMenu(isCancelled, parent, pv);
    }

    @Override
    public boolean isPremiumEnabled() {
        return false;
    }

    @Override
    public void handleFree(DownloadLink link) throws Exception {
        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
    }

    @Override
    public void handlePremium(DownloadLink link, Account account) throws Exception {
        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
    }

    private void setConfigElements() {
        this.getConfig().addEntry(new ConfigEntry(ConfigContainer.TYPE_CHECKBOX, this.getPluginConfig(), ON_LINKCHECK, _GUI.T.AddLinksDialog_AddLinksDialog_() + " on link check ?").setDefaultValue(false));
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }
}
