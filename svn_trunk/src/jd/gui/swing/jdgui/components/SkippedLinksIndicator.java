package jd.gui.swing.jdgui.components;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPopupMenu;
import javax.swing.Timer;

import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.packagecontroller.AbstractPackageChildrenNodeFilter;
import jd.plugins.DownloadLink;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.swing.components.tooltips.ExtTooltip;
import org.appwork.swing.components.tooltips.PanelToolTip;
import org.appwork.swing.components.tooltips.ToolTipController;
import org.appwork.swing.components.tooltips.TooltipPanel;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;
import org.jdownloader.DomainInfo;
import org.jdownloader.actions.AppAction;
import org.jdownloader.captcha.blacklist.BlacklistEntry;
import org.jdownloader.captcha.blacklist.BlockAllDownloadCaptchasEntry;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByHost;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByLink;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByPackage;
import org.jdownloader.captcha.blacklist.CaptchaBlackList;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.toolbar.UnskipLinksAction;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.SkipReason;
import org.jdownloader.updatev2.gui.LAFOptions;

/**
 * The one status bar indicator for everything the user or the downloads have skipped: Skipped links and the download captchas the user
 * has skipped ("Skip all captchas", "Skip all captchas from host xy", ...). It is only shown while the downloads are running (not paused,
 * not stopping) and as long as something is skipped. The tooltip tells what is skipped, a left click unskips everything, a right click
 * offers to unskip only some of it (the captchas of one host or the other ones). Without an animation, unlike {@link CaptchaIndicator}.
 */
public class SkippedLinksIndicator extends IconedProcessIndicator {
    private static final long serialVersionUID = 1L;

    /** Decides which hosts a not-skip action is about. */
    private interface HostMatcher {
        boolean accept(String host);
    }

    /**
     * What is skipped right now: The blocks of the captcha blacklist (all captchas, a host, a package or a single link) and the links that
     * are skipped because of a captcha. Immutable.
     */
    private static final class SkipState {
        private static final SkipState EMPTY = new SkipState(false, new HashMap<String, Integer>(), 0, 0, 0);
        /* All download captchas are skipped. */
        private final boolean          all;
        /* The hosts which are skipped as a whole, with the number of links of them which are skipped. */
        private final Map<String, Integer> hosts;
        /* Links which are skipped because of a captcha, but their host is not skipped as a whole. */
        private final int              otherLinks;
        /* Blocks of single packages and links. */
        private final int              otherEntries;
        private final int              totalLinks;

        private SkipState(final boolean all, final Map<String, Integer> hosts, final int otherLinks, final int otherEntries, final int totalLinks) {
            this.all = all;
            this.hosts = hosts;
            this.otherLinks = otherLinks;
            this.otherEntries = otherEntries;
            this.totalLinks = totalLinks;
        }

        private boolean isEmpty() {
            return !all && hosts.isEmpty() && otherLinks == 0 && otherEntries == 0;
        }

        private boolean hasOthers() {
            return otherLinks > 0 || otherEntries > 0;
        }

        private List<String> getSortedHosts() {
            final List<String> ret = new ArrayList<String>(hosts.keySet());
            Collections.sort(ret, String.CASE_INSENSITIVE_ORDER);
            return ret;
        }

        /** Reads the blacklist and the download list. Can take a while with many links, so not for the EDT. */
        private static SkipState read() {
            boolean all = false;
            int otherEntries = 0;
            final Map<String, Integer> hosts = new HashMap<String, Integer>();
            for (final BlacklistEntry entry : CaptchaBlackList.getInstance().listEntries()) {
                if (entry instanceof BlockAllDownloadCaptchasEntry) {
                    all = true;
                } else if (entry instanceof BlockDownloadCaptchasByHost) {
                    final String host = ((BlockDownloadCaptchasByHost) entry).getBlockedHost();
                    if (host != null) {
                        hosts.put(host, Integer.valueOf(0));
                    }
                } else if (entry instanceof BlockDownloadCaptchasByPackage || entry instanceof BlockDownloadCaptchasByLink) {
                    otherEntries++;
                }
            }
            int otherLinks = 0;
            int totalLinks = 0;
            for (final DownloadLink link : listCaptchaSkippedLinks(null)) {
                totalLinks++;
                final Integer count = hosts.get(link.getHost());
                if (count != null) {
                    hosts.put(link.getHost(), Integer.valueOf(count.intValue() + 1));
                } else {
                    otherLinks++;
                }
            }
            return new SkipState(all, hosts, otherLinks, otherEntries, totalLinks);
        }
    }

    /** The download links which are skipped because of a captcha, only the ones of the hosts the matcher accepts if there is one. */
    private static List<DownloadLink> listCaptchaSkippedLinks(final HostMatcher matcher) {
        return DownloadController.getInstance().getChildrenByFilter(new AbstractPackageChildrenNodeFilter<DownloadLink>() {
            @Override
            public int returnMaxResults() {
                return 0;
            }

            @Override
            public boolean acceptNode(final DownloadLink node) {
                return node.isSkipped() && SkipReason.CAPTCHA.equals(node.getSkipReason()) && (matcher == null || matcher.accept(node.getHost()));
            }
        });
    }

    private final StatusBarImpl statusBar;
    private final DelayedRunnable updateDelayer;
    /* The following fields are only accessed in the EDT. */
    private SkipState             state                = SkipState.EMPTY;
    private boolean               shown                = false;
    private long                  lastBlackListVersion = -1;
    private int                   lastSkipCounter      = -1;

    public SkippedLinksIndicator(final StatusBarImpl statusBar) {
        super(new AbstractIcon(IconKey.ICON_SKIPPED, 16));
        this.statusBar = statusBar;
        setTitle(_GUI.T.StatusBarImpl_skippedLinksMarker_title());
        /* No animation. */
        setIndeterminate(false);
        setEnabled(true);
        setValue(100);
        updateDelayer = new DelayedRunnable(ToolTipController.EXECUTER, 200, 1000) {
            @Override
            public String getID() {
                return "SkippedLinksIndicator";
            }

            @Override
            public void delayedrun() {
                final SkipState newState = SkipState.read();
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        apply(newState);
                    }
                };
            }
        };
        /*
         * The blacklist and the skipped links change without a common event: Check the (cheap) change markers regularly, the download list
         * is only read again if one of them changed.
         */
        new Timer(1000, new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                refresh();
            }
        }).start();
    }

    /**
     * Shows or hides the indicator, and reads what is skipped if that has changed. Runs in the EDT. Also called when the state of the
     * downloads changes, so the indicator disappears right away on pause or stop.
     */
    public void refresh() {
        if (!DownloadWatchDog.getInstance().isRunningAndNotPaused()) {
            setShown(false);
            lastBlackListVersion = -1;
            lastSkipCounter = -1;
            return;
        }
        final long blackListVersion = CaptchaBlackList.getInstance().getVersion();
        final int skipCounter = DownloadWatchDog.getInstance().getSession().getSkipCounter();
        if (blackListVersion == lastBlackListVersion && skipCounter == lastSkipCounter) {
            return;
        }
        lastBlackListVersion = blackListVersion;
        lastSkipCounter = skipCounter;
        /* Skipped links are known right away, the captcha blocks are read in the background. */
        setShown(skipCounter > 0 || !state.isEmpty());
        updateDelayer.resetAndStart();
    }

    private void apply(final SkipState newState) {
        state = newState;
        setShown(DownloadWatchDog.getInstance().isRunningAndNotPaused() && (DownloadWatchDog.getInstance().getSession().getSkipCounter() > 0 || !newState.isEmpty()));
    }

    private void setShown(final boolean newShown) {
        if (shown == newShown) {
            return;
        }
        shown = newShown;
        if (newShown) {
            statusBar.addProcessIndicator(this);
        } else {
            statusBar.removeProcessIndicator(this);
        }
    }

    private static Icon getHostIcon(final String host) {
        try {
            return DomainInfo.getInstance(host).getIcon(16);
        } catch (final Throwable e) {
            /* Icon is optional */
            return null;
        }
    }

    private static JLabel createTooltipLabel(final String text, final Icon icon) {
        final JLabel label = new JLabel(text);
        label.setForeground(LAFOptions.getInstance().getColorForTooltipForeground());
        if (icon != null) {
            label.setIcon(icon);
        }
        return label;
    }

    @Override
    public ExtTooltip createExtTooltip(final Point mousePosition) {
        final SkipState current = state;
        final TooltipPanel rows = new TooltipPanel("ins 0,wrap 1", "[grow,fill]", "[]");
        final JLabel title = createTooltipLabel(getTitle(), null);
        SwingUtils.toBold(title);
        rows.add(title);
        final int skipCounter = DownloadWatchDog.getInstance().getSession().getSkipCounter();
        if (skipCounter > 0) {
            rows.add(createTooltipLabel(_GUI.T.SkippedLinksIndicator_links(skipCounter), null), "gapleft 12");
        }
        if (!current.isEmpty()) {
            final JLabel captchaTitle = createTooltipLabel(_GUI.T.SkippedLinksIndicator_captchas(), null);
            SwingUtils.toBold(captchaTitle);
            rows.add(captchaTitle, "gaptop 5");
        }
        if (current.all) {
            rows.add(createTooltipLabel(_GUI.T.SkippedLinksIndicator_all(String.valueOf(current.totalLinks)), null), "gapleft 12");
        } else {
            for (final String host : current.getSortedHosts()) {
                rows.add(createTooltipLabel(_GUI.T.SkippedLinksIndicator_host(host, String.valueOf(current.hosts.get(host))), getHostIcon(host)), "gapleft 12");
            }
            if (current.hasOthers()) {
                rows.add(createTooltipLabel(_GUI.T.SkippedLinksIndicator_others(String.valueOf(current.otherLinks)), null), "gapleft 12");
            }
        }
        rows.add(createTooltipLabel(_GUI.T.SkippedLinksIndicator_hint(), null), "gaptop 5");
        return new PanelToolTip(rows);
    }

    private void addAction(final JPopupMenu popup, final String name, final Icon icon, final Runnable action) {
        popup.add(new AppAction() {
            private static final long serialVersionUID = 1L;
            {
                setName(name);
                setSmallIcon(icon);
            }

            @Override
            public void actionPerformed(final ActionEvent e) {
                action.run();
            }
        });
    }

    protected JPopupMenu createPopup() {
        final SkipState current = state;
        final JPopupMenu popup = new JPopupMenu();
        addAction(popup, _GUI.T.SkippedLinksIndicator_unskipAll(), new AbstractIcon(IconKey.ICON_UNDO, 16), new Runnable() {
            @Override
            public void run() {
                UnskipLinksAction.unSkipAll();
            }
        });
        if (!current.all) {
            /* While all captchas are skipped, not skipping only some of them would change nothing. */
            for (final String host : current.getSortedHosts()) {
                addAction(popup, _GUI.T.SkippedLinksIndicator_unskipHost(host), getHostIcon(host), new Runnable() {
                    @Override
                    public void run() {
                        unSkipHost(host);
                    }
                });
            }
            if (current.hasOthers()) {
                final Set<String> skippedHosts = new HashSet<String>(current.hosts.keySet());
                addAction(popup, _GUI.T.SkippedLinksIndicator_unskipOthers(), new AbstractIcon(IconKey.ICON_UNDO, 16), new Runnable() {
                    @Override
                    public void run() {
                        unSkipOthers(skippedHosts);
                    }
                });
            }
        }
        return popup;
    }

    @Override
    public void mouseReleased(final MouseEvent e) {
        if (e.isPopupTrigger() || e.getButton() == MouseEvent.BUTTON3) {
            final JPopupMenu popup = createPopup();
            popup.show(SkippedLinksIndicator.this, e.getPoint().x, 0 - popup.getPreferredSize().height);
        } else if (e.getButton() == MouseEvent.BUTTON1) {
            UnskipLinksAction.unSkipAll();
        }
    }

    /** The captchas of the given host are not skipped any more. */
    private void unSkipHost(final String host) {
        for (final BlacklistEntry entry : CaptchaBlackList.getInstance().listEntries()) {
            if (entry instanceof BlockDownloadCaptchasByHost && host.equals(((BlockDownloadCaptchasByHost) entry).getBlockedHost())) {
                CaptchaBlackList.getInstance().remove(entry);
            }
        }
        unSkipLinks(new HostMatcher() {
            @Override
            public boolean accept(final String linkHost) {
                return host.equals(linkHost);
            }
        });
    }

    /** The captchas which are not skipped by host are not skipped any more: The blocks of single packages and links, and their links. */
    private void unSkipOthers(final Set<String> skippedHosts) {
        for (final BlacklistEntry entry : CaptchaBlackList.getInstance().listEntries()) {
            if (entry instanceof BlockDownloadCaptchasByPackage || entry instanceof BlockDownloadCaptchasByLink) {
                CaptchaBlackList.getInstance().remove(entry);
            }
        }
        unSkipLinks(new HostMatcher() {
            @Override
            public boolean accept(final String linkHost) {
                return !skippedHosts.contains(linkHost);
            }
        });
    }

    /**
     * Resets the links that are skipped because of a captcha (only the ones of the hosts the matcher accepts, all if there is none). The
     * blocks have to be removed before, otherwise the links would be skipped again. Looks for the links in a thread of its own, the
     * download list can be big.
     */
    private void unSkipLinks(final HostMatcher matcher) {
        final Thread thread = new Thread("SkippedLinksIndicator:UnSkip") {
            @Override
            public void run() {
                final List<DownloadLink> links = listCaptchaSkippedLinks(matcher);
                if (links.size() > 0) {
                    DownloadWatchDog.getInstance().unSkip(links);
                }
            }
        };
        thread.setDaemon(true);
        thread.start();
    }
}
