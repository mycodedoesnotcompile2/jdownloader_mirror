package org.jdownloader.gui.toolbar;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Set;

import jd.controlling.TaskQueue;
import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.packagecontroller.AbstractPackageChildrenNodeFilter;
import jd.controlling.proxy.ProxyController;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.appwork.utils.StringUtils;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.captcha.blacklist.BlacklistEntry;
import org.jdownloader.captcha.blacklist.BlockAllDownloadCaptchasEntry;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByHost;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByLink;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByPackage;
import org.jdownloader.captcha.blacklist.CaptchaBlackList;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.SkipReason;

public class UnskipLinksAction extends CustomizableAppAction implements ActionContext {

    public UnskipLinksAction() {
        addContextSetup(this);
        setName(_GUI.T.UnskipAllSkippedLinksAction());
        setIconKey(IconKey.ICON_SKIPPED);
    }

    /**
     * @param path
     */
    public void updateIcon(String path) {
        if (StringUtils.equals(ProxyController.getInstance().getLatestProfilePath(), path) && StringUtils.isNotEmpty(path)) {
            setIconKey(IconKey.ICON_PROXY_ROTATE);
        } else {
            setIconKey(IconKey.ICON_PROXY);
        }
    }

    /** Decides which links an unskip is about. */
    public interface LinkMatcher {
        boolean accept(DownloadLink link);
    }

    /**
     * All unskip methods below run in the {@link TaskQueue}, so they can be called from the EDT: They search the download list and change
     * the blacklist in order with the other changes of the download list.
     */
    private static void enqueue(final Runnable runnable) {
        TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                runnable.run();
                return null;
            }
        });
    }

    /** True for the blocks of the download captchas (all, host, package, link). */
    public static boolean isDownloadEntry(final BlacklistEntry entry) {
        return entry instanceof BlockAllDownloadCaptchasEntry || entry instanceof BlockDownloadCaptchasByHost || entry instanceof BlockDownloadCaptchasByPackage || entry instanceof BlockDownloadCaptchasByLink;
    }

    /** Removes all captcha blocks of the downloads. */
    private static void removeDownloadEntries() {
        for (final BlacklistEntry entry : CaptchaBlackList.getInstance().listEntries()) {
            if (isDownloadEntry(entry)) {
                CaptchaBlackList.getInstance().remove(entry);
            }
        }
    }

    /** Removes the blocks of the captchas of the given hosts. */
    private static void removeHostBlocks(final Set<String> hosts) {
        for (final BlacklistEntry entry : CaptchaBlackList.getInstance().listEntries()) {
            if (entry instanceof BlockDownloadCaptchasByHost && hosts.contains(((BlockDownloadCaptchasByHost) entry).getBlockedHost())) {
                CaptchaBlackList.getInstance().remove(entry);
            }
        }
    }

    /**
     * The download links which are skipped because of a captcha, only the ones the matcher accepts if there is one. Can take a while with
     * many links, so not for the EDT.
     */
    public static List<DownloadLink> listCaptchaSkippedLinks(final LinkMatcher matcher) {
        return DownloadController.getInstance().getChildrenByFilter(new AbstractPackageChildrenNodeFilter<DownloadLink>() {
            @Override
            public int returnMaxResults() {
                return 0;
            }

            @Override
            public boolean acceptNode(final DownloadLink node) {
                return node.isSkipped() && SkipReason.CAPTCHA.equals(node.getSkipReason()) && (matcher == null || matcher.accept(node));
            }
        });
    }

    /**
     * Resets the links that are skipped because of a captcha (only the ones the matcher accepts, all if there is none). The blocks have to
     * be removed before, otherwise the links would be skipped again. With removeHostBlocks the blocks of the hosts of the links that are
     * reset are removed too, otherwise the links would be skipped again as soon as the host asks for a captcha.
     */
    private static void unSkipCaptchaLinksNow(final LinkMatcher matcher, final boolean removeHostBlocks) {
        final List<DownloadLink> links = listCaptchaSkippedLinks(matcher);
        if (removeHostBlocks) {
            final Set<String> hosts = new HashSet<String>();
            for (final DownloadLink link : links) {
                hosts.add(link.getHost());
            }
            removeHostBlocks(hosts);
        }
        if (links.size() > 0) {
            DownloadWatchDog.getInstance().unSkip(links);
        }
    }

    /**
     * Unskips everything: Removes all captcha blocks of the downloads ("Skip all captchas", ...) and resets all skipped links, whatever
     * the reason is. The blocks have to be removed first, otherwise the links would be skipped again.
     */
    public static void unSkipAll() {
        enqueue(new Runnable() {
            @Override
            public void run() {
                removeDownloadEntries();
                DownloadWatchDog.getInstance().unSkipAllSkipped();
            }
        });
    }

    /** All captchas are not skipped any more: Removes all captcha blocks of the downloads and resets all links skipped by a captcha. */
    public static void unSkipCaptchas() {
        enqueue(new Runnable() {
            @Override
            public void run() {
                removeDownloadEntries();
                unSkipCaptchaLinksNow(null, false);
            }
        });
    }

    /**
     * All captcha skips are not skipped any more, except the block of all captchas: Removes the blocks of hosts, packages and single links
     * and resets all links skipped by a captcha.
     */
    public static void unSkipCaptchaSkips() {
        enqueue(new Runnable() {
            @Override
            public void run() {
                for (final BlacklistEntry entry : CaptchaBlackList.getInstance().listEntries()) {
                    if (entry instanceof BlockDownloadCaptchasByHost || entry instanceof BlockDownloadCaptchasByPackage || entry instanceof BlockDownloadCaptchasByLink) {
                        CaptchaBlackList.getInstance().remove(entry);
                    }
                }
                unSkipCaptchaLinksNow(null, false);
            }
        });
    }

    /** The captchas of the given hosts are not skipped any more, in the whole download list: Removes the host blocks and resets the links. */
    public static void unSkipCaptchaHosts(final Set<String> hostsToUnskip) {
        final Set<String> hosts = new HashSet<String>(hostsToUnskip);
        enqueue(new Runnable() {
            @Override
            public void run() {
                removeHostBlocks(hosts);
                unSkipCaptchaLinksNow(new LinkMatcher() {
                    @Override
                    public boolean accept(final DownloadLink link) {
                        return hosts.contains(link.getHost());
                    }
                }, false);
            }
        });
    }

    /**
     * The captchas of the given packages are not skipped any more: Removes the blocks of the packages and of their links, resets all their
     * links that are skipped because of a captcha (also the ones skipped one by one) and removes the blocks of the hosts of these links.
     */
    public static void unSkipCaptchaPackages(final Collection<FilePackage> packages) {
        final Set<FilePackage> set = Collections.newSetFromMap(new IdentityHashMap<FilePackage, Boolean>());
        set.addAll(packages);
        enqueue(new Runnable() {
            @Override
            public void run() {
                for (final BlacklistEntry entry : CaptchaBlackList.getInstance().listEntries()) {
                    if (entry instanceof BlockDownloadCaptchasByPackage) {
                        if (set.contains(((BlockDownloadCaptchasByPackage) entry).getFilePackage())) {
                            CaptchaBlackList.getInstance().remove(entry);
                        }
                    } else if (entry instanceof BlockDownloadCaptchasByLink) {
                        final DownloadLink link = ((BlockDownloadCaptchasByLink) entry).getDownloadLink();
                        if (link != null && set.contains(link.getParentNode())) {
                            CaptchaBlackList.getInstance().remove(entry);
                        }
                    }
                }
                unSkipCaptchaLinksNow(new LinkMatcher() {
                    @Override
                    public boolean accept(final DownloadLink link) {
                        return set.contains(link.getParentNode());
                    }
                }, true);
            }
        });
    }

    /**
     * True if the captchas of one of the given links are blocked: By a block of the link, of its package or of its host (a block of all
     * captchas is not counted). Only reads, so it can be used to decide whether an action is enabled.
     */
    public static boolean isBlocked(final Collection<DownloadLink> links) {
        final Set<FilePackage> packages = Collections.newSetFromMap(new IdentityHashMap<FilePackage, Boolean>());
        final Set<DownloadLink> selected = Collections.newSetFromMap(new IdentityHashMap<DownloadLink, Boolean>());
        final Set<String> hosts = new HashSet<String>();
        for (final DownloadLink link : links) {
            selected.add(link);
            packages.add(link.getParentNode());
            hosts.add(link.getHost());
        }
        for (final BlacklistEntry entry : CaptchaBlackList.getInstance().listEntries()) {
            if (entry instanceof BlockDownloadCaptchasByLink) {
                if (selected.contains(((BlockDownloadCaptchasByLink) entry).getDownloadLink())) {
                    return true;
                }
            } else if (entry instanceof BlockDownloadCaptchasByPackage) {
                if (packages.contains(((BlockDownloadCaptchasByPackage) entry).getFilePackage())) {
                    return true;
                }
            } else if (entry instanceof BlockDownloadCaptchasByHost) {
                if (hosts.contains(((BlockDownloadCaptchasByHost) entry).getBlockedHost())) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Unskips the given links, whatever the reason is, and removes the captcha blocks that apply to them (the block of the link, of its
     * package and of its host), otherwise they would be skipped again. A block of all captchas stays.
     */
    public static void unSkipLinks(final Collection<DownloadLink> linksToUnskip) {
        final List<DownloadLink> links = new ArrayList<DownloadLink>(linksToUnskip);
        enqueue(new Runnable() {
            @Override
            public void run() {
                final Set<FilePackage> packages = Collections.newSetFromMap(new IdentityHashMap<FilePackage, Boolean>());
                final Set<DownloadLink> selected = Collections.newSetFromMap(new IdentityHashMap<DownloadLink, Boolean>());
                final Set<String> hosts = new HashSet<String>();
                final List<DownloadLink> skipped = new ArrayList<DownloadLink>();
                for (final DownloadLink link : links) {
                    selected.add(link);
                    packages.add(link.getParentNode());
                    hosts.add(link.getHost());
                    if (link.isSkipped()) {
                        skipped.add(link);
                    }
                }
                for (final BlacklistEntry entry : CaptchaBlackList.getInstance().listEntries()) {
                    if (entry instanceof BlockDownloadCaptchasByLink) {
                        if (selected.contains(((BlockDownloadCaptchasByLink) entry).getDownloadLink())) {
                            CaptchaBlackList.getInstance().remove(entry);
                        }
                    } else if (entry instanceof BlockDownloadCaptchasByPackage) {
                        if (packages.contains(((BlockDownloadCaptchasByPackage) entry).getFilePackage())) {
                            CaptchaBlackList.getInstance().remove(entry);
                        }
                    } else if (entry instanceof BlockDownloadCaptchasByHost) {
                        if (hosts.contains(((BlockDownloadCaptchasByHost) entry).getBlockedHost())) {
                            CaptchaBlackList.getInstance().remove(entry);
                        }
                    }
                }
                if (skipped.size() > 0) {
                    DownloadWatchDog.getInstance().unSkip(skipped);
                }
            }
        });
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        try {
            unSkipAll();

        } catch (Throwable e1) {
            Dialog.getInstance().showExceptionDialog(_GUI.T.lit_error_occured(), e1.getMessage(), e1);
        } finally {

        }
    }

}
