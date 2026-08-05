package org.jdownloader.gui.views.components.packagetable;

import java.io.File;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.controlling.packagizer.PackagizerController;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.settings.UrlDisplayType;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

public class LinkTreeUtils {
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static <T extends AbstractNode> java.util.List<T> getPackages(AbstractNode contextObject, java.util.List<AbstractNode> selection, java.util.List<T> container) {
        HashSet<T> ret = new HashSet<T>();
        if (contextObject != null) {
            if (contextObject instanceof AbstractPackageNode) {
                ret.add((T) contextObject);
            } else {
                ret.add((T) ((AbstractPackageChildrenNode) contextObject).getParentNode());
            }
        }
        if (selection != null) {
            for (AbstractNode a : selection) {
                if (a instanceof AbstractPackageNode) {
                    ret.add((T) a);
                } else {
                    ret.add((T) ((AbstractPackageChildrenNode) a).getParentNode());
                }
            }
        }
        container.addAll(ret);
        return container;
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static <T extends AbstractNode> java.util.List<T> getSelectedChildren(List<AbstractNode> selection2, java.util.List<T> container) {
        HashSet<AbstractNode> has = new HashSet<AbstractNode>(selection2);
        HashSet<T> ret = new HashSet<T>();
        for (AbstractNode node : selection2) {
            if (node instanceof AbstractPackageChildrenNode) {
                ret.add((T) node);
            } else {
                // if we selected a package, and ALL it's links, we want all links
                // if we selected a package, and only a few links, we probably want only these few links.
                // if we selected a package, and it is NOT expanded, we want all links
                boolean readL = ((AbstractPackageNode) node).getModifyLock().readLock();
                try {
                    if (!((AbstractPackageNode) node).isExpanded()) {
                        // add allTODO
                        List<T> childs = ((AbstractPackageNode) node).getChildren();
                        ret.addAll(childs);
                        // LinkGrabberTableModel.getInstance().getAllChildrenNodes()
                    } else {
                        List<T> childs = ((AbstractPackageNode) node).getChildren();
                        boolean containsNone = true;
                        boolean containsAll = true;
                        for (AbstractNode l : childs) {
                            if (has.contains(l)) {
                                containsNone = false;
                            } else {
                                containsAll = false;
                            }
                        }
                        if (containsAll || containsNone) {
                            ret.addAll(childs);
                        }
                    }
                } finally {
                    ((AbstractPackageNode) node).getModifyLock().readUnlock(readL);
                }
            }
        }
        container.addAll(ret);
        return container;
    }

    public static File getDownloadDirectory(AbstractNode node) {
        if (node instanceof DownloadLink) {
            final FilePackage parent = ((DownloadLink) node).getFilePackage();
            final String directory = parent == null ? null : parent.getView().getDownloadDirectory();
            return getDownloadDirectory(directory, parent == null ? null : parent.getName(), node);
        }
        if (node instanceof FilePackage) {
            final String directory = ((FilePackage) node).getView().getDownloadDirectory();
            return getDownloadDirectory(directory, ((FilePackage) node).getName(), node);
        }
        if (node instanceof CrawledLink) {
            final CrawledPackage parent = ((CrawledLink) node).getParentNode();
            final String directory = parent == null ? null : parent.getDownloadFolder();
            return getDownloadDirectory(directory, parent == null ? null : parent.getName(), node);
        }
        if (node instanceof CrawledPackage) {
            final String directory = ((CrawledPackage) node).getDownloadFolder();
            return getDownloadDirectory(directory, ((CrawledPackage) node).getName(), node);
        }
        throw new WTFException("Unknown Type: " + node.getClass());
    }

    public static File getRawDownloadDirectory(AbstractNode node) {
        if (node == null) {
            return getRawDownloadDirectory((String) null);
        }
        if (node instanceof DownloadLink) {
            final FilePackage parent = ((DownloadLink) node).getFilePackage();
            return getRawDownloadDirectory(parent == null ? null : parent.getView().getDownloadDirectory());
        }
        if (node instanceof FilePackage) {
            return getRawDownloadDirectory(((FilePackage) node).getView().getDownloadDirectory());
        }
        if (node instanceof CrawledLink) {
            final CrawledPackage parent = ((CrawledLink) node).getParentNode();
            return getRawDownloadDirectory(parent == null ? null : parent.getRawDownloadFolder());
        }
        if (node instanceof CrawledPackage) {
            return getRawDownloadDirectory(((CrawledPackage) node).getRawDownloadFolder());
        }
        throw new WTFException("Unknown Type: " + node.getClass());
    }

    private static File getRawDownloadDirectory(String path) {
        if (StringUtils.isEmpty(path)) {
            /* Return default value */
            return new File(org.jdownloader.settings.staticreferences.CFG_GENERAL.DEFAULT_DOWNLOAD_FOLDER.getValue());
        }
        if (CrossSystem.isAbsolutePath(path)) {
            return new File(path);
        }
        return new File(org.jdownloader.settings.staticreferences.CFG_GENERAL.DEFAULT_DOWNLOAD_FOLDER.getValue(), path);
    }

    /** Returns download directory with dynamic tags replaced. */
    public static File getDownloadDirectory(String path, String packagename, AbstractNode node) {
        if (StringUtils.isEmpty(path)) {
            /* Return default value */
            return new File(PackagizerController.replaceDynamicTags(org.jdownloader.settings.staticreferences.CFG_GENERAL.DEFAULT_DOWNLOAD_FOLDER.getValue(), packagename, node));
        }
        path = PackagizerController.replaceDynamicTags(path, packagename, node);
        if (CrossSystem.isAbsolutePath(path)) {
            return new File(path);
        }
        return new File(PackagizerController.replaceDynamicTags(org.jdownloader.settings.staticreferences.CFG_GENERAL.DEFAULT_DOWNLOAD_FOLDER.getValue(), packagename, node), path);
    }

    public static String getUrlByType(final UrlDisplayType dt, final AbstractNode node) {
        final DownloadLink link;
        if (node instanceof DownloadLink) {
            link = (DownloadLink) node;
        } else if (node instanceof CrawledLink) {
            link = ((CrawledLink) node).getDownloadLink();
        } else {
            return null;
        }
        switch (dt) {
        case CUSTOM:
            return link.getCustomUrl();
        case REFERRER:
            return link.getReferrerUrl();
        case CONTAINER:
            return link.getContainerUrl();
        case ORIGIN:
            return link.getOriginUrl();
        case CONTENT:
            switch (link.getUrlProtection()) {
            case UNSET:
                final String contentURL = link.getContentUrl();
                if (contentURL != null) {
                    return contentURL;
                }
                return link.getPluginPatternMatcher();
            default:
                return null;
            }
        default:
            return null;
        }
    }

    /**
     * Collects the URLs of the given {@link UrlDisplayType} for all selected children. Each URL is returned only once (duplicates are
     * removed) while keeping the original order.
     *
     * @param fallbackToDisplayUrl
     *            if true, links for which the requested {@link UrlDisplayType} is not available fall back to their default display URL
     *            ({@link org.jdownloader.controlling.DownloadLinkView#getDisplayUrl()}) instead of being skipped.
     */
    public static Set<String> getURLs(SelectionInfo<? extends AbstractPackageNode, ? extends AbstractPackageChildrenNode> selectionInfo, final UrlDisplayType urlDisplayType, final boolean fallbackToDisplayUrl) {
        final LinkedHashSet<String> urls = new LinkedHashSet<String>();
        if (selectionInfo == null || selectionInfo.isEmpty() || urlDisplayType == null) {
            return urls;
        }
        final List<? extends AbstractPackageChildrenNode> children = selectionInfo.getChildren();
        for (final AbstractPackageChildrenNode<?> node : children) {
            String url = getUrlByType(urlDisplayType, node);
            if (url == null && fallbackToDisplayUrl) {
                final DownloadLink link;
                if (node instanceof DownloadLink) {
                    link = (DownloadLink) node;
                } else if (node instanceof CrawledLink) {
                    link = ((CrawledLink) node).getDownloadLink();
                } else {
                    link = null;
                }
                if (link != null) {
                    url = link.getView().getDisplayUrl();
                }
            }
            if (url == null) {
                continue;
            }
            urls.add(url);
        }
        return urls;
    }

    public static Set<String> getURLs(SelectionInfo<? extends AbstractPackageNode, ? extends AbstractPackageChildrenNode> selectionInfo, final boolean openInBrowser) {
        return getURLs(selectionInfo, openInBrowser, JsonConfig.create(GeneralSettings.class).isCopySingleRealURL());
    }

    public static Set<String> getURLs(SelectionInfo<? extends AbstractPackageNode, ? extends AbstractPackageChildrenNode> selectionInfo, final boolean openInBrowser, final boolean copySingleRealURL) {
        final LinkedHashSet<String> urls = new LinkedHashSet<String>();
        if (selectionInfo == null || selectionInfo.isEmpty()) {
            return urls;
        }
        String rawURL = null;
        final List<? extends AbstractPackageChildrenNode> children = selectionInfo.getChildren();
        for (final AbstractPackageChildrenNode<?> node : children) {
            final DownloadLink link;
            if (node instanceof DownloadLink) {
                link = (DownloadLink) node;
            } else if (node instanceof CrawledLink) {
                link = ((CrawledLink) node).getDownloadLink();
            } else {
                continue;
            }
            if (link == null) {
                continue;
            }
            rawURL = link.getCustomUrl();
            if (rawURL == null) {
                rawURL = link.getContentUrlOrPatternMatcher();
            }
            final String url = link.getView().getDisplayUrl();
            if (url == null) {
                continue;
            }
            urls.add(url);
        }
        /**
         * Allows to skip url content protection for single links. <br>
         * See: https://support.jdownloader.org/knowledgebase/article/copy-view-added-urls
         */
        if (!openInBrowser && copySingleRealURL && children.size() == 1 && rawURL != null && rawURL.matches("((?-i)ftp|https?)://.+")) {
            urls.clear();
            urls.add(rawURL);
        }
        return urls;
    }
}
