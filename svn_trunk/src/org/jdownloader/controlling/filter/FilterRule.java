package org.jdownloader.controlling.filter;

import java.util.ArrayList;

import org.appwork.storage.Storable;
import org.appwork.utils.Files;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.myjdownloader.client.json.AbstractJsonData;

import jd.controlling.linkcollector.VariousCrawledLinkFlags;
import jd.controlling.linkcrawler.CrawledLink;
import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog.BooleanStatusFilter;
import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog.ConditionFilter;
import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog.DownloadListDupeFilter;
import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog.LinkEnabledFilter;
import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog.OnlineStatusFilter;
import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog.OriginFilter;
import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog.PluginStatusFilter;

public abstract class FilterRule extends AbstractJsonData implements Storable {
    private FilesizeFilter     filesizeFilter;
    private RegexFilter        hosterURLFilter;
    private RegexFilter        sourceURLFilter;
    private OnlineStatusFilter onlineStatusFilter;
    private OriginFilter       originFilter;
    /**
     * @deprecated Legacy filter that combined the {@link VariousCrawledLinkFlags} into a single multi-select filter. Kept
     *             solely so old configs can still be deserialized and migrated to {@link #linkEnabledFilter} /
     *             {@link #downloadListDupeFilter} via {@link #_migrateLegacyFilters()}. Do not use for new code.
     */
    @Deprecated
    private ConditionFilter    conditionFilter;

    /**
     * @deprecated only used by Storable (JSON key "conditionFilter") and {@link #_migrateLegacyFilters()}. Returns the raw
     *             (possibly null) stored value on purpose.
     */
    @Deprecated
    public ConditionFilter getConditionFilter() {
        return conditionFilter;
    }

    private boolean broken = false;

    public boolean _isBroken() {
        return broken;
    }

    public void _setBroken(boolean broken) {
        this.broken = broken;
    }

    /**
     * @deprecated only used by Storable and {@link #_migrateLegacyFilters()}.
     */
    @Deprecated
    public void setConditionFilter(ConditionFilter conditionFilter) {
        this.conditionFilter = conditionFilter;
    }

    private LinkEnabledFilter       linkEnabledFilter;
    private DownloadListDupeFilter  downloadListDupeFilter;

    public LinkEnabledFilter getLinkEnabledFilter() {
        if (linkEnabledFilter == null) {
            linkEnabledFilter = new LinkEnabledFilter();
        }
        return linkEnabledFilter;
    }

    public void setLinkEnabledFilter(LinkEnabledFilter linkEnabledFilter) {
        this.linkEnabledFilter = linkEnabledFilter;
    }

    public DownloadListDupeFilter getDownloadListDupeFilter() {
        if (downloadListDupeFilter == null) {
            downloadListDupeFilter = new DownloadListDupeFilter();
        }
        return downloadListDupeFilter;
    }

    public void setDownloadListDupeFilter(DownloadListDupeFilter downloadListDupeFilter) {
        this.downloadListDupeFilter = downloadListDupeFilter;
    }

    /**
     * Migrates the deprecated {@link #conditionFilter} into the dedicated {@link #linkEnabledFilter} /
     * {@link #downloadListDupeFilter}. Idempotent: after migration the legacy filter is cleared so subsequent calls (and the
     * next save) are no-ops.
     */
    public void _migrateLegacyFilters() {
        final ConditionFilter legacy = conditionFilter;
        if (legacy == null) {
            return;
        }
        conditionFilter = null;
        if (!legacy.isEnabled() || legacy.getConditions() == null) {
            return;
        }
        final BooleanStatusFilter.Matchtype matchType;
        switch (legacy.getMatchType()) {
        case IS_FALSE:
            matchType = BooleanStatusFilter.Matchtype.IS_FALSE;
            break;
        case IS_TRUE:
        default:
            matchType = BooleanStatusFilter.Matchtype.IS_TRUE;
            break;
        }
        for (final VariousCrawledLinkFlags condition : legacy.getConditions()) {
            if (condition == null) {
                continue;
            }
            switch (condition) {
            case DOWNLOAD_LIST_DUPE:
                setDownloadListDupeFilter(new DownloadListDupeFilter(matchType, true));
                break;
            case IS_ENABLED:
                setLinkEnabledFilter(new LinkEnabledFilter(matchType, true));
                break;
            }
        }
    }

    public OriginFilter getOriginFilter() {
        if (originFilter == null) {
            originFilter = new OriginFilter();
        }
        return originFilter;
    }

    public void setOriginFilter(OriginFilter originFilter) {
        this.originFilter = originFilter;
    }

    private BooleanFilter matchesAlwaysFilter;
    private String        iconKey;
    private String        testUrl;
    private long          created = System.currentTimeMillis();

    public long getCreated() {
        return created;
    }

    public void setCreated(long created) {
        this.created = created;
    }

    public String getTestUrl() {
        return testUrl;
    }

    public void setTestUrl(String testUrl) {
        this.testUrl = testUrl;
    }

    private PluginStatusFilter pluginStatusFilter;

    public String getIconKey() {
        return iconKey;
    }

    public void setIconKey(String iconKey) {
        this.iconKey = iconKey;
    }

    /**
     * is used for predefined rules only
     */
    private String id;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    /**
     * Static Rules cannot be modified. we use them for predefined rules
     */
    private boolean staticRule = false;

    public boolean isStaticRule() {
        return staticRule;
    }

    public void setStaticRule(boolean removeAllowed) {
        this.staticRule = removeAllowed;
    }

    public BooleanFilter getMatchAlwaysFilter() {
        if (matchesAlwaysFilter == null) {
            matchesAlwaysFilter = new BooleanFilter(false);
        }
        return matchesAlwaysFilter;
    }

    public void setMatchAlwaysFilter(BooleanFilter match) {
        this.matchesAlwaysFilter = match;
    }

    public FilesizeFilter getFilesizeFilter() {
        if (filesizeFilter == null) {
            filesizeFilter = new FilesizeFilter();
        }
        return filesizeFilter;
    }

    public void setFilesizeFilter(FilesizeFilter size) {
        this.filesizeFilter = size;
    }

    /**
     * Returns false if no filterrule is enabled
     *
     * @return
     */
    public boolean _isValid() {
        return getPackagenameFilter().isEnabled() || getCommentFilter().isEnabled() || getMatchAlwaysFilter().isEnabled() || getFilenameFilter().isEnabled() || getFilesizeFilter().isEnabled() || getFiletypeFilter().isEnabled() || getHosterURLFilter().isEnabled() || getSourceURLFilter().isEnabled() || getOriginFilter().isEnabled() || getLinkEnabledFilter().isEnabled() || getDownloadListDupeFilter().isEnabled() || getOnlineStatusFilter().isEnabled() || getPluginStatusFilter().isEnabled();
    }

    public String toString(CrawledLink link) {
        StringBuilder sb = new StringBuilder();
        java.util.List<String> cond = new ArrayList<String>();
        if (getMatchAlwaysFilter().isEnabled()) {
            cond.add(getMatchAlwaysFilter().toString());
        } else {
            if (getOnlineStatusFilter().isEnabled()) {
                cond.add(onlineStatusFilter.toString());
            }
            if (getOriginFilter().isEnabled()) {
                cond.add(originFilter.toString());
            }
            if (getLinkEnabledFilter().isEnabled()) {
                cond.add(linkEnabledFilter.toString());
            }
            if (getDownloadListDupeFilter().isEnabled()) {
                cond.add(downloadListDupeFilter.toString());
            }
            if (getPluginStatusFilter().isEnabled()) {
                cond.add(pluginStatusFilter.toString());
            }
            if (getFilenameFilter().isEnabled()) {
                if (link != null && link.getName() != null) {
                    cond.add(_GUI.T.FilterRule_toString_name2(link.getName(), filenameFilter.toString()));
                } else {
                    cond.add(_GUI.T.FilterRule_toString_name(filenameFilter.toString()));
                }
            }
            if (getPackagenameFilter().isEnabled()) {
                if (link != null && link.getParentNode() != null && link.getParentNode().getName() != null) {
                    cond.add(_GUI.T.FilterRule_toString_package2(link.getParentNode().getName(), packagenameFilter.toString()));
                } else {
                    cond.add(_GUI.T.FilterRule_toString_packagename(packagenameFilter.toString()));
                }
            }
            if (getCommentFilter().isEnabled()) {
                if (link != null && link.getComment() != null) {
                    cond.add(_GUI.T.FilterRule_toString_comment2(link.getComment(), commentFilter.toString()));
                } else {
                    cond.add(_GUI.T.FilterRule_toString_comment(commentFilter.toString()));
                }
            }
            if (getFilesizeFilter().isEnabled()) {
                if (link != null && link.getSize() > 0) {
                    cond.add(_GUI.T.FilterRule_toString_size2(SizeFormatter.formatBytes(link.getSize()), filesizeFilter.toString()));
                } else {
                    cond.add(_GUI.T.FilterRule_toString_size(filesizeFilter.toString()));
                }
            }
            if (getFiletypeFilter().isEnabled()) {
                if (link != null && link.getName() != null && Files.getExtension(link.getName()) != null) {
                    String ext = Files.getExtension(link.getName());
                    cond.add(_GUI.T.FilterRule_toString_type2(ext, filetypeFilter.toString()));
                } else {
                    cond.add(_GUI.T.FilterRule_toString_type(filetypeFilter.toString()));
                }
            }
            if (getHosterURLFilter().isEnabled()) {
                if (link != null) {
                    cond.add(_GUI.T.FilterRule_toString_hoster2(link.getURL(), hosterURLFilter.toString()));
                } else {
                    cond.add(_GUI.T.FilterRule_toString_hoster(hosterURLFilter.toString()));
                }
            }
            if (getSourceURLFilter().isEnabled()) {
                cond.add(_GUI.T.FilterRule_toString_source(sourceURLFilter.toString()));
            }
        }
        for (int i = 0; i < cond.size(); i++) {
            if (i > 0) {
                if (i < cond.size() - 1) {
                    sb.append(_GUI.T.FilterRule_toString_comma3(cond.get(i)));
                } else {
                    sb.append(" " + _GUI.T.FilterRule_toString_and2(cond.get(i)).trim());
                }
            } else {
                sb.append(cond.get(i));
            }
        }
        return sb.toString();
    }

    public String toString() {
        return toString(null);
    }

    public RegexFilter getHosterURLFilter() {
        if (hosterURLFilter == null) {
            hosterURLFilter = new RegexFilter();
        }
        return hosterURLFilter;
    }

    public void setHosterURLFilter(RegexFilter hoster) {
        this.hosterURLFilter = hoster;
    }

    public RegexFilter getSourceURLFilter() {
        if (sourceURLFilter == null) {
            sourceURLFilter = new RegexFilter();
        }
        return sourceURLFilter;
    }

    public void setSourceURLFilter(RegexFilter source) {
        this.sourceURLFilter = source;
    }

    public FiletypeFilter getFiletypeFilter() {
        if (filetypeFilter == null) {
            filetypeFilter = new FiletypeFilter();
        }
        return filetypeFilter;
    }

    public void setFiletypeFilter(FiletypeFilter type) {
        this.filetypeFilter = type;
    }

    public void setOnlineStatusFilter(OnlineStatusFilter onlineStatusFilter) {
        this.onlineStatusFilter = onlineStatusFilter;
    }

    public OnlineStatusFilter getOnlineStatusFilter() {
        if (onlineStatusFilter == null) {
            onlineStatusFilter = new OnlineStatusFilter();
        }
        return onlineStatusFilter;
    }

    public void setPluginStatusFilter(PluginStatusFilter pluginStatusFilter) {
        this.pluginStatusFilter = pluginStatusFilter;
    }

    public PluginStatusFilter getPluginStatusFilter() {
        if (pluginStatusFilter == null) {
            pluginStatusFilter = new PluginStatusFilter();
        }
        return pluginStatusFilter;
    }

    public RegexFilter getFilenameFilter() {
        if (filenameFilter == null) {
            filenameFilter = new RegexFilter();
        }
        return filenameFilter;
    }

    public void setFilenameFilter(RegexFilter filename) {
        this.filenameFilter = filename;
    }

    private FiletypeFilter filetypeFilter;
    private RegexFilter    filenameFilter;
    private RegexFilter    packagenameFilter;
    private RegexFilter    commentFilter;

    public RegexFilter getPackagenameFilter() {
        if (packagenameFilter == null) {
            packagenameFilter = new RegexFilter();
        }
        return packagenameFilter;
    }

    public void setPackagenameFilter(RegexFilter packagenameFilter) {
        this.packagenameFilter = packagenameFilter;
    }

    public RegexFilter getCommentFilter() {
        if (commentFilter == null) {
            commentFilter = new RegexFilter();
        }
        return commentFilter;
    }

    public void setCommentFilter(RegexFilter commentFilter) {
        this.commentFilter = commentFilter;
    }

    private boolean enabled;
    private String  name;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public boolean isEnabled() {
        return enabled && _isValid();
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }
}
