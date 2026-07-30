package org.jdownloader.controlling.filter;

import org.appwork.storage.Storable;
import org.jdownloader.translate._JDT;

public class LinkgrabberFilterRule extends FilterRule implements Storable {

    public LinkgrabberFilterRule() {
        // required by Storable
    }

    private boolean accept;

    public void setAccept(boolean b) {
        accept = b;
    }

    public boolean isAccept() {
        return accept;
    }

    private boolean negated;

    /**
     * If true, this rule is inverted when used as a view (quick filter) in the linkgrabber sidebar: an active view then hides all
     * links that do NOT match the conditions, i.e. only matching links stay visible. Only evaluated in the view path
     * (org.jdownloader.gui.views.linkgrabber.quickfilter.ExceptionFilter), NOT in the add-links drop logic
     * (LinkFilterController).
     */
    public void setNegated(boolean b) {
        negated = b;
    }

    public boolean isNegated() {
        return negated;
    }

    public LinkgrabberFilterRuleWrapper compile() {
        LinkgrabberFilterRuleWrapper ret = new LinkgrabberFilterRuleWrapper(this);
        return ret;
    }

    public LinkgrabberFilterRule duplicate() {
        LinkgrabberFilterRule ret = new LinkgrabberFilterRule();
        ret.accept = accept;
        ret.negated = negated;
        ret.setEnabled(isEnabled());
        ret.setIconKey(getIconKey());
        ret.setFilenameFilter(getFilenameFilter());
        ret.setPackagenameFilter(getPackagenameFilter());
        ret.setFilesizeFilter(getFilesizeFilter());
        ret.setMatchAlwaysFilter(getMatchAlwaysFilter());
        ret.setFiletypeFilter(getFiletypeFilter());
        ret.setOnlineStatusFilter(getOnlineStatusFilter());
        ret.setOriginFilter(getOriginFilter());
        ret.setLinkEnabledFilter(getLinkEnabledFilter());
        ret.setDownloadListDupeFilter(getDownloadListDupeFilter());
        ret.setPluginStatusFilter(getPluginStatusFilter());
        ret.setHosterURLFilter(getHosterURLFilter());
        ret.setName(_JDT.T.LinkgrabberFilterRule_duplicate(getName()));
        ret.setSourceURLFilter(getSourceURLFilter());

        return ret;
    }

}
