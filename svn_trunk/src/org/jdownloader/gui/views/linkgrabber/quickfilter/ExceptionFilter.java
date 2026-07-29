package org.jdownloader.gui.views.linkgrabber.quickfilter;

import org.appwork.utils.Hash;
import org.jdownloader.controlling.filter.LinkgrabberFilterRuleWrapper;
import org.jdownloader.images.NewTheme;

import jd.controlling.linkcrawler.CrawledLink;

public class ExceptionFilter extends Filter {

    private final String                       description;
    private final LinkgrabberFilterRuleWrapper wrapperRule;
    private final String                       id;

    public ExceptionFilter(LinkgrabberFilterRuleWrapper rule) {
        super(rule.getName());
        this.wrapperRule = rule;
        if (rule.getRule().getIconKey() != null) {
            setIcon(NewTheme.I().getIcon(rule.getRule().getIconKey(), 16));
        }
        id = "Custom_" + Hash.getMD5(rule.getName() + ":" + getDescription());
        enabled = Boolean.TRUE.equals(CONFIG.get(getID(), true));
        description = rule.getRule().toString();
    }

    @Override
    public String getDescription() {
        return description;
    }

    public LinkgrabberFilterRuleWrapper getWrapperRule() {
        return wrapperRule;
    }

    @Override
    protected String getID() {
        return id;
    }

    @Override
    public boolean isFiltered(CrawledLink link) {
        // Checks are ordered cheapest first (boolean/enum), most expensive (regex) last, so a non-matching rule
        // fails fast on the cheapest condition. Result is order independent (logical AND of all checks).
        if (!wrapperRule.checkOnlineStatus(link)) {
            return false;
        }
        if (!wrapperRule.checkPluginStatus(link)) {
            return false;
        }
        if (!wrapperRule.checkLinkEnabled(link)) {
            return false;
        }
        if (!wrapperRule.checkOrigin(link)) {
            return false;
        }
        if (!wrapperRule.checkDownloadListDupe(link)) {
            return false;
        }
        if (!wrapperRule.checkFileSize(link)) {
            return false;
        }
        if (!wrapperRule.checkFileType(link)) {
            return false;
        }
        if (!wrapperRule.checkHoster(link)) {
            return false;
        }
        if (!wrapperRule.checkSource(link)) {
            return false;
        }
        if (!wrapperRule.checkPackageName(link)) {
            return false;
        }
        if (!wrapperRule.checkFileName(link)) {
            return false;
        }
        if (!wrapperRule.checkComment(link)) {
            return false;
        }
        return true;
    }

}
