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
        enabled = Boolean.TRUE.equals(CONFIG.get(getID(), isDefaultEnabled()));
        description = rule.getRule().toString();
    }

    /**
     * Whether the checkbox of this view is checked by default (no stored state yet).
     *
     * Normal (non-negated) views default to checked/ON, because for them "checked" means "do not hide anything". Negated
     * (inverted) views default to unchecked/OFF, so that a freshly created inverted view does not immediately hide all
     * non-matching links; the user has to activate it explicitly.
     */
    private boolean isDefaultEnabled() {
        return !wrapperRule.getRule().isNegated();
    }

    /**
     * Whether this view currently filters the linkgrabber table (i.e. belongs to the actively hiding set).
     *
     * The checkbox semantics differ by rule type: a normal (non-negated) view is active when it is UNCHECKED (checked =
     * "show all"), whereas a negated view is active when it is CHECKED (checked = "show only matching"). This keeps the
     * intuitive "checked = on" behaviour for the inverted views the user asked for, without changing the normal views.
     */
    public boolean isViewActive() {
        if (wrapperRule.getRule().isNegated()) {
            return isEnabled();
        }
        return !isEnabled();
    }

    @Override
    public void setEnabled(boolean enabled) {
        if (this.enabled == enabled) {
            return;
        }
        this.enabled = enabled;
        // only persist deviations from the (rule-type dependent) default state
        if (enabled == isDefaultEnabled()) {
            CONFIG.remove(getID());
        } else {
            CONFIG.put(getID(), enabled);
        }
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
        final boolean matches = matches(link);
        if (wrapperRule.getRule().isNegated()) {
            // inverted view: hide everything that does NOT match, i.e. keep only matching links visible
            return !matches;
        }
        return matches;
    }

    private boolean matches(CrawledLink link) {
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
