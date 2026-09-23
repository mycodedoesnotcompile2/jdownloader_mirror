package org.jdownloader.captcha.v2;

import java.util.HashSet;
import java.util.Set;

import org.appwork.storage.Storable;
import org.jdownloader.captcha.v2.Challenge.CaptchaRequestType;
import org.jdownloader.gui.translate._GUI;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public class CaptchaChallengeFilter implements Storable {
    public enum CaptchaFilterType {
        BLACKLIST,
        WHITELIST
    }

    private String                  name                = null;
    private String                  domain              = null;
    /**
     * Captcha types excluded from this rule. Empty/null means "no exclusions", i.e. the rule applies to all captcha types, including
     * ones added in the future - new {@link CAPTCHA_TYPE} constants are active by default instead of requiring existing rules to be
     * migrated.
     */
    private Set<CAPTCHA_TYPE>       excludedCaptchaTypes = null;
    private boolean                 regex               = false;
    private boolean                 enabled             = true;
    /**
     * Request types excluded from this rule. Empty/null means "no exclusions", i.e. the rule applies to all request types, including
     * ones added in the future - new {@link CaptchaRequestType} constants are active by default instead of requiring existing rules to
     * be migrated.
     */
    private Set<CaptchaRequestType> excludedCaptchaRequestTypes = null;
    private CaptchaFilterType       filterType          = CaptchaFilterType.BLACKLIST;
    private boolean                 broken              = false;
    private String                  id                  = null;
    private long                    created             = System.currentTimeMillis();
    private int                     position            = 0;
    /**
     * Identifier of the captcha solver service this rule applies to (e.g. the solver host "2captcha.com"). An empty/null value means the
     * rule applies to all solvers.
     */
    private String                  solver              = null;
    /** True for built-in default rules that must not be edited or removed (e.g. the example rule shown in the settings table). */
    private boolean                 staticRule          = false;

    public CaptchaChallengeFilter() {
        // __Storable__ constructor
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public boolean _isBroken() {
        return broken;
    }

    public void _setBroken(boolean broken) {
        this.broken = broken;
    }

    public String _getId() {
        return id;
    }

    public void _setId(String id) {
        this.id = id;
    }

    public long getCreated() {
        return created;
    }

    public void setCreated(long created) {
        this.created = created;
    }

    public int getPosition() {
        return position;
    }

    public void setPosition(int position) {
        this.position = position;
    }

    public String getSolver() {
        return solver;
    }

    public void setSolver(String solver) {
        this.solver = solver;
    }

    public boolean isStaticRule() {
        return staticRule;
    }

    public void setStaticRule(boolean staticRule) {
        this.staticRule = staticRule;
    }

    public String getDomain() {
        return domain;
    }

    public void setDomain(String domain) {
        this.domain = domain;
    }

    public Set<CAPTCHA_TYPE> getExcludedCaptchaTypes() {
        if (excludedCaptchaTypes == null) {
            excludedCaptchaTypes = new HashSet<CAPTCHA_TYPE>();
        }
        return excludedCaptchaTypes;
    }

    public void setExcludedCaptchaTypes(Set<CAPTCHA_TYPE> excludedCaptchaTypes) {
        this.excludedCaptchaTypes = excludedCaptchaTypes;
    }

    public boolean isRegex() {
        return regex;
    }

    public void setRegex(boolean regex) {
        this.regex = regex;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public Set<CaptchaRequestType> getExcludedCaptchaRequestTypes() {
        if (excludedCaptchaRequestTypes == null) {
            excludedCaptchaRequestTypes = new HashSet<CaptchaRequestType>();
        }
        return excludedCaptchaRequestTypes;
    }

    public void setExcludedCaptchaRequestTypes(Set<CaptchaRequestType> excludedCaptchaRequestTypes) {
        this.excludedCaptchaRequestTypes = excludedCaptchaRequestTypes;
    }

    public CaptchaFilterType getFilterType() {
        if (filterType == null) {
            return CaptchaFilterType.BLACKLIST;
        }
        return filterType;
    }

    public void setFilterType(CaptchaFilterType filterType) {
        this.filterType = filterType;
    }

    /** Returns a copy of this rule (new id, own name suffix), for the "Duplicate" table action. Never called on a static rule. */
    public CaptchaChallengeFilter duplicate() {
        final CaptchaChallengeFilter ret = new CaptchaChallengeFilter();
        ret.setEnabled(isEnabled());
        ret.setName(_GUI.T.CaptchaRules_duplicate_name(getName()));
        ret.setDomain(getDomain());
        ret.setRegex(isRegex());
        ret.setFilterType(getFilterType());
        ret.setSolver(getSolver());
        ret.setExcludedCaptchaTypes(new HashSet<CAPTCHA_TYPE>(getExcludedCaptchaTypes()));
        ret.setExcludedCaptchaRequestTypes(new HashSet<CaptchaRequestType>(getExcludedCaptchaRequestTypes()));
        return ret;
    }

    /**
     * Checks if this filter is valid (has meaningful configuration)
     */
    public boolean _isValid() {
        if (broken) {
            return false;
        }
        if (!enabled) {
            return false;
        }
        if (domain == null || domain.trim().length() == 0) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("CaptchaChallengeFilter[");
        if (id != null) {
            sb.append("id=").append(id).append(", ");
        }
        if (name != null) {
            sb.append("name=").append(name).append(", ");
        }
        sb.append("type=").append(filterType);
        sb.append(", domain=").append(domain);
        sb.append(", regex=").append(regex);
        sb.append(", enabled=").append(enabled);
        sb.append(", position=").append(position);
        if (excludedCaptchaTypes != null && !excludedCaptchaTypes.isEmpty()) {
            sb.append(", excludedCaptchaTypes=").append(excludedCaptchaTypes);
        }
        if (excludedCaptchaRequestTypes != null && !excludedCaptchaRequestTypes.isEmpty()) {
            sb.append(", excludedRequestTypes=").append(excludedCaptchaRequestTypes);
        }
        if (broken) {
            sb.append(", BROKEN");
        }
        sb.append("]");
        return sb.toString();
    }
}