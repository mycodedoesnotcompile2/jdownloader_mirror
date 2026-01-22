package org.jdownloader.captcha.v2;

import java.util.HashSet;
import java.util.Set;

import org.appwork.storage.Storable;
import org.jdownloader.captcha.v2.Challenge.CaptchaRequestType;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public class CaptchaChallengeFilter implements Storable {
    public enum CaptchaFilterType {
        BLACKLIST,
        WHITELIST
    }

    private String                  domain              = null;
    private Set<CAPTCHA_TYPE>       captchaTypes        = null;
    private boolean                 regex               = false;
    private boolean                 enabled             = true;
    private Set<CaptchaRequestType> captchaRequestTypes = null;
    private CaptchaFilterType       filterType          = CaptchaFilterType.BLACKLIST;
    private boolean                 broken              = false;
    private String                  id                  = null;
    private long                    created             = System.currentTimeMillis();
    private int                     position            = 0;

    public CaptchaChallengeFilter() {
        // __Storable__ constructor
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

    /* Returns domain or domain-regex. */
    public String getDomain() {
        return domain;
    }

    public void setDomain(String domain) {
        this.domain = domain;
    }

    public Set<CAPTCHA_TYPE> getCaptchaTypes() {
        if (captchaTypes == null) {
            captchaTypes = new HashSet<CAPTCHA_TYPE>();
        }
        return captchaTypes;
    }

    public void setCaptchaTypes(Set<CAPTCHA_TYPE> captchaTypes) {
        this.captchaTypes = captchaTypes;
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

    public Set<CaptchaRequestType> getCaptchaRequestTypes() {
        if (captchaRequestTypes == null) {
            captchaRequestTypes = new HashSet<CaptchaRequestType>();
        }
        return captchaRequestTypes;
    }

    public void setCaptchaRequestTypes(Set<CaptchaRequestType> captchaRequestTypes) {
        this.captchaRequestTypes = captchaRequestTypes;
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
        sb.append("type=").append(filterType);
        sb.append(", domain=").append(domain);
        sb.append(", regex=").append(regex);
        sb.append(", enabled=").append(enabled);
        sb.append(", position=").append(position);
        if (captchaTypes != null && !captchaTypes.isEmpty()) {
            sb.append(", captchaTypes=").append(captchaTypes);
        }
        if (captchaRequestTypes != null && !captchaRequestTypes.isEmpty()) {
            sb.append(", requestTypes=").append(captchaRequestTypes);
        }
        if (broken) {
            sb.append(", BROKEN");
        }
        sb.append("]");
        return sb.toString();
    }
}