package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import org.jdownloader.captcha.v2.Challenge.CaptchaRequestType;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * One row of the domain table of the {@link CaptchaWizardDialog}: a domain together with the captcha type the user needs to solve there.
 * Identity is (domain, captcha type); the enabled state and the request type are no part of it.
 */
public class CaptchaWizardEntry {
    private static final String SEPARATOR = "|";
    private final String        domain;
    private final CAPTCHA_TYPE  captchaType;
    /* Only information, null for entries which were not taken from the captcha history. */
    private final CaptchaRequestType requestType;
    private boolean             enabled;

    public CaptchaWizardEntry(final String domain, final CAPTCHA_TYPE captchaType, final CaptchaRequestType requestType, final boolean enabled) {
        this.domain = domain;
        this.captchaType = captchaType;
        this.requestType = requestType;
        this.enabled = enabled;
    }

    public String getDomain() {
        return domain;
    }

    public CAPTCHA_TYPE getCaptchaType() {
        return captchaType;
    }

    public CaptchaRequestType getRequestType() {
        return requestType;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /** A key which identifies the entry (domain and captcha type), independent of the upper/lower case of the domain. */
    public String getKey() {
        return domain.toLowerCase() + SEPARATOR + captchaType.name();
    }

    @Override
    public int hashCode() {
        return domain.toLowerCase().hashCode() * 31 + captchaType.hashCode();
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof CaptchaWizardEntry)) {
            return false;
        }
        final CaptchaWizardEntry other = (CaptchaWizardEntry) obj;
        return captchaType == other.captchaType && domain.equalsIgnoreCase(other.domain);
    }
}
