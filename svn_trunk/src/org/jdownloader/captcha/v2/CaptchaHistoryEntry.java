package org.jdownloader.captcha.v2;

import org.appwork.storage.Storable;

import org.jdownloader.captcha.v2.Challenge.CaptchaRequestType;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * Stores information about solved CAPTCHAs for historical purposes. This class implements the Storable interface and can be persisted in
 * JsonStorage.
 */
public class CaptchaHistoryEntry implements Storable {
    private long         timestamp;
    private String       domain;
    private CAPTCHA_TYPE captcha_type;
    private CaptchaRequestType captchaRequestType;

    /**
     * Default constructor (required for Storable). Sets timestamp to the current system time in milliseconds. The request type is not set.
     */
    public CaptchaHistoryEntry() {
        this.timestamp = System.currentTimeMillis();
        this.domain = null;
        this.captcha_type = null;
        this.captchaRequestType = null;
    }

    /**
     * Constructor with parameters. Automatically sets timestamp to the current system time in milliseconds. The request type is not set.
     *
     * @param domain
     *            the domain
     * @param captcha_type
     *            the CAPTCHA type
     */
    public CaptchaHistoryEntry(String domain, CAPTCHA_TYPE captcha_type) {
        this.timestamp = System.currentTimeMillis();
        this.domain = domain;
        this.captcha_type = captcha_type;
        this.captchaRequestType = null;
    }

    public CaptchaHistoryEntry(Challenge<?> challenge) {
        if (challenge == null) {
            throw new IllegalArgumentException("challenge cannot be null");
        }
        this.timestamp = System.currentTimeMillis();
        this.domain = challenge.getHost();
        this.captcha_type = CAPTCHA_TYPE.getCaptchaTypeForChallenge(challenge);
        this.captchaRequestType = challenge.getCaptchaRequestType();
    }

    /**
     * Constructor with all parameters. Automatically sets timestamp to the current system time in milliseconds.
     *
     * @param domain
     *            the domain
     * @param captcha_type
     *            the CAPTCHA type
     * @param captchaRequestType
     *            the source of the captcha (download, account login, crawler, ...), can be null
     */
    public CaptchaHistoryEntry(String domain, CAPTCHA_TYPE captcha_type, CaptchaRequestType captchaRequestType) {
        this.timestamp = System.currentTimeMillis();
        this.domain = domain;
        this.captcha_type = captcha_type;
        this.captchaRequestType = captchaRequestType;
    }

    /**
     * Returns the timestamp
     *
     * @return the timestamp in milliseconds
     */
    public long getTimestamp() {
        return timestamp;
    }

    /**
     * Sets the timestamp
     *
     * @param timestamp
     *            the timestamp to set
     */
    public void setTimestamp(long timestamp) {
        this.timestamp = timestamp;
    }

    /**
     * Returns the domain
     *
     * @return the domain
     */
    public String getDomain() {
        return domain;
    }

    /**
     * Sets the domain
     *
     * @param domain
     *            the domain to set
     */
    public void setDomain(String domain) {
        this.domain = domain;
    }

    /**
     * Returns the CAPTCHA type
     *
     * @return the captcha_type
     */
    public CAPTCHA_TYPE getCaptcha_type() {
        return captcha_type;
    }

    /**
     * Sets the CAPTCHA type
     *
     * @param captcha_type
     *            the captcha_type to set
     */
    public void setCaptcha_type(CAPTCHA_TYPE captcha_type) {
        this.captcha_type = captcha_type;
    }

    /**
     * Returns the source of the captcha (download, account login, crawler, ...)
     *
     * @return the request type, null if unknown (e.g. entries of older versions)
     */
    public CaptchaRequestType getCaptchaRequestType() {
        return captchaRequestType;
    }

    public void setCaptchaRequestType(CaptchaRequestType captchaRequestType) {
        this.captchaRequestType = captchaRequestType;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("CaptchaHistoryEntry [timestamp=");
        sb.append(timestamp);
        sb.append(", domain=");
        sb.append(domain);
        sb.append(", captcha_type=");
        sb.append(captcha_type);
        sb.append(", captchaRequestType=");
        sb.append(captchaRequestType);
        sb.append("]");
        return sb.toString();
    }
}