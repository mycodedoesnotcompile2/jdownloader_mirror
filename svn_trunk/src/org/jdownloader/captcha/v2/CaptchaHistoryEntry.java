package org.jdownloader.captcha.v2;

import org.appwork.storage.Storable;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * Stores information about solved CAPTCHAs for historical purposes. This class implements the Storable interface and can be persisted in
 * JsonStorage.
 */
public class CaptchaHistoryEntry implements Storable {
    private long         timestamp;
    private String       domain;
    private CAPTCHA_TYPE captcha_type;

    /**
     * Default constructor (required for Storable) Sets timestamp to the current system time in milliseconds.
     */
    public CaptchaHistoryEntry() {
        /* Storable */
        this.timestamp = System.currentTimeMillis();
        this.domain = null;
        this.captcha_type = null;
    }

    /**
     * Constructor with parameters Automatically sets timestamp to the current system time in milliseconds.
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
    }

    /**
     * @return the timestamp
     */
    public long getTimestamp() {
        return timestamp;
    }

    /**
     * @param timestamp
     *            the timestamp to set
     */
    public void setTimestamp(long timestamp) {
        this.timestamp = timestamp;
    }

    /**
     * @return the domain
     */
    public String getDomain() {
        return domain;
    }

    /**
     * @param domain
     *            the domain to set
     */
    public void setDomain(String domain) {
        this.domain = domain;
    }

    /**
     * @return the captcha_type
     */
    public CAPTCHA_TYPE getCaptcha_type() {
        return captcha_type;
    }

    /**
     * @param captcha_type
     *            the captcha_type to set
     */
    public void setCaptcha_type(CAPTCHA_TYPE captcha_type) {
        this.captcha_type = captcha_type;
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
        sb.append("]");
        return sb.toString();
    }
}