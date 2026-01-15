package org.jdownloader.captcha.v2;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * Filter class for querying CaptchaHistoryEntry objects with flexible criteria. Use this class to specify filter conditions when retrieving
 * history entries.
 *
 * All filter criteria are optional - only set the ones you want to filter by. Multiple criteria are combined with AND logic.
 */
public class CaptchaHistoryEntryFilter {
    private CAPTCHA_TYPE captchaType;
    private String       domain;
    private Boolean      isLoginCaptcha;
    private Long         minTimestamp;
    private Long         maxTimestamp;
    private Integer      limit;

    /**
     * Creates a new empty filter. By default, no filters are set and all entries will match.
     */
    public CaptchaHistoryEntryFilter() {
    }

    /**
     * Filters by CAPTCHA type.
     *
     * @param captchaType
     *            the CAPTCHA type to filter for, or null to disable this filter
     * @return this filter for method chaining
     */
    public CaptchaHistoryEntryFilter withCaptchaType(CAPTCHA_TYPE captchaType) {
        this.captchaType = captchaType;
        return this;
    }

    /**
     * Filters by domain.
     *
     * @param domain
     *            the domain to filter for, or null to disable this filter
     * @return this filter for method chaining
     */
    public CaptchaHistoryEntryFilter withDomain(String domain) {
        this.domain = domain;
        return this;
    }

    /**
     * Filters by login/non-login captcha status.
     *
     * @param isLoginCaptcha
     *            true to filter for login captchas only, false for non-login only, or null to disable this filter
     * @return this filter for method chaining
     */
    public CaptchaHistoryEntryFilter withLoginCaptcha(Boolean isLoginCaptcha) {
        this.isLoginCaptcha = isLoginCaptcha;
        return this;
    }

    /**
     * Filters by minimum timestamp (inclusive). Only entries with timestamp >= minTimestamp will be included.
     *
     * @param minTimestamp
     *            the minimum timestamp in milliseconds, or null to disable this filter
     * @return this filter for method chaining
     */
    public CaptchaHistoryEntryFilter withMinTimestamp(Long minTimestamp) {
        this.minTimestamp = minTimestamp;
        return this;
    }

    /**
     * Filters by maximum timestamp (inclusive). Only entries with timestamp <= maxTimestamp will be included.
     *
     * @param maxTimestamp
     *            the maximum timestamp in milliseconds, or null to disable this filter
     * @return this filter for method chaining
     */
    public CaptchaHistoryEntryFilter withMaxTimestamp(Long maxTimestamp) {
        this.maxTimestamp = maxTimestamp;
        return this;
    }

    /**
     * Filters by a time range (inclusive on both ends). Only entries with minTimestamp <= timestamp <= maxTimestamp will be included.
     *
     * @param minTimestamp
     *            the minimum timestamp in milliseconds
     * @param maxTimestamp
     *            the maximum timestamp in milliseconds
     * @return this filter for method chaining
     */
    public CaptchaHistoryEntryFilter withTimestampRange(long minTimestamp, long maxTimestamp) {
        this.minTimestamp = Long.valueOf(minTimestamp);
        this.maxTimestamp = Long.valueOf(maxTimestamp);
        return this;
    }

    /**
     * Limits the number of results returned. Limits are applied after filtering, typically returning the most recent entries.
     *
     * @param limit
     *            the maximum number of entries to return, or null for no limit
     * @return this filter for method chaining
     */
    public CaptchaHistoryEntryFilter withLimit(Integer limit) {
        this.limit = limit;
        return this;
    }

    /**
     * Returns the CAPTCHA type filter.
     *
     * @return the CAPTCHA type filter or null if not set
     */
    public CAPTCHA_TYPE getCaptchaType() {
        return captchaType;
    }

    /**
     * Returns the domain filter.
     *
     * @return the domain filter or null if not set
     */
    public String getDomain() {
        return domain;
    }

    /**
     * Returns the login captcha filter.
     *
     * @return true to filter for login captchas, false for non-login, or null if not set
     */
    public Boolean getIsLoginCaptcha() {
        return isLoginCaptcha;
    }

    /**
     * Returns the minimum timestamp filter.
     *
     * @return the minimum timestamp or null if not set
     */
    public Long getMinTimestamp() {
        return minTimestamp;
    }

    /**
     * Returns the maximum timestamp filter.
     *
     * @return the maximum timestamp or null if not set
     */
    public Long getMaxTimestamp() {
        return maxTimestamp;
    }

    /**
     * Returns the result limit.
     *
     * @return the limit or null if not set
     */
    public Integer getLimit() {
        return limit;
    }

    /**
     * Checks if a given entry matches all filter criteria. This is used internally by CaptchaHistoryManager for filtering.
     *
     * @param entry
     *            the entry to check
     * @return true if the entry matches all filter criteria, false otherwise
     */
    public boolean matches(CaptchaHistoryEntry entry) {
        if (entry == null) {
            return false;
        }
        // Check CAPTCHA type filter
        if (captchaType != null) {
            if (!captchaType.equals(entry.getCaptcha_type())) {
                return false;
            }
        }
        // Check domain filter
        if (domain != null) {
            if (!domain.equals(entry.getDomain())) {
                return false;
            }
        }
        // Check login captcha filter
        if (isLoginCaptcha != null) {
            if (isLoginCaptcha.booleanValue() != entry.isLoginCaptcha()) {
                return false;
            }
        }
        // Check minimum timestamp filter
        if (minTimestamp != null) {
            if (entry.getTimestamp() < minTimestamp.longValue()) {
                return false;
            }
        }
        // Check maximum timestamp filter
        if (maxTimestamp != null) {
            if (entry.getTimestamp() > maxTimestamp.longValue()) {
                return false;
            }
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("CaptchaHistoryEntryFilter [");
        boolean first = true;
        if (captchaType != null) {
            if (!first) {
                sb.append(", ");
            }
            sb.append("captchaType=").append(captchaType.name());
            first = false;
        }
        if (domain != null) {
            if (!first) {
                sb.append(", ");
            }
            sb.append("domain=").append(domain);
            first = false;
        }
        if (isLoginCaptcha != null) {
            if (!first) {
                sb.append(", ");
            }
            sb.append("isLoginCaptcha=").append(isLoginCaptcha);
            first = false;
        }
        if (minTimestamp != null) {
            if (!first) {
                sb.append(", ");
            }
            sb.append("minTimestamp=").append(minTimestamp);
            first = false;
        }
        if (maxTimestamp != null) {
            if (!first) {
                sb.append(", ");
            }
            sb.append("maxTimestamp=").append(maxTimestamp);
            first = false;
        }
        if (limit != null) {
            if (!first) {
                sb.append(", ");
            }
            sb.append("limit=").append(limit);
            first = false;
        }
        sb.append("]");
        return sb.toString();
    }
}