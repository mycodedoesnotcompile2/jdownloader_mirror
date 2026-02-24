package org.appwork.remoteapi;

import java.util.HashSet;

import org.appwork.utils.StringUtils;

public class ContentSecurityHeader {
    public static final String NONE = "'none'";

    private HashSet<String> connectSrc = new HashSet<String>();

    private HashSet<String> defaultSrc = new HashSet<String>();

    private HashSet<String> fontSrc = new HashSet<String>();

    private HashSet<String> imgSrc = new HashSet<String>();

    private String          reportURI;

    private HashSet<String> scriptSrc = new HashSet<String>();

    private HashSet<String> styleSrc = new HashSet<String>();

    /**
     * Creates a CSP header builder with a maximal restrictive default policy.
     *
     * <p>
     * Default is {@code default-src 'none'} which blocks all resource loads and connection targets unless explicitly allowed by additional
     * directives.
     * </p>
     */
    public ContentSecurityHeader() {
        this.defaultSrc.add(NONE);
    }

    /**
     * Adds a value to the CSP {@code connect-src} directive.
     *
     * <p>
     * {@code connect-src} controls the allowed targets for script-driven connections such as Fetch/XHR/WebSocket/EventSource/beacon.
     * </p>
     *
     * <p>
     * Specification:
     * <a href="https://www.w3.org/TR/CSP3/#directive-connect-src">W3C CSP Level 3 - connect-src</a>
     * </p>
     *
     * <p>
     * Developer reference:
     * <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/connect-src">MDN - CSP connect-src</a>
     * </p>
     *
     * @param string
     *            Source expression like {@code 'self'}, {@code https://api.example.com}, or {@code wss://example.com}
     */
    public void addConnectSrc(String string) {
        if (StringUtils.isEmpty(string)) {
            return;
        }
        this.connectSrc.add(string);
    }

    public void addDefaultSrc(String string) {
        if (StringUtils.isEmpty(string)) {
            return;
        }
        if (!NONE.equals(string)) {
            this.defaultSrc.remove(NONE);
        }
        this.defaultSrc.add(string);
    }

    public void addFontSrc(String string) {
        if (StringUtils.isEmpty(string)) {
            return;
        }
        this.fontSrc.add(string);
    }

    public void addImgSrc(String string) {
        if (StringUtils.isEmpty(string)) {
            return;
        }
        this.imgSrc.add(string);
    }

    public void addScriptSrc(String string) {
        if (StringUtils.isEmpty(string)) {
            return;
        }
        this.scriptSrc.add(string);
    }

    public void addStyleSrc(String string) {
        if (StringUtils.isEmpty(string)) {
            return;
        }
        this.styleSrc.add(string);
    }
    private void append(StringBuilder sb, String key, HashSet<String> set) {
        if (set.size() == 0) {
            return;
        }
        if (sb.length() > 0) {
            sb.append("; ");
        }
        sb.append(key);
        for (String s : set) {
            sb.append(" ").append(s);
        }
    }

    public void setReportURI(String string) {
        this.reportURI = string;
    }

    public String toHeaderString() {
        StringBuilder sb = new StringBuilder();
        this.append(sb, "default-src", this.defaultSrc);
        this.append(sb, "style-src", this.styleSrc);
        this.append(sb, "img-src", this.imgSrc);
        this.append(sb, "script-src", this.scriptSrc);
        this.append(sb, "font-src", this.fontSrc);
        this.append(sb, "connect-src", this.connectSrc);
        if (StringUtils.isNotEmpty(this.reportURI)) {
            if (sb.length() > 0) {
                sb.append("; ");
            }
            sb.append("report-uri ").append(this.reportURI);
        }
        return sb.toString();
    }
}
