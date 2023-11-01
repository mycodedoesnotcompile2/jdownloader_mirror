package org.appwork.remoteapi;

import java.util.HashSet;

import org.appwork.utils.StringUtils;

public class ContentSecurityHeader {
    /**
     *
     */
    public ContentSecurityHeader() {
        // TODO Auto-generated constructor stub
    }

    private HashSet<String> defaultSrc = new HashSet<String>();

    public void addDefaultSrc(String string) {
        this.defaultSrc.add(string);
    }

    private HashSet<String> scriptSrc = new HashSet<String>();

    public void addScriptSrc(String string) {
        this.scriptSrc.add(string);
    }

    private HashSet<String> fontSrc = new HashSet<String>();

    public void addFontSrc(String string) {
        this.fontSrc.add(string);
    }

    private HashSet<String> imgSrc = new HashSet<String>();

    public void addImgSrc(String string) {
        this.imgSrc.add(string);
    }

    public String toHeaderString() {
        StringBuilder sb = new StringBuilder();
        this.append(sb, "default-src", this.defaultSrc);
        this.append(sb, "style-src", this.styleSrc);
        this.append(sb, "img-src", this.imgSrc);
        this.append(sb, "script-src", this.scriptSrc);
        this.append(sb, "font-src", this.fontSrc);
        if (StringUtils.isNotEmpty(this.reportURI)) {
            if (sb.length() > 0) {
                sb.append("; ");
            }
            sb.append("report-uri ").append(this.reportURI);
        }
        return sb.toString();
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

    private HashSet<String> styleSrc = new HashSet<String>();
    private String          reportURI;

    public void addStyleSrc(String string) {
        this.styleSrc.add(string);
    }

    public void setReportURI(String string) {
        this.reportURI = string;
    }
}