//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.http;

import java.net.IDN;
import java.net.URL;
import java.util.Date;
import java.util.Locale;
import java.util.regex.Pattern;

import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.extmanager.LoggerFactory;

public class Cookie {
    public static class ServerCookie extends Cookie {
        public ServerCookie() {
        }

        @Override
        public SOURCE getSource() {
            return SOURCE.SERVER;
        }

        @Override
        public ServerCookie newCookieInstance() {
            return new ServerCookie();
        }

        @Override
        public boolean isCustomCookie() {
            return false;
        }

        @Override
        public boolean isValid() {
            if (!super.isValid()) {
                return false;
            } else if (Boolean.TRUE.equals(isSecure()) && !isSecureProtocol()) {
                return false;
            } else {
                return true;
            }
        }
    }

    public static class ImportCookie extends Cookie {
        public ImportCookie() {
        }

        @Override
        public SOURCE getSource() {
            return SOURCE.IMPORT;
        }

        @Override
        public ImportCookie newCookieInstance() {
            return new ImportCookie();
        }

        @Override
        public boolean isCustomCookie() {
            return false;
        }
    }

    public static class ManualCookie extends Cookie {
        public ManualCookie() {
        }

        @Override
        public SOURCE getSource() {
            return SOURCE.MANUAL;
        }

        @Override
        public ManualCookie newCookieInstance() {
            return new ManualCookie();
        }

        @Override
        public boolean isCustomCookie() {
            return true;
        }
    }

    public static class LegacyCookie extends Cookie {
        public LegacyCookie() {
        }

        @Override
        public SOURCE getSource() {
            return SOURCE.LEGACY;
        }

        @Override
        public LegacyCookie newCookieInstance() {
            return new LegacyCookie();
        }
    }

    public static enum SOURCE {
        SERVER {
            @Override
            public ServerCookie newCookieInstance() {
                return new ServerCookie();
            }
        },
        IMPORT {
            @Override
            public ImportCookie newCookieInstance() {
                return new ImportCookie();
            }
        },
        MANUAL {
            @Override
            public ManualCookie newCookieInstance() {
                return new ManualCookie();
            }
        },
        LEGACY {
            @Override
            public LegacyCookie newCookieInstance() {
                return new LegacyCookie();
            }
        };
        public abstract Cookie newCookieInstance();
    }

    public final static String UNSECURE_HTTP = "http";
    public final static String SECURE_HTTPS  = "https";

    public SOURCE getSource() {
        return SOURCE.MANUAL;
    }

    private String protocol = null;

    public String getProtocol() {
        return protocol;
    }

    public Cookie newCookieInstance() {
        return new Cookie();
    }

    public boolean isValid() {
        return getKey() != null;
    }

    public boolean isSecureProtocol() {
        final String protocol = getProtocol();
        if (protocol == null) {
            // no protocol available but secure flag is set
            return Boolean.TRUE.equals(isSecure());
        }
        return SECURE_HTTPS.equalsIgnoreCase(protocol);
    }

    public void setProtocol(String protocol) {
        this.protocol = protocol;
    }

    public static <T extends Cookie> T copyOf(T cookie) {
        final T ret = (T) cookie.newCookieInstance();
        ret.update(cookie);
        ret.setProtocol(cookie.getProtocol());
        ret.setPath(cookie.getPath());
        ret.setKey(cookie.getKey());
        ret.setHost(cookie.getHost());
        return ret;
    }

    private String  path     = null;
    private String  host     = null;
    private String  value    = null;
    private String  key      = null;
    private Boolean secure   = null;
    private Boolean hostOnly = null;
    private String  sameSite = null;

    public String getSetSameSite() {
        return sameSite;
    }

    public String getSameSite() {
        final String ret = getSetSameSite();
        if (ret != null) {
            return ret;
        }
        return "Lax";// default
    }

    public void setSameSite(String sameSite) {
        if ("Lax".equalsIgnoreCase(sameSite)) {
            this.sameSite = "Lax";
        } else if ("Strict".equalsIgnoreCase(sameSite)) {
            this.sameSite = "Strict";
        } else if ("None".equalsIgnoreCase(sameSite)) {
            this.sameSite = "None";
        } else {
            this.sameSite = null;
        }
    }

    public Boolean isHostOnly() {
        return hostOnly;
    }

    public void setHostOnly(Boolean hostOnly) {
        this.hostOnly = hostOnly;
    }

    public Boolean isSecure() {
        return this.secure;
    }

    public void setSecure(Boolean secure) {
        this.secure = secure;
    }

    private String domain       = null;
    private long   hostTime     = -1;
    private long   creationTime = System.currentTimeMillis();
    private long   expireTime   = -1;

    private Cookie() {
    }

    @Deprecated
    public Cookie(final String host, final String key, final String value) {
        this.setHost(host);
        this.setKey(key);
        this.setValue(value);
    }

    public boolean isCustomCookie() {
        if (SOURCE.MANUAL.equals(getSource())) {
            return true;
        }
        if (getDomain() == null) {
            return true;
        }
        return false;
    }

    @Override
    public boolean equals(final Object obj) {
        if (obj == null) {
            return false;
        }
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof Cookie)) {
            return false;
        }
        final Cookie other = (Cookie) obj;
        host: {
            final String thisHost = getHost();
            final String otherHost = other.getHost();
            if (thisHost == null || otherHost == null) {
                // entries that don't have host set
                break host;
            }
            if (StringUtils.equalsIgnoreCase(thisHost, otherHost)) {
                break host;
            }
            return false;
        }
        key: {
            if (StringUtils.equals(this.getKey(), other.getKey())) {
                break key;
            }
            return false;
        }
        path: {
            if (StringUtils.equals(this.getPath(), other.getPath())) {
                break path;
            }
            return false;
        }
        hostOnly: {
            final Boolean thisHostOnly = isHostOnly();
            final Boolean otherHostOnly = other.isHostOnly();
            if (thisHostOnly == null || otherHostOnly == null) {
                // old entries without this information
                break hostOnly;
            }
            if (thisHostOnly.equals(otherHostOnly)) {
                break hostOnly;
            }
            return false;
        }
        domain: {
            final String thisDomain = getDomain();
            final String otherDomain = other.getDomain();
            if (StringUtils.equals(thisDomain, otherDomain)) {
                // same domain
                break domain;
            }
            if (StringUtils.isAllNotEmpty(thisDomain, otherDomain) && !StringUtils.equalsIgnoreCase(thisDomain, otherDomain)) {
                return false;
            }
        }
        return true;
    }

    public boolean matches(final URL url) {
        if (Boolean.TRUE.equals(isSecure()) && !Cookie.SECURE_HTTPS.equalsIgnoreCase(url.getProtocol())) {
            return false;
        }
        final String domain = getDomain();
        if (Boolean.TRUE.equals(isHostOnly()) && !StringUtils.equalsIgnoreCase(url.getHost(), domain)) {
            return false;
        }
        if (domain != null && !StringUtils.endsWithCaseInsensitive("." + url.getHost(), "." + domain)) {
            return false;
        }
        if (!isCookiePathMatch(url)) {
            return false;
        }
        return true;
    }

    public boolean isCookiePathMatch(URL url) {
        String cookiePath = getPath();
        if (cookiePath == null || cookiePath.isEmpty()) {
            cookiePath = "/";
        }
        String requestPath = url.getPath();
        if (requestPath == null || requestPath.isEmpty()) {
            requestPath = "/";
        }
        if (requestPath.equals(cookiePath)) {
            return true;
        }
        if (requestPath.startsWith(cookiePath)) {
            if (cookiePath.endsWith("/")) {
                return true;
            }
            if (requestPath.charAt(cookiePath.length()) == '/') {
                return true;
            }
        }
        return false;
    }

    public long getCreationTime() {
        return this.creationTime;
    }

    public String getDomain() {
        return this.domain;
    }

    public long getExpireDate() {
        return this.expireTime;
    }

    public String getHost() {
        return this.host;
    }

    public long getHostTime() {
        return this.hostTime;
    }

    public String getKey() {
        return this.key;
    }

    public String getPath() {
        final String ret = this.path;
        if (StringUtils.isEmpty(ret)) {
            return "/";
        }
        return ret;
    }

    public String getValue() {
        return this.value;
    }

    @Override
    public int hashCode() {
        return StringUtils.valueOrEmpty(this.getKey()).toLowerCase(Locale.ENGLISH).hashCode();
    }

    public boolean isExpired() {
        if (this.expireTime == -1) {
            // System.out.println("isexpired: no expireDate found! " + this.host + " " + this.key);
            return false;
        }
        if (this.hostTime == -1) {
            return false;
        } else {
            final long timediff = this.creationTime - this.hostTime;
            final long check = System.currentTimeMillis() - timediff;
            final boolean expired = check > this.expireTime;
            return expired;
            // System.out.println(this.host + " " + this.key + " " + this.creationTime + " " + this.hostTime + " " + this.expireTime + " " +
            // check);
            // if (check > this.expireTime) {
            // System.out.println("Expired: " + this.host + " " + this.key);
            // return true;
            // } else {
            // return false;
            // }
        }
    }

    public void setCreationTime(final long time) {
        this.creationTime = time;
    }

    public boolean setDomain(String domain) {
        if (domain != null) {
            domain = cleanAttributeValue(domain);
            /** RFC 6265, remove leading dot **/
            domain = domain.replaceFirst("^\\.+", "");
            /** normalize domain */
            domain = IDN.toASCII(domain).toLowerCase(Locale.ROOT);
        }
        if (StringUtils.isEmpty(domain)) {
            this.domain = null;
            return false;
        }
        this.domain = domain;
        return true;
    }

    public void setExpireDate(final long time) {
        this.expireTime = time;
    }

    public void setExpires(final String expires) {
        if (expires == null) {
            this.expireTime = -1;
            return;
        }
        final Date expireDate = TimeFormatter.parseDateString(expires);
        if (expireDate != null) {
            this.expireTime = expireDate.getTime();
            return;
        }
        this.expireTime = -1;
        LoggerFactory.getDefaultLogger().severe("Cookie: no Format for " + expires + " found!");
        return;
    }

    public void setHost(final String host) {
        this.host = Browser.getHost(host, false);
    }

    public void setHostTime(final long time) {
        this.hostTime = time;
    }

    public boolean updateHost(String newHost) {
        final String oldHost = getHost();
        if (StringUtils.equalsIgnoreCase(oldHost, newHost)) {
            if (StringUtils.isNotEmpty(oldHost)) {
                /* oldHost equals newHost */
                return true;
            }
            /* host is null */
            return false;
        }
        newHost = newHost.toLowerCase(Locale.ROOT);
        String domain = getDomain();
        if (domain == null) {
            setHost(newHost);
            return true;
        } else if (StringUtils.endsWithCaseInsensitive(domain, "." + oldHost)) {
            domain = domain.replaceFirst("(?i)\\." + Pattern.quote(oldHost) + "$", "." + newHost);
            setDomain(domain);
            setHost(newHost);
            return true;
        }
        return false;
    }

    public void setHostTime(final String date) {
        if (date == null) {
            this.hostTime = -1;
            return;
        }
        final Date responseDate = TimeFormatter.parseDateString(date);
        if (responseDate != null) {
            this.hostTime = responseDate.getTime();
            return;
        }
        this.hostTime = -1;
        LoggerFactory.getDefaultLogger().severe("Cookie: no Format for " + date + " found!");
        return;
    }

    public void setKey(final String key) {
        this.key = key;
    }

    public void setPath(String path) {
        path = cleanAttributeValue(path);
        if (StringUtils.isEmpty(path)) {
            this.path = null;
        } else {
            this.path = path;
        }
    }

    protected String cleanAttributeValue(String val) {
        if (val == null) {
            return null;
        }
        val = val.trim();
        /** remove leading and trailing ' and " **/
        if (val.length() >= 2) {
            char first = val.charAt(0);
            char last = val.charAt(val.length() - 1);
            if (first == '"' && last == '"' || first == '\'' && last == '\'') {
                val = val.substring(1, val.length() - 1).trim();
            }
        }
        return val;
    }

    public void setValue(final String value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return this.getKey() + "=" + this.getValue() + " @Host:" + this.getHost() + "|Domain:" + getDomain() + "|HostOnly:" + isHostOnly() + "|Path:" + getPath() + "|SameSite:" + getSameSite() + "|Secure:" + isSecure() + "|Source:" + getSource();
    }

    protected synchronized void update(final Cookie source) {
        if (Boolean.TRUE.equals(isSecure()) && UNSECURE_HTTP.equals(source.getProtocol())) {
            /** unsecure http cookie never can change a secure(d) cookie **/
            DebugMode.debugger();
            return;
        }
        this.setCreationTime(source.getCreationTime());
        this.setExpireDate(source.getExpireDate());
        this.setValue(source.getValue());
        this.setHostTime(source.getHostTime());
        final Boolean hostOnly = source.isHostOnly();
        if (hostOnly != null) {
            setHostOnly(hostOnly);
        }
        final Boolean secure = source.isSecure();
        if (secure != null) {
            setSecure(secure);
        }
        final String domain = source.getDomain();
        if (domain != null) {
            setDomain(domain);
        }
        final String sameSite = source.getSetSameSite();
        if (sameSite != null) {
            setSameSite(sameSite);
        }
    }
}
