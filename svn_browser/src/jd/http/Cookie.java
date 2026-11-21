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
import java.util.Objects;
import java.util.regex.Pattern;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.extmanager.LoggerFactory;

public abstract class Cookie {
    public static final class ServerCookie extends Cookie {
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
        public boolean isPersistent() {
            return true;
        }

        @Override
        public boolean setPersistent(boolean b) {
            return false;
        }

        @Override
        public boolean matchesScope(Cookie obj) {
            if (obj == this) {
                return true;
            } else if (obj instanceof Cookie) {
                /*
                 * we only want Server and Import cookies to be handled/supported by equals method, Legacy/Manual should never equal
                 * Server/Import cookies
                 */
                switch (obj.getSource()) {
                case SERVER:
                case IMPORT:
                    return super.matchesScope(obj);
                default:
                    return false;
                }
            }
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

    public static final class ImportCookie extends Cookie {
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
        public boolean isPersistent() {
            return true;
        }

        @Override
        public boolean setPersistent(boolean b) {
            return false;
        }

        @Override
        public boolean matchesScope(Cookie obj) {
            if (obj == this) {
                return true;
            } else if (obj instanceof Cookie) {
                /*
                 * we only want Server and Import cookies to be handled/supported by equals method, Legacy/Manual should never equal
                 * Server/Import cookies
                 */
                switch (obj.getSource()) {
                case SERVER:
                case IMPORT:
                    return super.matchesScope(obj);
                default:
                    return false;
                }
            }
            return false;
        }

        @Override
        public boolean isCustomCookie() {
            return false;
        }
    }

    public static final class ManualCookie extends Cookie {
        public ManualCookie() {
        }

        public ManualCookie(final String host, final String key, final String value) {
            this.setHost(host);
            this.setKey(key);
            this.setValue(value);
        }

        @Override
        public SOURCE getSource() {
            return SOURCE.MANUAL;
        }

        boolean expiredFlag = false;

        public void setExpired() {
            expiredFlag = true;
        }

        @Override
        public boolean isExpired() {
            if (expiredFlag) {
                return true;
            }
            return super.isExpired();
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

    public static final class LegacyCookie extends Cookie {
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

        @Override
        public boolean isCustomCookie() {
            return getDomain() == null;
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

        public boolean isSameSource(Cookie cookie) {
            return cookie.getSource() == this;
        }
    }

    public final static String UNSECURE_HTTP = "http";
    public final static String SECURE_HTTPS  = "https";

    public abstract SOURCE getSource();

    private String protocol = null;

    public String getProtocol() {
        return protocol;
    }

    public abstract Cookie newCookieInstance();

    public boolean isValid() {
        if (StringUtils.isEmpty(getKey())) {
            /* key must not be empty, value may be null */
            return false;
        }
        final boolean ret;
        final String host = getHost();
        if (isIPAddress(getHost())) {
            final String domain = getDomain();
            ret = domain == null || host.equalsIgnoreCase(domain);
        } else {
            final String domain = getDomain();
            ret = domain == null || !isIPAddress(domain);
        }
        return ret;
    }

    public long getVersion() {
        return 2026_11_20l;
    }

    public boolean isSecureProtocol() {
        final String protocol = getProtocol();
        if (protocol == null) {
            // no protocol available but secure flag is set
            return Boolean.TRUE.equals(isSecure());
        }
        return SECURE_HTTPS.equalsIgnoreCase(protocol);
    }

    public boolean setProtocol(String protocol) {
        final String oldProtocol = this.protocol;
        return !StringUtils.equalsIgnoreCase(oldProtocol, this.protocol = protocol);
    }

    public static <T extends Cookie> T copyOf(T cookie) {
        final T ret = (T) cookie.newCookieInstance();
        ret.setCreationTime(cookie.getCreationTime());
        ret.setPersistent(cookie.isPersistent());
        ret.setExpireDate(cookie.getExpireDate());
        ret.setValue(cookie.getValue());
        ret.setHostTime(cookie.getHostTime());
        ret.setHostOnly(cookie.isHostOnly());
        ret.setSecure(cookie.isSecure());
        ret.setDomain(cookie.getDomain());
        ret.setSameSite(cookie.getSetSameSite());
        ret.setProtocol(cookie.getProtocol());
        ret.setPath(cookie.getPath());
        ret.setKey(cookie.getKey());
        ret.setHost(cookie.getOriginHost());
        return ret;
    }

    private String  path       = null;
    private String  host       = null;
    private String  originHost = null;
    private String  value      = null;
    private String  key        = null;
    private Boolean secure     = null;
    private Boolean hostOnly   = null;
    private String  sameSite   = null;

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

    public boolean setSameSite(String sameSite) {
        final String oldSameSite = this.sameSite;
        if ("Lax".equalsIgnoreCase(sameSite)) {
            this.sameSite = "Lax";
        } else if ("Strict".equalsIgnoreCase(sameSite)) {
            this.sameSite = "Strict";
        } else if ("None".equalsIgnoreCase(sameSite)) {
            this.sameSite = "None";
        } else {
            this.sameSite = null;
        }
        return !StringUtils.equals(oldSameSite, this.sameSite);
    }

    protected static final Pattern IPV4_PATTERN = Pattern.compile("^(?:(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)\\.){3}(?:25[0-5]|2[0-4]\\d|1\\d\\d|[1-9]?\\d)$");

    protected static boolean isIPAddress(String host) {
        if (host == null || host.isEmpty()) {
            return false;
        } else if (host.indexOf(':') != -1) {
            return true;
        } else {
            return IPV4_PATTERN.matcher(host).matches();
        }
    }

    public Boolean isHostOnly() {
        if (isIPAddress(getHost())) {
            return Boolean.TRUE;
        }
        return hostOnly;
    }

    public boolean setHostOnly(Boolean hostOnly) {
        if (Objects.equals(this.hostOnly, hostOnly)) {
            return false;
        }
        this.hostOnly = hostOnly;
        return true;
    }

    public Boolean isSecure() {
        return this.secure;
    }

    public boolean setSecure(Boolean secure) {
        if (Objects.equals(this.secure, secure)) {
            return false;
        }
        this.secure = secure;
        return true;
    }

    private String domain       = null;
    private long   hostTime     = -1;
    private long   creationTime = System.currentTimeMillis();
    private long   expireTime   = -1;

    private Cookie() {
    }

    public abstract boolean isCustomCookie();

    @Override
    public final boolean equals(final Object obj) {
        return super.equals(obj);
    }

    public boolean matchesScope(final Cookie other) {
        if (other == null) {
            return false;
        } else if (this == other) {
            return true;
        }
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

    private boolean persistent = true;

    public boolean isPersistent() {
        return persistent;
    }

    public boolean setPersistent(boolean persistent) {
        final boolean oldPersistent = isPersistent();
        return oldPersistent != (this.persistent = persistent);
    }

    public boolean matches(final URL url) {
        if (Boolean.TRUE.equals(isSecure()) && !Cookie.SECURE_HTTPS.equalsIgnoreCase(url.getProtocol())) {
            return false;
        }
        String urlHost = url.getHost();
        try {
            urlHost = normalizeHost(urlHost);
        } catch (IllegalArgumentException ignore) {
            LogV3.log(ignore);
        }
        final String host = getHost();
        final String domain = getDomain();
        if (isIPAddress(urlHost) && (host != null && !urlHost.equalsIgnoreCase(host) || domain != null && !urlHost.equalsIgnoreCase(domain))) {
            return false;
        }
        final String scope = domain != null ? domain : getOriginHost();
        if (Boolean.TRUE.equals(isHostOnly()) && scope != null && !StringUtils.equalsIgnoreCase(urlHost, scope)) {
            return false;
        }
        if (domain != null && !StringUtils.endsWithCaseInsensitive("." + urlHost, "." + domain)) {
            return false;
        }
        if (!isCookiePathMatch(url)) {
            return false;
        }
        return true;
    }

    /* RFC 6265 (Sektion 5.1.4) */
    protected boolean setPath(URL url) {
        if (url == null) {
            return setPath("/");
        }
        final String uriPath = url.getPath();
        if (uriPath == null || uriPath.isEmpty() || uriPath.charAt(0) != '/') {
            return setPath("/");
        }
        final int lastSlashIndex = uriPath.lastIndexOf('/');
        if (lastSlashIndex <= 0) {
            return setPath("/");
        }
        return setPath(uriPath.substring(0, lastSlashIndex));
    }

    protected String normalizeHost(String host) throws IllegalArgumentException {
        if (host == null || host.isEmpty()) {
            return host;
        }
        boolean hasUpper = false;
        for (int i = 0, len = host.length(); i < len; i++) {
            final char c = host.charAt(i);
            if (c >= 128) {
                return IDN.toASCII(host).toLowerCase(Locale.ROOT);
            }
            if (hasUpper || c >= 'A' && c <= 'Z') {
                hasUpper = true;
            }
        }
        return hasUpper ? host.toLowerCase(Locale.ROOT) : host;
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

    /* returns host without subdomain */
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

    public boolean setCreationTime(final long time) {
        final long oldCreationTime = this.creationTime;
        return oldCreationTime != (this.creationTime = time);
    }

    protected String removeLeadingDot(String value) {
        if (value == null || !value.startsWith(".")) {
            return value;
        }
        /** RFC 6265, remove leading dot **/
        return value.replaceFirst("^\\.+", "");
    }

    public boolean setDomain(String domain) {
        final String oldDomain = this.domain;
        if (domain != null) {
            domain = cleanAttributeValue(domain);
            domain = removeLeadingDot(domain);
            /** normalize domain */
            try {
                domain = normalizeHost(domain);
            } catch (IllegalArgumentException e) {
                LoggerFactory.getDefaultLogger().log(e);
                DebugMode.debugger();
                return false;
            }
        }
        if (StringUtils.isEmpty(domain)) {
            domain = null;
        }
        return !StringUtils.equalsIgnoreCase(oldDomain, this.domain = domain);
    }

    protected String normalizeSetHost(String host) {
        if (host == null || host.isEmpty()) {
            return host;
        }
        host = removeLeadingDot(host);
        try {
            return normalizeHost(host);
        } catch (IllegalArgumentException e) {
            LoggerFactory.getDefaultLogger().log(e);
            return host.toLowerCase(Locale.ROOT);
        }
    }

    public boolean setExpireDate(final long time) {
        final long oldExpireTime = this.expireTime;
        return oldExpireTime != (this.expireTime = time);
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

    /* returns full host if available, else getHost() */
    public String getOriginHost() {
        final String ret = originHost;
        if (ret != null) {
            return ret;
        }
        return getHost();
    }

    public boolean setHost(final String host) {
        final String oldHost = this.host;
        return !StringUtils.equalsIgnoreCase(oldHost, this.host = Browser.getHost(this.originHost = normalizeSetHost(Browser.getHost(host, true)), false));
    }

    public boolean updateHost(String newHost) {
        if (newHost == null) {
            return false;
        }
        newHost = removeLeadingDot(newHost);
        final class UpdateOriginHost {
            String update(final Cookie cookie, String newHost) {
                final String oldOriginHost = cookie.getOriginHost();
                final String oldHost = cookie.getHost();
                if (oldHost == null || oldOriginHost == null || oldHost.equals(oldOriginHost)) {
                    return newHost;
                }
                final String ret = oldOriginHost.replaceFirst("(?i)\\." + Pattern.quote(oldHost) + "$", "." + newHost);
                return ret;
            }
        }
        final String oldHost = getHost();
        if (StringUtils.equalsIgnoreCase(oldHost, newHost)) {
            /* oldHost equals newHost */
            return true;
        }
        newHost = newHost.toLowerCase(Locale.ROOT);
        final String oldDomain = getDomain();
        if (oldDomain == null) {
            return setHost(new UpdateOriginHost().update(this, newHost));
        } else if (StringUtils.equalsIgnoreCase(oldHost, oldDomain)) {
            boolean ret = setDomain(newHost);
            ret |= setHost(new UpdateOriginHost().update(this, newHost));
            return ret;
        } else if (StringUtils.endsWithCaseInsensitive(oldDomain, "." + oldHost)) {
            final String updatedDomain = oldDomain.replaceFirst("(?i)\\." + Pattern.quote(oldHost) + "$", "." + newHost);
            boolean ret = setDomain(updatedDomain);
            ret |= setHost(new UpdateOriginHost().update(this, newHost));
            return ret;
        }
        return false;
    }

    public boolean setHostTime(final long hostTime) {
        final long oldHostTime = this.hostTime;
        return oldHostTime != (this.hostTime = hostTime);
    }

    public boolean setHostTime(final String date) {
        if (date == null) {
            return setHostTime(-1);
        }
        final Date responseDate = TimeFormatter.parseDateString(date);
        if (responseDate != null) {
            return setHostTime(responseDate.getTime());
        }
        LoggerFactory.getDefaultLogger().severe("Cookie: no Format for " + date + " found!");
        return setHostTime(-1);
    }

    public boolean setKey(final String key) {
        final String oldKey = this.key;
        return !StringUtils.equals(oldKey, this.key = key);
    }

    protected static boolean testPath(String path) {
        path = cleanAttributeValue(path);
        if (StringUtils.isEmpty(path) || !path.startsWith("/")) {
            return false;
        }
        return true;
    }

    public boolean setPath(String path) {
        final String oldPath = this.path;
        if (!testPath(path)) {
            path = null;
        }
        return !StringUtils.equals(oldPath, this.path = cleanAttributeValue(path));
    }

    protected static String cleanAttributeValue(String val) {
        if (val == null) {
            return null;
        }
        val = val.trim();
        /** remove leading and trailing ' and " **/
        if (val.length() >= 2) {
            final char first = val.charAt(0);
            final char last = val.charAt(val.length() - 1);
            if (first == '"' && last == '"' || first == '\'' && last == '\'') {
                val = val.substring(1, val.length() - 1).trim();
            }
        }
        return val;
    }

    public boolean setValue(final String value) {
        final String oldValue = this.value;
        return !StringUtils.equals(oldValue, this.value = value);
    }

    @Override
    public String toString() {
        return (isExpired() ? "EXPIRED:" : "") + getKey() + "=" + this.getValue() + " @Host:" + this.getOriginHost() + "(" + getHost() + ")" + "|Domain:" + getDomain() + "|HostOnly:" + isHostOnly() + "|Path:" + getPath() + "|SameSite:" + getSameSite() + "|Secure:" + isSecure() + "|Source:" + getSource();
    }
}
