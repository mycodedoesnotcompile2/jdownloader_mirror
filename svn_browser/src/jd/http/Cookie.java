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

import java.net.URL;
import java.util.Date;
import java.util.Locale;

import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.extmanager.LoggerFactory;

public class Cookie {
    public final static String UNSECURE_HTTP = "http";
    public final static String SECURE_HTTPS  = "https";
    private String             protocol      = null;

    public String getProtocol() {
        return protocol;
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

    private String  path     = null;
    private String  host     = null;
    private String  value    = null;
    private String  key      = null;
    private Boolean secure   = null;
    private Boolean hostOnly = null;
    private String  sameSite = null;
    private int     version  = 1;

    public int getVersion() {
        return version;
    }

    public void setVersion(int version) {
        this.version = version;
    }

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

    public Cookie() {
    }

    @Deprecated
    public Cookie(final String host, final String key, final String value) {
        this.setHost(host);
        this.setKey(key);
        this.setValue(value);
    }

    public boolean isCustomCookie() {
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
        /*
         * setting cookie via br.setCookie always has a null path, in order to allow updating of existing cookie entries we need to not
         * analyse original path that is null! -raztoki20160506
         */
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
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && !isCookiePathMatch(url)) {
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
            // Cookie-Path endet bereits auf "/" (z.B. cookiePath="/app/", requestPath="/app/login")
            if (cookiePath.endsWith("/")) {
                return true;
            }
            // Das nächste Zeichen im Request-Path nach dem Cookie-Path ist "/"
            // (z.B. cookiePath="/app", requestPath="/app/login")
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
        return (this.getHost() + "_" + this.getKey().toLowerCase(Locale.ENGLISH)).hashCode();
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

    public void setDomain(final String domain) {
        if (domain == null) {
            this.domain = null;
        } else {
            // RFC 6265, remove leading dot
            this.domain = domain.replaceFirst("^\\.+", "");
        }
    }

    public void setExpireDate(final long time) {
        this.expireTime = time;
    }

    public void setExpires(final String expires) {
        if (expires == null) {
            this.expireTime = -1;
            // System.out.println("setExpire: Cookie: no expireDate found! " + this.host + " " + this.key);
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
        this.host = Browser.getHost(host);
    }

    public void setHostTime(final long time) {
        this.hostTime = time;
    }

    public void setHostTime(final String date) {
        if (date == null) {
            this.hostTime = -1;
            // System.out.println("Cookie: no HostTime found! " + this.host + " " + this.key);
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

    public void setPath(final String path) {
        if (StringUtils.isEmpty(path)) {
            this.path = null;
        } else {
            this.path = path;
        }
    }

    public void setValue(final String value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return this.getKey() + "=" + this.getValue() + " @Host:" + this.getHost() + "/Domain:" + getDomain() + "/HostOnly:" + isHostOnly() + "/Path:" + getPath() + "/SameSite:" + getSameSite();
    }

    protected synchronized void update(final Cookie source) {
        if (Boolean.TRUE.equals(isSecure()) && UNSECURE_HTTP.equals(source.getProtocol())) {
            // unsecure http cookie never can change a secure(d) cookie
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
