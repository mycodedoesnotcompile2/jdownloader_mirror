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

import java.util.Date;
import java.util.Locale;

import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.extmanager.LoggerFactory;

public class Cookie {
    private String  path   = null;
    private String  host   = null;
    private String  value  = null;
    private String  key    = null;
    private Boolean secure = null;

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

    public Cookie(final String host, final String key, final String value) {
        this.setHost(host);
        this.setKey(key);
        this.setValue(value);
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
        if (!StringUtils.equalsIgnoreCase(this.getHost(), other.getHost())) {
            return false;
        }
        if (!StringUtils.equals(this.getKey(), other.getKey())) {
            return false;
        }
        /*
         * setting cookie via br.setCookie always has a null path, in order to allow updating of existing cookie entries we need to not
         * analyse original path that is null! -raztoki20160506
         */
        if (!StringUtils.equals(this.getPath(), other.getPath())) {
            return false;
        }
        /*
         * domain property is not used at the moment, that's why we ignore it
         *
         * TODO: add proper support once a service really uses this feature
         *
         * if (!StringUtils.equalsIgnoreCase(this.getDomain(), other.getDomain())) { return false; }
         */
        return true;
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
        if (this.path != null) {
            return this.path;
        } else {
            return "/";
        }
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
        this.domain = domain;
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
        return this.getKey() + "=" + this.getValue() + " @" + this.getHost();
    }

    protected synchronized void update(final Cookie cookie2) {
        this.setCreationTime(cookie2.getCreationTime());
        this.setExpireDate(cookie2.getExpireDate());
        this.setValue(cookie2.getValue());
        this.setHostTime(cookie2.getHostTime());
    }
}
