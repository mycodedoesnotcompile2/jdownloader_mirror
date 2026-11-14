package jd.plugins;

import org.appwork.storage.Storable;
import org.appwork.storage.TypeRef;

import jd.http.Cookie;

public class CookieStorable implements Storable {
    public static final TypeRef<CookieStorable> TYPE_REF = new TypeRef<CookieStorable>() {
    };

    public String getPath() {
        if (this.cookie != null) {
            return this.cookie.getPath();
        }
        return this.path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getHost() {
        if (this.cookie != null) {
            return this.cookie.getHost();
        }
        return this.host;
    }

    public void setHost(String host) {
        this.host = host;
    }

    public String getValue() {
        if (this.cookie != null) {
            return this.cookie.getValue();
        } else {
            return this.value;
        }
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getKey() {
        if (this.cookie != null) {
            return this.cookie.getKey();
        }
        return this.key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getDomain() {
        if (this.cookie != null) {
            return this.cookie.getDomain();
        }
        return this.domain;
    }

    public void setDomain(String domain) {
        this.domain = domain;
    }

    public long getHostTime() {
        if (this.cookie != null) {
            return this.cookie.getHostTime();
        }
        return this.hostTime;
    }

    public void setHostTime(long hostTime) {
        this.hostTime = hostTime;
    }

    public long getCreationTime() {
        if (this.cookie != null) {
            return this.cookie.getCreationTime();
        }
        return this.creationTime;
    }

    public void setCreationTime(long creationTime) {
        this.creationTime = creationTime;
    }

    public long getExpireTime() {
        if (this.cookie != null) {
            return this.cookie.getExpireDate();
        }
        return this.expireTime;
    }

    public void setExpireTime(long expireTime) {
        this.expireTime = expireTime;
    }

    private String path       = null;
    private String host       = null;
    private String originHost = null;

    public String getOriginHost() {
        if (cookie != null) {
            return cookie.getOriginHost();
        }
        final String ret = originHost;
        if (ret != null) {
            return ret;
        }
        return getHost();
    }

    public void setOriginHost(String originHost) {
        this.originHost = originHost;
    }

    private String value    = null;
    private String key      = null;
    private String domain   = null;
    private String sameSite = null;
    private String protocol = null;

    public String getProtocol() {
        if (this.cookie != null) {
            return this.cookie.getProtocol();
        }
        return protocol;
    }

    public void setProtocol(String protocol) {
        this.protocol = protocol;
    }

    public String getSameSite() {
        if (this.cookie != null) {
            return this.cookie.getSetSameSite();
        }
        return sameSite;
    }

    public void setSameSite(String sameSite) {
        this.sameSite = sameSite;
    }

    private Boolean secure   = null;
    private Boolean hostOnly = null;

    public Boolean getSecure() {
        if (this.cookie != null) {
            return this.cookie.isSecure();
        }
        return secure;
    }

    public void setSecure(Boolean secure) {
        this.secure = secure;
    }

    private long         hostTime     = -1;
    private long         creationTime = -1;
    private long         expireTime   = -1;
    private final Cookie cookie;
    private String       source       = null;

    public CookieStorable(/* Storable */) {
        this(null);
    }

    public CookieStorable(Cookie cookie) {
        this.cookie = cookie;
    }

    public Cookie _restore() {
        Cookie.SOURCE source = Cookie.SOURCE.LEGACY;
        try {
            if (getSource() != null) {
                source = Cookie.SOURCE.valueOf(getSource());
            }
        } catch (IllegalArgumentException e) {
        }
        if (Cookie.SOURCE.LEGACY.equals(source)) {
            if (getHostTime() > 0) {
                source = Cookie.SOURCE.SERVER;
            } else if (getDomain() != null || getSecure() != null) {
                source = Cookie.SOURCE.IMPORT;
            } else if (getDomain() == null && "/".equals(getPath())) {
                source = Cookie.SOURCE.MANUAL;
            }
        }
        final Cookie ret = source.newCookieInstance();
        ret.setHost(getOriginHost());
        ret.setValue(this.getValue());
        ret.setKey(this.getKey());
        ret.setDomain(this.getDomain());
        ret.setHostTime(this.getHostTime());
        ret.setCreationTime(this.getCreationTime());
        ret.setExpireDate(this.getExpireTime());
        ret.setSecure(getSecure());
        ret.setPath(getPath());
        ret.setHostOnly(getHostOnly());
        ret.setSameSite(getSameSite());
        ret.setProtocol(getProtocol());
        return ret;
    }

    public Boolean getHostOnly() {
        if (this.cookie != null) {
            return this.cookie.isHostOnly();
        }
        return hostOnly;
    }

    public void setHostOnly(Boolean hostOnly) {
        this.hostOnly = hostOnly;
    }

    public String getSource() {
        if (this.cookie != null) {
            return this.cookie.getSource().name();
        }
        return source;
    }

    /**
     * @param source
     *            the source to set
     */
    public void setSource(String source) {
        this.source = source;
    }
}