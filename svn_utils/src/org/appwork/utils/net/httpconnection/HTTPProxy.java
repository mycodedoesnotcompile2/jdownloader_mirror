/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.SocketAddress;
import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.locale._AWU;

public class HTTPProxy {
    public static enum TYPE implements LabelInterface {
        NONE {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_none();
            }
        },
        DIRECT {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_direct();
            }
        },
        HTTP {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_http();
            }
        },
        HTTPS {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_https();
            }
        },
        SOCKS4 {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_socks4();
            }
        },
        SOCKS4A {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_socks4a();
            }
        },
        SOCKS5 {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_socks5();
            }
        },
        AUTO {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_auto_detect();
            }
        };
    }

    public static final HTTPProxy NONE = new HTTPProxy(TYPE.NONE) {
                                           @Override
                                           public void setConnectMethodPrefered(final boolean value) {
                                           }

                                           @Override
                                           public void setLocal(final String local) {
                                           }

                                           @Override
                                           public void setPass(final String pass) {
                                           }

                                           @Override
                                           public void setPort(final int port) {
                                           }

                                           @Override
                                           public void setType(final TYPE type) {
                                               super.setType(TYPE.NONE);
                                           }

                                           @Override
                                           public void setUser(final String user) {
                                           }
                                       };

    public static List<HTTPProxy> getFromSystemProperties() {
        final java.util.List<HTTPProxy> ret = new ArrayList<HTTPProxy>();
        try {
            {
                /* try to parse http proxy from system properties */
                final String host = System.getProperties().getProperty("http.proxyHost");
                if (!StringUtils.isEmpty(host)) {
                    int port = 80;
                    final String ports = System.getProperty("http.proxyPort");
                    if (!StringUtils.isEmpty(ports)) {
                        port = Integer.parseInt(ports);
                    }
                    final HTTPProxy pr = new HTTPProxy(HTTPProxy.TYPE.HTTP, host, port);
                    final String user = System.getProperty("http.proxyUser");
                    final String pass = System.getProperty("http.proxyPassword");
                    if (!StringUtils.isEmpty(user)) {
                        pr.setUser(user);
                    }
                    if (!StringUtils.isEmpty(pass)) {
                        pr.setPass(pass);
                    }
                    ret.add(pr);
                }
            }
            {
                /* try to parse http proxy from system properties */
                final String host = System.getProperties().getProperty("https.proxyHost");
                if (!StringUtils.isEmpty(host)) {
                    int port = 443;
                    final String ports = System.getProperty("https.proxyPort");
                    if (!StringUtils.isEmpty(ports)) {
                        port = Integer.parseInt(ports);
                    }
                    final HTTPProxy pr = new HTTPProxy(HTTPProxy.TYPE.HTTPS, host, port);
                    final String user = System.getProperty("https.proxyUser");
                    final String pass = System.getProperty("https.proxyPassword");
                    if (!StringUtils.isEmpty(user)) {
                        pr.setUser(user);
                    }
                    if (!StringUtils.isEmpty(pass)) {
                        pr.setPass(pass);
                    }
                    ret.add(pr);
                }
            }
            {
                /* try to parse socks5 proxy from system properties */
                final String host = System.getProperties().getProperty("socksProxyHost");
                if (!StringUtils.isEmpty(host)) {
                    int port = 1080;
                    final String ports = System.getProperty("socksProxyPort");
                    if (!StringUtils.isEmpty(ports)) {
                        port = Integer.parseInt(ports);
                    }
                    final HTTPProxy pr = new HTTPProxy(HTTPProxy.TYPE.SOCKS5, host, port);
                    ret.add(pr);
                }
            }
        } catch (final Throwable e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
        return ret;
    }

    public static HTTPProxy getHTTPProxy(final HTTPProxyStorable storable) {
        if (storable == null || storable.getType() == null) {
            return null;
        }
        final HTTPProxy ret;
        switch (storable.getType()) {
        case NONE:
            ret = new HTTPProxy(TYPE.NONE);
            break;
        case DIRECT:
            ret = new HTTPProxy(TYPE.DIRECT);
            ret.setLocal(storable.getAddress());
            break;
        case HTTP:
            ret = new HTTPProxy(TYPE.HTTP);
            ret.setHost(storable.getAddress());
            break;
        case HTTPS:
            ret = new HTTPProxy(TYPE.HTTPS);
            ret.setHost(storable.getAddress());
            break;
        case SOCKS4:
            ret = new HTTPProxy(TYPE.SOCKS4);
            ret.setHost(storable.getAddress());
            break;
        case SOCKS4A:
            ret = new HTTPProxy(TYPE.SOCKS4A);
            ret.setHost(storable.getAddress());
            break;
        case SOCKS5:
            ret = new HTTPProxy(TYPE.SOCKS5);
            ret.setHost(storable.getAddress());
            break;
        case AUTO:
            ret = new HTTPProxy(TYPE.AUTO);
            ret.setHost(storable.getAddress());
            break;
        default:
            return null;
        }
        ret.setPreferNativeImplementation(storable.isPreferNativeImplementation());
        ret.setConnectMethodPrefered(storable.isConnectMethodPrefered());
        ret.setResolveHostname(storable.isResolveHostName());
        ret.setPass(storable.getPassword());
        ret.setUser(storable.getUsername());
        ret.setPort(storable.getPort());
        // ret.setKeepAliveSupported(storable.isKeepAliveSupported());
        return ret;
    }

    private static String[] getInfo(final String host, final String port) {
        final String[] info = new String[2];
        if (host == null) {
            return info;
        }
        final String tmphost = host.replaceFirst("^https?://", "");
        String tmpport = new org.appwork.utils.Regex(host, ":(\\d+)(/|$)").getMatch(0);
        if (tmpport != null) {
            info[1] = tmpport;
        } else {
            if (port != null) {
                tmpport = new Regex(port, "(\\d+)").getMatch(0);
            }
            if (tmpport != null) {
                info[1] = tmpport;
            } else {
                info[1] = "8080";
            }
        }
        info[0] = new Regex(tmphost, "(.*?)(:\\d+$|:\\d+/|/|$)").getMatch(0);
        return info;
    }

    public static HTTPProxyStorable getStorable(final HTTPProxy proxy) {
        if (proxy == null || proxy.getType() == null) {
            return null;
        } else {
            final HTTPProxyStorable ret = new HTTPProxyStorable();
            proxy.fillProxyStorable(ret);
            return ret;
        }
    }

    public void fillProxyStorable(final HTTPProxyStorable ret) {
        switch (getType()) {
        case NONE:
            ret.setType(HTTPProxyStorable.TYPE.NONE);
            ret.setAddress(null);
            break;
        case DIRECT:
            ret.setType(HTTPProxyStorable.TYPE.DIRECT);
            ret.setAddress(getLocal());
            break;
        case HTTP:
            ret.setType(HTTPProxyStorable.TYPE.HTTP);
            ret.setAddress(getHost());
            break;
        case HTTPS:
            ret.setType(HTTPProxyStorable.TYPE.HTTPS);
            ret.setAddress(getHost());
            break;
        case SOCKS4:
            ret.setType(HTTPProxyStorable.TYPE.SOCKS4);
            ret.setAddress(getHost());
            break;
        case SOCKS4A:
            ret.setType(HTTPProxyStorable.TYPE.SOCKS4A);
            ret.setAddress(getHost());
            break;
        case SOCKS5:
            ret.setType(HTTPProxyStorable.TYPE.SOCKS5);
            ret.setAddress(getHost());
            break;
        case AUTO:
            ret.setType(HTTPProxyStorable.TYPE.AUTO);
            ret.setAddress(getHost());
            break;
        default:
            break;
        }
        ret.setConnectMethodPrefered(isConnectMethodPrefered());
        ret.setPreferNativeImplementation(isPreferNativeImplementation());
        ret.setResolveHostName(isResolveHostname());
        ret.setPort(getPort());
        ret.setPassword(getPass());
        ret.setUsername(getUser());
        ret._setDoNotStoreInstance(isDoNotStoreInstance());
        // ret.setKeepAliveSupported(isKeepAliveSupported());
    }

    public static HTTPProxy parseHTTPProxy(final String s) {
        if (StringUtils.isEmpty(s)) {
            return null;
        } else {
            final String type = new Regex(s, "(https?|socks(5|4a|4)|direct|none)://").getMatch(0);
            final String auth = new Regex(s, "://(.+)@").getMatch(0);
            final String host = new Regex(s, "://(.+@)?(.*?)(/|$)").getMatch(1);
            final HTTPProxy ret;
            if (StringUtils.equalsIgnoreCase("none", type)) {
                ret = new HTTPProxy(TYPE.NONE);
            } else if (StringUtils.equalsIgnoreCase("https", type)) {
                ret = new HTTPProxy(TYPE.HTTPS);
                ret.setPort(443);
            } else if (StringUtils.equalsIgnoreCase("http", type)) {
                ret = new HTTPProxy(TYPE.HTTP);
                ret.setPort(8080);
            } else if (StringUtils.equalsIgnoreCase("socks5", type)) {
                ret = new HTTPProxy(TYPE.SOCKS5);
                ret.setPort(1080);
            } else if (StringUtils.equalsIgnoreCase("socks4", type)) {
                ret = new HTTPProxy(TYPE.SOCKS4);
                ret.setPort(1080);
            } else if (StringUtils.equalsIgnoreCase("socks4a", type)) {
                ret = new HTTPProxy(TYPE.SOCKS4A);
                ret.setPort(1080);
            } else if (StringUtils.equalsIgnoreCase("direct", type)) {
                ret = new HTTPProxy(TYPE.DIRECT);
                ret.setLocal(host);
            } else {
                return null;
            }
            final String hostname = new Regex(host, "(.*?)(:\\d+$|$)").getMatch(0);
            final String port = new Regex(host, ".*?:(\\d+)$").getMatch(0);
            if (!StringUtils.isEmpty(hostname)) {
                ret.setHost(hostname);
            }
            if (!StringUtils.isEmpty(port)) {
                ret.setPort(Integer.parseInt(port));
            }
            final String username = new Regex(auth, "(.*?)(:|$)").getMatch(0);
            final String password = new Regex(auth, ".*?:(.+)").getMatch(0);
            if (!StringUtils.isEmpty(username)) {
                ret.setUser(username);
            }
            if (!StringUtils.isEmpty(password)) {
                ret.setPass(password);
            }
            switch (ret.getType()) {
            case NONE:
                return ret;
            default:
                if (!StringUtils.isEmpty(ret.getHost())) {
                    return ret;
                } else {
                    return null;
                }
            }
        }
    }

    protected String  local                      = null;
    protected String  user                       = null;
    protected String  pass                       = null;
    protected int     port                       = 80;
    protected String  host                       = null;
    protected TYPE    type                       = TYPE.DIRECT;
    protected boolean useConnectMethod           = false;
    protected boolean preferNativeImplementation = false;
    protected boolean resolveHostname            = false;
    protected boolean keepAliveSupported         = false;

    protected boolean doNotStoreInstance         = false;

    public boolean isDoNotStoreInstance() {
        return doNotStoreInstance;
    }

    /**
     * Do not save the proxy to disk at all. This is usefull if the proxy was found By some proxy script
     *
     * @param doNotStoreInstance
     */
    public void setDoNotStoreInstance(boolean doNotStoreInstance) {
        this.doNotStoreInstance = doNotStoreInstance;
    }

    public boolean isKeepAliveSupported() {
        return keepAliveSupported;
    }

    public void setKeepAliveSupported(boolean keepAliveSupported) {
        this.keepAliveSupported = keepAliveSupported;
    }

    public boolean isResolveHostname() {
        return resolveHostname;
    }

    public void setResolveHostname(boolean resolveHostname) {
        this.resolveHostname = resolveHostname;
    }

    protected HTTPProxy() {
    }

    public HTTPProxy(final HTTPProxy proxy) {
        this.set(proxy);
    }

    public HTTPProxy(final InetAddress direct) {
        this.setType(TYPE.DIRECT);
        this.setLocal(direct.getHostAddress());
    }

    public HTTPProxy(final TYPE type) {
        this.setType(type);
    }

    public HTTPProxy(final TYPE type, final String host, final int port) {
        this.setPort(port);
        this.setType(type);
        this.setHost(HTTPProxy.getInfo(host, Integer.toString(port))[0]);
    }

    public String _toString() {
        final TYPE type = getType();
        if (type != null) {
            switch (type) {
            case NONE:
                return _AWU.T.proxy_none();
            case HTTP:
                return _AWU.T.proxy_http(getHost(), this.getPort());
            case HTTPS:
                return _AWU.T.proxy_https(getHost(), this.getPort());
            case SOCKS5:
                return _AWU.T.proxy_socks5(getHost(), this.getPort());
            case SOCKS4:
                return _AWU.T.proxy_socks4(getHost(), this.getPort());
            case SOCKS4A:
                return _AWU.T.proxy_socks4a(getHost(), this.getPort());
            case DIRECT:
                return _AWU.T.proxy_direct(getLocal());
            case AUTO:
                return _AWU.T.proxy_type_auto_detect();
            default:
                return "UNKNOWN:" + type;
            }
        }
        return "UNKNOWN";
    }

    @Override
    public HTTPProxy clone() {
        final HTTPProxy ret = new HTTPProxy();
        ret.cloneProxy(this);
        return ret;
    }

    protected void cloneProxy(final HTTPProxy proxy) {
        if (proxy != null) {
            this.set(proxy);
        }
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        } else if (obj == null || !(obj instanceof HTTPProxy)) {
            return false;
        } else {
            final HTTPProxy p = (HTTPProxy) obj;
            if (this.getType() != p.getType()) {
                return false;
            } else {
                switch (this.getType()) {
                case DIRECT:
                    return StringUtils.equals(this.getLocal(), p.getLocal());
                case NONE:
                    return true;
                default:
                    if (!StringUtils.equals(this.getHost(), p.getHost())) {
                        return false;
                    } else if (this.getPort() != p.getPort()) {
                        return false;
                    } else if (!StringUtils.equals(StringUtils.isEmpty(this.getUser()) ? null : this.getUser(), StringUtils.isEmpty(p.getUser()) ? null : p.getUser())) {
                        return false;
                    } else if (!StringUtils.equals(StringUtils.isEmpty(this.getPass()) ? null : this.getPass(), StringUtils.isEmpty(p.getPass()) ? null : p.getPass())) {
                        return false;
                    } else {
                        return true;
                    }
                }
            }
        }
    }

    public boolean equalsWithSettings(HTTPProxy proxy) {
        if (this.equals(proxy)) {
            switch (this.getType()) {
            case HTTP:
            case HTTPS:
                if (this.isConnectMethodPrefered() != proxy.isConnectMethodPrefered()) {
                    return false;
                } else {
                    break;
                }
            case SOCKS4:
            case SOCKS4A:
            case SOCKS5:
                if (isKeepAliveSupported() != proxy.isKeepAliveSupported()) {
                    return false;
                } else {
                    break;
                }
            default:
                break;
            }
            if (this.isPreferNativeImplementation() != proxy.isPreferNativeImplementation()) {
                return false;
            } else if (isResolveHostname() != proxy.isResolveHostname()) {
                return false;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }

    public String getHost() {
        return this.host;
    }

    public String getLocal() {
        return this.local;
    }

    public String getPass() {
        return this.pass;
    }

    public int getPort() {
        return this.port;
    }

    public TYPE getType() {
        return this.type;
    }

    public String getUser() {
        return this.user;
    }

    @Override
    public int hashCode() {
        return HTTPProxy.class.hashCode();
    }

    public boolean isConnectMethodPrefered() {
        return this.useConnectMethod;
    }

    /**
     * this proxy is DIRECT = using a local bound IP
     *
     * @return
     */
    public boolean isDirect() {
        return this.type == TYPE.DIRECT;
    }

    public boolean isLocal() {
        return this.isDirect() || this.isNone();
    }

    /**
     * this proxy is NONE = uses default gateway
     *
     * @return
     */
    public boolean isNone() {
        return this.type == TYPE.NONE;
    }

    /**
     * @return the preferNativeImplementation
     */
    public boolean isPreferNativeImplementation() {
        return this.preferNativeImplementation;
    }

    /**
     * this proxy is REMOTE = using http,socks proxy
     *
     * @return
     */
    public boolean isRemote() {
        return !this.isDirect() && !this.isNone();
    }

    protected void set(final HTTPProxy proxy) {
        if (proxy != null) {
            this.setUser(proxy.getUser());
            this.setHost(proxy.getHost());
            this.setLocal(proxy.getLocal());
            this.setPass(proxy.getPass());
            this.setPort(proxy.getPort());
            this.setType(proxy.getType());
            this.setResolveHostname(proxy.isResolveHostname());
            this.setConnectMethodPrefered(proxy.isConnectMethodPrefered());
            this.setPreferNativeImplementation(proxy.isPreferNativeImplementation());
            this.setKeepAliveSupported(proxy.isKeepAliveSupported());
            this.setDoNotStoreInstance(proxy.isDoNotStoreInstance());
        }
    }

    public void setConnectMethodPrefered(final boolean value) {
        this.useConnectMethod = value;
    }

    public void setHost(String host) {
        if (host != null) {
            this.host = host.trim();
        } else {
            this.host = host;
        }
    }

    /**
     * @param localIP
     *            the localIP to set
     */
    public void setLocal(final String local) {
        if (local != null) {
            this.local = local.trim();
        } else {
            this.local = local;
        }
    }

    public void setPass(final String pass) {
        this.pass = pass;
    }

    public void setPort(final int port) {
        this.port = port;
    }

    /**
     * @param preferNativeImplementation
     *            the preferNativeImplementation to set
     */
    public void setPreferNativeImplementation(final boolean preferNativeImplementation) {
        this.preferNativeImplementation = preferNativeImplementation;
    }

    public void setType(final TYPE type) {
        this.type = type;
    }

    public void setUser(final String user) {
        this.user = user;
    }

    @Override
    public String toString() {
        String ret = _toString();
        if (StringUtils.isNotEmpty(user)) {
            ret = user + ":" + (StringUtils.isEmpty(pass) ? "-" : "*****") + "@" + ret;
        }
        if (this.isPreferNativeImplementation()) {
            ret = ret + "(prefer native)";
        }
        return ret;
    }

    public static TYPE convertNativeProxyType(final Proxy proxy) {
        if (proxy != null) {
            switch (proxy.type()) {
            case SOCKS:
                try {
                    if (ReflectionUtils.isInstanceOf("sun.net.SocksProxy", proxy) && ReflectionUtils.invoke("sun.net.SocksProxy", "protocolVersion", proxy, Number.class).intValue() == 4) {
                        return TYPE.SOCKS4;
                    }
                } catch (final Throwable ignore) {
                }
                return TYPE.SOCKS5;
            case HTTP:
                return TYPE.HTTP;
            case DIRECT:
                return TYPE.NONE;
            default:
                return null;
            }
        } else {
            return null;
        }
    }

    public Proxy toNativeProxy() {
        switch (getType()) {
        case NONE:
        case DIRECT:
            return Proxy.NO_PROXY;
        case HTTP:
            return new Proxy(Proxy.Type.HTTP, new InetSocketAddress(getHost(), getPort()));
        case SOCKS4:
        case SOCKS4A:
            try {
                final Proxy ret = ReflectionUtils.invoke("sun.net.SocksProxy", "create", null, Proxy.class, new Class<?>[] { SocketAddress.class, int.class }, new InetSocketAddress(getHost(), getPort()), 4);
                if (ret != null) {
                    return ret;
                }
            } catch (final Throwable e) {
            }
            return new Proxy(Proxy.Type.SOCKS, new InetSocketAddress(getHost(), getPort()));
        case SOCKS5:
            return new Proxy(Proxy.Type.SOCKS, new InetSocketAddress(getHost(), getPort()));
        default:
            return null;
        }
    }

    public static HTTPProxy fromNativeProxy(final Proxy proxy) {
        if (proxy != null) {
            switch (proxy.type()) {
            case DIRECT:
                return new HTTPProxy(TYPE.NONE);
            case HTTP:
                if (proxy.address() instanceof InetSocketAddress) {
                    final String host = ((InetSocketAddress) proxy.address()).getHostName();
                    final int port = ((InetSocketAddress) proxy.address()).getPort();
                    return new HTTPProxy(TYPE.HTTP, host, port);
                } else {
                    return null;
                }
            case SOCKS:
                if (proxy.address() instanceof InetSocketAddress) {
                    final String host = ((InetSocketAddress) proxy.address()).getHostName();
                    final int port = ((InetSocketAddress) proxy.address()).getPort();
                    return new HTTPProxy(convertNativeProxyType(proxy), host, port);
                } else {
                    return null;
                }
            default:
                return null;
            }
        } else {
            return null;
        }
    }

    /**
     * @param plist
     * @return
     */
    public static List<? extends HTTPProxy> convert(List<Proxy> plist) {
        final ArrayList<HTTPProxy> ret = new ArrayList<HTTPProxy>();
        if (plist != null) {
            for (final Proxy nativeProxy : plist) {
                try {
                    final HTTPProxy proxy = fromNativeProxy(nativeProxy);
                    if (proxy != null) {
                        ret.add(proxy);
                    }
                } catch (Throwable e) {
                    e.printStackTrace();
                }
            }
        }
        return ret;
    }
}
