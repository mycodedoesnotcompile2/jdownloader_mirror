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

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.Authenticator;
import java.net.HttpURLConnection;
import java.net.PasswordAuthentication;
import java.net.ProtocolException;
import java.net.Proxy;
import java.net.ProxySelector;
import java.net.SocketAddress;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.WeakHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSession;

import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.Exceptions;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.net.CountingInputStream;
import org.appwork.utils.net.EmptyInputStream;
import org.appwork.utils.net.NullOutputStream;

/**
 * @author daniel
 *
 */
public class NativeHTTPConnectionImpl implements HTTPConnection {
    protected final static ProxySelector                DEFAULT_PROXY_SELECTOR      = ProxySelector.getDefault();
    protected final URL                                 httpURL;
    protected final HTTPProxy                           proxy;
    protected final HTTPHeaderMap<String>               requestProperties;
    protected final HTTPHeaderMap<List<String>>         headers;
    protected HttpURLConnection                         con;
    protected int                                       readTimeout                 = 30000;
    protected int                                       connectTimeout              = 30000;
    private int[]                                       allowedResponseCodes        = new int[0];
    protected long                                      postTodoLength              = -1;
    protected RequestMethod                             httpMethod                  = RequestMethod.GET;
    protected HTTPOutputStream                          outputStream                = null;
    protected InputStream                               inputStream                 = null;
    protected InputStream                               convertedInputStream        = null;
    protected boolean                                   inputStreamConnected        = false;
    protected boolean                                   outputClosed                = false;
    protected int                                       httpResponseCode            = -1;
    protected String                                    httpResponseMessage         = "";
    protected String                                    customcharset               = null;
    protected long                                      requestTime                 = -1;
    protected long[]                                    ranges;
    protected boolean                                   contentDecoded              = true;
    private boolean                                     connected                   = false;
    private boolean                                     wasConnected                = false;
    private boolean                                     sslTrustALL                 = false;
    private boolean                                     legacyConnectFlag           = true;
    protected final CopyOnWriteArrayList<String>        connectExceptions           = new CopyOnWriteArrayList<String>();
    private final static WeakHashMap<Thread, HTTPProxy> CURRENT_THREAD_PROXY_AUTH   = new WeakHashMap<Thread, HTTPProxy>();
    private final static WeakHashMap<Thread, Proxy>     CURRENT_THREAD_NATIVE_PROXY = new WeakHashMap<Thread, Proxy>();
    private static boolean                              proxySelectorSet            = false;
    static {
        // JDK1.8_111, java disabled basic auth
        // https://www.oracle.com/technetwork/java/javase/8u111-relnotes-3124969.html
        // jdk.http.auth.tunneling.disabledSchemes="" and jdk.http.auth.proxying.disabledSchemes=""
        try {
            Authenticator.setDefault(new Authenticator() {
                @Override
                protected PasswordAuthentication getPasswordAuthentication() {
                    HTTPProxy foundProxy = null;
                    synchronized (NativeHTTPConnectionImpl.CURRENT_THREAD_PROXY_AUTH) {
                        foundProxy = NativeHTTPConnectionImpl.CURRENT_THREAD_PROXY_AUTH.remove(Thread.currentThread());
                    }
                    if (foundProxy != null) {
                        String user = foundProxy.getUser();
                        String pass = foundProxy.getPass();
                        if (user != null || pass != null) {
                            if (StringUtils.isEmpty(user)) {
                                user = "";
                            }
                            if (StringUtils.isEmpty(pass)) {
                                pass = "";
                            }
                            return new PasswordAuthentication(user, pass.toCharArray());
                        }
                    }
                    return null;
                }
            });
        } catch (final Throwable e) {
            e.printStackTrace();
        }
        try {
            ProxySelector.setDefault(new ProxySelector() {
                @Override
                public List<Proxy> select(URI uri) {
                    final Proxy nativeProxy;
                    synchronized (NativeHTTPConnectionImpl.CURRENT_THREAD_NATIVE_PROXY) {
                        if (!CURRENT_THREAD_NATIVE_PROXY.containsKey(Thread.currentThread()) && DEFAULT_PROXY_SELECTOR != null) {
                            return DEFAULT_PROXY_SELECTOR.select(uri);
                        }
                        nativeProxy = NativeHTTPConnectionImpl.CURRENT_THREAD_NATIVE_PROXY.get(Thread.currentThread());
                    }
                    final List<Proxy> ret = new ArrayList<Proxy>();
                    if (nativeProxy != null) {
                        ret.add(nativeProxy);
                    } else {
                        ret.add(Proxy.NO_PROXY);
                    }
                    return ret;
                }

                @Override
                public void connectFailed(URI uri, SocketAddress sa, IOException ioe) {
                    if (DEFAULT_PROXY_SELECTOR != null) {
                        DEFAULT_PROXY_SELECTOR.connectFailed(uri, sa, ioe);
                    }
                }
            });
            proxySelectorSet = true;
        } catch (final Throwable e) {
            e.printStackTrace();
        }
    }

    protected boolean appendPortToHostHeaderonCONNECT(HTTPProxy p) {
        // some proxy implementation might expect 'HOST: host:port' header
        return false;
    }

    public NativeHTTPConnectionImpl(final URL url) {
        this(url, null);
    }

    protected String getExceptionMessage(Exception e) {
        if (e == null) {
            return null;
        } else if (e.getMessage() != null) {
            return e.getClass().getName() + "(" + e.getMessage() + ")";
        } else {
            return e.getClass().getName();
        }
    }

    /**
     * workaround to always add port to host header as there are known proxy implementations that expect it
     *
     * @param url
     * @param p
     * @return
     */
    protected URL getProxyConnectHostHeaderWorkaroundURL(final URL url, final HTTPProxy p) {
        if (appendPortToHostHeaderonCONNECT(p) && JVMVersion.get() <= JVMVersion.JAVA18 && p != null && HTTPProxy.TYPE.HTTP.equals(p.getType()) && StringUtils.equalsIgnoreCase(url.getProtocol(), "https")) {
            try {
                final int hostPort = url.getPort() != -1 ? url.getPort() : url.getDefaultPort();
                final Field portField = url.getClass().getDeclaredField("port");
                portField.setAccessible(true);
                final Field handlerField = url.getClass().getDeclaredField("handler");
                handlerField.setAccessible(true);
                final URLStreamHandler handler = (URLStreamHandler) handlerField.get(url);
                final URL ret = new URL(null, url.toExternalForm(), new URLStreamHandler() {
                    @Override
                    protected int getDefaultPort() {
                        if (Exceptions.getStackTrace(new Exception()).contains("sendCONNECT")) {
                            // dirty hack to enforce adding port to host header
                            final int ret = hostPort + 1;
                            return ret;
                        } else {
                            return url.getDefaultPort();
                        }
                    }

                    @Override
                    protected URLConnection openConnection(URL u) throws IOException {
                        try {
                            final Method openConnection = handler.getClass().getDeclaredMethod("openConnection", new Class[] { URL.class });
                            openConnection.setAccessible(true);
                            return (URLConnection) openConnection.invoke(handler, u);
                        } catch (Exception e) {
                            throw new IOException(e);
                        }
                    }

                    @Override
                    protected URLConnection openConnection(URL u, Proxy p) throws IOException {
                        try {
                            final Method openConnection = handler.getClass().getDeclaredMethod("openConnection", new Class[] { URL.class, Proxy.class });
                            openConnection.setAccessible(true);
                            return (URLConnection) openConnection.invoke(handler, u, p);
                        } catch (Exception e) {
                            throw new IOException(e);
                        }
                    }
                });
                portField.set(ret, hostPort);
                return ret;
            } catch (final Throwable e) {
                e.printStackTrace();
            }
        }
        return url;
    }

    public NativeHTTPConnectionImpl(final URL url, final HTTPProxy proxy) {
        this.httpURL = getProxyConnectHostHeaderWorkaroundURL(url, proxy);
        this.proxy = isProxySupported(proxy);
        this.requestProperties = new HTTPHeaderMap<String>();
        this.headers = new HTTPHeaderMap<List<String>>();
        if (proxy != null) {
            switch (proxy.getType()) {
            case HTTP:
            case HTTPS:
                this.setRequestProperty(HTTPConstants.HEADER_REQUEST_PROXY_CONNECTION, "close");
                break;
            default:
                break;
            }
        }
    }

    protected HTTPProxy isProxySupported(HTTPProxy proxy) {
        if (proxy == null) {
            return null;
        } else if (proxy.toNativeProxy() == null) {
            throw new IllegalArgumentException("Unsupported ProxyType " + this.proxy.getType());
        } else {
            return proxy;
        }
    }

    protected String getSSLSocketStreamOptionsID(final String host, final int port, final boolean trustAllFlag) {
        return host + ":" + port + ":" + trustAllFlag;
    }

    protected SSLSocketStreamOptions getNewSSLSocketStreamOptionsInstance(String host, int port, boolean trustAllFlag) {
        final String id = getSSLSocketStreamOptionsID(host, port, trustAllFlag);
        final SSLSocketStreamOptions ret = new SSLSocketStreamOptions(id, trustAllFlag) {
            @Override
            protected void initCipherSuitesLists() {
            }
        };
        return getSSLSocketStreamOptions(ret);
    }

    protected SSLSocketStreamOptions getSSLSocketStreamOptions(SSLSocketStreamOptions options) {
        return options;
    }

    protected void setRequestMethod(HttpURLConnection con, RequestMethod requestMethod) throws ProtocolException {
        try {
            con.setRequestMethod(requestMethod.name());
        } catch (ProtocolException e) {
            try {
                final Field field = ReflectionUtils.getField(HttpURLConnection.class, "method", con, String.class);
                field.setAccessible(true);
                field.set(con, requestMethod.name());
            } catch (Throwable e2) {
                throw Exceptions.addSuppressed(e, e2);
            }
        }
    }

    private static SSLSocketStreamFactory defaultSSLSocketStreamFactory = null;

    public static void setDefaultSSLSocketStreamFactory(SSLSocketStreamFactory defaultSSLSocketStreamFactory) {
        NativeHTTPConnectionImpl.defaultSSLSocketStreamFactory = defaultSSLSocketStreamFactory;
    }

    public static SSLSocketStreamFactory getDefaultSSLSocketStreamFactory() {
        final SSLSocketStreamFactory ret = defaultSSLSocketStreamFactory;
        if (ret != null) {
            return ret;
        } else {
            return NativeSSLSocketStreamFactory.getInstance();
        }
    }

    protected SSLSocketStreamFactory getSSLSocketStreamFactory(final SSLSocketStreamOptions sslSocketStreamOptions) {
        final SSLSocketStreamFactory ret = sslSocketStreamOptions != null ? sslSocketStreamOptions.getSSLSocketStreamFactory() : null;
        if (ret != null) {
            return ret;
        } else {
            return getDefaultSSLSocketStreamFactory();
        }
    }

    @Override
    public void connect() throws IOException {
        if (this.isConnected()) {
            return;/* oder fehler */
        }
        this.wasConnected = false;
        final long startTime = Time.systemIndependentCurrentJVMTimeMillis();
        final Proxy nativeProxy = proxy != null ? proxy.toNativeProxy() : null;
        try {
            if (nativeProxy != null) {
                synchronized (NativeHTTPConnectionImpl.CURRENT_THREAD_PROXY_AUTH) {
                    CURRENT_THREAD_PROXY_AUTH.put(Thread.currentThread(), this.proxy);
                }
                /** http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6626700 **/
                /**
                 * Request for ability to turn off authentication caching in HttpURLConnection
                 **/
                org.appwork.sunwrapper.sun.net.www.protocol.http.AuthCacheValueWrapper.setAuthCacheImpl();
                if (proxySelectorSet && (HTTPProxy.TYPE.SOCKS4.equals(proxy.getType()) || HTTPProxy.TYPE.SOCKS4A.equals(proxy.getType()))) {
                    // only ProxySelector.select provides special flow/support for sun.net.SocksProxy in java.net.SocksSocketImpl
                    synchronized (NativeHTTPConnectionImpl.CURRENT_THREAD_NATIVE_PROXY) {
                        CURRENT_THREAD_NATIVE_PROXY.put(Thread.currentThread(), nativeProxy);
                    }
                    this.con = (HttpURLConnection) this.httpURL.openConnection();
                } else {
                    this.con = (HttpURLConnection) this.httpURL.openConnection(nativeProxy);
                }
            } else {
                synchronized (NativeHTTPConnectionImpl.CURRENT_THREAD_PROXY_AUTH) {
                    NativeHTTPConnectionImpl.CURRENT_THREAD_PROXY_AUTH.remove(Thread.currentThread());
                }
                this.con = (HttpURLConnection) this.httpURL.openConnection();
            }
            if (this.con instanceof HttpsURLConnection) {
                final HttpsURLConnection scon = (HttpsURLConnection) this.con;
                final boolean trustAll = isSSLTrustALL();
                final String urlHost = httpURL.getHost();
                final int hostPort = httpURL.getPort() != -1 ? httpURL.getPort() : httpURL.getDefaultPort();
                // TODO: add support for SSLSocketStreamOptions cache/retry
                final SSLSocketStreamOptions options = getNewSSLSocketStreamOptionsInstance(urlHost, hostPort, trustAll);
                final SSLSocketStreamFactory nativeSSLSocketStreamFactory = getSSLSocketStreamFactory(options);
                scon.setSSLSocketFactory(nativeSSLSocketStreamFactory.getSSLSocketFactory(options, urlHost));
                final HostnameVerifier hostNameVerifier = scon.getHostnameVerifier();
                scon.setHostnameVerifier(new HostnameVerifier() {
                    @Override
                    public boolean verify(String host, SSLSession sslSession) {
                        if (trustAll) {
                            return true;
                        } else {
                            try {
                                return Boolean.TRUE.equals(HTTPConnectionUtils.verifySSLHostname(hostNameVerifier, sslSession, host));
                            } catch (IOException e) {
                                connectExceptions.add(getExceptionMessage(e));
                                return false;
                            }
                        }
                    }
                });
            }
            this.con.setConnectTimeout(this.connectTimeout);
            this.con.setReadTimeout(this.readTimeout);
            setRequestMethod(con, getRequestMethod());
            this.con.setAllowUserInteraction(false);
            this.con.setInstanceFollowRedirects(false);
            if (this.isRequiresOutputStream()) {
                this.con.setDoOutput(true);
            } else {
                this.outputClosed = true;
                this.con.setDoOutput(false);
            }
            final Iterator<Entry<String, String>> it = this.requestProperties.entrySet().iterator();
            while (it.hasNext()) {
                final Entry<String, String> next = it.next();
                if (next.getValue() == null) {
                    continue;
                }
                if (StringUtils.equalsIgnoreCase("Content-Length", next.getKey())) {
                    /* content length to check if we send out all data */
                    this.postTodoLength = Long.parseLong(next.getValue().trim());
                }
                this.con.setRequestProperty(next.getKey(), next.getValue());
            }
            this.con.connect();
            this.connected = true;
            this.wasConnected = true;
            this.requestTime = Time.systemIndependentCurrentJVMTimeMillis() - startTime;
            if (this.isRequiresOutputStream() == false) {
                this.outputStream = new HTTPOutputStream(new NullOutputStream());
                this.outputClosed = true;
                this.connectInputStream();
            } else {
                this.outputStream = new HTTPOutputStream(this.con.getOutputStream());
            }
        } finally {
            if (proxySelectorSet) {
                synchronized (NativeHTTPConnectionImpl.CURRENT_THREAD_NATIVE_PROXY) {
                    CURRENT_THREAD_NATIVE_PROXY.remove(Thread.currentThread());
                }
            }
        }
    }

    protected synchronized void connectInputStream() throws IOException {
        if (this.isRequiresOutputStream()) {
            final long done = this.outputStream.transferedBytes();
            if (done != this.postTodoLength) {
                throw new IOException("Content-Length " + this.postTodoLength + " does not match send " + done + " bytes");
            }
        }
        if (this.inputStreamConnected) {
            return;
        }
        if (this.isRequiresOutputStream()) {
            /* flush outputstream in case some buffers are not flushed yet */
            this.outputStream.flush();
        }
        IOException inputException = null;
        try {
            this.inputStream = this.con.getInputStream();
        } catch (final IOException e) {
            inputException = e;
            this.inputStream = this.con.getErrorStream();
        }
        this.inputStreamConnected = true;
        this.httpResponseCode = this.con.getResponseCode();
        this.httpResponseMessage = this.con.getResponseMessage();
        final Iterator<Entry<String, List<String>>> it = this.con.getHeaderFields().entrySet().iterator();
        while (it.hasNext()) {
            final Entry<String, List<String>> next = it.next();
            final String key = next.getKey();
            final List<String> value = next.getValue();
            List<String> list = this.headers.get(key);
            if (list == null) {
                list = new ArrayList<String>();
                this.headers.put(key, list);
            }
            list.addAll(value);
        }
        if (this.inputStream == null && inputException != null) {
            if (this.getContentLength() == 0) {
                this.inputStream = new InputStream() {
                    @Override
                    public int read() throws IOException {
                        return -1;
                    }
                };
            } else {
                /* in case we dont have an error Stream */
                throw inputException;
            }
        }
    }

    @Override
    public void disconnect() {
        this.disconnect(false);
    }

    public void disconnect(final boolean freeConnection) {
        try {
            this.con.disconnect();
        } catch (final Throwable e) {
        } finally {
            this.connected = false;
            if (freeConnection) {
                this.con = null;
            }
        }
    }

    @Override
    public void finalizeConnect() throws IOException {
        this.connect();
        this.connectInputStream();
    }

    @Override
    public int[] getAllowedResponseCodes() {
        return this.allowedResponseCodes;
    }

    @Override
    public String getCharset() {
        int i;
        if (this.customcharset != null) {
            return this.customcharset;
        }
        return this.getContentType() != null && (i = this.getContentType().toLowerCase().indexOf("charset=")) > 0 ? this.getContentType().substring(i + 8).trim() : null;
    }

    @Override
    public long getCompleteContentLength() {
        final long[] range = this.getRange();
        if (range != null) {
            return range[2];
        } else {
            return this.getContentLength();
        }
    }

    public long getContentLength() {
        final String length = this.getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH);
        if (length != null && length.trim().matches("^\\d+$")) {
            return Long.parseLong(length.trim());
        } else {
            return -1;
        }
    }

    @Override
    public String getContentType() {
        final String type = this.getHeaderField("Content-Type");
        if (type == null) {
            return "unknown";
        } else {
            return type;
        }
    }

    @Override
    public String getHeaderField(final String string) {
        final List<String> ret = this.headers.get(string);
        if (ret == null || ret.size() == 0) {
            return null;
        } else {
            return ret.get(0);
        }
    }

    @Override
    public Map<String, List<String>> getHeaderFields() {
        return this.headers;
    }

    @Override
    public List<String> getHeaderFields(final String string) {
        final List<String> ret = this.headers.get(string);
        if (ret == null || ret.size() == 0) {
            return null;
        }
        return ret;
    }

    protected InputStream getRawInputStream() {
        return inputStream;
    }

    @Override
    public InputStream getInputStream() throws IOException {
        if (!isLegacyConnectEnabled() && !this.isConnected()) {
            throw new IllegalStateException("not connected!");
        }
        this.connect();
        this.connectInputStream();
        final int code = this.getResponseCode();
        if (code == 407) {
            /* auth invalid/missing */
            throw new ProxyAuthException(this.proxy);
        } else if (this.isOK() || code == 404 || code == 403 || code == 416 || code == 401) {
            if (this.convertedInputStream == null) {
                final InputStream rawInputStream = getRawInputStream();
                if (this.contentDecoded && !RequestMethod.HEAD.equals(this.getRequestMethod())) {
                    if (getContentLength() == 0) {
                        // Content-Length is 0, return EmptyInputStream
                        this.convertedInputStream = new EmptyInputStream();
                        this.contentDecoded = false;
                    } else {
                        /**
                         * java.net.HttpURLConnection transparently handles Content-Transfer-Encoding as it already handles chunked
                         * Transfer-Encoding
                         *
                         */
                        /* we convert different content-encodings to normal inputstream */
                        final String encoding = this.getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_ENCODING);
                        if (encoding == null || encoding.length() == 0 || "none".equalsIgnoreCase(encoding) || "identity".equalsIgnoreCase(encoding)) {
                            /* no encoding */
                            this.convertedInputStream = new CountingInputStream(rawInputStream);
                            this.contentDecoded = false;
                        } else if ("gzip".equalsIgnoreCase(encoding) || "x-gzip".equalsIgnoreCase(encoding)) {
                            /* gzip encoding */
                            this.convertedInputStream = new CountingGZIPInputStream(rawInputStream);
                            this.contentDecoded = true;
                        } else if ("deflate".equalsIgnoreCase(encoding) || "x-deflate".equalsIgnoreCase(encoding)) {
                            /* deflate encoding */
                            this.convertedInputStream = new CountingInflaterInputStream(new CountingInputStream(rawInputStream));
                            this.contentDecoded = true;
                        } else {
                            /* unsupported */
                            this.convertedInputStream = new CountingInputStream(rawInputStream);
                            this.contentDecoded = false;
                        }
                    }
                } else {
                    /* use original inputstream */
                    this.convertedInputStream = new CountingInputStream(rawInputStream);
                    this.contentDecoded = false;
                }
            }
            return this.convertedInputStream;
        } else {
            throw new IOException(this.getResponseCode() + " " + this.getResponseMessage());
        }
    }

    @Override
    public HTTPOutputStream getOutputStream() throws IOException {
        if (this.outputStream != null && this.isRequiresOutputStream()) {
            return outputStream;
        } else {
            throw new IOException("OutputStream is not available");
        }
    }

    @Override
    public long[] getRange() {
        if (this.ranges == null) {
            this.ranges = HTTPConnectionUtils.parseContentRange(this);
        }
        return this.ranges;
    }

    protected String getRequestInfo() {
        final StringBuilder sb = new StringBuilder();
        sb.append("----------------(Native)Request Information-------------\r\n");
        sb.append("URL: ").append(this.getURL()).append("\r\n");
        sb.append("Host: ").append(this.getURL().getHost()).append("\r\n");
        if (proxy != null) {
            sb.append("Proxy: ").append(proxy.toString()).append("\r\n");
        }
        sb.append("Connection-Timeout: ").append(this.connectTimeout + "ms").append("\r\n");
        sb.append("Read-Timeout: ").append(this.readTimeout + "ms").append("\r\n");
        if (this.connectExceptions.size() > 0) {
            sb.append("----------------ConnectionExceptions-------------------------\r\n");
            int index = 0;
            for (String connectException : this.connectExceptions) {
                sb.append(index++).append(":").append(connectException).append("\r\n");
            }
        }
        sb.append("----------------(Native)Request-------------------------\r\n");
        if (this.isConnected() || this.wasConnected()) {
            sb.append(this.httpMethod.toString()).append(' ').append(this.getURL().getPath()).append(" HTTP/1.1\r\n");
            final Iterator<Entry<String, String>> it = this.getRequestProperties().entrySet().iterator();
            while (it.hasNext()) {
                final Entry<String, String> next = it.next();
                if (next.getValue() == null) {
                    continue;
                }
                sb.append(next.getKey());
                sb.append(": ");
                sb.append(next.getValue());
                sb.append("\r\n");
            }
        } else {
            sb.append("-------------Not Connected Yet!-----------------\r\n");
        }
        return sb.toString();
    }

    @Override
    public RequestMethod getRequestMethod() {
        return this.httpMethod;
    }

    @Override
    public Map<String, String> getRequestProperties() {
        return this.requestProperties;
    }

    @Override
    public String getRequestProperty(final String string) {
        return this.requestProperties.get(string);
    }

    @Override
    public long getRequestTime() {
        return this.requestTime;
    }

    @Override
    public int getResponseCode() {
        return this.httpResponseCode;
    }

    protected String getResponseInfo() {
        final StringBuilder sb = new StringBuilder();
        sb.append("----------------Response Information------------\r\n");
        try {
            if (this.isConnected() || this.wasConnected()) {
                sb.append("Connection-Time: ").append(this.requestTime + "ms").append("\r\n");
                sb.append("----------------Response------------------------\r\n");
                this.connectInputStream();
                sb.append(this.getRequestMethod());
                sb.append(" ");
                sb.append(this.con.getURL());
                sb.append(" ");
                sb.append(this.getResponseCode());
                sb.append(" ");
                sb.append(this.getResponseMessage());
                sb.append("\r\n");
                for (final Entry<String, List<String>> next : this.getHeaderFields().entrySet()) {
                    // Achtung cookie reihenfolge ist wichtig!!!
                    for (int i = next.getValue().size() - 1; i >= 0; i--) {
                        if (next.getKey() == null) {
                            sb.append(next.getValue().get(i));
                            sb.append("\r\n");
                        } else {
                            sb.append(next.getKey());
                            sb.append(": ");
                            sb.append(next.getValue().get(i));
                            sb.append("\r\n");
                        }
                    }
                }
                sb.append("------------------------------------------------\r\n");
            } else {
                sb.append("-------------Not Connected Yet!------------------\r\n");
            }
        } catch (final IOException nothing) {
            sb.append("----------No InputStream Available!--------------\r\n");
        }
        sb.append("\r\n");
        return sb.toString();
    }

    @Override
    public String getResponseMessage() {
        return this.httpResponseMessage;
    }

    @Override
    public URL getURL() {
        return this.httpURL;
    }

    @Override
    public boolean isConnected() {
        if (this.con != null && this.connected) {
            return true;
        }
        return false;
    }

    @Override
    public boolean isContentDecoded() {
        if (convertedInputStream == null && contentDecoded && isConnected()) {
            try {
                getInputStream();
            } catch (IOException e) {
                LogV3.log(e);
            }
        }
        return this.contentDecoded;
    }

    @Override
    public boolean isContentDisposition() {
        return this.getHeaderField("Content-Disposition") != null;
    }

    @Override
    public boolean isOK() {
        final int code = this.getResponseCode();
        if (code >= 200 && code < 400) {
            return true;
        }
        if (this.isResponseCodeAllowed(code)) {
            return true;
        }
        return false;
    }

    protected boolean isResponseCodeAllowed(final int code) {
        for (final int c : this.allowedResponseCodes) {
            if (c == code || c == -1) {
                return true;
            }
        }
        return false;
    }

    protected boolean isRequiresOutputStream() {
        return httpMethod.requiresOutputStream;
    }

    @Override
    public void setAllowedResponseCodes(final int[] codes) {
        if (codes == null) {
            throw new IllegalArgumentException("codes==null");
        }
        this.allowedResponseCodes = codes;
    }

    @Override
    public void setCharset(final String Charset) {
        this.customcharset = Charset;
    }

    @Override
    public void setConnectTimeout(final int connectTimeout) {
        this.connectTimeout = connectTimeout;
    }

    @Override
    public void setContentDecoded(final boolean b) {
        if (this.convertedInputStream != null) {
            throw new IllegalStateException("InputStream already in use!");
        }
        this.contentDecoded = b;
    };

    @Override
    public void setReadTimeout(final int readTimeout) {
        try {
            this.readTimeout = Math.max(0, readTimeout);
            if (this.isConnected()) {
                this.con.setReadTimeout(this.readTimeout);
            }
        } catch (final Throwable e) {
            e.printStackTrace();
        }
    }

    public HTTPProxy getProxy() {
        return this.proxy;
    }

    @Override
    public void setRequestMethod(final RequestMethod method) {
        this.httpMethod = method;
    }

    @Override
    public void setRequestProperty(final String key, final String value) {
        this.requestProperties.put(key, value);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append(this.getRequestInfo());
        sb.append(this.getResponseInfo());
        return sb.toString();
    }

    /**
     * @return
     */
    private boolean wasConnected() {
        return this.wasConnected;
    }

    @Override
    public void setSSLTrustALL(boolean trustALL) {
        this.sslTrustALL = trustALL;
    }

    @Override
    public boolean isSSLTrustALL() {
        return this.sslTrustALL;
    }

    @Override
    public void setLegacyConnectEnabled(boolean enabled) {
        this.legacyConnectFlag = enabled;
    }

    @Override
    public boolean isLegacyConnectEnabled() {
        return legacyConnectFlag;
    }

    /**
     * @see org.appwork.utils.net.httpconnection.HTTPConnection#setProfiler(org.appwork.utils.net.httpconnection.HTTPConnectionProfilerInterface)
     */
    @Override
    public void setProfiler(HTTPConnectionProfilerInterface profiler) {
    }

    /**
     * @see org.appwork.utils.net.httpconnection.HTTPConnection#getProfiler()
     */
    @Override
    public HTTPConnectionProfilerInterface getProfiler() {
        return null;
    }
}
