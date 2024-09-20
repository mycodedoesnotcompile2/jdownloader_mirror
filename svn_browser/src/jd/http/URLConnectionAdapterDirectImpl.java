package jd.http;

import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.net.InetAddress;
import java.net.SocketAddress;
import java.net.URL;

import jd.http.requests.PostFormDataRequest;
import jd.http.requests.PostRequest;
import jd.parser.Regex;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.httpconnection.HTTPConnection;
import org.appwork.utils.net.httpconnection.HTTPConnectionImpl;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.SSLSocketStreamInterface;
import org.appwork.utils.net.httpconnection.SSLSocketStreamOptions;
import org.appwork.utils.net.httpconnection.SocketStreamInterface;
import org.appwork.utils.net.socketconnection.SocketConnection;

public class URLConnectionAdapterDirectImpl extends HTTPConnectionImpl implements URLConnectionAdapter {
    /** The request. */
    private Request request;

    /**
     * constructor
     *
     * @param url
     *            the {@link URL}
     */
    public URLConnectionAdapterDirectImpl(final URL url) {
        super(url);
    }

    private boolean allResponseCodesAllowed = false;

    public void setAllResponseCodesAllowed(boolean b) {
        this.allResponseCodesAllowed = b;
    }

    public boolean isAllResponseCodesAllowed() {
        return this.allResponseCodesAllowed;
    }

    @Override
    protected boolean isResponseCodeAllowed(final int code) {
        return this.isAllResponseCodesAllowed() || super.isResponseCodeAllowed(code);
    }

    @Override
    protected SSLSocketStreamOptions getSSLSocketStreamOptions(SSLSocketStreamOptions options) {
        final Request request = this.getRequest();
        if (request != null) {
            return request.getSSLSocketStreamOptions(options, this);
        } else {
            return options;
        }
    }

    /**
     * constructor
     *
     * @param url
     *            the {@link URL}
     * @param proxy
     *            the {@link HTTPProxy}
     */
    public URLConnectionAdapterDirectImpl(final URL url, final HTTPProxy proxy) {
        super(url, proxy);
    }

    /** {@inheritDoc} */
    @Override
    public InputStream getErrorStream() {
        try {
            return super.getInputStream();
        } catch (final IOException e) {
            return null;
        }
    }

    @Override
    public boolean isLegacyConnectEnabled() {
        return false;
    }

    @Override
    public long[] getRange() {
        final long[] ret = super.getRange();
        if (ret == null && this.getResponseCode() == 206) {
            this.ranges = URLConnectionAdapterDirectImpl.buildFakeContentRange(this);
            return this.ranges;
        }
        return ret;
    }

    /**
     * fakes the content-range for servers that don't send it in response but content-length matches requested range and response-code
     * equals 206
     *
     * @param connection
     * @return
     */
    protected static long[] buildFakeContentRange(HTTPConnection connection) {
        if (connection.getResponseCode() == 206 && connection.getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_RANGE) == null) {
            final long contentLength = connection.getContentLength();
            final String rangeRequested = connection.getRequestProperty(HTTPConstants.HEADER_REQUEST_RANGE);
            if (rangeRequested != null && contentLength >= 0) {
                final String fromByte = new Regex(rangeRequested, "bytes\\s*=\\s*(\\d*)\\s*-").getMatch(0);
                final String toByte = new Regex(rangeRequested, "bytes\\s*=\\s*.*?-\\s*(\\d*)").getMatch(0);
                if (StringUtils.isNotEmpty(fromByte)) {
                    if (StringUtils.isNotEmpty(toByte)) {
                        if (contentLength == Long.parseLong(toByte) - Long.parseLong(fromByte) + 1) {
                            return new long[] { Long.parseLong(fromByte), Long.parseLong(toByte), -1 };
                        }
                    } else {
                        final long from = Long.parseLong(fromByte);
                        final long to = from + contentLength - 1;
                        return new long[] { from, to, to + 1 };
                    }
                }
            }
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public long getLongContentLength() {
        return this.getContentLength();
    }

    @Override
    protected boolean isRequiresOutputStream() {
        return super.isRequiresOutputStream() || this.request != null && this.request.requireOutputStream();
    }

    /** {@inheritDoc} */
    @Override
    public Request getRequest() {
        return this.request;
    }

    /** {@inheritDoc} */
    @Override
    public void setRequest(final Request request) {
        this.request = request;
        if (request != null) {
            final InetAddress customInetAddress = request.getCustomInetAddress();
            if (customInetAddress != null) {
                this.setRemoteIPs(new InetAddress[] { customInetAddress });
            }
        }
    }

    @Override
    protected boolean isKeepAliveOK() {
        return (this.request == null || this.request.isKeepAlivePermitted(this)) && super.isKeepAliveOK();
    }

    @Override
    public InputStream getInputStream() throws IOException {
        if (this.convertedInputStream == null && !RequestMethod.HEAD.equals(this.getRequestMethod())) {
            super.getInputStream();
            if (!this.isContentDecoded()) {
                final String encoding = this.getHeaderField("Content-Encoding");
                if ("br".equalsIgnoreCase(encoding)) {
                    this.convertedInputStream = new WrappedBrotliInputStream(this.convertedInputStream);
                    this.contentDecoded = true;
                }
            }
        }
        return super.getInputStream();
    }

    @Override
    public InputStream setInputStream(InputStream is) throws IOException {
        if (is == null) {
            throw new IllegalArgumentException();
        }
        InputStream ret = this.convertedInputStream;
        if (ret == null) {
            ret = this.getInputStream();
        }
        if (is == ret) {
            return is;
        }
        this.convertedInputStream = is;
        return ret;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder(300);
        final Request req = this.getRequest();
        if (req != null) {
            sb.append("Caller: " + req.getCaller());
            sb.append(URLConnectionAdapter.CRLF);
            sb.append("BrowserID:" + req.getBrowserID() + "|BrowserParentID:" + req.getBrowserParentID() + "|RequestID:" + req.getRequestID() + "|URL:" + req.getURL());
            sb.append(URLConnectionAdapter.CRLF);
        }
        sb.append(this.getRequestInfo());
        if (req != null) {
            if (req instanceof PostRequest) {
                final String log = ((PostRequest) req).log();
                if (log != null) {
                    sb.append(URLConnectionAdapter.CRLF);
                    sb.append(log);
                }
            } else if (req instanceof PostFormDataRequest) {
                final String postDataString = ((PostFormDataRequest) req).getPostDataString();
                if (postDataString != null) {
                    sb.append(URLConnectionAdapter.CRLF);
                    sb.append(postDataString);
                }
            }
            sb.append(URLConnectionAdapter.CRLF);
        }
        sb.append(this.getResponseInfo());
        return sb.toString();
    }

    protected SocketAddress endPointSocketAddress;

    @Override
    public void disconnect() {
        try {
            this.getEndPointSocketAddress();
        } finally {
            super.disconnect();
        }
    }

    @Override
    public SocketAddress getEndPointSocketAddress() {
        if (this.endPointSocketAddress == null) {
            this.endPointSocketAddress = SocketConnection.getRootEndPointSocketAddress(this.getConnectionSocket());
        }
        return this.endPointSocketAddress;
    }

    @Override
    public String getCipherSuite() {
        final SocketStreamInterface socket = this.getConnectionSocket();
        if (socket != null && socket instanceof SSLSocketStreamInterface) {
            return ((SSLSocketStreamInterface) socket).getCipherSuite();
        } else {
            return null;
        }
    }

    private PushbackInputStream pushbackInputStream = null;

    @Override
    protected void resetConnection() {
        super.resetConnection();
        this.pushbackInputStream = null;
    }
}
