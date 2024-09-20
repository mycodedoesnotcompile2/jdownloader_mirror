package jd.http;

import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.SocketAddress;
import java.net.URL;

import jd.http.requests.PostFormDataRequest;
import jd.http.requests.PostRequest;

import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.HTTPProxyHTTPConnectionImpl;
import org.appwork.utils.net.httpconnection.SSLSocketStreamInterface;
import org.appwork.utils.net.httpconnection.SSLSocketStreamOptions;
import org.appwork.utils.net.httpconnection.SocketStreamInterface;
import org.appwork.utils.net.socketconnection.SocketConnection;

public class URLConnectionAdapterHTTPProxyImpl extends HTTPProxyHTTPConnectionImpl implements URLConnectionAdapter {
    /** The request. */
    private Request request;

    /**
     * constructor
     *
     * @param url
     *            the {@link URL}
     * @param proxy
     *            the {@link HTTPProxy}
     */
    public URLConnectionAdapterHTTPProxyImpl(final URL url, final HTTPProxy proxy) {
        super(url, proxy);
    }

    @Override
    public boolean isLegacyConnectEnabled() {
        return false;
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
    public long[] getRange() {
        final long[] ret = super.getRange();
        if (ret == null && this.getResponseCode() == 206) {
            this.ranges = URLConnectionAdapterDirectImpl.buildFakeContentRange(this);
            return this.ranges;
        }
        return ret;
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

    /** {@inheritDoc} */
    @Override
    public long getLongContentLength() {
        return this.getContentLength();
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
                this.setCustomEndPointInetAddress(customInetAddress);
            }
        }
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

    @Override
    protected boolean isRequiresOutputStream() {
        return super.isRequiresOutputStream() || this.request != null && this.request.requireOutputStream();
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
    public String getCipherSuite() {
        final SocketStreamInterface socket = this.getConnectionSocket();
        if (socket != null && socket instanceof SSLSocketStreamInterface) {
            return ((SSLSocketStreamInterface) socket).getCipherSuite();
        } else {
            return null;
        }
    }

    @Override
    public SocketAddress getEndPointSocketAddress() {
        if (this.endPointSocketAddress == null) {
            this.endPointSocketAddress = SocketConnection.getRootEndPointSocketAddress(this.getConnectionSocket());
        }
        return this.endPointSocketAddress;
    }
}
