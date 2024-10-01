/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Spaltestraße 58
 *         91183 Abenberg
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
package org.appwork.utils.net.httpclient;

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.UnknownHostException;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.concurrent.CopyOnWriteArrayList;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.txtresource.TranslationFactory;
import org.appwork.utils.Application;
import org.appwork.utils.Exceptions;
import org.appwork.utils.IO;
import org.appwork.utils.Interruptible;
import org.appwork.utils.InterruptibleThread;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.ChunkedOutputStream;
import org.appwork.utils.net.CountingConnection;
import org.appwork.utils.net.CountingInputStream;
import org.appwork.utils.net.DownloadProgress;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.net.BasicHTTP.ReadIOException;
import org.appwork.utils.net.BasicHTTP.WriteIOException;
import org.appwork.utils.net.httpconnection.HTTPConnection;
import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;
import org.appwork.utils.net.httpconnection.HTTPConnectionFactory;
import org.appwork.utils.net.httpconnection.HTTPConnectionProfilerAdapter;
import org.appwork.utils.net.httpconnection.HTTPOutputStream;
import org.appwork.utils.net.httpconnection.HTTPProxy;

public class HttpClient {
    // maybe add a factory System property someday
    private static final HttpClient DEFAULT_HTTP_CLIENT = new HttpClient();

    public static class RequestContext implements Interruptible {
        private Boolean             addedToInterruptible;
        private HttpClient          client;
        private HTTPConnection      connection;
        private DownloadProgress    downloadProgress;
        private CountingInputStream inputStream;
        private RequestMethod       method;
        private OutputStream        target         = new ByteArrayOutputStream();
        private int                 postDataLength;
        private InputStream         postDataStream;
        private int                 readTimeout    = 0;
        public URL                  redirectTo;
        private long                resumePosition = -1;
        private DownloadProgress    uploadProgress;
        private String              url;
        public long                 redirectsStarted;
        private volatile boolean    executed       = false;

        /**
         * @param delete
         */
        public RequestContext() {
        }

        public static RequestContext get(final String url) {
            return new RequestContext().setMethod(RequestMethod.GET).setUrl(url);
        }

        /**
         * @return
         */
        public int getCode() {
            return this.getConnection().getResponseCode();
        }

        /**
         * @return
         */
        public HTTPConnection getConnection() {
            return this.connection;
        }

        public DownloadProgress getDownloadProgress() {
            return this.downloadProgress;
        }

        public CountingInputStream getInputStream() throws HttpClientException, InterruptedException {
            this.ensureExecution();
            return this.inputStream;
        }

        public RequestMethod getMethod() {
            return this.method;
        }

        public OutputStream getTarget() {
            return this.target;
        }

        /**
         * @return
         */
        public int getPostDataLength() {
            return this.postDataLength;
        }

        /**
         * @return
         */
        public InputStream getPostDataStream() {
            return this.postDataStream;
        }

        public int getReadTimeout() {
            return this.readTimeout;
        }

        /**
         * @return
         * @throws IOException
         * @throws InterruptedException
         */
        public byte[] getResponseBytes() throws IOException, InterruptedException {
            final CountingInputStream fromContext = this.getInputStream();
            if (fromContext != null) {
                this.target = new ByteArrayOutputStream();
                IO.readStreamToOutputStream(-1, fromContext, this.target, true);
            }
            if (this.target instanceof ByteArrayOutputStream) {
                return ((ByteArrayOutputStream) this.target).toByteArray();
            }
            return null;
        }

        /**
         * @return
         * @throws IOException
         * @throws InterruptedException
         */
        public String getResponseString() throws IOException, InterruptedException {
            return this.getResponseString(null);
        }

        /**
         * @param utf8
         * @return
         * @throws IOException
         * @throws InterruptedException
         */
        public String getResponseString(Charset charset) throws IOException, InterruptedException {
            this.ensureExecution();
            if (charset == null) {
                String ct = this.getConnection().getCharset();
                if (StringUtils.isEmpty(ct)) {
                    ct = "UTF-8";
                }
                charset = Charset.forName(ct);
            }
            final CountingInputStream fromContext = this.getInputStream();
            if (fromContext != null) {
                this.target = new ByteArrayOutputStream();
                IO.readStreamToOutputStream(-1, fromContext, this.target, true);
            }
            if (this.target instanceof ByteArrayOutputStream) {
                return ((ByteArrayOutputStream) this.target).toString(charset.displayName());
            }
            return null;
        }

        /**
         * @throws InterruptedException
         * @throws HttpClientException
         *
         */
        private void ensureExecution() throws HttpClientException, InterruptedException {
            if (!this.executed) {
                this.execute();
            }
        }

        public long getResumePosition() {
            return this.resumePosition;
        }

        public DownloadProgress getUploadProgress() {
            return this.uploadProgress;
        }

        public String getUrl() {
            return this.url;
        }

        /**
         * @see org.appwork.utils.Interruptible#interrupt(java.lang.Thread)
         */
        @Override
        public void interrupt(final Thread arg0) {
            final HTTPConnection c = this.connection;
            if (c != null) {
                c.disconnect();
            }
        }

        /**
         *
         */
        public void linkInterrupt() {
            if (this.addedToInterruptible == Boolean.TRUE) {
                InterruptibleThread.remove(this);
            }
        }

        /**
         * @return
         */
        public RequestContext log() {
            LogV3.info(this + "");
            return this;
        }

        /**
         * @param bs
         */
        public void onBytesLoaded(final byte[] bytes, final int offset, final int length) {
            final DownloadProgress dl = this.getDownloadProgress();
            if (dl != null) {
                if (offset > 0) {
                    throw new WTFException("Unsupported");
                }
                dl.onBytesLoaded(bytes, length);
                dl.increaseLoaded(length);
            }
        }

        public void onConnect() throws IOException {
            final DownloadProgress ul = this.getUploadProgress();
            if (ul != null) {
                ul.onConnect(this.getConnection());
            }
            final DownloadProgress dl = this.getDownloadProgress();
            if (dl != null) {
                dl.onConnect(this.getConnection());
            }
        }

        /**
         * @throws IOException
         *
         */
        public void onConnected() throws IOException {
            final DownloadProgress ul = this.getUploadProgress();
            if (ul != null) {
                ul.onConnected(this.getConnection());
            }
            final DownloadProgress dl = this.getDownloadProgress();
            if (dl != null) {
                dl.onConnected(this.getConnection());
            }
        }

        /**
         * @param completeContentLength
         */
        public void onContentLength(final long completeContentLength) {
            final DownloadProgress dl = this.getDownloadProgress();
            if (dl != null) {
                dl.setTotal(completeContentLength);
            }
        }

        /**
         *
         */
        public void onDisconnected() {
            final DownloadProgress ul = this.getUploadProgress();
            if (ul != null) {
                ul.onDisconnected(this.getConnection());
            }
            final DownloadProgress dl = this.getDownloadProgress();
            if (dl != null) {
                dl.onDisconnected(this.getConnection());
            }
        }

        /**
         * @param resumePosition2
         */
        public void onReadingStreamStarted() {
            final DownloadProgress dl = this.getDownloadProgress();
            if (dl != null) {
                dl.setLoaded(this.getResumePosition());
            }
        }

        private void setConnection(final HTTPConnection connection) {
            this.connection = connection;
            connection.setProfiler(new HTTPConnectionProfilerAdapter() {
                @Override
                public void onDisconnected(final HTTPConnection httpConnectionImp) {
                    RequestContext.this.client.requests.remove(RequestContext.this);
                }
            });
        }

        public RequestContext setDownloadProgress(final DownloadProgress downloadProgress) {
            this.downloadProgress = downloadProgress;
            return this;
        }

        public RequestContext setMethod(final RequestMethod method) {
            this.method = method;
            return this;
        }

        public RequestContext setTarget(final OutputStream outputstream) {
            this.target = outputstream;
            return this;
        }

        public RequestContext setPostDataLength(final int postDataLength) {
            this.postDataLength = postDataLength;
            return this;
        }

        public RequestContext setPostDataStream(final InputStream postDataStream) {
            this.postDataStream = postDataStream;
            return this;
        }

        public RequestContext setReadTimeout(final int readTimeout) {
            this.readTimeout = readTimeout;
            return this;
        }

        public RequestContext setResumePosition(final long resumePosition) {
            this.resumePosition = resumePosition;
            return this;
        }

        public RequestContext setUploadProgress(final DownloadProgress uploadProgress) {
            this.uploadProgress = uploadProgress;
            return this;
        }

        public RequestContext setUrl(final String url) {
            this.url = url;
            return this;
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            try {
                return this.connection + "\r\n\r\n" + this.getResponseString(Charset.forName("UTF-8"));
            } catch (final IOException e) {
                return this.connection + "";
            } catch (final InterruptedException e) {
                Thread.currentThread().interrupt();
                return "<INTERRUPTED>" + this.connection + "";
            }
        }

        /**
         *
         */
        public void unlinkInterrupt() {
            this.addedToInterruptible = InterruptibleThread.add(this);
        }

        /**
         * @param e
         */
        public void onException(final Throwable e) {
            final DownloadProgress ul = this.getUploadProgress();
            if (ul != null) {
                ul.onException(this.connection, e);
            }
            final DownloadProgress dl = this.getDownloadProgress();
            if (dl != null) {
                ul.onException(this.connection, e);
            }
        }

        /**
         *
         */
        public void onPostStart() {
            final DownloadProgress ul = this.getUploadProgress();
            if (ul != null) {
                ul.setTotal(this.getPostDataLength());
            }
        }

        /**
         * @param buffer
         * @param i
         * @param len
         */
        public void onBytesPosted(final byte[] bytes, final int offset, final int length) {
            final DownloadProgress dl = this.getUploadProgress();
            if (dl != null) {
                if (offset > 0) {
                    throw new WTFException("Unsupported");
                }
                dl.onBytesLoaded(bytes, length);
                dl.increaseLoaded(length);
            }
        }

        /**
         * @param target
         * @return
         * @throws FileNotFoundException
         */
        public RequestContext setTarget(final File target) {
            // outputstream that is only opened if we actually want to write data
            return this.setTarget(new FilterOutputStream(null) {
                private boolean opened = false;

                @Override
                public void write(final int b) throws IOException {
                    this.open();
                    super.write(b);
                }

                private void open() throws FileNotFoundException {
                    if (!this.opened) {
                        this.opened = true;
                        System.out.println("Open " + target);
                        this.out = new BufferedOutputStream(new FileOutputStream(target));
                    }
                }

                @Override
                public void write(final byte[] b) throws IOException {
                    this.open();
                    super.write(b);
                }

                @Override
                public void write(final byte[] b, final int off, final int len) throws IOException {
                    this.open();
                    super.write(b, off, len);
                }

                @Override
                public void flush() throws IOException {
                    if (this.out == null) {
                        return;
                    }
                    this.out.flush();
                    super.flush();
                }

                @Override
                public void close() throws IOException {
                    if (this.out == null) {
                        return;
                    }
                    System.out.println("Close " + target);
                    this.out.close();
                    super.close();
                }
            });
        }

        /**
         * @return
         * @throws InterruptedException
         * @throws HttpClientException
         */
        public RequestContext execute() throws HttpClientException, InterruptedException {
            HttpClient client = this.client;
            if (client == null) {
                client = DEFAULT_HTTP_CLIENT;
            }
            return client.execute(this);
        }

        /**
         * @param target2
         * @return
         * @throws FileNotFoundException
         */
        public RequestContext target(final File target) {
            return this.setTarget(target);
        }
    }

    protected final static Charset          UTF8           = Charset.forName("UTF-8");
    protected HashSet<Integer>              allowedResponseCodes;
    protected int                           connectTimeout = 15000;
    protected LogInterface                  logger         = null;
    protected HTTPProxy                     proxy          = HTTPProxy.NONE;
    protected int                           readTimeout    = 30000;
    protected final HashMap<String, String> requestHeader;
    private final List<RequestContext>      requests       = new CopyOnWriteArrayList<RequestContext>();

    public HttpClient() {
        this.requestHeader = new HashMap<String, String>();
    }

    /**
     * @param connection
     *            TODO
     * @throws IOException
     *
     */
    protected void checkResponseCode(final RequestContext context) throws InvalidResponseCode {
        final HashSet<Integer> allowedResponseCodes = this.getAllowedResponseCodes();
        if (allowedResponseCodes != null) {
            if (allowedResponseCodes.contains(-1)) {
                // allow all
                return;
            } else if (!allowedResponseCodes.contains(context.getConnection().getResponseCode())) {
                throw new InvalidResponseCode(context);
            }
        }
    }

    public void clearRequestHeader() {
        this.getRequestHeader().clear();
    }

    /**
     * This method MUST return the raw outputstream. If you want to wrap it, like gzip, encrypt, ... you MUST overwrite the. Chunked
     * encoding MUST not get applied here as well {@link #wrapPostOutputStream(HTTPConnection, OutputStream)} method
     *
     * @param finalUploadProgress
     * @param connection
     * @param returnOutputStream
     * @return
     * @throws IOException
     * @throws InterruptedException
     * @throws UnknownHostException
     */
    protected HTTPOutputStream connect(final RequestContext context, final boolean returnOutputStream) throws IOException, InterruptedException, UnknownHostException {
        int lookupTry = 0;
        try {
            while (true) {
                try {
                    context.getConnection().connect();
                    context.onConnected();
                    if (Thread.interrupted()) {
                        throw new InterruptedException();
                    }
                    break;
                } catch (final UnknownHostException e) {
                    if (++lookupTry > 3) {
                        throw e;
                    }
                    if (Thread.interrupted()) {
                        throw new InterruptedException();
                    }
                    /* dns lookup failed, short wait and try again */
                    Thread.sleep(200);
                }
            }
        } catch (final IOException e) {
            throw new HttpClientException(context, new ReadIOException(e));
        }
        if (!returnOutputStream) {
            return null;
        } else {
            final HTTPOutputStream raw = context.getConnection().getOutputStream();
            return raw;
        }
    }

    protected HTTPConnection createHTTPConnection(final RequestContext context) throws HttpClientException {
        HTTPConnection connection;
        try {
            connection = HTTPConnectionFactory.createHTTPConnection(context.redirectTo != null ? context.redirectTo : new URL(context.getUrl()), this.getProxy());
        } catch (final MalformedURLException e) {
            throw new HttpClientException(null, e);
        }
        context.setConnection(connection);
        return connection;
    }

    /**
     * This method is not synchronized, but thread safe. do not use the getConnection method of the client, but use response.getConnection()
     * instead
     */
    public RequestContext delete(final String url) throws IOException, InterruptedException {
        return this.execute(new RequestContext().setMethod(RequestMethod.DELETE).setUrl(url));
    }

    public boolean followRedirect(final RequestContext context) throws IOException, InterruptedException {
        if (context.connection.getResponseCode() == 301 || context.connection.getResponseCode() == 302 || context.connection.getResponseCode() == 303 || context.connection.getResponseCode() == 307) {
            final String red = context.connection.getHeaderField(HTTPConstants.HEADER_RESPONSE_LOCATION);
            if (red != null) {
                if (context.connection.getResponseCode() == 302) {
                    Thread.sleep(125);
                } else {
                    Thread.sleep(250);
                }
                context.redirectTo = new URL(URLHelper.parseLocation(context.redirectTo == null ? new URL(context.getUrl()) : context.redirectTo, red));
                if (context.redirectsStarted <= 0) {
                    context.redirectsStarted = Time.systemIndependentCurrentJVMTimeMillis();
                }
                return true;
            } else {
                throw new InvalidRedirectException(context);
            }
        } else {
            return false;
        }
    }

    public RequestContext get(final String url) throws IOException, InterruptedException {
        return this.execute(new RequestContext().setMethod(RequestMethod.GET).setUrl(url));
    }

    public HashSet<Integer> getAllowedResponseCodes() {
        return this.allowedResponseCodes;
    }

    public int getConnectTimeout() {
        return this.connectTimeout;
    }

    public LogInterface getLogger() {
        return this.logger;
    }

    public HTTPProxy getProxy() {
        return this.proxy;
    }

    public int getReadTimeout() {
        return this.readTimeout;
    }

    protected long getRedirectTimeout(final URL url) {
        return (60 * 60 * 1000l);
    }

    /**
     * @return
     */
    public HashMap<String, String> getRequestHeader() {
        return this.requestHeader;
    }

    public String getRequestHeader(final String key) {
        return this.getRequestHeader().get(key);
    }

    /**
     * @param <E>
     * @param HttpClientException
     */
    private <E extends Throwable> E handleInterrupt(final E exception) throws InterruptedException, E {
        if (exception instanceof InterruptedException) {
            throw (InterruptedException) exception;
        } else if (Thread.interrupted() || exception instanceof InterruptedIOException) {
            throw Exceptions.addSuppressed(new InterruptedException("Connection Closed by Interrupt"), exception);
        } else {
            return exception;
        }
    }

    protected void log(final HTTPConnection connection) {
        final LogInterface logger = this.getLogger();
        if (logger != null && connection != null) {
            try {
                logger.info(connection.toString());
            } catch (final Throwable e) {
                this.log(e);
            }
        }
    }

    protected void log(final Throwable e) {
        final LogInterface logger = this.getLogger();
        if (logger != null && e != null) {
            logger.log(e);
        }
    }

    /**
     * @param connection2
     */
    protected void onBeforeConnect(final HTTPConnection connection) {
    }

    /**
     * This method is not synchronized, but thread safe. do not use the getConnection method of the client, but use response.getConnection()
     * instead
     */
    public RequestContext post(final String url, final byte[] data) throws IOException, InterruptedException {
        return this.execute(new RequestContext().setMethod(RequestMethod.POST).setUrl(url).setPostDataStream(new ByteArrayInputStream(data)));
    }

    /**
     * This method is not synchronized, but thread safe. do not use the getConnection method of the client, but use response.getConnection()
     * instead
     */
    public RequestContext post(final String url, final String utf8STring) throws IOException, InterruptedException {
        return this.post(url, utf8STring.getBytes(UTF8));
    }

    protected void prepareConnection(final RequestContext context) {
        this.setAllowedResponseCodes(context);
        context.connection.setConnectTimeout(this.getConnectTimeout());
        context.connection.setReadTimeout(context.getReadTimeout() < 0 ? this.getReadTimeout() : context.getReadTimeout());
        context.connection.setRequestMethod(context.method);
        context.connection.setRequestProperty(HTTPConstants.HEADER_REQUEST_ACCEPT_LANGUAGE, TranslationFactory.getDesiredLanguage());
        context.connection.setRequestProperty(HTTPConstants.HEADER_REQUEST_USER_AGENT, "AppWork " + Application.getApplication());
        context.connection.setRequestProperty(HTTPConstants.HEADER_REQUEST_ACCEPT_CHARSET, UTF8.name());
        context.connection.setRequestProperty(HTTPConstants.HEADER_REQUEST_CONNECTION, "Close");
        if (context.getPostDataLength() >= 0) {
            context.connection.setRequestProperty(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, String.valueOf(context.getPostDataLength()));
        }
        for (final Entry<String, String> next : this.getRequestHeader().entrySet()) {
            context.connection.setRequestProperty(next.getKey(), next.getValue());
        }
    }

    public void putRequestHeader(final String key, final String value) {
        this.getRequestHeader().put(key, value);
    }

    protected void readInputStream(final RequestContext context, final CountingInputStream is) throws InterruptedException, IOException {
        final CountingInputStream wrapper = new CountingInputStream(is) {
            private boolean firstRead = true;

            private void onDone() throws IOException {
                this.close();
                if (context.getConnection().getCompleteContentLength() >= 0) {
                    final long completeLength = Math.max(0, context.getResumePosition()) + ((CountingConnection) is).transferedBytes();
                    if (completeLength != context.getConnection().getCompleteContentLength()) {
                        throw new IncompleteResponseException(context, completeLength);
                    }
                }
                try {
                    context.getConnection().disconnect();
                } catch (final Throwable e) {
                } finally {
                    context.onDisconnected();
                }
            }

            /**
             * @see org.appwork.utils.net.CountingInputStream#read()
             */
            @Override
            public int read() throws IOException {
                if (this.firstRead) {
                    context.onReadingStreamStarted();
                    this.firstRead = false;
                }
                final int ret = super.read();
                if (ret >= 0) {
                    context.onBytesLoaded(new byte[] { (byte) ret }, 0, 1);
                }
                return ret;
            }

            /**
             * @see org.appwork.utils.net.CountingInputStream#read(byte[], int, int)
             */
            @Override
            public int read(final byte[] b, final int off, final int len) throws IOException {
                if (this.firstRead) {
                    context.onReadingStreamStarted();
                    this.firstRead = false;
                }
                try {
                    if (len == 0) {
                        return 0;
                    }
                    if (is.transferedBytes() == 0) {
                        // read the first byte quick
                        // first load only 1 byte to get the "data input" timestamp correctly
                        final byte[] first = new byte[1];
                        final int ret = super.read(first, 0, 1);
                        if (ret <= 0) {
                            if (ret < 0) {
                                this.onDone();
                            }
                            return ret;
                        }
                        context.onBytesLoaded(first, 0, ret);
                        final byte[] rest = new byte[len - 1];
                        final int r = super.read(rest, 0, len - 1);
                        if (r <= 0) {
                            if (ret < 0) {
                                this.onDone();
                            }
                            return ret;
                        }
                        context.onBytesLoaded(rest, 0, r);
                        b[off] = first[0];
                        System.arraycopy(rest, 0, b, off + 1, rest.length);
                        return r + ret;
                    }
                    final int ret = super.read(b, off, len);
                    if (ret < 0) {
                        this.onDone();
                    } else {
                        context.onBytesLoaded(b, off, len);
                    }
                    return ret;
                } finally {
                }
            }
        };
        final OutputStream out = context.getTarget();
        if (out == null) {
            context.inputStream = wrapper;
        } else {
            context.linkInterrupt();
            try {
                final byte[] b = new byte[512 * 1024];
                while (true) {
                    final int len;
                    try {
                        if ((len = wrapper.read(b)) == -1) {
                            break;
                        }
                    } catch (final IOException e) {
                        throw new ReadIOException(e);
                    }
                    if (Thread.interrupted()) {
                        throw new InterruptedException();
                    }
                    if (len > 0) {
                        try {
                            out.write(b, 0, len);
                        } catch (final IOException e) {
                            throw new WriteIOException(e);
                        }
                    }
                }
            } finally {
                out.close();
                context.unlinkInterrupt();
                wrapper.close();
            }
        }
    }

    /**
     * @param url
     * @param postData
     * @param baos
     * @return
     * @return
     * @throws InterruptedException
     * @throws HttpClientException
     */
    public RequestContext execute(final RequestContext context) throws InterruptedException, HttpClientException {
        context.executed = true;
        context.client = this;
        this.requests.add(context);
        final HTTPConnection connection = this.createHTTPConnection(context);
        boolean followRedirect = false;
        context.linkInterrupt();
        try {
            try {
                this.prepareConnection(context);
                boolean rangeRequest = false;
                final long rp = context.getResumePosition();
                if (rp >= 0) {
                    connection.setRequestProperty(HTTPConstants.HEADER_REQUEST_RANGE, "bytes=" + rp + "-");
                    rangeRequest = true;
                }
                context.onConnect();
                if (context.getPostDataStream() != null) {
                    final HTTPOutputStream directHTTPConnectionOutputStream = this.connect(context, true);
                    final OutputStream outputStream = this.wrapPostOutputStream(connection, directHTTPConnectionOutputStream);
                    final InputStream input = context.getPostDataStream();
                    context.onPostStart();
                    if (context.getPostDataLength() > 0) {
                        final byte[] buffer = new byte[32767];
                        int len;
                        while ((len = input.read(buffer)) != -1) {
                            if (Thread.currentThread().isInterrupted()) {
                                throw new InterruptedException();
                            }
                            if (len > 0) {
                                outputStream.write(buffer, 0, len);
                                context.onBytesPosted(buffer, 0, len);
                            }
                        }
                        input.close();
                    }
                    final boolean before = directHTTPConnectionOutputStream.isClosingAllowed();
                    try {
                        directHTTPConnectionOutputStream.setClosingAllowed(false);
                        outputStream.flush();
                        outputStream.close();
                    } catch (final IOException e) {
                        throw new WriteIOException(e);
                    } finally {
                        directHTTPConnectionOutputStream.setClosingAllowed(before);
                    }
                    connection.finalizeConnect();
                    context.onConnected();
                } else {
                    this.connect(context, false);
                }
                context.onConnected();
                if (rangeRequest && connection.getResponseCode() == 200) {
                    throw new BadRangeResponse(context);
                }
                if (connection.getCompleteContentLength() >= 0) {
                    /* contentLength is known */
                    context.onContentLength(connection.getCompleteContentLength());
                }
                followRedirect = this.followRedirect(context);
                if (!followRedirect) {
                    this.checkResponseCode(context);
                    InputStream input = connection.getInputStream();
                    if (!(input instanceof CountingConnection)) {
                        input = new CountingInputStream(input);
                    }
                    this.readInputStream(context, (CountingInputStream) input);
                }
            } catch (final ReadIOException e) {
                throw this.handleInterrupt(new HttpClientException(context, e));
            } catch (final WriteIOException e) {
                throw this.handleInterrupt(new HttpClientException(context, e));
            } catch (final HttpClientException e) {
                throw this.handleInterrupt(e);
            } catch (final IOException e) {
                throw this.handleInterrupt(new HttpClientException(context, new ReadIOException(e)));
            } finally {
                this.log(connection);
            }
        } catch (final InterruptedException e) {
            context.onException(e);
            throw e;
        } catch (final HttpClientException e) {
            context.onException(e);
            throw e;
        } finally {
            context.unlinkInterrupt();
        }
        if (followRedirect) {
            if (context.redirectsStarted > 0 && Time.systemIndependentCurrentJVMTimeMillis() >= context.redirectsStarted) {
                throw new RedirectTimeoutException(context, null);
            }
            // the redirect will not be a POST again
            context.setPostDataStream(null);
            context.setPostDataLength(-1);
            context.method = RequestMethod.GET;
            return this.execute(context);
        }
        return context;
    }

    public void setAllowedResponseCodes(final int... codes) {
        final HashSet<Integer> allowedResponseCodes = new HashSet<Integer>();
        for (final int i : codes) {
            allowedResponseCodes.add(i);
        }
        this.allowedResponseCodes = allowedResponseCodes;
    }

    protected void setAllowedResponseCodes(final RequestContext context) {
        final HashSet<Integer> allowedResponseCodes = this.getAllowedResponseCodes();
        if (allowedResponseCodes != null) {
            final int[] ret = new int[allowedResponseCodes.size()];
            int i = 0;
            for (final Integer allowed : allowedResponseCodes) {
                ret[i++] = allowed.intValue();
            }
            context.connection.setAllowedResponseCodes(ret);
        }
    }

    public void setConnectTimeout(final int connectTimeout) {
        this.connectTimeout = Math.max(1000, connectTimeout);
    }

    public void setLogger(final LogInterface logger) {
        this.logger = logger;
    }

    public void setProxy(final HTTPProxy proxy) {
        this.proxy = proxy;
    }

    public void setReadTimeout(final int readTimeout) {
        this.readTimeout = Math.max(1000, readTimeout);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return super.toString();
    }

    protected OutputStream wrapPostOutputStream(final HTTPConnection connection, OutputStream os) throws IOException {
        if (StringUtils.equalsIgnoreCase(connection.getRequestProperty(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING), HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED)) {
            os = new ChunkedOutputStream(os);
        }
        return os;
    }
}
