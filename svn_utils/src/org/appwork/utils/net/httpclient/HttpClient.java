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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.io.OutputStream;
import java.net.URL;
import java.net.UnknownHostException;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map.Entry;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.txtresource.TranslationFactory;
import org.appwork.utils.Application;
import org.appwork.utils.Exceptions;
import org.appwork.utils.IO;
import org.appwork.utils.Interruptible;
import org.appwork.utils.InterruptibleThread;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.ChunkedOutputStream;
import org.appwork.utils.net.CountingConnection;
import org.appwork.utils.net.CountingInputStream;
import org.appwork.utils.net.DownloadProgress;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.net.UploadProgress;
import org.appwork.utils.net.BasicHTTP.BadRangeResponse;
import org.appwork.utils.net.BasicHTTP.BadResponseLengthException;
import org.appwork.utils.net.BasicHTTP.BasicHTTPException;
import org.appwork.utils.net.BasicHTTP.IncompleteResponseException;
import org.appwork.utils.net.BasicHTTP.InvalidRedirectException;
import org.appwork.utils.net.BasicHTTP.InvalidResponseCode;
import org.appwork.utils.net.BasicHTTP.ReadIOException;
import org.appwork.utils.net.BasicHTTP.RedirectTimeoutException;
import org.appwork.utils.net.BasicHTTP.WriteIOException;
import org.appwork.utils.net.httpconnection.HTTPConnection;
import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;
import org.appwork.utils.net.httpconnection.HTTPConnectionFactory;
import org.appwork.utils.net.httpconnection.HTTPOutputStream;
import org.appwork.utils.net.httpconnection.HTTPProxy;

public class HttpClient implements Interruptible {
    protected final static Charset          UTF8           = Charset.forName("UTF-8");
    protected HashSet<Integer>              allowedResponseCodes;
    protected final HashMap<String, String> requestHeader;
    protected volatile HTTPConnection       connection;
    protected int                           connectTimeout = 15000;
    protected int                           readTimeout    = 30000;
    protected HTTPProxy                     proxy          = HTTPProxy.NONE;
    protected LogInterface                  logger         = null;
    protected final Object                  lock           = new Object();

    public HttpClient() {
        this.requestHeader = new HashMap<String, String>();
    }

    /**
     * @param connection
     *            TODO
     * @throws IOException
     *
     */
    protected void checkResponseCode(HTTPConnection connection) throws InvalidResponseCode {
        final HashSet<Integer> allowedResponseCodes = this.getAllowedResponseCodes();
        if (allowedResponseCodes != null) {
            if (allowedResponseCodes.contains(-1)) {
                // allow all
                return;
            } else if (!allowedResponseCodes.contains(connection.getResponseCode())) {
                throw this.createInvalidResponseCodeException(connection);
            }
        }
    }

    public void clearRequestHeader() {
        getRequestHeader().clear();
    }

    /**
     * @param connection
     *            TODO
     * @return
     */
    protected InvalidResponseCode createInvalidResponseCodeException(HTTPConnection connection) {
        return new InvalidResponseCode(connection);
    }

    protected long getRedirectTimeout(final URL url) {
        return (60 * 60 * 1000l);
    }

    @Override
    public void interrupt(Thread arg0) {
        final HTTPConnection con = getConnection();
        if (con != null) {
            try {
                con.disconnect();
            } catch (Throwable e) {
                log(e);
            }
        }
    }

    protected void log(HTTPConnection connection) {
        final LogInterface logger = getLogger();
        if (logger != null && connection != null) {
            try {
                logger.info(connection.toString());
            } catch (final Throwable e) {
                log(e);
            }
        }
    }

    protected void log(Throwable e) {
        final LogInterface logger = getLogger();
        if (logger != null && e != null) {
            logger.log(e);
        }
    }

    protected HTTPConnection createHTTPConnection(final URL url) {
        connection = null;
        final HTTPConnection connection = HTTPConnectionFactory.createHTTPConnection(url, getProxy());
        this.connection = connection;
        RequestContext localContext = context.get();
        if (localContext != null) {
            localContext.connection = connection;
        }
        return connection;
    }

    public URL followRedirect(HTTPConnection connection, final URL url) throws IOException, InterruptedException {
        if (connection.getResponseCode() == 301 || connection.getResponseCode() == 302 || connection.getResponseCode() == 303 || connection.getResponseCode() == 307) {
            final String red = connection.getHeaderField(HTTPConstants.HEADER_RESPONSE_LOCATION);
            if (red != null) {
                if (connection.getResponseCode() == 302) {
                    Thread.sleep(125);
                } else {
                    Thread.sleep(250);
                }
                return new URL(URLHelper.parseLocation(url, red));
            } else {
                throw new InvalidRedirectException(connection);
            }
        } else {
            return null;
        }
    }

    /**
     *
     * Please do not forget to close the output stream.
     *
     * @param url
     * @param progress
     * @param maxSize
     * @param baos
     * @throws IOException
     * @throws InterruptedException
     */
    private void download(final URL url, final DownloadProgress downloadProgress, final long maxSize, final OutputStream baos, final long resumePosition, final long redirectTimeoutTimeStamp) throws BasicHTTPException, InterruptedException {
        final DownloadProgress progress;
        if (downloadProgress == null) {
            progress = new DownloadProgress();
        } else {
            progress = downloadProgress;
        }
        URL followRedirect = null;
        final HTTPConnection connection = createHTTPConnection(url);
        try {
            final boolean addedInterruptible = Boolean.TRUE.equals(InterruptibleThread.add(this));
            try {
                prepareConnection(connection, RequestMethod.GET, -1);
                final boolean rangeRequest;
                if (resumePosition > 0) {
                    connection.setRequestProperty(HTTPConstants.HEADER_REQUEST_RANGE, "bytes=" + resumePosition + "-");
                    rangeRequest = true;
                } else {
                    rangeRequest = false;
                }
                progress.onConnect(connection);
                connection.connect();
                progress.onConnected(connection);
                if (rangeRequest && connection.getResponseCode() == 200) {
                    throw new BadRangeResponse(connection);
                }
                followRedirect = followRedirect(connection, url);
                if (followRedirect == null) {
                    this.checkResponseCode(connection);
                    InputStream is = connection.getInputStream();
                    if (!(is instanceof CountingConnection)) {
                        is = new CountingInputStream(is);
                    }
                    if (connection.getCompleteContentLength() >= 0) {
                        /* contentLength is known */
                        if (maxSize > 0 && connection.getCompleteContentLength() > maxSize) {
                            throw new BadResponseLengthException(connection, maxSize);
                        }
                        progress.setTotal(connection.getCompleteContentLength());
                    } else {
                        /* no contentLength is known */
                    }
                    readInputStream(maxSize, baos, resumePosition, null, progress, connection, (CountingInputStream) is);
                } else {
                    if (redirectTimeoutTimeStamp > 0 && System.currentTimeMillis() >= redirectTimeoutTimeStamp) {
                        throw new RedirectTimeoutException(connection);
                    }
                }
            } catch (final ReadIOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, e));
            } catch (final WriteIOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, e));
            } catch (final BasicHTTPException e) {
                throw handleInterrupt(e);
            } catch (final IOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, new ReadIOException(e)));
            } finally {
                if (addedInterruptible) {
                    InterruptibleThread.remove(this);
                }
                log(connection);
            }
        } catch (InterruptedException e) {
            progress.onException(connection, e);
            throw e;
        } catch (BasicHTTPException e) {
            progress.onException(connection, e);
            throw e;
        } finally {
        }
        if (followRedirect != null) {
            download(followRedirect, progress, maxSize, baos, resumePosition, redirectTimeoutTimeStamp);
        }
    }

    protected void readInputStream(final long maxSize, final OutputStream baos, final long resumePosition, final DownloadProgress finalUploadProgress, final DownloadProgress progress, final HTTPConnection connection, final CountingInputStream is) throws InterruptedException, IOException {
        CountingInputStream wrapper = new CountingInputStream(is) {
            /**
             * @see org.appwork.utils.net.CountingInputStream#read()
             */
            @Override
            public int read() throws IOException {
                int ret = super.read();
                if (ret >= 0) {
                    if (progress != null) {
                        progress.onBytesLoaded(new byte[] { (byte) ret }, 1);
                    }
                    if (progress != null) {
                        progress.increaseLoaded(1);
                    }
                    checkForMaxSizeReached(maxSize, connection, is);
                }
                return ret;
            }

            protected void checkForMaxSizeReached(final long maxSize, final HTTPConnection connection, final CountingInputStream is) throws BadResponseLengthException {
                if (maxSize > 0 && ((CountingConnection) is).transferedBytes() > maxSize) {
                    throw new BadResponseLengthException(connection, maxSize);
                }
            }

            private void onDone() throws IOException {
                this.close();
                if (connection.getCompleteContentLength() >= 0) {
                    final long completeLength = Math.max(0, resumePosition) + ((CountingConnection) is).transferedBytes();
                    if (completeLength != connection.getCompleteContentLength()) {
                        throw new IncompleteResponseException(connection, completeLength);
                    }
                }
                try {
                    connection.disconnect();
                } catch (final Throwable e) {
                } finally {
                    if (finalUploadProgress != null) {
                        finalUploadProgress.onDisconnected(connection);
                    }
                    if (progress != null) {
                        progress.onDisconnected(connection);
                    }
                }
            }

            /**
             * @see org.appwork.utils.net.CountingInputStream#read(byte[], int, int)
             */
            @Override
            public int read(byte[] b, int off, int len) throws IOException {
                try {
                    if (len == 0) {
                        return 0;
                    }
                    if (is.transferedBytes() == 0) {
                        // read the first byte quick
                        // first load only 1 byte to get the "data input" timestamp correctly
                        byte[] first = new byte[1];
                        int ret = super.read(first, 0, 1);
                        if (ret <= 0) {
                            if (ret < 0) {
                                onDone();
                            }
                            return ret;
                        }
                        if (progress != null) {
                            progress.onBytesLoaded(first, ret);
                        }
                        if (progress != null) {
                            progress.increaseLoaded(ret);
                        }
                        byte[] rest = new byte[len - 1];
                        int r = super.read(rest, 0, len - 1);
                        if (r <= 0) {
                            if (ret < 0) {
                                onDone();
                            }
                            return ret;
                        }
                        if (progress != null) {
                            progress.onBytesLoaded(rest, r + ret);
                        }
                        b[off] = first[0];
                        System.arraycopy(rest, 0, b, off + 1, rest.length);
                        if (progress != null) {
                            progress.increaseLoaded(r);
                        }
                        return r + ret;
                    }
                    int ret = super.read(b, off, len);
                    if (ret < 0) {
                        onDone();
                    }
                    return ret;
                } finally {
                    checkForMaxSizeReached(maxSize, connection, is);
                }
            }
        };
        RequestContext localContext = context.get();
        if (localContext != null && localContext.provideInputStream) {
            localContext.inputStream = wrapper;
        } else {
            try {
                byte[] b = new byte[512 * 1024];
                if (progress != null) {
                    progress.setLoaded(Math.max(0, resumePosition));
                }
                while (true) {
                    final int len;
                    try {
                        if ((len = wrapper.read(b)) == -1) {
                            break;
                        }
                    } catch (IOException e) {
                        throw new ReadIOException(e);
                    }
                    if (Thread.interrupted()) {
                        throw new InterruptedException();
                    }
                    if (len > 0) {
                        try {
                            baos.write(b, 0, len);
                        } catch (IOException e) {
                            throw new WriteIOException(e);
                        }
                    }
                }
            } finally {
                wrapper.close();
            }
        }
    }

    public HashSet<Integer> getAllowedResponseCodes() {
        return this.allowedResponseCodes;
    }

    public HTTPConnection getConnection() {
        return this.connection;
    }

    public int getConnectTimeout() {
        return this.connectTimeout;
    }

    public LogInterface getLogger() {
        return this.logger;
    }

    public String getPage(final URL url) throws IOException, InterruptedException {
        synchronized (this.lock) {
            URL followRedirect = null;
            ToStringByteArrayOutputStream baos = new ToStringByteArrayOutputStream();
            final HTTPConnection connection = createHTTPConnection(url);
            final boolean addedInterruptible = Boolean.TRUE.equals(InterruptibleThread.add(this));
            try {
                prepareConnection(connection, RequestMethod.GET, -1);
                connect(null, connection, false);
                followRedirect = followRedirect(connection, url);
                if (followRedirect == null) {
                    this.checkResponseCode(connection);
                    // TODO: parse Content-Type, charset
                    InputStream input = connection.getInputStream();
                    if (!(input instanceof CountingConnection)) {
                        input = new CountingInputStream(input);
                    }
                    readInputStream(-1, baos, 0, null, null, connection, (CountingInputStream) input);
                }
            } catch (final ReadIOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, e));
            } catch (final WriteIOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, e));
            } catch (final BasicHTTPException e) {
                throw handleInterrupt(e);
            } catch (final IOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, new ReadIOException(e)));
            } finally {
                try {
                    if (addedInterruptible) {
                        InterruptibleThread.remove(this);
                    }
                    log(connection);
                } finally {
                    try {
                        connection.disconnect();
                    } catch (final Throwable e) {
                    }
                }
            }
            if (followRedirect != null) {
                return getPage(followRedirect);
            } else {
                return baos.toString(UTF8);
            }
        }
    }

    public HTTPProxy getProxy() {
        return this.proxy;
    }

    public int getReadTimeout() {
        return this.readTimeout;
    }

    /**
     * @return
     */
    public HashMap<String, String> getRequestHeader() {
        return this.requestHeader;
    }

    public String getRequestHeader(final String key) {
        return getRequestHeader().get(key);
    }

    public String getResponseHeader(final String string) {
        final HTTPConnection con = getConnection();
        return con != null ? con.getHeaderField(string) : null;
    }

    public HTTPConnection openGetConnection(final URL url) throws IOException, InterruptedException {
        return this.openGetConnection(url, getReadTimeout());
    }

    public HTTPConnection openGetConnection(final URL url, final int readTimeout) throws BasicHTTPException, InterruptedException {
        synchronized (this.lock) {
            boolean close = true;
            URL followRedirect = null;
            final HTTPConnection connection = createHTTPConnection(url);
            final boolean addedInterruptible = Boolean.TRUE.equals(InterruptibleThread.add(this));
            try {
                prepareConnection(connection, RequestMethod.GET, -1);
                int lookupTry = 0;
                while (true) {
                    try {
                        connection.connect();
                        break;
                    } catch (final UnknownHostException e) {
                        if (++lookupTry > 3) {
                            throw e;
                        }
                        /* dns lookup failed, short wait and try again */
                        Thread.sleep(200);
                    }
                }
                followRedirect = followRedirect(connection, url);
                if (followRedirect == null) {
                    this.checkResponseCode(connection);
                    close = false;
                }
            } catch (final ReadIOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, e));
            } catch (final WriteIOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, e));
            } catch (final BasicHTTPException e) {
                throw handleInterrupt(e);
            } catch (final IOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, new ReadIOException(e)));
            } finally {
                try {
                    if (addedInterruptible) {
                        InterruptibleThread.remove(this);
                    }
                    log(connection);
                } finally {
                    if (close) {
                        try {
                            connection.disconnect();
                        } catch (final Throwable e2) {
                        }
                    }
                }
            }
            if (followRedirect != null) {
                return openGetConnection(followRedirect, readTimeout);
            } else {
                return connection;
            }
        }
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

    @SuppressWarnings("resource")
    public HTTPConnection openPostConnection(final URL url, final UploadProgress progress, final InputStream is, final HashMap<String, String> customHeaders, final long contentLength) throws BasicHTTPException, InterruptedException {
        boolean close = true;
        synchronized (this.lock) {
            final HTTPConnection connection = createHTTPConnection(url);
            final boolean addedInterruptible = Boolean.TRUE.equals(InterruptibleThread.add(this));
            try {
                prepareConnection(connection, RequestMethod.POST, -1);
                /* connection specific headers */
                if (customHeaders != null) {
                    for (final Entry<String, String> next : customHeaders.entrySet()) {
                        if (HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH.equalsIgnoreCase(next.getKey())) {
                            continue;
                        } else {
                            connection.setRequestProperty(next.getKey(), next.getValue());
                        }
                    }
                }
                if (contentLength >= 0) {
                    connection.setRequestProperty(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, Long.toString(contentLength));
                } else {
                    connection.setRequestProperty(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING, HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED);
                }
                onBeforeConnect(connection);
                HTTPOutputStream directHttpConnectionOuputStream = connect(null, connection, true);
                OutputStream outputStream = wrapPostOutputStream(connection, directHttpConnectionOuputStream);
                final byte[] buf = new byte[64000];
                while (true) {
                    final int read;
                    try {
                        read = is.read(buf);
                    } catch (IOException e) {
                        throw new ReadIOException(e);
                    }
                    if (read == -1) {
                        break;
                    }
                    try {
                        outputStream.write(buf, 0, read);
                    } catch (final IOException e) {
                        throw new WriteIOException(e);
                    }
                    if (progress != null) {
                        progress.onBytesUploaded(buf, read);
                        progress.increaseUploaded(read);
                    }
                    if (Thread.interrupted()) {
                        throw new InterruptedException();
                    }
                }
                final boolean before = directHttpConnectionOuputStream.isClosingAllowed();
                try {
                    directHttpConnectionOuputStream.setClosingAllowed(false);
                    outputStream.flush();
                    outputStream.close();
                } catch (final IOException e) {
                    throw new WriteIOException(e);
                } finally {
                    directHttpConnectionOuputStream.setClosingAllowed(before);
                }
                connection.finalizeConnect();
                this.checkResponseCode(connection);
                close = false;
                return connection;
            } catch (final ReadIOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, e));
            } catch (final WriteIOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, e));
            } catch (final BasicHTTPException e) {
                throw handleInterrupt(e);
            } catch (final IOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, new ReadIOException(e)));
            } finally {
                try {
                    if (addedInterruptible) {
                        InterruptibleThread.remove(this);
                    }
                    log(connection);
                } finally {
                    if (close) {
                        try {
                            connection.disconnect();
                        } catch (final Throwable e2) {
                        }
                    }
                }
            }
        }
    }

    /**
     * @param connection2
     */
    protected void onBeforeConnect(HTTPConnection connection) {
    }

    protected OutputStream wrapPostOutputStream(final HTTPConnection connection, OutputStream os) throws IOException {
        if (StringUtils.equalsIgnoreCase(connection.getRequestProperty(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING), HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED)) {
            os = new ChunkedOutputStream(os);
        }
        return os;
    }

    public byte[] postPage(final URL url, final byte[] byteData) throws BasicHTTPException, InterruptedException {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        this.postPage(url, byteData, baos, null, null);
        return baos.toByteArray();
    }

    /**
     * @param url
     * @param postData
     * @param baos
     * @return
     * @throws InterruptedException
     * @throws BasicHTTPException
     */
    private void postPage(final URL url, byte[] postData, final OutputStream baos, final DownloadProgress uploadProgress, final DownloadProgress downloadProgress) throws InterruptedException, BasicHTTPException {
        final byte[] postBytes;
        if (postData == null) {
            postBytes = new byte[0];
        } else {
            postBytes = postData;
        }
        final DownloadProgress finalUploadProgress;
        if (uploadProgress == null) {
            finalUploadProgress = new DownloadProgress();
        } else {
            finalUploadProgress = uploadProgress;
        }
        final DownloadProgress finalDownloadProgress;
        if (downloadProgress == null) {
            finalDownloadProgress = new DownloadProgress();
        } else {
            finalDownloadProgress = downloadProgress;
        }
        final HTTPConnection connection = createHTTPConnection(url);
        final boolean addedInterruptible = Boolean.TRUE.equals(InterruptibleThread.add(this));
        URL followRedirect = null;
        try {
            try {
                prepareConnection(connection, RequestMethod.POST, -1);
                connection.setRequestProperty(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, String.valueOf(postBytes.length));
                finalUploadProgress.onConnect(connection);
                finalDownloadProgress.onConnect(connection);
                HTTPOutputStream directHTTPConnectionOutputStream = connect(finalUploadProgress, connection, true);
                final OutputStream outputStream = wrapPostOutputStream(connection, directHTTPConnectionOutputStream);
                finalUploadProgress.setTotal(postBytes.length);
                if (postBytes.length > 0) {
                    // write upload in 50*1024 steps
                    int offset = 0;
                    while (true) {
                        final int toWrite = Math.min(50 * 1024, postBytes.length - offset);
                        if (toWrite == 0) {
                            finalUploadProgress.setLoaded(postBytes.length);
                            break;
                        } else {
                            outputStream.write(postBytes, offset, toWrite);
                            if (Thread.interrupted()) {
                                throw new InterruptedException();
                            } else {
                                offset += toWrite;
                                finalUploadProgress.increaseLoaded(toWrite);
                            }
                        }
                    }
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
                finalDownloadProgress.onConnected(connection);
                if (connection.getCompleteContentLength() >= 0) {
                    /* contentLength is known */
                    finalDownloadProgress.setTotal(connection.getCompleteContentLength());
                } else {
                    /* no contentLength is known */
                }
                followRedirect = followRedirect(connection, url);
                if (followRedirect == null) {
                    this.checkResponseCode(connection);
                    InputStream input = connection.getInputStream();
                    try {
                        if (!(input instanceof CountingConnection)) {
                            input = new CountingInputStream(input);
                        }
                        readInputStream(-1, baos, 0, finalUploadProgress, finalDownloadProgress, connection, (CountingInputStream) input);
                    } finally {
                        input.close();
                    }
                }
            } catch (final ReadIOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, e));
            } catch (final WriteIOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, e));
            } catch (final BasicHTTPException e) {
                throw handleInterrupt(e);
            } catch (final IOException e) {
                throw handleInterrupt(new BasicHTTPException(connection, new ReadIOException(e)));
            } finally {
                if (addedInterruptible) {
                    InterruptibleThread.remove(this);
                }
                log(connection);
            }
        } catch (InterruptedException e) {
            finalUploadProgress.onException(connection, e);
            finalDownloadProgress.onException(connection, e);
            throw e;
        } catch (BasicHTTPException e) {
            finalUploadProgress.onException(connection, e);
            finalDownloadProgress.onException(connection, e);
            throw e;
        } finally {
        }
        if (followRedirect != null) {
            postPage(followRedirect, postData, baos, uploadProgress, downloadProgress);
        }
    }

    // legacy
    protected OutputStream connect(final DownloadProgress finalUploadProgress, final HTTPConnection connection) throws IOException, InterruptedException, UnknownHostException {
        return connect(finalUploadProgress, connection, false);
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
    protected HTTPOutputStream connect(final DownloadProgress finalUploadProgress, final HTTPConnection connection, boolean returnOutputStream) throws IOException, InterruptedException, UnknownHostException {
        int lookupTry = 0;
        try {
            while (true) {
                try {
                    connection.connect();
                    if (finalUploadProgress != null) {
                        finalUploadProgress.onConnected(connection);
                    }
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
            throw new BasicHTTPException(connection, new ReadIOException(e));
        }
        if (!returnOutputStream) {
            return null;
        } else {
            final HTTPOutputStream raw = connection.getOutputStream();
            return raw;
        }
    }

    /**
     * @param <E>
     * @param basicHTTPException
     */
    private <E extends Throwable> E handleInterrupt(E exception) throws InterruptedException, E {
        if (exception instanceof InterruptedException) {
            throw (InterruptedException) exception;
        } else if (Thread.interrupted() || exception instanceof InterruptedIOException) {
            throw Exceptions.addSuppressed(new InterruptedException("Connection Closed by Interrupt"), exception);
        } else {
            return exception;
        }
    }

    public String postPage(final URL url, final String postString) throws BasicHTTPException, InterruptedException {
        return postPage(url, postString, UTF8);
    }

    public class ToStringByteArrayOutputStream extends ByteArrayOutputStream {
        @Override
        public synchronized String toString() {
            return new String(buf, 0, count, UTF8);
        }
    }

    public String postPage(final URL url, final String postString, final Charset charset) throws BasicHTTPException, InterruptedException {
        final ByteArrayOutputStream baos = new ToStringByteArrayOutputStream();
        this.postPage(url, postString != null ? postString.getBytes(UTF8) : null, baos, null, null);
        return baos.toString();
    }

    public void putRequestHeader(final String key, final String value) {
        getRequestHeader().put(key, value);
    }

    protected void setAllowedResponseCodes(final HTTPConnection connection) {
        final HashSet<Integer> allowedResponseCodes = this.getAllowedResponseCodes();
        if (allowedResponseCodes != null) {
            final int[] ret = new int[allowedResponseCodes.size()];
            int i = 0;
            for (Integer allowed : allowedResponseCodes) {
                ret[i++] = allowed.intValue();
            }
            connection.setAllowedResponseCodes(ret);
        }
    }

    public void setAllowedResponseCodes(final int... codes) {
        final HashSet<Integer> allowedResponseCodes = new HashSet<Integer>();
        for (final int i : codes) {
            allowedResponseCodes.add(i);
        }
        this.allowedResponseCodes = allowedResponseCodes;
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

    protected void prepareConnection(HTTPConnection connection, RequestMethod method, final int readTimeout) {
        this.setAllowedResponseCodes(connection);
        connection.setConnectTimeout(this.getConnectTimeout());
        connection.setReadTimeout(readTimeout < 0 ? getReadTimeout() : readTimeout);
        RequestContext overwrittenMethod = context.get();
        if (overwrittenMethod != null) {
            method = overwrittenMethod.method;
        }
        connection.setRequestMethod(method);
        connection.setRequestProperty(HTTPConstants.HEADER_REQUEST_ACCEPT_LANGUAGE, TranslationFactory.getDesiredLanguage());
        connection.setRequestProperty(HTTPConstants.HEADER_REQUEST_USER_AGENT, "AppWork " + Application.getApplication());
        connection.setRequestProperty(HTTPConstants.HEADER_REQUEST_ACCEPT_CHARSET, UTF8.name());
        connection.setRequestProperty(HTTPConstants.HEADER_REQUEST_CONNECTION, "Close");
        for (final Entry<String, String> next : getRequestHeader().entrySet()) {
            connection.setRequestProperty(next.getKey(), next.getValue());
        }
    }

    public static class RequestContext {
        private boolean             provideInputStream = false;
        private CountingInputStream inputStream;

        public CountingInputStream getInputStream() {
            return inputStream;
        }

        /**
         * The client will not read the stream, but provide a direct reference to the http inputstream - you may implement own read methods;
         *
         * @return
         */
        public RequestContext requestInputStream() {
            provideInputStream = true;
            return this;
        }

        private HTTPConnection connection;

        public RequestMethod getMethod() {
            return method;
        }

        public RequestContext setMethod(RequestMethod method) {
            this.method = method;
            return this;
        }

        public String getUrl() {
            return url;
        }

        public RequestContext setUrl(String url) {
            this.url = url;
            return this;
        }

        private InputStream postDataStream;

        public RequestContext setPostDataStream(InputStream postDataStream) {
            this.postDataStream = postDataStream;
            return this;
        }

        private RequestMethod method;
        private String        url;

        /**
         * @param delete
         */
        public RequestContext() {
        }

        /**
         * @return
         */
        public InputStream getPostDataStream() {
            return postDataStream;
        }
    }

    private ThreadLocal<RequestContext> context = new ThreadLocal<RequestContext>();

    /**
     * This method is not synchronized, but thread safe. do not use the getConnection method of the client, but use response.getConnection()
     * instead
     */
    public Response delete(String url) throws IOException, InterruptedException {
        return execute(new RequestContext().setMethod(RequestMethod.DELETE).setUrl(url));
    }

    /**
     * This method is not synchronized, but thread safe. do not use the getConnection method of the client, but use response.getConnection()
     * instead
     */
    public Response execute(RequestContext context) throws IOException, InterruptedException {
        this.context.set(context);
        try {
            ToStringByteArrayOutputStream os = new ToStringByteArrayOutputStream();
            InputStream post = context.getPostDataStream();
            if (post != null) {
                postPage(new URL(context.getUrl()), IO.readStream(-1, post), os, null, null);
            } else {
                download(new URL(context.url), null, -1, os, 0, 0);
            }
            return new Response(context.connection, os, context);
        } finally {
            this.context.set(null);
        }
    }

    public Response get(String url) throws IOException, InterruptedException {
        return execute(new RequestContext().setMethod(RequestMethod.GET).setUrl(url));
    }

    /**
     * This method is not synchronized, but thread safe. do not use the getConnection method of the client, but use response.getConnection()
     * instead
     */
    public Response post(String url, byte[] data) throws IOException, InterruptedException {
        return execute(new RequestContext().setMethod(RequestMethod.POST).setUrl(url).setPostDataStream(new ByteArrayInputStream(data)));
    }

    /**
     * This method is not synchronized, but thread safe. do not use the getConnection method of the client, but use response.getConnection()
     * instead
     */
    public Response post(String url, String utf8STring) throws IOException, InterruptedException {
        return post(url, utf8STring.getBytes(UTF8));
    }
}
