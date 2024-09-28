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
package org.appwork.utils.net.BasicHTTP;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
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
import org.appwork.utils.net.httpconnection.HTTPConnection;
import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;
import org.appwork.utils.net.httpconnection.HTTPConnectionFactory;
import org.appwork.utils.net.httpconnection.HTTPOutputStream;
import org.appwork.utils.net.httpconnection.HTTPProxy;

public class BasicHTTP implements Interruptible {
    protected final static Charset          UTF8           = Charset.forName("UTF-8");
    protected HashSet<Integer>              allowedResponseCodes;
    protected final HashMap<String, String> requestHeader;
    protected volatile HTTPConnection       connection;
    protected int                           connectTimeout = 15000;
    protected int                           readTimeout    = 30000;
    protected HTTPProxy                     proxy          = HTTPProxy.NONE;
    protected LogInterface                  logger         = null;
    protected final Object                  lock           = new Object();

    public BasicHTTP() {
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

    /**
     * @param url
     * @param progress
     * @param file
     * @throws InterruptedException
     * @throws IOException
     */
    public void download(final URL url, final DownloadProgress progress, final File file) throws BasicHTTPException, InterruptedException {
        final FileOutputStream fos;
        try {
            fos = new FileOutputStream(file, true);
        } catch (final FileNotFoundException e) {
            throw new BasicHTTPException(null, new WriteIOException(e));
        }
        try {
            HTTPConnection downloadConnection = null;
            try {
                synchronized (lock) {
                    this.download(url, progress, 0, fos, file.length());
                    downloadConnection = getConnection();
                }
            } catch (final BasicHTTPException e) {
                throw e;
            } catch (final InterruptedException e) {
                throw e;
            } catch (final Exception e) {
                // we cannot say if read or write
                throw new BasicHTTPException(downloadConnection, e);
            }
        } finally {
            try {
                fos.close();
            } catch (final Throwable t) {
            }
        }
    }

    public byte[] download(final URL url, final DownloadProgress progress, final long maxSize) throws BasicHTTPException, InterruptedException {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            this.download(url, progress, maxSize, baos, -1);
        } catch (final BasicHTTPException e) {
            throw e;
        } catch (final InterruptedException e) {
            throw e;
        } finally {
            try {
                baos.close();
            } catch (final Throwable t) {
            }
        }
        return baos.toByteArray();
    }

    protected long getRedirectTimeout(final URL url) {
        return (60 * 60 * 1000l);
    }

    public void download(final URL url, final DownloadProgress progress, final long maxSize, final OutputStream baos, final long resumePosition) throws BasicHTTPException, InterruptedException {
        download(url, progress, maxSize, baos, resumePosition, System.currentTimeMillis() + getRedirectTimeout(url));
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
        this.connection = null;
        final HTTPConnection connection = HTTPConnectionFactory.createHTTPConnection(url, getProxy());
        this.connection = connection;
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
    protected void download(final URL url, final DownloadProgress downloadProgress, final long maxSize, final OutputStream baos, final long resumePosition, final long redirectTimeoutTimeStamp) throws BasicHTTPException, InterruptedException {
        final DownloadProgress progress;
        if (downloadProgress == null) {
            progress = new DownloadProgress();
        } else {
            progress = downloadProgress;
        }
        synchronized (this.lock) {
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
                        try {
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
                            // first load only 1 byte to get the "data input" timestamp correctly
                            byte[] b = new byte[1];
                            progress.setLoaded(Math.max(0, resumePosition));
                            while (true) {
                                final int len;
                                try {
                                    if ((len = is.read(b)) == -1) {
                                        break;
                                    }
                                } catch (IOException e) {
                                    throw new ReadIOException(e);
                                }
                                if (Thread.interrupted()) {
                                    throw new InterruptedException();
                                }
                                if (len > 0) {
                                    progress.onBytesLoaded(b, len);
                                    try {
                                        baos.write(b, 0, len);
                                    } catch (IOException e) {
                                        throw new WriteIOException(e);
                                    }
                                    if (maxSize > 0 && ((CountingConnection) is).transferedBytes() > maxSize) {
                                        throw new BadResponseLengthException(connection, maxSize);
                                    }
                                    progress.increaseLoaded(len);
                                    if (b.length == 1) {
                                        b = new byte[512 * 1024];
                                    }
                                }
                            }
                            if (connection.getCompleteContentLength() >= 0) {
                                final long completeLength = Math.max(0, resumePosition) + ((CountingConnection) is).transferedBytes();
                                if (completeLength != connection.getCompleteContentLength()) {
                                    throw new IncompleteResponseException(connection, completeLength);
                                }
                            }
                        } finally {
                            is.close();
                        }
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
                try {
                    connection.disconnect();
                } catch (final Throwable e) {
                } finally {
                    progress.onDisconnected(connection);
                }
            }
            if (followRedirect != null) {
                download(followRedirect, progress, maxSize, baos, resumePosition, redirectTimeoutTimeStamp);
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
            final StringBuilder sb = new StringBuilder();
            final HTTPConnection connection = createHTTPConnection(url);
            final boolean addedInterruptible = Boolean.TRUE.equals(InterruptibleThread.add(this));
            try {
                prepareConnection(connection, RequestMethod.GET, -1);
                connect(null, connection, false);
                followRedirect = followRedirect(connection, url);
                if (followRedirect == null) {
                    this.checkResponseCode(connection);
                    // TODO: parse Content-Type, charset
                    final BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream(), UTF8));
                    try {
                        String str;
                        while ((str = in.readLine()) != null) {
                            if (Thread.interrupted()) {
                                throw new InterruptedException();
                            } else {
                                if (sb.length() > 0) {
                                    sb.append("\r\n");
                                }
                                sb.append(str);
                            }
                        }
                    } finally {
                        in.close();
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
                return sb.toString();
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

    protected void prepareConnection(HTTPConnection connection, RequestMethod method, final int readTimeout) {
        this.setAllowedResponseCodes(connection);
        connection.setConnectTimeout(this.getConnectTimeout());
        connection.setReadTimeout(readTimeout < 0 ? getReadTimeout() : readTimeout);
        connection.setRequestMethod(method);
        connection.setRequestProperty(HTTPConstants.HEADER_REQUEST_ACCEPT_LANGUAGE, TranslationFactory.getDesiredLanguage());
        connection.setRequestProperty(HTTPConstants.HEADER_REQUEST_USER_AGENT, "AppWork " + Application.getApplication());
        connection.setRequestProperty(HTTPConstants.HEADER_REQUEST_ACCEPT_CHARSET, UTF8.name());
        connection.setRequestProperty(HTTPConstants.HEADER_REQUEST_CONNECTION, "Close");
        for (final Entry<String, String> next : getRequestHeader().entrySet()) {
            connection.setRequestProperty(next.getKey(), next.getValue());
        }
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
    public void postPage(final URL url, byte[] postData, final OutputStream baos, final DownloadProgress uploadProgress, final DownloadProgress downloadProgress) throws InterruptedException, BasicHTTPException {
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
        synchronized (this.lock) {
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
                            // first load only 1 byte to get the "data input" timestamp correctly
                            byte[] b = new byte[1];
                            while (true) {
                                final int len;
                                try {
                                    if ((len = input.read(b)) == -1) {
                                        break;
                                    }
                                } catch (final IOException e) {
                                    throw new ReadIOException(e);
                                }
                                if (Thread.interrupted()) {
                                    throw new InterruptedException();
                                }
                                if (len > 0) {
                                    finalDownloadProgress.onBytesLoaded(b, len);
                                    try {
                                        baos.write(b, 0, len);
                                    } catch (final IOException e) {
                                        throw new WriteIOException(e);
                                    }
                                    finalDownloadProgress.increaseLoaded(len);
                                    if (b.length == 1) {
                                        b = new byte[32767];
                                    }
                                }
                            }
                            if (connection.getCompleteContentLength() >= 0) {
                                final long loaded = ((CountingConnection) input).transferedBytes();
                                if (loaded != connection.getCompleteContentLength()) {
                                    throw new IncompleteResponseException(connection, loaded);
                                }
                            }
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
                try {
                    connection.disconnect();
                } catch (final Throwable e) {
                } finally {
                    finalUploadProgress.onDisconnected(connection);
                    finalDownloadProgress.onDisconnected(connection);
                }
            }
            if (followRedirect != null) {
                download(followRedirect, downloadProgress, -1, baos, 0, System.currentTimeMillis());
            }
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

    public String postPage(final URL url, final String postString, final Charset charset) throws BasicHTTPException, InterruptedException {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream() {
            @Override
            public synchronized String toString() {
                return new String(buf, 0, count, charset);
            }
        };
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
}
