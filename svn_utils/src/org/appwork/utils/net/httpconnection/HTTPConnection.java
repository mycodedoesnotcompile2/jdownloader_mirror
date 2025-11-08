/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
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
package org.appwork.utils.net.httpconnection;

import java.io.IOException;
import java.io.InputStream;
import java.lang.ref.WeakReference;
import java.net.URL;
import java.util.List;
import java.util.Map;

public interface HTTPConnection {
    public static class HTTPResponseException extends IOException {
        /**
         *
         */
        private static final long             serialVersionUID = 1L;
        private WeakReference<HTTPConnection> connection;

        public HTTPResponseException(HTTPConnection connection) {
            this.connection = new WeakReference<HTTPConnection>(connection);
        }

        public HTTPConnection getConnection() {
            WeakReference<HTTPConnection> connection = this.connection;
            HTTPConnection ret = null;
            if (connection == null || (ret = connection.get()) == null) {
                this.connection = null;
                return null;
            }
            return ret;
        }
    };

    public static class HTTPResponseCodeException extends HTTPResponseException {
        /**
         *
         */
        private static final long serialVersionUID = 1L;
        private final int         responseCode;

        public int getResponseCode() {
            return responseCode;
        }

        public String getResponseMessage() {
            return responseMessage;
        }

        private final String responseMessage;

        protected HTTPResponseCodeException(HTTPConnection connection) {
            super(connection);
            this.responseCode = connection.getResponseCode();
            this.responseMessage = connection.getResponseMessage();
        }

        @Override
        public String toString() {
            return getResponseCode() + " " + getResponseMessage();
        }
    }

    public static enum RequestMethod {
        NOTIFY(false), // UPNP
        MSEARCH(false), // UPNP
        SUBSCRIBE(false), // UPNP
        UNSUBSCRIBE(false), // UPNP
        PUT(true), // HTTP 1.1/WebDAV
        DELETE(true), // WebDAV
        OPTIONS(false), // HTTP 1.1
        GET(false), // HTTP 1.1
        POST(true), // HTTP 1.1
        HEAD(false), // HTTP 1.1
        PROPFIND(true);// WebDAV
        public final boolean requiresOutputStream;

        /**
         *
         */
        private RequestMethod(boolean requiresOutputStream) {
            this.requiresOutputStream = requiresOutputStream;
        }
    }

    void setLegacyConnectEnabled(boolean enabled);

    boolean isLegacyConnectEnabled();

    /**
     * establish a connection
     *
     * @throws IOException
     */
    void connect() throws IOException;

    /**
     * disconnect the connection
     */
    void disconnect();

    void finalizeConnect() throws IOException;

    int[] getAllowedResponseCodes();

    /**
     * returns Charset
     *
     * @return
     */
    public String getCharset();

    /**
     * always returns the complete length of the content. will also return the complete filesize in range requests
     *
     * @return
     */
    long getCompleteContentLength();

    /**
     * returns length of current content, eg the complete file or the chunk that got requested
     *
     * @return
     */
    long getContentLength();

    String getContentType();

    String getHeaderField(String string);

    /* WARNING: this returns a Case-Sensitive map */
    Map<String, List<String>> getHeaderFields();

    List<String> getHeaderFields(String string);

    InputStream getInputStream() throws IOException;

    HTTPOutputStream getOutputStream() throws IOException;

    long[] getRange();

    RequestMethod getRequestMethod();

    Map<String, String> getRequestProperties();

    String getRequestProperty(String string);

    public long getRequestTime();

    int getResponseCode();

    String getResponseMessage();

    URL getURL();

    boolean isConnected();

    boolean isContentDecoded();

    boolean isContentDisposition();

    boolean isOK();

    HTTPProxy getProxy();

    void setAllowedResponseCodes(int[] codes);

    public void setCharset(String charset);

    void setConnectTimeout(int connectTimeout);

    void setContentDecoded(boolean b);

    void setReadTimeout(int readTimeout);

    void setRequestMethod(RequestMethod method);

    void setRequestProperty(String key, String string);

    void setSSLTrustALL(boolean trustALL);

    boolean isSSLTrustALL();

    /**
     * @param profiler
     *            the profiler to set
     */
    void setProfiler(HTTPConnectionProfilerInterface profiler);

    /**
     * @return the profiler
     */
    HTTPConnectionProfilerInterface getProfiler();
}
