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
package org.appwork.utils.net.httpserver.responses;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Locale;
import java.util.zip.GZIPOutputStream;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.utils.Files;
import org.appwork.utils.ReusableByteArrayOutputStream;
import org.appwork.utils.net.ChunkedOutputStream;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.httpserver.requests.HttpRequestInterface;

/**
 * @author daniel
 * 
 */
public class FileResponse {

    public static String getMimeType(final String name) {
        final String extension = Files.getExtension(name);
        String mime = FileResponse.MIMES.get(extension.toLowerCase(Locale.ENGLISH));
        if (mime == null) {
            mime = "application/octet-stream";
        }
        return mime;
    }

    private final HttpRequestInterface     request;
    private final HttpResponseInterface    response;
    private File                           inputFile;

    private URL                            inputURL;

    private static HashMap<String, String> MIMES = new HashMap<String, String>();

    static {
        FileResponse.MIMES.put("html", "text/html");
        FileResponse.MIMES.put("htm", "text/html");
        FileResponse.MIMES.put("txt", "text/plain");
        FileResponse.MIMES.put("gif", "image/gif");
        FileResponse.MIMES.put("css", "text/css");
        FileResponse.MIMES.put("js", "text/javascript");
        FileResponse.MIMES.put("png", "image/png");
        FileResponse.MIMES.put("jpeg", "image/jpeg");
        FileResponse.MIMES.put("jpg", "image/jpeg");
        FileResponse.MIMES.put("jpe", "image/jpeg");
        FileResponse.MIMES.put("ico", "image/x-icon");
    }

    public FileResponse(final HttpRequestInterface request, final HttpResponseInterface response, final File inputFile) {
        this.request = request;
        this.response = response;
        this.inputFile = inputFile;
    }

    public FileResponse(final HttpRequestInterface request, final HttpResponseInterface response, final URL inputURL) {
        this.request = request;
        this.response = response;
        this.inputURL = inputURL;
    }

    /* do we allow gzip-encoded? */
    protected boolean allowGZIP() {
        final HTTPHeader acceptEncoding = this.request.getRequestHeaders().get(HTTPConstants.HEADER_REQUEST_ACCEPT_ENCODING);
        if (acceptEncoding != null) {
            final String value = acceptEncoding.getValue();
            if (value != null && value.contains("gzip")) { return true; }
        }
        return false;
    }

    /* do we allow Ranges for the given Content? */
    protected boolean allowRanges() {
        if (this.inputURL != null) {
            /* we do not support ranges for URL resources at the moment */
            return false;
        }
        return false;
    }

    /* what is the size of the given Content ? */
    protected long getContentLength(final long knownLength) {
        /* TODO: check for unsatisfied range or not allowed range */
        final boolean allowRanges = this.allowRanges();
        if (this.inputURL != null) {
            /* we do not know size of URL resources in advance! */
            if (knownLength >= 0) { return knownLength; }
            return -1;
        } else {
            if (!allowRanges) {
                /* send complete file */
                return this.inputFile.length();
            } else {
                return -1;
            }
        }
    }

    /* return filename for given Content, eg used for Content-Disposition */
    protected String getFileName() {
        String name = null;
        if (this.inputFile != null) {
            name = this.inputFile.getName();
        } else {
            name = this.inputURL.getFile();
        }
        return name;
    }

    /* return mimetype for given Content */
    protected String getMimeType() {
        return FileResponse.getMimeType(this.getFileName());
    }

    public void sendFile() throws IOException {
        InputStream is = null;
        URLConnection con = null;
        GZIPOutputStream gos = null;
        OutputStream os = null;
        final ReusableByteArrayOutputStream ros = null;
        boolean chunked = false;
        boolean gzip = false;
        long knownLength = -1;
        try {
            /* get inputstream */
            if (this.inputURL != null) {
                con = this.inputURL.openConnection();
                knownLength = con.getContentLengthLong();
                is = con.getInputStream();
            } else if (this.inputFile != null) {
                is = new FileInputStream(this.inputFile);
                knownLength = this.inputFile.length();
            }
            this.response.setResponseCode(ResponseCode.SUCCESS_OK);
            if (this.allowRanges()) {
                /* do we allow ranges? */
                this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_ENCODING, "bytes"));
            }
            if (this.allowGZIP()) {
                /* do we use gzip for content encoding? */
                if (!this.useContentDisposition()) {
                    /* only allow gzip when not offering to save the file */
                    gzip = true;
                    this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_ENCODING, "gzip"));
                }
            }
            final long length = this.getContentLength(knownLength);
            if (length >= 0 && !gzip) {
                /* we know content length, send it */
                this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, length + ""));
            } else {
                /*
                 * content length is unknown or we use gzipped coding, let us
                 * use chunked encoding
                 */
                chunked = true;
                this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING, "chunked"));
            }
            /* set content-type */
            this.response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, this.getMimeType()));
            if (this.useContentDisposition()) {
                /* offer file to download */
                this.response.getResponseHeaders().add(new HTTPHeader("Content-Disposition", "attachment;filename*=UTF-8''" + URLEncoder.encode(this.getFileName(), "UTF-8")));
            }
            /* configure outputstream */
            if (gzip) {
                if (chunked) {
                    os = gos = new GZIPOutputStream(new ChunkedOutputStream(this.response.getOutputStream(true)));
                } else {
                    os = gos = new GZIPOutputStream(this.response.getOutputStream(true));
                }
            } else {
                if (chunked) {
                    os = new ChunkedOutputStream(this.response.getOutputStream(true));
                } else {
                    os = this.response.getOutputStream(true);
                }
            }
            /* forward the data from inputstream to outputstream */
            final byte[] buffer = new byte[1024];
            int read = 0;
            while ((read = is.read(buffer)) >= 0) {
                if (read > 0) {
                    os.write(buffer, 0, read);
                } else {
                    Thread.yield();
                }
            }
        } finally {
            try {
                /* gzip first */
                gos.finish();
            } catch (final Throwable e) {
            }
            try {
                /* gzip first */
                gos.close();
            } catch (final Throwable e) {
            }
            try {
                /* output next, can be chunked */
                os.close();
            } catch (final Throwable e) {
            }
            try {
                is.close();
            } catch (final Throwable e) {
            }
        }
    }

    /* do we want the client to download this file or not? */
    protected boolean useContentDisposition() {
        if (this.inputURL != null) { return false; }
        return true;
    }

}
