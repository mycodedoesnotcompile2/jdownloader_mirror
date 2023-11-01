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
package org.appwork.remoteapi.upload;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.remoteapi.RemoteAPIRequest;
import org.appwork.remoteapi.RemoteAPIResponse;
import org.appwork.remoteapi.exceptions.BasicRemoteAPIException;
import org.appwork.remoteapi.exceptions.InternalApiException;
import org.appwork.remoteapi.exceptions.RemoteAPIException;
import org.appwork.utils.Regex;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.net.HTTPHeader;

/**
 * @author daniel
 * 
 */
public class RemoteAPIUpload implements RemoteUploadAPIInterface {

    private static enum STEP {
        CREATE,
        QUERY,
        RESUME
    }

    private final File                          uploadFolder;

    protected final HashMap<String, UploadUnit> uploadUnits = new HashMap<String, UploadUnit>();

    public RemoteAPIUpload(final File uploadFolder) {
        this.uploadFolder = uploadFolder;
    }

    public File get(final String eTag) {
        synchronized (uploadUnits) {
            final UploadUnit ret = uploadUnits.get("\"" + eTag + "\"");
            if (ret != null && ret.isComplete() && ret.isUploading() == false) {
                ret.setLastAccess(System.currentTimeMillis());
                return ret._getFile();
            }
        }
        return null;
    }

    public File getUploadFolder() {
        return uploadFolder;
    }

    @Override
    public List<UploadUnit> list() {
        synchronized (uploadUnits) {
            return new ArrayList<UploadUnit>(uploadUnits.values());
        }
    }

    /**
     * @param uploadUnit
     */
    protected void onComplete(final UploadUnit uploadUnit) {
    }

    protected void onCreate(final UploadUnit uploadUnit) {
    }

    /**
     * @param uploadUnit
     */
    protected void onResume(final UploadUnit uploadUnit) {
    }

    @Override
    public boolean remove(final String eTag) {
        synchronized (uploadUnits) {
            return uploadUnits.remove("\"" + eTag + "\"") != null;
        }
    }

    @Override
    public void uploadFile(final RemoteAPIRequest request, final RemoteAPIResponse response) throws BasicRemoteAPIException {
        UploadUnit uploadUnit = null;
        STEP step = null;
        boolean processUpload = false;
        RandomAccessFile fos = null;
        HTTPHeader contentRange = null;
        try {
            synchronized (uploadUnits) {
                final HTTPHeader ifMatch = request.getRequestHeaders().get(HTTPConstants.HEADER_REQUEST_IF_MATCH);
                final HTTPHeader contentLength = request.getRequestHeaders().get(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH);
                contentRange = request.getRequestHeaders().get(HTTPConstants.HEADER_RESPONSE_CONTENT_RANGE);
                long contentLengthLong = -1;
                long contentSize = -1;
                if (contentLength != null) {
                    contentLengthLong = Long.parseLong(contentLength.getValue());
                } else {
                    contentLengthLong = 0;
                }
                if (contentRange != null) {
                    final String contentSizeString = new Regex(contentRange.getValue(), ".*?/\\s*?(\\d+)").getMatch(0);
                    if (contentSizeString != null) {
                        contentSize = Long.parseLong(contentSizeString);
                    }
                }
                if (ifMatch != null) {
                    /* check for existing UploadUnit */
                    uploadUnit = uploadUnits.get(ifMatch.getValue());
                }
                if (uploadUnit == null) {
                    step = STEP.CREATE;
                } else {
                    /* upload Unit does still exist */
                    if (contentLengthLong == 0) {
                        step = STEP.QUERY;
                    } else {
                        step = STEP.RESUME;
                    }
                }
                switch (step) {
                case QUERY:
                    if (uploadUnit.getExpectedFinalSize() != contentSize) {
                        /* size missmatch, so not found */
                        throw new RemoteAPIException(UploadError.SIZE_MISMATCH);
                    }
                    /* add ETag Header */
                    response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_ETAG, uploadUnit._getQuotedETag()));
                    uploadUnit.setLastAccess(System.currentTimeMillis());
                    if (uploadUnit.isComplete()) {
                        /* upload is complete */
                        response.setResponseCode(ResponseCode.SUCCESS_OK);
                    } else {
                        /*
                         * add Range Header to signal current received
                         * contentSize
                         */
                        if (uploadUnit.getSize() != 0) {
                            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_REQUEST_RANGE, "0-" + (uploadUnit.getSize() - 1)));
                        }
                        /* upload is still incomplete */
                        response.setResponseCode(ResponseCode.RESUME_INCOMPLETE);
                    }
                    return;
                case CREATE:
                    if (ifMatch != null) {
                        /* given ETag no longer available */
                        throw new RemoteAPIException(UploadError.ETAG_NOT_FOUND);
                    }
                    /* upload is still incomplete */
                    if (contentSize <= 0) {
                        /* no or invalid contentSize given */
                        throw new RemoteAPIException(UploadError.BAD_REQUEST);
                    }
                    uploadUnit = new UploadUnit(contentSize);
                    /* add ETag Header */
                    response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_ETAG, uploadUnit._getQuotedETag()));
                    response.setResponseCode(ResponseCode.RESUME_INCOMPLETE);
                    uploadUnit.setLastAccess(System.currentTimeMillis());
                    final File uploadFile = new File(uploadFolder, uploadUnit.getETag());
                    uploadUnit._setFile(uploadFile);
                    uploadUnits.put(uploadUnit._getQuotedETag(), uploadUnit);
                    onCreate(uploadUnit);
                    return;
                case RESUME:
                    if (uploadUnit.getExpectedFinalSize() != contentSize) {
                        /* size missmatch, so not found */
                        throw new RemoteAPIException(UploadError.SIZE_MISMATCH);
                    }
                    if (uploadUnit.isUploading()) {
                        /* file is already in process */
                        throw new RemoteAPIException(UploadError.UPLOAD_IN_PROGRESS);
                    }
                    /* add ETag Header */
                    response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_ETAG, uploadUnit._getQuotedETag()));
                    uploadUnit.setLastAccess(System.currentTimeMillis());
                    processUpload = true;
                    uploadUnit.setUploading(true);
                    onResume(uploadUnit);
                }
            }
            /* now we handle the upload */
            final MessageDigest md = MessageDigest.getInstance("SHA-1");
            fos = new RandomAccessFile(uploadUnit._getFile(), "rw");
            if (contentRange != null) {
                final String startRange = new Regex(contentRange.getValue(), "^\\s*?bytes\\s*?(\\d+)").getMatch(0);
                if (startRange != null) {
                    final long start = Long.parseLong(startRange);
                    if (start > uploadUnit.getExpectedFinalSize()) { throw new RemoteAPIException(UploadError.BAD_RANGE); }
                    fos.seek(start);
                }
            } else {
                fos.seek(0);
            }
            final byte[] buffer = new byte[32767];
            int read = 0;
            final InputStream is = request.getInputStream();
            while ((read = is.read(buffer)) != -1) {
                if (read > 0) {
                    fos.write(buffer, 0, read);
                    md.update(buffer, 0, read);
                }
            }
            fos.close();
            uploadUnit.setLastAccess(System.currentTimeMillis());
            final String chunkHash = HexFormatter.byteArrayToHex(md.digest());
            if (uploadUnit.isComplete()) {
                /* upload is complete */
                onComplete(uploadUnit);
                response.setResponseCode(ResponseCode.SUCCESS_OK);
            } else {
                /* add Range Header to signal current received contentSize */
                if (uploadUnit.getSize() != 0) {
                    response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_REQUEST_RANGE, "0-" + (uploadUnit.getSize() - 1)));
                }
                /* upload is still incomplete */
                response.setResponseCode(ResponseCode.RESUME_INCOMPLETE);
            }
            final OutputStream os = response.getOutputStream(true);
            os.write(chunkHash.getBytes("UTF-8"));
        } catch (final Throwable e) {
            if (e instanceof RemoteAPIException) { throw (RemoteAPIException) e; }
            throw new InternalApiException(e);
        } finally {
            try {
                response.getOutputStream(true).close();
            } catch (final Throwable e) {
            }
            try {
                fos.close();
            } catch (final Throwable e) {
            }
            if (processUpload && uploadUnit != null) {
                uploadUnit.setUploading(false);
            }
        }
    }
}
