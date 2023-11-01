/**
 * 
 * ====================================================================================================================================================
 *         "My JDownloader Client" License
 *         The "My JDownloader Client" will be called [The Product] from now on.
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
package org.jdownloader.myjdownloader.client;

import java.util.Map;

import org.jdownloader.myjdownloader.client.exceptions.ExceptionResponse;
import org.jdownloader.myjdownloader.client.exceptions.MyJDownloaderException;
import org.jdownloader.myjdownloader.client.exceptions.UnexpectedIOException;
import org.jdownloader.myjdownloader.client.exceptions.storage.StorageAlreadyExistsException;
import org.jdownloader.myjdownloader.client.exceptions.storage.StorageInvalidIDException;
import org.jdownloader.myjdownloader.client.exceptions.storage.StorageInvalidKeyException;
import org.jdownloader.myjdownloader.client.exceptions.storage.StorageKeyNotFoundException;
import org.jdownloader.myjdownloader.client.exceptions.storage.StorageLimitReachedException;
import org.jdownloader.myjdownloader.client.exceptions.storage.StorageNotFoundException;
import org.jdownloader.myjdownloader.client.json.ErrorResponse;
import org.jdownloader.myjdownloader.client.json.JSonRequest;
import org.jdownloader.myjdownloader.client.json.RequestIDOnly;
import org.jdownloader.myjdownloader.client.json.ServerErrorType;
import org.jdownloader.myjdownloader.client.json.storage.StorageGetValueResponse;
import org.jdownloader.myjdownloader.client.json.storage.StorageListResponse;

public class StorageMyJDClient<GenericType> {
    
    private final AbstractMyJDClient<GenericType> api;
    
    public StorageMyJDClient(final AbstractMyJDClient<GenericType> abstractMyJDClient) {
        this.api = abstractMyJDClient;
    }
    
    protected <T> T callServer(final String query, final JSonRequest jsonRequest, final SessionInfo session, final Class<T> class1) throws MyJDownloaderException {
        try {
            return this.api.callServer(query, jsonRequest, session, class1);
        } catch (final UnexpectedIOException e) {
            if (e.getCause() instanceof ExceptionResponse) {
                ErrorResponse error = null;
                try {
                    final ExceptionResponse cause = (ExceptionResponse) e.getCause();
                    error = this.api.jsonToObject(cause.getContent(), (GenericType) ErrorResponse.class);
                } catch (final Throwable e2) {
                }
                if (error != null) {
                    switch (error.getSrc()) {
                        case MYJD:
                            final ServerErrorType type = ServerErrorType.valueOf(error.getType());
                            switch (type) {
                                case STORAGE_ALREADY_EXISTS:
                                    throw new StorageAlreadyExistsException();
                                case STORAGE_INVALID_KEY:
                                    throw new StorageInvalidKeyException();
                                case STORAGE_INVALID_STORAGEID:
                                    throw new StorageInvalidIDException();
                                case STORAGE_NOT_FOUND:
                                    throw new StorageNotFoundException();
                                case STORAGE_KEY_NOT_FOUND:
                                    throw new StorageKeyNotFoundException();
                                case STORAGE_LIMIT_REACHED:
                                    throw new StorageLimitReachedException();
                            }
                    }
                }
            }
            throw e;
        }
    }
    
    public void create(final String storageID) throws MyJDownloaderException, StorageAlreadyExistsException, StorageInvalidIDException {
        final SessionInfo sessionInfo = this.api.getSessionInfo();
        final String url = "/storage/createstorage?sessiontoken=" + this.api.urlencode(sessionInfo.getSessionToken());
        final JSonRequest re = new JSonRequest();
        re.setParams(new Object[] { storageID });
        re.setUrl(url);
        this.callServer(url, re, sessionInfo, RequestIDOnly.class);
    }
    
    public void drop(final String storageID) throws MyJDownloaderException, StorageNotFoundException, StorageInvalidIDException {
        final SessionInfo sessionInfo = this.api.getSessionInfo();
        final String url = "/storage/dropstorage?sessiontoken=" + this.api.urlencode(sessionInfo.getSessionToken());
        final JSonRequest request = new JSonRequest();
        request.setParams(new Object[] { storageID });
        request.setUrl(url);
        this.callServer(url, request, sessionInfo, RequestIDOnly.class);
    }
    
    public String getValue(final String storageID, final String key) throws MyJDownloaderException {
        final SessionInfo sessionInfo = this.api.getSessionInfo();
        final String url = "/storage/getvalue?sessiontoken=" + this.api.urlencode(sessionInfo.getSessionToken());
        final JSonRequest request = new JSonRequest();
        request.setParams(new Object[] { storageID, key });
        request.setUrl(url);
        return this.callServer(url, request, sessionInfo, StorageGetValueResponse.class).getValue();
    }
    
    public Map<String, Long> list() throws MyJDownloaderException {
        final SessionInfo sessionInfo = this.api.getSessionInfo();
        final String url = "/storage/liststorages?sessiontoken=" + this.api.urlencode(sessionInfo.getSessionToken());
        return this.callServer(url, null, sessionInfo, StorageListResponse.class).getList();
    }
    
    public Map<String, Long> listKeys(final String storageID) throws MyJDownloaderException {
        final SessionInfo sessionInfo = this.api.getSessionInfo();
        final String url = "/storage/listkeys?sessiontoken=" + this.api.urlencode(sessionInfo.getSessionToken());
        final JSonRequest request = new JSonRequest();
        request.setParams(new Object[] { storageID });
        request.setUrl(url);
        return this.callServer(url, request, sessionInfo, StorageListResponse.class).getList();
    }
    
    public void putValue(final String storageID, final String key, final String value) throws MyJDownloaderException {
        final SessionInfo sessionInfo = this.api.getSessionInfo();
        final String url = "/storage/putvalue?sessiontoken=" + this.api.urlencode(sessionInfo.getSessionToken());
        final JSonRequest request = new JSonRequest();
        request.setParams(new Object[] { storageID, key, value });
        request.setUrl(url);
        this.callServer(url, request, sessionInfo, RequestIDOnly.class);
    }
    
    public void removeKey(final String storageID, final String key) throws MyJDownloaderException {
        final SessionInfo sessionInfo = this.api.getSessionInfo();
        final String url = "/storage/removekey?sessiontoken=" + this.api.urlencode(sessionInfo.getSessionToken());
        final JSonRequest request = new JSonRequest();
        request.setParams(new Object[] { storageID, key });
        request.setUrl(url);
        this.callServer(url, request, sessionInfo, RequestIDOnly.class);
    }
    
}
