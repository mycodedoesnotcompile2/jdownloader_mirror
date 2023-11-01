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

import org.jdownloader.myjdownloader.client.exceptions.DirectConnectionException;
import org.jdownloader.myjdownloader.client.exceptions.MyJDownloaderException;
import org.jdownloader.myjdownloader.client.exceptions.UnexpectedIOException;
import org.jdownloader.myjdownloader.client.json.DirectConnectionInfo;
import org.jdownloader.myjdownloader.client.json.DirectConnectionInfos;

public class AbstractMyJDDeviceClient<GenericType> {
    
    private final AbstractMyJDClient<GenericType> api;
    private final String                          deviceID;
    private DirectConnectionInfo                  connection   = null;
    private boolean                               autoFallback = true;
    
    public AbstractMyJDDeviceClient(final String deviceID, final AbstractMyJDClient<GenericType> abstractMyJDClient) {
        this.api = abstractMyJDClient;
        this.deviceID = deviceID;
    }
    
    public Object callAction(final String action, final GenericType returnType, final Object... args) throws MyJDownloaderException {
        final DirectConnectionInfo lconnection = this.connection;
        String host = null;
        if (lconnection != null) {
            host = "http://" + lconnection.getIp() + ":" + lconnection.getPort();
        }
        try {
            return this.api.callAction(host, this.getDeviceID(), action, returnType, args);
        } catch (final MyJDownloaderException e) {
            if (this.onDirectConnectionException(lconnection, e)) {
                return this.api.callAction(null, this.getDeviceID(), action, returnType, args);
            } else {
                throw e;
            }
        }
    }
    
    public String getDeviceID() {
        return this.deviceID;
    }
    
    public DirectConnectionInfo getDirectConnectionInfo() {
        return this.connection;
    }
    
    public DirectConnectionInfos getDirectConnectionInfos() throws MyJDownloaderException {
        return this.api.getDirectConnectionInfos(this.getDeviceID());
    }
    
    public boolean isAutoFallbackEnabled() {
        return this.autoFallback;
    }
    
    protected boolean onDirectConnectionException(final DirectConnectionInfo directConnectionInfo, final MyJDownloaderException e) throws MyJDownloaderException {
        if (directConnectionInfo != null && e instanceof UnexpectedIOException) {
            if (this.isAutoFallbackEnabled() == false) {
                Throwable cause = e;
                Exception rootCause = e;
                while ((cause = cause.getCause()) != null) {
                    if (cause instanceof Exception) {
                        rootCause = (Exception) cause;
                    } else {
                        break;
                    }
                }
                throw new DirectConnectionException(rootCause);
            }
            return true;
        }
        return false;
    }
    
    public void setAutoFallbackEnabled(final boolean autoFallback) {
        this.autoFallback = autoFallback;
    }
    
    public void setDirectConnectionInfo(final DirectConnectionInfo connection) {
        this.connection = connection;
    }
    
    public boolean verifyDirectConnectionInfo(final DirectConnectionInfo connection) throws MyJDownloaderException {
        if (connection == null) { return false; }
        return this.api.verifyDirectConnectionInfo(this.getDeviceID(), connection);
    }
}
