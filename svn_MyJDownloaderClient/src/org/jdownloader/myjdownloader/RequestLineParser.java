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
package org.jdownloader.myjdownloader;

public class RequestLineParser {
    
    public static RequestLineParser parse(final byte[] array) {
        try {
            for (int i = 0; i < array.length; i++) {
                if (array[i] == ' ') {
                    /* /t_sessiontoken(40)_deviceid(32) */
                    if (i + 2 >= array.length || array[i + 2] != 't') { return null; }
                    if (i + 3 >= array.length || array[i + 3] != '_') { return null; }
                    if (i + 44 >= array.length || array[i + 44] != '_') { return null; }
                    final String sessionToken = new String(array, i + 4, 40, "ISO-8859-1");
                    if (i + 77 >= array.length || array[i + 77] != '/') { return new RequestLineParser(null, sessionToken, null); }
                    final String deviceID = new String(array, i + 45, 32, "ISO-8859-1");
                    final int x = i + 45 + 32;
                    for (i = x; i < array.length; i++) {
                        if (array[i] == ' ' || array[i] == '?') {
                            final String requestURL = new String(array, x, i - x, "ISO-8859-1");
                            return new RequestLineParser(deviceID, sessionToken, requestURL);
                        }
                    }
                    return new RequestLineParser(deviceID, sessionToken, null);
                }
            }
        } catch (final Exception e) {
            e.printStackTrace();
        }
        return null;
    }
    
    private final String deviceID;
    
    private final String sessionToken;
    
    private final String requestURL;
    
    private RequestLineParser(final String deviceID, final String sessionToken, final String requestURL) {
        this.deviceID = deviceID;
        this.sessionToken = sessionToken;
        this.requestURL = requestURL;
    }
    
    public String getDeviceID() {
        return this.deviceID;
    }
    
    public String getRequestURL() {
        return this.requestURL;
    }
    
    public String getSessionToken() {
        return this.sessionToken;
    }
}
