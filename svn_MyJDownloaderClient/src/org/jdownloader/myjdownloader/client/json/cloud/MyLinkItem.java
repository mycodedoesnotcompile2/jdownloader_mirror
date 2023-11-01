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
package org.jdownloader.myjdownloader.client.json.cloud;

public class MyLinkItem {
    public static enum CONTENTTYPE {
        NA,
        UNKNOWN,
        PLAINTEXT,
        FILE,
        CNL2
    }
    
    private String  contentType = CONTENTTYPE.NA.name();
    
    private long    timestamp   = -1;
    
    private String  content     = null;
    
    private String  id          = null;
    
    private String  title       = null;
    private String  source      = null;
    
    private String  deviceID    = null;
    private Boolean pin         = null;
    
    public MyLinkItem(/* storable */) {
    }
    
    public CONTENTTYPE _getContentType() {
        try {
            if (this.contentType == null) { return CONTENTTYPE.NA; }
            return CONTENTTYPE.valueOf(this.contentType);
        } catch (final Throwable e) {
            return CONTENTTYPE.UNKNOWN;
        }
    }
    
    public void _setContentType(CONTENTTYPE contentType) {
        if (contentType == null) {
            contentType = CONTENTTYPE.NA;
        }
        this.contentType = contentType.name();
    }
    
    public String getContent() {
        return this.content;
    }
    
    public String getContentType() {
        return this.contentType;
    }
    
    public String getDeviceID() {
        return this.deviceID;
    }
    
    public String getId() {
        return this.id;
    }
    
    public Boolean getPin() {
        return this.pin;
    }
    
    public String getSource() {
        return this.source;
    }
    
    public long getTimestamp() {
        return this.timestamp;
    }
    
    public String getTitle() {
        return this.title;
    }
    
    public void setContent(final String content) {
        this.content = content;
    }
    
    public void setContentType(final String contentType) {
        this.contentType = contentType;
    }
    
    public void setDeviceID(final String deviceID) {
        this.deviceID = deviceID;
    }
    
    public void setId(final String id) {
        this.id = id;
    }
    
    public void setPin(final Boolean pin) {
        this.pin = pin;
    }
    
    public void setSource(final String source) {
        this.source = source;
    }
    
    public void setTimestamp(final long timestamp) {
        this.timestamp = timestamp;
    }
    
    public void setTitle(final String title) {
        this.title = title;
    }
    
}
