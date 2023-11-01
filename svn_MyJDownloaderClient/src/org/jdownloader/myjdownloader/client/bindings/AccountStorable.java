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
package org.jdownloader.myjdownloader.client.bindings;

import org.jdownloader.myjdownloader.client.json.AbstractJsonData;

public class AccountStorable extends AbstractJsonData {

    private Long    UUID = null;
    private boolean enabled;

    private boolean valid;

    private String  hostname;

    private String  username;
    private String  errorType;

    public String getErrorType() {
        return this.errorType;
    }

    public void setErrorType(final String errorType) {
        this.errorType = errorType;
    }

    public String getErrorString() {
        return this.errorString;
    }

    public void setErrorString(final String errorString) {
        this.errorString = errorString;
    }

    private String errorString;
    private Long   validUntil  = null;

    private Long   trafficLeft = null;
    private Long   trafficMax  = null;

    @SuppressWarnings("unused")
    protected AccountStorable(/* Storable */) {
    }

    public String getHostname() {
        return this.hostname;
    }

    public Long getTrafficLeft() {
        return this.trafficLeft;
    }

    public Long getTrafficMax() {
        return this.trafficMax;
    }

    public String getUsername() {
        return this.username;
    }

    public Long getUUID() {
        return this.UUID;
    }

    public Long getValidUntil() {
        return this.validUntil;
    }

    public boolean isEnabled() {
        return this.enabled;
    }

    public boolean isValid() {
        return this.valid;
    }

    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    public void setHostname(final String hostname) {
        this.hostname = hostname;
    }

    public void setTrafficLeft(final Long trafficLeft) {
        if (trafficLeft == null) {
            this.trafficLeft = null;
        } else {
            this.trafficLeft = Math.max(-1, trafficLeft);
        }
    }

    public void setTrafficMax(final Long trafficMax) {
        if (trafficMax == null) {
            this.trafficMax = null;
        } else {

            this.trafficMax = Math.max(-1, trafficMax);

        }
    }

    public void setUsername(final String username) {
        this.username = username;
    }

    public void setUUID(final Long uUID) {
        this.UUID = uUID;
    }

    public void setValid(final boolean valid) {
        this.valid = valid;
    }

    public void setValidUntil(final Long validUntil) {
        this.validUntil = validUntil;
    }

    public static enum AccountError {
        TEMP_DISABLED,
        EXPIRED,
        INVALID,
        PLUGIN_ERROR;
    }
}
