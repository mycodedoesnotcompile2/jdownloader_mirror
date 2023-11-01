/**
 * ====================================================================================================================================================
 * "My JDownloader Client" License
 * The "My JDownloader Client" will be called [The Product] from now on.
 * ====================================================================================================================================================
 * Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 * Schwabacher Straße 117
 * 90763 Fürth
 * Germany
 * === Preamble ===
 * This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 * The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 * These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 * <p>
 * === 3rd Party Licences ===
 * Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 * to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 * <p>
 * === Definition: Commercial Usage ===
 * If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 * If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 * If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 * Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 * If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 * "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 * <p>
 * If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ====================================================================================================================================================
 */
package org.jdownloader.myjdownloader.client;

import java.util.Arrays;
import java.util.concurrent.atomic.AtomicLong;

public class SessionInfo {

    private static final int NULLHASHCODE = "EMPTYSESIONTOKEN".hashCode();

    private static boolean equals(final String a, final String b) {
        return a == b || a != null && a.equals(b);
    }

    private byte[] deviceSecret;
    private byte[] deviceEncryptionToken;
    private byte[] serverEncryptionToken;
    private String sessionToken;
    private volatile boolean valid = true;
    private final AtomicLong useCounter = new AtomicLong(0);

    private String regainToken;

    public SessionInfo(/* STorable */) {

    }

    public SessionInfo(final byte[] deviceSecret, final byte[] serverEncryptionToken, final byte[] deviceEncryptionToken, final String sessionToken, final String regainToken) {
        this.deviceSecret = deviceSecret;
        this.deviceEncryptionToken = deviceEncryptionToken;
        this.serverEncryptionToken = serverEncryptionToken;
        this.sessionToken = sessionToken;
        this.regainToken = regainToken;
    }

    @Override
    public boolean equals(final Object o) {
        boolean equals = this == o;
        equals = equals || o != null;
        equals = equals && this.getClass() == o.getClass();
        final SessionInfo other = (SessionInfo) o;
        equals = equals && Arrays.equals(this.getDeviceSecret(), other.getDeviceSecret());
        equals = equals && Arrays.equals(this.getDeviceEncryptionToken(), other.getDeviceEncryptionToken());
        equals = equals && Arrays.equals(this.getServerEncryptionToken(), other.getServerEncryptionToken());
        equals = equals && SessionInfo.equals(this.getSessionToken(), other.getSessionToken());
        equals = equals && SessionInfo.equals(this.getRegainToken(), other.getRegainToken());
        return equals;
    }

    public byte[] getDeviceEncryptionToken() {
        return this.deviceEncryptionToken;
    }

    public byte[] getDeviceSecret() {
        return this.deviceSecret;
    }

    public String getRegainToken() {
        return this.regainToken;
    }

    public byte[] getServerEncryptionToken() {
        return this.serverEncryptionToken;
    }

    public String getSessionToken() {
        return this.sessionToken;
    }

    @Override
    public int hashCode() {
        final String lsessionToken = this.sessionToken;
        if (lsessionToken != null) {
            return lsessionToken.hashCode();
        }
        return SessionInfo.NULLHASHCODE;
    }

    public void setDeviceEncryptionToken(final byte[] deviceEncryptionToken) {
        this.deviceEncryptionToken = deviceEncryptionToken;
    }

    public void setDeviceSecret(final byte[] deviceSecret) {
        this.deviceSecret = deviceSecret;
    }

    public void setRegainToken(final String regainToken) {
        this.regainToken = regainToken;
    }

    public void setServerEncryptionToken(final byte[] serverEncryptionToken) {
        this.serverEncryptionToken = serverEncryptionToken;
    }

    public void setSessionToken(final String sessionToken) {
        this.sessionToken = sessionToken;
    }

    public boolean isValid() {
        return valid;
    }

    public void setValid(boolean valid) {
        this.valid = valid;
    }



    public AtomicLong _useCounter() {
        return useCounter;
    }
}
