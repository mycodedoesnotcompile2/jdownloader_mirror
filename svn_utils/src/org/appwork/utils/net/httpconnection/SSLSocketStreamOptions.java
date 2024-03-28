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
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpconnection;

import java.net.SocketException;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.net.ssl.SSLException;
import javax.net.ssl.SSLHandshakeException;

import org.appwork.utils.Application;
import org.appwork.utils.Exceptions;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.CrossSystem.ARCHFamily;

/**
 * @author daniel
 * @date Aug 9, 2019
 *
 */
public class SSLSocketStreamOptions implements Cloneable {
    protected final boolean trustAllFlag;

    public List<String> getRetryReasons() {
        return retryReasons;
    }

    protected final AtomicBoolean sniEnabled = new AtomicBoolean(true);
    protected final AtomicBoolean valid      = new AtomicBoolean(true);
    protected final String        id;

    public boolean isValid() {
        return valid.get();
    }

    public boolean isSNIEnabled() {
        return sniEnabled.get();
    }

    public List<String> getPreferredCipherSuites() {
        return preferredCipherSuites;
    }

    public List<String> getAvoidedCipherSuites() {
        return avoidedCipherSuites;
    }

    protected final List<String> retryReasons          = new CopyOnWriteArrayList<String>();
    protected final List<String> preferredCipherSuites = new CopyOnWriteArrayList<String>();
    protected final List<String> avoidedCipherSuites   = new CopyOnWriteArrayList<String>();
    protected final Set<String>  disabledCipherSuites  = new CopyOnWriteArraySet<String>();
    protected final Set<String>  enabledCipherSuites   = new CopyOnWriteArraySet<String>();
    protected final Set<String>  customFactorySettings = new CopyOnWriteArraySet<String>();

    public Set<String> getCustomFactorySettings() {
        return customFactorySettings;
    }

    public Set<String> getEnabledCipherSuites() {
        return enabledCipherSuites;
    }

    public String getId() {
        return id;
    }

    public SSLSocketStreamOptions(final String id, final boolean trustFlag) {
        this.id = id;
        this.trustAllFlag = trustFlag;
        this.sslSocketStreamFactory = null;
        initCipherSuitesLists();
    }

    public SSLSocketStreamOptions(SSLSocketStreamOptions importFrom) {
        id = importFrom.getId();
        trustAllFlag = importFrom.isTrustAll();
        sniEnabled.set(importFrom.isSNIEnabled());
        valid.set(importFrom.isValid());
        preferredCipherSuites.addAll(importFrom.getPreferredCipherSuites());
        avoidedCipherSuites.addAll(importFrom.getAvoidedCipherSuites());
        disabledCipherSuites.addAll(importFrom.getDisabledCipherSuites());
        enabledCipherSuites.addAll(importFrom.getEnabledCipherSuites());
        customFactorySettings.addAll(importFrom.getCustomFactorySettings());
        retryReasons.addAll(importFrom.getRetryReasons());
        this.sslSocketStreamFactory = importFrom.getSSLSocketStreamFactory();
    }

    public boolean hasCipherSuitesPreferences() {
        return getAvoidedCipherSuites().size() > 0 || getPreferredCipherSuites().size() > 0;
    }

    public String[] sortCipherSuites(String[] cipherSuites) {
        return cipherSuites;
    }

    protected final SSLSocketStreamFactory sslSocketStreamFactory;

    public SSLSocketStreamFactory getSSLSocketStreamFactory() {
        return sslSocketStreamFactory;
    }

    protected final long JAVA_1_8_261 = JVMVersion.parseJavaVersionString("1.8.0_261");
    protected final long JAVA_1_8_272 = JVMVersion.parseJavaVersionString("1.8.0_272");
    protected final long JAVA_11_12   = JVMVersion.parseJavaVersionString("11.0.12");

    protected void initCipherSuitesLists() {
        // https://stackoverflow.com/questions/48905291/java-9-aes-gcm-performance/60335761
        // https://stackoverflow.com/questions/25992131/slow-aes-gcm-encryption-and-decryption-with-java-8u20/27028067
        // https://stackoverflow.com/questions/60341295/java-still-terrible-gcm-performance
        final boolean gcmFixed;
        if (JVMVersion.isMinimum(JVMVersion.JAVA_1_8) && JVMVersion.get() < JVMVersion.JAVA_9) {
            // JVM 1.8 backports, https://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8201633
            if (StringUtils.containsIgnoreCase(System.getProperty("java.runtime.name"), "OpenJDK")) {
                gcmFixed = JVMVersion.isMinimum(JAVA_1_8_272);
            } else if (StringUtils.containsIgnoreCase(System.getProperty("java.runtime.name"), "Java(TM)")) {
                gcmFixed = JVMVersion.isMinimum(JAVA_1_8_261);
            } else {
                gcmFixed = false;
            }
        } else {
            gcmFixed = JVMVersion.isMinimum(JAVA_11_12);
        }
        final ARCHFamily arch = CrossSystem.getARCHFamily();
        final boolean hardwareAESSupport = ARCHFamily.X86.equals(arch) || (ARCHFamily.ARM.equals(arch) && Application.is64BitJvm());
        if (!gcmFixed || !hardwareAESSupport) {
            // servers with 'server-preferred order'
            disabledCipherSuites.add("AES_128_GCM");
            disabledCipherSuites.add("GCM");
            // avoid GCM cipher
            avoidedCipherSuites.add("AES_128_GCM");
            avoidedCipherSuites.add("GCM");
            switch (arch) {
            case ARM:
                // lightweight cipher
                preferredCipherSuites.add("CHACHA20");
                break;
            default:
                break;
            }
        }
    }

    public Set<String> getDisabledCipherSuites() {
        return disabledCipherSuites;
    }

    public boolean isTrustAll() {
        return trustAllFlag;
    }

    @Override
    public SSLSocketStreamOptions clone() {
        return new SSLSocketStreamOptions(this);
    }

    public String enableNextDisabledCipher(final String cipher) {
        final Iterator<String> it = disabledCipherSuites.iterator();
        while (it.hasNext()) {
            final String next = it.next();
            if (StringUtils.containsIgnoreCase(next, cipher) && disabledCipherSuites.remove(next)) {
                enabledCipherSuites.add(next);
                return next;
            }
        }
        return null;
    }

    public String addRetryReason(final String reason) {
        retryReasons.add(reason);
        return reason;
    }

    protected void removeRetryReason(final String reason) {
        retryReasons.remove(reason);
    }

    public boolean isHandshakeException(final Exception e) {
        return e != null && (Exceptions.containsInstanceOf(e, SSLHandshakeException.class) || StringUtils.containsIgnoreCase(e.getMessage(), "Received close_notify during handshake") || StringUtils.containsIgnoreCase(e.getMessage(), "handshake_failure") || StringUtils.containsIgnoreCase(e.getMessage(), "Remote host terminated the handshake") || StringUtils.containsIgnoreCase(e.getMessage(), "Remote host closed connection during handshake"));
    }

    public boolean isTLSConfigurationException(final Exception e) {
        return e != null && (StringUtils.containsIgnoreCase(e.getMessage(), "protocol_version") || StringUtils.containsIgnoreCase(e.getMessage(), "cipher suites are inappropriate") || StringUtils.containsIgnoreCase(e.getMessage(), "No appropriate protocol"));
    }

    public boolean isConnectionResetException(final Exception e) {
        return e != null && (Exceptions.containsInstanceOf(e, SSLException.class, SocketException.class) && (StringUtils.containsIgnoreCase(e.getMessage(), "reset")));
    }

    public String retry(final SSLSocketStreamFactory factory, final Exception e) {
        if (valid.get()) {
            final String eMessage = e != null ? e.getMessage() : null;
            if (StringUtils.containsIgnoreCase(eMessage, "unrecognized_name")) {
                if (sniEnabled.compareAndSet(true, false)) {
                    return addRetryReason("disable SNI");
                }
            }
            final String factoryRetry = factory != null ? factory.retry(this, e) : null;
            if (factoryRetry != null) {
                removeRetryReason(factoryRetry);
                return addRetryReason("Factory:" + factory + "=" + factoryRetry);
            }
            if (eMessage != null) {
                if (eMessage.matches("(?i).*\\s+DH\\s*key\\s*pair.*")) {
                    if (disabledCipherSuites.add("_DHE_")) {
                        return addRetryReason("(KeyPair(DH))disable cipher:DHE");
                    } else if (disabledCipherSuites.add("_ECDHE_")) {
                        return addRetryReason("(KeyPair(DH))disable cipher:ECDHE");
                    } else if (disabledCipherSuites.add("_ECDH_")) {
                        return addRetryReason("(KeyPair(DH))disable cipher:ECDH");
                    }
                } else if (eMessage.matches("(?i).*\\s+ECDH\\s*key\\s*pair.*")) {
                    if (disabledCipherSuites.add("_ECDHE_")) {
                        return addRetryReason("(KeyPair(ECDH))disable cipher:ECDHE");
                    } else if (disabledCipherSuites.add("_ECDH_")) {
                        return addRetryReason("(KeyPair(ECDH))disable cipher:ECDH");
                    }
                } else if (eMessage.matches("(?i).*(on|in)\\s+DH.*") || eMessage.matches("(?i).*\\s+DH\\s*Server\\s*Key\\s*Exchange.*")) {
                    if (disabledCipherSuites.add("_DHE_")) {
                        return addRetryReason("(ServerKeyExchange(DH))disable cipher:DHE");
                    }
                } else if (eMessage.matches("(?i).*(on|in)\\s+ECDH.*") || eMessage.matches("(?i).*\\s+ECDH\\s*Server\\s*Key\\s*Exchange.*")) {
                    if (disabledCipherSuites.add("_ECDHE_")) {
                        return addRetryReason("(ServerKeyExchange(ECDH))disable cipher:ECDHE");
                    } else if (disabledCipherSuites.add("_ECDH_")) {
                        return addRetryReason("(ServerKeyExchange(ECDH))disable cipher:ECDH");
                    }
                }
            }
            if (isTLSConfigurationException(e)) {
                // enable next disabled GCM cipherSuite and retry
                final String enabledCipher = enableNextDisabledCipher("GCM");
                if (enabledCipher != null) {
                    return addRetryReason("(TLS)enable cipher:" + enabledCipher);
                }
            }
            if (isHandshakeException(e)) {
                // enable next disabled GCM cipherSuite and retry
                final String enabledCipher = enableNextDisabledCipher("GCM");
                if (enabledCipher != null) {
                    return addRetryReason("(Handshake)enable cipher:" + enabledCipher);
                }
            }
            if (isConnectionResetException(e)) {
                // enable next disabled GCM cipherSuite and retry
                final String enabledCipher = enableNextDisabledCipher("GCM");
                if (enabledCipher != null) {
                    return addRetryReason("(Reset)enable cipher:" + enabledCipher);
                }
            }
            valid.set(false);
        }
        return null;
    }
}
