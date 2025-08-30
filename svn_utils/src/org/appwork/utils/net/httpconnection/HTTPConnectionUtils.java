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
package org.appwork.utils.net.httpconnection;

import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.math.BigInteger;
import java.net.Inet4Address;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.NetworkInterface;
import java.net.URLDecoder;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.security.cert.Certificate;
import java.security.cert.CertificateParsingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLPeerUnverifiedException;
import javax.net.ssl.SSLSession;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.utils.Application;
import org.appwork.utils.JDK8BufferHelper;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.encoding.RFC2047;
import org.appwork.utils.net.CountingInputStreamInterface;

public class HTTPConnectionUtils {
    public static enum IPVERSION {
        IPV4_ONLY,
        IPV4_IPV6,
        IPV6_IPV4,
        /**
         * see https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/net/doc-files/net-properties.html
         */
        SYSTEM;
    }

    public static InetSocketAddress removeHostName(InetSocketAddress addr) {
        if (addr == null || addr.getAddress() == null) {
            return addr;
        } else {
            return new InetSocketAddress(addr.getAddress(), addr.getPort());
        }
    }

    public static String debug(CountingInputStreamInterface countingInputStream) {
        if (countingInputStream == null) {
            return null;
        }
        final StringBuilder sb = new StringBuilder();
        CountingInputStreamInterface next = countingInputStream;
        while (next != null) {
            sb.append(next.getClass().getName()).append("=").append(next.transferedBytes());
            InputStream parent = next.getParentInputStream();
            if (parent instanceof CountingInputStreamInterface) {
                next = (CountingInputStreamInterface) parent;
                sb.append("->");
            } else {
                break;
            }
        }
        return sb.toString();
    }

    public static Boolean verifySSLHostname(HostnameVerifier hostNameVerifier, final SSLSession sslSession, final String host) throws IOException {
        try {
            if (hostNameVerifier == null) {
                hostNameVerifier = HttpsURLConnection.getDefaultHostnameVerifier();
            }
            final Boolean hostNameVerifierResult = hostNameVerifier != null ? hostNameVerifier.verify(host, sslSession) : null;
            if (sslSession != null && sslSession.getPeerCertificates().length > 0) {
                final Certificate certificate = sslSession.getPeerCertificates()[0];
                if (certificate instanceof X509Certificate) {
                    final String hostname = host.toLowerCase(Locale.ENGLISH);
                    final HashSet<String> subjects = new HashSet<String>();
                    final X509Certificate x509 = (X509Certificate) certificate;
                    subjects.add(new Regex(x509.getSubjectX500Principal().getName(), "CN\\s*=\\s*(.*?)(,| |$)").getMatch(0));
                    try {
                        final Collection<List<?>> subjectAlternativeNames = x509.getSubjectAlternativeNames();
                        if (subjectAlternativeNames != null) {
                            for (final List<?> subjectAlternativeName : subjectAlternativeNames) {
                                final Integer generalNameType = (Integer) subjectAlternativeName.get(0);
                                switch (generalNameType) {
                                case 1:// rfc822Name
                                case 2:// dNSName
                                    subjects.add(subjectAlternativeName.get(1).toString());
                                    break;
                                }
                            }
                        }
                    } catch (CertificateParsingException e) {
                        e.printStackTrace();
                    }
                    subjects.remove(null);
                    boolean result = false;
                    for (String subject : subjects) {
                        subject = subject.toLowerCase(Locale.ENGLISH);
                        if (StringUtils.equals(subject, hostname)) {
                            result = true;
                            break;
                        } else if (subject.startsWith("*.") && hostname.length() > subject.length() - 1 && hostname.endsWith(subject.substring(1)) && hostname.substring(0, hostname.length() - subject.length() + 1).indexOf('.') < 0) {
                            /**
                             * http://en.wikipedia.org/wiki/ Wildcard_certificate
                             */
                            result = true;
                            break;
                        }
                    }
                    if (hostNameVerifierResult != null) {
                        if (hostNameVerifierResult.booleanValue() != result) {
                            org.appwork.loggingv3.LogV3.severe("Please check NativeSSLSocketStreamFactory.verifySSLHostname for " + host + "|HostnameVerifier<" + hostNameVerifierResult + "> != verifySSLHostname<" + result + ">");
                        }
                    }
                    if (!result) {
                        throw new IOException("HTTPS hostname wrong:  should be <" + host + "> != " + subjects);
                    } else {
                        return true;
                    }
                }
            }
            if (hostNameVerifierResult != null) {
                return hostNameVerifierResult.booleanValue();
            } else {
                return null;
            }
        } catch (SSLPeerUnverifiedException e) {
            return null;
        }
    }

    private static final AtomicReference<Object[]> lastIPv6EnvironmentResult = new AtomicReference<Object[]>();

    /**
     * https://en.wikipedia.org/wiki/6to4
     *
     * @param ip
     * @return
     */
    public static boolean is6to4Address(final Inet6Address ip) {
        final byte[] raw = ip != null ? ip.getAddress() : null;
        return raw != null && raw.length == 16 && (raw[0] == (byte) 0x20) && (raw[1] == (byte) 0x02);
    }

    /**
     * https://www.rfc-editor.org/rfc/rfc4193.html
     *
     * @param ip
     * @return
     */
    public static boolean isUniqueLocalUnicast(final Inet6Address ip) {
        final byte[] raw = ip != null ? ip.getAddress() : null;
        // L Set to 1 if the prefix is locally assigned. -> 0xfd
        return raw != null && raw.length == 16 && (raw[0] == (byte) 0xfd);
    }

    private static Map<InetAddress, Integer> GUA          = new HashMap<InetAddress, Integer>();
    private static Map<InetAddress, Integer> RESERVED_GUA = new HashMap<InetAddress, Integer>();
    static {
        try {
            // https://www.iana.org/assignments/ipv6-unicast-address-assignments/ipv6-unicast-address-assignments.xhtml
            GUA.put(InetAddress.getByName("2001:0200::"), 23);
            GUA.put(InetAddress.getByName("2001:0400::"), 23);
            GUA.put(InetAddress.getByName("2001:0600::"), 23);
            GUA.put(InetAddress.getByName("2001:0800::"), 22);
            GUA.put(InetAddress.getByName("2001:0c00::"), 23);
            RESERVED_GUA.put(InetAddress.getByName("2001:db8::"), 32);
            GUA.put(InetAddress.getByName("2001:0e00::"), 23);
            GUA.put(InetAddress.getByName("2001:1200::"), 23);
            GUA.put(InetAddress.getByName("2001:1400::"), 22);
            GUA.put(InetAddress.getByName("2001:1800::"), 23);
            GUA.put(InetAddress.getByName("2001:1a00::"), 23);
            GUA.put(InetAddress.getByName("2001:1c00::"), 22);
            GUA.put(InetAddress.getByName("2001:2000::"), 19);
            GUA.put(InetAddress.getByName("2001:4000::"), 23);
            GUA.put(InetAddress.getByName("2001:4200::"), 23);
            GUA.put(InetAddress.getByName("2001:4400::"), 23);
            GUA.put(InetAddress.getByName("2001:4600::"), 23);
            GUA.put(InetAddress.getByName("2001:4800::"), 23);
            GUA.put(InetAddress.getByName("2001:4a00::"), 23);
            GUA.put(InetAddress.getByName("2001:4c00::"), 23);
            GUA.put(InetAddress.getByName("2001:5000::"), 23);
            GUA.put(InetAddress.getByName("2001:8000::"), 19);
            GUA.put(InetAddress.getByName("2001:a000::"), 20);
            GUA.put(InetAddress.getByName("2001:b000::"), 20);
            GUA.put(InetAddress.getByName("2003:0000::"), 18);
            GUA.put(InetAddress.getByName("2400:0000::"), 12);
            GUA.put(InetAddress.getByName("2600:0000::"), 12);
            GUA.put(InetAddress.getByName("2610:0000::"), 23);
            GUA.put(InetAddress.getByName("2620:0000::"), 23);
            GUA.put(InetAddress.getByName("2630:0000::"), 23);
            GUA.put(InetAddress.getByName("2800:0000::"), 12);
            GUA.put(InetAddress.getByName("2a00:0000::"), 12);
            GUA.put(InetAddress.getByName("2a10:0000::"), 12);
            GUA.put(InetAddress.getByName("2c00:0000::"), 12);
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }

    /**
     * https://www.iana.org/assignments/ipv6-unicast-address-assignments/ipv6-unicast-address-assignments.xhtml
     *
     * @param ip
     * @return
     */
    public static boolean isGlobalUnicast(final Inet6Address ip) {
        final byte[] raw = ip != null ? ip.getAddress() : null;
        if (raw != null && raw.length == 16) {
            try {
                for (Entry<InetAddress, Integer> gua : RESERVED_GUA.entrySet()) {
                    if (contains(gua.getKey(), gua.getValue(), ip)) {
                        return false;
                    }
                }
                for (Entry<InetAddress, Integer> gua : GUA.entrySet()) {
                    if (contains(gua.getKey(), gua.getValue(), ip)) {
                        return true;
                    }
                }
            } catch (Throwable e) {
                e.printStackTrace();
            }
        }
        return false;
    }

    private static boolean contains(InetAddress network, int prefixLength, InetAddress address) {
        if (network == null || network.getAddress() == null) {
            return false;
        } else if (address == null || address.getAddress() == null) {
            return false;
        } else if (network.getAddress().length != address.getAddress().length) {
            return false;
        } else if (network.getAddress().length != 4 && network.getAddress().length != 16) {
            return false;
        } else {
            final int bits = 8 * network.getAddress().length;
            if (prefixLength < 1 || prefixLength > bits) {
                return false;
            }
            final BigInteger n = new BigInteger(1, network.getAddress());
            final BigInteger a = new BigInteger(1, address.getAddress());
            return n.shiftRight(bits - prefixLength).compareTo(a.shiftRight(bits - prefixLength)) == 0;
        }
    }

    /**
     * https://de.wikipedia.org/wiki/IPv6#Global_Unicast
     *
     * @param verifyTimeout
     * @param searchTimeout
     * @param cacheTimeout
     * @return
     */
    public static Boolean isGlobalIPv6Available(final int verifyTimeout, final int searchTimeout, final long cacheTimeout) {
        synchronized (lastIPv6EnvironmentResult) {
            Object[] current = lastIPv6EnvironmentResult.get();
            if (current != null) {
                if (Time.systemIndependentCurrentJVMTimeMillis() - (Long) current[1] < cacheTimeout) {
                    return current[0] != null ? Boolean.TRUE : Boolean.FALSE;
                } else if (current[0] != null) {
                    final Object[] verify = current;
                    final Thread thread = new Thread("isGlobalIPv6Available:verify:" + verify[0]) {
                        public void run() {
                            try {
                                final NetworkInterface netif = NetworkInterface.getByInetAddress((InetAddress) verify[0]);
                                if (netif != null && netif.isUp()) {
                                    final Object[] globalIPv6 = new Object[] { verify[0], Time.systemIndependentCurrentJVMTimeMillis() };
                                    lastIPv6EnvironmentResult.set(globalIPv6);
                                }
                            } catch (IOException ignore) {
                            } finally {
                                lastIPv6EnvironmentResult.compareAndSet(verify, null);
                            }
                        };
                    };
                    thread.setDaemon(true);
                    thread.start();
                    if (verifyTimeout >= 0) {
                        try {
                            thread.join(verifyTimeout);
                        } catch (InterruptedException ignore) {
                        }
                    }
                    current = lastIPv6EnvironmentResult.get();
                    if (current != null && current != verify && current[0] != null) {
                        return Boolean.TRUE;
                    }
                }
            }
            final Object[] search = current;
            final Thread thread = new Thread("isGlobalIPv6Available:search") {
                @Override
                public void run() {
                    try {
                        final Enumeration<NetworkInterface> networkInterfaces = NetworkInterface.getNetworkInterfaces();
                        while (networkInterfaces.hasMoreElements()) {
                            if (lastIPv6EnvironmentResult.get() != search) {
                                return;
                            }
                            final NetworkInterface networkInterface = networkInterfaces.nextElement();
                            try {
                                if (!networkInterface.isLoopback() && networkInterface.isUp()) {
                                    final Enumeration<InetAddress> inetAddress = networkInterface.getInetAddresses();
                                    while (inetAddress.hasMoreElements()) {
                                        final InetAddress next = inetAddress.nextElement();
                                        if (next instanceof Inet6Address) {
                                            final Inet6Address ipv6 = (Inet6Address) next;
                                            if (!ipv6.isLinkLocalAddress() && !ipv6.isSiteLocalAddress() && !is6to4Address(ipv6) && !isUniqueLocalUnicast(ipv6)) {
                                                if (isGlobalUnicast(ipv6)) {
                                                    final Object[] globalIPv6 = new Object[] { ipv6, Time.systemIndependentCurrentJVMTimeMillis() };
                                                    lastIPv6EnvironmentResult.set(globalIPv6);
                                                    return;
                                                }
                                            }
                                        }
                                    }
                                }
                            } catch (IOException ignore) {
                            }
                        }
                        final Object[] noGlobalIPv6 = new Object[] { null, Time.systemIndependentCurrentJVMTimeMillis() };
                        lastIPv6EnvironmentResult.compareAndSet(search, noGlobalIPv6);
                    } catch (IOException ignore) {
                    } finally {
                        lastIPv6EnvironmentResult.compareAndSet(search, null);
                    }
                }
            };
            thread.setDaemon(true);
            thread.start();
            if (searchTimeout >= 0) {
                try {
                    thread.join(searchTimeout);
                } catch (InterruptedException ignore) {
                }
            }
            if (thread.isAlive()) {
                final Object[] noGlobalIPv6 = new Object[] { null, Time.systemIndependentCurrentJVMTimeMillis() };
                lastIPv6EnvironmentResult.compareAndSet(search, noGlobalIPv6);
            }
            current = lastIPv6EnvironmentResult.get();
            if (current != null && search != current) {
                return current[0] != null ? Boolean.TRUE : Boolean.FALSE;
            } else {
                return null;
            }
        }
    }

    private final static String preferIPv6Addresses = System.getProperty("java.net.preferIPv6Addresses");
    private final static String preferIPv4Stack     = System.getProperty("java.net.preferIPv4Stack");

    public static IPVERSION selectIPVersion(IPVERSION ipVersion) {
        if (ipVersion == null) {
            ipVersion = IPVERSION.SYSTEM;
        }
        switch (ipVersion) {
        case IPV4_ONLY:
        case IPV4_IPV6:
        case IPV6_IPV4:
            return ipVersion;
        case SYSTEM:
        default:
            if ("true".equals(preferIPv4Stack)) {
                return IPVERSION.IPV4_ONLY;
            } else if (JVMVersion.isMinimum(JVMVersion.JAVA_9) && "system".equals(preferIPv6Addresses)) {
                return IPVERSION.SYSTEM;
            } else if ("true".equals(preferIPv6Addresses)) {
                return IPVERSION.IPV6_IPV4;
            } else if ("false".equals(preferIPv6Addresses)) {
                return IPVERSION.IPV4_IPV6;
            } else {
                if (Boolean.TRUE.equals(isGlobalIPv6Available(1000, 1000, 60 * 1000l))) {
                    return IPVERSION.IPV6_IPV4;
                } else {
                    return ipVersion;
                }
            }
        }
    }

    public static InetAddress[] sortAndFilter(final InetAddress[] inetAddresses, final IPVERSION ipVersion) {
        if (inetAddresses != null && ipVersion != null) {
            final InetAddress[] ret;
            switch (ipVersion) {
            case IPV4_ONLY:
                final List<InetAddress> ipv4Only = new ArrayList<InetAddress>();
                for (final InetAddress ip : inetAddresses) {
                    if (ip instanceof Inet4Address) {
                        ipv4Only.add(ip);
                    }
                }
                ret = ipv4Only.toArray(new InetAddress[0]);
                break;
            case IPV4_IPV6:
                ret = inetAddresses.clone();
                Arrays.sort(ret, new Comparator<InetAddress>() {
                    private final int compare(boolean x, boolean y) {
                        return (x == y) ? 0 : (x ? 1 : -1);
                    }

                    @Override
                    public int compare(InetAddress o1, InetAddress o2) {
                        final boolean x = o1 instanceof Inet6Address;
                        final boolean y = o2 instanceof Inet6Address;
                        return compare(x, y);
                    }
                });
                break;
            case IPV6_IPV4:
                ret = inetAddresses.clone();
                Arrays.sort(ret, new Comparator<InetAddress>() {
                    private final int compare(boolean x, boolean y) {
                        return (x == y) ? 0 : (x ? 1 : -1);
                    }

                    @Override
                    public int compare(InetAddress o1, InetAddress o2) {
                        final boolean x = o1 instanceof Inet4Address;
                        final boolean y = o2 instanceof Inet4Address;
                        return compare(x, y);
                    }
                });
                break;
            case SYSTEM:
            default:
                ret = inetAddresses;
                break;
            }
            return ret;
        } else {
            return inetAddresses;
        }
    }

    public static InetAddress[] resolvHostIP(final String host, IPVERSION ipVersion) throws UnknownHostException {
        ipVersion = selectIPVersion(ipVersion);
        InetAddress[] ret = HTTPConnectionUtils.resolvHostIP(host);
        ret = sortAndFilter(ret, ipVersion);
        if (ret != null && ret.length > 0) {
            return ret;
        } else {
            throw new UnknownHostException("Could not resolve(" + ipVersion + "):" + host);
        }
    }

    public final static byte R = (byte) 13;
    public final static byte N = (byte) 10;

    public static class DispositionHeader {
        private final String header;

        public String getHeader() {
            return header;
        }

        private final String raw;

        public final String getRaw() {
            return raw;
        }

        public final String getFilename() {
            return filename;
        }

        public final Charset getEncoding() {
            return encoding;
        }

        @Override
        public String toString() {
            return "RAW:" + getRaw() + "|Decoded:" + getFilename() + "|Encoding:" + getEncoding();
        }

        private final String  filename;
        private final Charset encoding;

        protected DispositionHeader(final String header, final String raw, final String filename, final Charset encoding) {
            this.raw = raw;
            this.filename = filename;
            this.encoding = encoding;
            this.header = header;
        }
    }

    public static String getFileNameFromDispositionHeader(final String contentdisposition) {
        final DispositionHeader ret = parseDispositionHeader(contentdisposition);
        if (ret != null) {
            return ret.getFilename();
        } else {
            return null;
        }
    }

    public static DispositionHeader parseDispositionHeader(final String contentdisposition) {
        // http://greenbytes.de/tech/tc2231/
        if (!StringUtils.isEmpty(contentdisposition)) {
            if (contentdisposition.matches("(?i)^\\s*(attachment|inline)\\s*;?\\s*$")) {
                return null;
            } else if (contentdisposition.matches("(?i)^\\s*(attachment|inline)\\s*;?\\s*filename\\s*=\\s*$")) {
                return null;
            } else if (contentdisposition.matches("(?i).*(;| |^)filename\\*\\s*=\\s*[\\w\\-]*'[\\w\\-]*'.+")) {
                /* RFC2231, with encoding/language */
                /* TODO: add support for 'parameter continuations' */
                final String rfc2231[] = new Regex(contentdisposition, "(?:;| |^)filename\\*\\s*=\\s*([\\w\\-]*)'([\\w\\-]*)'(.*?)($|;\\s*|;$)").getRow(0);
                final String encoding = rfc2231[0];
                final String language = rfc2231[1];
                final String raw = rfc2231[2];
                if (StringUtils.isEmpty(encoding)) {
                    org.appwork.loggingv3.LogV3.severe("Missing encoding: " + contentdisposition);
                    return null;
                } else if (raw == null) {
                    org.appwork.loggingv3.LogV3.severe("Broken/Unsupported: " + contentdisposition);
                    return null;
                } else {
                    try {
                        final Charset charSet = Charset.forName(encoding.trim());
                        String fileName = URLDecoder.decode(raw.trim(), charSet.name()).trim();
                        fileName = fileName.replaceFirst("^" + Matcher.quoteReplacement("\\") + "+", Matcher.quoteReplacement("_"));
                        if (StringUtils.isNotEmpty(fileName)) {
                            final CharSequence rfc2047 = new RFC2047().decode(fileName);
                            if (rfc2047 != null && rfc2047 != fileName) {
                                return new DispositionHeader(contentdisposition, raw, rfc2047.toString(), charSet);
                            } else {
                                return new DispositionHeader(contentdisposition, raw, fileName, charSet);
                            }
                        } else {
                            return null;
                        }
                    } catch (final Exception e) {
                        if (encoding.startsWith("\"")) {
                            String fixedQuotes = contentdisposition.replaceFirst(Pattern.quote(encoding), Matcher.quoteReplacement(encoding.substring(1)));
                            if (fixedQuotes.endsWith("\"")) {
                                fixedQuotes = fixedQuotes.substring(0, fixedQuotes.length() - 1);
                            }
                            return parseDispositionHeader(fixedQuotes);
                        } else {
                            org.appwork.loggingv3.LogV3.severe("Decoding Error: " + raw + "|" + encoding + "|" + contentdisposition + "|" + e.getMessage());
                            return null;
                        }
                    }
                }
            } else if (contentdisposition.matches("(?i).*(;| |^)(filename|file_name|name)\\s*=.+")) {
                final String raw = new Regex(contentdisposition, "(?:;| |^)(filename|file_name|name)\\s*=\\s*(\"|'|)(.*?)(\\2$|\\2;$|\\2;.)").getMatch(2);
                if (raw == null) {
                    org.appwork.loggingv3.LogV3.severe("Broken/Unsupported: " + contentdisposition);
                    return null;
                } else {
                    String fileName = raw.trim();
                    final CharSequence rfc2047 = new RFC2047().decode(fileName);
                    if (rfc2047 != null && rfc2047 != fileName) {
                        return new DispositionHeader(contentdisposition, raw, rfc2047.toString(), null);
                    } else {
                        fileName = fileName.replaceFirst("^" + Matcher.quoteReplacement("\\") + "+", Matcher.quoteReplacement("_"));
                        if (StringUtils.isNotEmpty(fileName)) {
                            return new DispositionHeader(contentdisposition, raw, fileName, null);
                        } else {
                            return null;
                        }
                    }
                }
            } else if (contentdisposition.matches("(?i).*(;| |^)filename\\*\\s*=.+")) {
                /* RFC2231, without encoding/language */
                /* TODO: add support for 'parameter continuations' */
                final String rfc2231[] = new Regex(contentdisposition, "(?:;| |^)filename\\*\\s*=\\s*(\"|)(.*?)(\\1$|\\1;\\s*|\\1;$)").getRow(0);
                if (rfc2231 == null) {
                    org.appwork.loggingv3.LogV3.severe("Broken/Unsupported: " + contentdisposition);
                    return null;
                } else {
                    final String raw = rfc2231[1];
                    if ("\"".equals(rfc2231[0]) && "\"".equals(rfc2231[2]) && raw.matches("(?i)\\s*[\\w\\-]*'[\\w\\-]*'.+")) {
                        // RFC2231 encoded, with double quotes around the parameter value.
                        return parseDispositionHeader("attachment; filename*=" + raw);
                    } else {
                        final String fileName = raw.trim();
                        if (StringUtils.isNotEmpty(fileName)) {
                            return new DispositionHeader(contentdisposition, raw, fileName, null);
                        } else {
                            return null;
                        }
                    }
                }
            } else if (contentdisposition.matches("(?i).*xfilename.*")) {
                return null;
            } else {
                org.appwork.loggingv3.LogV3.severe("Broken/Unsupported: " + contentdisposition);
                return null;
            }
        } else {
            return null;
        }
    }

    public static ByteBuffer readheader(final InputStream in, final boolean readSingleLine) throws IOException {
        ByteBuffer bigbuffer = ByteBuffer.wrap(new byte[4096]);
        final byte[] minibuffer = new byte[1];
        int position;
        while (in.read(minibuffer) >= 0) {
            if (Thread.currentThread().isInterrupted()) {
                throw new InterruptedIOException();
            }
            if (bigbuffer.remaining() < 1) {
                final ByteBuffer newbuffer = ByteBuffer.wrap(new byte[bigbuffer.capacity() * 2]);
                JDK8BufferHelper.flip(bigbuffer);
                newbuffer.put(bigbuffer);
                bigbuffer = newbuffer;
            }
            bigbuffer.put(minibuffer);
            if (readSingleLine) {
                if (bigbuffer.position() >= 1) {
                    /*
                     * \n only line termination, for fucking buggy non rfc servers
                     */
                    position = bigbuffer.position();
                    if (bigbuffer.get(position - 1) == HTTPConnectionUtils.N) {
                        break;
                    }
                    if (bigbuffer.position() >= 2) {
                        /* \r\n, correct line termination */
                        if (bigbuffer.get(position - 2) == HTTPConnectionUtils.R && bigbuffer.get(position - 1) == HTTPConnectionUtils.N) {
                            break;
                        }
                    }
                }
            } else {
                if (bigbuffer.position() >= 2) {
                    position = bigbuffer.position();
                    if (bigbuffer.get(position - 2) == HTTPConnectionUtils.N && bigbuffer.get(position - 1) == HTTPConnectionUtils.N) {
                        /*
                         * \n\n for header<->content divider, or fucking buggy non rfc servers
                         */
                        break;
                    }
                    if (bigbuffer.position() >= 4) {
                        /* \r\n\r\n for header<->content divider , correct line termination */
                        if (bigbuffer.get(position - 4) == HTTPConnectionUtils.R && bigbuffer.get(position - 3) == HTTPConnectionUtils.N && bigbuffer.get(position - 2) == HTTPConnectionUtils.R && bigbuffer.get(position - 1) == HTTPConnectionUtils.N) {
                            break;
                        }
                        /* X\n\r\n for header<->content divider, for server that use \n line termination */
                        if (bigbuffer.get(position - 4) != HTTPConnectionUtils.R && bigbuffer.get(position - 3) == HTTPConnectionUtils.N && bigbuffer.get(position - 2) == HTTPConnectionUtils.R && bigbuffer.get(position - 1) == HTTPConnectionUtils.N) {
                            break;
                        }
                    }
                }
            }
        }
        JDK8BufferHelper.flip(bigbuffer);
        return bigbuffer;
    }

    public static long[] parseRequestRange(HTTPConnection connection) {
        final String requestRange = connection != null ? connection.getRequestProperty(HTTPConstants.HEADER_REQUEST_RANGE) : null;
        return parseRequestRange(requestRange);
    }

    /**
     * https://www.rfc-editor.org/rfc/rfc7233
     *
     * @param requestRange
     * @return
     */
    public static long[] parseRequestRange(final String requestRange) {
        final String from = new Regex(requestRange, "bytes\\s*=\\s*(\\d*)\\s*-").getMatch(0);
        final String to = new Regex(requestRange, "bytes\\s*=\\s*.*?-\\s*(\\d*)").getMatch(0);
        final long[] ret = new long[] { -1l, -1l };
        if (StringUtils.isNotEmpty(from)) {
            ret[0] = Long.parseLong(from);
        }
        if (StringUtils.isNotEmpty(to)) {
            ret[1] = Long.parseLong(to);
        }
        return ret;
    }

    /**
     * https://www.rfc-editor.org/rfc/rfc7233
     *
     * @param httpConnection
     * @return
     */
    public static long[] parseContentRange(final HTTPConnection httpConnection) {
        final String contentRange = httpConnection != null ? httpConnection.getHeaderField(HTTPConstants.HEADER_RESPONSE_CONTENT_RANGE) : null;
        if (contentRange != null) {
            String[] range = null;
            // the total size can be given by a * Like: bytes 26395608-29695059/*
            if ((range = new Regex(contentRange, "\\s*(\\d+)\\s*-\\s*(\\d+)\\s*/\\s*(?:\\{\\s*)?(\\d+|\\*)").getRow(0)) != null) {
                /* RFC-2616 */
                /* START-STOP/SIZE */
                /* Content-Range=[133333332-199999999/200000000] */
                final long gotSB = Long.parseLong(range[0]);
                final long gotEB = Long.parseLong(range[1]);
                final long gotS = "*".equals(range[2]) ? -1 : Long.parseLong(range[2]);
                return new long[] { gotSB, gotEB, gotS };
            } else if ((range = new Regex(contentRange, "\\s*(\\d+)\\s*-\\s*/\\s*(?:\\{\\s*)?(\\d+|\\*)").getRow(0)) != null && (httpConnection == null || httpConnection.getResponseCode() != 416)) {
                /* only parse this when we have NO 416 (invalid range request) */
                /* NON RFC-2616! STOP is missing */
                /*
                 * this happend for some stupid servers, seems to happen when request is bytes=9500- (x till end)
                 */
                /* START-/SIZE */
                /* content-range: bytes 1020054729-/1073741824 */
                final long gotSB = Long.parseLong(range[0]);
                if ("*".equals(range[1])) {
                    return new long[] { gotSB, -1, -1 };
                } else {
                    final long gotS = Long.parseLong(range[1]);
                    return new long[] { gotSB, gotS - 1, gotS };
                }
            } else if ((httpConnection == null || httpConnection.getResponseCode() == 416) && (range = new Regex(contentRange, ".\\s*\\*\\s*/\\s*(?:\\{\\s*)?(\\d+|\\*)").getRow(0)) != null) {
                /* a 416 may respond with content-range * | content.size answer */
                return new long[] { -1, -1, "*".equals(range[0]) ? -1 : Long.parseLong(range[0]) };
            } else if ((httpConnection == null || httpConnection.getResponseCode() == 206) && (range = new Regex(contentRange, "[ \\*]+/\\s*(?:\\{\\s*)?(\\d+)").getRow(0)) != null) {
                /* RFC-2616 */
                /* a nginx 206 may respond with */
                /* content-range: bytes * / 554407633 */
                /*
                 * A response with status code 206 (Partial Content) MUST NOT include a Content-Range field with a byte-range- resp-spec of
                 * "*".
                 */
                return new long[] { -1, Long.parseLong(range[0]), Long.parseLong(range[0]) };
            } else if ((range = new Regex(contentRange, "\\s*bytes\\s*-\\s*(\\d+)/\\s*(?:\\{\\s*)?(\\d+)").getRow(0)) != null) {
                /**
                 * HTTP/1.1 200 OK Server: nginx/1.4.6 (Ubuntu)
                 *
                 * Content-Range: bytes -7601730/7601731
                 */
                return new long[] { 0, Long.parseLong(range[0]), Long.parseLong(range[1]) };
            } else {
                org.appwork.loggingv3.LogV3.info("parseContentRange: format is unknown: " + contentRange);
                return null;
            }
        } else {
            return null;
        }
    }

    private static Boolean LOCALHOST_RESOLVE = null;

    public static InetAddress[] getLoopback(IPVERSION ipVersion) throws UnknownHostException {
        final CopyOnWriteArrayList<InetAddress> loInetAddresses = new CopyOnWriteArrayList<InetAddress>();
        final Thread lookup = new Thread("getLoopback") {
            @Override
            public void run() {
                NetworkInterface lo = null;
                try {
                    final Enumeration<NetworkInterface> networkInterfaces = NetworkInterface.getNetworkInterfaces();
                    while (networkInterfaces.hasMoreElements()) {
                        final NetworkInterface networkInterface = networkInterfaces.nextElement();
                        if (networkInterface.isUp() && networkInterface.isLoopback()) {
                            if (lo == null && "lo".equals(networkInterface.getName())) {
                                lo = networkInterface;
                            }
                            final Enumeration<InetAddress> inetAddress = networkInterface.getInetAddresses();
                            while (inetAddress.hasMoreElements()) {
                                final InetAddress next = inetAddress.nextElement();
                                loInetAddresses.addIfAbsent(next);
                            }
                        }
                    }
                } catch (IOException ignore) {
                }
                if (lo == null) {
                    try {
                        lo = NetworkInterface.getByName("lo");
                        if (lo != null && lo.isUp() && lo.isLoopback()) {
                            final Enumeration<InetAddress> inetAddress = lo.getInetAddresses();
                            while (inetAddress.hasMoreElements()) {
                                final InetAddress next = inetAddress.nextElement();
                                loInetAddresses.addIfAbsent(next);
                            }
                        }
                    } catch (IOException ignore) {
                    }
                }
            }
        };
        lookup.setDaemon(true);
        lookup.start();
        try {
            lookup.join(1000);
        } catch (InterruptedException e) {
        }
        if (loInetAddresses.isEmpty()) {
            if (!Boolean.FALSE.equals(LOCALHOST_RESOLVE)) {
                try {
                    loInetAddresses.addAllAbsent(Arrays.asList(InetAddress.getAllByName("localhost")));
                    LOCALHOST_RESOLVE = Boolean.TRUE;
                } catch (UnknownHostException ignore) {
                    LOCALHOST_RESOLVE = Boolean.FALSE;
                }
            }
            try {
                loInetAddresses.addIfAbsent(InetAddress.getByName("127.0.0.1"));
            } catch (UnknownHostException ignore) {
            }
            try {
                loInetAddresses.addIfAbsent(InetAddress.getByName("::1"));
            } catch (UnknownHostException ignore) {
            }
        }
        ipVersion = selectIPVersion(ipVersion);
        InetAddress[] ret = loInetAddresses.toArray(new InetAddress[0]);
        ret = sortAndFilter(ret, ipVersion);
        if (ret != null && ret.length > 0) {
            return ret;
        } else {
            throw new UnknownHostException("Could not resolve(" + ipVersion + ") loopback");
        }
    }

    public static InetAddress[] resolvHostIP(String host) throws UnknownHostException {
        final String resolvHost;
        if (StringUtils.isEmpty(host)) {
            throw new UnknownHostException("Could not resolve: -empty host-");
        } else if (!host.matches("^[a-zA-Z0-9\\-\\.]+$") && Application.getJavaVersion() >= Application.JAVA16) {
            resolvHost = java.net.IDN.toASCII(host.trim());
        } else {
            /* remove spaces....so literal IP's work without resolving */
            resolvHost = host.trim();
        }
        for (int resolvTry = 0; resolvTry < 2; resolvTry++) {
            try {
                /* resolv all possible ip's */
                return InetAddress.getAllByName(resolvHost);
            } catch (final UnknownHostException e) {
                try {
                    Thread.sleep(500);
                } catch (final InterruptedException e1) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }
        throw new UnknownHostException("Could not resolve: -" + host + "<->" + resolvHost + "-");
    }
}
