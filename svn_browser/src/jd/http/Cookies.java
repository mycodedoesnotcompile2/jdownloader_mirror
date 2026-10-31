//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.http;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringTokenizer;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.regex.Pattern;

import jd.parser.Regex;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.logging2.LogInterface;

public class Cookies {
    public static Cookies parseSetCookies(final Request request) throws IOException {
        final URLConnectionAdapter httpConnection = request.getHttpConnection();
        final String protocol = request.getURL().getProtocol();
        final String date = httpConnection.getHeaderField("Date");
        final List<String> setCookies = httpConnection.getHeaderFields("Set-Cookie");
        final Cookies ret = new Cookies();
        if (setCookies == null || setCookies.size() == 0) {
            return ret;
        }
        final String tldDomain = Browser.getHost(request.getURL());
        final String domain = request.getURL().getHost();
        for (int i = 0; i < setCookies.size(); i++) {
            final String setCookie = setCookies.get(i);
            final Cookie cookie = new Cookie();
            cookie.setHost(tldDomain);
            cookie.setProtocol(protocol);
            cookie.setHostOnly(Boolean.TRUE);// default,changes on domain attribute
            cookie.setSecure(Boolean.FALSE);// default,changes on secure attribute
            cookie.setDomain(domain);
            cookie.setHostTime(date);
            final StringTokenizer st = new StringTokenizer(setCookie, ";");
            while (st.hasMoreTokens()) {
                final String cookieKeyValueString = st.nextToken().trim();
                /* Key and Value */
                final String keyValuePair[] = new Regex(cookieKeyValueString, "(.*?)=(.*)").getRow(0);
                final String key;
                final String value;
                if (keyValuePair == null || keyValuePair.length == 0) {
                    if (StringUtils.isNotEmpty(cookieKeyValueString)) {
                        key = cookieKeyValueString.trim();
                        value = null;
                    } else {
                        continue;
                    }
                } else if (keyValuePair.length == 1) {
                    key = keyValuePair[0].trim();
                    value = null;
                } else if (keyValuePair.length == 2) {
                    key = keyValuePair[0].trim();
                    value = keyValuePair[1].trim();
                } else {
                    continue;
                }
                if (key != null) {
                    if ("samesite".equalsIgnoreCase(key)) {
                        cookie.setSameSite(value);
                    } else if ("path".equalsIgnoreCase(key)) {
                        cookie.setPath(value);
                    } else if ("expires".equalsIgnoreCase(key)) {
                        cookie.setExpires(value);
                    } else if ("domain".equalsIgnoreCase(key)) {
                        cookie.setDomain(value);
                        cookie.setHostOnly(Boolean.FALSE);// no longer hostOnly
                    } else if ("secure".equalsIgnoreCase(key)) {
                        cookie.setSecure(Boolean.TRUE);// now secure
                    } else if ("HttpOnly".equalsIgnoreCase(key)) {
                        // HttpOnly
                    } else if ("Max-Age".equalsIgnoreCase(key)) {
                        // Max-Age
                        try {
                            // RFC 6265, section 5.2.2
                            final long maxAge = Long.parseLong(value);
                            if (maxAge <= 0) {
                                cookie.setExpireDate(1l);// 01/01/1970
                            } else if (cookie.getHostTime() > 0) {
                                cookie.setExpireDate(cookie.getHostTime() + maxAge * 1000l);
                            }
                        } catch (final Throwable e) {
                        }
                    } else {
                        if (cookie.getKey() == null) {
                            cookie.setKey(key);
                            cookie.setValue(value);
                        }
                    }
                }
            }
            if (cookie.getKey() != null) {
                if (Boolean.TRUE.equals(cookie.isSecure()) && !cookie.isSecureProtocol()) {
                    LogV3.warning("Ignore unsecure cookie:" + cookie);
                } else {
                    ret.add(cookie);
                }
            }
        }
        return ret;
    }

    @Deprecated
    public static Cookies parseCookies(final String cookieString, final URL url, final String serverTime, boolean isSetCookie) {
        final String header = cookieString;
        String path = null;
        String expires = null;
        final String tldDomain = Browser.getHost(url);
        String domain = isSetCookie ? url.getHost() : tldDomain;
        Boolean hostOnly = isSetCookie ? Boolean.TRUE : null;// default,changes on domain attribute
        String sameSite = isSetCookie ? "Lax" : null;
        Boolean secure = isSetCookie ? Boolean.FALSE : null;// default,changes on secure attribute
        final String protocol = url.getProtocol();
        final LinkedHashMap<String, String> tmp = new LinkedHashMap<String, String>();
        /* Cookie individual elements */
        final StringTokenizer st = new StringTokenizer(header, ";");
        while (st.hasMoreTokens()) {
            final String cookieKeyValueString = st.nextToken().trim();
            /* Key and Value */
            final String keyValuePair[] = new Regex(cookieKeyValueString, "(.*?)=(.*)").getRow(0);
            final String key;
            final String value;
            if (keyValuePair == null || keyValuePair.length == 0) {
                if (StringUtils.isNotEmpty(cookieKeyValueString)) {
                    key = cookieKeyValueString.trim();
                    value = null;
                } else {
                    continue;
                }
            } else if (keyValuePair.length == 1) {
                key = keyValuePair[0].trim();
                value = null;
            } else if (keyValuePair.length == 2) {
                key = keyValuePair[0].trim();
                value = keyValuePair[1].trim();
            } else {
                continue;
            }
            if (key != null) {
                if ("samesite".equalsIgnoreCase(key)) {
                    sameSite = value;
                } else if ("path".equalsIgnoreCase(key)) {
                    path = value;
                } else if ("expires".equalsIgnoreCase(key)) {
                    expires = value;
                } else if ("domain".equalsIgnoreCase(key)) {
                    domain = value;
                    hostOnly = Boolean.FALSE;// no longer hostOnly
                } else if ("secure".equalsIgnoreCase(key)) {
                    secure = Boolean.TRUE;// now secure
                } else if ("HttpOnly".equalsIgnoreCase(key)) {
                    // HttpOnly
                } else if ("Max-Age".equalsIgnoreCase(key)) {
                    // Max-Age
                } else {
                    if (!isSetCookie || tmp.size() == 0) {
                        /**
                         * SetCookie only contains a single cookie
                         */
                        tmp.put(key, value);
                    }
                }
            }
        }
        final Cookies cookies = new Cookies();
        for (final Entry<String, String> next : tmp.entrySet()) {
            /*
             * no cookies are cookies without a value
             */
            if (next.getValue() != null) {
                final Cookie cookie = new Cookie();
                cookie.setHost(tldDomain);
                cookie.setPath(path);
                cookie.setProtocol(protocol);
                cookie.setDomain(domain);
                cookie.setExpires(expires);
                cookie.setHostOnly(hostOnly);
                cookie.setValue(next.getValue());
                cookie.setKey(next.getKey());
                cookie.setHostTime(serverTime);
                cookie.setSecure(secure);
                cookie.setSameSite(sameSite);
                if (Boolean.TRUE.equals(cookie.isSecure()) && !cookie.isSecureProtocol()) {
                    LogV3.warning("Ignore unsecure cookie:" + cookie);
                } else {
                    cookies.add(cookie);
                }
            }
        }
        return cookies;
    }

    /* Wrapper */
    public static Cookies parseCookiesFromString(final String str) {
        return Cookies.parseCookiesFromJsonString(str, null);
    }

    private static Long parseTimestamp(Object timeStamp) {
        final Object ret = timeStamp != null ? ReflectionUtils.cast(timeStamp, Number.class) : null;
        if (ret != null && ret instanceof Number) {
            final long ts = ((Number) ret).longValue();
            return ts * 1000l;
        } else {
            return null;
        }
    }

    /**
     * Parses exported cookies from other applications. 2020-04-30: So far supported formats: Chrome/Opera addon editthiscookie.com ||
     * Firefox: FlagThisCookie: https://github.com/jrie/flagCookies
     */
    public static Cookies parseCookiesFromJsonString(final String str, final LogInterface logger) {
        return Cookies.parseCookiesFromObject(str, logger);
    }

    public static Cookies parseCookiesFromObject(Object input, final LogInterface logger) {
        if (input == null) {
            return null;
        }
        if (input instanceof String) {
            final String jsonStr = input.toString();
            if (jsonStr == null || !jsonStr.matches("(?s)^\\s*\\{.*\\}\\s*$") && !jsonStr.matches("(?s)^\\s*\\[.*\\]\\s*$")) {
                return null;
            }
            try {
                input = JSonStorage.restoreFromString(jsonStr, TypeRef.OBJECT);
            } catch (final JSonMapperException jme) {
                if (logger != null) {
                    logger.exception(HexFormatter.byteArrayToHex(String.valueOf(jsonStr).getBytes()), jme);
                    logger.info("Failed to parse cookie json");
                }
                return null;
            }
        }
        try {
            final List<Object> entries = ReflectionUtils.wrapList(input, false, Object.class);
            if (entries != null) {
                final Cookies ret = new Cookies();
                for (final Object entry : entries) {
                    final Cookie cookie = new Cookie();
                    final List<String> cookiesList = ReflectionUtils.wrapList(entry, false, String.class);
                    if (cookiesList != null) {
                        final int listSize = cookiesList.size();
                        if (listSize == 1) {
                            cookie.setKey(cookiesList.get(0));
                        } else if (listSize == 2) {
                            cookie.setKey(cookiesList.get(0));
                            cookie.setValue(cookiesList.get(1));
                        } else if (listSize == 3) {
                            cookie.setKey(cookiesList.get(0));
                            cookie.setValue(cookiesList.get(1));
                            cookie.setHost(cookiesList.get(2));
                        } else {
                            // wtf
                            continue;
                        }
                    } else if (entry instanceof Map) {
                        /* Cookies exported via browser addon 'EditThisCookie': https://www.editthiscookie.com/ */
                        final Map<String, Object> cookieInfo = (Map<String, Object>) entry;
                        final String domain = (String) cookieInfo.get("domain");
                        final String key = (String) cookieInfo.get("name");
                        final String value = (String) cookieInfo.get("value");
                        final String path = (String) cookieInfo.get("path");
                        final String sameSite = (String) cookieInfo.get("sameSite");
                        final Boolean secure = (Boolean) cookieInfo.get("secure");
                        final Boolean hostOnly = (Boolean) cookieInfo.get("hostOnly");
                        if (StringUtils.isEmpty(domain) || StringUtils.isEmpty(key) || StringUtils.isEmpty(value) || StringUtils.isEmpty(path)) {
                            /* Skip invalid objects */
                            continue;
                        }
                        cookie.setSameSite(sameSite);
                        cookie.setHost(domain);
                        cookie.setPath(path);
                        cookie.setDomain(domain);
                        cookie.setKey(key);
                        cookie.setValue(value);
                        cookie.setSecure(secure);
                        cookie.setHostOnly(hostOnly);
                        if (Boolean.TRUE.equals(secure)) {
                            cookie.setProtocol(Cookie.SECURE_HTTPS);
                        }
                        final Long expirationDate = Cookies.parseTimestamp(cookieInfo.get("expirationDate"));
                        if (expirationDate != null) {
                            cookie.setExpireDate(expirationDate.longValue());
                        } else {
                            cookie.setExpireDate(-1);
                        }
                    } else {
                        continue;
                    }
                    ret.add(cookie);
                }
                if (ret.isEmpty()) {
                    throw new Exception("no parsed cookies!?");
                } else {
                    return ret;
                }
            } else if (input instanceof Map) {
                final Cookies ret = new Cookies();
                /* E.g. cookies exported via browser addon 'FlagCookies' */
                final Map<String, Object> cookieMap = (Map<String, Object>) input;
                final String userAgent = (String) cookieMap.get("userAgent");
                ret.setUserAgent(userAgent);
                final Iterator<Entry<String, Object>> iteratorOuter = cookieMap.entrySet().iterator();
                while (iteratorOuter.hasNext()) {
                    final Entry<String, Object> entryOuter = iteratorOuter.next();
                    if (!(entryOuter.getValue() instanceof Map)) {
                        /* Skip invalid entries */
                        continue;
                    }
                    /* Go through all domains */
                    final Iterator<Entry<String, Object>> iteratorDomain = ((Map<String, Object>) entryOuter.getValue()).entrySet().iterator();
                    while (iteratorDomain.hasNext()) {
                        final Entry<String, Object> entryDomain = iteratorDomain.next();
                        /* Go through cookie objects */
                        final Iterator<Entry<String, Object>> iteratorCookies = ((Map<String, Object>) entryDomain.getValue()).entrySet().iterator();
                        while (iteratorCookies.hasNext()) {
                            final Entry<String, Object> entryCookie = iteratorCookies.next();
                            final Map<String, Object> cookieInfo = (Map<String, Object>) entryCookie.getValue();
                            final String domain = (String) cookieInfo.get("domain");
                            final String key = (String) cookieInfo.get("name");
                            final String value = (String) cookieInfo.get("value");
                            final String path = (String) cookieInfo.get("path");
                            final String sameSite = (String) cookieInfo.get("sameSite");
                            final Boolean secure = (Boolean) cookieInfo.get("secure");
                            final Boolean hostOnly = (Boolean) cookieInfo.get("hostOnly");
                            if (StringUtils.isEmpty(domain) || StringUtils.isEmpty(key) || StringUtils.isEmpty(value) || StringUtils.isEmpty(path)) {
                                /* Skip invalid objects */
                                continue;
                            }
                            final Cookie cookie = new Cookie();
                            cookie.setSameSite(sameSite);
                            cookie.setHost(domain);
                            cookie.setPath(path);
                            cookie.setDomain(domain);
                            cookie.setKey(key);
                            cookie.setValue(value);
                            cookie.setSecure(secure);
                            cookie.setHostOnly(hostOnly);
                            if (Boolean.TRUE.equals(secure)) {
                                cookie.setProtocol(Cookie.SECURE_HTTPS);
                            }
                            final Long expirationDate = Cookies.parseTimestamp(cookieInfo.get("expirationDate"));
                            if (expirationDate != null) {
                                cookie.setExpireDate(expirationDate.longValue());
                            } else {
                                cookie.setExpireDate(-1);
                            }
                            ret.add(cookie);
                        }
                    }
                }
                if (ret.isEmpty()) {
                    throw new Exception("no parsed cookies!?");
                } else {
                    return ret;
                }
            } else {
                /* Unknown/Unsupported format */
                throw new WTFException("unknown/unsupported format");
            }
        } catch (final Exception e) {
            if (logger != null) {
                logger.log(e);
                logger.info("Failed to process cookie object: Invalid/unsupported format?");
            }
            return null;
        }
    }

    @Deprecated
    public static Cookies parseCookies(final String cookieString, final URL url, final String serverTime) {
        return Cookies.parseCookies(cookieString, url, serverTime, false);
    }

    private final CopyOnWriteArrayList<Cookie> cookies   = new CopyOnWriteArrayList<Cookie>();
    private String                             userAgent = null;

    public Cookies() {
    }

    public Cookies(final Cookies cookies) {
        this.add(cookies);
    }

    public synchronized void add(final Cookie cookie) {
        for (final Cookie cookie2 : this.cookies) {
            if (cookie2.equals(cookie)) {
                cookie2.update(cookie);
                return;
            }
        }
        this.cookies.addIfAbsent(cookie);
    }

    public void add(final Cookies newcookies) {
        if (newcookies == null) {
            return;
        }
        for (final Cookie cookie : newcookies.getCookies()) {
            this.add(cookie);
        }
    }

    public boolean setUserAgent(final String useragent) {
        if (StringUtils.isEmpty(useragent)) {
            this.userAgent = null;
            return false;
        }
        this.userAgent = useragent;
        return true;
    }

    public String getUserAgent() {
        return this.userAgent;
    }

    public void clear() {
        this.cookies.clear();
    }

    public static String NOTDELETEDPATTERN = "^(?i)((?!^deleted$).)*$";

    @Deprecated
    public Cookie get(final String key, final String valuePattern) {
        return get(null, key, valuePattern);
    }

    private static final Comparator<Cookie> BEST_MATCH_COMPARATOR = new Comparator<Cookie>() {
        @Override
        public int compare(Cookie c1, Cookie c2) {
            // 0. Custom-Cookie Check
            final boolean custom1 = c1.isCustomCookie();
            final boolean custom2 = c2.isCustomCookie();
            if (custom1 != custom2) {
                return custom1 ? -1 : 1; // Custom-Cookie gewinnt sofort
            }
            // 1. Pfadlänge (absteigend)
            final int p1 = c1.getPath().length();
            final int p2 = c2.getPath().length();
            if (p1 != p2) {
                return Integer.compare(p2, p1);
            }
            // 2. Host-Only (true vor false)
            final boolean h1 = Boolean.TRUE.equals(c1.isHostOnly());
            final boolean h2 = Boolean.TRUE.equals(c2.isHostOnly());
            if (h1 != h2) {
                return h1 ? -1 : 1;
            }
            // 3. Domain-Länge (absteigend)
            final int d1 = c1.getDomain() != null ? c1.getDomain().length() : 0;
            final int d2 = c2.getDomain() != null ? c2.getDomain().length() : 0;
            if (d1 != d2) {
                return Integer.compare(d2, d1);
            }
            return 0;
        }
    };

    public String getCookieString(URL url) {
        if (cookies.isEmpty()) {
            return null;
        }
        final Map<String, List<Cookie>> keyValues = new LinkedHashMap<String, List<Cookie>>();
        final StringBuilder buffer = new StringBuilder();
        final List<Cookie> toSend = new ArrayList<Cookie>();
        for (final Cookie cookie : cookies) {
            if (cookie.isExpired() || !cookie.matches(url)) {
                continue;
            }
            toSend.add(cookie);
        }
        for (final Cookie cookie : toSend) {
            List<Cookie> list = keyValues.get(cookie.getKey());
            if (list == null) {
                list = new ArrayList<Cookie>();
                keyValues.put(cookie.getKey(), list);
            }
            list.add(cookie);
        }
        for (List<Cookie> values : keyValues.values()) {
            if (values.size() == 1) {
                continue;
            }
            Collections.sort(values, BEST_MATCH_COMPARATOR);
        }
        for (final Entry<String, List<Cookie>> keyValue : keyValues.entrySet()) {
            if (buffer.length() > 0) {
                buffer.append("; ");
            }
            buffer.append(keyValue.getKey());
            buffer.append("=");
            final List<Cookie> values = keyValue.getValue();
            if (values.size() > 1) {
                DebugMode.debugger();
            }
            buffer.append(values.get(0).getValue());
        }
        return buffer.toString();
    }

    public Cookie get(URL url, final String key, final String valuePattern) {
        final Cookie cookie = this.get(url, key);
        if (cookie == null) {
            return null;
        }
        if (valuePattern == null) {
            return cookie;
        }
        final String cookieValue = cookie.getValue();
        if (cookieValue != null && Pattern.compile(valuePattern).matcher(cookieValue).matches()) {
            return cookie;
        } else {
            return null;
        }
    }

    @Deprecated
    public Cookie get(final String key) {
        return get((URL) null, key);
    }

    public Cookie get(URL url, final String key) {
        if (key == null) {
            return null;
        }
        for (final Cookie cookie : this.cookies) {
            if (cookie.getKey().equals(key) && (url == null || cookie.matches(url))) {
                return cookie;
            }
        }
        for (final Cookie cookie : this.cookies) {
            if (cookie.getKey().equalsIgnoreCase(key) && (url == null || cookie.matches(url))) {
                return cookie;
            }
        }
        return null;
    }

    public List<Cookie> getCookies() {
        return this.cookies;
    }

    public boolean isEmpty() {
        return this.cookies.isEmpty();
    }

    public void remove(final Cookie cookie) {
        this.cookies.remove(cookie);
    }

    /**
     * Removes Cookie from current session, based on keyName
     *
     * @author raztoki
     * @since JD2
     */
    @Deprecated
    public void remove(final String keyName) {
        if (keyName != null) {
            final Cookie ckie = this.get(keyName);
            if (ckie != null) {
                this.cookies.remove(ckie);
            }
        }
    }

    @Override
    public String toString() {
        final StringBuilder ret = new StringBuilder();
        for (final Cookie el : this.cookies) {
            if (ret.length() > 0) {
                ret.append("\r\n");
            }
            ret.append(el.toString());
        }
        return ret.toString();
    }
}
