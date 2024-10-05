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
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.logging2.LogInterface;

public class Cookies {
    public static Cookies parseSetCookies(final Request request) throws IOException {
        final URLConnectionAdapter httpConnection = request.getHttpConnection();
        final String date = httpConnection.getHeaderField("Date");
        final List<String> setCookies = httpConnection.getHeaderFields("Set-Cookie");
        final Cookies ret = new Cookies();
        if (setCookies == null || setCookies.size() == 0) {
            return ret;
        }
        final String host = Browser.getHost(request.getURL());
        for (int i = 0; i < setCookies.size(); i++) {
            final String setCookie = setCookies.get(i);
            final Cookie cookie = new Cookie();
            cookie.setHost(host);
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
                    if ("path".equalsIgnoreCase(key)) {
                        cookie.setPath(value);
                    } else if ("expires".equalsIgnoreCase(key)) {
                        cookie.setExpires(value);
                    } else if ("domain".equalsIgnoreCase(key)) {
                        cookie.setDomain(value);
                    } else if ("secure".equalsIgnoreCase(key)) {
                        cookie.setSecure(true);
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
                ret.add(cookie);
            }
        }
        return ret;
    }

    public static Cookies parseCookies(final String cookieString, final String host, final String serverTime, boolean isSetCookie) {
        final String header = cookieString;
        String path = null;
        String expires = null;
        String domain = null;
        boolean secure = false;
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
                if ("path".equalsIgnoreCase(key)) {
                    path = value;
                } else if ("expires".equalsIgnoreCase(key)) {
                    expires = value;
                } else if ("domain".equalsIgnoreCase(key)) {
                    domain = value;
                } else if ("secure".equalsIgnoreCase(key)) {
                    secure = true;
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
                cookies.add(cookie);
                cookie.setHost(host);
                cookie.setPath(path);
                cookie.setDomain(domain);
                cookie.setExpires(expires);
                cookie.setValue(next.getValue());
                cookie.setKey(next.getKey());
                cookie.setHostTime(serverTime);
                cookie.setSecure(secure);
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
        return parseCookiesFromObject(str, logger);
    }

    public static Cookies parseCookiesFromObject(Object input, final LogInterface logger) {
        if (input == null) {
            return null;
        }
        if (input instanceof String) {
            final String jsonStr = input.toString();
            if (jsonStr == null || (!jsonStr.matches("(?s)^\\s*\\{.*\\}\\s*$") && !jsonStr.matches("(?s)^\\s*\\[.*\\]\\s*$"))) {
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
            final long timeStamp = Time.timestamp();
            if (input instanceof List) {
                /* Cookies from EditThisCookie or JDownloaders' proprietary cookie list format */
                final Cookies cookies = new Cookies();
                final List<Object> cookiesO = (List<Object>) input;
                for (final Object cookieO : cookiesO) {
                    final Cookie cookie = new Cookie();
                    if (cookieO instanceof String[]) {
                        /* JDownloaders' cookies from LinkCrawler rule when rule stores them */
                        final String[] cookiesStringList = (String[]) cookieO;
                        final Object keyO = cookiesStringList[0];
                        final Object valueO = cookiesStringList[1];
                        cookie.setKey(keyO.toString());
                        if(valueO != null) {
                            cookie.setValue(valueO.toString());
                        }
           
                        if(cookiesStringList.length == 3) {
                            cookie.setHost(cookiesStringList[2].toString());
                        }
                    } else if (cookieO instanceof List) {
                        /* JDownloaders' cookies from LinkCrawler rule: List of lists */
                        final List<String> cookiesList = (List<String>) cookieO;
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
                    } else {
                        /* Cookies exported via browser addon 'EditThisCookie': https://www.editthiscookie.com/ */
                        final Map<String, Object> cookieInfo = (Map<String, Object>) cookieO;
                        final String domain = (String) cookieInfo.get("domain");
                        final String key = (String) cookieInfo.get("name");
                        final String value = (String) cookieInfo.get("value");
                        final String path = (String) cookieInfo.get("path");
                        final boolean secure = ((Boolean) cookieInfo.get("secure")).booleanValue();
                        if (StringUtils.isEmpty(domain) || StringUtils.isEmpty(key) || StringUtils.isEmpty(value) || StringUtils.isEmpty(path)) {
                            /* Skip invalid objects */
                            continue;
                        }
                        cookie.setHost(domain);
                        cookie.setPath(path);
                        cookie.setDomain(domain);
                        final Long expirationDate = Cookies.parseTimestamp(cookieInfo.get("expirationDate"));
                        if (expirationDate != null) {
                            cookie.setExpireDate(expirationDate.longValue());
                        } else {
                            cookie.setExpireDate(-1);
                        }
                        cookie.setKey(key);
                        cookie.setValue(value);
                        cookie.setSecure(secure);
                        cookies.add(cookie);
                    }
                    if (false && cookie.getExpireDate() >= 0) {
                        cookie.setHostTime(timeStamp);
                        cookie.setCreationTime(timeStamp);
                        if (cookie.isExpired()) {
                            logger.info("Skip expired cookie:" + cookie);
                            continue;
                        }
                    }
                    cookies.add(cookie);
                }
                if (cookies.isEmpty()) {
                    throw new Exception("no parsed cookies!?");
                } else {
                    return cookies;
                }
            } else if (input instanceof Map) {
                final Cookies cookies = new Cookies();
                /* E.g. cookies exported via browser addon 'FlagCookies' */
                final Map<String, Object> cookieMap = (Map<String, Object>) input;
                final String userAgent = (String) cookieMap.get("userAgent");
                cookies.setUserAgent(userAgent);
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
                            final boolean secure = ((Boolean) cookieInfo.get("secure")).booleanValue();
                            if (StringUtils.isEmpty(domain) || StringUtils.isEmpty(key) || StringUtils.isEmpty(value) || StringUtils.isEmpty(path)) {
                                /* Skip invalid objects */
                                continue;
                            }
                            final Cookie cookie = new Cookie();
                            cookie.setHost(domain);
                            cookie.setPath(path);
                            cookie.setDomain(domain);
                            final Long expirationDate = Cookies.parseTimestamp(cookieInfo.get("expirationDate"));
                            if (expirationDate != null) {
                                cookie.setExpireDate(expirationDate.longValue());
                            } else {
                                cookie.setExpireDate(-1);
                            }
                            cookie.setKey(key);
                            cookie.setValue(value);
                            cookie.setSecure(secure);
                            if (false && cookie.getExpireDate() >= 0) {
                                cookie.setHostTime(timeStamp);
                                cookie.setCreationTime(timeStamp);
                                if (cookie.isExpired()) {
                                    logger.info("Skip expired cookie:" + cookie);
                                    continue;
                                }
                            }
                            cookies.add(cookie);
                        }
                    }
                }
                if (cookies.isEmpty()) {
                    throw new Exception("no parsed cookies!?");
                } else {
                    return cookies;
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

    public static Cookies parseCookies(final String cookieString, final String host, final String serverTime) {
        return Cookies.parseCookies(cookieString, host, serverTime, false);
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
        this.cookies.add(cookie);
    }

    public void add(final Cookies newcookies) {
        if (newcookies != null) {
            for (final Cookie cookie : newcookies.getCookies()) {
                this.add(cookie);
            }
        }
    }

    public boolean setUserAgent(final String useragent) {
        if (StringUtils.isEmpty(useragent)) {
            this.userAgent = null;
            return false;
        } else {
            this.userAgent = useragent;
            return true;
        }
    }

    public String getUserAgent() {
        return this.userAgent;
    }

    public void clear() {
        this.cookies.clear();
    }

    public static String NOTDELETEDPATTERN = "^(?i)((?!^deleted$).)*$";

    public Cookie get(final String key, final String valuePattern) {
        final Cookie cookie = this.get(key);
        if (cookie != null) {
            if (valuePattern == null) {
                return cookie;
            } else {
                final String cookieValue = cookie.getValue();
                if (cookieValue != null && Pattern.compile(valuePattern).matcher(cookieValue).matches()) {
                    return cookie;
                } else {
                    return null;
                }
            }
        } else {
            return null;
        }
    }

    public Cookie get(final String key) {
        if (key == null) {
            return null;
        }
        for (final Cookie cookie : this.cookies) {
            if (cookie.getKey().equals(key)) {
                return cookie;
            }
        }
        for (final Cookie cookie : this.cookies) {
            if (cookie.getKey().equalsIgnoreCase(key)) {
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
