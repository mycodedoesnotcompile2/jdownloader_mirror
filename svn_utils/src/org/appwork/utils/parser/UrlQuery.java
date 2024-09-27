package org.appwork.utils.parser;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.KeyValueStringEntry;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.net.URLHelper;

public class UrlQuery {
    // TODO: Compare/merge with HttpConnection.parseParameterList
    public static UrlQuery parse(String query) throws MalformedURLException {
        final UrlQuery ret = new UrlQuery();
        if (query == null) {
            return ret;
        } else {
            try {
                query = URLHelper.createURL(query).getQuery();
            } catch (final IOException e) {
                if (StringUtils.startsWithCaseInsensitive(e.getMessage(), "unknown protocol")) {
                    try {
                        final URL url = URLHelper.createURL(query.replaceFirst("^\\w+://", "http://"));
                        URLHelper.verifyURL(url);
                        query = url.getQuery();
                    } catch (IOException ignore) {
                    }
                } else if (StringUtils.startsWithCaseInsensitive(e.getMessage(), "no protocol")) {
                    try {
                        final URL url = URLHelper.createURL("http://" + query);
                        URLHelper.verifyURL(url);
                        query = url.getQuery();
                    } catch (IOException ignore) {
                    }
                }
            }
        }
        if (query == null) {
            return ret;
        } else {
            query = query.trim();
            final StringBuilder sb = new StringBuilder();
            String key = null;
            for (int i = 0; i < query.length(); i++) {
                char c = query.charAt(i);
                // https://tools.ietf.org/html/rfc3986#section-3.4
                // The characters slash ("/") and question mark ("?") may represent data
                // within the query component.
                if (c == '?' && i == 0) {
                    sb.setLength(0);
                } else if (c == '&') {
                    if (key != null || sb.length() > 0) {
                        ret.add(key, sb.toString());
                    }
                    sb.setLength(0);
                    key = null;
                } else if (c == '=' && key == null) {
                    key = sb.toString();
                    sb.setLength(0);
                } else {
                    sb.append(c);
                }
            }
            if (key != null || sb.length() > 0) {
                ret.add(key, sb.toString());
            }
            return ret;
        }
    }

    private final List<KeyValueStringEntry> list = new ArrayList<KeyValueStringEntry>();

    public UrlQuery() {
    }

    @Override
    public String toString() {
        final StringBuilder ret = new StringBuilder();
        for (final KeyValueStringEntry s : this.list) {
            if (ret.length() > 0) {
                ret.append("&");
            }
            if (s.getKey() == null) {
                ret.append(s.getValue());
            } else {
                ret.append(s.getKey()).append("=").append(s.getValue());
            }
        }
        return ret.toString();
    }

    /**
     * WARNING.... this is raw
     *
     * @deprecated Use {@link #append(String, String, boolean)} or {@link #appendEncoded(String, String)}
     */
    @Deprecated
    public UrlQuery add(String key, String value) {
        this.list.add(new KeyValueStringEntry(key, value));
        return this;
    }

    /**
     * Gets the first entry for the key . The result is probably urlEncoded
     *
     * @param key
     * @return
     */
    public String get(String key) {
        for (final KeyValueStringEntry s : this.list) {
            if (StringUtils.equals(s.getKey(), key)) {
                return s.getValue();
            }
        }
        return null;
    }

    public String getDecoded(String key) {
        try {
            return this.getDecoded(key, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            // UTF-8 always exists
            throw new WTFException(e);
        }
    }

    public String getDecoded(String key, String encoding) throws UnsupportedEncodingException {
        if (StringUtils.isEmpty(encoding)) {
            encoding = "UTF-8";
        }
        for (final KeyValueStringEntry s : this.list) {
            if (StringUtils.equals(s.getKey(), key)) {
                return URLDecoder.decode(s.getValue(), encoding);
            }
        }
        return null;
    }

    public List<KeyValueStringEntry> list() {
        return java.util.Collections.unmodifiableList(this.list);
    }

    public static UrlQuery get(Map<String, String> post) {
        final UrlQuery ret = new UrlQuery();
        if (post != null) {
            for (final Entry<String, String> es : post.entrySet()) {
                ret.add(es.getKey(), es.getValue());
            }
        }
        return ret;
    }

    public UrlQuery addAndReplace(String key, String value) {
        final KeyValueStringEntry entry = new KeyValueStringEntry(key, value);
        final int index = this.remove(key);
        if (index < 0) {
            // add new/at the end
            this.list.add(entry);
        } else {
            // replace/add at index
            this.list.add(index, entry);
        }
        return this;
    }

    /**
     * Removes all entries for the key and returns the index of the first removed one
     *
     * @param key
     * @return
     */
    public int remove(String key) {
        int first = -1;
        int i = 0;
        for (final Iterator<KeyValueStringEntry> it = this.list.iterator(); it.hasNext();) {
            final KeyValueStringEntry value = it.next();
            if (StringUtils.equals(value.getKey(), key)) {
                it.remove();
                if (first < 0) {
                    first = i;
                }
            }
            i++;
        }
        return first;
    }

    public boolean containsKey(String key) {
        for (final KeyValueStringEntry es : this.list) {
            if (StringUtils.equals(es.getKey(), key)) {
                return true;
            }
        }
        return false;
    }

    public boolean addIfNoAvailable(String key, String value) {
        if (this.containsKey(key)) {
            return false;
        } else {
            this.add(key, value);
            return true;
        }
    }

    /**
     * Tries to split the information if a key is used several times.
     *
     *
     * @return
     */
    public List<UrlQuery> split() {
        final ArrayList<UrlQuery> ret = new ArrayList<UrlQuery>();
        final ArrayList<KeyValueStringEntry> lst = new ArrayList<KeyValueStringEntry>(this.list);
        while (true) {
            final UrlQuery map = new UrlQuery();
            for (final Iterator<KeyValueStringEntry> it = lst.iterator(); it.hasNext();) {
                final KeyValueStringEntry es = it.next();
                if (!map.containsKey(es.getKey())) {
                    map.add(es.getKey(), es.getValue());
                    it.remove();
                }
            }
            if (map.size() > 0) {
                ret.add(map);
            }
            if (lst.size() == 0) {
                break;
            }
        }
        return ret;
    }

    private int size() {
        return this.list.size();
    }

    public static UrlQuery get(List<KeyValueStringEntry> post) {
        final UrlQuery ret = new UrlQuery();
        ret.addAll(post);
        return ret;
    }

    public void addAll(List<KeyValueStringEntry> post) {
        if (post != null) {
            this.list.addAll(post);
        }
    }

    public LinkedHashMap<String, String> toMap(final boolean decodeValue) {
        final LinkedHashMap<String, String> ret = new LinkedHashMap<String, String>();
        try {
            for (final KeyValueStringEntry e : this.list) {
                if (decodeValue) {
                    ret.put(e.getKey(), URLDecoder.decode(e.getValue(), "UTF-8"));
                } else {
                    ret.put(e.getKey(), e.getValue());
                }
            }
        } catch (UnsupportedEncodingException e) {
            // UTF-8 must be available!
            throw new WTFException(e);
        }
        return ret;
    }

    public LinkedHashMap<String, String> toMap() {
        return toMap(false);
    }

    public UrlQuery append(String key, String value, boolean urlencode) {
        if (value == null) {
            this.addAndReplace(key, "");
        } else {
            this.addAndReplace(key, urlencode ? URLEncode.encodeRFC2396(value) : value);
        }
        return this;
    }

    public UrlQuery appendEncoded(String key, String value) {
        return this.append(key, value, true);
    }
}