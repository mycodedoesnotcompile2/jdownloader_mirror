package org.appwork.csv;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Locale;

import org.appwork.utils.StringUtils;;

public class CSVBuilder {
    private String seperator;

    public String getSeperator() {
        return seperator;
    }

    public void setSeperator(final String seperator) {
        this.seperator = seperator;
    }

    public class Entry extends ArrayList<String> {
    }

    private final ArrayList<Entry>                entries             = new ArrayList<Entry>();
    private Entry                                 current;
    private final LinkedHashMap<String, Integer>  headers;
    private final LinkedHashMap<Integer, Integer> width;
    private Character                             headerLineCharacter = '-';

    public Character getHeaderLineCharacter() {
        return headerLineCharacter;
    }

    public void setHeaderLineCharacter(Character headerLineCharacter) {
        this.headerLineCharacter = headerLineCharacter;
    }

    public CSVBuilder(final String trenn) {
        seperator = trenn;
        headers = new LinkedHashMap<String, Integer>();
        width = new LinkedHashMap<Integer, Integer>();
    }

    public void next() {
        current = new Entry();
        entries.add(current);
    }

    @Override
    public String toString() {
        String ret = "";
        int i = 0;
        final ArrayList<String> keys = new ArrayList<String>();
        for (final java.util.Map.Entry<String, Integer> es : headers.entrySet()) {
            keys.add(es.getKey());
            if (hides.contains(es.getKey())) {
                continue;
            }
            if (ret.length() > 0) {
                ret += seperator;
            }
            if (getAlignRight(es.getKey())) {
                ret += StringUtils.fillPre(es.getKey(), " ", width.get(i));
            } else {
                ret += StringUtils.fillPost(es.getKey(), " ", width.get(i));
            }
            i++;
        }
        ret += "\r\n";
        if (headerLineCharacter != null) {
            final String line = StringUtils.fillPost("", "" + headerLineCharacter, ret.length() - 2);
            ret += line;
            ret = line + "\r\n" + ret;
        }
        for (final Entry e : entries) {
            ret += "\r\n";
            for (i = 0; i < Math.max(headers.size(), e.size()); i++) {
                final String key = keys.get(i);
                if (hides.contains(key)) {
                    continue;
                }
                if (i > 0) {
                    ret += seperator;
                }
                if (e.size() > i) {
                    if (getAlignRight(key)) {
                        ret += StringUtils.fillPre(e.get(i), " ", width.get(i));
                    } else {
                        ret += StringUtils.fillPost(e.get(i), " ", width.get(i));
                    }
                } else {
                    ret += "";
                }
            }
        }
        return ret;
    }

    protected boolean getAlignRight(final String string) {
        if (string.endsWith("InEUR")) {
            return true;
        }
        if (string.endsWith("InDays")) {
            return true;
        }
        return false;
    }

    public void add(final String key, final String format) {
        if (!headers.containsKey(key)) {
            headers.put(key, headers.size());
        }
        final int index = headers.get(key);
        while (current.size() - 1 < index) {
            current.add("");
        }
        final Integer w = width.get(index);
        final int max = Math.max(format.length(), key.length());
        if (w == null || w.intValue() < max) {
            width.put(index, max);
        }
        current.set(index, format);
    }

    public void add(final String key, final int gewinn) {
        add(key, "" + gewinn);
    }

    public void add(final String key, final double gewinn) {
        add(key, (gewinn + "").replace(".", ","));
    }

    public final static SimpleDateFormat df = new SimpleDateFormat("dd.MM.YYYY HH:mm", Locale.GERMAN);

    public void add(final String key, final Date date) {
        add(key, df.format(date));
    }

    private final HashSet<String> hides = new HashSet<String>();

    public void hide(final String... headers) {
        hides.addAll(Arrays.asList(headers));
    }
}
