package org.appwork.dependencies;

import java.util.Date;

import org.appwork.storage.flexijson.mapper.typemapper.DateMapper;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;

public class Version implements Comparable<Version> {
    private String numeric;
    private long[] data;
    private String extension;
    private String org;
    private Date   date;

    public String getOrg() {
        return this.org;
    }

    public Version(String v) {
        try {
            v = v.trim();
            // DebugMode.breakIf(v.length() == 0, null);
            this.org = v;
            String versionOnly = new Regex(v, "(^[\\d\\.]+)").getMatch(0);
            ;
            if (versionOnly == null) {
                versionOnly = "";
            }
            this.extension = v.substring(versionOnly.length());
            while (versionOnly.endsWith(".")) {
                versionOnly = versionOnly.substring(0, versionOnly.length() - 1);
            }
            this.numeric = versionOnly;
            final String[] splitted = this.numeric.split("\\.");
            this.data = new long[splitted.length];
            for (int i = 0; i < splitted.length; i++) {
                this.data[i] = Long.parseLong(splitted[i]);
            }
        } catch (final Exception e) {
            this.numeric = "0";
            this.extension = this.org;
            this.data = new long[] { 0 };
        }
    }

    public String getNumeric() {
        return this.numeric;
    }

    @Override
    public int hashCode() {
        int ret = this.numeric.hashCode();
        if (this.extension != null) {
            ret += this.extension.hashCode();
        }
        return ret;
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof Version)) {
            return false;
        }
        final Version other = (Version) o;
        return other.numeric.equals(this.numeric) && StringUtils.equals(other.extension, this.extension);
    }

    @Override
    public String toString() {
        // use getOrg
        try {
            if (this.date != null) {
                return this.org + " (" + DateMapper.formatJsonDefault(this.date) + ")";
            } else {
                return this.org;
            }
        } catch (final Exception e) {
            return this.org + " (" + DateMapper.formatJsonDefault(this.date) + ")";
        }
    }

    @Override
    public int compareTo(final Version o) {
        for (int i = 0; i < Math.max(o.data.length, this.data.length); i++) {
            final long me = i < this.data.length ? this.data[i] : 0;
            final long other = i < o.data.length ? o.data[i] : 0;
            final int ret = CompareUtils.compareNumber(me, other);
            if (ret != 0) {
                return ret;
            }
        }
        if (o.extension.matches("-\\d+") && this.extension.matches("-\\d+")) {
            return CompareUtils.compare(Integer.parseInt(this.extension.substring(1)), Integer.parseInt(o.extension.substring(1)));
        }
        if (o.extension.startsWith("-") && this.extension.startsWith("-")) {
            return CompareUtils.compare(this.extension.substring(1), o.extension.substring(1));
        }
        // versions without any extension are higher
        return CompareUtils.compare(o.extension, this.extension);
    }

    public boolean isLowerThan(final Version o) {
        int ret = 0;
        for (int i = 0; i < Math.max(o.data.length, this.data.length); i++) {
            final long me = i < this.data.length ? this.data[i] : 0;
            final long other = i < o.data.length ? o.data[i] : 0;
            ret = CompareUtils.compareNumber(me, other);
            if (ret != 0) {
                break;
            }
        }
        return ret < 0;
    }

    /**
     * @param date
     */
    public void setDate(final Date date) {
        this.date = date;
    }

    public Date getDate() {
        return this.date;
    }

    /**
     * @return
     */
    public boolean isNumericOnly() {
        return StringUtils.equals(this.numeric, this.getOrg());
    }

    /**
     * @return
     */
    public String toDisplayString() {
        if (this.date != null) {
            return this.org + " (" + DateMapper.formatJsonDefault(this.date) + ")";
        } else {
            return this.org;
        }
    }
}
