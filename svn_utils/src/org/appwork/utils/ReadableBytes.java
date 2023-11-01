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
package org.appwork.utils;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.serializer.Deser;
import org.appwork.serializer.SC;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.Storable;
import org.appwork.storage.TypeRef;
import org.appwork.utils.ReadableBytes.ReadableBytesParseException;

/**
 * @author thomas
 * @date 20.09.2023
 *
 */
public class ReadableBytes implements Storable {
    public static enum Unit {
        // TIP etc is International Electrotechnical Commission (IEC) standard
        TB("TiB", "TB", 1024l * 1024l * 1024l * 1024l, 1000l * 1000l * 1000l * 1000l),
        GB("GiB", "GB", 1024l * 1024l * 1024l, 1000l * 1000l * 1000l),
        MB("MiB", "MB", 1024l * 1024l, 1000l * 1000l),
        KB("KiB", "KB", 1024l, 1000l),
        B("B", "B", 1l, 1l);

        private final long bytes;
        private final long kibytes;
        private String     kiName;
        private String     kName;

        private Unit(String kiName, String kName, long kibytes, long bytes) {
            this.bytes = bytes;
            this.kibytes = kibytes;
            this.kiName = kiName;
            this.kName = kName;
        }

        public final long getBytes1000() {
            return this.bytes;
        }

        /**
         * @param k
         * @return
         */
        long factor(boolean k) {
            return k ? kibytes : bytes;
        }

        /**
         * @return
         */
        String unit(boolean k) {
            return k ? kiName : kName;
        }
    }

    public static final TypeRef<ReadableBytes> TYPE = new SimpleTypeRef<ReadableBytes>(ReadableBytes.class);
    private static final Pattern               PATTERN;
    private static HashSet<String>             KIBIS;

    public ReadableBytes() {
    }

    /**
     * @param bytes2
     */
    public ReadableBytes(long bytes) {
        this.bytes = bytes;
    }

    /**
     * @param i
     * @param b
     */
    public ReadableBytes(int bytes, boolean kibi) {
        this(bytes);
        this.kibi = kibi;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return Deser.get(ReadableBytes.class).toString(this, SC.WITH_DOCUMENTATION);
    }

    private long bytes;

    public long getBytes() {
        return bytes;
    }

    public void setBytes(long bytes) {
        this.bytes = bytes;
    }

    private boolean                            kibi = true;
    private static final HashMap<String, Unit> UNIT_MAP;

    public boolean isKibi() {
        return kibi;
    }

    public void setKibi(boolean kibi) {
        this.kibi = kibi;
    }

    /**
     * @param longValue
     * @return
     */
    public static ReadableBytes fromBytes(long bytes) {
        ReadableBytes ret = new ReadableBytes(bytes);
        return ret;
    }

    /**
     * @return
     */
    public String format() {
        StringBuilder sb = new StringBuilder();
        long b = bytes;
        if (b < 0) {
            sb.append("-");
        }
        boolean k = isKibi();
        for (Unit u : Unit.values()) {
            if (b == 0) {
                break;
            }
            if (u == Unit.B) {
                sb.append(b).append(u.unit(k));
                break;
            }
            long factor = u.factor(k);
            if (b > factor) {
                long full = b / factor;
                sb.append(full).append(u.unit(k));
                b -= factor * full;
            }
        }
        if (sb.length() == 0) {
            return "0B";
        }
        return sb.toString();
    }

    public static class ReadableBytesParseException extends Exception {
        /**
         *
         */
        public ReadableBytesParseException() {
            // TODO Auto-generated constructor stub
        }

        /**
         * @param string
         */
        public ReadableBytesParseException(String string) {
            super(string);
        }
    }

    /**
     * @param value
     * @return
     * @throws ReadableBytesParseException
     */
    static {
        UNIT_MAP = new HashMap<String, Unit>();
        KIBIS = new HashSet<String>();
        for (Unit u : Unit.values()) {
            UNIT_MAP.put(u.unit(true).toLowerCase(Locale.ROOT), u);
            UNIT_MAP.put(u.unit(false).toLowerCase(Locale.ROOT), u);
            KIBIS.add(u.unit(true).toLowerCase(Locale.ROOT));
        }
        String p = "";
        for (Unit u : Unit.values()) {
            if (p.length() > 0) {
                p += "|";
            }
            p += u.unit(false);
            if (!u.unit(false).equals(u.unit(true))) {
                p += "|";
                p += u.unit(true);
            }
        }
        PATTERN = Pattern.compile("((?:\\d+)|(?:\\d+(?:\\.|\\,)\\d+))\\s*(" + p + ")", Pattern.CASE_INSENSITIVE);
    }

    public static ReadableBytes parse(String value) throws ReadableBytesParseException {
        value = value.replaceAll("\\s+", "");
        boolean negative = false;
        long result = 0;
        int index = 0;
        if (value.startsWith("-")) {
            index++;
            negative = true;
        }
        while (index < value.length()) {
            Matcher matcher = PATTERN.matcher(value);
            if (!matcher.find(index)) {
                throw new ReadableBytesParseException();
            }
            String num = matcher.group(1);
            String u = matcher.group(2).toLowerCase(Locale.ROOT);
            Unit unit = UNIT_MAP.get(u);
            if (unit == null) {
                throw new ReadableBytesParseException("Unknown unit: " + matcher.group("unit"));
            }
            double doub = Double.parseDouble(num.replace(",", "."));
            doub *= unit.factor(KIBIS.contains(u));
            result += (long) doub;
            index = matcher.end();
        }
        if (negative) {
            result += -1;
        }
        return fromBytes(result);
    }
}
