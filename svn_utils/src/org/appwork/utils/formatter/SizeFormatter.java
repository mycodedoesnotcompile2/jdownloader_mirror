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
package org.appwork.utils.formatter;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.util.regex.Pattern;

import org.appwork.utils.Regex;
import org.appwork.utils.locale._AWU;

/**
 * @author $Author: unknown$
 *
 */
public class SizeFormatter {
    /**
     * Formats filesize from Bytes to the best readable form
     *
     * @param fileSize
     *            in Bytes
     * @return
     */
    public static String formatBytes(final DecimalFormat c, final long fileSize) {
        return formatBytes((NumberFormat) c, fileSize);
    }

    public static String formatBytes(final NumberFormat c, final long fileSize) {
        final long abs = Math.abs(fileSize);
        if (abs >= Unit.TB.getBytes1024()) {
            return _AWU.T.literally_tebibyte(c.format(fileSize / (double) Unit.TB.getBytes1024()));
        } else if (abs >= Unit.GB.getBytes1024()) {
            return _AWU.T.literally_gibibyte(c.format(fileSize / (double) Unit.GB.getBytes1024()));
        } else if (abs >= Unit.MB.getBytes1024()) {
            return _AWU.T.literally_mebibyte(c.format(fileSize / (double) Unit.MB.getBytes1024()));
        } else if (abs >= Unit.KB.getBytes1024()) {
            return _AWU.T.literally_kibibyte(c.format(fileSize / (double) Unit.KB.getBytes1024()));
        } else {
            return _AWU.T.literally_byte(fileSize);
        }
    }

    @Deprecated
    public static String formatBytes(long fileSize) {
        return formatBytes(NumberFormat.getInstance(), fileSize);
    }

    public static enum Unit {
        TB(1024l * 1024l * 1024l * 1024l, 1000l * 1000l * 1000l * 1000l),
        GB(1024l * 1024l * 1024l, 1000l * 1000l * 1000l),
        MB(1024l * 1024l, 1000l * 1000l),
        KB(1024l, 1000l),
        B(1l, 1l);
        private final long bytes;
        private final long kibytes;

        public final long getBytes1024() {
            return this.kibytes;
        }

        private Unit(long kibytes, long bytes) {
            this.bytes = bytes;
            this.kibytes = kibytes;
        }

        public final long getBytes1000() {
            return this.bytes;
        }
    }

    public static Unit getSmallerUnit(Unit unit) {
        return Unit.values()[Math.min(Unit.values().length - 1, unit.ordinal() + 1)];
    }

    public static Unit getBiggerUnit(Unit unit) {
        return Unit.values()[Math.max(0, unit.ordinal() - 1)];
    }

    public static Unit getBestUnit(long fileSize) {
        final long abs = Math.abs(fileSize);
        if (abs >= Unit.TB.getBytes1024()) {
            return Unit.TB;
        } else if (abs >= Unit.GB.getBytes1024()) {
            return Unit.GB;
        } else if (abs >= Unit.MB.getBytes1024()) {
            return Unit.MB;
        } else if (abs >= Unit.KB.getBytes1024()) {
            return Unit.KB;
        } else {
            return Unit.B;
        }
    }

    @Deprecated
    public static long getSize(final String string) {
        return SizeFormatter.getSize(string, true, false);
    }

    private static final Pattern DOUBLE = Pattern.compile("([\\d]+)[.,:]([\\d]+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern NUMBER = Pattern.compile("([\\d]+)", Pattern.CASE_INSENSITIVE);

    @Deprecated
    public static long getSize(String string, boolean kibi, boolean allowNegative) {
        return getSize(null, string, kibi, allowNegative);
    }

    public static long getSize(final NumberFormat format, String string, boolean kibi, boolean allowNegative) {
        final boolean negative;
        if (allowNegative) {
            negative = Pattern.compile("\\D*\\-.*").matcher(string).matches();
        } else {
            negative = false;
        }
        if (format instanceof DecimalFormat) {
            final DecimalFormatSymbols symbols = ((DecimalFormat) format).getDecimalFormatSymbols();
            string = string.replace(Character.toString(symbols.getGroupingSeparator()), "");// remove GroupingSeparator
            string = string.replace(Character.toString(symbols.getDecimalSeparator()), ".");// convert DecimalSeparator to .(for
                                                                                            // Double.parseDouble)
        }
        String[][] matches = new Regex(string, SizeFormatter.DOUBLE).getMatches();
        if (matches == null || matches.length == 0) {
            matches = new Regex(string, SizeFormatter.NUMBER).getMatches();
        }
        if (matches != null && matches.length >= 1) {
            final long unitLong = kibi ? SizeFormatter.getBestUnit(string).getBytes1024() : SizeFormatter.getBestUnit(string).getBytes1000();
            if (matches[0].length == 2 && Long.parseLong(matches[0][1]) > 0) {
                final double ret = Double.parseDouble(matches[0][0] + "." + matches[0][1]) * unitLong;
                return negative ? -Math.round(ret) : Math.round(ret);
            } else {
                final long ret = Long.parseLong(matches[0][0]) * unitLong;
                return negative ? -ret : ret;
            }
        }
        return -1;
    }

    private static final Pattern TB = Pattern.compile("t((é|e)ra(-| )?)?(b|byte|ib|ig|o|octet)", Pattern.CASE_INSENSITIVE);
    private static final Pattern GB = Pattern.compile("g(iga(-| )?)?(b|byte|ib|ig|o|octet)", Pattern.CASE_INSENSITIVE);
    private static final Pattern MB = Pattern.compile("m((é|e)ga(-| )?)?(b|byte|ib|o|octet)", Pattern.CASE_INSENSITIVE);
    private static final Pattern KB = Pattern.compile("k(ilo(-| )?)?(b|byte|ib|o|octet)", Pattern.CASE_INSENSITIVE);

    private static Unit getBestUnit(String unitText) {
        if (new Regex(unitText, SizeFormatter.TB).patternFind()) {
            return Unit.TB;
        } else if (new Regex(unitText, SizeFormatter.GB).patternFind()) {
            return Unit.GB;
        } else if (new Regex(unitText, SizeFormatter.MB).patternFind()) {
            return Unit.MB;
        } else if (new Regex(unitText, SizeFormatter.KB).patternFind()) {
            return Unit.KB;
        } else {
            return Unit.B;
        }
    }

    @Deprecated
    public static long getSize(final String string, final boolean kibi) {
        return SizeFormatter.getSize(string, kibi, false);
    }
}
