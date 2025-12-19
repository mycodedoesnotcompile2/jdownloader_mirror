/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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

import java.text.DateFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Set;
import java.util.TimeZone;
import java.util.regex.Pattern;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import org.appwork.utils.BinaryLogic;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.Regex;
import org.appwork.utils.locale._AWU;

public class TimeFormatter {
    private static final java.util.List<SimpleDateFormat> dateformats  = new ArrayList<SimpleDateFormat>();
    static {
        try {
            // use "en" over "en_GB" , see https://bugs.openjdk.org/browse/JDK-8329375 and
            // https://unicode-org.atlassian.net/browse/CLDR-14412
            final Locale locale = Locale.ENGLISH;
            SimpleDateFormat sdf;
            TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz", locale));// RFC1123
            sdf.setLenient(false);
            TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("EEEE, dd-MMM-yy HH:mm:ss zzz", locale)); // RFC1036
            sdf.setLenient(false);
            TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("EEE MMM dd HH:mm:ss yyyy z", locale));
            sdf.setLenient(false);
            TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("EEE, dd-MMM-yy HH:mm:ss z", locale));
            sdf.setLenient(false);
            TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("EEE, dd-MMM-yyyy HH:mm:ss z", locale));
            sdf.setLenient(false);
            TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("dd-MMM-yyyy HH:mm:ss z", locale));
            sdf.setLenient(false);
            TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", locale));
            sdf.setLenient(false);
            TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("dd MMM yyyy HH:mm:ss z", locale));
            sdf.setLenient(false);
            TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("EEE MMM dd HH:mm:ss z yyyy", locale));
            sdf.setLenient(false);
            TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("EEE, dd-MMM-yyyy HH:mm:ss z", locale));
            sdf.setLenient(false);
            TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("EEEE, dd-MMM-yy HH:mm:ss z", locale));
            sdf.setLenient(false);
            if (JVMVersion.getVersion().isMinimum(JavaVersion.JVM_1_7)) {
                // Java 1.6 does not support X ISO 8601 time zone
                TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'SSSX", locale));
                sdf.setLenient(false);
                TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("yyyy'-'MM'-'dd'T'HH':'mm':'ssX", locale));
                sdf.setLenient(false);
            }
            TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("EEE, dd-MMM-yyyy HH:mm:ss z", locale));
            sdf.setLenient(true);
            TimeFormatter.dateformats.add(sdf = new SimpleDateFormat("EEE MMM dd yyyy HH:mm:ss 'GMT+0000'", locale));
            sdf.setTimeZone(TimeZone.getTimeZone("GMT+0000"));
            sdf.setLenient(false);
        } catch (final Throwable e) {
            e.printStackTrace();
        }
    }
    public static final int                               HIDE_SECONDS = 1 << 1;
    public static final int                               HIDE_MARKER  = 1 << 2;
    public static final int                               CLOCK        = 1 << 3;

    public static String formatMilliSeconds(final long totalSeconds, final int flags) {
        return TimeFormatter.formatSeconds(totalSeconds / 1000, flags);
    }

    public static String formatSeconds(long totalSeconds, final int flags) {
        long days, hours, minutes, seconds;
        final StringBuilder string = new StringBuilder();
        days = totalSeconds / (24 * 60 * 60);
        totalSeconds -= days * 24 * 60 * 60;
        hours = totalSeconds / (60 * 60);
        totalSeconds -= hours * 60 * 60;
        minutes = totalSeconds / 60;
        seconds = totalSeconds - minutes * 60;
        if (!BinaryLogic.containsAll(flags, TimeFormatter.CLOCK)) {
            /* show days as extra field */
            if (days != 0) {
                string.append(days);
                string.append('d');
            }
        } else {
            /* add days to hours field */
            if (days != 0) {
                hours += days * 24;
            }
        }
        if (hours != 0 || string.length() != 0 || BinaryLogic.containsAll(flags, TimeFormatter.CLOCK)) {
            if (string.length() != 0) {
                string.append(':');
            }
            string.append(hours);
            if (BinaryLogic.containsNone(flags, TimeFormatter.HIDE_MARKER)) {
                string.append(_AWU.T.Timeformater_short_hours());
            }
        }
        if (minutes != 0 || string.length() != 0 || BinaryLogic.containsAll(flags, TimeFormatter.CLOCK)) {
            if (string.length() != 0) {
                string.append(':');
            }
            string.append(StringFormatter.fillStart(minutes + "", 2, "0"));
            if (BinaryLogic.containsNone(flags, TimeFormatter.HIDE_MARKER)) {
                string.append(_AWU.T.Timeformater_short_minute());
            }
        }
        if (BinaryLogic.containsNone(flags, TimeFormatter.HIDE_SECONDS)) {
            if (string.length() != 0) {
                string.append(':');
            }
            string.append(StringFormatter.fillStart(seconds + "", 2, "0"));
            if (BinaryLogic.containsNone(flags, TimeFormatter.HIDE_MARKER)) {
                string.append(_AWU.T.Timeformater_short_seconds());
            }
        }
        return string.toString();
    }

    /**
     * formats (\\d+)\\w?:(\\d+) to ms
     *
     * @param text
     * @return
     */
    public static long formatStringToMilliseconds(final String text) {
        final String[] found = new Regex(text, "(\\d+)\\w?:(\\d+)").getRow(0);
        if (found == null) {
            return 0;
        }
        int hours = Integer.parseInt(found[0]);
        int minutes = Integer.parseInt(found[1]);
        if (hours >= 24) {
            hours = 24;
            minutes = 0;
        }
        if (minutes >= 60) {
            hours += 1;
            minutes = 0;
        }
        return hours * 60 * 60 * 1000 + minutes * 60 * 1000;
    }

    private static final Pattern GETMILLISECONDS_HOUR   = Pattern.compile("(h|st)", Pattern.CASE_INSENSITIVE);
    private static final Pattern GETMILLISECONDS_MINUTE = Pattern.compile("(m)", Pattern.CASE_INSENSITIVE);

    public static long getMilliSeconds(final String wait) {
        String[][] matches = new Regex(wait, "([\\d]+) ?[\\.|\\,|\\:] ?([\\d]+)").getMatches();
        if (matches == null || matches.length == 0) {
            matches = new Regex(wait, Pattern.compile("([\\d]+)")).getMatches();
        }
        if (matches == null || matches.length == 0) {
            return -1;
        }
        double res = 0;
        if (matches[0].length == 1) {
            res = Double.parseDouble(matches[0][0]);
        }
        if (matches[0].length == 2) {
            res = Double.parseDouble(matches[0][0] + "." + matches[0][1]);
        }
        if (GETMILLISECONDS_HOUR.matcher(wait).find()) {
            res *= 60 * 60 * 1000l;
        } else if (GETMILLISECONDS_MINUTE.matcher(wait).find()) {
            res *= 60 * 1000l;
        } else {
            res *= 1000l;
        }
        return Math.round(res);
    }

    public static long getMilliSeconds(final String dateString, String dateFormatString, final Locale locale) {
        if (dateString == null) {
            return -1;
        }
        String correctedDateString = dateString;
        if (!JavaVersion.getVersion().isMinimum(JavaVersion.JVM_1_7)) {
            final String newDateFormatString = dateFormatString.replaceAll("X+$", "");
            if (newDateFormatString != dateFormatString) {
                final long ret = getMilliSeconds(correctedDateString, newDateFormatString + "Z", locale);
                if (ret != -1) {
                    return ret;
                }
                dateFormatString = newDateFormatString;
            }
        }
        final Set<String> dateFormatVariants = new LinkedHashSet<String>();
        dateFormatVariants.add(dateFormatString);
        dateFormatVariants.add(dateFormatString.replaceAll("H+", "kk"));// Hour in day (0-23) to Hour in day (1-24)
        dateFormatVariants.add(dateFormatString.replaceAll("k+", "HH"));// Hour in day (1-24) to Hour in day (0-23)
        dateFormatVariants.add(dateFormatString.replaceAll("h+", "KK")); // Hour in am/pm (1-12) to Hour in am/pm (0-11)
        dateFormatVariants.add(dateFormatString.replaceAll("h+", "kk")); // Hour in am/pm (1-12) to Hour in day (1-24)
        dateFormatVariants.add(dateFormatString.replaceAll("h+", "HH")); // Hour in am/pm (1-12) to Hour in day (0-23)
        final Set<String> variants = new HashSet<String>();
        variants.add(correctedDateString);
        variants.add(correctedDateString.replaceAll("(?i)Sept($|\\s)", "Sep$1"));
        variants.add(correctedDateString.replaceAll("(?i)Sep($|\\s)", "Sept$1"));
        for (String input : variants) {
            for (final String dateFormatVariant : dateFormatVariants) {
                final SimpleDateFormat dateFormat = new SimpleDateFormat(dateFormatVariant, locale != null ? locale : Locale.ENGLISH);
                try {
                    dateFormat.setLenient(false);
                    if (dateFormatVariant.contains("'Z'")) {
                        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
                    }
                    return dateFormat.parse(input).getTime();
                } catch (final Exception ignore) {
                }
            }
        }
        return -1;
    }

    public static Date parseDateString(final String date) {
        if (date == null) {
            return null;
        }
        final Date ret = parseDateString(date, false);
        if (ret != null) {
            return ret;
        } else {
            return parseDateString(date, true);
        }
    }

    public static Date parseDateString(final String date, final boolean fix) {
        if (date == null) {
            return null;
        }
        for (final SimpleDateFormat format : TimeFormatter.dateformats) {
            String parseDate = date.trim();
            if (fix) {
                final DateFormatSymbols symbols = format.getDateFormatSymbols();
                for (final String shortWeekDay : symbols.getShortWeekdays()) {
                    if (shortWeekDay != null && shortWeekDay.length() > 0 && parseDate.contains(shortWeekDay)) {
                        /* fix broken weekDayName */
                        parseDate = parseDate.replaceAll("(" + Pattern.quote(shortWeekDay) + "[a-zA-Z]+)", shortWeekDay);
                        break;
                    }
                }
                for (final String shortMonth : symbols.getShortMonths()) {
                    if (shortMonth != null && shortMonth.length() > 0 && parseDate.contains(shortMonth)) {
                        /* fix broken shortMonthName */
                        parseDate = parseDate.replaceAll("(" + Pattern.quote(shortMonth) + "[a-zA-Z]+)", shortMonth);
                        break;
                    }
                }
                // en_GB has "Sept" but en has "Sep"
                parseDate = parseDate.replaceAll("(?i)Sept($|\\s)", "Sep$1");
            }
            try {
                synchronized (format) {
                    return format.parse(parseDate);
                }
            } catch (final Throwable e2) {
            }
        }
        return null;
    }

    /**
     * @param date
     * @return
     * @throws DatatypeConfigurationException
     */
    public static long getTimestampByGregorianTime(String date) throws DatatypeConfigurationException {
        final DatatypeFactory f = DatatypeFactory.newInstance();
        final XMLGregorianCalendar xgc = f.newXMLGregorianCalendar(date);
        return xgc.toGregorianCalendar().getTime().getTime();
    }
}
