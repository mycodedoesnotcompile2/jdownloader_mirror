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
package org.appwork.storage.flexijson.mapper.typemapper;

import java.text.ParseException;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;
import java.util.concurrent.CopyOnWriteArrayList;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.StorableDateFormat;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.mapper.DefaultObjectToJsonContext;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.mapper.FlexiTypeMapper;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.storage.simplejson.mapper.Setter;
import org.appwork.utils.DebugMode;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.StringUtils;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 *
 */
public class DateMapper implements FlexiTypeMapper {
    public static interface FormaterInterface {
        String format(DateMapper dateMapper, Object obj);
    }

    public class Delegate extends SimpleDateFormat implements FormaterInterface {
        private TimeZone internalTimeZone;

        /**
         * @param value
         */
        public Delegate(String value) {
            super(value);
            internalTimeZone = getTimeZone();
            ;
        }

        protected void updateTimeZone(DateMapper dateMapper, SimpleDateFormat threadSafe) {
            TimeZone currentTimeZone = dateMapper.getTimeZone();
            if (currentTimeZone != null) {
                TimeZone inter = internalTimeZone;
                if (inter == null || !StringUtils.equals(currentTimeZone.getID(), inter.getID())) {
                    LogV3.info("TimeZone update: " + internalTimeZone.getID() + " -> " + currentTimeZone.getID());
                    internalTimeZone = currentTimeZone;
                    threadSafe.setTimeZone(currentTimeZone);
                }
            }
        }

        /**
         * @see org.appwork.storage.flexijson.mapper.typemapper.DateMapper.FormaterInterface#format(org.appwork.storage.flexijson.mapper.typemapper.DateMapper,
         *      java.util.Date)
         */
        @Override
        public String format(DateMapper dateMapper, Object obj) {
            updateTimeZone(dateMapper, this);
            return format(obj);
        }
    }

    public static class LocalTimeFormat extends ThreadLocal<SimpleDateFormat> implements FormaterInterface {
        protected final String format;
        private TimeZone       internalTimeZone;

        public LocalTimeFormat(final String format) {
            this.format = format;
        }

        @Override
        public String toString() {
            return format;
        }

        @Override
        protected SimpleDateFormat initialValue() {
            final SimpleDateFormat ret = new SimpleDateFormat(format, Locale.US);
            internalTimeZone = ret.getTimeZone();
            ret.setLenient(false);
            return ret;
        }

        /**
         * @see org.appwork.storage.flexijson.mapper.typemapper.DateMapper.FormaterInterface#format(java.util.Date)
         */
        @Override
        public String format(DateMapper dateMapper, Object obj) {
            SimpleDateFormat threadSafe = get();
            updateTimeZone(dateMapper, threadSafe);
            return threadSafe.format(obj);
        }

        /**
         * @param timestamp
         * @return
         */
        public String format(DateMapper dateMapper, long timestamp) {
            return this.format(dateMapper, new Date(timestamp));
        }

        /**
         * @param s
         * @param pp
         * @return
         * @throws ParseException
         */
        public Date parse(DateMapper dateMapper, String s, ParsePosition pp) {
            SimpleDateFormat threadSafe = get();
            updateTimeZone(dateMapper, threadSafe);
            return threadSafe.parse(s, pp);
        }

        protected void updateTimeZone(DateMapper dateMapper, SimpleDateFormat threadSafe) {
            TimeZone currentTimeZone = dateMapper.getTimeZone();
            if (currentTimeZone != null) {
                TimeZone inter = internalTimeZone;
                if (inter == null || !StringUtils.equals(currentTimeZone.getID(), inter.getID())) {
                    LogV3.info("TimeZone update: " + internalTimeZone.getID() + " -> " + currentTimeZone.getID());
                    internalTimeZone = currentTimeZone;
                    threadSafe.setTimeZone(currentTimeZone);
                }
            }
        }
    }

    // 2022-11-10T17:15:22+0100
    // ISO-8601
    // X is not 1.6 ready
    public static final String                 JSON_DEFAULT_FORMAT = JVMVersion.getVersion().isMinimum(JavaVersion.JVM_1_7) ? "yyyy-MM-dd'T'HH:mm:ssXXX" : "yyyy-MM-dd'T'HH:mm:ssZZZ";
    public static final LocalTimeFormat        DEFAULT_FORMAT      = new LocalTimeFormat(JSON_DEFAULT_FORMAT) {
                                                                       public String format(DateMapper dateMapper, Object obj) {
                                                                           try {
                                                                               String ret = super.format(dateMapper, obj);
                                                                               if (JVMVersion.getVersion().isLowerThan(JavaVersion.JVM_1_7)) {
                                                                                   ret = ret.substring(0, ret.length() - 2) + ":" + ret.substring(ret.length() - 2);
                                                                               }
                                                                               long ms = 0;
                                                                               if (obj instanceof Number) {
                                                                                   ms = ((Number) obj).longValue();
                                                                               } else if (obj instanceof Date) {
                                                                                   ms = ((Date) obj).getTime();
                                                                               }
                                                                               if (ms % 1000 > 0) {
                                                                                   // add ms if required
                                                                                   ret = ret.replaceAll("(.*)([+-]\\d{2}:\\d{2})$", "$1." + (ms % 1000) + "$2");
                                                                               } else {
                                                                                   // remove time if 0
                                                                                   ret = ret.replaceAll("(.*)T00:00:00([+-]\\d{2}:\\d{2})$", "$1$2");
                                                                                   // remove Seconds if zero
                                                                                   ret = ret.replaceAll("(.*T\\d{2}:\\d{2}):00([+-]\\d{2}:\\d{2})$", "$1$2");
                                                                               }
                                                                               return ret;
                                                                           } catch (IllegalArgumentException e) {
                                                                               DebugMode.debugger();
                                                                               throw e;
                                                                           }
                                                                       };
                                                                   };
    private static final List<LocalTimeFormat> DEFAULT_FORMATS     = new ArrayList<DateMapper.LocalTimeFormat>();

    private static void addWithTimeZones(String tf) {
        if (JVMVersion.getVersion().isMinimum(JavaVersion.JVM_1_7)) {
            DEFAULT_FORMATS.add(new LocalTimeFormat(tf + "XXX"));
            DEFAULT_FORMATS.add(new LocalTimeFormat(tf + " XXX"));
        }
        if (JVMVersion.getVersion().isMinimum(JavaVersion.JVM_1_7)) {
            DEFAULT_FORMATS.add(new LocalTimeFormat(tf + "X"));
            DEFAULT_FORMATS.add(new LocalTimeFormat(tf + " X"));
        }
        DEFAULT_FORMATS.add(new LocalTimeFormat(tf + "ZZZ"));
        DEFAULT_FORMATS.add(new LocalTimeFormat(tf + "Z"));
        DEFAULT_FORMATS.add(new LocalTimeFormat(tf + "z"));
        DEFAULT_FORMATS.add(new LocalTimeFormat(tf + " ZZZ"));
        DEFAULT_FORMATS.add(new LocalTimeFormat(tf + " Z"));
        DEFAULT_FORMATS.add(new LocalTimeFormat(tf + " z"));
        DEFAULT_FORMATS.add(new LocalTimeFormat(tf));
    }

    private static TimeZone DEFAULT_TIMEZONE = TimeZone.getDefault();

    /**
     * @return
     */
    public TimeZone getTimeZone() {
        return DEFAULT_TIMEZONE;
    }

    static {
        // add all formats we want to support
        DEFAULT_FORMATS.add(DEFAULT_FORMAT);
        boolean XAllowed = JVMVersion.getVersion().isMinimum(JavaVersion.JVM_1_7);
        if (XAllowed) {
            DEFAULT_FORMATS.add(new LocalTimeFormat("yyyy-MM-dd'T'HH:mm:ssXXX")); // ISO 8601 with timezone
        }
        if (XAllowed) {
            DEFAULT_FORMATS.add(new LocalTimeFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")); // ISO 8601 with milliseconds and timezone
        }
        // ISO 8601 Formats
        addWithTimeZones("yyyy-MM-dd'T'HH:mm:ss.SSS");
        addWithTimeZones("yyyy-MM-dd'T'HH:mm:ss");
        addWithTimeZones("yyyy-MM-dd'T'HH:mm");
        // Common Date-Time Formats
        addWithTimeZones("yyyy-MM-dd HH:mm:ss.SSS");
        addWithTimeZones("yyyy-MM-dd HH:mm:ss");
        addWithTimeZones("yyyy-MM-dd HH:mm");
        addWithTimeZones("yyyy/MM/dd HH:mm:ss.SSS");
        addWithTimeZones("yyyy/MM/dd HH:mm:ss");
        addWithTimeZones("yyyy/MM/dd HH:mm");
        addWithTimeZones("dd.MM.yyyy HH:mm:ss.SSS");
        addWithTimeZones("dd.MM.yyyy HH:mm:ss");
        addWithTimeZones("dd.MM.yyyy HH:mm");
        addWithTimeZones("dd-MM-yyyy HH:mm:ss.SSS");
        addWithTimeZones("dd-MM-yyyy HH:mm:ss");
        addWithTimeZones("dd-MM-yyyy HH:mm");
        // Date Only Formats
        addWithTimeZones("yyyy-MM-dd"); // Date only
        addWithTimeZones("yyyy/MM/dd"); // Date only with slashes
        addWithTimeZones("MM/dd/yyyy"); // US date format
        addWithTimeZones("dd.MM.yyyy"); // European date format
        addWithTimeZones("dd-MM-yyyy"); // European date format with dashes
        // RFC 822 and other less common formats
        addWithTimeZones("EEE, dd MMM yyyy HH:mm:ss");// RFC 822
        // Time Only Formats
        addWithTimeZones("HH:mm:ss"); // Time only, 24-hour format
        addWithTimeZones("HH:mm"); // Time only, 24-hour format
        addWithTimeZones("hh:mm:ss a"); // Time only, 12-hour format with AM/PM
        addWithTimeZones("hh:mm a"); // Time only, 12-hour format with AM/PM
    }
    private List<LocalTimeFormat> formats;

    /**
     *
     */
    public DateMapper() {
        formats = new CopyOnWriteArrayList<LocalTimeFormat>();
        // ISO-8601
        formats.addAll(DEFAULT_FORMATS);
    }

    /**
     * @param string
     */
    public void addFormat(String format) {
        formats.add(new LocalTimeFormat(format));
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.simplejson.mapper.FlexiTypeMapper#mapObject(java.lang.Object)
     */
    public FlexiJSonNode obj2JSon(FlexiJSonMapper mapper, Object obj, Getter reference, DefaultObjectToJsonContext typeHirarchy) {
        updateDefaultTimeZone();
        FormaterInterface formater = getSerializationFormater(reference);
        if (formater == null) {
            if (mapper != null) {
                return mapper.createFlexiJSonValue(((Date) obj).getTime());
            } else {
                return new FlexiJSonValue(((Date) obj).getTime());
            }
        } else {
            if (mapper != null) {
                return mapper.createFlexiJSonValue(formater.format(this, obj));
            } else {
                return new FlexiJSonValue(formater.format(this, obj));
            }
        }
    }

    public FormaterInterface getSerializationFormater(Getter reference) {
        if (reference != null) {
            ClassCache cc = reference.classCache;
            List<StorableDateFormat> dateformat = cc.getAnnotations(reference.key, StorableDateFormat.class);
            if (dateformat != null && dateformat.size() > 0) {
                return new Delegate(dateformat.get(0).value());
            }
        }
        return formats.get(0);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.simplejson.mapper.FlexiTypeMapper#json2Obj(org.appwork.storage.simplejson.JSonNode)
     */
    public Object json2Obj(FlexiJSonMapper mapper, FlexiJSonNode node, CompiledType type, Setter reference) throws FlexiMapperException {
        if (node instanceof FlexiJSonValue) {
            switch (((FlexiJSonValue) node).getType()) {
            case LONG:
                // fallthrough!
            case DOUBLE:
                return numberToDate(((Number) ((FlexiJSonValue) node).getValue()));
            case STRING:
                if (((FlexiJSonValue) node).getValue().equals("")) {
                    return null;
                }
                final String s = (String) ((FlexiJSonValue) node).getValue();
                final Date ret = parseString(s);
                if (ret != null) {
                    return ret;
                } else {
                    throw new FlexiMapperException(node, type, "Cannot Map node to Date " + (String) ((FlexiJSonValue) node).getValue());
                }
            case NULL:
                return null;
            default:
                break;
            }
        }
        throw new FlexiMapperException(node, type, "Cannot Map node to Date");
    }

    protected Date numberToDate(Number number) {
        if (number == null || number.longValue() <= 0) {
            return null;
        } else {
            return new Date(number.longValue());
        }
    }

    public Date parseString(String s) {
        updateDefaultTimeZone();
        if ("$NOW".equalsIgnoreCase(s)) {
            return new Date();
        } else {
            if (JVMVersion.getVersion().isLowerThan(JavaVersion.JVM_1_7)) {
                if (s.contains("T")) {
                    // only if the format contains a time
                    s = s.replaceAll("Z$", "+0000");
                    s = s.replaceAll("(\\+|\\-)\\s*(\\d{2}):(\\d{2})$", "$1$2$3");
                    s = s.replaceAll("(\\+|\\-)\\s*(\\d{2})$", "$1$200");
                }
            }
            for (final LocalTimeFormat f : formats) {
                final ParsePosition pp = new ParsePosition(0);
                final Date ret = f.parse(this, s, pp);
                if (ret != null) {
                    if (pp.getErrorIndex() < 0 && pp.getIndex() == s.length()) {
                        return ret;
                    } else {
                        // DebugMode.debugger();
                    }
                }
            }
            return null;
        }
    }

    @Override
    public boolean canConvert2Json(Object obj, Getter getter) {
        return obj != null && obj instanceof Date;
    }

    @Override
    public boolean canConvert2Object(FlexiJSonNode node, CompiledType type, Setter setter) {
        return type.isInstanceOf(Date.class);
        // return type instanceof Class && Date.class.isAssignableFrom((Class) type);
    }

    /**
     * @param string
     * @return
     * @throws FlexiMapperException
     */
    public static Date parse(String dateString) throws FlexiMapperException {
        return new DateMapper().parseString(dateString);
    }

    /**
     * @param availableSince
     * @return
     */
    public static String formatJsonDefault(long timestamp) {
        updateDefaultTimeZone();
        DateMapper mapper = new DateMapper();
        return mapper.getSerializationFormater(null).format(mapper, new Date(timestamp));
    }

    protected static void updateDefaultTimeZone() {
        DEFAULT_TIMEZONE = TimeZone.getDefault();
    }

    /**
     * @param minimumBuildDate
     * @return
     */
    public static String formatJsonDefault(Date date) {
        updateDefaultTimeZone();
        DateMapper mapper = new DateMapper();
        return mapper.getSerializationFormater(null).format(mapper, date);
    }
}
