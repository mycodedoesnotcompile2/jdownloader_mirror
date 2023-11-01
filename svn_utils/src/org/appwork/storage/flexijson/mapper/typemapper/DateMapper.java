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

import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.CopyOnWriteArrayList;

import org.appwork.storage.StorableDateFormat;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.mapper.FlexiTypeMapper;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.storage.simplejson.mapper.Setter;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 *
 */
public class DateMapper implements FlexiTypeMapper {
    public class LocalTimeFormat extends ThreadLocal<SimpleDateFormat> {
        private final String format;

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
            ret.setLenient(false);
            return ret;
        }
    }

    // 2022-11-10T17:15:22+0100
    // ISO-8601
    public static final String    JSON_DEFAULT_FORMAT = "yyyy-MM-dd'T'HH:mm:sszzz";
    private List<LocalTimeFormat> formats;

    /**
     *
     */
    public DateMapper() {
        formats = new CopyOnWriteArrayList<LocalTimeFormat>();
        // ISO-8601
        // addFormat(JSON_DEFAULT_FORMAT);
        addFormat("yyyy-MM-dd'T'HH:mm:ss zzz");
        addFormat("yyyy-MM-dd'T'HH:mm:ssZ");
        addFormat("yyyy-MM-dd'T'HH:mm:ss");
        addFormat("yyyy/MM/dd HH:mm:ssZ");
        addFormat("yyyy/MM/dd HH:mm:ss");
        addFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");
        addFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
        addFormat("yyyy-MM-dd'T'HH:mmZ");
        addFormat("yyyy-MM-dd'T'HH:mm");
        addFormat("yyyy-MM-dd HH:mm:ssZ");
        addFormat("yyyy-MM-dd HH:mm:ss");
        addFormat("yyyy-MM-dd");
        addFormat("HH:mm:ss");
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

    public FlexiJSonNode obj2JSon(FlexiJSonMapper mapper, Object obj, Getter reference, List<CompiledType> typeHirarchy) {
        SimpleDateFormat formater = getSerializationFormater(reference);
        if (formater == null) {
            if (mapper != null) {
                return mapper.createFlexiJSonValue(((Date) obj).getTime());
            } else {
                return new FlexiJSonValue(((Date) obj).getTime());
            }
        } else {
            if (mapper != null) {
                return mapper.createFlexiJSonValue(formater.format((Date) obj));
            } else {
                return new FlexiJSonValue(formater.format((Date) obj));
            }
        }
    }

    public SimpleDateFormat getSerializationFormater(Getter reference) {
        if (reference != null) {
            ClassCache cc = reference.classCache;
            List<StorableDateFormat> dateformat = cc.getAnnotations(reference.key, StorableDateFormat.class);
            if (dateformat != null && dateformat.size() > 0) {
                return new SimpleDateFormat(dateformat.get(0).value());
            }
        }
        return formats.get(0).get();
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
                return new Date((Long) ((FlexiJSonValue) node).getValue());
            case STRING:
                if (((FlexiJSonValue) node).getValue().equals("")) {
                    return null;
                }
                String s = (String) ((FlexiJSonValue) node).getValue();
                Date ret = parseString(s);
                if (ret != null) {
                    return ret;
                }
                throw new FlexiMapperException(node, type, "Cannot Map node to Date " + (String) ((FlexiJSonValue) node).getValue());
            case NULL:
                return null;
            default:
                break;
            }
        }
        throw new FlexiMapperException(node, type, "Cannot Map node to Date");
    }

    public Date parseString(String s) {
        if ("$NOW".equalsIgnoreCase(s)) {
            return new Date();
        }
        s = s.replaceAll("Z$", "+0000");
        s = s.replaceAll("(\\+|\\-)\\s*(\\d{2}):(\\d{2})$", "$1$2$3");
        s = s.replaceAll("(\\+|\\-)\\s*(\\d{2})$", "$1$200");
        for (final LocalTimeFormat f : formats) {
            final ParsePosition pp = new ParsePosition(0);
            final Date ret = f.get().parse(s, pp);
            if (pp.getErrorIndex() < 0 && pp.getIndex() == s.length()) {
                return ret;
            }
        }
        return null;
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
    public static Date parseJsonDefault(String dateString) throws FlexiMapperException {
        return ((Date) new DateMapper().json2Obj(null, new FlexiJSonValue(String.valueOf(dateString)), CompiledType.create(Date.class), null));
    }

    /**
     * @param availableSince
     * @return
     */
    public static String formatJsonDefault(long timestamp) {
        return new SimpleDateFormat(DateMapper.JSON_DEFAULT_FORMAT, Locale.US).format(timestamp);
    }

    /**
     * @param minimumBuildDate
     * @return
     */
    public static String formatJsonDefault(Date date) {
        return formatJsonDefault(date.getTime());
    }
}
