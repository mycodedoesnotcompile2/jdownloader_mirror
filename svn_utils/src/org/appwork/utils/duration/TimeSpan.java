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
package org.appwork.utils.duration;

import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.StorableDoc;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.Time;

/**
 * @author thomas
 * @date 04.11.2022
 *
 */
@StorableDoc("A timespan instance defines an interval or a timeperiod. Available Units: [+-]*Y(ears)*M(onths)*W(eeks)*D(ays)*h(ours)*m(inutes)*s(seconds)*S(milliSeconds)\r\n.Each unit is optional, but there has to be at least one.")
public class TimeSpan implements Comparable<TimeSpan> {
    public static final TimeSpan HOURS_1      = TimeSpan.parseWithoutException("1h");
    public static final TimeSpan HOURS_12     = TimeSpan.parseWithoutException("12h");
    public static final TimeSpan DAYS_1       = TimeSpan.parseWithoutException("1D");
    public static final TimeSpan WEEKS_1      = TimeSpan.parseWithoutException("1W");
    public static final TimeSpan MINUTES_10   = TimeSpan.parseWithoutException("10m");
    public static final TimeSpan MINUTES_1    = TimeSpan.parseWithoutException("1m");
    public static final TimeSpan AVERAGE_YEAR = TimeSpan.parseWithoutException("1Y");
    public static final TimeSpan MINUTES_5    = TimeSpan.parseWithoutException("5m");
    public static final TimeSpan ZERO         = TimeSpan.parseWithoutException("0s");
    public static final TimeSpan MAX          = TimeSpan.fromMillis(Long.MAX_VALUE);
    // Long.MIN_VALUE does not work, because the value is stored internally as positive value;
    public static final TimeSpan MIN          = TimeSpan.fromMillis(Long.MAX_VALUE * -1);

    /**
     * @param string
     * @return
     */
    private static TimeSpan parseWithoutException(final String string) {
        try {
            return parse(string);
        } catch (final InvalidTimeSpanException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param longValue
     * @return
     * @throws InvalidTimeSpanException
     */
    public static TimeSpan fromMillis(final long durationInMS) {
        return new TimeSpan(durationInMS).normalized();
    }

    protected TimeSpan(final boolean negative, final long years, final long months, final long weeks, final long days, final long hours, final long minutes, final long seconds, final long milliseconds) {
        this.negative = negative;
        this.years = this.notNegative(years);
        this.months = this.notNegative(months);
        this.weeks = this.notNegative(weeks);
        this.days = this.notNegative(days);
        this.hours = this.notNegative(hours);
        this.minutes = this.notNegative(minutes);
        this.seconds = this.notNegative(seconds);
        this.milliseconds = this.notNegative(milliseconds);
    }

    protected final long notNegative(final long value) {
        if (value < 0) {
            throw new IllegalArgumentException("<0 not supported:" + value);
        } else {
            return value;
        }
    }

    protected TimeSpan(final TimeSpan timeSpan) {
        this(timeSpan.isNegative(), timeSpan.getYears(), timeSpan.getMonths(), timeSpan.getWeeks(), timeSpan.getDays(), timeSpan.getHours(), timeSpan.getMinutes(), timeSpan.getSeconds(), timeSpan.getMilliseconds());
    }

    /**
     * @param durationInMS
     */
    public TimeSpan(final long durationInMS) {
        this(durationInMS < 0, 0, 0, 0, 0, 0, 0, 0, durationInMS < 0 ? multiplyExact(durationInMS, -1) : durationInMS);
    }

    /**
     * Parse a readable time *Y(year)*M(month)*W(eeks)*D(ays)*h(ours)*m(inutes)*s(econds)*S(milliSeconds) while * can be fixed point or
     * floating point number (max long/double)
     *
     * This method also parses the PnDTnHnMn.nS (ISO-8601) format like java.time.Duration
     *
     * @param from
     * @return
     * @throws InvalidTimeSpanException
     * @throws NumberFormatException
     * @throws IllegalTargetUnitsException
     */
    public static TimeSpan parse(String spanAsString) throws InvalidTimeSpanException {
        try {
            spanAsString = spanAsString.trim();
            // fix for ms
            spanAsString = spanAsString.replace("ms", Unit.MILLISECONDS.unit + "");
            final char[] chars = spanAsString.toCharArray();
            TimeSpan ret = new TimeSpan(0);
            final StringBuilder sb = new StringBuilder();
            char c;
            boolean contentStarted = false;
            final HashSet<Unit> dupe = new HashSet<Unit>();
            for (int i = 0; i < chars.length; i++) {
                c = chars[i];
                if (Character.isWhitespace(c)) {
                    continue;
                }
                if (c == 'P' || c == 'p') {
                    ret = parseIso8601(chars);
                    return ret;
                }
                if (c == '!') {
                    if (sb.length() > 0) {
                        if (dupe.size() == 0) {
                            ret = ret.with(Unit.MILLISECONDS, Long.parseLong(sb.toString()));
                            sb.setLength(0);
                        } else {
                            throw new InvalidTimeSpanException("Unexpected End of literal: " + sb.toString());
                        }
                    }
                    break;
                } else if (c == '-' && !contentStarted) {
                    ret = ret.withNegative(!ret.isNegative());
                } else if (Character.isDigit(c)) {
                    // digit
                    contentStarted = true;
                    sb.append(c);
                } else {
                    // unit
                    contentStarted = true;
                    StringBuilder literal = new StringBuilder();
                    literal.append(c);
                    for (int ii = i + 1; ii < chars.length; ii++) {
                        if (Character.isWhitespace(chars[ii])) {
                            continue;
                        } else if (Character.isDigit(chars[ii]) || chars[ii] == '-' || chars[ii] == '!') {
                            break;
                        }
                        literal.append(chars[ii]);
                        i = ii;
                    }
                    for (final Unit unit : Unit.values()) {
                        if (unit.is(literal)) {
                            if (!dupe.add(unit)) {
                                throw new InvalidTimeSpanException("Unit duplicate: " + c);
                            }
                            ret = ret.with(unit, Long.parseLong(sb.toString()));
                            literal = null;
                            break;
                        }
                    }
                    if (literal != null) {
                        throw new InvalidTimeSpanException("Unknown unit:" + literal);
                    }
                    sb.setLength(0);
                }
            }
            if (sb.length() > 0) {
                if (dupe.size() == 0) {
                    ret = ret.with(Unit.MILLISECONDS, Long.parseLong(sb.toString()));
                    sb.setLength(0);
                } else {
                    throw new InvalidTimeSpanException("Unexpected End of literal: " + sb.toString());
                }
            }
            ret.toMillis();
            return ret;
        } catch (final NumberFormatException e) {
            throw new InvalidTimeSpanException(e);
        }
    }

    /**
     * @param chars
     * @param useAverageYear
     * @return
     * @throws InvalidTimeSpanException
     */
    public static TimeSpan parseIso8601(final char[] chars) throws InvalidTimeSpanException {
        final StringBuilder sb = new StringBuilder();
        char c;
        boolean time = false;
        boolean p = false;
        // int invert = 1;
        TimeSpan ret = new TimeSpan(0);
        final HashSet<Unit> dupe = new HashSet<Unit>();
        for (int i = 0; i < chars.length; i++) {
            c = chars[i];
            if (Character.isWhitespace(c)) {
                continue;
            } else if (c == '+') {
                continue;
            }
            if (c == '-' && !p) {
                ret = ret.withNegative(!ret.isNegative());
                continue;
            }
            if (c == 'P' || c == 'p') {
                if (p) {
                    throw new InvalidTimeSpanException("Illegal usage of 'p'");
                }
                p = true;
                continue;
            }
            if (!time && c == 'T') {
                time = true;
                continue;
            }
            if (c == '-') {
                throw new InvalidTimeSpanException("Illegal - at this position");
            }
            if (c == '.' || c == ',' || Character.isDigit(c)) {
                sb.append(c);
            } else {
                if (!p) {
                    throw new InvalidTimeSpanException("Missing leading [-+]P");
                }
                if (c == 'Y' || c == 'y') {
                    if (time) {
                        throw new InvalidTimeSpanException("Cannot use '" + c + "' if 'T' was set before");
                    } else if (!dupe.add(Unit.YEARS)) {
                        throw new InvalidTimeSpanException("Unit duplicate: " + c);
                    } else {
                        ret = ret.with(Unit.YEARS, Long.parseLong(sb.toString()));
                    }
                } else if (!time && (c == 'M' || c == 'm')) {
                    if (!dupe.add(Unit.MONTHS)) {
                        throw new InvalidTimeSpanException("Unit duplicate: " + c);
                    } else {
                        ret = ret.with(Unit.MONTHS, Long.parseLong(sb.toString()));
                    }
                } else if (c == 'W' || c == 'w') {
                    if (time) {
                        throw new InvalidTimeSpanException("Cannot use '" + c + "' if 'T' was set before");
                    } else if (!dupe.add(Unit.WEEKS)) {
                        throw new InvalidTimeSpanException("Unit duplicate: " + c);
                    } else {
                        ret = ret.with(Unit.WEEKS, Long.parseLong(sb.toString()));
                    }
                } else if (c == 'D' || c == 'd') {
                    if (!dupe.add(Unit.DAYS)) {
                        throw new InvalidTimeSpanException("Unit duplicate: " + c);
                    } else {
                        ret = ret.with(Unit.DAYS, Long.parseLong(sb.toString()));
                    }
                } else if (c == 'h' || c == 'H') {
                    if (!dupe.add(Unit.HOURS)) {
                        throw new InvalidTimeSpanException("Unit duplicate: " + c);
                    } else {
                        ret = ret.with(Unit.HOURS, Long.parseLong(sb.toString()));
                    }
                } else if (time && (c == 'M' || c == 'm')) {
                    if (!dupe.add(Unit.MINUTES)) {
                        throw new InvalidTimeSpanException("Unit duplicate: " + c);
                    } else {
                        ret = ret.with(Unit.MINUTES, Long.parseLong(sb.toString()));
                    }
                } else if (c == 's' || c == 'S') {
                    if (!dupe.add(Unit.SECONDS)) {
                        throw new InvalidTimeSpanException("Unit duplicate: " + c);
                    } else {
                        final double seconds = Double.parseDouble(sb.toString());
                        ret = ret.with(Unit.SECONDS, (int) seconds);
                        ret = ret.with(Unit.MILLISECONDS, (int) ((seconds - ret.get(Unit.SECONDS)) * 1000));
                    }
                }
                sb.setLength(0);
            }
        }
        return ret;
    }

    private final long    days;
    private final long    hours;
    private final long    milliseconds;
    private final long    minutes;
    private final long    months;
    private final boolean negative;
    private final long    seconds;
    private final long    weeks;
    private final long    years;

    public boolean isNegative() {
        return this.negative;
    }

    public TimeSpan withNegative(final boolean negative) {
        if (this.isNegative() == negative) {
            return this;
        } else {
            final TimeSpan ret = new TimeSpan(negative, this.getYears(), this.getMonths(), this.getWeeks(), this.getDays(), this.getHours(), this.getMinutes(), this.getSeconds(), this.getMilliseconds());
            ret.toMillis();
            return ret;
        }
    }

    /**
     * convert timespan allowing only the given target units. The timespan value itself is NOT changed by this method. If conversion is not
     * possible, because required units are not allowed, or conversion is not possible due to missing daysPerYear property a
     * CannotConvertException is thrown
     *
     * @param hours2
     * @return
     * @throws ContextMissingException
     * @throws IllegalTargetUnitsException
     * @throws InvalidTimeSpanException
     */
    public TimeSpan convert(final Unit... allowedTargetUnits) throws IllegalTargetUnitsException {
        // WARNING: THIS METHOD MUST NOT THROW AN CannotConvertException if allowedTargetUnits is null
        long ms = this.toMillis();
        TimeSpan ret = new TimeSpan(0);
        if (this.isNegative()) {
            ms = multiplyExact(ms, -1);
            ret = ret.withNegative(true);
        }
        final Set<Unit> allowed = allowedTargetUnits == null ? null : new HashSet<Unit>(Arrays.asList(allowedTargetUnits));
        final Unit[] values = Unit.values();
        for (int i = values.length - 1; i >= 0; i--) {
            final Unit unit = values[i];
            if (ms == 0) {
                ret = ret.with(unit, 0);
            } else if (allowed == null || allowed.contains(unit)) {
                if (ms >= unit.toMillis) {
                    ret = ret.with(unit, ms / unit.toMillis);
                    ms = ms % unit.toMillis;
                    if (ms == 0) {
                        break;
                    }
                }
            }
        }
        if (ms > 0) {
            throw new IllegalTargetUnitsException("Cannot convert '" + this.toString() + "' to the desired units '" + allowed + "' without rest:" + ms, null);
        } else {
            return ret;
        }
    }

    /**
     * @return the full time in this timespan converted to MS
     */
    public long toMillis() {
        long ret = 0;
        for (final Unit unit : Unit.values()) {
            ret = addExact(ret, multiplyExact(this.get(unit), unit.toMillis));
        }
        if (this.isNegative()) {
            ret = multiplyExact(ret, -1);
        }
        return ret;
    }

    private static long addExact(final long x, final long y) {
        final long r = x + y;
        // HD 2-12 Overflow iff both arguments have the opposite sign of the result
        if (((x ^ r) & (y ^ r)) < 0) {
            throw new ArithmeticException("long overflow");
        } else {
            return r;
        }
    }

    private static long multiplyExact(final long x, final long y) {
        final long r = x * y;
        final long ax = Math.abs(x);
        final long ay = Math.abs(y);
        if (((ax | ay) >>> 31 != 0)) {
            // Some bits greater than 2^31 that might cause overflow
            // Check the result using the divide operator
            // and check for the special case of Long.MIN_VALUE * -1
            if (((y != 0) && (r / y != x)) || (x == Long.MIN_VALUE && y == -1)) {
                throw new ArithmeticException("long overflow");
            }
        }
        return r;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        } else if (!(obj instanceof TimeSpan)) {
            return false;
        } else {
            final TimeSpan other = (TimeSpan) obj;
            if (this.isNegative() != other.isNegative()) {
                return false;
            }
            for (final Unit unit : Unit.values()) {
                if (this.get(unit) != other.get(unit)) {
                    return false;
                }
            }
            return true;
        }
    }

    /**
     * @param withContext
     *            TODO
     * @return
     */
    public String format() {
        final StringBuilder ret = new StringBuilder();
        if (this.isNegative()) {
            ret.append("-");
        }
        final Unit[] values = Unit.values();
        for (int i = values.length - 1; i >= 0; i--) {
            final Unit unit = values[i];
            final long value = this.get(unit);
            if (value != 0) {
                ret.append(value).append(unit.unit);
            }
        }
        if (ret.length() < 2) {
            ret.append("0s");
        }
        return ret.toString();
    }

    public long getDays() {
        return this.get(Unit.DAYS);
    }

    public long getHours() {
        return this.get(Unit.HOURS);
    }

    public long toLong(final Unit targetUnit) {
        return this.toMillis() / targetUnit.toMillis;
    }

    /**
     * @param days2
     * @return
     */
    public double toDouble(final Unit targetUnit) {
        return this.toLong(targetUnit);
    }

    /**
     * WARNING: this method only returns the millisecnds field in this timespan. This does NOT return the full timespan. Use toMillis()
     * instead
     *
     * @return
     */
    public long getMilliseconds() {
        return this.get(Unit.MILLISECONDS);
    }

    public long getMinutes() {
        return this.get(Unit.MINUTES);
    }

    public long getMonths() {
        return this.get(Unit.MONTHS);
    }

    public long getSeconds() {
        return this.get(Unit.SECONDS);
    }

    public long getWeeks() {
        return this.get(Unit.WEEKS);
    }

    public long getYears() {
        return this.get(Unit.YEARS);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        int hashCode = this.isNegative() ? -1 : 1;
        for (final Unit unit : Unit.values()) {
            hashCode += (int) this.get(unit);
        }
        return hashCode;
    }

    protected long get(final Unit unit) {
        switch (unit) {
        case YEARS:
            return this.years;
        case MONTHS:
            return this.months;
        case WEEKS:
            return this.weeks;
        case DAYS:
            return this.days;
        case HOURS:
            return this.hours;
        case MINUTES:
            return this.minutes;
        case SECONDS:
            return this.seconds;
        case MILLISECONDS:
            return this.milliseconds;
        default:
            throw new IllegalArgumentException("unsupported unit:" + unit);
        }
    }

    protected final long get(final Unit getUnit, final Unit setUnit, final long setValue) {
        if (getUnit.equals(setUnit)) {
            return setValue;
        } else {
            return this.get(getUnit);
        }
    }

    protected final long add(final Unit getUnit, final Unit setUnit, final long setValue) {
        final long getValue = this.get(getUnit);
        if (getUnit.equals(setUnit)) {
            return addExact(getValue, setValue);
        } else {
            return getValue;
        }
    }

    public TimeSpan with(final Unit unit, final long value) {
        if (unit == null) {
            return this;
        } else if (this.get(unit) == value) {
            return this;
        } else {
            final TimeSpan ret = new TimeSpan(this.isNegative(), this.get(Unit.YEARS, unit, value), this.get(Unit.MONTHS, unit, value), this.get(Unit.WEEKS, unit, value), this.get(Unit.DAYS, unit, value), this.get(Unit.HOURS, unit, value), this.get(Unit.MINUTES, unit, value), this.get(Unit.SECONDS, unit, value), this.get(Unit.MILLISECONDS, unit, value));
            ret.toMillis();
            return ret;
        }
    }

    public TimeSpan plus(final Unit unit, final long value) {
        if (unit == null) {
            return this;
        } else if (value == 0) {
            return this;
        } else {
            final TimeSpan ret = new TimeSpan(this.isNegative(), this.add(Unit.YEARS, unit, value), this.add(Unit.MONTHS, unit, value), this.add(Unit.WEEKS, unit, value), this.add(Unit.DAYS, unit, value), this.add(Unit.HOURS, unit, value), this.add(Unit.MINUTES, unit, value), this.add(Unit.SECONDS, unit, value), this.add(Unit.MILLISECONDS, unit, value));
            ret.toMillis();
            return ret;
        }
    }

    public TimeSpan minus(final Unit unit, long value) {
        if (unit == null) {
            return this;
        } else if (value == 0) {
            return this;
        } else {
            value = multiplyExact(value, -1);
            final TimeSpan ret = new TimeSpan(this.isNegative(), this.add(Unit.YEARS, unit, value), this.add(Unit.MONTHS, unit, value), this.add(Unit.WEEKS, unit, value), this.add(Unit.DAYS, unit, value), this.add(Unit.HOURS, unit, value), this.add(Unit.MINUTES, unit, value), this.add(Unit.SECONDS, unit, value), this.add(Unit.MILLISECONDS, unit, value));
            ret.toMillis();
            return ret;
        }
    }

    @Override
    public String toString() {
        return this.format();
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(final TimeSpan o) {
        return CompareUtils.compareNumber(this.toMillis(), o.toMillis());
    }

    /**
     * @param minutes1
     * @return
     */
    public boolean isLessThan(final TimeSpan other) {
        return this.compareTo(other) < 0;
    }

    /**
     * @param l
     * @return
     * @throws ContextMissingException
     */
    public boolean isLessThan(final long milliseconds) {
        return this.toMillis() < milliseconds;
    }

    /**
     * returns this if this is bigger than other
     *
     * @param minutes1
     * @return
     */
    public TimeSpan max(final TimeSpan other) {
        if (this.isLessThan(other)) {
            return other;
        } else {
            return this;
        }
    }

    public TimeSpan min(final TimeSpan other) {
        if (this.isLessThan(other)) {
            return this;
        } else {
            return other;
        }
    }

    /**
     * @param minutes1
     * @param weeks1
     * @return
     * @throws InvalidTimeSpanException
     */
    public TimeSpan limit(final TimeSpan min, final TimeSpan max) {
        final TimeSpan minWithContext = min;
        final TimeSpan maxWithContext = max;
        TimeSpan ret = this;
        if (ret.isLessThan(minWithContext)) {
            ret = minWithContext;
        }
        if (ret.isMoreThan(maxWithContext)) {
            ret = maxWithContext;
        }
        return ret;
    }

    /**
     * @param maxWithContext
     * @return
     */
    public boolean isMoreThan(final TimeSpan other) {
        return this.compareTo(other) > 0;
    }

    /**
     * @param parse
     * @return
     */
    public boolean isSameAs(final TimeSpan other) {
        return this.compareTo(other) == 0;
    }

    /**
     * @return
     */
    public String toReadableString() {
        final StringBuilder ret = new StringBuilder();
        final Unit[] values = Unit.values();
        for (int i = values.length - 1; i >= 0; i--) {
            final Unit u = values[i];
            if (u.getValue(this) != 0) {
                final long v = u.getValue(this);
                final String unit = u.getReadableName(v);
                if (ret.length() > 0) {
                    ret.append(", ");
                }
                ret.append(v).append(" ").append(unit);
            }
        }
        if (ret.length() == 0) {
            return "0 " + Unit.SECONDS.getReadableName(0);
        }
        if (this.isNegative()) {
            return "- " + ret.toString();
        } else {
            return ret.toString();
        }
    }

    /**
     * this is slow as he&/...
     *
     * @return
     */
    public TimeSpan shorten() {
        final LinkedList<Unit> all = new LinkedList<Unit>(Arrays.asList(Unit.values()));
        TimeSpan best = null;
        while (all.size() > 0) {
            try {
                final TimeSpan shorten = this.convert(all.toArray(new Unit[] {}));
                if (best == null || shorten.format().length() < best.format().length()) {
                    best = shorten;
                }
            } catch (final IllegalTargetUnitsException e) {
                break;
            }
            all.removeLast();
        }
        return best;
    }

    /**
     * @param milliseconds2
     * @param seconds2
     * @param minutes2
     * @param hours2
     * @return
     */
    public TimeSpan withZero(final Unit... units) {
        TimeSpan ret = this;
        for (final Unit unit : units) {
            ret = ret.with(unit, 0);
        }
        return ret;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#clone()
     */
    @Override
    protected TimeSpan clone() throws CloneNotSupportedException {
        return new TimeSpan(this);
    }

    /**
     * @return
     * @throws IllegalTargetUnitsException
     */
    public TimeSpan normalized() {
        try {
            return this.convert(Unit.values());
        } catch (final IllegalTargetUnitsException e) {
            throw new WTFException("Cannot happen - because we use all values", e);
        }
    }

    /**
     * the method returns true if the lastEvent was longer ago than the TimeSpan instance specifies.
     *
     * @param lastSuccess
     * @return
     */
    public boolean isExpired(final long lastEventIndependentTimeStamp) {
        return (Time.systemIndependentCurrentJVMTimeMillis() - lastEventIndependentTimeStamp) > this.toMillis();
    }

    /**
     * Throws ArithmeticException in case of a long overflow
     *
     * @param milliseconds2
     * @return
     */
    public int toInt(final Unit unit) throws ArithmeticException {
        return this.toIntExact(this.toLong(unit));
    }

    public int toIntExact(final long value) {
        final int iValue = (int) value;
        if (iValue != value) {
            throw new ArithmeticException("integer overflow:" + value + ">" + Integer.MAX_VALUE);
        } else {
            return iValue;
        }
    }
}
