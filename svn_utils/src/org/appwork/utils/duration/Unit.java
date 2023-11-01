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

import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import org.appwork.utils.locale._AWU;

public enum Unit {
    MILLISECONDS(TimeUnit.MILLISECONDS.toMillis(1), 'S', "(?i)ms", "(?i)milliseconds?") {
        @Override
        protected String getReadableName(long value) {
            if (Math.abs(value) == 1) {
                return _AWU.T.lit_millisecond();
            } else {
                return _AWU.T.lit_milliseconds();
            }
        }
    },
    SECONDS(TimeUnit.SECONDS.toMillis(1), 's', "(?i)seconds?") {
        @Override
        protected String getReadableName(long value) {
            if (Math.abs(value) == 1) {
                return _AWU.T.lit_second();
            } else {
                return _AWU.T.lit_seconds();
            }
        }
    },
    MINUTES(TimeUnit.MINUTES.toMillis(1), 'm', "(?i)minutes?") {
        @Override
        protected String getReadableName(long value) {
            if (Math.abs(value) == 1) {
                return _AWU.T.lit_minute();
            } else {
                return _AWU.T.lit_minutes();
            }
        }
    },
    HOURS(TimeUnit.HOURS.toMillis(1), 'h', "H", "(?i)hours?") {
        @Override
        protected String getReadableName(long value) {
            if (Math.abs(value) == 1) {
                return _AWU.T.lit_hour();
            } else {
                return _AWU.T.lit_hours();
            }
        }
    },
    DAYS(TimeUnit.DAYS.toMillis(1), 'D', "d", "(?i)days?") {
        @Override
        protected String getReadableName(long value) {
            if (Math.abs(value) == 1) {
                return _AWU.T.lit_day();
            } else {
                return _AWU.T.lit_days();
            }
        }
    },
    WEEKS(TimeUnit.DAYS.toMillis(7), 'W', "w", "(?i)weeks?") {
        @Override
        protected String getReadableName(long value) {
            if (Math.abs(value) == 1) {
                return _AWU.T.lit_week();
            } else {
                return _AWU.T.lit_weeks();
            }
        }
    },
    MONTHS((long) ((365d / 12) * TimeUnit.DAYS.toMillis(1)), 'M', "(?i)months?") {
        @Override
        protected String getReadableName(long value) {
            if (Math.abs(value) == 1) {
                return _AWU.T.lit_month();
            } else {
                return _AWU.T.lit_months();
            }
        }
    },
    YEARS(TimeUnit.DAYS.toMillis(365), 'Y', "y", "(?i)years?") {
        @Override
        protected String getReadableName(long value) {
            if (Math.abs(value) == 1) {
                return _AWU.T.lit_year();
            } else {
                return _AWU.T.lit_years();
            }
        }
    };

    public final Pattern[] patterns;
    public final long      toMillis;
    public final char      unit;

    Unit(long toMillis, char unit, String... patterns) {
        this.toMillis = toMillis;
        this.unit = unit;
        this.patterns = new Pattern[patterns.length];
        for (int i = 0; i < patterns.length; i++) {
            this.patterns[i] = Pattern.compile(patterns[i]);
        }
    }

    public TimeSpan deriveTimeSpan(TimeSpan timeSpan, long value) {
        return timeSpan.with(this, value);
    }

    public long getValue(TimeSpan timeSpan) {
        return timeSpan.get(this);
    }

    /**
     * @param v
     * @return
     */
    protected abstract String getReadableName(long value);

    /**
     * @param literal
     * @return
     */
    boolean is(StringBuilder literal) {
        if (literal.length() == 1 && literal.charAt(0) == unit) {
            return true;
        }
        for (Pattern p : patterns) {
            if (p.matcher(literal).matches()) {
                return true;
            }
        }
        return false;
    }
}
