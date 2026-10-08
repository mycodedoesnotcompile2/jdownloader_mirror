package org.jdownloader.captcha.v2;

import java.util.ArrayList;
import java.util.concurrent.TimeUnit;

import org.appwork.storage.Storable;

/**
 * One custom limit rule of an external captcha solver: at most {@link #getMaxCaptchas()} captchas within the last {@link #getInterval()}
 * {@link #getUnit()} (sliding window). All enabled rules of a solver apply at the same time, and only while the solver's "custom limits"
 * switch is enabled (see {@code CaptchaSolverPluginConfig#isCustomLimitsEnabled()}).
 */
public class CaptchaSolverLimitRule implements Storable {
    /**
     * Units the user can choose for the interval. The enum keeps the user's choice (e.g. "2 hours" stays "2 hours" instead of becoming "120
     * minutes").
     */
    public enum IntervalUnit {
        MINUTES(TimeUnit.MINUTES),
        HOURS(TimeUnit.HOURS),
        DAYS(TimeUnit.DAYS);
        private final TimeUnit timeUnit;

        private IntervalUnit(final TimeUnit timeUnit) {
            this.timeUnit = timeUnit;
        }

        public long toMillis(final long amount) {
            return timeUnit.toMillis(amount);
        }
    }

    /** Limits of the values the user can enter, shared by the settings table's spinners. */
    public static final int  MIN_VALUE        = 1;
    public static final int  MAX_INTERVAL     = 999;
    public static final int  MAX_MAX_CAPTCHAS = 9999;
    /** Optional label, may be empty. */
    private String           name             = null;
    private boolean          enabled          = true;
    /** Length of the time window, together with {@link #unit}. */
    private int              interval         = 1;
    private IntervalUnit     unit             = IntervalUnit.HOURS;
    /** Max number of captchas within the time window. */
    private int              maxCaptchas      = 100;

    public CaptchaSolverLimitRule() {
        // __Storable__ constructor
    }

    public CaptchaSolverLimitRule(final String name, final boolean enabled, final int interval, final IntervalUnit unit, final int maxCaptchas) {
        this.name = name;
        this.enabled = enabled;
        this.interval = interval;
        this.unit = unit;
        this.maxCaptchas = maxCaptchas;
    }

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        this.name = name;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    public int getInterval() {
        return interval;
    }

    public void setInterval(final int interval) {
        this.interval = interval;
    }

    public IntervalUnit getUnit() {
        return unit;
    }

    public void setUnit(final IntervalUnit unit) {
        this.unit = unit;
    }

    public int getMaxCaptchas() {
        return maxCaptchas;
    }

    public void setMaxCaptchas(final int maxCaptchas) {
        this.maxCaptchas = maxCaptchas;
    }

    /** Length of the time window in milliseconds. */
    public long _getIntervalMillis() {
        return unit.toMillis(interval);
    }

    /** Creates the example rules offered when custom limits are enabled for the first time: per minute, per hour and per day (disabled). */
    public static ArrayList<CaptchaSolverLimitRule> createExampleRules() {
        final ArrayList<CaptchaSolverLimitRule> ret = new ArrayList<CaptchaSolverLimitRule>();
        ret.add(new CaptchaSolverLimitRule("Example: max captchas per minute", false, 1, IntervalUnit.MINUTES, 10));
        ret.add(new CaptchaSolverLimitRule("Example: max captchas per hour", false, 1, IntervalUnit.HOURS, 200));
        ret.add(new CaptchaSolverLimitRule("Example: max captchas per day", false, 1, IntervalUnit.DAYS, 1000));
        return ret;
    }
}
