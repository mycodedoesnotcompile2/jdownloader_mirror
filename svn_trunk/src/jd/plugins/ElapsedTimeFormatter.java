package jd.plugins;

import java.util.Date;

public class ElapsedTimeFormatter {
    public enum TimeUnit {
        SECOND(1000L),
        MINUTE(60L * 1000L),
        HOUR(60L * 60L * 1000L),
        DAY(24L * 60L * 60L * 1000L),
        WEEK(7L * 24L * 60L * 60L * 1000L),
        MONTH(30L * 24L * 60L * 60L * 1000L),
        YEAR(365L * 24L * 60L * 60L * 1000L);

        private final long milliseconds;

        private TimeUnit(long milliseconds) {
            this.milliseconds = milliseconds;
        }

        public long getMilliseconds() {
            return milliseconds;
        }
    }

    private TimeUnit maxUnit            = null;
    private boolean  useNaturalLanguage = false;

    public ElapsedTimeFormatter() {
    }

    public ElapsedTimeFormatter setMaxUnit(TimeUnit maxUnit) {
        this.maxUnit = maxUnit;
        return this;
    }

    public ElapsedTimeFormatter setUseNaturalLanguage(boolean useNaturalLanguage) {
        this.useNaturalLanguage = useNaturalLanguage;
        return this;
    }

    public String formatElapsedTime(long elapsedMillis) {
        final boolean future = elapsedMillis < 0;
        final long absMillis = Math.abs(elapsedMillis);
        final long seconds = absMillis / 1000;
        final long minutes = seconds / 60;
        final long hours = minutes / 60;
        final long days = hours / 24;
        final long weeks = days / 7;
        final long months = days / 30;
        final long years = days / 365;
        final String result;
        if (maxUnit != null) {
            result = formatWithMaxUnit(absMillis, future);
        } else if (seconds < 60) {
            if (useNaturalLanguage && !future) {
                result = getTranslation("just_now");
            } else {
                result = formatTimeUnit(seconds, "second", future);
            }
        } else if (minutes < 60) {
            result = formatTimeUnit(minutes, "minute", future);
        } else if (hours < 24) {
            result = formatTimeUnit(hours, "hour", future);
        } else if (days < 7) {
            result = formatTimeUnit(days, "day", future);
        } else if (weeks < 4) {
            result = formatTimeUnit(weeks, "week", future);
        } else if (months < 12) {
            result = formatTimeUnit(months, "month", future);
        } else {
            result = formatTimeUnit(years, "year", future);
        }
        return result;
    }

    public String formatTimestamp(long timestamp) {
        final long now = System.currentTimeMillis();
        final long elapsed = now - timestamp;
        return formatElapsedTime(elapsed);
    }

    public String formatDate(Date date) {
        if (date == null) {
            throw new IllegalArgumentException("Date cannot be null");
        }
        return formatTimestamp(date.getTime());
    }

    private String formatWithMaxUnit(long absMillis, boolean future) {
        long value;
        String unit;
        switch (maxUnit) {
        case SECOND:
            value = absMillis / 1000;
            unit = "second";
            break;
        case MINUTE:
            value = absMillis / (60L * 1000L);
            unit = "minute";
            break;
        case HOUR:
            value = absMillis / (60L * 60L * 1000L);
            unit = "hour";
            break;
        case DAY:
            value = absMillis / (24L * 60L * 60L * 1000L);
            unit = "day";
            break;
        case WEEK:
            value = absMillis / (7L * 24L * 60L * 60L * 1000L);
            unit = "week";
            break;
        case MONTH:
            value = absMillis / (30L * 24L * 60L * 60L * 1000L);
            unit = "month";
            break;
        case YEAR:
            value = absMillis / (365L * 24L * 60L * 60L * 1000L);
            unit = "year";
            break;
        default:
            value = absMillis / 1000;
            unit = "second";
            break;
        }
        return formatTimeUnit(value, unit, future);
    }

    private String formatTimeUnit(long value, String unit, boolean future) {
        if (useNaturalLanguage && value == 1) {
            final String naturalKey = getNaturalLanguageKey(unit, future);
            if (naturalKey != null) {
                return getTranslation(naturalKey);
            }
        }
        final String unitKey = value == 1 ? unit : unit + "s";
        final String pattern = future ? "time_future" : "time_past";
        return getTranslation(pattern, String.valueOf(value), unitKey);
    }

    private String getNaturalLanguageKey(String unit, boolean future) {
        if (unit.equals("day")) {
            return future ? "tomorrow" : "yesterday";
        } else if (unit.equals("week")) {
            return future ? "next_week" : "last_week";
        } else if (unit.equals("month")) {
            return future ? "next_month" : "last_month";
        } else if (unit.equals("year")) {
            return future ? "next_year" : "last_year";
        }
        return null;
    }

    /**
     * Translation stub - to be replaced with actual translation logic later.
     *
     * @param key
     *            Translation key
     * @return Translated string
     */
    private String getTranslation(String key) {
        if (key.equals("just_now")) {
            return "just now";
        } else if (key.equals("yesterday")) {
            return "yesterday";
        } else if (key.equals("tomorrow")) {
            return "tomorrow";
        } else if (key.equals("last_week")) {
            return "last week";
        } else if (key.equals("next_week")) {
            return "next week";
        } else if (key.equals("last_month")) {
            return "last month";
        } else if (key.equals("next_month")) {
            return "next month";
        } else if (key.equals("last_year")) {
            return "last year";
        } else if (key.equals("next_year")) {
            return "next year";
        }
        return key;
    }

    /**
     * Translation stub with parameters - to be replaced with actual translation logic later. Pattern placeholders: {0} = value, {1} = unit
     *
     * @param key
     *            Translation key
     * @param value
     *            Numeric value
     * @param unit
     *            Time unit
     * @return Translated string
     */
    private String getTranslation(String key, String value, String unit) {
        if (key.equals("time_past")) {
            // Pattern: "{0} {1} ago"
            // Example: "5 seconds ago"
            return value + " " + unit + " ago";
        } else if (key.equals("time_future")) {
            // Pattern: "in {0} {1}"
            // Example: "in 5 seconds"
            return "in " + value + " " + unit;
        }
        return key;
    }
}
