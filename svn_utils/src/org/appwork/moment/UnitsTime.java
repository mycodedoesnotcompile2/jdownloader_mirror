package org.appwork.moment;

import java.util.Calendar;

public enum UnitsTime {
    /**
     * Always sort from short to long!!
     */
    SECOND(Calendar.SECOND, 1000l, 1000l),
    MINUTE(Calendar.MINUTE, 60 * SECOND.min, 60 * SECOND.max),
    HOUR(Calendar.HOUR, 60 * MINUTE.min, 60 * MINUTE.max),
    DAY(Calendar.DAY_OF_MONTH, 24 * HOUR.min, 24 * HOUR.max),
    MONTH(Calendar.MONTH, 28 * DAY.min, 31 * DAY.max),
    YEAR(Calendar.YEAR, 31 * DAY.min * 6 + 30 * DAY.min + 28 * DAY.min, 31 * DAY.max * 6 + 30 * DAY.max + 29 * DAY.max);
    protected final int calendarID;
    public final long   min;
    public final long   max;

    private UnitsTime(int calendarRepresentation, long min, long max) {
        this.calendarID = calendarRepresentation;
        this.min = min;
        this.max = max;
    }
}
