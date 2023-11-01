package org.appwork.moment;

import java.util.Calendar;
import java.util.Date;

import org.appwork.exceptions.WTFException;

public class Moment {
    private Calendar c;
    private long     timestamp;

    public Moment(long time) {
        this.c = Calendar.getInstance();
        this.timestamp = time;
        this.c.setTimeInMillis(time);
    }

    /**
     * @param cur
     */
    public Moment(Date cur) {
        this(cur.getTime());
    }

    /**
     *
     */
    public Moment() {
        this(System.currentTimeMillis());
    }

    public Moment map(int amount, UnitsTime interval) {
        this.timestamp = this.getMappedTimestamp(amount, interval);
        return this;
    }

    @Override
    public String toString() {
        return new Date(this.timestamp).toString();
    }

    public long getMappedTimestamp(int amount, UnitsTime interval) {
        this.c.setTimeInMillis(this.timestamp);
        switch (interval) {
        case SECOND:
            this.c.set(Calendar.MILLISECOND, 0);
            this.c.set(Calendar.SECOND, amount * (this.c.get(Calendar.SECOND) / amount));
            return this.c.getTimeInMillis();
        case MINUTE:
            this.c.set(Calendar.MILLISECOND, 0);
            this.c.set(Calendar.SECOND, 0);
            this.c.set(Calendar.MINUTE, amount * (this.c.get(Calendar.MINUTE) / amount));
            return this.c.getTimeInMillis();
        case HOUR:
            this.c.set(Calendar.MILLISECOND, 0);
            this.c.set(Calendar.SECOND, 0);
            this.c.set(Calendar.MINUTE, 0);
            this.c.set(Calendar.HOUR_OF_DAY, amount * (this.c.get(Calendar.HOUR_OF_DAY) / amount));
            return this.c.getTimeInMillis();
        case DAY:
            this.c.set(Calendar.MILLISECOND, 0);
            this.c.set(Calendar.SECOND, 0);
            this.c.set(Calendar.MINUTE, 0);
            this.c.set(Calendar.HOUR_OF_DAY, 0);
            this.c.set(Calendar.DAY_OF_MONTH, amount * (this.c.get(Calendar.DAY_OF_MONTH) / amount));
            return this.c.getTimeInMillis();
        case MONTH:
            this.c.set(Calendar.MILLISECOND, 0);
            this.c.set(Calendar.SECOND, 0);
            this.c.set(Calendar.MINUTE, 0);
            this.c.set(Calendar.HOUR_OF_DAY, 0);
            this.c.set(Calendar.DAY_OF_MONTH, 1);
            // Date d = new Date(this.c.getTimeInMillis());
            this.c.set(Calendar.MONTH, amount * (this.c.get(Calendar.MONTH) / amount));
            // d = new Date(this.c.getTimeInMillis());
            return this.c.getTimeInMillis();
        case YEAR:
            this.c.set(Calendar.MILLISECOND, 0);
            this.c.set(Calendar.SECOND, 0);
            this.c.set(Calendar.MINUTE, 0);
            this.c.set(Calendar.HOUR_OF_DAY, 0);
            this.c.set(Calendar.DAY_OF_MONTH, 1);
            this.c.set(Calendar.MONTH, 0);
            this.c.set(Calendar.YEAR, amount * (this.c.get(Calendar.YEAR) / amount));
            return this.c.getTimeInMillis();
        default:
            throw new WTFException("Not Supported: " + interval);
        }
    }

    public Moment add(int i, UnitsTime month) {
        this.c.setTimeInMillis(this.timestamp);
        this.c.add(month.calendarID, i);
        this.timestamp = this.c.getTimeInMillis();
        return this;
    }

    public long getTimeInMS() {
        return this.timestamp;
    }

    public Date toDate() {
        return new Date(this.timestamp);
    }

    public void setTimeInMS(long time) {
        this.timestamp = time;
    }

    /**
     * @return
     */
    public int getHourOfDay() {
        c.setTimeInMillis(timestamp);
        return c.get(Calendar.HOUR_OF_DAY);
    }

    /**
     * @return
     */
    public int getDayOfWeek() {
        c.setTimeInMillis(timestamp);
        return c.get(Calendar.DAY_OF_WEEK);
    }
}
