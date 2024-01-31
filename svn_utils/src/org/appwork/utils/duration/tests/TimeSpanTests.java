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
package org.appwork.utils.duration.tests;

import org.appwork.exceptions.WTFException;
import org.appwork.testframework.AWTest;
import org.appwork.utils.duration.IllegalTargetUnitsException;
import org.appwork.utils.duration.InvalidTimeSpanException;
import org.appwork.utils.duration.TimeSpan;
import org.appwork.utils.duration.Unit;

/**
 * @author thomas
 * @date 04.11.2022
 *
 */
public class TimeSpanTests extends AWTest {
    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        try {
            TimeSpan.MAX.toInt(Unit.MILLISECONDS);
            throw new Exception("ArithmeticException due to overflow expected");
        } catch (ArithmeticException e) {
            // expected
        }
        System.out.println(TimeSpan.MAX.format());
        System.out.println(TimeSpan.MIN.format());
        try {
            TimeSpan.parse(Long.MAX_VALUE + "s");
            throw new Exception("ArithmeticException due to overflow expected");
        } catch (ArithmeticException e) {
            // expected
        }
        String max = TimeSpan.fromMillis(Long.MAX_VALUE).normalized().format();
        assertEquals("292471208Y8M3D23h12m55s807S", max);
        assertTrue(TimeSpan.fromMillis(60000).toMillis() == TimeSpan.parse("1minute").toMillis());
        assertTrue(TimeSpan.fromMillis(Unit.MONTHS.toMillis).toMillis() == TimeSpan.parse("1month").toMillis());
        assertTrue(TimeSpan.fromMillis(1000).toMillis() == TimeSpan.parse("1000S").toMillis());
        long epoch151122 = 1668526692913l;
        // Support ms and S
        assertThat(TimeSpan.parse("1m")).is(TimeSpan.parse("1minute"));
        assertThat(TimeSpan.parse("1m")).is(TimeSpan.parse("1minutes"));
        assertThat(TimeSpan.parse("1M")).is(TimeSpan.parse("1months"));
        assertThat(TimeSpan.parse("1234ms")).is(TimeSpan.parse("1234S"));
        assertThat(TimeSpan.parse("1234")).is(TimeSpan.parse("1234S"));
        assertThat(TimeSpan.parse("1234!365").toMillis()).is(TimeSpan.parse("1s234S!365").toMillis());
        new AssertAnException<InvalidTimeSpanException>() {
            @Override
            public void run() throws InvalidTimeSpanException {
                TimeSpan.parse("1D2123");
            }
        }.start();
        new AssertAnException<InvalidTimeSpanException>() {
            @Override
            public void run() throws InvalidTimeSpanException {
                TimeSpan.parse("1D2D");
            }
        }.start();
        new AssertAnException<InvalidTimeSpanException>() {
            @Override
            public void run() throws InvalidTimeSpanException {
                TimeSpan.parse("P1D3D");
            }
        }.start();
        assertThat(TimeSpan.parse("240h").shorten().format()).is("10D");
        assertEquals(TimeSpan.parse("365D").shorten().format(), "1Y");
        assertEquals(parse("1Y").toMillis(), parseJava18("P365D").toMillis());
        assertTrue(parse("1Y").normalized().isSameAs(parseJava18("P365D").normalized()));
        assertTrue(TimeSpan.parse("1h").isMoreThan(TimeSpan.parse("59m59s")));
        assertTrue(TimeSpan.parse("59m59s").isLessThan(TimeSpan.parse("1h")));
        assertTrue(TimeSpan.parse("59m60s").isSameAs(TimeSpan.parse("1h")));
        assertEquals(parse("1s123millisecond").toMillis(), parseJava18("PT1.123s").toMillis());
        parseJava18("-PT6H3M");
        new AssertAnException<IllegalTargetUnitsException>() {
            @Override
            public void run() throws IllegalTargetUnitsException {
                TimeSpan parsed = null;
                try {
                    parsed = parse(Integer.MAX_VALUE + "D");
                    TimeSpan cnv = parsed.convert(Unit.HOURS, Unit.MINUTES);
                } catch (Exception e1) {
                    if (e1 instanceof IllegalTargetUnitsException) {
                        throw (IllegalTargetUnitsException) e1;
                    }
                    throw new WTFException(e1);
                }
            }
        }.start();
        assertEquals(TimeSpan.parse("1D!365"), TimeSpan.parse("1D"));
        assertThat(parse(Integer.MAX_VALUE + "W8D").format()).is(Integer.MAX_VALUE + "W8D");
        testByMillis(2149633612l);
        assertThat(TimeSpan.parse("1h")).isHigherThan(TimeSpan.parse("59m"));
        assertThat(TimeSpan.parse("23h")).isLowerThan(TimeSpan.parse("1D"));
        assertThat(TimeSpan.parse("24h")).isSameAs(TimeSpan.parse("1D"));
        assertThat(TimeSpan.parse("61m")).isHigherThan(TimeSpan.parse("1h"));
        assertThat(TimeSpan.parse("2Y")).isHigherThan(TimeSpan.parse("1Y"));
        assertThat(TimeSpan.parse("2Y")).isSameAs(TimeSpan.parse("24M"));
        assertTrue(TimeSpan.parse("1h").toMillis() == TimeSpan.parse("1h").toMillis());
        new AssertAnException<InvalidTimeSpanException>() {
            @Override
            public void run() throws InvalidTimeSpanException {
                TimeSpan.parse((2147483647l + 1) + "m");
            }
        }.start();
        new AssertAnException<InvalidTimeSpanException>() {
            @Override
            public void run() throws InvalidTimeSpanException {
                TimeSpan.parse("1.234D");
            }
        }.start();
        // for (long i = 0; i < Long.MAX_VALUE; i += Math.random() * 100000000) {
        // testByMillis(i);
        // }
        assertThat(TimeSpan.fromMillis(300000)).is(TimeSpan.parse("5m"));
        assertEquals(TimeSpan.parse("365D!365").normalized(), TimeSpan.parse("1Y"));
        assertThat(parse("1Y")).isSameAs(parseJava18("P365D"));
        assertThat(parse("1Y!2022-01-01T00:00:00+0100")).isSameAs(parseJava18("P365D"));
        assertThat(parse("1Y!365.0")).isSameAs(parseJava18("P365D"));
        assertEqualsNot(parse("1Y"), parseJava18("P365D"));
        assertEquals(TimeSpan.parse("365D").normalized().format(), "1Y");
        assertEquals(TimeSpan.parse("12M47D49h").normalized().format(), "1Y1M2W4D15h");
        assertEquals(TimeSpan.parse("12M52W47D49h").normalized().format(), "2Y1M2W3D15h");
        assertThat(TimeSpan.parse("1Y").toLong(Unit.MONTHS)).isNumber(12);
        assertEquals(TimeSpan.parse("1M").normalized().format(), TimeSpan.parse("30D10h").normalized().format());
        assertEquals(TimeSpan.parse("100Y12M47d49h").normalized().format(), "101Y1M2W4D15h");
        assertEquals(TimeSpan.parse("12M").normalized(), TimeSpan.parse("1Y").normalized());
        assertEquals(TimeSpan.parse("12M").normalized(), TimeSpan.parse("1Y").normalized());
        assertEquals(TimeSpan.parse("7D").normalized(), TimeSpan.parse("1W").normalized());
        assertEquals(TimeSpan.parse((7 * 24) + "h").normalized(), TimeSpan.parse("1W").normalized());
        assertEqualsNot(TimeSpan.parse("1M").normalized(), TimeSpan.parse("30d").normalized());
        assertEqualsNot(TimeSpan.parse("1M").normalized(), TimeSpan.parse("31d").normalized());
        assertEqualsNot(TimeSpan.parse("1M").normalized(), TimeSpan.parse("28d").normalized());
        assertEquals(TimeSpan.parse("6D").normalized(), TimeSpan.parse("4D48h").normalized());
        assertEquals(TimeSpan.parse("2m").normalized(), TimeSpan.parse("120s").normalized());
        assertEquals(TimeSpan.parse("120s").normalized().format(), TimeSpan.parse("119s1000S").normalized().format());
        assertEquals("24h120s", TimeSpan.parse("24h120s").format());
        assertEquals("1D2m", TimeSpan.parse("24h120s").normalized().format());
        // TimeSpan dur2 = TimeSpan.parse("24h120.5s").normalize();
        // TimeSpan dur3 = TimeSpan.parse("24h120s").convert(Unit.MINUTES, Unit.HOURS);
        // Object span1Y = TimeSpan.parse("1Y").convert(Calendar.getInstance(),Unit.MINUTES, Unit.HOURS);
        assertThat(TimeSpan.parse("24h120s").format()).is("24h120s");
        assertThat(TimeSpan.parse("24 h 120 s").format()).is("24h120s");
        assertThat(Unit.SECONDS.getValue(TimeSpan.parse("24h120s"))).isNumber(120);
        assertThat(TimeSpan.parse("1D").toLong(Unit.HOURS)).isNumber(24);
        assertThat(TimeSpan.parse("24h").toLong(Unit.HOURS)).isNumber(24);
        assertThat(Unit.MINUTES.getValue(TimeSpan.parse("24h120s"))).isNumber(0);
        assertThat(TimeSpan.parse("1M").toLong(Unit.MONTHS)).isNumber(1);
        assertThat(TimeSpan.parse("1M").toLong(Unit.MONTHS)).isNumber(1);
        assertThat(TimeSpan.parse("1M").getMilliseconds()).isNumber(0);
        String format = TimeSpan.parse("1Y").convert(Unit.WEEKS, Unit.HOURS).format();
        assertThat(format).is("52W24h");
        format = TimeSpan.parse("1Y").convert(Unit.MONTHS).format();
        assertThat(format).is("12M");
        format = TimeSpan.parse("1Y").convert(Unit.DAYS, Unit.HOURS).format();
        try {
            format = TimeSpan.parse("1Y").convert(Unit.WEEKS).format();
            throw new Exception("Expected Exception");
        } catch (IllegalTargetUnitsException e) {
        }
        TimeSpan dur = TimeSpan.parse("365D");
        // String norm = dur.normalize().format();
        assertTrue(TimeSpan.parse("1H!NOW") instanceof TimeSpan);
        assertTrue(TimeSpan.parse("1H!365") instanceof TimeSpan);
        assertTrue(parse("-1Y").format().equals("-1Y"));
        assertThat(dur.toLong(Unit.DAYS)).isNumber(365);
        assertThat(Unit.DAYS.getValue(dur)).isNumber(365);
        assertThat(Unit.DAYS.getValue(dur.normalized())).isNumber(0);
        long hours = dur.toLong(Unit.HOURS);
        assertThat(hours).isNumber(365 * 24);
        assertThat(dur.convert(Unit.HOURS).format()).is((int) hours + "h");
        TimeSpan normed = dur.normalized();
        assertEquals("1Y", normed.format());
        normed = dur.normalized();
        assertEquals("1Y", normed.format());
        assertThat(TimeSpan.parse("52W 1D")).is(TimeSpan.parse("52W1D"));
        parseJava18("-PT6H3M");
        parse("-5h77m");
        try {
            parseJava18("PT-6H3M");
            throw new Exception();
        } catch (InvalidTimeSpanException e) {
        }
        assertEquals("-4h1m", parseJava18("-PT2H121M").normalized().format());
        assertEquals(parse("-6h3m"), parseJava18("-PT6H3M"));
        assertEquals("100s", parse("100s").format());
        assertEquals("1m40s", parse("100s").normalized().format());
        TimeSpan d = parse("200h");
        String formated = d.normalized().format();
        assertEquals("1W1D8h", formated);
        formated = parse("25h").normalized().format();
        assertEquals("1D1h", formated);
        assertEquals(parse(Integer.MAX_VALUE + "s").toMillis(), parse("3550W5D3h14m7s").toMillis());
        double years = parse("1000Y").toDouble(Unit.YEARS);
        assertThat(years).is(1000d);
        // parse("1000Y").toTotalMillis(Calendar.getInstance());
        // long millis = parse("1000Y").toTotalMillis(System.currentTimeMillis());
        assertEquals(parse("5m"), parseJava18("PT5M"));
        assertEquals(parse("1W").normalized(), parseJava18("P7D").normalized());
        assertEquals(parse("5m"), parseJava18("PT5M"));
        assertEquals(parse("2D3h4m"), parseJava18("P2DT3H4M"));
        assertThat(parse("-1Y")).isSameAs(parseJava18("-P365D"));
        assertThat(parse("  -1  Y  ")).isSameAs(parseJava18("-P365D"));
        assertEquals(parse("1D"), parseJava18("P1D"));
        assertEquals(parse("1h"), parseJava18("PT1H"));
        assertEquals(parse("1m"), parseJava18("PT1M"));
        assertEquals(parse("1s"), parseJava18("PT1S"));
        assertEquals(parse("1s123S"), parseJava18("PT1.123S"));
        assertEquals(parse("1 s 1 2 3S"), parseJava18("PT1.123S"));
        assertEquals(parse("15m"), parseJava18("PT15M"));
        assertEquals(parse("10h"), parseJava18("PT10H"));
        assertEquals(parse("2D"), parseJava18("P2D"));
        assertEquals(parse("-6h3m"), parseJava18("-PT6H3M"));
        assertEquals(parse("1m"), parseJava18("PT1M"));
        assertEquals(TimeSpan.parse("14M47D49h").normalized().format(), "1Y3M2W4D15h");
    }

    /**
     * @param i
     * @throws Exception
     * @throws ContextMissingException
     */
    private void testByMillis(long i) throws Exception {
        TimeSpan span = TimeSpan.fromMillis(i);
        TimeSpan normalized = span.normalized();
        String formated;
        assertThat(normalized.format()).equals(formated = span.format());
        if (i % 10000 == 0) {
            System.out.println(i + " - " + 100 * (i / ((double) Long.MAX_VALUE)) + " - " + formated);
        }
        TimeSpan reparsed = TimeSpan.parse(formated);
        assertThat(reparsed.toLong(Unit.MILLISECONDS)).isNumber(i);
    }

    /**
     * @param string
     * @return
     * @throws Exception
     * @throws InvalidTimeSpanException
     * @throws NumberFormatException
     */
    private TimeSpan parseJava18(String string) throws NumberFormatException, InvalidTimeSpanException, Exception {
        java.time.Duration ret = java.time.Duration.parse(string);
        long millisJava = ret.getSeconds() * 1000l + ret.getNano() / 1000000;
        TimeSpan dur = parse(string);
        assertEquals(parse(string).toMillis(), millisJava);
        return dur;
    }

    /**
     * @param string
     * @return
     * @throws Exception
     */
    private TimeSpan parse(String string) throws Exception {
        TimeSpan ret = TimeSpan.parse(string);
        TimeSpan test = TimeSpan.parse(ret.format());
        assertEquals(ret, test);
        return ret;
    }

    public static void main(String[] args) {
        run();
    }
}
