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
package org.appwork.storage.flexijson.mapper.tests;

import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;

import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.mapper.typemapper.DateMapper;
import org.appwork.testframework.AWTest;
import org.appwork.utils.DebugMode;

/**
 * @author thomas
 * @date 21.11.2022
 *
 */
public class DateMapperTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        // Get the current timezone offset in ISO 8601 format
        TimeZone timeZone = TimeZone.getDefault();
        Calendar calendar = Calendar.getInstance(timeZone);
        int offsetInMillis = timeZone.getOffset(calendar.getTimeInMillis());
        int offsetHours = offsetInMillis / (1000 * 60 * 60);
        int offsetMinutes = Math.abs((offsetInMillis / (1000 * 60)) % 60);
        final String currentTimezoneOffset = String.format("%+03d:%02d", offsetHours, offsetMinutes);
        TimeZone restore = TimeZone.getDefault();
        TimeZone p2 = TimeZone.getTimeZone("GMT+02:00");
        try {
            TimeZone.setDefault(p2);
            Map<String, String> testDates = new LinkedHashMap<String, String>() {
                {
                    put("03:00 PM", "1970-01-01T15:00+02:00");
                    put("01.04.2024+02:00", "2024-04-01" + currentTimezoneOffset);
                    put("2024-04-01T15:01:02Z", "2024-04-01T17:01:02+02:00");
                    put("2024-04-01T15:01:02.123Z", "2024-04-01T17:01:02.123+02:00");
                    put("04/01/2024", "2024-04-01" + currentTimezoneOffset);
                    put("2024-04-01T15:00:00+02:00", "2024-04-01T15:00+02:00");
                    put("2024-04-01T15:00:00.000+02:00", "2024-04-01T15:00+02:00");
                    put("2024-04-01 15:00:00", "2024-04-01T15:00" + currentTimezoneOffset);
                    put("2024/04/01 15:00:00", "2024-04-01T15:00" + currentTimezoneOffset);
                    put("2024-04-01", "2024-04-01" + currentTimezoneOffset);
                    put("2024/04/01", "2024-04-01" + currentTimezoneOffset);
                    put("01.04.2024", "2024-04-01" + currentTimezoneOffset);
                    put("01.04.2024 00:00", "2024-04-01" + currentTimezoneOffset);
                    put("01-04-2024", "2024-04-01" + currentTimezoneOffset);
                    put("2024-04-01CEST", "2024-04-01" + currentTimezoneOffset);
                    put("2024/04/01+02:00", "2024-04-01" + currentTimezoneOffset);
                    put("2024-04-01 15:00:00+0200", "2024-04-01T15:00+02:00");
                    put("2024-04-01 15:00:00+02:00", "2024-04-01T15:00+02:00");
                    put("2024/04/01 15:00:00+0200", "2024-04-01T15:00+02:00");
                    put("2024/04/01 15:00:00+02:00", "2024-04-01T15:00+02:00");
                    put("2024-04-01 15:00:00 +0200", "2024-04-01T15:00+02:00");
                    put("2024-04-01 15:00:00 +02:00", "2024-04-01T15:00+02:00");
                    put("2024/04/01 15:00:00 +0200", "2024-04-01T15:00+02:00");
                    put("2024/04/01 15:00:00 +02:00", "2024-04-01T15:00+02:00");
                    put("15:00:00", "1970-01-01T15:00+02:00");
                    put("15:00", "1970-01-01T15:00+02:00");
                    put("15:00 +03:00", "1970-01-01T14:00+02:00");
                    put("03:00:00 PM", "1970-01-01T15:00+02:00");
                }
            };
            String date2 = DateMapper.formatJsonDefault(1717592073325l);
            // incl. ms
            assertEquals("2024-06-05T14:54:33.325+02:00", date2);
            for (Entry<String, String> str : testDates.entrySet()) {
                Date date = DateMapper.parse(str.getKey());
                String normalized = DateMapper.formatJsonDefault(date);
                System.out.println("Input  " + str.getKey() + "\r\n" + "Expect " + str.getValue() + "\r\nresult " + normalized);
                DebugMode.breakIf(!str.getValue().equals(normalized));
                assertEquals(str.getValue(), normalized);
            }
        } finally {
            TimeZone.setDefault(restore);
        }
        String source = "2022-01-01T00:00:00Z";
        Instant instant = Instant.parse(source);
        java.util.Date fromInstant = java.util.Date.from(instant);
        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ", Locale.US);
        Date fsdfds = format.parse("2022-01-01T00:00:00CET");
        String formated = format.format(fsdfds);
        {
            Date date = new DateMapper().parseString("2022-01-01T00:00:00+0100");
            assertNotNull(date);
        }
        {
            TimeZone.setDefault(TimeZone.getTimeZone("CET"));
            try {
                // without timezone - we expect the local time - so this test works only - this is ISO 8601
                Date date = FlexiUtils.jsonToObject("\"2022-01-01\"", new SimpleTypeRef<Date>(Date.class));
                assertEquals(date.toInstant().toString(), "2021-12-31T23:00:00Z");
                String json = FlexiUtils.serializeMinimized(date);
                assertEquals(json, "\"2022-01-01+01:00\"");
            } finally {
                TimeZone.setDefault(restore);
            }
        }
        {
            TimeZone.setDefault(TimeZone.getTimeZone("CET"));
            try {
                // without timezone - we expect the local time - so this test works only - this is ISO 8601
                Date date = FlexiUtils.jsonToObject("\"2022-01-01T00:00:00\"", new SimpleTypeRef<Date>(Date.class));
                assertEquals(date.toInstant().toString(), "2021-12-31T23:00:00Z");
                String json = FlexiUtils.serializeMinimized(date);
                assertEquals(json, "\"2022-01-01+01:00\"");
            } finally {
                TimeZone.setDefault(restore);
            }
        }
        TimeZone.setDefault(TimeZone.getTimeZone("Europe/Berlin"));
        try {
            {
                Date date = FlexiUtils.jsonToObject("\"2022-01-01T00:00:00 -08:00\"", new SimpleTypeRef<Date>(Date.class));
                assertEquals(date.toInstant().toString(), "2022-01-01T08:00:00Z");
                String json = FlexiUtils.serializeMinimized(date);
                assertEquals(json, "\"2022-01-01T09:00+01:00\"");
            }
            {
                Date date = FlexiUtils.jsonToObject("\"2022-01-01 00:00:00 -08:00\"", new SimpleTypeRef<Date>(Date.class));
                assertEquals(date.toInstant().toString(), "2022-01-01T08:00:00Z");
                String json = FlexiUtils.serializeMinimized(date);
                assertEquals(json, "\"2022-01-01T09:00+01:00\"");
            }
            {
                Date date = FlexiUtils.jsonToObject("\"2022-01-01T00:00:00Z\"", new SimpleTypeRef<Date>(Date.class));
                assertEquals(date.toInstant().toString(), "2022-01-01T00:00:00Z");
                String json = FlexiUtils.serializeMinimized(date);
                assertEquals(json, "\"2022-01-01T01:00+01:00\"");
            }
            {
                Date date = FlexiUtils.jsonToObject("\"2022-01-01T00:00:00 CET\"", new SimpleTypeRef<Date>(Date.class));
                assertEquals(date.toInstant().toString(), "2021-12-31T23:00:00Z");
                String json = FlexiUtils.serializeMinimized(date);
                assertEquals(json, "\"2022-01-01+01:00\"");
            }
            {
                Date date = FlexiUtils.jsonToObject("\"2022-01-01T00:00:00+0100\"", new SimpleTypeRef<Date>(Date.class));
                assertEquals(date.toInstant().toString(), "2021-12-31T23:00:00Z");
                String json = FlexiUtils.serializeMinimized(date);
                assertEquals(json, "\"2022-01-01+01:00\"");
            }
        } finally {
            TimeZone.setDefault(restore);
        }
    }
}
