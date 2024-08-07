//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.nutils;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;

import org.appwork.utils.formatter.StringFormatter;

import jd.nutils.encoding.Encoding;
import jd.parser.Regex;

public class Formatter {
    /**
     * The format describing an http date.
     */
    private static SimpleDateFormat DATE_FORMAT = null;

    /**
     * Formatiert Sekunden in das zeitformat stunden:minuten:sekunden returns "~" vor values <0
     *
     * @param eta
     * @return formatierte Zeit
     */
    public static String formatSeconds(long eta) {
        return formatSeconds(eta, true);
    }

    public static String formatSeconds(long eta, boolean showsec) {
        if (eta < 0) {
            return "~";
        }
        long days = eta / (24 * 60 * 60);
        eta -= days * 24 * 60 * 60;
        long hours = eta / (60 * 60);
        eta -= hours * 60 * 60;
        long minutes = eta / 60;
        long seconds = eta - minutes * 60;
        StringBuilder ret = new StringBuilder();
        if (days != 0) {
            ret.append(days).append('d');
        }
        if (hours != 0 || ret.length() != 0) {
            if (ret.length() != 0) {
                ret.append(':');
            }
            ret.append(hours).append('h');
        }
        if (minutes != 0 || ret.length() != 0) {
            final int numFill = ret.length() != 0 ? 2 : 1;
            if (ret.length() != 0) {
                ret.append(':');
            }
            ret.append(Formatter.fillInteger(minutes, numFill, "0")).append('m');
        }
        if (showsec) {
            final int numFill = ret.length() != 0 ? 2 : 1;
            if (ret.length() != 0) {
                ret.append(':');
            }
            ret.append(Formatter.fillInteger(seconds, numFill, "0")).append('s');
        }
        return ret.toString();
    }

    /**
     * FOIrmatiert im format hours:minutes:seconds.ms
     *
     * @param ms
     */
    public static String formatMilliseconds(long ms) {
        return formatSeconds(ms / 1000) + "." + Formatter.fillInteger(ms % 1000, 3, "0");
    }

    @Deprecated
    public static String formatFilesize(double value, int size) {
        if (value > 1024 && size < 5) {
            return formatFilesize(value / 1024.0, ++size);
        } else {
            final DecimalFormat c = new DecimalFormat();
            switch (size) {
            case 0:
                return c.format(value) + " B";
            case 1:
                return c.format(value) + " KiB";
            case 2:
                return c.format(value) + " MiB";
            case 3:
                return c.format(value) + " GiB";
            case 4:
                return c.format(value) + " TiB";
            }
        }
        return null;
    }

    @Deprecated
    public static String formatReadable(long fileSize) {
        if (fileSize < 0) {
            fileSize = 0;
        }
        final DecimalFormat c = new DecimalFormat();
        if (fileSize >= (1024 * 1024 * 1024 * 1024l)) {
            return c.format(fileSize / (1024 * 1024 * 1024 * 1024.0)) + " TiB";
        } else if (fileSize >= (1024 * 1024 * 1024l)) {
            return c.format(fileSize / (1024 * 1024 * 1024.0)) + " GiB";
        } else if (fileSize >= (1024 * 1024l)) {
            return c.format(fileSize / (1024 * 1024.0)) + " MiB";
        } else if (fileSize >= 1024l) {
            return c.format(fileSize / 1024.0) + " KiB";
        } else {
            return fileSize + " B";
        }
    }

    /**
     * @deprecated Use {@link StringFormatter#fillString(String,String,String,int)} instead
     */
    public static String fillString(String binaryString, String pre, String post, int length) {
        return StringFormatter.fillString(binaryString, pre, post, length);
    }

    /**
     * Hängt an i solange fill vorne an bis die zechenlänge von i gleich num ist
     *
     * @param i
     * @param num
     * @param fill
     * @return aufgefüllte Zeichenkette
     */
    public static String fillInteger(long i, int num, String fill) {
        String ret = "" + i;
        while (ret.length() < num) {
            ret = fill + ret;
        }
        return ret;
    }

    /**
     * GIbt den Integer der sich in src befindet zurück. alle nicht integerzeichen werden ausgefiltert
     *
     * @param src
     * @return Integer in src
     */
    public static int filterInt(String src) {
        try {
            return Integer.parseInt(Encoding.filterString(src, "1234567890"));
        } catch (NumberFormatException e) {
            return 0;
        }
    }

    public static long filterLong(String src) {
        try {
            return Long.parseLong(Encoding.filterString(src, "1234567890"));
        } catch (NumberFormatException e) {
            return 0;
        }
    }

    /**
     * Extracts the Revision from rev. $Revision: 6506 $
     *
     * @param rev
     * @return
     */
    public static long getRevision(String rev) {
        if (rev == null) {
            return -1;
        }
        try {
            int ret = 0;
            if (!rev.startsWith("$")) {
                String base = new Regex(rev, "Base:\\s*(\\d+)").getMatch(0);
                if (base != null) {
                    ret += Long.parseLong(base);
                }
            }
            String number = new Regex(rev, "Revision:.*?(\\d+)").getMatch(0);
            if (number != null) {
                ret += Long.parseLong(number);
                return ret;
            }
        } catch (Throwable e) {
            e.printStackTrace();
        }
        return -1;
    }
}
