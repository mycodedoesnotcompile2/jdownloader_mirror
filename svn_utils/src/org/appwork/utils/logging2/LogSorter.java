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
package org.appwork.utils.logging2;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.utils.Application;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;

/**
 * @author Thomas
 * @date 03.05.2018
 *
 */
public class LogSorter {
    public static class LogLine implements Comparable<LogLine> {
        String         text;
        private long   ts;
        private String clazz;
        private String thread;

        /**
         * @param sub
         * @return
         */
        public static LogLine create(String sub) {
            LogLine ret = new LogLine();
            ret.ts = Long.parseLong(new Regex(sub, "\\-\\-ID\\:\\d+TS\\:(\\d+)\\-").getMatch(0));
            ret.thread = new Regex(sub, "\\-\\-ID\\:(\\d+)").getMatch(0);
            String[] data = new Regex(sub, ".*?\\[([^\\]]+)\\]\\s*\\-\\>\\s*(.*)").getRow(0);
            if (data == null) {
                ret.clazz = null;
                ret.text = sub;
            } else {
                ret.clazz = data[0];
                ret.text = data[1].trim();
            }
            return ret;
        }

        /*
         * (non-Javadoc)
         *
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            // TODO Auto-generated method stub
            return ts + " - " + new Date(ts) + " " + text + "  [" + clazz + "]";
        }

        /*
         * (non-Javadoc)
         *
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(LogLine o) {
            // TODO Auto-generated method stub
            return CompareUtils.compare(ts, o.ts);
        }
    }

    public static void main(String[] args) throws DialogClosedException, DialogCanceledException {
        Application.setApplication(".appwork");
        String log = Dialog.getInstance().showInputDialog(Dialog.STYLE_LARGE, "Log", null);
        Pattern pattern = Pattern.compile("\\-\\-ID:\\d+TS\\:\\d+\\-");
        Pattern thread = Pattern.compile("^------------------------Thread\\: (\\d)\\:([^\r\n]+?)-----------------------[\r\n]+");
        ArrayList<LogLine> lines = new ArrayList<LogLine>();
        String threadID = null;
        HashMap<String, String> threads = new HashMap<String, String>();
        while (log.length() > 0) {
            if (log.trim().startsWith("------------------------Thread: ")) {
                Matcher matcher = thread.matcher(log);
                if (matcher.find()) {
                    String all = matcher.group(0);
                    String id = matcher.group(1);
                    String name = matcher.group(2);
                    threads.put(id.trim(), name.trim());
                    threadID = id.trim();
                    log = log.substring(all.length());
                }
            }
            Matcher matcher = pattern.matcher(log);
            int index = 0;
            if (matcher.find() && matcher.find()) {
                index = matcher.start();
            } else {
                index = log.length();
            }
            String sub = log.substring(0, index);
            lines.add(LogLine.create(sub));
            log = log.substring(index);
        }
        Collections.sort(lines);
        LogLine last = null;
        for (LogLine l : lines) {
            if (l.ts == 1535116136132l) {
                System.out.println();
            }
            if (last == null || !StringUtils.equals(last.clazz, l.clazz)) {
                System.out.println("Origin: " + l.clazz);
            }
            System.out.println(l.thread + " " + l.ts + "/" + new Date(l.ts) + "  " + l.text);
            ;
            last = l;
        }
    }
}
