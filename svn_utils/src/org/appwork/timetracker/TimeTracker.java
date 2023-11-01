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
package org.appwork.timetracker;

import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.concurrent.CopyOnWriteArrayList;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.LogInterface;

/**
 * @author thomas
 * @date 12.11.2015
 *
 */
public class TimeTracker {
    private final String                            id;
    private final CopyOnWriteArrayList<TrackerRule> rules   = new CopyOnWriteArrayList<TrackerRule>();
    private final LinkedList<TrackedEntry>          entries = new LinkedList<TrackedEntry>();
    private final LogInterface                      logger;

    public String getId() {
        return id;
    }

    /**
     * @param typeID
     */
    public TimeTracker(String typeID) {
        this.id = typeID;
        logger = LogV3.I().getLogger("TimeTracker " + id);
    }

    public void addRule(TrackerRule rule) {
        if (rule != null) {
            rules.addIfAbsent(rule);
        }
    }

    public void wait(final TrackerJob job) throws InterruptedException {
        if (rules.size() == 0 || job == null) {
            return;
        } else {
            final TrackedEntry entry;
            synchronized (this) {
                final long waitFor = getWaitFor(job.getWeight());
                entry = createEntry(waitFor, job.getWeight());
                entries.add(entry);
            }
            if (entry.getWaitFor() > 0) {
                logger.info("Wait " + TimeFormatter.formatMilliSeconds(entry.getWaitFor(), 0));
                job.waitForNextSlot(entry.getWaitFor());
            }
            job.run();
            logger.info("RUN");
        }
    }

    /**
     * @param weight
     * @return
     */
    protected long getWaitFor(int weight) {
        int sum = 0;
        final LinkedList<TrackerRule> lRules = new LinkedList<TrackerRule>(rules);
        final ListIterator<TrackedEntry> itEntres = entries.listIterator(entries.size());
        long time = System.currentTimeMillis();
        logger.info("Find wait For");
        long interval = 0;
        while (itEntres.hasPrevious()) {
            final TrackedEntry entry = itEntres.previous();
            logger.info("Check Entry " + new Date(entry.getTime()));
            // time = Math.max(entry.getTime(), time);
            sum += entry.getWeight();
            final Iterator<TrackerRule> it = lRules.iterator();
            while (it.hasNext()) {
                final TrackerRule r = it.next();
                final long diff = time - entry.getTime();
                if (diff > r.getInterval()) {
                    it.remove();
                    if (lRules.size() == 0) {
                        logger.info("" + sum + "/" + r.getAmount() + " in the last " + r.getInterval());
                        // cleanup
                        while (itEntres.hasPrevious()) {
                            itEntres.previous();
                            itEntres.remove();
                        }
                        return interval;
                    }
                    continue;
                } else if (sum >= r.getAmount()) {
                    long ninterval = Math.max(interval, r.getInterval() - diff);
                    if (ninterval > interval) {
                        logger.info("" + sum + "/" + r.getAmount() + " in the last " + r.getInterval() + " -> Wait " + TimeFormatter.formatMilliSeconds(interval, 0));
                        interval = ninterval;
                    }
                }
            }
        }
        logger.info("" + sum + " --> " + TimeFormatter.formatMilliSeconds(interval, 0));
        return interval;
    }

    /**
     * @param waitFor
     * @param weight
     * @return
     */
    protected TrackedEntry createEntry(long waitFor, int weight) {
        return new TrackedEntry(weight, waitFor);
    }
}
