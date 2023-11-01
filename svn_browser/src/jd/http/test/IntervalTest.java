package jd.http.test;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import jd.http.Browser;

import org.appwork.utils.Application;
import org.appwork.utils.logging2.extmanager.LoggerFactory;

public class IntervalTest {
    static int interval = 50;
    static int threads  = 6;
    static int loop     = 100;

    public static void main(String[] args) throws Exception {
        Application.setApplication(".awutests");
        Browser br = new Browser();
        br.setLogger(LoggerFactory.getDefaultLogger());
        // br.setVerbose(true);
        // br.setDebug(true);
        Browser.setRequestIntervalLimitGlobal("jdownloader.org", true, 1000);
        Browser.setBurstRequestIntervalLimitGlobal("ipcheck0.jdownloader.org", true, 1000, 10, 5000);
        br.getPage("http://jdownloader.org");
        for (int i = 0; i < 1000; i++) {
            System.out.println(i);
            br.getPage("http://ipcheck0.jdownloader.org");
            if (i > 60) {
                Browser.setBurstRequestIntervalLimitGlobal("ipcheck0.jdownloader.org", true, 1000, 100, 5000);
            } else if (i > 50) {
                Browser.setBurstRequestIntervalLimitGlobal("ipcheck0.jdownloader.org", true, 1000, 0, 0);
            } else if (i > 20) {
                Browser.setRequestIntervalLimitGlobal("ipcheck0.jdownloader.org", true, 0);
            }
        }
        Browser.setRequestIntervalLimitGlobal("ipcheck1.jdownloader.org", true, IntervalTest.interval);
        Browser.setRequestIntervalLimitGlobal("ipcheck2.jdownloader.org", true, IntervalTest.interval);
        // Browser.setRequestIntervalLimitGlobal("ipcheck0.jdownloader.org", true, -1);
        br.getPage("http://ipcheck0.jdownloader.org");
        final AtomicLong wait = new AtomicLong(0);
        final List<Thread> threadList = new ArrayList<Thread>();
        for (int thread = 0; thread < IntervalTest.threads; thread++) {
            final Browser brc = br.cloneBrowser();
            final Thread t = new Thread("Thread:" + thread) {
                @Override
                public void run() {
                    final long start = System.currentTimeMillis();
                    try {
                        int loop = IntervalTest.loop;
                        long last = -1;
                        int endPoint = 0;
                        while (loop >= 0) {
                            switch (endPoint) {
                            case 0:
                                brc.getPage("http://ipcheck0.jdownloader.org");
                                break;
                            case 1:
                                brc.getPage("http://ipcheck1.jdownloader.org");
                                break;
                            case 2:
                                brc.getPage("http://ipcheck2.jdownloader.org");
                                break;
                            }
                            endPoint = (endPoint + 1) % 3;
                            loop--;
                            final long now = System.currentTimeMillis();
                            final long dif = now - last;
                            // System.out.println(Thread.currentThread().getName() + "|dif(" + loop + "):" + dif);
                            if (false && last > 0 && dif < IntervalTest.interval) {
                                throw new Exception("dif(" + loop + "):" + dif);
                            } else {
                                last = now;
                            }
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                        System.exit(1);
                    } finally {
                        final long took = System.currentTimeMillis() - start;
                        System.out.println(Thread.currentThread().getName() + " took:" + took);
                        wait.addAndGet(took);
                    }
                };
            };
            threadList.add(t);
            t.start();
        }
        for (Thread t : threadList) {
            t.join();
        }
        System.out.println("Took:" + wait.get() + " for " + IntervalTest.loop * IntervalTest.threads + " = " + wait.get() / (IntervalTest.loop * IntervalTest.threads));
    }
}
