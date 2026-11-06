package org.jdownloader.captcha.blacklist;

import java.lang.ref.WeakReference;

import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;
import jd.controlling.linkcrawler.LinkCrawler;
import jd.plugins.PluginForDecrypt;

import org.jdownloader.captcha.v2.Challenge;

public class BlockCurrentCrawlerCaptchasEntry implements BlacklistEntry {
    private final WeakReference<LinkCrawler> crawler;
    private final String                     reason;

    public BlockCurrentCrawlerCaptchasEntry(SkipException skipException, LinkCrawler crawler) {
        this.crawler = new WeakReference<LinkCrawler>(crawler);
        this.reason = skipException == null ? null : skipException.getReason();
    }

    @Override
    public boolean canCleanUp() {
        final LinkCrawler lcrawler = getCrawler();
        return lcrawler == null || !lcrawler.isRunning();
    }

    public LinkCrawler getCrawler() {
        return crawler.get();
    }

    @Override
    public String toString() {
        final LinkCrawler lcrawler = getCrawler();
        if (lcrawler != null) {
            return "BlockCrawlerCaptchasEntry:" + lcrawler.getCreated();
        } else {
            return "BlockCrawlerCaptchasEntry";
        }
    }

    @Override
    public boolean matches(Challenge c) {
        final LinkCrawler lcrawler = getCrawler();
        if (lcrawler != null && lcrawler.isRunning()) {
            if (c.getCaptchaRequestType() == Challenge.CaptchaRequestType.DECRYPTER) {
                return ((PluginForDecrypt) c.getPlugin()).getCrawler() == lcrawler;
            }
        }
        return false;
    }

    @Override
    public SkipRequest getSkipRequest() {
        return SkipRequest.STOP_CURRENT_ACTION;
    }

    @Override
    public String getReason() {
        return reason;
    }
}
