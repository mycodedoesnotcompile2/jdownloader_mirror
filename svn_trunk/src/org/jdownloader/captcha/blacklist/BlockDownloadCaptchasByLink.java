package org.jdownloader.captcha.blacklist;

import java.lang.ref.WeakReference;

import org.jdownloader.captcha.v2.Challenge;

import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

public class BlockDownloadCaptchasByLink implements SessionBlackListEntry<Object> {
    private final WeakReference<DownloadLink> downloadLink;
    private final SkipRequest                 skipRequest;
    private final String                      reason;

    public BlockDownloadCaptchasByLink(SkipException skipException, DownloadLink downloadLink) {
        this.downloadLink = new WeakReference<DownloadLink>(downloadLink);
        this.skipRequest = skipException.getSkipRequest();
        this.reason = skipException.getReason();
    }

    @Override
    public boolean canCleanUp() {
        final DownloadLink link = getDownloadLink();
        return link == null || FilePackage.isDefaultFilePackage(link.getFilePackage()) || CaptchaBlackList.getInstance().isWhitelisted(link);
    }

    public DownloadLink getDownloadLink() {
        return downloadLink.get();
    }

    @Override
    public String toString() {
        final DownloadLink link = getDownloadLink();
        if (link != null) {
            return "BlockDownloadCaptchasByLink:" + link.getUniqueID();
        } else {
            return "BlockDownloadCaptchasByLink";
        }
    }

    @Override
    public boolean matches(Challenge<Object> c) {
        final DownloadLink link = getDownloadLink();
        if (link != null) {
            return link == c.getDownloadLink();
        } else {
            return false;
        }
    }

    @Override
    public SkipRequest getSkipRequest() {
        return skipRequest;
    }

    @Override
    public String getReason() {
        return reason;
    }
}
