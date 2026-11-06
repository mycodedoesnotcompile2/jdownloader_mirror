package org.jdownloader.captcha.blacklist;

import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;

import org.jdownloader.captcha.v2.Challenge;

public class BlockAllDownloadCaptchasEntry implements SessionBlackListEntry {
    private final String reason;

    public BlockAllDownloadCaptchasEntry(SkipException skipException) {
        this.reason = skipException.getReason();
    }

    @Override
    public boolean canCleanUp() {
        return false;
    }

    @Override
    public String toString() {
        return "BlockAllDownloadCaptchasEntry";
    }

    @Override
    public boolean matches(Challenge c) {
        return c.getCaptchaRequestType() != Challenge.CaptchaRequestType.DECRYPTER;
    }

    @Override
    public SkipRequest getSkipRequest() {
        return SkipRequest.BLOCK_ALL_CAPTCHAS;
    }

    @Override
    public String getReason() {
        return reason;
    }
}
