package org.jdownloader.captcha.blacklist;

import jd.controlling.captcha.SkipRequest;

import org.jdownloader.captcha.v2.Challenge;

public class BlockAllDownloadCaptchasEntry implements SessionBlackListEntry {
    public BlockAllDownloadCaptchasEntry() {
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
}
