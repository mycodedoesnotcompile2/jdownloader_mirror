package org.jdownloader.captcha.blacklist;

import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;

import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.Challenge;

public class BlockDownloadCaptchasByHost implements SessionBlackListEntry<Object> {
    private final String blockedHost;
    private final String reason;

    public String getBlockedHost() {
        return blockedHost;
    }

    public BlockDownloadCaptchasByHost(String host) {
        this(null, host);
    }

    public BlockDownloadCaptchasByHost(SkipException skipException, String host) {
        this.blockedHost = host;
        this.reason = skipException == null ? null : skipException.getReason();
    }

    @Override
    public boolean canCleanUp() {
        return false;
    }

    @Override
    public String toString() {
        return "BlockDownloadCaptchasByHost:" + getBlockedHost();
    }

    @Override
    public boolean matches(Challenge<Object> c) {
        return StringUtils.equals(c.getHost(), getBlockedHost());
    }

    @Override
    public SkipRequest getSkipRequest() {
        return SkipRequest.BLOCK_HOSTER;
    }

    @Override
    public String getReason() {
        return reason;
    }
}
