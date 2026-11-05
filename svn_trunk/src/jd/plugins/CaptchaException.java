package jd.plugins;

import org.jdownloader.captcha.blacklist.BlacklistEntry;

import jd.controlling.captcha.SkipRequest;

public class CaptchaException extends PluginException {
    public static class CaptchaRefreshException extends CaptchaException {
        public CaptchaRefreshException() {
            super(LinkStatus.ERROR_RETRY, SkipRequest.REFRESH, null);
        }
    }

    private final SkipRequest    skipRequest;
    private final BlacklistEntry blackListEntry;

    public BlacklistEntry getBlackListEntry() {
        return blackListEntry;
    }

    public SkipRequest getSkipRequest() {
        return skipRequest;
    }

    public CaptchaException(SkipRequest skipRequest) {
        this(skipRequest, null);
    }

    public CaptchaException(BlacklistEntry blackListEntry) {
        this(blackListEntry.getSkipRequest(), blackListEntry);
    }

    protected CaptchaException(int linkStatus, SkipRequest skipRequest, BlacklistEntry blackListEntry) {
        super(linkStatus);
        this.skipRequest = skipRequest;
        this.blackListEntry = blackListEntry;
    }

    protected CaptchaException(SkipRequest skipRequest, BlacklistEntry blackListEntry) {
        this(LinkStatus.ERROR_CAPTCHA, skipRequest, blackListEntry);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append(getClass().getName());
        if (skipRequest != null) {
            sb.append(":SkipRequest=").append(skipRequest.name());
        }
        if (blackListEntry != null) {
            sb.append(":BlacklistEntry=").append(blackListEntry.toString());
        }
        return sb.toString();
    }
}
