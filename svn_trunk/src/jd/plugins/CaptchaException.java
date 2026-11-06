package jd.plugins;

import org.jdownloader.captcha.blacklist.BlacklistEntry;

import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;

public class CaptchaException extends PluginException {
    public static class CaptchaRefreshException extends CaptchaException {
        public CaptchaRefreshException(SkipException skipException) {
            super(LinkStatus.ERROR_RETRY, SkipRequest.REFRESH, null, skipException.getReason());
        }
    }

    private final SkipRequest    skipRequest;
    private final BlacklistEntry blackListEntry;
    private final String         reason;

    public String getReason() {
        return reason;
    }

    public BlacklistEntry getBlackListEntry() {
        return blackListEntry;
    }

    public SkipRequest getSkipRequest() {
        return skipRequest;
    }

    public CaptchaException(SkipException skipException) {
        this(skipException.getSkipRequest(), null, skipException.getReason());
    }

    public CaptchaException(SkipRequest skipRequest) {
        this(skipRequest, null, null);
    }

    public CaptchaException(BlacklistEntry blackListEntry) {
        this(blackListEntry.getSkipRequest(), blackListEntry, blackListEntry.getReason());
    }

    protected CaptchaException(int linkStatus, SkipRequest skipRequest, BlacklistEntry blackListEntry, final String reason) {
        super(linkStatus);
        this.skipRequest = skipRequest;
        this.blackListEntry = blackListEntry;
        this.reason = reason;
    }

    protected CaptchaException(SkipRequest skipRequest, BlacklistEntry blackListEntry, final String reason) {
        this(LinkStatus.ERROR_CAPTCHA, skipRequest, blackListEntry, reason);
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
