package jd.controlling.captcha;

import org.jdownloader.captcha.v2.Challenge;

public class SkipException extends Exception {
    /**
     *
     */
    private static final long  serialVersionUID = 1L;
    private final SkipRequest  skipRequest;
    private final Challenge<?> challenge;
    private final String       reason;

    public Challenge<?> getChallenge() {
        return challenge;
    }

    /** Why the challenge is skipped (e.g. "Silent mode"), or null if there is no further information. */
    public String getReason() {
        return reason;
    }

    public SkipException(Challenge<?> challenge, SkipRequest skipRequest) {
        this(challenge, skipRequest, (String) null);
    }

    public SkipException(Challenge<?> challenge, SkipRequest skipRequest, Throwable cause) {
        this(challenge, skipRequest, (String) null);
        this.initCause(cause);
    }

    public SkipException(Challenge<?> challenge, SkipRequest skipRequest, final String message) {
        super(skipRequest.name() + (message != null ? (":" + message) : ""));
        this.skipRequest = skipRequest;
        this.challenge = challenge;
        this.reason = message;
    }

    public SkipRequest getSkipRequest() {
        return skipRequest;
    }
}
