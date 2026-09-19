package org.jdownloader.captcha.v2;

/**
 * Result of evaluating a challenge against the {@link CaptchaChallengeFilterController} rules for a given solver.
 */
public enum CaptchaChallengeFilterResult {
    /** No rule matched the challenge (default: the challenge is allowed). */
    NOT_FILTERED,
    /** A blacklist rule matched: the challenge is blocked for this solver. */
    FILTERED_BLACKLIST,
    /** A whitelist rule matched: the challenge is explicitly allowed for this solver. */
    FILTERED_WHITELIST
}
