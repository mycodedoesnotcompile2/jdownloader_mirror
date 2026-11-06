package org.jdownloader.captcha.blacklist;

import org.jdownloader.captcha.v2.Challenge;

import jd.controlling.captcha.SkipRequest;

public interface BlacklistEntry<T> {
    public boolean canCleanUp();

    public boolean matches(Challenge<T> c);

    public SkipRequest getSkipRequest();

    public String getReason();
}
