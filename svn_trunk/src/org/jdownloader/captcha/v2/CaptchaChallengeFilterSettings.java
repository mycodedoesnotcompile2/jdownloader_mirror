package org.jdownloader.captcha.v2;

import java.util.ArrayList;

import org.appwork.storage.config.ConfigInterface;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultJsonObject;

/**
 * Persistent storage for the global list of {@link CaptchaChallengeFilter} rules managed by {@link CaptchaChallengeFilterController}. The
 * filters used to live inside each captcha solver's own config; they are now stored centrally here, with every rule carrying its own solver
 * assignment.
 */
public interface CaptchaChallengeFilterSettings extends ConfigInterface {
    @DefaultJsonObject("[]")
    @AboutConfig
    ArrayList<CaptchaChallengeFilter> getFilterList();

    void setFilterList(ArrayList<CaptchaChallengeFilter> list);

    @DefaultBooleanValue(true)
    @AboutConfig
    boolean isFilterListEnabled();

    void setFilterListEnabled(boolean b);
}
