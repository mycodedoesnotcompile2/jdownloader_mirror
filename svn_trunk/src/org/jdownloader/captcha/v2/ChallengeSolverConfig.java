package org.jdownloader.captcha.v2;

import java.util.ArrayList;

import org.appwork.storage.config.ConfigInterface;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;

@Deprecated
public interface ChallengeSolverConfig extends ConfigInterface {
    @AboutConfig
    @DefaultBooleanValue(true)
    boolean isBlackWhiteListingEnabled();

    void setBlackWhiteListingEnabled(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    boolean isEnabled();

    void setEnabled(boolean b);

    @AboutConfig
    ArrayList<String> getBlacklistEntries();

    @AboutConfig
    ArrayList<String> getWhitelistEntries();

    void setBlacklistEntries(ArrayList<String> list);

    void setWhitelistEntries(ArrayList<String> list);
}
