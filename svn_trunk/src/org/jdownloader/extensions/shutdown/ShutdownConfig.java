package org.jdownloader.extensions.shutdown;

import jd.plugins.ExtensionConfigInterface;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.SpinnerValidator;

public interface ShutdownConfig extends ExtensionConfigInterface {
    @DefaultBooleanValue(false)
    @AboutConfig
    @DescriptionForConfigEntry("Forcing Shutdown works only on some systems.")
    boolean isForceShutdownEnabled();

    void setForceShutdownEnabled(boolean b);

    @DefaultBooleanValue(false)
    @AboutConfig
    boolean isForceForMacInstalled();

    void setForceForMacInstalled(boolean b);

    @DefaultBooleanValue(false)
    @AboutConfig
    @DescriptionForConfigEntry("If enabled, JD will shut down the system after downloads have finished")
    boolean isShutdownActive();

    void setShutdownActive(boolean b);

    @DefaultEnumValue("SHUTDOWN")
    @AboutConfig
    Mode getShutdownMode();

    void setShutdownMode(Mode mode);

    @DefaultIntValue(60)
    @AboutConfig
    @DescriptionForConfigEntry("Seconds after shutdown request until JDownloader closes.")
    @SpinnerValidator(max = 600, min = 5, step = 10)
    int getCountdownTime();

    void setCountdownTime(int seconds);

    @DefaultBooleanValue(false)
    @AboutConfig
    @DescriptionForConfigEntry("If you want the 'Shutdown enabled' flag to be enabled in a new session, then enable this flag")
    boolean isShutdownActiveByDefaultEnabled();

    void setShutdownActiveByDefaultEnabled(boolean b);

    @DefaultBooleanValue(true)
    @AboutConfig
    boolean isShowWarningDialog();

    public void setShowWarningDialog(boolean b);
}
