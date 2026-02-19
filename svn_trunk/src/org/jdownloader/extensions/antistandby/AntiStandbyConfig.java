package org.jdownloader.extensions.antistandby;

import java.util.Set;

import jd.plugins.ExtensionConfigInterface;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumArrayValue;
import org.appwork.storage.config.annotations.DefaultOnNull;

public interface AntiStandbyConfig extends ExtensionConfigInterface {

    @AboutConfig
    @DefaultEnumArrayValue({ "RUNNING" })
    @DefaultOnNull
    public Set<Condition> getCondition();

    public void setCondition(Set<Condition> mode);

    @AboutConfig
    @DefaultBooleanValue(false)
    public boolean isDisplayRequired();

    public void setDisplayRequired(boolean b);
}
