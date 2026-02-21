package org.jdownloader.extensions.antistandby;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import jd.plugins.ExtensionConfigInterface;

import org.appwork.storage.Storage;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultFactory;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.defaults.AbstractDefaultFactory;
import org.appwork.storage.config.handler.KeyHandler;

public interface AntiStandbyConfig extends ExtensionConfigInterface {

    class DefaultCondition extends AbstractDefaultFactory<Set<Condition>> {
        @Override
        public Set<Condition> getDefaultValue(KeyHandler<Set<Condition>> keyHandler) {
            final Storage storage;
            if (keyHandler != null && (storage = keyHandler.getStorageHandler().getPrimitiveStorage(keyHandler)) != null) {
                final String oldKey = "mode";
                final Object oldValue = storage.get(oldKey, null);
                if ("RUNNING".equals(oldValue)) {
                    storage.remove(oldKey);
                    return new HashSet<Condition>(Arrays.asList(Condition.RUNNING));
                } else if ("CRAWLING".equals(oldValue)) {
                    storage.remove(oldKey);
                    return new HashSet<Condition>(Arrays.asList(Condition.CRAWLING));
                } else if ("DOWNLOADING".equals(oldValue)) {
                    storage.remove(oldKey);
                    return new HashSet<Condition>(Arrays.asList(Condition.DOWNLOADING));
                } else if ("DOWNLOADINGDORCRAWLING".equals(oldValue)) {
                    storage.remove(oldKey);
                    return new HashSet<Condition>(Arrays.asList(Condition.CRAWLING, Condition.DOWNLOADING));
                }
            }
            return new HashSet<Condition>(Arrays.asList(Condition.CRAWLING, Condition.DOWNLOADING, Condition.EXTRACTING));
        }
    }

    @AboutConfig
    // @DefaultEnumArrayValue({ "CRAWLING", "DOWNLOADING", "EXTRACTING" })// disabled for migration
    @DefaultFactory(DefaultCondition.class)
    @DefaultOnNull
    public Set<Condition> getCondition();

    public void setCondition(Set<Condition> mode);

    @AboutConfig
    @DefaultBooleanValue(false)
    public boolean isDisplayRequired();

    public void setDisplayRequired(boolean b);
}
