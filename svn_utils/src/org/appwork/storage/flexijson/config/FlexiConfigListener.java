package org.appwork.storage.flexijson.config;

import java.util.EventListener;

public interface FlexiConfigListener extends EventListener {
    public void onValueModified(FlexiConfigBuilder builder, final Object storage, final String key, final Object oldValue, final Object newValue);
}