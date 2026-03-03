package org.appwork.remoteapi.annotations.tags;

import org.appwork.remoteapi.annotations.APITagDefinition;

public class InternalAPITag implements APITagDefinition {
    @Override
    public String getName() {
        return "INTERNAL";
    }

    @Override
    public String getDescription() {
        return "Internal usage only! No Production usage allowed!";
    }

    @Override
    public String getIcon() {
        return "warning";
    }
}
