package org.appwork.remoteapi.annotations.tags;

import org.appwork.remoteapi.annotations.APITagDefinition;

public class DeprecatedAPITag implements APITagDefinition {
    @Override
    public String getName() {
        return "DEPRECATED";
    }

    @Override
    public String getDescription() {
        return "Deprecated API. Kept for backward compatibility.";
    }

    @Override
    public String getIcon() {
        return "warning";
    }
}

