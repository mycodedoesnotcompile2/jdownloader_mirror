package org.appwork.remoteapi.annotations.tags;

import org.appwork.remoteapi.annotations.APITagDefinition;

public class ExperimentalAPITag implements APITagDefinition {
    @Override
    public String getName() {
        return "EXPERIMENTAL";
    }

    @Override
    public String getDescription() {
        return "Not for production usage. May change at any time. Use with care.";
    }

    @Override
    public String getIcon() {
        return "flask";
    }
}

