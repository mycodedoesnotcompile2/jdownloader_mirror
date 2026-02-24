package org.appwork.remoteapi.annotations;

/**
 * Runtime definition for an API tag used by docs generation.
 */
public interface APITagDefinition {
    String getName();

    String getDescription();

    /**
     * Optional icon identifier (for example an icon key or symbolic name).
     */
    String getIcon();
}

