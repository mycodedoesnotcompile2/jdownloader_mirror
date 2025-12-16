package jd.plugins.components.gopro;

import org.appwork.storage.StorableValidatorIgnoresMissingSetter;
import org.appwork.storage.flexijson.mapper.FlexiEnumFallback;

@StorableValidatorIgnoresMissingSetter
public enum GoProType {
    Photo,
    Video,
    MultiClipEdit,
    @FlexiEnumFallback("TimeLapse")
    TimeLapseAsZipPackage("TimeLapse"),
    TimeLapseAsSingleImages("TimeLapse"),
    TimeLapseVideo,
    @FlexiEnumFallback("Burst")
    BurstAsZipPackage("Burst"),
    BurstAsSingleImages("Burst"),
    Livestream,
    LoopedVideo,
    BurstVideo,
    Continuous,
    ExternalVideo,
    Session;

    public final String apiID;

    private GoProType() {
        apiID = name();
    }

    private GoProType(String apiID) {
        this.apiID = apiID;
    }
}
