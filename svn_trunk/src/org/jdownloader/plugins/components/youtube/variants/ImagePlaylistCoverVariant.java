package org.jdownloader.plugins.components.youtube.variants;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.StringUtils;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.youtube.YoutubeConfig;
import org.jdownloader.plugins.components.youtube.YoutubeHelper;
import org.jdownloader.plugins.config.PluginJsonConfig;

import jd.plugins.DownloadLink;

public class ImagePlaylistCoverVariant extends ImageVariant {
    public ImagePlaylistCoverVariant(VariantBase base) {
        super(base);
    }

    public int getWidth() {
        switch (getBaseVariant()) {
        case PLAYLIST_COVER_HQ:
            return 480;
        case PLAYLIST_COVER_LQ:
            return 120;
        case PLAYLIST_COVER_MAX:
            return 1280;
        case PLAYLIST_COVER_MQ:
            return 320;
        default:
            throw new WTFException("Unsupported:" + getBaseVariant());
        }
    }

    @Override
    public String getFileNamePattern(final DownloadLink link) {
        final YoutubeConfig cfg = PluginJsonConfig.get(YoutubeConfig.class);
        final String playlistID = link.getStringProperty(YoutubeHelper.YT_PLAYLIST_ID);
        String pattern;
        if (playlistID != null) {
            pattern = cfg.getPackagePatternForPlaylists();
        } else {
            pattern = cfg.getPackagePatternForChannelPackages();
        }
        if (!StringUtils.endsWithCaseInsensitive(pattern, ".*EXT*")) {
            pattern += ".*EXT*";
        }
        return pattern;
    }

    @Override
    public String _getName(Object caller) {
        return _GUI.T.Youtube_covervariant_name(getBaseVariant().getiTagData().getImageQuality().getLocaleName());
    }

    @Override
    public String getFileNameQualityTag() {
        return getBaseVariant().getiTagData().getImageQuality().getLocaleTag();
    }

    public int getHeight() {
        switch (getBaseVariant()) {
        case PLAYLIST_COVER_HQ:
            return 360;
        case PLAYLIST_COVER_LQ:
            return 90;
        case PLAYLIST_COVER_MAX:
            return 720;
        case PLAYLIST_COVER_MQ:
            return 180;
        default:
            throw new WTFException("Unsupported:" + getBaseVariant());
        }
    }
}
