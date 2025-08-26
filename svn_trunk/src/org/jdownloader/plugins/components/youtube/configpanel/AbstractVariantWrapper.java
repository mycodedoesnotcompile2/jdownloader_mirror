package org.jdownloader.plugins.components.youtube.configpanel;

import java.util.Locale;

import org.appwork.txtresource.TranslationFactory;
import org.jdownloader.plugins.components.youtube.Projection;
import org.jdownloader.plugins.components.youtube.VariantIDStorable;
import org.jdownloader.plugins.components.youtube.itag.AudioType;
import org.jdownloader.plugins.components.youtube.variants.AbstractVariant;
import org.jdownloader.plugins.components.youtube.variants.AudioInterface;
import org.jdownloader.plugins.components.youtube.variants.ImageVariant;
import org.jdownloader.plugins.components.youtube.variants.SubtitleVariant;
import org.jdownloader.plugins.components.youtube.variants.VideoVariant;

public class AbstractVariantWrapper {
    private VariantIDStorable blackListEntry;

    public AbstractVariantWrapper(AbstractVariant variant) {
        this.variant = variant;
        blackListEntry = new VariantIDStorable(variant);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof AbstractVariantWrapper)) {
            return false;
        } else {
            return ((AbstractVariantWrapper) obj).blackListEntry.createUniqueID().equals(blackListEntry.createUniqueID());
        }
    }

    @Override
    public int hashCode() {
        return blackListEntry.createUniqueID().hashCode();
    }

    private boolean enabled = true;

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    final public AbstractVariant variant;

    public AudioType getAudioType() {
        if (variant instanceof AudioInterface) {
            return ((AudioInterface) variant).getAudioType();
        }
        return null;
    }

    public String getLanguageCode() {
        String lng = null;
        if (variant instanceof AudioInterface) {
            lng = ((AudioInterface) variant).getAudioId();
        } else if (variant instanceof SubtitleVariant) {
            lng = ((SubtitleVariant) variant).getLanguageCode();
        }
        if (lng != null) {
            lng = lng.replaceAll("[^a-zA-Z\\-]*", "");
        }
        return lng;
    }

    public Locale getLanguageLocale() {
        if (variant instanceof AudioInterface) {
            return ((AudioInterface) variant).getAudioLocale();
        } else if (variant instanceof SubtitleVariant) {
            final String lng = ((SubtitleVariant) variant).getLanguageCode();
            if (lng == null) {
                return null;
            }
            return TranslationFactory.stringToLocale(lng.replaceAll("[^a-zA-Z\\-]*", ""));
        } else {
            return null;
        }
    }

    public int getWidth() {
        if (variant instanceof VideoVariant) {
            return ((VideoVariant) variant).getVideoWidth();
        } else if (variant instanceof ImageVariant) {
            return ((ImageVariant) variant).getWidth();
        } else {
            return -1;
        }
    }

    public int getAudioBitrate() {
        if (variant instanceof AudioInterface) {
            return ((AudioInterface) variant).getAudioBitrate().getKbit();
        } else {
            return -1;
        }
    }

    public int getFramerate() {
        if (variant instanceof VideoVariant) {
            return ((VideoVariant) variant).getVideoFrameRate();
        } else {
            return -1;
        }
    }

    public String getVideoCodec() {
        if (variant instanceof VideoVariant) {
            return ((VideoVariant) variant).getVideoCodec().getLabel();
        } else {
            return "";
        }
    }

    public String getAudioCodec() {
        if (variant instanceof AudioInterface) {
            return ((AudioInterface) variant).getAudioCodec().getLabel();
        } else {
            return "";
        }
    }

    public int getHeight() {
        if (variant instanceof VideoVariant) {
            return ((VideoVariant) variant).getVideoHeight();
        } else if (variant instanceof ImageVariant) {
            return ((ImageVariant) variant).getHeight();
        } else {
            return -1;
        }
    }

    public VariantIDStorable getVariableIDStorable() {
        return blackListEntry;
    }

    public Projection getProjection() {
        if (variant instanceof VideoVariant) {
            return ((VideoVariant) variant).getProjection();
        } else {
            return null;
        }
    }
}
