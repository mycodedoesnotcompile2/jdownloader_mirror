package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "fakirdebrid.net", type = Type.HOSTER)
public interface FakirdebridNetConfig extends PluginConfigInterface {
    final String                                         text_BlockDownloadSlotsForCloudDownloads = "Block download slots while files are being transferred to the multihost's servers first?";
    public static final FakirdebridNetConfig.TRANSLATION TRANSLATION                              = new TRANSLATION();
    public static final BlockDownloadSlotsMode           DEFAULT_MODE                             = BlockDownloadSlotsMode.BLOCK;

    public static class TRANSLATION {
        public String getBlockDownloadSlotsForCloudDownloads_label() {
            return text_BlockDownloadSlotsForCloudDownloads;
        }
    }

    public static enum BlockDownloadSlotsMode implements LabelInterface {
        BLOCK {
            @Override
            public String getLabel() {
                return "Block download slot and wait (max. 10 minutes)";
            }
        },
        DONT_BLOCK {
            @Override
            public String getLabel() {
                return "Don't block download slot and retry later";
            }
        },
        DEFAULT {
            @Override
            public String getLabel() {
                return "Default: " + DEFAULT_MODE.getLabel();
            }

            @Override
            public BlockDownloadSlotsMode getMode() {
                return DEFAULT_MODE.getMode();
            }
        };

        public BlockDownloadSlotsMode getMode() {
            return this;
        }
    }

    @AboutConfig
    @DefaultEnumValue("DEFAULT")
    @DefaultOnNull
    @Order(10)
    @DescriptionForConfigEntry(text_BlockDownloadSlotsForCloudDownloads)
    BlockDownloadSlotsMode getBlockDownloadSlotsForCloudDownloads();

    void setBlockDownloadSlotsForCloudDownloads(final BlockDownloadSlotsMode mode);
}
