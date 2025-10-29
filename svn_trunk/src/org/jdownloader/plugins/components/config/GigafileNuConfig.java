package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "gigafile.nu", type = Type.CRAWLER)
public interface GigafileNuConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION  = new TRANSLATION();
    public static final CrawlMode   DEFAULT_MODE = CrawlMode.FILES_FOLDERS;

    public static class TRANSLATION {
        public String getCrawlMode2_label() {
            return "Crawl mode";
        }
    }

    public static enum CrawlMode implements LabelInterface {
        ZIP {
            @Override
            public String getLabel() {
                return "Add .zip container/single file only";
            }
        },
        FILES_FOLDERS {
            @Override
            public String getLabel() {
                return "Add individual files";
            }
        },
        ALL {
            @Override
            public String getLabel() {
                return "Add individual files and .zip container";
            }
        },
        DEFAULT {
            @Override
            public String getLabel() {
                return "Default: " + DEFAULT_MODE.getLabel();
            }

            @Override
            public CrawlMode getMode() {
                return DEFAULT_MODE.getMode();
            }
        };

        public CrawlMode getMode() {
            return this;
        }
    }

    @AboutConfig
    @DefaultEnumValue("DEFAULT")
    @DefaultOnNull
    @Order(10)
    CrawlMode getCrawlMode();

    void setCrawlMode(final CrawlMode mode);
}