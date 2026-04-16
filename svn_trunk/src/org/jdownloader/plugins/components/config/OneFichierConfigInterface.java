package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "1fichier.com", type = Type.HOSTER)
public interface OneFichierConfigInterface extends PluginConfigInterface {
    public static final OneFichierConfigInterface.OneFichierConfigInterfaceTranslation TRANSLATION = new OneFichierConfigInterfaceTranslation();

    public static class OneFichierConfigInterfaceTranslation {
        public String getFreeDownloadWaitBetweenDownloadsLimitMode_label() {
            return "Free download: What to do on error 'You must wait between downloads', 'You already downloading a file' or similar?";
        }

        public String getNoFreeSlotsMode_label() {
            return "Free download: What to do on error 'download is temporarily limited due to high demand', 'no free slots' or similar?";
        }

        public String getNoFreeSlotsWaitMinutes_label() {
            return "Free download: Wait time (minutes) for errors related to 'download is temporarily limited due to high demand' or 'no free slots'?";
        }

        public String getNoFreeSlotsInstantRetryEnabled_label() {
            return "Free download: Allow some instant retries on error 'download is temporarily limited due to high demand', 'no free slots' or similar?";
        }

        public String getSSLMode_label() {
            return "SSL mode: Control http(s) handling for downloads";
        }

        public String getSmallFilesWaitIntervalSeconds_label() {
            return "Wait x seconds for small files (smaller than 50 MB) to prevent IP block";
        }

        public String getLinkcheckMode_label() {
            return "Linkcheck mode for password protected links";
        }

        public String getMaxPremiumChunks_label() {
            return "Max number of chunks for premium downloads (See 1fichier.com/hlp.html#dllent)";
        }

        public String getGlobalRequestIntervalLimit1fichierComMilliseconds_label() {
            return "Global request limit for 1fichier.com milliseconds";
        }

        public String getGlobalRequestIntervalLimitAPI1fichierComMilliseconds_label() {
            return "Global request limit for api.1fichier.com milliseconds";
        }
    }

    public static enum FreeDownloadWaitBetweenDownloadsLimitMode implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto: Put wait status on all 1fichier links";
            }
        },
        GLOBAL_RECONNECT {
            @Override
            public String getLabel() {
                return "Put wait status on all 1fichier links and trigger reconnect";
            }
        },
        GLOBAL_WAIT {
            @Override
            public String getLabel() {
                return "Put wait status on all 1fichier links";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("AUTO")
    @DescriptionForConfigEntry("Free download: What to do on error 'You must wait between downloads', 'You already downloading a file' or similar?")
    @Order(10)
    FreeDownloadWaitBetweenDownloadsLimitMode getFreeDownloadWaitBetweenDownloadsLimitMode();

    void setFreeDownloadWaitBetweenDownloadsLimitMode(final FreeDownloadWaitBetweenDownloadsLimitMode mode);

    public static enum FreeDownloadNoFreeSlotsMode implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto: Put wait status on all 1fichier links";
            }
        },
        GLOBAL_RECONNECT {
            @Override
            public String getLabel() {
                return "Put wait status on all 1fichier links and trigger reconnect";
            }
        },
        GLOBAL_WAIT {
            @Override
            public String getLabel() {
                return "Put wait status on all 1fichier links";
            }
        },
        PER_FILE {
            @Override
            public String getLabel() {
                return "Put wait status on each link where this error happens";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("AUTO")
    @DescriptionForConfigEntry("Free download: What to do on error 'download is temporarily limited due to high demand', 'no free slots' or similar?")
    @Order(11)
    FreeDownloadNoFreeSlotsMode getNoFreeSlotsMode();

    void setNoFreeSlotsMode(final FreeDownloadNoFreeSlotsMode mode);

    @AboutConfig
    @DefaultIntValue(15)
    @Order(12)
    @SpinnerValidator(min = 1, max = 600, step = 1)
    int getNoFreeSlotsWaitMinutes();

    void setNoFreeSlotsWaitMinutes(int num);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(13)
    boolean isNoFreeSlotsInstantRetryEnabled();

    void setNoFreeSlotsInstantRetryEnabled(boolean b);

    public static enum SSLMode implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto | Use website preferred protocol";
            }
        },
        FORCE_HTTPS {
            @Override
            public String getLabel() {
                return "Try to force https";
            }
        },
        FORCE_HTTP {
            @Override
            public String getLabel() {
                return "Try to force http";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("AUTO")
    @Order(20)
    @DescriptionForConfigEntry("Auto: Use whichever protocol the website provides. Account users can change it under 1fichier.com/console/params.pl -> 'Force the downloads without SSL'. Changes of this setting from forced http or https to auto may only apply for newly added downloads!")
    SSLMode getSSLMode();

    void setSSLMode(final SSLMode mode);

    public static enum LinkcheckMode implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto | Website Mass linkcheck";
            }
        },
        PREFER_SINGLE_LINKCHECK {
            @Override
            public String getLabel() {
                return "Single linkcheck for single links via API if possible";
            }
        },
        MASS_LINKCHECK {
            @Override
            public String getLabel() {
                return "Website Mass linkcheck";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("AUTO")
    @Order(35)
    @DescriptionForConfigEntry("1fichier does not provide the file information for password protected links via their mass linkcheck API. If you own a 1fichier account, and set the correct download password in beforehand, this setting can allow JDownloader to find the file information for single password protected items.")
    LinkcheckMode getLinkcheckMode();

    void setLinkcheckMode(final LinkcheckMode mode);

    @AboutConfig
    @DefaultIntValue(10)
    @SpinnerValidator(min = 0, max = 60)
    @Order(40)
    int getSmallFilesWaitIntervalSeconds();

    void setSmallFilesWaitIntervalSeconds(int i);

    @AboutConfig
    @DefaultIntValue(3)
    @Order(50)
    @SpinnerValidator(min = 0, max = 20, step = 1)
    int getMaxPremiumChunks();

    void setMaxPremiumChunks(int num);

    @AboutConfig
    @SpinnerValidator(min = 2500, max = 30000, step = 500)
    @DefaultIntValue(2500)
    @DescriptionForConfigEntry("Define global request limit for 1fichier.com milliseconds")
    @Order(60)
    int getGlobalRequestIntervalLimit1fichierComMilliseconds();

    void setGlobalRequestIntervalLimit1fichierComMilliseconds(int millis);

    @AboutConfig
    @SpinnerValidator(min = 2500, max = 30000, step = 500)
    @DefaultIntValue(2500)
    @DescriptionForConfigEntry("Define global request limit for api.1fichier.com milliseconds")
    @Order(70)
    int getGlobalRequestIntervalLimitAPI1fichierComMilliseconds();

    void setGlobalRequestIntervalLimitAPI1fichierComMilliseconds(int millis);
}