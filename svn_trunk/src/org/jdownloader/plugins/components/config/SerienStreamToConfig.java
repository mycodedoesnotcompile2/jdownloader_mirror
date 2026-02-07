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

@PluginHost(host = "s.to", type = Type.CRAWLER)
public interface SerienStreamToConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getLanguagePriorityString_label() {
            return "Sprach-Präferenz";
        }

        public String getHosterPriorityString_label() {
            return "Hoster-Präferenz";
        }

        public String getSeasonCrawlMode_label() {
            return "Serien-Links ohne Staffelnummer wie folgt behandeln, wenn es mehr als eine Staffel gibt";
        }
    }

    @AboutConfig
    @DescriptionForConfigEntry("Lege die Priorität der Sprachen fest, z.B. 'Deutsch, Englisch' (= bevorzuge DE über EN). Nur die erste verfügbare Sprache wird hinzugefügt. Wenn keine der bevorzugten Sprachen gefunden wird, werden alle präferierten Mirrors in allen Sprachen hinzugefügt!")
    @Order(100)
    String getLanguagePriorityString();

    void setLanguagePriorityString(final String str);

    @AboutConfig
    @DescriptionForConfigEntry("Lege die Priorität der Quellen fest, z.B. 'VOE, Streamtape, Vidoza'. Nur die erste verfügbare Quelle wird hinzugefügt. Wenn keine der bevorzugten Mirror gefunden wird, werden alle hinzugefügt! Wenn eine bevorzugte Sprache und Mirror ausgewählt wurde wird versucht, die beste passende Kombination zu finden.")
    @Order(200)
    String getHosterPriorityString();

    void setHosterPriorityString(final String str);

    public static enum SeasonCrawlMode implements LabelInterface {
        ASK {
            @Override
            public String getLabel() {
                return "Fragen";
            }
        },
        ALL_SEASONS {
            @Override
            public String getLabel() {
                return "Alle Staffeln crawlen";
            }
        },
        FIRST_SEASON_PRESENTED_IN_BROWSER {
            @Override
            public String getLabel() {
                return "Die erste Staffel crawlen, die im Browser präsentiert wird";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("ASK")
    @Order(300)
    @DefaultOnNull
    SeasonCrawlMode getSeasonCrawlMode();

    void setSeasonCrawlMode(final SeasonCrawlMode mode);
}