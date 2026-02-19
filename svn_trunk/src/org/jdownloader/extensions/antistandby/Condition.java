package org.jdownloader.extensions.antistandby;

import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.extensions.antistandby.translate.T;

public enum Condition implements LabelInterface {
    RUNNING {
        @Override
        public String getLabel() {
            return T.T.gui_config_antistandby_whilejd2();
        }
    },
    DOWNLOADING {
        @Override
        public String getLabel() {
            return T.T.gui_config_antistandby_whiledl2();
        }
    },
    CRAWLING {
        @Override
        public String getLabel() {
            return T.T.gui_config_antistandby_whilecrawl();
        }
    },
    EXTRACTING {
        @Override
        public String getLabel() {
            return T.T.gui_config_antistandby_whileextract();
        }
    };

    @Override
    abstract public String getLabel();
}
