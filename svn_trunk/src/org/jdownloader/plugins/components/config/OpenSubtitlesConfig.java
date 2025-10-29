package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "opensubtitles.org", type = Type.HOSTER)
public interface OpenSubtitlesConfig extends PluginConfigInterface {
    final String                    text_ActionOnCaptchaRequired = "Action to perform when captcha is required for downloading";
    public static final TRANSLATION TRANSLATION                  = new TRANSLATION();

    public static class TRANSLATION {
        public String getActionOnCaptchaRequired_label() {
            return "Action to perform when captcha is required for downloading";
        }
    }

    public static enum ActionOnCaptchaRequired implements LabelInterface {
        PROCESS_CAPTCHA {
            @Override
            public String getLabel() {
                return "Ask for captcha";
            }
        },
        RETRY_LATER {
            @Override
            public String getLabel() {
                return "Try again later";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("PROCESS_CAPTCHA")
    @Order(10)
    ActionOnCaptchaRequired getActionOnCaptchaRequired();

    void setActionOnCaptchaRequired(final ActionOnCaptchaRequired action);
}