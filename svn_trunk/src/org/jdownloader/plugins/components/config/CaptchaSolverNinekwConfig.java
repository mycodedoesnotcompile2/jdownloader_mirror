package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultStringValue;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "9kw.eu", type = Type.HOSTER)
public interface CaptchaSolverNinekwConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getMouse_label() {
            return "Solve mouse captchas";
        }

        public String getFeedback_label() {
            return "Send feedback to 9kw.eu";
        }

        public String getHttps_label() {
            return "Use HTTPS";
        }

        public String getSelfsolve_label() {
            return "Self-solve captchas";
        }

        public String getLowcredits_label() {
            return "Handle low credits";
        }

        public String getHighqueue_label() {
            return "Handle high queue";
        }

        public String getConfirm_label() {
            return "Confirm captchas";
        }

        public String getMouseconfirm_label() {
            return "Confirm mouse captchas";
        }

        public String getPrio_label() {
            return "Priority";
        }

        public String getHour_label() {
            return "Hour limit";
        }

        public String getMinute_label() {
            return "Minute limit";
        }

        public String getThreadpool_size_label() {
            return "Thread pool size";
        }

        public String getBlackWhiteListingEnabled_label() {
            return "Enable black/white listing";
        }

        public String getBlacklistEntries_label() {
            return "Blacklist entries";
        }

        public String getWhitelistEntries_label() {
            return "Whitelist entries";
        }

        public String getBlacklistCheck_label() {
            return "Check blacklist";
        }

        public String getBlacklist_label() {
            return "Blacklist";
        }

        public String getWhitelistCheck_label() {
            return "Check whitelist";
        }

        public String getWhitelist_label() {
            return "Whitelist";
        }

        public String getBlacklistPrioCheck_label() {
            return "Check blacklist priority";
        }

        public String getBlacklistPrio_label() {
            return "Blacklist priority";
        }

        public String getWhitelistPrioCheck_label() {
            return "Check whitelist priority";
        }

        public String getWhitelistPrio_label() {
            return "Whitelist priority";
        }

        public String getBlacklistTimeoutCheck_label() {
            return "Check blacklist timeout";
        }

        public String getBlacklistTimeout_label() {
            return "Blacklist timeout";
        }

        public String getWhitelistTimeoutCheck_label() {
            return "Check whitelist timeout";
        }

        public String getWhitelistTimeout_label() {
            return "Whitelist timeout";
        }

        public String getHosteroptions_label() {
            return "Hoster options";
        }

        public String getDebug_label() {
            return "Enable debug mode";
        }
    }

    @AboutConfig
    @DefaultBooleanValue(true)
    @Order(10)
    boolean isMouse();

    void setMouse(boolean mouse);

    @AboutConfig
    @DefaultBooleanValue(true)
    @Order(20)
    boolean isFeedback();

    void setFeedback(boolean feedback);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(30)
    boolean isHttps();

    void setHttps(boolean https);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(40)
    boolean isSelfsolve();

    void setSelfsolve(boolean selfsolve);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(50)
    boolean isLowcredits();

    void setLowcredits(boolean lowcredits);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(60)
    boolean isHighqueue();

    void setHighqueue(boolean highqueue);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(70)
    boolean isConfirm();

    void setConfirm(boolean confirm);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(80)
    boolean isMouseconfirm();

    void setMouseconfirm(boolean mouseconfirm);

    @AboutConfig
    @SpinnerValidator(min = 0, max = 10, step = 1)
    @DefaultIntValue(0)
    @Order(90)
    int getPrio();

    void setPrio(int prio);

    @AboutConfig
    @SpinnerValidator(min = 0, max = 9999, step = 1)
    @DefaultIntValue(0)
    @Order(100)
    int getHour();

    void setHour(int hour);

    @AboutConfig
    @SpinnerValidator(min = 0, max = 9999, step = 1)
    @DefaultIntValue(0)
    @Order(110)
    int getMinute();

    void setMinute(int minute);

    @AboutConfig
    @SpinnerValidator(min = 1, max = 100, step = 1)
    @DefaultIntValue(3)
    @Order(120)
    int getThreadpool_size();

    void setThreadpool_size(int threadpool_size);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(130)
    boolean isBlackWhiteListingEnabled();

    void setBlackWhiteListingEnabled(boolean blackWhiteListingEnabled);

    @AboutConfig
    @DefaultStringValue("")
    @Order(140)
    String getBlacklistEntries();

    void setBlacklistEntries(String blacklistEntries);

    @AboutConfig
    @DefaultStringValue("")
    @Order(150)
    String getWhitelistEntries();

    void setWhitelistEntries(String whitelistEntries);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(160)
    boolean isBlacklistCheck();

    void setBlacklistCheck(boolean blacklistCheck);

    @AboutConfig
    @DefaultStringValue("")
    @Order(170)
    String getBlacklist();

    void setBlacklist(String blacklist);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(180)
    boolean isWhitelistCheck();

    void setWhitelistCheck(boolean whitelistCheck);

    @AboutConfig
    @DefaultStringValue("")
    @Order(190)
    String getWhitelist();

    void setWhitelist(String whitelist);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(200)
    boolean isBlacklistPrioCheck();

    void setBlacklistPrioCheck(boolean blacklistPrioCheck);

    @AboutConfig
    @DefaultStringValue("")
    @Order(210)
    String getBlacklistPrio();

    void setBlacklistPrio(String blacklistPrio);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(220)
    boolean isWhitelistPrioCheck();

    void setWhitelistPrioCheck(boolean whitelistPrioCheck);

    @AboutConfig
    @DefaultStringValue("")
    @Order(230)
    String getWhitelistPrio();

    void setWhitelistPrio(String whitelistPrio);

    @AboutConfig
    @DefaultStringValue("")
    @Order(240)
    String getHosteroptions();

    void setHosteroptions(String hosteroptions);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(250)
    boolean isDebug();

    void setDebug(boolean debug);
}