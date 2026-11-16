package org.jdownloader.utils;

/**
 * Central collection of all links to the JDownloader support portal (https://support.jdownloader.org) and the JDownloader knowledge/wiki pages
 * (https://jdownloader.org/knowledge).<br>
 * The first part of every field name is its category (GENERAL_, CAPTCHA_, ACCOUNT_, MYJD_, RECONNECT_, SETTINGS_, DEVELOPMENT_, PLUGINS_).<br>
 * All fields are compile time constants and can therefore also be used inside annotations.
 */
public class HelpLinks {
    private HelpLinks() {
    }

    /* General */
    public static final String GENERAL_SUPPORT_HOME                            = "https://support.jdownloader.org/";
    public static final String GENERAL_KNOWLEDGE_INDEX                         = "https://jdownloader.org/knowledge/index";
    public static final String GENERAL_KNOWLEDGE_CHAT                          = "https://jdownloader.org/knowledge/chat";
    public static final String GENERAL_COPY_VIEW_ADDED_URLS                    = "https://support.jdownloader.org/knowledgebase/article/copy-view-added-urls";
    public static final String GENERAL_DOWNLOADS_NOT_START                     = "https://support.jdownloader.org/knowledgebase/article/warum-starten-meine-downloads-nicht";
    public static final String GENERAL_UNINSTALL_JDOWNLOADER                   = "https://support.jdownloader.org/knowledgebase/article/how-can-i-uninstall-jdownloader";
    public static final String GENERAL_TROUBLESHOOTING_JDOWNLOADER_IS_SLOW     = "https://support.jdownloader.org/knowledgebase/article/troubleshooting-jdownloader-is-slow";
    public static final String GENERAL_VMOPTIONS_FILE                          = "https://support.jdownloader.org/knowledgebase/article/vmoptions-file";
    public static final String GENERAL_CREATE_AND_UPLOAD_SESSION_LOGS          = "https://support.jdownloader.org/knowledgebase/article/how-to-create-and-upload-session-logs";
    public static final String GENERAL_COLLECT_LINKS_FROM_UNSUPPORTED_WEBSITES = "https://support.jdownloader.org/knowledgebase/article/collect-and-download-links-from-unsupported-websites";

    /* Captcha */
    public static final String CAPTCHA_JD_OPENS_BROWSER                        = "https://support.jdownloader.org/knowledgebase/article/jd-opens-my-browser-to-display-captchas";
    public static final String CAPTCHA_ERROR_SKIPPED_REQUIRED                  = "https://support.jdownloader.org/knowledgebase/article/error-skipped-captcha-is-required";
    public static final String CAPTCHA_IN_PREMIUM_MODE                         = "https://support.jdownloader.org/knowledgebase/article/captchas-in-premium-mode-and-during-account-login";
    public static final String CAPTCHA_CUSTOM_BROWSER_SOLVER                   = "https://support.jdownloader.org/knowledgebase/article/define-custom-browser-solver-captcha-browser";

    /* Account */
    public static final String ACCOUNT_COOKIE_LOGIN_INSTRUCTIONS               = "https://support.jdownloader.org/knowledgebase/article/account-cookie-login-instructions";

    /* MyJDownloader */
    public static final String MYJD_WHAT_IS_MYJDOWNLOADER                      = "https://support.jdownloader.org/knowledgebase/article/what-is-myjdownloader";

    /* Reconnect */
    public static final String RECONNECT_MODEM                                 = "https://jdownloader.org/knowledge/wiki/reconnect/modem";
    public static final String RECONNECT_RECORDER                              = "https://jdownloader.org/knowledge/wiki/reconnect/reconnect-recorder";
    public static final String RECONNECT_ROUTER                                = "https://support.jdownloader.org/knowledgebase/article/router-reconnect";

    /* Settings */
    public static final String SETTINGS_CHUNKLOAD                              = "https://jdownloader.org/knowledge/wiki/glossary/chunkload";

    /* Development */
    public static final String DEVELOPMENT_SETUP_IDE_ECLIPSE                   = "https://support.jdownloader.org/knowledgebase/article/setup-ide-eclipse";

    /* Plugins */
    public static final String PLUGINS_WETRANSFER_ACCOUNT_LOGIN_INSTRUCTIONS   = "https://support.jdownloader.org/knowledgebase/article/wetransfer-account-login-instructions";
    public static final String PLUGINS_ARDMEDIATHEK_LOGIN_INSTRUCTIONS         = "https://support.jdownloader.org/knowledgebase/article/ardmediathek-login-instructions";
    public static final String PLUGINS_FSHARE_CUSTOM_API_CREDENTIALS           = "https://support.jdownloader.org/knowledgebase/article/fshare-custom-api-credentials";
    public static final String PLUGINS_1FICHIER_SETTINGS_AND_TROUBLESHOOTING   = "https://support.jdownloader.org/knowledgebase/article/plugins-1fichiercom-settings-and-troubleshooting";
}
