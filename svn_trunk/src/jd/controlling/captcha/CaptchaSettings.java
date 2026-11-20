package jd.controlling.captcha;

import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;

import org.appwork.storage.config.ConfigInterface;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultJsonObject;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LookUpKeys;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.captcha.v2.CaptchaHistoryEntry;

public interface CaptchaSettings extends ConfigInterface {
    public static enum MODE {
        // AUTO_ONLY,
        NORMAL,
        SKIP_ALL,
        // SKIP_MANUAL
    }

    public static enum CAPTCHA_TIMEOUT_ACTION {
        SKIP,
        SKIP_HOSTER,
        RETRY,
        ASK
    }

    /**
     * The skip actions of the download (HOSTER) captcha dialogs. The one chosen in
     * {@link CaptchaSettings#getHosterCaptchaDialogDefaultAction()} is the action of the visible button of the dialog, the other ones are
     * available in its dropdown.
     */
    public static enum HosterCaptchaDialogDefaultAction {
        /** Skips only this captcha. */
        SKIP_SINGLE,
        /** Skips this captcha and all further captchas of the host. */
        BLOCK_HOSTER,
        /** Skips this captcha and all further captchas of the package. */
        BLOCK_PACKAGE,
        /** Skips this captcha and all further download captchas. */
        BLOCK_ALL_CAPTCHAS,
        /** Skips this captcha and stops all downloads. */
        STOP_DOWNLOADS
    }

    /**
     * The skip actions of the CRAWLER captcha dialogs (and of all other dialogs that are no download captchas). The one chosen in
     * {@link CaptchaSettings#getCrawlerCaptchaDialogDefaultAction()} is the action of the visible button of the dialog, the other ones are
     * available in its dropdown.
     */
    public static enum CrawlerCaptchaDialogDefaultAction {
        /** Cancels only this captcha. */
        SKIP_SINGLE,
        /** Cancels this captcha and stops the crawl job. */
        CANCEL_LINKGRABBING,
        /** Cancels this captcha and does not ask for further crawler captchas. */
        STOP_SHOWING_CRAWLER_CAPTCHAS
    }

    public static enum INTERACTIVE_CAPTCHA_PRIVACY_LEVEL {
        AUTO,
        NORMAL,
        STRICT
    }

    public static final CaptchaSettings CFG = JsonConfig.create(CaptchaSettings.class);

    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry("Developer option: True to show the \"Test & Debug\" tab in the captcha settings, which allows to run test challenges through the captcha solvers. This tab is always shown if JDownloader runs from the IDE.")
    boolean isShowCaptchaDebugPanel();

    void setShowCaptchaDebugPanel(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("Enable the notifications for external captcha solvers")
    boolean isRemoteCaptchaBubbleEnabled();

    void setRemoteCaptchaBubbleEnabled(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("Affects login captchas only: If enabled, the manual captcha solvers (browser, dialog, MyJDownloader) are asked first for a login captcha, before the external captcha solvers. The external solvers are still used afterwards as usual (see the setting \"Start the next solver after\" of the solvers). Background: When the user is adding an account or an account gets checked and a captcha is required, it should be solved fast and the user is typically faster than external solvers. If you disable this setting, login captchas are handled exactly the same way as any other captchas, in the order of the solver table.")
    boolean isAvoidAutoSolverForLoginCaptchas();

    void setAvoidAutoSolverForLoginCaptchas(boolean b);

    @AboutConfig
    @DefaultEnumValue("SKIP")
    @LookUpKeys({ "captchatimeoutaction", "hostercaptchatimeoutaction" })
    @DescriptionForConfigEntry("Defines what should happen if a captcha prompt runs into a timeout meaning that neither the user nor any captcha solver did answer the captcha prompt. This setting affects all HOSTER download/account-check processes.")
    CAPTCHA_TIMEOUT_ACTION getOnHosterCaptchaTimeoutAction();

    void setOnHosterCaptchaTimeoutAction(CAPTCHA_TIMEOUT_ACTION b);

    @AboutConfig
    @DefaultEnumValue("SKIP")
    @LookUpKeys("crawlercaptchatimeoutaction")
    @DescriptionForConfigEntry("Defines what should happen if a captcha prompt runs into a timeout meaning that neither the user nor any captcha solver did answer the captcha prompt. This setting affects all CRAWLER processes.")
    CAPTCHA_TIMEOUT_ACTION getOnCrawlerCaptchaTimeoutAction();

    void setOnCrawlerCaptchaTimeoutAction(CAPTCHA_TIMEOUT_ACTION b);

    @AboutConfig
    @DefaultEnumValue("SKIP_SINGLE")
    @DescriptionForConfigEntry("The skip action that is behind the visible button of the HOSTER (download) captcha dialogs, all other skip actions are available in the dropdown of the button. It only applies to the button, closing the dialog (ESC, close button) just skips the single captcha. Login captchas can only be skipped one by one.")
    HosterCaptchaDialogDefaultAction getHosterCaptchaDialogDefaultAction();

    void setHosterCaptchaDialogDefaultAction(HosterCaptchaDialogDefaultAction action);

    @AboutConfig
    @DefaultEnumValue("SKIP_SINGLE")
    @DescriptionForConfigEntry("The skip action that is behind the visible button of the CRAWLER captcha dialogs (and of all other captcha dialogs that are no download captchas), all other skip actions are available in the dropdown of the button. It only applies to the button, closing the dialog (ESC, close button) just skips the single captcha.")
    CrawlerCaptchaDialogDefaultAction getCrawlerCaptchaDialogDefaultAction();

    void setCrawlerCaptchaDialogDefaultAction(CrawlerCaptchaDialogDefaultAction action);

    @AboutConfig
    @DefaultIntValue(95)
    @DescriptionForConfigEntry("Do not Change me unless you know 100000% what this value is used for!")
    int getAutoCaptchaPriorityThreshold();

    void setAutoCaptchaPriorityThreshold(int value);

    @AboutConfig
    @DescriptionForConfigEntry("Captcha Mode: Normal = JD will ask for captchas, SKIP ALL = JDownloader will not ask for captchas. Items that require a captcha to be solved will be skipped.")
    @DefaultEnumValue("NORMAL")
    MODE getCaptchaMode();

    void setCaptchaMode(MODE mode);

    @AboutConfig
    @DefaultIntValue(10 * 60 * 1000)
    @DescriptionForConfigEntry("Timeout in milliseconds after which a challenge (captcha) invalidates (e.g sessions run out...) this timeout should be set by the plugins")
    int getDefaultChallengeTimeout();

    void setDefaultChallengeTimeout(int ms);

    @AboutConfig
    @DefaultIntValue(10 * 60 * 1000)
    @SpinnerValidator(min = 2 * 60 * 1000, max = 10 * 60 * 1000, step = 1000)
    @DescriptionForConfigEntry("Default max time in milliseconds a single captcha solver has to solve a challenge before it gets killed, unless the solver itself specifies a lower value (e.g. a server-side polling timeout).")
    int getDefaultMaxSolverChallengePollingTimeoutMillis();

    void setDefaultMaxSolverChallengePollingTimeoutMillis(int ms);

    @AboutConfig
    @DefaultIntValue(10000)
    @DescriptionForConfigEntry("Timeout in milliseconds during which the user can abort the upload of a captcha to an external captcha solver. The chance to abort is offered via a notification, so this only has an effect if the external captcha solver notifications are enabled and external solver accounts are used")
    int getExternalCaptchaSolverChanceToAbortExternalCaptchaSolver();

    void setExternalCaptchaSolverChanceToAbortExternalCaptchaSolver(int ms);

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("If enabled, a click on the \"Hate captchas\" text of a captcha dialog stops the countdown of the dialog, so the dialog stays open while the user reads the premium information. Only captchas that can be kept alive have a countdown that can be stopped (currently reCAPTCHA v2), the countdown of all other captchas always runs until the end. A stopped countdown does not extend the timeout of the captcha itself: The dialog is closed when the captcha runs out.")
    boolean isCancelDialogCountdownOnHateCaptchaClick();

    void setCancelDialogCountdownOnHateCaptchaClick(boolean b);

    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry("If enabled, moving or dragging the mouse over the captcha image of a captcha dialog stops the countdown of the dialog. Only captchas that can be kept alive have a countdown that can be stopped (currently reCAPTCHA v2), the countdown of all other captchas always runs until the end. A stopped countdown does not extend the timeout of the captcha itself: The dialog is closed when the captcha runs out. See also CancelDialogCountdownOnMouseClick.")
    boolean isCancelDialogCountdownOnMouseMove();

    void setCancelDialogCountdownOnMouseMove(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    boolean isCancelDialogCountdownOnMouseClick();

    void setCancelDialogCountdownOnMouseClick(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("List of last used captcha type/domain. This information is used in JDownloader GUI for example for captcha solver recommendations or as a cache to know in beforehand, which plugin will request which type of captcha.")
    @DefaultJsonObject("[]")
    CopyOnWriteArrayList<CaptchaHistoryEntry> getCaptchaHistoryEntries();

    void setCaptchaHistoryEntries(CopyOnWriteArrayList<CaptchaHistoryEntry> entry);

    @AboutConfig
    @DescriptionForConfigEntry("Domains the user selected in the last run of the captcha wizard. They are preselected the next time the wizard is opened.")
    @DefaultJsonObject("[]")
    Set<String> getCaptchaWizardEntries();

    void setCaptchaWizardEntries(Set<String> entries);

    @AboutConfig
    @DescriptionForConfigEntry("Privacy level for interactive captchas ('Browser captchas'). For such captchas, the URL of the website where the captcha needs to be solved might be opened in your browser or sent to your external captcha solvers. This URL may contain information about the file you are about to download e.g. 'https://example.com/file/123456/test.rar'. Set this to strict then only the base URL will be used instead e.g. 'https://example.com/'. WARNING: This can cause problems with captcha solving such as invalid captcha responses or make the captchas harder to solve in your browser.")
    @DefaultEnumValue("AUTO")
    INTERACTIVE_CAPTCHA_PRIVACY_LEVEL getInteractiveCaptchaPrivacyLevel();

    void setInteractiveCaptchaPrivacyLevel(INTERACTIVE_CAPTCHA_PRIVACY_LEVEL mode);

    @AboutConfig(inGUIVisible = false)
    @DescriptionForConfigEntry("Order of the captcha solvers (IDs of the solver services). The solvers are started in this order, see the setting 'Start the next solver after (seconds)' of each solver. Solvers which are not in this list (e.g. new ones) come last in their default order.")
    @DefaultJsonObject("[]")
    List<String> getSolverOrder();

    void setSolverOrder(List<String> serviceIDs);
}
