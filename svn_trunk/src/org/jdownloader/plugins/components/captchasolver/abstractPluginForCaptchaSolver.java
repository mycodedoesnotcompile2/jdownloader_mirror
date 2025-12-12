package org.jdownloader.plugins.components.captchasolver;

import java.util.List;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.DebugMode;
import org.jdownloader.DomainInfo;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.PluginChallengeSolver;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.cutcaptcha.CutCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.PluginForHost;

/**
 * Abstract base class for captcha solver plugins.
 */
public abstract class abstractPluginForCaptchaSolver extends PluginForHost {
    // Minimum balance threshold for notifications, TODO: Add functionality
    private static final double MIN_BALANCE_THRESHOLD = 1.0; // $1.00 USD/EUR

    /**
     * Enum representing different types of captchas with canHandle functionality.
     */
    public enum CAPTCHA_TYPE {
        IMAGE {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return c instanceof ImageCaptchaChallenge;
            }

            @Override
            public String getDisplayName() {
                return "Image Captcha";
            }

            @Override
            public String getDemoUrl() {
                return null;
            }

            @Override
            public DomainInfo getDomainInfo() {
                // TODO: Add DomainInfo with fitting icon
                return DomainInfo.getInstance("jdownloader.org");
            }

            @Override
            public String getDescription() {
                return "Simple image-based captcha where you need to solve a visual challenge";
            }

            @Override
            public String getDomain() {
                return null;
            }
        },
        SINGLE_CLICK_CAPTCHA {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return c instanceof ClickCaptchaChallenge;
            }

            @Override
            public String getDisplayName() {
                return "Single Click image Captcha";
            }

            @Override
            public String getDemoUrl() {
                return null;
            }

            @Override
            public DomainInfo getDomainInfo() {
                // TODO: Add DomainInfo with fitting icon
                return DomainInfo.getInstance("jdownloader.org");
            }

            @Override
            public String getDescription() {
                return "Simple click-based captcha where you click on a single element";
            }

            @Override
            public String getDomain() {
                return null;
            }
        },
        MULTI_CLICK_CAPTCHA {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return c instanceof MultiClickCaptchaChallenge;
            }

            @Override
            public String getDisplayName() {
                return "Multi Click image Captcha";
            }

            @Override
            public String getDemoUrl() {
                return null;
            }

            @Override
            public DomainInfo getDomainInfo() {
                // TODO: Add DomainInfo with fitting icon
                return DomainInfo.getInstance("jdownloader.org");
            }

            @Override
            public String getDescription() {
                return "Advanced click-based captcha requiring multiple clicks on specific elements";
            }

            @Override
            public String getDomain() {
                return null;
            }
        },
        RECAPTCHA_V2_INVISIBLE {
            @Override
            public boolean canHandle(Challenge<?> c) {
                if (!(c instanceof RecaptchaV2Challenge)) {
                    return false;
                }
                final RecaptchaV2Challenge cl = (RecaptchaV2Challenge) c;
                if (cl.isInvisible()) {
                    return true;
                } else {
                    return false;
                }
            }

            @Override
            public String getDisplayName() {
                return "reCAPTCHA v2 Invisible";
            }

            @Override
            public String getDemoUrl() {
                // TODO: Find reCaptcha invisible demo
                return "https://www.google.com/recaptcha/api2/demo";
            }

            @Override
            public DomainInfo getDomainInfo() {
                return DomainInfo.getInstance(getDomain());
            }

            @Override
            public String getDescription() {
                return "Google's invisible reCAPTCHA v2 that runs in the background without user interaction";
            }

            @Override
            public String getDomain() {
                return "google.com";
            }
        },
        RECAPTCHA_V2_ENTERPRISE {
            @Override
            public boolean canHandle(Challenge<?> c) {
                if (!(c instanceof RecaptchaV2Challenge)) {
                    return false;
                }
                final RecaptchaV2Challenge cl = (RecaptchaV2Challenge) c;
                return cl.isEnterprise();
            }

            @Override
            public String getDisplayName() {
                return "reCAPTCHA v2 Enterprise";
            }

            @Override
            public String getDemoUrl() {
                // TODO: Find better reCaptcha enterprise demo
                return "https://recaptcha-demo.appspot.com/recaptcha-v2-invisible.php";
            }

            @Override
            public DomainInfo getDomainInfo() {
                return DomainInfo.getInstance(getDomain());
            }

            @Override
            public String getDescription() {
                return "Google's enterprise reCAPTCHA v2 with advanced security features";
            }

            @Override
            public String getDomain() {
                return "google.com";
            }
        },
        RECAPTCHA_V2 {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return c instanceof RecaptchaV2Challenge;
            }

            @Override
            public String getDisplayName() {
                return "reCAPTCHA v2";
            }

            @Override
            public String getDemoUrl() {
                return "https://www.google.com/recaptcha/api2/demo";
            }

            @Override
            public DomainInfo getDomainInfo() {
                return DomainInfo.getInstance(getDomain());
            }

            @Override
            public String getDescription() {
                return "Google's reCAPTCHA v2 with checkbox verification";
            }

            @Override
            public String getDomain() {
                return "google.com";
            }
        },
        HCAPTCHA {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return c instanceof HCaptchaChallenge;
            }

            @Override
            public String getDisplayName() {
                return "hCaptcha";
            }

            @Override
            public String getDemoUrl() {
                return "https://hcaptcha.com/?tab=demo";
            }

            @Override
            public DomainInfo getDomainInfo() {
                return DomainInfo.getInstance(getDomain());
            }

            @Override
            public String getDescription() {
                return "Privacy-focused alternative to reCAPTCHA with image-based challenges";
            }

            @Override
            public String getDomain() {
                return "hcaptcha.com";
            }
        },
        CUTCAPTCHA {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return c instanceof CutCaptchaChallenge;
            }

            @Override
            public String getDisplayName() {
                return "Cutcaptcha";
            }

            @Override
            public String getDemoUrl() {
                return "https://www.cutcaptcha.com/";
            }

            @Override
            public DomainInfo getDomainInfo() {
                return DomainInfo.getInstance(getDomain());
            }

            @Override
            public String getDescription() {
                return "Interactive puzzle-based captcha service. Mostly used for provider 'Filecrypt'.";
            }

            @Override
            public String getDomain() {
                return "cutcaptcha.com";
            }
        },
        /* Types that exist but we don't support. */
        GEETEST_V1 {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return false;
            }

            @Override
            public String getDisplayName() {
                return "Geetest v1";
            }

            @Override
            public String getDemoUrl() {
                return "https://www.geetest.com/en/adaptive-captcha-demo";
            }

            @Override
            public DomainInfo getDomainInfo() {
                return DomainInfo.getInstance(getDomain());
            }

            @Override
            public String getDescription() {
                return "Geetest v1 captcha (not currently supported)";
            }

            @Override
            public String getDomain() {
                return "geetest.com";
            }
        },
        GEETEST_V4 {
            @Override
            public boolean canHandle(Challenge<?> c) {
                /* 2025-12-11: Currently not supported by JDownloader */
                return false;
            }

            @Override
            public String getDisplayName() {
                return "Geetest v4";
            }

            @Override
            public String getDemoUrl() {
                return "https://www.geetest.com/en/adaptive-captcha-demo";
            }

            @Override
            public DomainInfo getDomainInfo() {
                return DomainInfo.getInstance(getDomain());
            }

            @Override
            public String getDescription() {
                return "Geetest v4 captcha";
            }

            @Override
            public String getDomain() {
                return "geetest.com";
            }
        },
        CLOUDFLARE_TURNSTILE {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return c instanceof CloudflareTurnstileChallenge;
            }

            @Override
            public String getDisplayName() {
                return "Cloudflare Turnstile";
            }

            @Override
            public String getDemoUrl() {
                // TODO: Add better demo URL
                return "https://2captcha.com/demo/cloudflare-turnstile";
            }

            @Override
            public DomainInfo getDomainInfo() {
                return DomainInfo.getInstance(getDomain());
            }

            @Override
            public String getDescription() {
                return "Cloudflare's Turnstile captcha alternative with improved performance";
            }

            @Override
            public String getDomain() {
                return "cloudflare.com";
            }
        },
        MT_CAPTCHA {
            /* https://www.mtcaptcha.com/ */
            @Override
            public boolean canHandle(Challenge<?> c) {
                /* 2025-04-30: Not supported by JDownloader yet. */
                return false;
            }

            @Override
            public String getDisplayName() {
                return "mTCaptcha";
            }

            @Override
            public String getDemoUrl() {
                return "https://www.mtcaptcha.com/";
            }

            @Override
            public DomainInfo getDomainInfo() {
                return DomainInfo.getInstance(getDomain());
            }

            @Override
            public String getDescription() {
                return "mTCaptcha service (not currently supported)";
            }

            @Override
            public String getDomain() {
                return "mtcaptcha.com";
            }
        };

        /**
         * Checks if this captcha type can handle the given challenge.
         *
         * @param c
         *            The challenge to check
         * @return true if this captcha type can handle the challenge, false otherwise
         */
        public abstract boolean canHandle(Challenge<?> c);

        /**
         * Returns the display name of this captcha type.
         *
         * @return Display name as String
         */
        public abstract String getDisplayName();

        /**
         * Returns the demo URL for this captcha type.
         *
         * @return Demo URL as String, or null if not applicable
         */
        public abstract String getDemoUrl();

        /**
         * Returns domain information for this captcha type.
         *
         * @return DomainInfo object, currently always null
         */
        public abstract DomainInfo getDomainInfo();

        /**
         * Returns a description of this captcha type.
         *
         * @return Description as String
         */
        public abstract String getDescription();

        /**
         * Returns the domain of the captcha provider.
         *
         * @return Domain as String (e.g. "google.com"), or null if not applicable
         */
        public abstract String getDomain();

        public static CAPTCHA_TYPE getCaptchaTypeForChallenge(Challenge<?> c) {
            if (c == null) {
                return null;
            }
            for (CAPTCHA_TYPE type : CAPTCHA_TYPE.values()) {
                if (type.canHandle(c)) {
                    return type;
                }
            }
            return null;
        }
    }

    /** Returns false if the solver does not have enough balance to solve the given captcha challenge. */
    public boolean enoughBalanceFor(final Challenge<?> c, final Account account) throws Exception {
        return true;
    }

    /**
     * Checks if this solver can handle a specific challenge.
     *
     * @param c
     *            The challenge to check
     * @return true if this solver can handle the challenge, false otherwise
     */
    public final boolean canHandle(final Challenge<?> c) {
        if (!validateBlackWhite(c)) {
            return false;
        }
        final List<CAPTCHA_TYPE> supportedTypes = this.getSupportedCaptchaTypes();
        for (final CAPTCHA_TYPE supportedType : supportedTypes) {
            if (supportedType.canHandle(c)) {
                return true;
            }
        }
        return false;
    }

    public <T> PluginChallengeSolver<T> getPluginChallengeSolver(final Challenge<T> c, Account account) throws Exception {
        if (!canHandle(c)) {
            return null;
        }
        final abstractPluginForCaptchaSolver plugin = getNewPluginInstance(getLazyP());
        plugin.setBrowser(plugin.createNewBrowserInstance());
        return new PluginChallengeSolver<T>(plugin, account);
    }

    /**
     * Constructor for the plugin.
     *
     * @param wrapper
     *            The plugin wrapper
     */
    public abstractPluginForCaptchaSolver(PluginWrapper wrapper) {
        super(wrapper);
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            this.enablePremium(getBuyPremiumUrl());
        }
    }

    /**
     * Returns the features supported by this plugin.
     *
     * @return Array of supported features
     */
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION };
    }

    public abstract String getBuyPremiumUrl();

    /**
     * Reports a captcha as invalid.
     *
     * @param response
     *            The captcha response to report as invalid
     * @return true if the report was successfully sent, false otherwise
     */
    public abstract boolean setInvalid(final AbstractResponse<?> response, final Account account) throws Exception;

    /**
     * Reports a captcha as valid.
     *
     * @param response
     *            The captcha response to report as valid
     * @return true if the report was successfully sent, false otherwise
     */
    public abstract boolean setValid(final AbstractResponse<?> response, final Account account) throws Exception;

    public boolean setUnused(final AbstractResponse<?> response, final Account account) throws Exception {
        return false;
    }

    /**
     * Returns the list of captcha types supported by this solver. <br>
     * Important: If a solver supports all reCaptcha captcha types, return RECAPTCHA_V2, RECAPTCHA_V2_ENTERPRISE AND RECAPTCHA_V2_INVISIBLE
     * !
     *
     *
     *
     * @return List of supported captcha types
     */
    public abstract java.util.List<CAPTCHA_TYPE> getSupportedCaptchaTypes();

    /**
     * Determines whether the user should be notified when the account balance is low.
     *
     * @return true if the user should be notified on low balance, false otherwise
     */
    public boolean notifyOnLowBalance() {
        // TODO: Implement setting and logic
        return true;
    }

    /** Returns interval used for polling when waiting for captcha solution from solver. */
    protected int getPollingIntervalMillis() {
        return 5000;
    }

    /**
     * Fetches account information for a given account. This abstract method overrides the one from PluginForHost and forces subclasses to
     * implement it specifically for captcha solver services.
     *
     * @param account
     *            The account to fetch information for
     * @return The account information
     * @throws Exception
     *             If an error occurs during the fetch operation
     */
    @Override
    public abstract AccountInfo fetchAccountInfo(final Account account) throws Exception;

    public abstract void solve(CESSolverJob<?> job, Account account) throws Exception;

    public boolean validateBlackWhite(Challenge<?> c) {
        // TODO: Add functionality
        return true;
    }

    protected static void checkInterruption() throws InterruptedException {
        if (Thread.interrupted()) {
            throw new InterruptedException();
        }
    }

    @Override
    public AvailableStatus requestFileInformation(DownloadLink parameter) throws Exception {
        /* Must override but should never be called. */
        throw new WTFException();
    }

    @Override
    public void handleFree(DownloadLink link) throws Exception {
        /* Must override but should never be called. */
        throw new WTFException();
    }
}