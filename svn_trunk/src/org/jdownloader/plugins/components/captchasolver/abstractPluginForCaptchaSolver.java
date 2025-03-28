package org.jdownloader.plugins.components.captchasolver;

import java.util.List;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.DebugMode;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.PluginChallengeSolver;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.cutcaptcha.CutCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.keycaptcha.KeyCaptchaCategoryChallenge;
import org.jdownloader.captcha.v2.challenge.keycaptcha.KeyCaptchaPuzzleChallenge;
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
    // Minimum balance threshold for notifications
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
        },
        RECAPTCHA_V2 {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return c instanceof RecaptchaV2Challenge;
            }
        },
        HCAPTCHA {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return c instanceof HCaptchaChallenge;
            }
        },
        CUTCAPTCHA {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return c instanceof CutCaptchaChallenge;
            }
        },
        SINGLE_CLICK_CAPTCHA {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return c instanceof ClickCaptchaChallenge;
            }
        },
        MULTI_CLICK_CAPTCHA {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return c instanceof MultiClickCaptchaChallenge;
            }
        },
        /* Types that exist but we don't support. */
        GEETEST_V1 {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return false;
            }
        },
        GEETEST_V4 {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return false;
            }
        },
        KEY_CAPTCHA {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return c instanceof KeyCaptchaPuzzleChallenge || c instanceof KeyCaptchaCategoryChallenge;
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
        boolean allowedByType = false;
        final List<CAPTCHA_TYPE> supportedTypes = this.getSupportedCaptchaTypes();
        for (final CAPTCHA_TYPE supportedType : supportedTypes) {
            if (supportedType.canHandle(c)) {
                allowedByType = true;
                break;
            }
        }
        if (!allowedByType) {
            return false;
        }
        return true;
    }

    public <T> PluginChallengeSolver<T> getPluginChallengeSolver(final Challenge<T> c, Account account) throws Exception {
        if (!canHandle(c)) {
            return null;
        }
        final abstractPluginForCaptchaSolver plugin = getNewPluginInstance(getLazyP());
        final PluginForCaptchaSolverSolverService dummyService = new PluginForCaptchaSolverSolverService();
        return new PluginChallengeSolver<T>(plugin, account, /* TODO */dummyService);
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
        // TODO: Implement logic
        return true;
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