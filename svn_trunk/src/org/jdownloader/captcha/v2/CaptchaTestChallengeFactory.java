package org.jdownloader.captcha.v2;

import java.awt.Rectangle;
import java.util.HashMap;
import java.util.Map;

import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.jdownloader.captcha.v2.Challenge.CaptchaRequestType;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;
import org.jdownloader.plugins.controller.PluginClassLoader;
import org.jdownloader.plugins.controller.PluginClassLoader.PluginClassLoaderChild;
import org.jdownloader.plugins.controller.host.HostPluginController;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;

import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

/**
 * Builds throwaway {@link Challenge}s used by the "Test Captcha solver" column (see {@code CaptchaSolverCaptchaTypesSettingsPanelBuilder}
 * in the anticaptcha settings GUI, IDE-only) to manually trigger a real solve attempt for a given
 * {@link jd.plugins.CaptchaType.CAPTCHA_TYPE} without a real download/login/crawl in progress. Each challenge uses the captcha provider's
 * own publicly documented test site key (it always evaluates successfully on the provider's side, so solving it never affects a real site);
 * the {@link PluginForHost} instance it is attached to is only a technical carrier required by the {@link Challenge} API and is unrelated
 * to the captcha provider itself.
 */
public class CaptchaTestChallengeFactory {
    private CaptchaTestChallengeFactory() {
    }

    /** A throwaway, always-available host plugin instance used purely as the technical Plugin owner of test challenges. */
    private static PluginForHost newCarrierPlugin() {
        final LazyHostPlugin lazyPlugin = HostPluginController.getInstance().get("DirectHTTP");
        if (lazyPlugin == null) {
            return null;
        }
        final PluginClassLoaderChild classLoader = PluginClassLoader.getThreadPluginClassLoaderChild();
        try {
            return Plugin.getNewPluginInstance(null, lazyPlugin, classLoader);
        } catch (final PluginException e) {
            return null;
        }
    }

    /** Google's officially documented "always passes" reCAPTCHA v2 test site key, paired with their public demo page. */
    public static Challenge<String> newRecaptchaV2Challenge(final CaptchaRequestType requestType) {
        final PluginForHost plugin = newCarrierPlugin();
        if (plugin == null) {
            return null;
        }
        try {
            final RecaptchaV2Challenge challenge = new RecaptchaV2Challenge("6LeIxAcTAAAAAJcZVRqyHh71UMIEGNQ_MXjiZKhI", null, plugin, plugin.getBrowser(), "google.com") {
                @Override
                public Double getMinScore() {
                    return null;
                }

                @Override
                public boolean isV3() {
                    return false;
                }

                @Override
                public boolean isEnterprise() {
                    return false;
                }

                @Override
                public String getType() {
                    return AbstractRecaptchaV2.TYPE.NORMAL.name();
                }

                @Override
                public String getSiteUrl() {
                    return "https://www.google.com/recaptcha/api2/demo";
                }
            };
            challenge.setCaptchaRequestType(requestType);
            return challenge;
        } catch (final PluginException e) {
            return null;
        }
    }

    /**
     * reCAPTCHA v2 Invisible test data, taken from an existing plugin that uses this type in production: jd.plugins.hoster.CopyCaseCom (see
     * its login handling, {@code CaptchaHelperHostPluginRecaptchaV2} with {@code getType() == TYPE.INVISIBLE}). No official always-passing
     * invisible test key is publicly documented by Google, so this is a real site key instead - it is bound to copycase.com's domain by
     * Google, hence the matching site URL below.
     */
    public static Challenge<String> newRecaptchaV2InvisibleChallenge(final CaptchaRequestType requestType) {
        final PluginForHost plugin = newCarrierPlugin();
        if (plugin == null) {
            return null;
        }
        try {
            final RecaptchaV2Challenge challenge = new RecaptchaV2Challenge("6LcjZ0EgAAAAAAZRgmPrZBH7aVM09gggWOzKNFIp", null, plugin, plugin.getBrowser(), "copycase.com") {
                @Override
                public Double getMinScore() {
                    return null;
                }

                @Override
                public boolean isV3() {
                    return false;
                }

                @Override
                public boolean isEnterprise() {
                    return false;
                }

                @Override
                public String getType() {
                    return AbstractRecaptchaV2.TYPE.INVISIBLE.name();
                }

                @Override
                public String getSiteUrl() {
                    return "https://copycase.com/login";
                }
            };
            challenge.setCaptchaRequestType(requestType);
            return challenge;
        } catch (final PluginException e) {
            return null;
        }
    }

    /**
     * reCAPTCHA v3 (plain, non-enterprise) test data, taken from 2captcha's own public demo page
     * (https://2captcha.com/de/demo/recaptcha-v3), which embeds a real, live {@code grecaptcha.execute(...)} call with this site key and
     * action - no in-repo plugin has a hardcoded v3 key to reuse (they all fetch it dynamically from the page at runtime), so this is the
     * only stable, publicly documented source found for this type.
     */
    public static Challenge<String> newRecaptchaV3Challenge(final CaptchaRequestType requestType) {
        final PluginForHost plugin = newCarrierPlugin();
        if (plugin == null) {
            return null;
        }
        try {
            final RecaptchaV2Challenge challenge = new RecaptchaV2Challenge("6LfB5_IbAAAAAMCtsjEHEHKqcB9iQocwwxTiihJu", null, plugin, plugin.getBrowser(), "2captcha.com") {
                @Override
                public Double getMinScore() {
                    return null;
                }

                @Override
                public boolean isV3() {
                    return true;
                }

                @Override
                public boolean isEnterprise() {
                    return false;
                }

                @Override
                public String getType() {
                    return AbstractRecaptchaV2.TYPE.NORMAL.name();
                }

                @Override
                public Map<String, Object> getV3Action() {
                    final Map<String, Object> action = new HashMap<String, Object>();
                    action.put("action", "demo_action");
                    return action;
                }

                @Override
                public String getSiteUrl() {
                    return "https://2captcha.com/de/demo/recaptcha-v3";
                }
            };
            challenge.setCaptchaRequestType(requestType);
            return challenge;
        } catch (final PluginException e) {
            return null;
        }
    }

    /**
     * reCAPTCHA v3 Enterprise test data, taken from jd.plugins.hoster.FilerNet (its disabled fallback branch in doWebsiteApi(), kept for
     * reference after filer.net switched to hCaptcha in production; source comment there: 2025-11-20, key found in
     * https://filer.net/assets/GetFileView-D0EkjwK_-1766004219124.js). {@code isEnterprise()=true} together with a non-null
     * {@code getV3Action()} makes {@link RecaptchaV2Challenge#isV3()} effectively true too (see
     * {@code AbstractRecaptchaV2#getVersion(String)}), which is why this maps to CAPTCHA_TYPE.RECAPTCHA_V3_ENTERPRISE rather than a plain
     * enterprise checkbox. {@code getMinScore()} is overridden the same way FilerNet does it, since Google rejects the v3 action call below
     * that threshold otherwise ("Score too low").
     */
    public static Challenge<String> newRecaptchaV3EnterpriseChallenge(final CaptchaRequestType requestType) {
        final PluginForHost plugin = newCarrierPlugin();
        if (plugin == null) {
            return null;
        }
        try {
            final RecaptchaV2Challenge challenge = new RecaptchaV2Challenge("6LfUvREsAAAAAHd79QK9HOfIAEVGqK4G4JxovEEn", null, plugin, plugin.getBrowser(), "filer.net") {
                @Override
                public Double getMinScore() {
                    /* Same threshold as FilerNet's own RECAPTCHA_ENTERPRISE_MIN_SCORE constant. */
                    return 0.5d;
                }

                @Override
                public boolean isV3() {
                    return true;
                }

                @Override
                public boolean isEnterprise() {
                    return true;
                }

                @Override
                public String getType() {
                    return AbstractRecaptchaV2.TYPE.NORMAL.name();
                }

                @Override
                public Map<String, Object> getV3Action() {
                    final Map<String, Object> action = new HashMap<String, Object>();
                    action.put("action", "download");
                    return action;
                }

                @Override
                public String getSiteUrl() {
                    return "https://filer.net/";
                }
            };
            challenge.setCaptchaRequestType(requestType);
            return challenge;
        } catch (final PluginException e) {
            return null;
        }
    }

    /**
     * reCAPTCHA v2 Enterprise test data, taken from jd.plugins.hoster.MetArtCom (its {@code isEnterprise()==true} /
     * {@code getType()==TYPE.INVISIBLE} overrides, no {@code getV3Action()}). Note: at runtime, CAPTCHA_TYPE.getCaptchaTypeForChallenge()
     * classifies invisible challenges as RECAPTCHA_V2_INVISIBLE before it ever checks {@code isEnterprise()} (declaration order), so a real
     * MetArt challenge is actually handled as RECAPTCHA_V2_INVISIBLE, not RECAPTCHA_V2_ENTERPRISE. It is used here regardless since it is
     * the only in-repo source found for the enterprise flag, and this test button only needs a realistic challenge to solve, not a
     * challenge that re-classifies as this exact type.
     */
    public static Challenge<String> newRecaptchaV2EnterpriseChallenge(final CaptchaRequestType requestType) {
        final PluginForHost plugin = newCarrierPlugin();
        if (plugin == null) {
            return null;
        }
        try {
            final RecaptchaV2Challenge challenge = new RecaptchaV2Challenge("6Ld3osYaAAAAAAXX89R8I6MFE1m5loKSWfUIfjLd", null, plugin, plugin.getBrowser(), "metart.com") {
                @Override
                public Double getMinScore() {
                    return null;
                }

                @Override
                public boolean isV3() {
                    return false;
                }

                @Override
                public boolean isEnterprise() {
                    return true;
                }

                @Override
                public String getType() {
                    return AbstractRecaptchaV2.TYPE.INVISIBLE.name();
                }

                @Override
                public String getSiteUrl() {
                    return "https://www.metart.com/";
                }
            };
            challenge.setCaptchaRequestType(requestType);
            return challenge;
        } catch (final PluginException e) {
            return null;
        }
    }

    /**
     * 2026-09-22: Do not use the official hCaptcha test-key here because this will not return a result or at least our existing external
     * solvers and BrowserSolver cannot cope with it!!
     */
    public static Challenge<String> newHCaptchaChallenge(final CaptchaRequestType requestType) {
        final PluginForHost plugin = newCarrierPlugin();
        if (plugin == null) {
            return null;
        }
        try {
            final HCaptchaChallenge challenge = new HCaptchaChallenge("122129ec-9e86-4ace-949e-19422b57364e", plugin, plugin.getBrowser(), "ddownload.com") {
                @Override
                protected String getSiteUrl() {
                    return "https://ddownload.com/";
                }
            };
            challenge.setCaptchaRequestType(requestType);
            return challenge;
        } catch (final PluginException e) {
            return null;
        }
    }

    /** Cloudflare's officially documented "always passes" Turnstile test site key, paired with their public demo page. */
    public static Challenge<String> newCloudflareTurnstileChallenge(final CaptchaRequestType requestType) {
        final PluginForHost plugin = newCarrierPlugin();
        if (plugin == null) {
            return null;
        }
        try {
            final CloudflareTurnstileChallenge challenge = new CloudflareTurnstileChallenge(plugin, "1x00000000000000000000AA") {
                @Override
                protected String getSiteUrl() {
                    return "https://2captcha.com/demo/cloudflare-turnstile";
                }

                @Override
                public BrowserViewport getBrowserViewport(final BrowserWindow screenResource, final Rectangle elementBounds) {
                    return null;
                }

                @Override
                public String getHTML(final HttpRequest request, final String id) {
                    return null;
                }
            };
            challenge.setCaptchaRequestType(requestType);
            return challenge;
        } catch (final PluginException e) {
            return null;
        }
    }
}
