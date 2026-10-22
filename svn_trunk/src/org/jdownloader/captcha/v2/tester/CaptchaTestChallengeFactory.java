package org.jdownloader.captcha.v2.tester;

import java.awt.Rectangle;
import java.io.File;
import java.security.SecureRandom;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.JSonStorage;
import org.appwork.utils.Hash;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.Challenge.CaptchaRequestType;
import org.jdownloader.captcha.v2.challenge.altcha.AltchaChallenge;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.BasicCaptchaChallenge;
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.controller.PluginClassLoader;
import org.jdownloader.plugins.controller.PluginClassLoader.PluginClassLoaderChild;
import org.jdownloader.plugins.controller.host.HostPluginController;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;

import jd.http.Browser;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

/**
 * Builds throwaway {@link Challenge}s used by the IDE-only "Test &amp; Debug" tab in the captcha settings (see
 * {@code CaptchaTestPanel}) to manually trigger a real solve attempt for a given {@link jd.plugins.CaptchaType.CAPTCHA_TYPE} without a real
 * download/login/crawl in progress. The site key/URL come from {@link CaptchaTestParameters} (pre-filled with each captcha provider's own
 * publicly documented test site key where one exists); the {@link PluginForHost} instance a challenge is attached to is only a technical
 * carrier required by the {@link Challenge} API and is unrelated to the captcha provider itself.
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
            final PluginForHost plugin = Plugin.getNewPluginInstance(null, lazyPlugin, classLoader);
            /*
             * A freshly created Plugin instance defaults to LogController.TRASH (a no-op logger, see Plugin#logger) until something
             * explicitly calls setLogger() -- real downloads get that wired up by the download pipeline, this throwaway carrier never goes
             * through it. Without this, abstractPluginForCaptchaSolver#getPluginChallengeSolver() (which prefers the challenge's own
             * plugin logger for both the solver and its Browser instance) would silently discard all solver/browser logging for test
             * challenges. Instant flush so the log shows up in the console right away instead of only after the periodic flush timeout.
             */
            final LogSource logger = LogController.getInstance().getLogger(plugin.getClass().getName());
            logger.setInstantFlush(true);
            plugin.setLogger(logger);
            return plugin;
        } catch (final PluginException e) {
            return null;
        }
    }

    /**
     * Builds a fresh test challenge for the given captcha type, or null if the type has no test data (see
     * {@link CAPTCHA_TYPE#hasTestChallenges()}) or no carrier plugin is available.
     */
    public static Challenge<?> newChallenge(final CAPTCHA_TYPE type, final CaptchaRequestType requestType, final CaptchaTestParameters parameters) {
        final PluginForHost plugin = newCarrierPlugin();
        if (plugin == null) {
            return null;
        }
        if (CaptchaTestParameters.usesImage(type)) {
            final File imageFile = parameters.getImageFile();
            if (imageFile == null || !imageFile.isFile()) {
                return null;
            }
            final Challenge<?> challenge;
            final String description = parameters.getChallengeDescription();
            switch (type) {
            case IMAGE_SINGLE_CLICK_CAPTCHA:
                /* Exactly one click expected (max = 1 makes MultiClickCaptchaChallenge#isSingleClick() true). */
                challenge = new MultiClickCaptchaChallenge(imageFile, description, plugin, 1, 1);
                break;
            case IMAGE_MULTI_CLICK_CAPTCHA:
                /* The test image has exactly "minClicks" targets -> max = min so the dialog closes automatically after the last click. */
                challenge = new MultiClickCaptchaChallenge(imageFile, description, plugin, parameters.getMinClicks(), parameters.getMinClicks());
                break;
            case IMAGE:
            default:
                final BasicCaptchaChallenge imageChallenge = new BasicCaptchaChallenge("test", imageFile, "", description, plugin, 0);
                imageChallenge.setCaseSensitive(parameters.getCaseSensitive());
                challenge = imageChallenge;
                break;
            }
            challenge.setCaptchaRequestType(requestType);
            return challenge;
        }
        final String siteKey = parameters.getSiteKey();
        final String siteUrl = parameters.getSiteUrl();
        final String domain = Browser.getHost(siteUrl, false);
        try {
            final Challenge<String> challenge;
            switch (type) {
            case RECAPTCHA_V2:
                challenge = newRecaptchaChallenge(plugin, siteKey, siteUrl, domain, false, false, null, null, AbstractRecaptchaV2.TYPE.NORMAL);
                break;
            case RECAPTCHA_V2_INVISIBLE:
                challenge = newRecaptchaChallenge(plugin, siteKey, siteUrl, domain, false, false, null, null, AbstractRecaptchaV2.TYPE.INVISIBLE);
                break;
            case RECAPTCHA_V2_ENTERPRISE:
                challenge = newRecaptchaChallenge(plugin, siteKey, siteUrl, domain, false, true, null, null, AbstractRecaptchaV2.TYPE.INVISIBLE);
                break;
            case RECAPTCHA_V3:
                challenge = newRecaptchaChallenge(plugin, siteKey, siteUrl, domain, true, false, parameters.getAction(), parameters.getMinScore(), AbstractRecaptchaV2.TYPE.NORMAL);
                break;
            case RECAPTCHA_V3_ENTERPRISE:
                challenge = newRecaptchaChallenge(plugin, siteKey, siteUrl, domain, true, true, parameters.getAction(), parameters.getMinScore(), AbstractRecaptchaV2.TYPE.NORMAL);
                break;
            case HCAPTCHA:
                challenge = new HCaptchaChallenge(siteKey, plugin, plugin.getBrowser(), domain) {
                    @Override
                    protected String getSiteUrl() {
                        return siteUrl;
                    }
                };
                break;
            case CLOUDFLARE_TURNSTILE:
                challenge = new CloudflareTurnstileChallenge(plugin, siteKey) {
                    @Override
                    protected String getSiteUrl() {
                        return siteUrl;
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
                break;
            case ALTCHA:
                /* The "site key" field holds the challenge URL. If it is empty, a v1 test challenge is generated. */
                final String challengeUrl = StringUtils.isEmpty(siteKey) ? null : siteKey;
                final String challengeJson = challengeUrl == null ? newAltchaTestChallengeJson() : null;
                challenge = new AltchaChallenge(plugin, challengeUrl, challengeJson) {
                    @Override
                    protected String getSiteUrl() {
                        return siteUrl;
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
                break;
            default:
                return null;
            }
            challenge.setCaptchaRequestType(requestType);
            return challenge;
        } catch (final PluginException e) {
            return null;
        }
    }

    /**
     * Generates an ALTCHA v1 challenge JSON (see altcha-lib createChallenge) since there is no stable public demo page with a challenge
     * URL. The challenge is only created here, never solved: challenge = SHA-256(salt + number), signature = HMAC-SHA-256(challenge, key).
     * The small maxnumber keeps the proof of work cheap for the solver.
     */
    private static String newAltchaTestChallengeJson() {
        final SecureRandom random = new SecureRandom();
        final byte[] saltBytes = new byte[12];
        random.nextBytes(saltBytes);
        final long expires = System.currentTimeMillis() / 1000 + 10 * 60;
        final String salt = HexFormatter.byteArrayToHex(saltBytes) + "?expires=" + expires + "&";
        final int maxnumber = 50000;
        final int number = random.nextInt(maxnumber) + 1;
        final String challenge = Hash.getSHA256(salt + number);
        final String signature;
        try {
            final Mac mac = Mac.getInstance("HmacSHA256");
            mac.init(new SecretKeySpec("jdownloader-altcha-test".getBytes("UTF-8"), "HmacSHA256"));
            signature = HexFormatter.byteArrayToHex(mac.doFinal(challenge.getBytes("UTF-8")));
        } catch (final Exception e) {
            throw new WTFException(e);
        }
        final Map<String, Object> json = new LinkedHashMap<String, Object>();
        json.put("algorithm", "SHA-256");
        json.put("challenge", challenge);
        json.put("maxnumber", maxnumber);
        json.put("salt", salt);
        json.put("signature", signature);
        return JSonStorage.serializeToJson(json);
    }

    /**
     * @param v3Action
     *            Only used if isV3 is true. {@code isEnterprise=true} together with a non-null v3 action makes
     *            {@link RecaptchaV2Challenge#isV3()} effectively true too (see {@code AbstractRecaptchaV2#getVersion(String)}).
     */
    private static Challenge<String> newRecaptchaChallenge(final PluginForHost plugin, final String siteKey, final String siteUrl, final String domain, final boolean isV3, final boolean isEnterprise, final String v3Action, final Double minScore, final AbstractRecaptchaV2.TYPE recaptchaType) throws PluginException {
        return new RecaptchaV2Challenge(siteKey, null, plugin, plugin.getBrowser(), domain) {
            @Override
            public Double getMinScore() {
                return minScore;
            }

            @Override
            public boolean isV3() {
                return isV3;
            }

            @Override
            public boolean isEnterprise() {
                return isEnterprise;
            }

            @Override
            public String getType() {
                return recaptchaType.name();
            }

            @Override
            public Map<String, Object> getV3Action() {
                if (!isV3) {
                    return null;
                }
                final Map<String, Object> action = new HashMap<String, Object>();
                action.put("action", v3Action);
                return action;
            }

            @Override
            public String getSiteUrl() {
                return siteUrl;
            }
        };
    }
}
