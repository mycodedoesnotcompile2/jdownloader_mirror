package org.jdownloader.captcha.v2.test;

import java.io.File;

import org.appwork.utils.ide.IDEUtils;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * User-editable input for a test challenge built by {@link CaptchaTestChallengeFactory} (see the IDE-only "Test &amp; Debug" tab in the
 * captcha settings). {@link #getDefaults(CAPTCHA_TYPE)} provides the pre-filled values for each captcha type.
 */
public class CaptchaTestParameters {
    private String siteKey;
    private String siteUrl;
    private String action;
    private String expectedResult;
    private File   imageFile;
    private Double minScore;
    private int    minClicks = 1;

    public CaptchaTestParameters(final String siteKey, final String siteUrl, final String action, final String expectedResult) {
        this.siteKey = siteKey;
        this.siteUrl = siteUrl;
        this.action = action;
        this.expectedResult = expectedResult;
    }

    public String getSiteKey() {
        return siteKey;
    }

    public void setSiteKey(final String siteKey) {
        this.siteKey = siteKey;
    }

    public String getSiteUrl() {
        return siteUrl;
    }

    public void setSiteUrl(final String siteUrl) {
        this.siteUrl = siteUrl;
    }

    /** Only used by reCAPTCHA v3 types. */
    public String getAction() {
        return action;
    }

    public void setAction(final String action) {
        this.action = action;
    }

    /** Optional: if set, the solver's answer is compared against it automatically. */
    public String getExpectedResult() {
        return expectedResult;
    }

    public void setExpectedResult(final String expectedResult) {
        this.expectedResult = expectedResult;
    }

    /** Only used by reCAPTCHA v3 types: the minimum score the solver has to reach, null = none requested (default). */
    public Double getMinScore() {
        return minScore;
    }

    public void setMinScore(final Double minScore) {
        this.minScore = minScore;
    }

    /** Only used by multi click captcha types: the minimum amount of clicks required to solve the captcha. */
    public int getMinClicks() {
        return minClicks;
    }

    public void setMinClicks(final int minClicks) {
        this.minClicks = minClicks;
    }

    /** True if {@link #getMinClicks()} is relevant for the given captcha type. */
    public static boolean usesMinClicks(final CAPTCHA_TYPE type) {
        switch (type) {
        case IMAGE_MULTI_CLICK_CAPTCHA:
            return true;
        default:
            return false;
        }
    }

    /** True if {@link #getMinScore()} is relevant for the given captcha type. */
    public static boolean usesMinScore(final CAPTCHA_TYPE type) {
        switch (type) {
        case RECAPTCHA_V3:
        case RECAPTCHA_V3_ENTERPRISE:
            return true;
        default:
            return false;
        }
    }

    /** Only used by image captcha types: the captcha image the solver has to read. */
    public File getImageFile() {
        return imageFile;
    }

    public void setImageFile(final File imageFile) {
        this.imageFile = imageFile;
    }

    /** True if {@link #getImageFile()} is relevant for the given captcha type (and the site URL/key are not). */
    public static boolean usesImage(final CAPTCHA_TYPE type) {
        switch (type) {
        case IMAGE:
        case IMAGE_SINGLE_CLICK_CAPTCHA:
        case IMAGE_MULTI_CLICK_CAPTCHA:
            return true;
        default:
            return false;
        }
    }

    /** True if {@link #getAction()} is relevant for the given captcha type. */
    public static boolean usesAction(final CAPTCHA_TYPE type) {
        switch (type) {
        case RECAPTCHA_V3:
        case RECAPTCHA_V3_ENTERPRISE:
            return true;
        default:
            return false;
        }
    }

    /**
     * Returns the bundled test image of the image captcha type. It lies in the project's source folder next to this class, so it is looked
     * up via the project folder (IDE-only, see IDEUtils) and not via the installation folder. Returns null if it cannot be found.
     */
    private static File getDefaultTestImage(final String fileName) {
        final File projectFolder = IDEUtils.getProjectFolder(CaptchaTestParameters.class);
        if (projectFolder == null) {
            return null;
        }
        final File file = new File(projectFolder, "src/" + CaptchaTestParameters.class.getPackage().getName().replace('.', '/') + "/" + fileName);
        return file.isFile() ? file : null;
    }

    /** Returns the pre-filled test values for the given captcha type, or null if the type has no test data (see hasTestChallenges()). */
    public static CaptchaTestParameters getDefaults(final CAPTCHA_TYPE type) {
        switch (type) {
        case IMAGE: {
            /* Test image next to this class in the project folder, its text is case-sensitive. */
            final CaptchaTestParameters ret = new CaptchaTestParameters(null, null, null, "KoYy2j");
            ret.setImageFile(getDefaultTestImage("test_image_captcha.png"));
            return ret;
        }
        case IMAGE_SINGLE_CLICK_CAPTCHA: {
            /* Test image: 3x3 grid of trash cans, the full one (center) has to be clicked. No expected result: the click position varies. */
            final CaptchaTestParameters ret = new CaptchaTestParameters(null, null, null, null);
            ret.setImageFile(getDefaultTestImage("test_single_click_captcha.jpg"));
            return ret;
        }
        case IMAGE_MULTI_CLICK_CAPTCHA: {
            /* Test image: 8 open circles have to be clicked. */
            final CaptchaTestParameters ret = new CaptchaTestParameters(null, null, null, null);
            ret.setMinClicks(8);
            ret.setImageFile(getDefaultTestImage("test_multi_click_captcha.png"));
            return ret;
        }
        case RECAPTCHA_V2:
            /* Google's officially documented "always passes" reCAPTCHA v2 test site key, paired with their public demo page. */
            return new CaptchaTestParameters("6LeIxAcTAAAAAJcZVRqyHh71UMIEGNQ_MXjiZKhI", "https://www.google.com/recaptcha/api2/demo", null, null);
        case RECAPTCHA_V2_INVISIBLE:
            /*
             * Taken from an existing plugin that uses this type in production: jd.plugins.hoster.CopyCaseCom (login handling). No official
             * always-passing invisible test key is publicly documented by Google, so this is a real site key instead - it is bound to
             * copycase.com's domain by Google, hence the matching site URL.
             */
            return new CaptchaTestParameters("6LcjZ0EgAAAAAAZRgmPrZBH7aVM09gggWOzKNFIp", "https://copycase.com/login", null, null);
        case RECAPTCHA_V2_ENTERPRISE:
            /*
             * Taken from jd.plugins.hoster.MetArtCom (isEnterprise()==true / type INVISIBLE, no v3 action). Note: at runtime,
             * CAPTCHA_TYPE.getCaptchaTypeForChallenge() classifies invisible challenges as RECAPTCHA_V2_INVISIBLE before it checks
             * isEnterprise() (declaration order), so a real MetArt challenge is actually handled as RECAPTCHA_V2_INVISIBLE. It is used here
             * anyway since it is the only in-repo source found for the enterprise flag.
             */
            return new CaptchaTestParameters("6Ld3osYaAAAAAAXX89R8I6MFE1m5loKSWfUIfjLd", "https://www.metart.com/", null, null);
        case RECAPTCHA_V3:
            /*
             * Taken from 2captcha's public demo page (https://2captcha.com/de/demo/recaptcha-v3) which embeds a real
             * grecaptcha.execute(...) call with this site key and action - no in-repo plugin has a hardcoded v3 key (they fetch it from the
             * page at runtime).
             */
            return new CaptchaTestParameters("6LfB5_IbAAAAAMCtsjEHEHKqcB9iQocwwxTiihJu", "https://2captcha.com/de/demo/recaptcha-v3", "demo_action", null);
        case RECAPTCHA_V3_ENTERPRISE:
            /*
             * Taken from jd.plugins.hoster.FilerNet (its disabled fallback branch in doWebsiteApi(), key found in
             * https://filer.net/assets/GetFileView-D0EkjwK_-1766004219124.js, 2025-11-20). isEnterprise()=true together with a non-null v3
             * action makes the challenge effectively v3 too, which is why this maps to RECAPTCHA_V3_ENTERPRISE.
             */
            return new CaptchaTestParameters("6LfUvREsAAAAAHd79QK9HOfIAEVGqK4G4JxovEEn", "https://filer.net/", "download", null);
        case HCAPTCHA:
            /*
             * 2026-09-22: Do not use the official hCaptcha test-key here because this will not return a result or at least our existing
             * external solvers and BrowserSolver cannot cope with it!!
             */
            return new CaptchaTestParameters("122129ec-9e86-4ace-949e-19422b57364e", "https://ddownload.com/", null, null);
        case CLOUDFLARE_TURNSTILE:
            /* Cloudflare's officially documented "always passes" Turnstile test site key, paired with a public demo page. */
            // return new CaptchaTestParameters("1x00000000000000000000AA", "https://2captcha.com/demo/cloudflare-turnstile", null,
            // "XXXX.DUMMY.TOKEN.XXXX");
            /* 2026-10-08: Do not use test sitekey anymore since some captcha solver services (e.g. 2captcha) reject them. */
            return new CaptchaTestParameters("0x4AAAAAACGwIXkmZ2lsGdCV", "https://rapidgator.net/", null, null);
        default:
            return null;
        }
    }
}
