package jd.plugins;

import java.util.List;

import javax.swing.Icon;

import org.jdownloader.DomainInfo;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.challenge.clickcaptcha.ClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.cutcaptcha.CutCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;

public class CaptchaType {
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
            public String getDescription() {
                return "Simple image-based captcha where you need to solve a visual challenge";
            }

            @Override
            public String getDomain() {
                return null;
            }
        },
        IMAGE_SINGLE_CLICK_CAPTCHA {
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
            public String getDescription() {
                return "Simple click-based captcha where you click on a single element";
            }

            @Override
            public String getDomain() {
                return null;
            }
        },
        IMAGE_MULTI_CLICK_CAPTCHA {
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
            public String getDescription() {
                return "Advanced click-based captcha requiring multiple clicks on specific elements";
            }

            @Override
            public String getDomain() {
                return null;
            }
        },
        RECAPTCHA_V3 {
            @Override
            public boolean canHandle(Challenge<?> c) {
                if (!(c instanceof RecaptchaV2Challenge)) {
                    return false;
                }
                final RecaptchaV2Challenge cl = (RecaptchaV2Challenge) c;
                return cl.isV3() && !cl.isEnterprise();
            }

            @Override
            public String getDisplayName() {
                return "reCAPTCHA v3";
            }

            @Override
            public String getDemoUrl() {
                return "https://www.google.com/recaptcha/api2/demo";
            }

            @Override
            public String getDescription() {
                return "Google's reCAPTCHA v3 that runs in the background without user interaction";
            }

            @Override
            public String getDomain() {
                return "google.com";
            }
        },
        RECAPTCHA_V3_ENTERPRISE {
            @Override
            public boolean canHandle(Challenge<?> c) {
                if (!(c instanceof RecaptchaV2Challenge)) {
                    return false;
                }
                final RecaptchaV2Challenge cl = (RecaptchaV2Challenge) c;
                return cl.isV3() && cl.isEnterprise();
            }

            @Override
            public String getDisplayName() {
                return "reCAPTCHA v3 Enterprise";
            }

            @Override
            public String getDemoUrl() {
                return "https://www.google.com/recaptcha/api2/demo";
            }

            @Override
            public String getDescription() {
                return "Google's reCAPTCHA v3 Enterprise that runs in the background without user interaction";
            }

            @Override
            public String getDomain() {
                return "google.com";
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
                return "https://www.google.com/recaptcha/api2/demo";
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
                return "https://recaptcha-demo.appspot.com/recaptcha-v2-invisible.php";
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
            public String getDescription() {
                return "Geetest v1 captcha (not currently supported)";
            }

            @Override
            public String getDomain() {
                return "geetest.com";
            }

            @Override
            public boolean isJDownloaderSupported() {
                return false;
            }
        },
        GEETEST_V4 {
            @Override
            public boolean canHandle(Challenge<?> c) {
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
            public String getDescription() {
                return "Geetest v4 captcha (not currently supported)";
            }

            @Override
            public String getDomain() {
                return "geetest.com";
            }

            @Override
            public boolean isJDownloaderSupported() {
                return false;
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
                return "https://2captcha.com/demo/cloudflare-turnstile";
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
            @Override
            public boolean canHandle(Challenge<?> c) {
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
            public String getDescription() {
                return "mTCaptcha service (not currently supported)";
            }

            @Override
            public String getDomain() {
                return "mtcaptcha.com";
            }

            @Override
            public boolean isJDownloaderSupported() {
                return false;
            }
        },
        FRIENDLY_CAPTCHA {
            @Override
            public boolean canHandle(Challenge<?> c) {
                return false;
            }

            @Override
            public String getDisplayName() {
                return "Friendly Captcha";
            }

            @Override
            public String getDemoUrl() {
                return "https://friendlycaptcha.com/#demo";
            }

            @Override
            public String getDescription() {
                return "Friendly Captcha 'Truly Invisible CAPTCHA, Privacy-First, Bot Protection'";
            }

            @Override
            public String getDomain() {
                return "friendlycaptcha.com";
            }

            @Override
            public boolean isJDownloaderSupported() {
                return false;
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

        /**
         * Checks if this captcha type is supported by JDownloader. This method can be used to collect captchas available in captcha solving
         * services, including those that JDownloader itself does not yet support.
         *
         * @return true if this captcha type is supported by JDownloader, false otherwise. Defaults to true for all types unless overridden.
         */
        public boolean isJDownloaderSupported() {
            return true;
        }

        /**
         * Returns icon for service. <br>
         * Can return null!!
         */
        public Icon getIcon() {
            if (this.getDomain() != null) {
                return DomainInfo.getInstance(this.getDomain()).getFavIcon();
            } else {
                /* Fallback icon */
                return NewTheme.I().getIcon(IconKey.ICON_IMAGE, 16);
            }
        }

        /**
         * Retrieves the captcha type that can handle the given challenge.
         *
         * @param c
         *            The challenge to check
         * @return The appropriate CAPTCHA_TYPE, or null if no type can handle it
         */
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

    private Boolean     enabled     = null;
    private AccountInfo accountInfo = null;
    final CAPTCHA_TYPE  ctype;

    public AccountInfo getAccountInfo() {
        return accountInfo;
    }

    public void setAccountInfo(AccountInfo accountInfo) {
        this.accountInfo = accountInfo;
    }

    public CaptchaType(final CAPTCHA_TYPE ctype) {
        this.ctype = ctype;
    }

    private String getEnabledProperty() {
        return "captcha_type_" + ctype + "_enabled";
    }

    public boolean isEnabled() {
        final Account ac = getAccount();
        if (ac == null) {
            return true;
        }
        return ac.getBooleanProperty(getEnabledProperty(), true);
    }

    protected Account getAccount() {
        final AccountInfo ai = getAccountInfo();
        if (ai == null) {
            return null;
        }
        return ai.getAccount();
    }

    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
        final Account ac = getAccount();
        if (ac != null) {
            ac.setProperty(getEnabledProperty(), this.enabled);
        }
    }

    public CAPTCHA_TYPE getCAPTCHA_TYPE_STATIC() {
        return this.ctype;
    }

    public boolean isSupported() {
        final AccountInfo ai = getAccountInfo();
        if (ai == null) {
            return false;
        }
        final Account acc = ai.getAccount();
        if (acc == null) {
            return false;
        }
        final PluginForHost plg = acc.getPlugin();
        if (!(plg instanceof abstractPluginForCaptchaSolver)) {
            return false;
        }
        final abstractPluginForCaptchaSolver captchaSolverPlugin = (abstractPluginForCaptchaSolver) plg;
        final List<CAPTCHA_TYPE> supportedTypes = captchaSolverPlugin.getSupportedCaptchaTypes();
        if (supportedTypes == null) {
            return false;
        }
        if (supportedTypes.contains(this.ctype)) {
            return true;
        }
        return false;
    }

    @Override
    public String toString() {
        return this.ctype + " | enabled: " + this.enabled;
    }
}
