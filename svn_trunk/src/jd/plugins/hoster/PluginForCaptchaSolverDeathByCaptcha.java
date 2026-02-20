package jd.plugins.hoster;

import java.awt.Color;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Currency;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Highlighter.HighlightPainter;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtPasswordField;
import org.appwork.swing.components.ExtTextField;
import org.appwork.swing.components.ExtTextHighlighter;
import org.appwork.utils.StringUtils;
import org.appwork.utils.ImageProvider.ImageProvider;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.cutcaptcha.CutCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.TokenCaptchaResponse;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.gui.InputChangedCallbackInterface;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.accounts.AccountBuilderInterface;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigDeathbycaptcha;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.gui.swing.components.linkbutton.JLink;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
import jd.http.requests.FormData;
import jd.http.requests.PostFormDataRequest;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.DefaultEditAccountPanelAPIKeyLogin;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import net.miginfocom.swing.MigLayout;

@HostPlugin(revision = "$Revision: 52344 $", interfaceVersion = 3, names = { "deathbycaptcha.com" }, urls = { "" })
public class PluginForCaptchaSolverDeathByCaptcha extends abstractPluginForCaptchaSolver {
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION };
    }

    public PluginForCaptchaSolverDeathByCaptcha(PluginWrapper wrapper) {
        super(wrapper);
    }

    private static final String PROPERTY_ACCOUNT_LOGIN_TYPE          = "login_type";
    private static final int    ACCOUNT_LOGIN_TYPE_AUTHTOKEN         = 0;
    private static final int    ACCOUNT_LOGIN_TYPE_USER_AND_PASSWORD = 1;

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("Accept", "application/json");
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setAllowedResponseCodes(200, 400);
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getBuyPremiumUrl() {
        return "https://" + getHost() + "/?refid=1235181816";
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        /**
         * 2026-01-16: API docs claim that reCaptcha enterprise is supported (as "beta") but when uploading a reCaptcha challenge there is
         * no place to specify that it's a reCaptcha enterprise so I highly doubt that. <br>
         * Docs: https://deathbycaptcha.com/api#supported_captchas
         */
        // types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        types.add(CAPTCHA_TYPE.CLOUDFLARE_TURNSTILE);
        types.add(CAPTCHA_TYPE.CUTCAPTCHA);
        /* 2025-02-24: Not supported anymore, see isChallengeSupported and: https://deathbycaptcha.com/api#supported_captchas */
        // types.add(CAPTCHA_TYPE.HCAPTCHA);
        types.add(CAPTCHA_TYPE.MT_CAPTCHA);
        types.add(CAPTCHA_TYPE.GEETEST_V1);
        types.add(CAPTCHA_TYPE.GEETEST_V4);
        types.add(CAPTCHA_TYPE.FRIENDLY_CAPTCHA);
        return types;
    }

    private String getBaseURL() {
        return "https://" + getHost();
    }

    protected String getApiBase() {
        return "https://api.dbcapi.me/api";
    }

    @Override
    public String getAGBLink() {
        return getBaseURL() + "/terms-of-service";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        }
        return str.matches("[a-zA-Z0-9]{100,}");
    }

    @Override
    protected String getAPILoginHelpURL() {
        return getBaseURL() + "/login#login-form";
    }

    @Override
    public AccountInfo fetchAccountInfo(Account account) throws Exception {
        final String username = account.getUser();
        final String password = account.getPass();
        final Number loginTypeProperty = (Number) account.getProperty(PROPERTY_ACCOUNT_LOGIN_TYPE);
        final int loginType = loginTypeProperty != null ? loginTypeProperty.intValue() : -1;
        Map<String, Object> entries = null;
        int[] loginTypesToTry = new int[2];
        int tryCount = 0;
        if (loginType == -1) {
            /* Login type hasn't been set before -> Try user:pw and also token login but only if password looks like valid token. */
            if (this.looksLikeValidAPIKey(password)) {
                loginTypesToTry[tryCount++] = ACCOUNT_LOGIN_TYPE_AUTHTOKEN;
            }
            loginTypesToTry[tryCount++] = ACCOUNT_LOGIN_TYPE_USER_AND_PASSWORD;
        } else if (loginType == ACCOUNT_LOGIN_TYPE_AUTHTOKEN) {
            loginTypesToTry[tryCount++] = ACCOUNT_LOGIN_TYPE_AUTHTOKEN;
        } else {
            loginTypesToTry[tryCount++] = ACCOUNT_LOGIN_TYPE_USER_AND_PASSWORD;
        }
        for (int i = 0; i < tryCount; i++) {
            final boolean isLastTry = i >= tryCount;
            final int currentLoginType = loginTypesToTry[i];
            final UrlQuery query = new UrlQuery();
            if (currentLoginType == ACCOUNT_LOGIN_TYPE_AUTHTOKEN) {
                query.addAndReplace("authtoken", URLEncode.encodeRFC2396(password));
            } else {
                query.addAndReplace("username", URLEncode.encodeRFC2396(username));
                query.addAndReplace("password", URLEncode.encodeRFC2396(password));
            }
            try {
                final Request req = br.createPostRequest(this.getApiBase() + "/user", query);
                entries = this.callAPI(req);
                account.setProperty(PROPERTY_ACCOUNT_LOGIN_TYPE, currentLoginType);
                break;
            } catch (final PluginException pe) {
                if (loginType != -1 || isLastTry) {
                    throw pe;
                }
            }
        }
        final Double creditsInDollarCent = ((Number) entries.get("balance")).doubleValue();
        final Double creditsInDollar = creditsInDollarCent / 100;
        final AccountInfo ai = new AccountInfo();
        ai.setAccountBalance(creditsInDollar, Currency.getInstance("USD"));
        if (this.isLoginViaAuthtoken(account)) {
            /* Set unique username for accounts which were added via token login */
            account.setUser(entries.get("user").toString());
        }
        ai.setStatus("Balance: " + ai.getAccountBalanceFormatted() + " | Rate: " + entries.get("rate"));
        return ai;
    }

    @Override
    public void solve(CESSolverJob<?> job, Account account) throws Exception {
        final Challenge<?> challenge = job.getChallenge();
        // TODO
        // job.showBubble(this);
        try {
            // TODO
            // challenge.sendStatsSolving(this);
            job.setStatus(SolverStatus.UPLOADING);
            final PostFormDataRequest r = new PostFormDataRequest(getApiBase() + "/captcha");
            final String username = account.getUser();
            final String password = account.getPass();
            if (this.isLoginViaAuthtoken(account)) {
                r.addFormData(new FormData("authtoken", password));
            } else {
                r.addFormData(new FormData("username", username));
                r.addFormData(new FormData("password", password));
            }
            final String type;
            if (challenge instanceof RecaptchaV2Challenge) {
                final RecaptchaV2Challenge rc_challenge = (RecaptchaV2Challenge) challenge;
                final Map<String, Object> token_param = new HashMap<String, Object>();
                token_param.put("googlekey", rc_challenge.getSiteKey());
                token_param.put("pageurl", rc_challenge.getSiteUrl());
                final Map<String, Object> v3action = rc_challenge.getV3Action();
                if (v3action != null) {
                    type = "RecaptchaV3";
                    r.addFormData(new FormData("type", "5"));
                    token_param.put("action", v3action.get("action"));
                    final Double minScore = rc_challenge.getMinScore();
                    if (minScore != null) {
                        token_param.put("min_score", minScore);
                    }
                } else {
                    if (rc_challenge.isV3()) {
                        type = "RecaptchaV3";
                        r.addFormData(new FormData("type", "5"));
                    } else if (rc_challenge.isInvisible()) {
                        type = "RecaptchaV2 invisible";
                        r.addFormData(new FormData("type", "4"));
                    } else {
                        type = "RecaptchaV2";
                        r.addFormData(new FormData("type", "4"));
                    }
                }
                r.addFormData(new FormData("token_params", JSonStorage.serializeToJson(token_param)));
            } else if (challenge instanceof CutCaptchaChallenge) {
                type = "CutCaptcha";
                final CutCaptchaChallenge cc = (CutCaptchaChallenge) challenge;
                r.addFormData(new FormData("type", "19"));
                final Map<String, Object> cutcaptcha_params = new HashMap<String, Object>();
                cutcaptcha_params.put("apikey", cc.getApiKey());
                cutcaptcha_params.put("miserykey", cc.getSiteKey());
                cutcaptcha_params.put("pageurl", cc.getSiteUrl());
                r.addFormData(new FormData("cutcaptcha_params", JSonStorage.serializeToJson(cutcaptcha_params)));
            } else if (challenge instanceof CloudflareTurnstileChallenge) {
                type = "CloudflareTurnstileCaptcha";
                final CloudflareTurnstileChallenge cc = (CloudflareTurnstileChallenge) challenge;
                r.addFormData(new FormData("type", "12"));
                final Map<String, Object> turnstile_params = new HashMap<String, Object>();
                turnstile_params.put("sitekey", cc.getSiteKey());
                turnstile_params.put("pageurl", cc.getSiteUrl());
                r.addFormData(new FormData("turnstile_params", JSonStorage.serializeToJson(turnstile_params)));
            } else if (challenge instanceof ImageCaptchaChallenge) {
                type = "Image";
                final ImageCaptchaChallenge bcc = (ImageCaptchaChallenge) challenge;
                final BufferedImage image = ImageProvider.read(bcc.getImageFile());
                final byte[] bytes = IconIO.toJpgBytes(image);
                r.addFormData(new FormData("captchafile", "captcha", "application/octet-stream", bytes));
            } else {
                throw new IllegalArgumentException("Unexpected captcha challenge type");
            }
            br.getPage(r);
            final Map<String, Object> uploadresp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final int captchaID = ((Number) uploadresp.get("captcha")).intValue();
            if (captchaID <= 0) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA, "Failed to upload captcha");
            }
            job.setStatus(SolverStatus.SOLVING);
            long startTime = System.currentTimeMillis();
            Map<String, Object> pollresp = null;
            while (true) {
                this.sleep(this.getPollingIntervalMillis(account), null);
                br.getPage(getApiBase() + "/captcha/" + captchaID);
                pollresp = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final int status = ((Number) pollresp.get("status")).intValue();
                final boolean is_correct = ((Boolean) pollresp.get("is_correct")).booleanValue();
                if (is_correct) {
                    break;
                }
                if (status == 255) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA, "Captcha solution incorrect");
                } else if (status != 0) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA, "Captcha solve error: status " + status);
                } else if (System.currentTimeMillis() - startTime > 60 * 60 * 1000) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA, "Captcha solve timeout");
                }
            }
            final String solution = (String) pollresp.get("text");
            job.getLogger().info("CAPTCHA(" + type + ") solved: " + solution);
            AbstractResponse resp = null;
            if (challenge instanceof RecaptchaV2Challenge || challenge instanceof HCaptchaChallenge || challenge instanceof CloudflareTurnstileChallenge || challenge instanceof CutCaptchaChallenge) {
                resp = new TokenCaptchaResponse((Challenge<String>) challenge, this, solution);
            } else {
                resp = new CaptchaResponse((Challenge<String>) challenge, this, solution);
            }
            resp.setCaptchaSolverTaskID(Integer.toString(captchaID));
            job.setAnswer(resp);
            return;
        } catch (Exception e) {
            // TODO
            // challenge.sendStatsError(this, e);
            throw e;
        }
    }

    @Override
    public boolean setInvalid(AbstractResponse<?> response, Account account) {
        /* API docs: https://deathbycaptcha.com/api#api_details_report */
        UrlQuery query = new UrlQuery();
        if (this.isLoginViaAuthtoken(account)) {
            query.addAndReplace("authtoken", URLEncode.encodeRFC2396(account.getPass()));
        } else {
            query.addAndReplace("password", URLEncode.encodeRFC2396(account.getPass())).addAndReplace("username", URLEncode.encodeRFC2396(account.getUser()));
        }
        try {
            final Request req = br.createPostRequest(this.getApiBase() + "/captcha/" + response.getCaptchaSolverTaskID() + "/report", query);
            this.callAPI(req);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    private boolean isLoginViaAuthtoken(final Account account) {
        return account.getIntegerProperty(PROPERTY_ACCOUNT_LOGIN_TYPE, ACCOUNT_LOGIN_TYPE_AUTHTOKEN) == ACCOUNT_LOGIN_TYPE_AUTHTOKEN;
    }

    private Map<String, Object> callAPI(final Request req) throws IOException, PluginException {
        br.getPage(req);
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        if (Boolean.TRUE.equals(entries.get("is_banned"))) {
            throw new AccountInvalidException("Account is banned");
        }
        final String error = (String) entries.get("error");
        final int status = ((Number) entries.get("status")).intValue();
        if (status == 0) {
            /* No error */
            return entries;
        }
        if (error == null) {
            return entries;
        }
        throw new AccountInvalidException(error);
    }

    @Override
    public AccountBuilderInterface getAccountFactory(final InputChangedCallbackInterface callback) {
        return new DeathbycaptchaAccountFactory(callback, this);
    }

    public static class DeathbycaptchaAccountFactory extends MigPanel implements AccountBuilderInterface {
        /**
         *
         */
        private static final long   serialVersionUID           = 1L;
        // Translation keys
        private static final String LOGIN_METHOD               = "login_method";
        private static final String LOGIN_METHOD_API           = "login_method_api";
        private static final String LOGIN_METHOD_USER_PASSWORD = "login_method_user_password";
        private static final String API_KEY_LABEL              = "api_key_label";
        private static final String OBTAIN_API_KEY             = "obtain_api_key";

        /**
         * Returns translations for the specified language code with English fallback.
         *
         * @param langCode
         *            Language code: "en", "de", "es", or "fr"
         * @return HashMap containing translations
         */
        private static Map<String, String> getTranslations(final String langCode) {
            Map<String, String> english = getEnglishTranslations();
            if ("en".equals(langCode)) {
                return english;
            }
            Map<String, String> targetLang;
            if ("de".equals(langCode)) {
                targetLang = getGermanTranslations();
            } else if ("es".equals(langCode)) {
                targetLang = getSpanishTranslations();
            } else if ("fr".equals(langCode)) {
                targetLang = getFrenchTranslations();
            } else {
                // Unknown language, return English
                return english;
            }
            // Merge: start with English, then overlay target language
            Map<String, String> merged = new HashMap<String, String>();
            merged.putAll(english);
            merged.putAll(targetLang);
            return merged;
        }

        private static Map<String, String> getEnglishTranslations() {
            Map<String, String> translations = new HashMap<String, String>();
            translations.put(LOGIN_METHOD, "Login Method:");
            translations.put(LOGIN_METHOD_API, "API Token");
            translations.put(LOGIN_METHOD_USER_PASSWORD, "Username & Password");
            translations.put(API_KEY_LABEL, "API Key:");
            translations.put(OBTAIN_API_KEY, "Get your API key here: ");
            return translations;
        }

        private static Map<String, String> getGermanTranslations() {
            Map<String, String> translations = new HashMap<String, String>();
            translations.put(LOGIN_METHOD, "Anmeldungsmethode:");
            translations.put(LOGIN_METHOD_API, "API-Token");
            translations.put(LOGIN_METHOD_USER_PASSWORD, "Benutzername & Passwort");
            translations.put(API_KEY_LABEL, "API-Schlüssel:");
            translations.put(OBTAIN_API_KEY, "Hol dir deinen API-Schlüssel hier: ");
            return translations;
        }

        private static Map<String, String> getSpanishTranslations() {
            Map<String, String> translations = new HashMap<String, String>();
            translations.put(LOGIN_METHOD, "Método de inicio de sesión:");
            translations.put(LOGIN_METHOD_API, "Token API");
            translations.put(LOGIN_METHOD_USER_PASSWORD, "Usuario y contraseña");
            translations.put(API_KEY_LABEL, "Clave API:");
            translations.put(OBTAIN_API_KEY, "Obtén tu clave API aquí: ");
            return translations;
        }

        private static Map<String, String> getFrenchTranslations() {
            Map<String, String> translations = new HashMap<String, String>();
            translations.put(LOGIN_METHOD, "Méthode de connexion :");
            translations.put(LOGIN_METHOD_API, "Jeton API");
            translations.put(LOGIN_METHOD_USER_PASSWORD, "Nom d'utilisateur et mot de passe");
            translations.put(API_KEY_LABEL, "Clé API :");
            translations.put(OBTAIN_API_KEY, "Obtenez votre clé API ici : ");
            return translations;
        }

        protected String getPassword() {
            if (this.pass == null) {
                return null;
            } else {
                return new String(this.pass.getPassword());
            }
        }

        protected String getUsername() {
            if (name == null) {
                return "";
            }
            if (_GUI.T.jd_gui_swing_components_AccountDialog_help_username().equals(this.name.getText())) {
                return null;
            }
            return this.name.getText();
        }

        protected String getApikey() {
            if (apikey == null) {
                return null;
            } else {
                return this.apikey.getText();
            }
        }

        private final ExtTextField                         name;
        private final ExtPasswordField                     pass;
        private final ExtPasswordField                     apikey;
        private final JLabel                               apikeyLabel;
        private final InputChangedCallbackInterface        callback;
        private JLabel                                     usernameLabel = null;
        private final JLabel                               passwordLabel;
        private final PluginForCaptchaSolverDeathByCaptcha plg;
        // Components for account type selection
        private final JComboBox                            accountTypeComboBox;
        private final JPanel                               authtokenLoginAccountPanel;
        private final JPanel                               userPassLoginAccountPanel;
        // Translations
        private final Map<String, String>                  translations;

        public boolean updateAccount(Account input, Account output) {
            boolean changed = false;
            if (!StringUtils.equals(input.getUser(), output.getUser())) {
                output.setUser(input.getUser());
                changed = true;
            }
            if (!StringUtils.equals(input.getPass(), output.getPass())) {
                output.setPass(input.getPass());
                changed = true;
            }
            return changed;
        }

        public DeathbycaptchaAccountFactory(final InputChangedCallbackInterface callback, final PluginForCaptchaSolverDeathByCaptcha plg) {
            super("ins 0, wrap 2", "[][grow,fill]", "");
            this.plg = plg;
            this.callback = callback;
            // Initialize internal translations with English fallback
            this.translations = getTranslations(System.getProperty("user.language"));
            final String apikey_help_url_without_protocol = plg.getAPILoginHelpURL().replaceFirst("^https?://", "");
            final String apikey_help_url = plg.getAPILoginHelpURL();
            // Add account type dropdown
            add(new JLabel(translations.get(LOGIN_METHOD)));
            /**
             * Important developer information: If you edit the list down below, also check/update methods setAccount, getAccount and
             * validateInputs
             */
            accountTypeComboBox = new JComboBox(new String[] { translations.get(LOGIN_METHOD_API), translations.get(LOGIN_METHOD_USER_PASSWORD) });
            /* Select API login as default value */
            accountTypeComboBox.setSelectedIndex(0);
            accountTypeComboBox.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    updateVisibleComponents();
                    callback.onChangedInput(accountTypeComboBox);
                }
            });
            add(accountTypeComboBox);
            // Create API key login panel
            authtokenLoginAccountPanel = new JPanel(new MigLayout("ins 0, wrap 2", "[][grow,fill]", ""));
            apikeyLabel = new JLink(translations.get(API_KEY_LABEL), apikey_help_url);
            authtokenLoginAccountPanel.add(apikeyLabel);
            this.apikey = new ExtPasswordField() {
                @Override
                public void onChanged() {
                    callback.onChangedInput(apikey);
                }
            };
            this.apikey.setHelpText(translations.get(OBTAIN_API_KEY) + apikey_help_url_without_protocol);
            authtokenLoginAccountPanel.add(this.apikey);
            // Create username/password login panel
            userPassLoginAccountPanel = new JPanel(new MigLayout("ins 0, wrap 2", "[][grow,fill]", ""));
            usernameLabel = new JLabel(_GUI.T.jd_gui_swing_components_AccountDialog_name());
            userPassLoginAccountPanel.add(usernameLabel);
            this.name = new ExtTextField() {
                @Override
                public void onChanged() {
                    callback.onChangedInput(name);
                }

                {
                    final HighlightPainter painter = new DefaultHighlighter.DefaultHighlightPainter(Color.yellow);
                    addTextHighlighter(new ExtTextHighlighter(painter, Pattern.compile("^(\\s+)")));
                    addTextHighlighter(new ExtTextHighlighter(painter, Pattern.compile("(\\s+)$")));
                    refreshTextHighlighter();
                }
            };
            name.setHelpText(_GUI.T.jd_gui_swing_components_AccountDialog_help_username());
            userPassLoginAccountPanel.add(name);
            // Password field
            passwordLabel = new JLabel(_GUI.T.jd_gui_swing_components_AccountDialog_pass());
            userPassLoginAccountPanel.add(passwordLabel);
            this.pass = new ExtPasswordField() {
                @Override
                public void onChanged() {
                    callback.onChangedInput(pass);
                }

                {
                    final HighlightPainter painter = new DefaultHighlighter.DefaultHighlightPainter(Color.yellow);
                    addTextHighlighter(new ExtTextHighlighter(painter, Pattern.compile("^(\\s+)")) {
                        public boolean highlight(javax.swing.text.Highlighter highlighter, CharSequence charSequence) {
                            if (Cookies.parseCookiesFromString(charSequence.toString()) != null) {
                                return false;
                            } else {
                                return super.highlight(highlighter, charSequence);
                            }
                        };
                    });
                    addTextHighlighter(new ExtTextHighlighter(painter, Pattern.compile("(\\s+)$")) {
                        public boolean highlight(javax.swing.text.Highlighter highlighter, CharSequence charSequence) {
                            if (Cookies.parseCookiesFromString(charSequence.toString()) != null) {
                                return false;
                            } else {
                                return super.highlight(highlighter, charSequence);
                            }
                        };
                    });
                    applyTextHighlighter(null);
                }
            };
            userPassLoginAccountPanel.add(pass);
            pass.setHelpText(_GUI.T.BuyAndAddPremiumAccount_layoutDialogContent_pass());
            // Set initial visibility
            updateVisibleComponents();
        }

        @Override
        public boolean handleClipboardAutoFill() {
            return DefaultEditAccountPanelAPIKeyLogin.handleClipboardAutoFill(apikey, name, plg);
        }

        /** Returns true if API login will be used based on the selected account type. */
        private boolean isAPILoginTypeSelected() {
            return accountTypeComboBox.getSelectedIndex() == 0;
        }

        private void updateVisibleComponents() {
            final boolean isAPILogin = isAPILoginTypeSelected();
            authtokenLoginAccountPanel.setVisible(isAPILogin);
            userPassLoginAccountPanel.setVisible(!isAPILogin);
            if (isAPILogin) {
                this.remove(userPassLoginAccountPanel);
                add(authtokenLoginAccountPanel, "span 2, grow");
            } else {
                this.remove(authtokenLoginAccountPanel);
                add(userPassLoginAccountPanel, "span 2, grow");
            }
            // Trigger layout update
            revalidate();
            repaint();
            // Notify parent container to update its layout
            Container parent = getParent();
            while (parent != null) {
                parent.revalidate();
                parent.repaint();
                parent = parent.getParent();
            }
        }

        public InputChangedCallbackInterface getCallback() {
            return callback;
        }

        public void setAccount(final Account defaultAccount) {
            if (defaultAccount == null) {
                /* This should never happen */
                return;
            }
            /* If user edits existing account ensure that GUI matches users' account type. */
            final Number loginType = (Number) defaultAccount.getProperty(PROPERTY_ACCOUNT_LOGIN_TYPE);
            if (loginType != null) {
                if (loginType.intValue() == ACCOUNT_LOGIN_TYPE_AUTHTOKEN) {
                    /* API token login */
                    apikey.setText(defaultAccount.getPass());
                    accountTypeComboBox.setSelectedIndex(0);
                } else {
                    /* Username and password login */
                    name.setText(defaultAccount.getUser());
                    pass.setText(defaultAccount.getPass());
                    accountTypeComboBox.setSelectedIndex(1);
                }
            }
            updateVisibleComponents();
        }

        @Override
        public boolean validateInputs() {
            if (isAPILoginTypeSelected()) {
                // API key login validation
                final String apikey = this.getApikey();
                if (plg.looksLikeValidAPIKey(apikey)) {
                    this.apikeyLabel.setForeground(Color.BLACK);
                    return true;
                } else {
                    this.apikeyLabel.setForeground(Color.RED);
                    return false;
                }
            } else {
                // Username and password login validation
                final boolean userok;
                final boolean passok;
                if (StringUtils.isEmpty(this.getUsername())) {
                    usernameLabel.setForeground(Color.RED);
                    userok = false;
                } else {
                    usernameLabel.setForeground(Color.BLACK);
                    userok = true;
                }
                final String pw = getPassword();
                if (StringUtils.isEmpty(pw)) {
                    /* Password field is never allowed to be empty/null. */
                    passok = false;
                } else {
                    passok = true;
                }
                if (!passok) {
                    passwordLabel.setForeground(Color.RED);
                } else {
                    passwordLabel.setForeground(Color.BLACK);
                }
                return userok && passok;
            }
        }

        @Override
        public Account getAccount() {
            if (isAPILoginTypeSelected()) {
                final String apikey = this.getApikey();
                if (plg.looksLikeValidAPIKey(apikey)) {
                    final Account account = new Account(getUsername(), apikey);
                    account.setProperty(PROPERTY_ACCOUNT_LOGIN_TYPE, ACCOUNT_LOGIN_TYPE_AUTHTOKEN);
                    return account;
                }
            }
            final Account account = new Account(getUsername(), getPassword());
            account.setProperty(PROPERTY_ACCOUNT_LOGIN_TYPE, ACCOUNT_LOGIN_TYPE_USER_AND_PASSWORD);
            return account;
        }

        @Override
        public JComponent getComponent() {
            return this;
        }
    }

    @Override
    public Class<? extends CaptchaSolverPluginConfigDeathbycaptcha> getConfigInterface() {
        return CaptchaSolverPluginConfigDeathbycaptcha.class;
    }
}