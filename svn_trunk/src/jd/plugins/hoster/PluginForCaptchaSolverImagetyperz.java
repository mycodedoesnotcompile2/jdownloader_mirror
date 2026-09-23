package jd.plugins.hoster;

import java.awt.Color;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Currency;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;

import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Highlighter.HighlightPainter;

import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtPasswordField;
import org.appwork.swing.components.ExtTextField;
import org.appwork.swing.components.ExtTextHighlighter;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver.FeedbackType;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.TokenCaptchaResponse;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.gui.InputChangedCallbackInterface;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.accounts.AccountBuilderInterface;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigImagetyperz;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.seamless.util.io.IO;

import jd.PluginWrapper;
import jd.gui.swing.components.linkbutton.JLink;
import jd.http.Browser;
import jd.http.Cookies;
import jd.http.Request;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.DefaultEditAccountPanelAPIKeyLogin;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import net.miginfocom.swing.MigLayout;

@HostPlugin(revision = "$Revision: 53488 $", interfaceVersion = 3, names = { "imagetyperz.com" }, urls = { "" })
public class PluginForCaptchaSolverImagetyperz extends abstractPluginForCaptchaSolver {
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION };
    }

    public PluginForCaptchaSolverImagetyperz(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium(getBuyPremiumUrl());
    }

    private static final String PROPERTY_ACCOUNT_LOGIN_TYPE          = "login_type";
    private static final int    ACCOUNT_LOGIN_TYPE_AUTHTOKEN         = 0;
    private static final int    ACCOUNT_LOGIN_TYPE_USER_AND_PASSWORD = 1;

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put("User-Agent", "JDownloader");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public String getBuyPremiumUrl() {
        return this.getBaseURL() + "/Forms/Registration.aspx";
    }

    @Override
    public List<FeedbackType> getSupportedFeedbackTypes() {
        final List<FeedbackType> types = new ArrayList<FeedbackType>();
        types.add(FeedbackType.REPORT_INVALID_CAPTCHAS);
        return types;
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        /** TODO: Test and add more captcha types, see their API docs. They even claim to support hCaptcha. */
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        // types.add(CAPTCHA_TYPE.CLOUDFLARE_TURNSTILE);
        // types.add(CAPTCHA_TYPE.HCAPTCHA);
        // types.add(CAPTCHA_TYPE.GEETEST_V1);
        // types.add(CAPTCHA_TYPE.GEETEST_V4);
        return types;
    }

    private String getBaseURL() {
        return "https://" + getHost();
    }

    protected String getApiBase() {
        /* API docs: https://www.imagetyperz.com/Forms/NewAPI.aspx */
        /**
         * Their API docs are documented in a chaotic way, here is a different version of them:
         * https://www.imagetyperz.com/Forms/api/api.html <br>
         * 2026-09-18: Changed API domain from imagetypers.com to captchatypers.com.
         */
        return "https://captchatypers.com";
    }

    @Override
    public String getAGBLink() {
        return getBaseURL() + "/Forms/terms-of-service.aspx";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        }
        return str.matches("[a-fA-F0-9]{32}");
    }

    @Override
    protected String getAPILoginHelpURL() {
        return getBaseURL() + "/Forms/NewClientHome.aspx";
    }

    @Override
    public AccountInfo fetchAccountInfo(Account account) throws Exception {
        final String username = account.getUser();
        final String password = account.getPass();
        /*
         * Determine which login type(s) to try. Each login type is tried at most once. If a login type has already been established for
         * this account, only that one is used; otherwise every applicable type is tried once (token first if the password looks like a
         * token).
         */
        final List<Integer> loginTypesToTry = getLoginTypesToTry(account, password);
        for (int i = 0; i < loginTypesToTry.size(); i++) {
            final boolean isLastTry = i == loginTypesToTry.size() - 1;
            final int currentLoginType = loginTypesToTry.get(i).intValue();
            final UrlQuery query = new UrlQuery(true);
            query.addAndReplace("action", "REQUESTBALANCE");
            final String path;
            if (currentLoginType == ACCOUNT_LOGIN_TYPE_AUTHTOKEN) {
                query.appendEncoded("token", password);
                path = "/Forms/RequestBalanceToken.ashx";
            } else {
                query.appendEncoded("username", account.getUser());
                query.appendEncoded("password", password);
                path = "/Forms/RequestBalance.ashx";
            }
            try {
                final Request req = br.createPostRequest(this.getApiBase() + path, query);
                callAPI(req);
                account.setProperty(PROPERTY_ACCOUNT_LOGIN_TYPE, currentLoginType);
                break;
            } catch (final PluginException pe) {
                if (isLastTry) {
                    throw pe;
                }
                /* Not the last candidate -> try the next login type. */
            }
        }
        /* REQUESTBALANCE returns the account balance in US dollars (NOT cents), see imagetyperz-api/API-docs. */
        final double balance = Double.parseDouble(br.getRequest().getHtmlCode().trim());
        final AccountInfo ai = new AccountInfo();
        ai.setAccountBalance(balance, Currency.getInstance("USD"));
        return ai;
    }

    @Override
    public void solve(CESSolverJob<?> job, Account account) throws Exception {
        final Challenge<?> c = job.getChallenge();
        job.setStatus(SolverStatus.UPLOADING);
        /*
         * Image captchas are solved synchronously (the upload response already contains the answer as "<id>|<solution>"), while reCAPTCHA
         * uploads only return an id that has to be polled afterwards.
         */
        boolean expectImmediateAnswer = false;
        String uploadPath = null;
        final UrlQuery uploadQuery = new UrlQuery();
        uploadQuery.addAndReplace("action", "UPLOADCAPTCHA");
        final UrlQuery pollingQuery = new UrlQuery();
        String pollingPath = "/captchaapi/GetRecaptchaTextToken.ashx";
        if (this.isLoginViaAuthtoken(account)) {
            uploadQuery.addAndReplace("token", account.getPass());
            pollingQuery.addAndReplace("token", account.getPass());
            uploadPath = "/captchaapi/UploadRecaptchaToken.ashx";
        } else {
            uploadQuery.addAndReplace("username", account.getUser());
            uploadQuery.addAndReplace("password", account.getPass());
            pollingQuery.addAndReplace("username", account.getUser());
            pollingQuery.addAndReplace("password", account.getPass());
            uploadPath = "/captchaapi/UploadRecaptchaV1.ashx";
        }
        final String type;
        if (c instanceof RecaptchaV2Challenge) {
            type = "reCaptcha";
            final RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) c;
            if (challenge.isEnterprise()) {
                uploadPath = "/captchaapi/UploadRecaptchaEnt.ashx";
            } else {
                /* For 'normal' reCaptcha captchas, API calls differ depending on login type */
                if (this.isLoginViaAuthtoken(account)) {
                    uploadPath = "/captchaapi/UploadRecaptchaToken.ashx";
                } else {
                    uploadPath = "/captchaapi/UploadRecaptchaV1.ashx";
                }
            }
            uploadQuery.addAndReplace("pageurl", challenge.getSiteDomain());
            uploadQuery.addAndReplace("googlekey", challenge.getSiteKey());
            if (challenge.isEnterprise()) {
                String enterprise_type = "v2";// default
                if (challenge.isInvisible()) {
                    enterprise_type = "v2";// invisible
                    uploadQuery.addAndReplace("isinvisible", "1");
                }
                if (challenge.isV3()) {
                    enterprise_type = "v3";// v3
                }
                if (challenge.getV3Action() != null) {
                    final String action = (String) challenge.getV3Action().get("action");
                    if (action != null) {
                        enterprise_type = "v3";// v3
                        uploadQuery.addAndReplace("captchaaction", action);
                    }
                }
                uploadQuery.addAndReplace("enterprise_type", enterprise_type);
            } else {
                /* recaptchatype: 1 = normal, 2 = invisible, 3 = v3 (see imagetyperz-api/API-docs). */
                if (challenge.isInvisible()) {
                    uploadQuery.addAndReplace("recaptchatype", "2");
                } else if (challenge.isV3()) {
                    uploadQuery.addAndReplace("recaptchatype", "3");
                } else {
                    uploadQuery.addAndReplace("recaptchatype", "1");
                }
            }
            if (challenge.getMinScore() != null) {
                uploadQuery.addAndReplace("score", Double.toString(challenge.getMinScore()));
            }
            /* Set polling params */
            if (this.isLoginViaAuthtoken(account)) {
                pollingPath = "/captchaapi/GetRecaptchaTextToken.ashx";
            } else {
                pollingPath = "/captchaapi/GetRecaptchaText.ashx";
            }
        } else if (c instanceof ImageCaptchaChallenge) {
            type = "Image";
            /* Image captchas are solved synchronously and the answer is returned directly in the upload response. */
            expectImmediateAnswer = true;
            if (this.isLoginViaAuthtoken(account)) {
                uploadPath = "/Forms/UploadFileAndGetTextNEWToken.ashx";
            } else {
                uploadPath = "/Forms/UploadFileAndGetTextNew.ashx";
            }
            final ImageCaptchaChallenge challenge = (ImageCaptchaChallenge) c;
            final byte[] data = IO.readBytes(challenge.getImageFile());
            uploadQuery.addAndReplace("file", Base64.encodeToString(data, false));
        } else {
            throw new IllegalArgumentException("Unexpected captcha challenge type");
        }
        this.callAPI(br.createPostRequest(this.getApiBase() + uploadPath, uploadQuery));
        String solution = null;
        final String captchaID;
        if (expectImmediateAnswer) {
            /* Image captcha: the response is "<captchaID>|<solution>", e.g. "123|polum". */
            final String responseText = br.getRequest().getHtmlCode();
            final String[] idAndSolution = responseText.split("\\|", 2);
            if (idAndSolution.length != 2) {
                throw new SolverException("Unexpected image captcha response: " + responseText);
            }
            captchaID = idAndSolution[0];
            solution = idAndSolution[1];
        } else {
            captchaID = br.getRequest().getHtmlCode();
            if (!captchaID.matches("\\d+")) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA, "Invalid captcha_id format or invalid response");
            }
            job.setStatus(SolverStatus.SOLVING);
            pollingQuery.addAndReplace("captchaID", captchaID);
            while (true) {
                Thread.sleep(getPollingIntervalMillis(account));
                this.callAPI(br.createPostRequest(this.getApiBase() + pollingPath, pollingQuery));
                if (!br.containsHTML("NOT_DECODED")) {
                    solution = br.getRequest().getHtmlCode();
                    break;
                }
                /* Not done yet -> Continue */
                checkInterruption();
            }
        }
        job.getLogger().info("CAPTCHA(" + type + ") solved: " + solution);
        AbstractResponse resp = null;
        if (c instanceof RecaptchaV2Challenge) {
            resp = new TokenCaptchaResponse((Challenge<String>) c, this, solution);
        } else {
            resp = new CaptchaResponse((Challenge<String>) c, this, solution);
        }
        resp.setCaptchaSolverTaskID(captchaID);
        job.setAnswer(resp);
        return;
    }

    @Override
    public boolean setInvalid(AbstractResponse<?> response, Account account) {
        /* API docs: https://www.imagetyperz.com/Forms/NewAPI.aspx */
        UrlQuery query = new UrlQuery(true);
        query.addAndReplace("action", "SETBADIMAGE");
        query.addAndReplace("imageid", response.getCaptchaSolverTaskID());
        final String path;
        if (this.isLoginViaAuthtoken(account)) {
            query = query.append("token", account.getPass(), true);
            path = "/Forms/SetBadImageToken.htm";
        } else {
            query.append("password", account.getPass(), true);
            query.append("username", account.getUser(), true);
            path = "/Forms/SetBadImage.ashx";
        }
        try {
            final Request req = br.createPostRequest(this.getApiBase() + path, query);
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

    /**
     * Returns the login types to try, in order. A login type that has already been established for this account is used exclusively;
     * otherwise every applicable type is returned once (token login first, but only if the password looks like a token).
     */
    private List<Integer> getLoginTypesToTry(final Account account, final String password) {
        final Number storedLoginType = (Number) account.getProperty(PROPERTY_ACCOUNT_LOGIN_TYPE);
        final List<Integer> ret = new ArrayList<Integer>();
        if (storedLoginType != null) {
            ret.add(Integer.valueOf(storedLoginType.intValue()));
            return ret;
        }
        if (this.looksLikeValidAPIKey(password)) {
            ret.add(Integer.valueOf(ACCOUNT_LOGIN_TYPE_AUTHTOKEN));
        }
        ret.add(Integer.valueOf(ACCOUNT_LOGIN_TYPE_USER_AND_PASSWORD));
        return ret;
    }

    /**
     * Maps the API error tokens (returned as "ERROR: &lt;TOKEN&gt;", see https://www.imagetyperz.com/Forms/NewAPI.aspx) to human readable
     * messages. Keys are upper-case and without the "ERROR:" prefix.
     */
    private static final Map<String, String> ERROR_MESSAGES;
    static {
        final Map<String, String> map = new HashMap<String, String>();
        map.put("INVALID_REQUEST", "Invalid request sent to the captcha service.");
        map.put("INVALID_USERNAME", "No username was provided.");
        map.put("INVALID_PASSWORD", "No password was provided.");
        map.put("INVALID_AFFILIATEID", "No affiliate ID was provided.");
        map.put("INVALID_IMAGE_FILE", "No file was uploaded, or the uploaded file is not an image.");
        map.put("AUTHENTICATION_FAILED", "The provided username and password are invalid.");
        map.put("INVALID_IMAGE_SIZE_30_KB", "The uploaded image file must not be larger than 30 KB.");
        map.put("UNKNOWN", "Unknown error at the captcha service. Please try again.");
        map.put("NOT_DECODED", "The captcha has not been solved yet.");
        map.put("INVALID_TOKEN", "No API token was provided, or the token is invalid.");
        map.put("INVALID_IMAGE_ID", "No image ID was provided, or the provided image ID is invalid.");
        ERROR_MESSAGES = Collections.unmodifiableMap(map);
    }

    /** Returns the human readable message for an API error token, or a generic fallback containing the raw token when unknown. */
    private static String getHumanReadableErrorMessage(final String errorToken) {
        final String message = ERROR_MESSAGES.get(errorToken.toUpperCase(Locale.ENGLISH));
        if (message != null) {
            return message;
        }
        return "Captcha service error: " + errorToken;
    }

    /** True for error tokens that indicate an account/credential problem (should invalidate the account). */
    private static boolean isAccountError(final String errorToken) {
        final String token = errorToken.toUpperCase(Locale.ENGLISH);
        return token.equals("AUTHENTICATION_FAILED") || token.equals("INVALID_USERNAME") || token.equals("INVALID_PASSWORD") || token.equals("INVALID_TOKEN");
    }

    private void callAPI(final Request req) throws Exception {
        br.getPage(req);
        String error = br.getRegex("^ERROR:(.+)").getMatch(0);
        if (error == null) {
            /* No error */
            return;
        }
        error = error.trim();
        if (error.equalsIgnoreCase("NOT_DECODED")) {
            /*
             * Not really an error: the captcha is still being processed. The poll loop keeps retrying while the response contains
             * "NOT_DECODED".
             */
            return;
        }
        final String message = getHumanReadableErrorMessage(error);
        /* Credential/authentication related errors -> mark the account as invalid. */
        if (this.getPluginEnvironment() == PluginEnvironment.ACCOUNT_CHECK || isAccountError(error)) {
            throw new AccountInvalidException(message);
        }
        throw new SolverException(message);
    }

    @Override
    public AccountBuilderInterface getAccountFactory(final InputChangedCallbackInterface callback) {
        return new ImagetyperzAccountFactory(callback, this);
    }

    public static class ImagetyperzAccountFactory extends MigPanel implements AccountBuilderInterface {
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

        private final ExtTextField                      name;
        private final ExtPasswordField                  pass;
        private final ExtPasswordField                  apikey;
        private final JLabel                            apikeyLabel;
        private final InputChangedCallbackInterface     callback;
        private JLabel                                  usernameLabel = null;
        private final JLabel                            passwordLabel;
        private final PluginForCaptchaSolverImagetyperz plg;
        // Components for account type selection
        private final JComboBox                         accountTypeComboBox;
        private final JPanel                            authtokenLoginAccountPanel;
        private final JPanel                            userPassLoginAccountPanel;
        // Translations
        private final Map<String, String>               translations;

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

        public ImagetyperzAccountFactory(final InputChangedCallbackInterface callback, final PluginForCaptchaSolverImagetyperz plg) {
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
    public Class<? extends CaptchaSolverPluginConfigImagetyperz> getConfigInterface() {
        return CaptchaSolverPluginConfigImagetyperz.class;
    }
}