package org.jdownloader.plugins.components.google;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map.Entry;

import javax.swing.JComponent;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;

import org.appwork.swing.components.ExtTextField;
import org.appwork.swing.components.TextComponentInterface;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.InputDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.parser.UrlQuery;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.InputDialog;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.components.config.GoogleConfig;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.translate._JDT;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import jd.controlling.accountchecker.AccountCheckerThread;
import jd.http.Browser;
import jd.http.Cookie;
import jd.http.Cookies;
import jd.nutils.JDHash;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.parser.html.InputField;
import jd.plugins.Account;
import jd.plugins.AccountInvalidException;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.components.GoogleService;

public class GoogleHelper {
    // private static final String COOKIES2 = "googleComCookies";
    private static final String META_HTTP_EQUIV_REFRESH_CONTENT_D_S_URL_39_39 = "<meta\\s+http-equiv=\"refresh\"\\s+content\\s*=\\s*\"(\\d+)\\s*;\\s*url\\s*=\\s*([^\"]+)";
    private static final String PROPERTY_ACCOUNT_user_agent                   = "user_agent";
    private Browser             br;
    private boolean             cacheEnabled                                  = true;

    public void setCacheEnabled(boolean cacheEnabled) {
        this.cacheEnabled = cacheEnabled;
    }

    LogInterface logger = null;

    public LogInterface getLogger() {
        return logger;
    }

    public void setLogger(final LogInterface logger) {
        this.logger = logger;
    }

    private void log(String str) {
        LogInterface logger = getLogger();
        if (logger != null) {
            logger.info(str);
        }
    }

    public GoogleHelper(final Browser browser) {
        init(browser, null);
    }

    public GoogleHelper(final Browser browser, final LogInterface logger) {
        init(browser, logger);
    }

    private void init(final Browser browser, final LogInterface logger) {
        this.br = browser;
        Thread thread = Thread.currentThread();
        boolean forceUpdateAndBypassCache = thread instanceof AccountCheckerThread && ((AccountCheckerThread) thread).getJob().isForce();
        cacheEnabled = !forceUpdateAndBypassCache;
        this.logger = logger;
    }

    private void postPageFollowRedirects(Browser br, String url, UrlQuery post) throws IOException, InterruptedException {
        boolean before = br.isFollowingRedirects();
        br.setFollowRedirects(false);
        int wait = 0;
        try {
            log("Google Login: POST " + url + " Data: " + post);
            br.postPage(url, post);
            url = null;
            if (br.getRedirectLocation() != null) {
                url = br.getRedirectLocation();
            }
            String[] redirect = br.getRegex(META_HTTP_EQUIV_REFRESH_CONTENT_D_S_URL_39_39).getRow(0);
            if (redirect != null) {
                url = Encoding.htmlDecode(redirect[1]);
                wait = Integer.parseInt(redirect[0]) * 1000;
            }
        } finally {
            br.setFollowRedirects(before);
        }
        if (url != null) {
            if (wait > 0) {
                Thread.sleep(wait);
            }
            getPageFollowRedirects(br, url);
        }
    }

    private void getPageFollowRedirects(Browser br, String url) throws IOException, InterruptedException {
        boolean before = br.isFollowingRedirects();
        br.setFollowRedirects(false);
        try {
            int max = 20;
            int wait = 0;
            while (max-- > 0) {
                url = breakRedirects(url);
                if (url == null) {
                    break;
                }
                if (wait > 0) {
                    Thread.sleep(wait);
                }
                log("Google Login: GET " + url);
                br.getPage(url);
                url = null;
                if (br.getRedirectLocation() != null) {
                    url = br.getRedirectLocation();
                    continue;
                }
                String[] redirect = br.getRegex(META_HTTP_EQUIV_REFRESH_CONTENT_D_S_URL_39_39).getRow(0);
                if (redirect != null) {
                    url = Encoding.htmlDecode(redirect[1]);
                    wait = Integer.parseInt(redirect[0]) * 1000;
                }
            }
        } finally {
            br.setFollowRedirects(before);
        }
    }

    protected String breakRedirects(String url) throws MalformedURLException, IOException {
        if (StringUtils.isEmpty(url) || new URL(url).getHost().toLowerCase(Locale.ENGLISH).contains(getService().serviceName)) {
            return null;
        }
        return url;
    }

    private Browser prepBR(final Browser br) {
        br.setCookie("https://google.com", "PREF", "hl=en-GB");
        return br;
    }

    public static String getUserAgent() {
        final String cfgUserAgent = PluginJsonConfig.get(GoogleConfig.class).getUserAgent();
        if (StringUtils.isEmpty(cfgUserAgent) || cfgUserAgent.equalsIgnoreCase("JDDEFAULT")) {
            /* Return default */
            /*
             * 2020-06-19: Firefox Users will get their User-Agent via "Flag Cookies" addon on cookie import but Opera & Chrome users won't
             * which is why we'll use a Chrome User-Agent as default
             */
            /* Last updated: 2020-06-19 */
            return "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.106 Safari/537.36";
        } else {
            /* Return user selection */
            return cfgUserAgent;
        }
    }

    private Thread showCookieLoginInformation(final String host) {
        final String serviceNameForUser;
        final String realhost;
        if (host.equals("youtube.com")) {
            realhost = "youtube.com";
            serviceNameForUser = "YouTube";
        } else if (host.equals("drive.google.com")) {
            /* For GoogleDrive downloads */
            realhost = "drive.google.com";
            serviceNameForUser = "Google Drive";
        } else {
            realhost = "google.com";
            serviceNameForUser = "Google";
        }
        final Thread thread = new Thread() {
            public void run() {
                try {
                    final String help_article_url = "https://support.jdownloader.org/Knowledgebase/Article/View/account-cookie-login-instructions";
                    String message = "";
                    final String title;
                    if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                        title = serviceNameForUser + " - Login";
                        message += "Hallo liebe(r) " + serviceNameForUser + " NutzerIn\r\n";
                        message += "Um deinen " + serviceNameForUser + " Account in JDownloader verwenden zu können, musst du folgende Schritte beachten:\r\n";
                        message += "Öffne " + realhost + " in deinem Browser und folge dieser Anleitung:\r\n";
                        message += help_article_url;
                    } else {
                        title = serviceNameForUser + " - Login";
                        message += "Hello dear " + serviceNameForUser + " user\r\n";
                        message += "Open " + realhost + " in your browser and follow these instructions:\r\n";
                        message += help_article_url;
                    }
                    final ConfirmDialog dialog = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN, title, message);
                    dialog.setTimeout(3 * 60 * 1000);
                    if (CrossSystem.isOpenBrowserSupported() && !Application.isHeadless()) {
                        CrossSystem.openURL(help_article_url);
                    }
                    final ConfirmDialogInterface ret = UIOManager.I().show(ConfirmDialogInterface.class, dialog);
                    ret.throwCloseExceptions();
                } catch (final Throwable e) {
                    getLogger().log(e);
                }
            };
        };
        thread.setDaemon(true);
        thread.start();
        return thread;
    }

    public void login(final Account account, final boolean forceLoginValidation) throws Exception {
        synchronized (account) {
            /*
             * User-Agent handling (by priority): Prefer last saved User-Agent given via user cookies --> "Fallback to" User-Agent from user
             * given cookies --> Fallback to User defined User-Agent via plugin setting
             */
            final String userDefinedUserAgent = getUserAgent();
            this.br.setDebug(true);
            this.br.setCookiesExclusive(true);
            /* TODO: Do we still need this? */
            this.br.setCookie("https://google.com", "PREF", "hl=en-GB");
            final Cookies userCookies = account.loadUserCookies();
            if (userCookies == null) {
                showCookieLoginInformation(account.getHoster());
                throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_required());
            }
            /* Check stored cookies */
            logger.info("Attempting to perform login with user cookies");
            br.setCookies(userCookies);
            /* No User-Agent given in users' cookies? Add User selected User-Agent */
            if (!StringUtils.isEmpty(userCookies.getUserAgent())) {
                logger.info("Using User-Agent given in user cookies: " + userCookies.getUserAgent());
                /* Save User-Agent so it gets re-used next time */
                account.setProperty(PROPERTY_ACCOUNT_user_agent, userCookies.getUserAgent());
                /* No need to do this - User-Agent is already set above via setCookies! */
                // br.getHeaders().put("User-Agent", userCookies.getUserAgent());
            } else {
                logger.info("Using user defined User-Agent: " + userDefinedUserAgent);
                br.getHeaders().put("User-Agent", userDefinedUserAgent);
            }
            if (isCacheEnabled() && hasBeenValidatedRecently(account) && !forceLoginValidation) {
                logger.info("Trust cookies without check");
                return;
            }
            br.setAllowedResponseCodes(new int[] { 400 });
            if (!validateCookies(account)) {
                logger.info("Login with stored cookies failed");
                /* Give up. We only got these cookies so login via username and password is not possible! */
                logger.info("Login failed --> No password available but only cookies --> Give up");
                account.removeProperty(PROPERTY_ACCOUNT_user_agent);
                errorAccountInvalid(account);
            }
            logger.info("Login with cookies successful");
            /* Double-check */
            try {
                getSAPISidHash(br, br.getURL());
            } catch (final PluginException e) {
                /* If this happens, we cannot perform authed requests with this account so effectively the login process is not complete. */
                throw new AccountInvalidException("Login incomplete ('SAPISID' cookie missing?), try again with fresh cookies");
            }
            validate(account);
            /* Only store cookies if username via username + password has been used. */
            displayAdditionalCookieLoginInformation(account);
            return;
        }
    }

    private final String PROPERTY_HAS_SHOWN_ADDITIONAL_COOKIE_LOGIN_INFORMATION = "has_shown_additional_cookie_login_information";

    private void displayAdditionalCookieLoginInformation(final Account account) {
        if (account.getBooleanProperty(PROPERTY_HAS_SHOWN_ADDITIONAL_COOKIE_LOGIN_INFORMATION, false)) {
            return;
        }
        final Thread thread = new Thread() {
            public void run() {
                try {
                    String message = "";
                    final String title;
                    String language = System.getProperty("user.language").toLowerCase();
                    if ("de".equals(language)) {
                        title = "Google Login - wichtige Information zum Login";
                        message += "<html><b>! ! ! WICHTIGE INFORMATION ZUM GOOGLE LOGIN ! ! !</b><br><br>";
                        message += "<ol>";
                        message += "<li>Für die Nutzung von Google-Diensten raten wir davon ab, den Haupt-Account zu verwenden.</li>";
                        message += "<li>Login Information: Wir empfehlen, die Session/Cookies, die du gerade in JDownloader eingegeben hast, nicht weiter im Browser zu verwenden.</li>";
                        message += "</ol>";
                        message += "Solltest du diesen Hinweis missachten, werden deine Google-Cookies in JDownloader höchstwahrscheinlich nach sehr kurzer Zeit ungültig.<br><br>";
                        message += "Wir empfehlen, <b>vor dem Export der Cookies eine neue Session zu eröffnen</b>, die danach nicht mehr im Browser verwendet wird,<br>" + "z.&nbsp;B. durch Login in einem Inkognitofenster oder in einem separaten Browserprofil.</html>";
                    } else if ("es".equals(language)) {
                        title = "Google Login - información importante sobre el inicio de sesión";
                        message += "<html><b>! ! ! INFORMACIÓN IMPORTANTE SOBRE EL INICIO DE SESIÓN DE GOOGLE ! ! !</b><br><br>";
                        message += "<ol>";
                        message += "<li>Para evitar bloqueos de cuenta, recomendamos no usar la cuenta principal de Google para servicios de Google en JDownloader.</li>";
                        message += "<li>Información de inicio de sesión: Recomendamos no usar la misma sesión/cookies de Google que acaba de ingresar en JDownloader también en su navegador.</li>";
                        message += "</ol>";
                        message += "Si ignora esta advertencia, las cookies de Google en JDownloader probablemente caducarán en muy poco tiempo.<br><br>";
                        message += "Recomendamos <b>crear una nueva sesión antes de exportar las cookies</b>, que después no vuelva a usar en el navegador,<br>" + "por ejemplo, iniciando sesión en una ventana de incógnito o en un perfil de navegador separado.</html>";
                    } else if ("fr".equals(language)) {
                        title = "Google Login - informations importantes concernant la connexion";
                        message += "<html><b>! ! ! INFORMATIONS IMPORTANTES SUR LA CONNEXION GOOGLE ! ! !</b><br><br>";
                        message += "<ol>";
                        message += "<li>Pour l’utilisation des services Google, nous recommandons de ne pas utiliser votre compte principal.</li>";
                        message += "<li>Informations de connexion : nous recommandons de ne pas utiliser dans votre navigateur la même session/cookies Google que vous venez d’entrer dans JDownloader.</li>";
                        message += "</ol>";
                        message += "Si vous ignorez cet avertissement, vos cookies Google dans JDownloader deviendront probablement invalides très rapidement.<br><br>";
                        message += "Nous recommandons de <b>créer une nouvelle session avant d’exporter les cookies</b>, qui ne sera ensuite plus utilisée dans le navigateur,<br>" + "par exemple via une fenêtre de navigation privée ou un profil de navigateur séparé.</html>";
                    } else {
                        title = "Google Login - important login information";
                        message += "<html><b>! ! ! IMPORTANT INFORMATION ABOUT GOOGLE LOGIN ! ! !</b><br><br>";
                        message += "<ol>";
                        message += "<li>For using Google services, we strongly advise not to use your main account.</li>";
                        message += "<li>Login information: We recommend not to use the same Google session/cookies you just entered in JDownloader in your browser.</li>";
                        message += "</ol>";
                        message += "If you ignore this warning, your Google cookies in JDownloader will most likely become invalid after a very short time.<br><br>";
                        message += "We recommend <b>creating a new session before exporting cookies</b>, which should not be used in the browser afterwards,<br>" + "e.g. by logging in via an incognito window or a separate browser profile.</html>";
                    }
                    final ConfirmDialog dialog = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN | UIOManager.BUTTONS_HIDE_CANCEL | Dialog.STYLE_HTML, title, message);
                    dialog.setTimeout(300 * 1000);
                    final ConfirmDialogInterface ret = UIOManager.I().show(ConfirmDialogInterface.class, dialog);
                    ret.throwCloseExceptions();
                } catch (final Throwable e) {
                    getLogger().log(e);
                }
            };
        };
        thread.setDaemon(true);
        thread.start();
        account.setProperty(PROPERTY_HAS_SHOWN_ADDITIONAL_COOKIE_LOGIN_INFORMATION, true);
    }

    public static void errorAccountInvalid(final Account account) throws AccountInvalidException {
        final Cookies userCookies = account.loadUserCookies();
        if (userCookies != null && !userCookies.isEmpty()) {
            account.removeProperty(PROPERTY_ACCOUNT_user_agent);
            if (account.hasEverBeenValid()) {
                throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
            } else {
                throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
            }
        } else {
            throw new AccountInvalidException();
        }
    }

    public boolean validateCookies(final Account account) throws IOException, InterruptedException, AccountInvalidException {
        logger.info("Validating cookies");
        /*
         * 2020-09-07: psp: I was unable to just use the google.com cookies for youtube so basically we now expect the user to import the
         * correct cookies for the service they want to use so usually either "google.com" or "youtube.com" coookies.
         */
        if (account.getHoster().equals("youtube.com")) {
            br.getPage("https://www.youtube.com/");
            br.followRedirect(true);
            boolean ret = br.containsHTML("\"key\"\\s*:\\s*\"logged_in\"\\s*,\\s*\"value\"\\s*:\\s*\"1\"");
            ret |= br.containsHTML("\"LOGGED_IN\"\\s*:\\s*true");
            return ret;
        } else if (account.getHoster().equals("drive.google.com")) {
            validateCookiesGoogleDrive(br, account);
            return true;
        } else {
            final boolean useTwoLoginValidations = false;
            if (useTwoLoginValidations) {
                /* Old check */
                getPageFollowRedirects(br, "https://accounts.google.com/CheckCookie?hl=en&checkedDomains=" + Encoding.urlEncode(getService().serviceName) + "&checkConnection=" + Encoding.urlEncode(getService().checkConnectionString) + "&pstMsg=1&chtml=LoginDoneHtml&service=" + Encoding.urlEncode(getService().serviceName) + "&continue=" + Encoding.urlEncode(getService().continueAfterCheckCookie) + "&gidl=CAA");
                if (validateSuccessOLD()) {
                    return true;
                } else {
                    logger.info("First cookie validation failed --> 2nd validation ...");
                    getPageFollowRedirects(br, "https://www.google.com/?gws_rd=ssl");
                    if (br.containsHTML("accounts\\.google\\.com/logout")) {
                        return true;
                    }
                }
            } else {
                /* New check */
                getPageFollowRedirects(br, "https://www.google.com/?gws_rd=ssl");
                if (br.containsHTML("accounts\\.google\\.com/logout")) {
                    return true;
                }
            }
            return false;
        }
    }

    /**
     *
     * @throws AccountInvalidException
     * @throws IOException
     */
    private void validateCookiesGoogleDrive(final Browser br, final Account account) throws AccountInvalidException, IOException {
        final boolean oldFollowRedirects = br.isFollowingRedirects();
        try {
            br.setFollowRedirects(true);
            /** Important! Some URLs will redirect to accounts.google.com when not logged in or login cookies expired, others will not. */
            // br.getPage("https://drive.google.com/");
            br.getPage("https://drive.google.com/drive/my-drive");
            if (br.getURL().contains("accounts.google.com")) {
                logger.warning("Looks like GDrive login failed");
                GoogleHelper.errorAccountInvalid(account);
            } else {
                logger.info("Looks like GDrive login was successful");
            }
        } finally {
            br.setFollowRedirects(oldFollowRedirects);
        }
    }

    /**
     * Validates login via e.g.
     * https://accounts.google.com/CheckCookie?hl=en&checkedDomains=youtube&checkConnection=youtube%3A210%3A1&pstMsg
     * =1&chtml=LoginDoneHtml&service=youtube&continue=https%3A%2F%2Fwww.youtube.com%2Fsignin%3Faction_handle_signin%3Dtrue&gidl=CAA
     */
    protected boolean validateSuccessOLD() {
        return br.containsHTML("accounts/SetSID");
    }

    protected void validate(Account account) {
        account.setProperty("LAST_VALIDATE_" + getService().name(), System.currentTimeMillis());
    }

    protected boolean hasBeenValidatedRecently(Account account) {
        long lastValidated = account.getLongProperty("LAST_VALIDATE_" + getService().name(), -1);
        if (lastValidated > 0 && System.currentTimeMillis() - lastValidated < getValidatedCacheTimeout()) {
            return true;
        }
        return false;
    }

    protected long getValidatedCacheTimeout() {
        return 1 * 60 * 60 * 1000l;
    }

    private void handle2FactorAuthSmsDeprecated(Form form) throws Exception {
        // //*[@id="verifyText"]
        if (br.containsHTML("idv-delivery-error-container")) {
            // <div class="infobanner">
            // <p class="error-msg infobanner-content"
            // id="idv-delivery-error-container">
            // You seem to be having trouble getting your verification code.
            // Please try again later.
            // </p>
            // </div>
            throw new PluginException(LinkStatus.ERROR_PREMIUM, "You seem to be having trouble getting your sms verification code.  Please try again later.");
        }
        String number = br.getRegex("<span\\s+class\\s*=\\s*\"twostepphone\".*?>(.*?)</span>").getMatch(0);
        if (number != null) {
            InputDialog d = new InputDialog(0, _JDT.T.Google_helper_2factor_sms_dialog_title(), _JDT.T.Google_helper_2factor_sms_dialog_msg(number.trim()), null, new AbstractIcon(IconKey.ICON_TEXT, 32), null, null);
            InputDialogInterface handler = UIOManager.I().show(InputDialogInterface.class, d);
            handler.throwCloseExceptions();
            InputField smsUserPin = form.getInputFieldByName("smsUserPin");
            smsUserPin.setValue(Encoding.urlEncode(handler.getText()));
            InputField persistentCookie = form.getInputFieldByName("PersistentCookie");
            persistentCookie.setValue(Encoding.urlEncode("on"));
            form.remove("smsSend");
            form.remove("retry");
            submitForm(br, form);
        } else {
            // new version implemented on 31th july 2015
            number = br.getRegex("<span\\s+class\\s*=\\s*\"twostepphone\".*?>(.*?)</span>").getMatch(0);
            InputDialog d = new InputDialog(0, _JDT.T.Google_helper_2factor_sms_dialog_title(), _JDT.T.Google_helper_2factor_sms_dialog_msg(number.trim()), null, new AbstractIcon(IconKey.ICON_TEXT, 32), null, null);
            InputDialogInterface handler = UIOManager.I().show(InputDialogInterface.class, d);
            handler.throwCloseExceptions();
            InputField smsUserPin = form.getInputFieldByName("smsUserPin");
            smsUserPin.setValue(Encoding.urlEncode(handler.getText()));
            InputField persistentCookie = form.getInputFieldByName("PersistentCookie");
            persistentCookie.setValue(Encoding.urlEncode("on"));
            form.remove("smsSend");
            form.remove("retry");
            submitForm(br, form);
        }
        handleIntersitial();
    }

    private void handle2FactorAuthSmsNew2(Form form) throws Exception {
        // //*[@id="verifyText"]
        if (br.containsHTML("idv-delivery-error-container")) {
            // <div class="infobanner">
            // <p class="error-msg infobanner-content"
            // id="idv-delivery-error-container">
            // You seem to be having trouble getting your verification code.
            // Please try again later.
            // </p>
            // </div>
            throw new PluginException(LinkStatus.ERROR_PREMIUM, "You seem to be having trouble getting your sms verification code.  Please try again later.");
        }
        final String number = getPhonenumberCensored();
        InputDialog d = new InputDialog(0, _JDT.T.Google_helper_2factor_sms_dialog_title(), _JDT.T.Google_helper_2factor_sms_dialog_msg(number), null, new AbstractIcon(IconKey.ICON_TEXT, 32), null, null) {
            @Override
            protected void initFocus(JComponent focus) {
                // super.initFocus(focus);
            }

            @Override
            protected TextComponentInterface getSmallInputComponent() {
                final ExtTextField ttx = new ExtTextField();
                final String TEXT_NOT_TO_TOUCH = "G-";
                ttx.addKeyListener(this);
                ttx.addMouseListener(this);
                ttx.setText(TEXT_NOT_TO_TOUCH);
                ((AbstractDocument) ttx.getDocument()).setDocumentFilter(new DocumentFilter() {
                    @Override
                    public void insertString(FilterBypass fb, int offset, String string, AttributeSet attr) throws BadLocationException {
                        if (offset < TEXT_NOT_TO_TOUCH.length()) {
                            return;
                        }
                        super.insertString(fb, offset, string, attr);
                    }

                    @Override
                    public void replace(FilterBypass fb, int offset, int length, String text, AttributeSet attrs) throws BadLocationException {
                        if (offset < TEXT_NOT_TO_TOUCH.length()) {
                            length = Math.max(0, length - TEXT_NOT_TO_TOUCH.length());
                            offset = TEXT_NOT_TO_TOUCH.length();
                        }
                        super.replace(fb, offset, length, text, attrs);
                    }

                    @Override
                    public void remove(FilterBypass fb, int offset, int length) throws BadLocationException {
                        if (offset < TEXT_NOT_TO_TOUCH.length()) {
                            length = Math.max(0, length + offset - TEXT_NOT_TO_TOUCH.length());
                            offset = TEXT_NOT_TO_TOUCH.length();
                        }
                        if (length > 0) {
                            super.remove(fb, offset, length);
                        }
                    }
                });
                return ttx;
            }
        };
        InputDialogInterface handler = UIOManager.I().show(InputDialogInterface.class, d);
        handler.throwCloseExceptions();
        InputField smsUserPin = form.getInputFieldByName("Pin");
        String txt = handler.getText();
        if (txt.startsWith("G-")) {
            txt = txt.substring(2);
        }
        smsUserPin.setValue(Encoding.urlEncode(txt));
        InputField persistentCookie = form.getInputFieldByName("TrustDevice");
        persistentCookie.setValue(Encoding.urlEncode("on"));
        form.remove("smsSend");
        form.remove("retry");
        submitForm(br, form);
        handleIntersitial();
    }

    private void handle2FactorAuthSmsNew(Form form) throws Exception {
        // //*[@id="verifyText"]
        if (br.containsHTML("idv-delivery-error-container")) {
            // <div class="infobanner">
            // <p class="error-msg infobanner-content"
            // id="idv-delivery-error-container">
            // You seem to be having trouble getting your verification code.
            // Please try again later.
            // </p>
            // </div>
            throw new PluginException(LinkStatus.ERROR_PREMIUM, "You seem to be having trouble getting your sms verification code.  Please try again later.");
        }
        final String number = getPhonenumberCensored();
        InputDialog d = new InputDialog(0, _JDT.T.Google_helper_2factor_sms_dialog_title(), _JDT.T.Google_helper_2factor_sms_dialog_msg(number), null, new AbstractIcon(IconKey.ICON_TEXT, 32), null, null) {
            @Override
            protected void initFocus(JComponent focus) {
                // super.initFocus(focus);
            }

            @Override
            protected TextComponentInterface getSmallInputComponent() {
                final ExtTextField ttx = new ExtTextField();
                final String TEXT_NOT_TO_TOUCH = "G-";
                ttx.addKeyListener(this);
                ttx.addMouseListener(this);
                ttx.setText(TEXT_NOT_TO_TOUCH);
                ((AbstractDocument) ttx.getDocument()).setDocumentFilter(new DocumentFilter() {
                    @Override
                    public void insertString(FilterBypass fb, int offset, String string, AttributeSet attr) throws BadLocationException {
                        if (offset < TEXT_NOT_TO_TOUCH.length()) {
                            return;
                        }
                        super.insertString(fb, offset, string, attr);
                    }

                    @Override
                    public void replace(FilterBypass fb, int offset, int length, String text, AttributeSet attrs) throws BadLocationException {
                        if (offset < TEXT_NOT_TO_TOUCH.length()) {
                            length = Math.max(0, length - TEXT_NOT_TO_TOUCH.length());
                            offset = TEXT_NOT_TO_TOUCH.length();
                        }
                        super.replace(fb, offset, length, text, attrs);
                    }

                    @Override
                    public void remove(FilterBypass fb, int offset, int length) throws BadLocationException {
                        if (offset < TEXT_NOT_TO_TOUCH.length()) {
                            length = Math.max(0, length + offset - TEXT_NOT_TO_TOUCH.length());
                            offset = TEXT_NOT_TO_TOUCH.length();
                        }
                        if (length > 0) {
                            super.remove(fb, offset, length);
                        }
                    }
                });
                return ttx;
            }
        };
        InputDialogInterface handler = UIOManager.I().show(InputDialogInterface.class, d);
        handler.throwCloseExceptions();
        String txt = handler.getText();
        if (txt.startsWith("G-")) {
            txt = txt.substring(2);
        }
        form.remove("pin");
        form.put("pin", Encoding.urlEncode(txt));
        final InputField persistentCookie = form.getInputFieldByName("TrustDevice");
        if (persistentCookie != null) {
            persistentCookie.setValue(Encoding.urlEncode("on"));
        } else {
            form.put("TrustDevice", "on");
        }
        form.remove("smsSend");
        form.remove("retry");
        submitForm(br, form);
        handleIntersitial();
    }

    private String getPhonenumberCensored() {
        String number = br.getRegex("<b dir=\"ltr\">(.*?)</b>").getMatch(0);
        if (number == null) {
            number = this.br.getRegex("<b dir=\"ltr\" class=\"[^<>\"]+\">([^<>\"]*?)</b>").getMatch(0);
        }
        if (number != null) {
            number = number.trim();
        }
        return number;
    }

    protected void handleIntersitial() throws Exception {
        // Form[] forms = br.getForms();
        Form remind = br.getFormBySubmitvalue("Remind+me+later");
        if (remind != null && "SmsAuthInterstitial".equals(remind.getAction())) {
            remind.remove("addBackupPhone");
            submitForm(br, remind);
        }
    }

    private void submitForm(Browser br, Form form) throws Exception {
        boolean before = br.isFollowingRedirects();
        br.setFollowRedirects(false);
        int wait = 0;
        String url = null;
        try {
            br.submitForm(form);
            if (br.getRedirectLocation() != null) {
                url = br.getRedirectLocation();
            }
            String[] redirect = br.getRegex(META_HTTP_EQUIV_REFRESH_CONTENT_D_S_URL_39_39).getRow(0);
            if (redirect != null) {
                url = Encoding.htmlDecode(redirect[1]);
                wait = Integer.parseInt(redirect[0]) * 1000;
            }
        } finally {
            br.setFollowRedirects(before);
        }
        if (url != null) {
            if (wait > 0) {
                Thread.sleep(wait);
            }
            getPageFollowRedirects(br, url);
        }
    }

    private String getText(Document doc, XPath xPath, String string) throws XPathExpressionException {
        Node n = (Node) xPath.evaluate(string, doc, XPathConstants.NODE);
        return (n != null ? n.getFirstChild().getTextContent().trim() : null);
    }

    private GoogleService service = GoogleService.YOUTUBE;

    public GoogleService getService() {
        return service;
    }

    public void setService(GoogleService service) {
        this.service = service;
    }

    private boolean isCacheEnabled() {
        return cacheEnabled;
    }
    // public void followRedirect() throws IOException, InterruptedException {
    // int wait = 0;
    // String url = null;
    // if (br.getRedirectLocation() != null) {
    // url = br.getRedirectLocation();
    //
    // }
    //
    // String[] redirect = br.getRegex(META_HTTP_EQUIV_REFRESH_CONTENT_D_S_URL_39_39).getRow(0);
    // if (redirect != null) {
    // url = Encoding.htmlDecode(redirect[1]);
    // wait = Integer.parseInt(redirect[0]) * 1000;
    // }
    //
    // if (url != null) {
    // if (wait > 0) {
    // Thread.sleep(wait);
    // }
    // getPageFollowRedirects(br, url, false);
    //
    // }
    // }

    /**
     * See https://stackoverflow.com/questions/16907352/reverse-engineering-javascript-behind-google-button
     *
     * @throws PluginException
     */
    public static String getSAPISidHash(final Browser br, final String url) throws PluginException {
        String sapisid = null;
        /* Get that cookie from any domain [most commonly google.com or youtube.com]. */
        final HashMap<String, Cookies> cookiemap = br.getCookies();
        final Iterator<Entry<String, Cookies>> iterator = cookiemap.entrySet().iterator();
        itloop: while (iterator.hasNext()) {
            final Entry<String, Cookies> entry = iterator.next();
            final Cookies cookies = entry.getValue();
            for (final Cookie cookie : cookies.getCookies()) {
                if (cookie.getKey().equalsIgnoreCase("SAPISID")) {
                    sapisid = cookie.getValue();
                    break itloop;
                }
            }
        }
        if (sapisid == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else {
            final long timeMillis = System.currentTimeMillis() / 1000;
            return timeMillis + "_" + JDHash.getSHA1(timeMillis + " " + sapisid + " " + url);
        }
    }

    public static Browser prepBrowserWebAPI(final Browser br, final Account account, final String domain) throws PluginException {
        final String domainWithProtocol = "https://" + domain;
        br.getHeaders().put("Accept", "*/*");
        br.getHeaders().put("Origin", domainWithProtocol);
        br.getHeaders().put("Referer", domainWithProtocol + "/");
        /**
         * Usage of "X-Referer" and "X-Origin" headers makes "content-workspacevideo-pa.googleapis.com/v1/drive/media/" request fail! </br>
         * For GoogleDrive internal linkcheck, those headers can be set (they are set in browsers original request) but they are not
         * mandatory.
         */
        // br.getHeaders().put("X-Referer", domainWithProtocol);
        // br.getHeaders().put("X-Origin", domainWithProtocol);
        br.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        br.getHeaders().put("X-Javascript-User-Agent", "google-api-javascript-client/1.1.0");
        if (account != null) {
            /* For logged in users: */
            final String sapisidhash = GoogleHelper.getSAPISidHash(br, domainWithProtocol);
            if (sapisidhash != null) {
                br.getHeaders().put("Authorization", "SAPISIDHASH " + sapisidhash);
            }
            br.getHeaders().put("X-Goog-Authuser", "0");
        }
        return br;
    }
}
