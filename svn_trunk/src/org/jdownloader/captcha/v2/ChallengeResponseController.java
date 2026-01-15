package org.jdownloader.captcha.v2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import org.appwork.storage.config.JsonConfig;
import org.appwork.timetracker.TimeTracker;
import org.appwork.timetracker.TimeTrackerController;
import org.appwork.timetracker.TrackerRule;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.api.captcha.CaptchaAPISolver;
import org.jdownloader.captcha.blacklist.BlacklistEntry;
import org.jdownloader.captcha.blacklist.CaptchaBlackList;
import org.jdownloader.captcha.event.ChallengeResponseEvent;
import org.jdownloader.captcha.event.ChallengeResponseEventSender;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.cutcaptcha.CutCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.oauth.AccountOAuthSolver;
import org.jdownloader.captcha.v2.challenge.oauth.OAuthDialogSolver;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.solver.CESChallengeSolver;
import org.jdownloader.captcha.v2.solver.antiCaptchaCom.AntiCaptchaComSolver;
import org.jdownloader.captcha.v2.solver.browser.AbstractBrowserChallenge;
import org.jdownloader.captcha.v2.solver.browser.BrowserSolver;
import org.jdownloader.captcha.v2.solver.cheapcaptcha.CheapCaptchaSolver;
import org.jdownloader.captcha.v2.solver.dbc.DeathByCaptchaSolver;
import org.jdownloader.captcha.v2.solver.endcaptcha.EndCaptchaSolver;
import org.jdownloader.captcha.v2.solver.gui.DialogBasicCaptchaSolver;
import org.jdownloader.captcha.v2.solver.gui.DialogClickCaptchaSolver;
import org.jdownloader.captcha.v2.solver.gui.DialogMultiClickCaptchaSolver;
import org.jdownloader.captcha.v2.solver.imagetyperz.ImageTyperzCaptchaSolver;
import org.jdownloader.captcha.v2.solver.jac.JACSolver;
import org.jdownloader.captcha.v2.solver.solver9kw.Captcha9kwSolver;
import org.jdownloader.captcha.v2.solver.solver9kw.Captcha9kwSolverClick;
import org.jdownloader.captcha.v2.solver.solver9kw.Captcha9kwSolverMultiClick;
import org.jdownloader.captcha.v2.solver.twocaptcha.TwoCaptchaSolver;
import org.jdownloader.captcha.v2.solverjob.ResponseList;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.controlling.UniqueAlltimeID;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;
import org.jdownloader.settings.staticreferences.CFG_CAPTCHA;
import org.jdownloader.updatev2.UpdateController;

import jd.controlling.AccountController;
import jd.controlling.AccountFilter;
import jd.controlling.captcha.CaptchaSettings;
import jd.controlling.captcha.CaptchaSettings.INTERACTIVE_CAPTCHA_PRIVACY_LEVEL;
import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;
import jd.parser.Regex;
import jd.plugins.Account;

public class ChallengeResponseController {
    private static final ChallengeResponseController INSTANCE         = new ChallengeResponseController();
    protected final static CaptchaSettings           CAPTCHA_SETTINGS = JsonConfig.create(CaptchaSettings.class);

    /**
     * get the only existing instance of ChallengeResponseController. This is a singleton
     *
     * @return
     */
    public static ChallengeResponseController getInstance() {
        return ChallengeResponseController.INSTANCE;
    }

    private ChallengeResponseEventSender eventSender;

    public ChallengeResponseEventSender getEventSender() {
        return eventSender;
    }

    private LogSource             logger;
    private TimeTrackerController trackerCache;

    /**
     * Create a new instance of ChallengeResponseController. This is a singleton class. Access the only existing instance by using
     * {@link #getInstance()}.
     */
    private ChallengeResponseController() {
        logger = LogController.getInstance().getLogger(getClass().getName());
        eventSender = new ChallengeResponseEventSender(logger);
        trackerCache = new TimeTrackerController();
        HashMap<String, ArrayList<CaptchaQualityEnsuranceRule>> rules = CFG_CAPTCHA.CFG.getQualityEnsuranceRules();
        if (rules == null) {
            rules = new HashMap<String, ArrayList<CaptchaQualityEnsuranceRule>>();
        }
        boolean save = false;
        save = addDefaultRules(rules, RecaptchaV2Challenge.RECAPTCHAV2, new CaptchaQualityEnsuranceRule(60, 10 * 60 * 1000), new CaptchaQualityEnsuranceRule(6, 60 * 1000), new CaptchaQualityEnsuranceRule(3, 30 * 1000), new CaptchaQualityEnsuranceRule(2, 10 * 1000)) || save;
        save = addDefaultRules(rules, HCaptchaChallenge.getChallengeType(), new CaptchaQualityEnsuranceRule(60, 10 * 60 * 1000), new CaptchaQualityEnsuranceRule(6, 60 * 1000), new CaptchaQualityEnsuranceRule(3, 30 * 1000), new CaptchaQualityEnsuranceRule(2, 10 * 1000)) || save;
        if (save) {
            CFG_CAPTCHA.CFG.setQualityEnsuranceRules(rules);
        }
        for (Entry<String, ArrayList<CaptchaQualityEnsuranceRule>> es : rules.entrySet()) {
            ArrayList<CaptchaQualityEnsuranceRule> rc = es.getValue();
            final TimeTracker tracker = trackerCache.getTracker(es.getKey());
            for (CaptchaQualityEnsuranceRule r : rc) {
                logger.info("Add Captcha Limit Rule for " + es.getKey() + " " + r.getLimit() + " Reqs in " + TimeFormatter.formatMilliSeconds(r.getInterval(), 0));
                tracker.addRule(new TrackerRule(r.getLimit(), r.getInterval()));
            }
        }
    }

    private boolean addDefaultRules(HashMap<String, ArrayList<CaptchaQualityEnsuranceRule>> rules, String key, CaptchaQualityEnsuranceRule... defList) {
        ArrayList<CaptchaQualityEnsuranceRule> rc = rules.get(key);
        if (rc == null || rc.size() == 0) {
            rc = new ArrayList<CaptchaQualityEnsuranceRule>();
            for (CaptchaQualityEnsuranceRule r : defList) {
                rc.add(r);
            }
            rules.put(key, rc);
            return true;
        }
        return false;
    }

    private final AtomicBoolean init = new AtomicBoolean(false);

    public void init() {
        if (!init.compareAndSet(false, true)) {
            return;
        }
        addSolver(JACSolver.getInstance());
        addSolver(DeathByCaptchaSolver.getInstance());
        addSolver(ImageTyperzCaptchaSolver.getInstance());
        addSolver(CheapCaptchaSolver.getInstance());
        addSolver(TwoCaptchaSolver.getInstance());
        addSolver(AntiCaptchaComSolver.getInstance());
        addSolver(EndCaptchaSolver.getInstance());
        addSolver(Captcha9kwSolver.getInstance());
        addSolver(Captcha9kwSolverClick.getInstance());
        addSolver(Captcha9kwSolverMultiClick.getInstance());
        if (!Application.isHeadless()) {
            addSolver(DialogBasicCaptchaSolver.getInstance());
            addSolver(DialogClickCaptchaSolver.getInstance());
            addSolver(DialogMultiClickCaptchaSolver.getInstance());
            addSolver(BrowserSolver.getInstance());
            addSolver(OAuthDialogSolver.getInstance());
        }
        addSolver(AccountOAuthSolver.getInstance());
        addSolver(CaptchaAPISolver.getInstance());
    }

    public List<ChallengeSolver<?>> listSolvers() {
        return new ArrayList<ChallengeSolver<?>>(solverList);
    }

    private final HashMap<String, SolverService> solverMap   = new HashMap<String, SolverService>();
    private final List<SolverService>            serviceList = new CopyOnWriteArrayList<SolverService>();

    private synchronized boolean addSolver(ChallengeSolver<?> solver) {
        if (solverMap.put(solver.getService().getID(), solver.getService()) == null) {
            serviceList.add(solver.getService());
        }
        return solverList.add(solver);
    }

    public <E> void fireNewAnswerEvent(SolverJob<E> job, AbstractResponse<E> abstractResponse) {
        eventSender.fireEvent(new ChallengeResponseEvent(this, ChallengeResponseEvent.Type.JOB_ANSWER, abstractResponse, job));
    }

    public List<SolverJob<?>> listJobs() {
        synchronized (activeJobs) {
            return new ArrayList<SolverJob<?>>(activeJobs);
        }
    }

    public boolean hasPendingJobs() {
        synchronized (activeJobs) {
            return activeJobs.size() > 0;
        }
    }

    public void fireBeforeSolveEvent(SolverJob<?> job, ChallengeSolver<?> solver) {
        eventSender.fireEvent(new ChallengeResponseEvent(this, ChallengeResponseEvent.Type.SOLVER_START, solver, job));
    }

    public void fireAfterSolveEvent(SolverJob<?> job, ChallengeSolver<?> solver) {
        job.getLogger().info("Solver " + solver + " finished job " + job);
        job._notifyAll();
        eventSender.fireEvent(new ChallengeResponseEvent(this, ChallengeResponseEvent.Type.SOLVER_END, solver, job));
    }

    private void fireNewJobEvent(SolverJob<?> job) {
        eventSender.fireEvent(new ChallengeResponseEvent(this, ChallengeResponseEvent.Type.NEW_JOB, job));
    }

    private void fireJobDone(SolverJob<?> job) {
        eventSender.fireEvent(new ChallengeResponseEvent(this, ChallengeResponseEvent.Type.JOB_DONE, job));
    }

    private final List<ChallengeSolver<?>>               solverList          = new CopyOnWriteArrayList<ChallengeSolver<?>>();
    private final List<SolverJob<?>>                     activeJobs          = new ArrayList<SolverJob<?>>();
    private final HashMap<UniqueAlltimeID, SolverJob<?>> challengeIDToJobMap = new HashMap<UniqueAlltimeID, SolverJob<?>>();

    /**
     * When one job gets a skiprequest, we have to check all pending jobs if this skiprequest affects them as well. if so, we have to skip
     * them as well.
     *
     * @param skipRequest
     * @param solver
     * @param challenge
     */
    public void setSkipRequest(SkipRequest skipRequest, ChallengeSolver<?> solver, Challenge<?> sourceChallenge) {
        synchronized (activeJobs) {
            for (SolverJob<?> job : activeJobs) {
                if (job.getChallenge() == sourceChallenge) {
                    job.setSkipRequest(skipRequest);
                } else if (job.getChallenge().canBeSkippedBy(skipRequest, solver, sourceChallenge)) {
                    job.setSkipRequest(skipRequest);
                }
            }
        }
    }

    public void keepAlivePendingChallenges(Challenge<?> c) {
        for (final SolverJob<?> job : activeJobs) {
            job.getChallenge().keepAlive();
        }
    }

    public <T> SolverJob<T> handle(final Challenge<T> c) throws InterruptedException, SkipException {
        if (c instanceof AbstractBrowserChallenge) {
            if (c instanceof RecaptchaV2Challenge) {
                UpdateController.getInstance().addFeedback("rc");
            } else if (c instanceof HCaptchaChallenge) {
                UpdateController.getInstance().addFeedback("hc");
            } else if (c instanceof CloudflareTurnstileChallenge) {
                UpdateController.getInstance().addFeedback("tc");
            }
        }
        CaptchaHistoryManager.getInstance().addEntry(c);
        LogSource logger = LogController.getInstance().getPreviousThreadLogSource();
        if (logger == null) {
            logger = this.logger;
        }
        logger.info("Log to " + logger.getName());
        logger.info("Handle Challenge: " + c);
        final List<ChallengeSolver<T>> solvers = createList(c);
        logger.info("Solver: " + solvers);
        if (solvers.size() == 0) {
            logger.info("No solver available!");
            if (c instanceof CloudflareTurnstileChallenge) {
                showNoBrowserSolverInfoDialog(c, "Cloudflare Turnstile");
            } else if (c instanceof CutCaptchaChallenge) {
                showNoBrowserSolverInfoDialog(c, "CutCaptcha");
            }
            /* See: https://support.jdownloader.org/knowledgebase/article/error-skipped-captcha-is-required */
            throw new SkipException(c, SkipRequest.BLOCK_HOSTER, "No solver available!");
        }
        final SolverJob<T> job = new SolverJob<T>(this, c, solvers);
        job.setLogger(logger);
        c.initController(job);
        final UniqueAlltimeID challengeID = c.getId();
        synchronized (activeJobs) {
            activeJobs.add(job);
            challengeIDToJobMap.put(challengeID, job);
        }
        try {
            for (final ChallengeSolver<T> cs : solvers) {
                logger.info("Send to solver: " + cs + " " + job);
                cs.enqueue(job);
            }
            logger.info("Fire New Job Event");
            fireNewJobEvent(job);
            logger.info("Wait");
            boolean timeout = false;
            while (!job.isSolved() && !job.isDone()) {
                final BlacklistEntry<?> blackListEntry = CaptchaBlackList.getInstance().matches(c);
                final Challenge<T> challenge = job.getChallenge();
                challenge.poll(job);
                if (!job.isSolved() && !job.isDone()) {
                    final long validUntil = challenge.getValidUntil();
                    if (validUntil != -1 && System.currentTimeMillis() > validUntil) {
                        timeout = true;
                        break;
                    }
                    job._wait(1000);
                } else {
                    break;
                }
                if (blackListEntry != null && job.setSkipRequest(SkipRequest.SINGLE)) {
                    break;
                }
            }
            if (timeout && job.setSkipRequest(SkipRequest.TIMEOUT)) {
                final Challenge<T> challenge = job.getChallenge();
                final long expired = System.currentTimeMillis() - challenge.getCreated();
                final int jobTimeout = challenge.getTimeout();
                logger.info("Challenge Timeout detected|Job:" + job + "|Expired:" + expired + "|Timeout:" + jobTimeout);
            }
            if (!SkipRequest.TIMEOUT.equals(job.getSkipRequest())) {
                keepAlivePendingChallenges(c);
            }
            if (job.getSkipRequest() != null) {
                throw new SkipException(c, job.getSkipRequest());
            }
            final ResponseList<T> response = job.getResponseAndKill();
            logger.info("All Responses: " + job.getResponses());
            logger.info("Solving Done. Result: " + response);
            return job;
        } catch (InterruptedException e) { // for example downloads have been stopped
            job.kill();
            throw e;
        } finally {
            try {
                synchronized (activeJobs) {
                    activeJobs.remove(job);
                    challengeIDToJobMap.remove(challengeID);
                }
            } finally {
                fireJobDone(job);
            }
            c.onHandled();
        }
    }

    /**
     * Wrapper function to be used when an interactive captcha challenge requests its siteURL. <br>
     * This function is to be used before such URLs are used anywhere in case they need to be modified for example due to the users' privacy
     * settings.
     */
    public String getSiteURL(String url) {
        if (INTERACTIVE_CAPTCHA_PRIVACY_LEVEL.STRICT.equals(CAPTCHA_SETTINGS.getInteractiveCaptchaPrivacyLevel())) {
            final String baseurl = new Regex(url, "(?i)(https?://[^/]+/?)").getMatch(0);
            if (baseurl != null) {
                url = baseurl;
            }
        }
        return url;
    }

    protected final static AtomicLong TIMESTAMP_NO_BROWSER_SOLVER_AVAILABLE_DIALOG_LAST_DISPLAYED = new AtomicLong(-1);

    public <T> Thread showNoBrowserSolverInfoDialog(final Challenge<T> c, String captcha_challenge_type) {
        while (true) {
            final long lastDisplay = TIMESTAMP_NO_BROWSER_SOLVER_AVAILABLE_DIALOG_LAST_DISPLAYED.get();
            if ((Time.systemIndependentCurrentJVMTimeMillis() - lastDisplay) < TimeUnit.HOURS.toMillis(1)) {
                /* Dialog has been shown already just recently -> Do not display now. */
                return null;
            } else if (TIMESTAMP_NO_BROWSER_SOLVER_AVAILABLE_DIALOG_LAST_DISPLAYED.compareAndSet(lastDisplay, Time.systemIndependentCurrentJVMTimeMillis())) {
                break;
            }
        }
        if (captcha_challenge_type == null) {
            captcha_challenge_type = c.getTypeID();
        }
        if (captcha_challenge_type != null && captcha_challenge_type.contains("turnstile")) {
            captcha_challenge_type = "Cloudflare Turnstile";
        }
        final String captcha_challenge_type_final = captcha_challenge_type;
        final Thread thread = new Thread() {
            public void run() {
                try {
                    final String help_article_url = "https://support.jdownloader.org/knowledgebase/article/error-skipped-captcha-is-required";
                    String message = "<html>";
                    final String title;
                    String lang = System.getProperty("user.language").toLowerCase();
                    if ("de".equals(lang)) {
                        title = "Externer Solver erforderlich für diese Captcha-Challenge";
                        message += "Die interaktive Art der Captcha-Herausforderung '" + captcha_challenge_type_final + "', die gelöst werden muss, kann derzeit nicht lokal in deinem Browser gelöst werden.<br><br>";
                        message += "Daher bleibt dir in JD nur die Möglichkeit, einen möglicherweise kostenpflichtigen Captcha-Lösungsdienst zu nutzen, der diesen Captcha-Typ verarbeiten kann.<br><br>";
                        message += "JD unterstützt verschiedene Captcha-Lösungsdienste, die im unten verlinkten Artikel aufgeführt werden.<br><br>";
                        message += "Für detailliertere Informationen lies bitte den folgenden Hilfe-Artikel:<br>";
                        message += "<a href=\"" + help_article_url + "\">" + help_article_url + "</a>";
                    } else if ("es".equals(lang)) {
                        title = "Se requiere un solucionador externo para este desafío de CAPTCHA";
                        message += "El tipo interactivo de desafío CAPTCHA '" + captcha_challenge_type_final + "' no puede resolverse actualmente de forma local en tu navegador.<br><br>";
                        message += "Por lo tanto, en JD solo tienes la opción de utilizar un servicio de resolución de captchas posiblemente de pago que pueda manejar este tipo de CAPTCHA.<br><br>";
                        message += "JD es compatible con varios servicios de resolución de CAPTCHA que se enumeran en el artículo enlazado a continuación.<br><br>";
                        message += "Para obtener información más detallada, lee el siguiente artículo de ayuda:<br>";
                        message += "<a href=\"" + help_article_url + "\">" + help_article_url + "</a>";
                    } else if ("fr".equals(lang)) {
                        title = "Solveur externe requis pour ce défi CAPTCHA";
                        message += "Le type interactif de défi CAPTCHA '" + captcha_challenge_type_final + "' ne peut actuellement pas être résolu localement dans votre navigateur.<br><br>";
                        message += "Par conséquent, dans JD, votre seule option est d’utiliser un service de résolution de CAPTCHA potentiellement payant capable de traiter ce type de CAPTCHA.<br><br>";
                        message += "JD prend en charge plusieurs services de résolution de CAPTCHA listés dans l’article ci-dessous.<br><br>";
                        message += "Pour plus d’informations détaillées, veuillez consulter l’article d’aide ci-dessous :<br>";
                        message += "<a href=\"" + help_article_url + "\">" + help_article_url + "</a>";
                    } else if ("hi".equals(lang)) {
                        title = "इस CAPTCHA चुनौती के लिए बाहरी सॉल्वर आवश्यक है";
                        message += "CAPTCHA चुनौती '" + captcha_challenge_type_final + "' की इंटरएक्टिव प्रकृति को आपके ब्राउज़र में वर्तमान में स्थानीय रूप से हल नहीं किया जा सकता।<br><br>";
                        message += "इसलिए JD में आपका एकमात्र विकल्प एक संभावित सशुल्क CAPTCHA समाधान सेवा का उपयोग करना है जो इस प्रकार के CAPTCHA को संभाल सके।<br><br>";
                        message += "JD कई CAPTCHA समाधान सेवाओं का समर्थन करता है, जिन्हें नीचे दिए गए लेख में सूचीबद्ध किया गया है।<br><br>";
                        message += "अधिक विस्तृत जानकारी के लिए कृपया नीचे दिया गया सहायता लेख पढ़ें:<br>";
                        message += "<a href=\"" + help_article_url + "\">" + help_article_url + "</a>";
                    } else if ("zh".equals(lang) || "zh-cn".equals(lang)) {
                        title = "此验证码挑战需要外部解答服务";
                        message += "交互式验证码挑战 '" + captcha_challenge_type_final + "' 当前无法在您的浏览器中本地完成。<br><br>";
                        message += "因此，在 JD 中，您只能使用能够处理此类验证码的可能需要付费的验证码解答服务。<br><br>";
                        message += "JD 支持多个验证码解答服务，具体列表请参阅以下文章。<br><br>";
                        message += "有关更详细的信息，请阅读以下帮助文章：<br>";
                        message += "<a href=\"" + help_article_url + "\">" + help_article_url + "</a>";
                    } else {
                        title = "External solver required for this captcha challenge";
                        message += "The interactive type of captcha challenge '" + captcha_challenge_type_final + "' cannot currently be solved locally in your browser.<br><br>";
                        message += "Therefore, in JD your only option is to use a captcha solving service that may require payment and is capable of handling this type of captcha.<br><br>";
                        message += "JD supports various captcha solving services listed in the help article below.<br><br>";
                        message += "For more detailed information, please read the help article below:<br>";
                        message += "<a href=\"" + help_article_url + "\">" + help_article_url + "</a>";
                    }
                    message += "</html>";
                    final ConfirmDialog dialog = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN | UIOManager.BUTTONS_HIDE_CANCEL | Dialog.STYLE_HTML, title, message);
                    dialog.setTimeout((int) TimeUnit.MINUTES.toMillis(3));
                    UIOManager.I().show(ConfirmDialogInterface.class, dialog).throwCloseExceptions();
                } catch (final Throwable e) {
                    // getLogger().log(e);
                }
            };
        };
        thread.setDaemon(true);
        thread.start();
        return thread;
    }

    @SuppressWarnings("unchecked")
    private <T> List<ChallengeSolver<T>> createList(final Challenge<T> c) {
        final List<ChallengeSolver<T>> ret = new ArrayList<ChallengeSolver<T>>();
        for (final ChallengeSolver<?> solver : solverList) {
            try {
                if (solver.isEnabled() && solver.validateLogins() && solver.canHandle(c) && solver.validateBlackWhite(c)) {
                    ret.add((ChallengeSolver<T>) solver);
                }
            } catch (final Throwable e) {
                logger.log(e);
            }
        }
        final AccountFilter af = new AccountFilter().setEnabled(true).setValid(true).setFeature(FEATURE.CAPTCHA_SOLVER);
        final List<Account> solverAccounts = AccountController.getInstance().listAccounts(af);
        final HashSet<String> unavailableSolverDomains = new HashSet<String>();
        for (final Account solverAccount : solverAccounts) {
            boolean success = false;
            try {
                final abstractPluginForCaptchaSolver plugin = (abstractPluginForCaptchaSolver) solverAccount.getPlugin();
                if (!plugin.canHandle(c)) {
                    continue;
                }
                final PluginChallengeSolver<T> solver = plugin.getPluginChallengeSolver(c, solverAccount);
                if (solver == null) {
                    /* E.g. solver cannot handle challenge it gets presented */
                    continue;
                }
                ret.add(solver);
                success = true;
            } catch (final Throwable e) {
                logger.log(e);
                logger.warning("Exception happened during collecting fitting solver plugins");
            } finally {
                if (success) {
                    unavailableSolverDomains.remove(solverAccount.getHoster());
                } else {
                    unavailableSolverDomains.add(solverAccount.getHoster());
                }
            }
        }
        logger.info("Solver accounts that cannot be used for this challenge: " + unavailableSolverDomains);
        avoidAutoSolver: if (c.isAccountLogin() && CAPTCHA_SETTINGS.isAvoidAutoSolverForLoginCaptchas()) {
            /*
             * Special handling for login captchas: Solve them locally if possible and wished in order to solve them faster since account
             * logins in JDownloader are supposed to happen fast.
             */
            final List<ChallengeSolver<T>> manualSolvers = new ArrayList<ChallengeSolver<T>>();
            for (final ChallengeSolver<T> solver : ret) {
                if (solver instanceof CESChallengeSolver) {
                    continue;
                }
                manualSolvers.add(solver);
            }
            if (manualSolvers.size() == 0) {
                break avoidAutoSolver;
            }
            /* Clear all external solvers and only allow local solvers. */
            ret.clear();
            ret.addAll(manualSolvers);
        }
        return ret;
    }

    public SolverJob<?> getJobByChallengeId(long id) {
        synchronized (challengeIDToJobMap) {
            return challengeIDToJobMap.get(new UniqueAlltimeID(id));
        }
    }

    public List<SolverService> listServices() {
        return new ArrayList<SolverService>(serviceList);
    }

    public SolverService getServiceByID(String key) {
        for (final SolverService service : serviceList) {
            if (service.getID().equals(key)) {
                return service;
            }
        }
        return null;
    }

    public void resetTiming() {
        final HashSet<Object> dupe = new HashSet<Object>();
        for (final ChallengeSolver<?> s : solverList) {
            if (dupe.add(s.getService())) {
                s.getService().getConfig().setWaitForMap(null);
            }
        }
    }

    public TimeTracker getTracker(String method) {
        return trackerCache.getTracker(method);
    }
}
