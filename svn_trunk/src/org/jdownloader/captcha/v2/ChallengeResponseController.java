package org.jdownloader.captcha.v2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import org.appwork.timetracker.TimeTracker;
import org.appwork.timetracker.TimeTrackerController;
import org.appwork.timetracker.TrackerRule;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.jdownloader.api.captcha.CaptchaAPISolver;
import org.jdownloader.captcha.blacklist.BlacklistEntry;
import org.jdownloader.captcha.blacklist.CaptchaBlackList;
import org.jdownloader.captcha.event.ChallengeResponseEvent;
import org.jdownloader.captcha.event.ChallengeResponseEventSender;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CloudflareTurnstileChallenge;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.keycaptcha.KeyCaptchaDialogSolver;
import org.jdownloader.captcha.v2.challenge.keycaptcha.jac.KeyCaptchaJACSolver;
import org.jdownloader.captcha.v2.challenge.oauth.AccountOAuthSolver;
import org.jdownloader.captcha.v2.challenge.oauth.OAuthDialogSolver;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
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
import org.jdownloader.captcha.v2.solver.solver9kw.Captcha9kwSolverPuzzle;
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
import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;
import jd.plugins.Account;

public class ChallengeResponseController {
    private static final ChallengeResponseController INSTANCE = new ChallengeResponseController();

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
        addSolver(Captcha9kwSolverPuzzle.getInstance());
        if (!Application.isHeadless()) {
            addSolver(DialogBasicCaptchaSolver.getInstance());
            addSolver(DialogClickCaptchaSolver.getInstance());
            addSolver(DialogMultiClickCaptchaSolver.getInstance());
            addSolver(BrowserSolver.getInstance());
            addSolver(OAuthDialogSolver.getInstance());
        }
        addSolver(AccountOAuthSolver.getInstance());
        addSolver(KeyCaptchaJACSolver.getInstance());
        if (!Application.isHeadless()) {
            addSolver(KeyCaptchaDialogSolver.getInstance());
        }
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

    private final List<ChallengeSolver<?>>               solverList                                                  = new CopyOnWriteArrayList<ChallengeSolver<?>>();
    private final List<SolverJob<?>>                     activeJobs                                                  = new ArrayList<SolverJob<?>>();
    private final HashMap<UniqueAlltimeID, SolverJob<?>> challengeIDToJobMap                                         = new HashMap<UniqueAlltimeID, SolverJob<?>>();
    protected static AtomicLong                          TIMESTAMP_NO_BROWSER_SOLVER_AVAILABLE_DIALOG_LAST_DISPLAYED = new AtomicLong(-1);

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
                showNoBrowserSolverInformation(c);
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

    public <T> Thread showNoBrowserSolverInformation(final Challenge<T> c) {
        synchronized (TIMESTAMP_NO_BROWSER_SOLVER_AVAILABLE_DIALOG_LAST_DISPLAYED) {
            if (Time.systemIndependentCurrentJVMTimeMillis() - TIMESTAMP_NO_BROWSER_SOLVER_AVAILABLE_DIALOG_LAST_DISPLAYED.longValue() < 1 * 60 * 60 * 1000) {
                /* Dialog has been shown already just recently -> Do not display now. */
                return null;
            }
            // TODO: Get a nicer captcha type string here once possible
            String captcha_challenge_type = c.getTypeID();
            if (captcha_challenge_type != null && captcha_challenge_type.contains("turnstile")) {
                captcha_challenge_type = "Cloudflare Turnstile";
            }
            final String captcha_challenge_type_final = captcha_challenge_type;
            final Thread thread = new Thread() {
                public void run() {
                    try {
                        final String help_article_url = "https://support.jdownloader.org/knowledgebase/article/error-skipped-captcha-is-required";
                        String message = "";
                        final String title;
                        String lang = System.getProperty("user.language").toLowerCase();
                        if ("de".equals(lang)) {
                            title = "Externer Solver erforderlich für diese Captcha-Challenge";
                            message += "Die interaktive Art der Captcha-Herausforderung '" + captcha_challenge_type_final + "', die gelöst werden muss, kann derzeit nicht lokal in deinem Browser gelöst werden.\r\n";
                            message += "Daher bleibt dir nur die Möglichkeit, einen [kostenpflichtigen] Captcha-Lösungsdienst zu nutzen, der diesen Captcha-Typ verarbeiten kann.\r\n";
                            message += "Dies ist keine Werbung, sondern ein technischer Hinweisdialog.\r\n";
                            message += "Für detailliertere Informationen lies bitte den unten stehenden Hilfe-Artikel:\r\n";
                            message += help_article_url;
                        } else if ("es".equals(lang)) {
                            title = "Se requiere un solucionador externo para este desafío de captcha";
                            message += "El tipo interactivo de desafío captcha '" + captcha_challenge_type_final + "' que debe resolverse actualmente no puede resolverse localmente en tu navegador.\r\n";
                            message += "Por lo tanto, tu única opción es obtener un servicio de resolución de captchas [de pago] que pueda manejar este tipo de captcha.\r\n";
                            message += "Esto no es un anuncio sino un cuadro de diálogo de información técnica.\r\n";
                            message += "Para obtener más información detallada, lee el artículo de ayuda a continuación:\r\n";
                            message += help_article_url;
                        } else if ("fr".equals(lang)) {
                            title = "Solveur externe requis pour ce défi captcha";
                            message += "Le type interactif de défi captcha '" + captcha_challenge_type_final + "' qui doit être résolu ne peut actuellement pas être résolu localement dans votre navigateur.\r\n";
                            message += "Par conséquent, votre seule option est d’utiliser un service de résolution de captchas [payant] capable de traiter ce type de captcha.\r\n";
                            message += "Ceci n’est pas une publicité mais une boîte de dialogue d’information technique.\r\n";
                            message += "Pour plus d’informations détaillées, veuillez lire l’article d’aide ci-dessous :\r\n";
                            message += help_article_url;
                        } else if ("hi".equals(lang)) {
                            title = "इस कैप्चा चुनौती के लिए बाहरी सॉल्वर आवश्यक है";
                            message += "इंटरएक्टिव प्रकार का कैप्चा चुनौती '" + captcha_challenge_type_final + "', जिसे हल करना है, वर्तमान में आपके ब्राउज़र में स्थानीय रूप से हल नहीं किया जा सकता।\r\n";
                            message += "इसलिए आपका एकमात्र विकल्प एक [सशुल्क] कैप्चा समाधान सेवा प्राप्त करना है जो इस प्रकार के कैप्चा को संभाल सकती है।\r\n";
                            message += "यह कोई विज्ञापन नहीं है बल्कि एक तकनीकी सूचना संवाद है।\r\n";
                            message += "अधिक विस्तृत जानकारी के लिए, कृपया नीचे दिया गया सहायता लेख पढ़ें:\r\n";
                            message += help_article_url;
                        } else if ("zh".equals(lang) || "zh-cn".equals(lang)) {
                            title = "此验证码挑战需要外部解答器";
                            message += "交互式验证码挑战 '" + captcha_challenge_type_final + "' 目前无法在您的浏览器中本地解决。\r\n";
                            message += "因此，您唯一的选择是使用可以处理此类验证码的 [付费] 验证码解答服务。\r\n";
                            message += "这不是广告，而是一个技术信息对话框。\r\n";
                            message += "有关更多详细信息，请阅读以下帮助文章：\r\n";
                            message += help_article_url;
                        } else {
                            // Default English
                            title = "External solver required for this captcha challenge";
                            message += "The interactive type of captcha challenge '" + captcha_challenge_type_final + "' that needs to be solved currently cannot be solved locally in your browser.\r\n";
                            message += "Therefore your only option is to get a [paid] captcha solver service which can handle this type of captcha.\r\n";
                            message += "This is not an advertisement but a technical information dialog.\r\n";
                            message += "For more detailed information, please read the help article down below:\r\n";
                            message += help_article_url;
                        }
                        final ConfirmDialog dialog = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN | UIOManager.BUTTONS_HIDE_CANCEL, title, message);
                        dialog.setTimeout(3 * 60 * 1000);
                        if (CrossSystem.isOpenBrowserSupported() && !Application.isHeadless()) {
                            CrossSystem.openURL(help_article_url);
                        }
                        TIMESTAMP_NO_BROWSER_SOLVER_AVAILABLE_DIALOG_LAST_DISPLAYED.set(Time.systemIndependentCurrentJVMTimeMillis());
                        final ConfirmDialogInterface ret = UIOManager.I().show(ConfirmDialogInterface.class, dialog);
                        ret.throwCloseExceptions();
                    } catch (final Throwable e) {
                        // getLogger().log(e);
                    }
                };
            };
            thread.setDaemon(true);
            thread.start();
            return thread;
        }
    }

    @SuppressWarnings("unchecked")
    private <T> List<ChallengeSolver<T>> createList(final Challenge<T> c) {
        final List<ChallengeSolver<T>> ret = new ArrayList<ChallengeSolver<T>>();
        for (final ChallengeSolver<?> s : solverList) {
            try {
                if (s.isEnabled() && s.validateLogins() && s.canHandle(c) && s.validateBlackWhite(c)) {
                    ret.add((ChallengeSolver<T>) s);
                }
            } catch (final Throwable e) {
                logger.log(e);
            }
        }
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            final AccountFilter af = new AccountFilter().setEnabled(true).setValid(true).setFeature(FEATURE.CAPTCHA_SOLVER);
            final List<Account> solverAccounts = AccountController.getInstance().listAccounts(af);
            final HashSet<String> unavailableSolverDomains = new HashSet<String>();
            for (final Account solverAccount : solverAccounts) {
                boolean success = false;
                try {
                    final abstractPluginForCaptchaSolver plugin = (abstractPluginForCaptchaSolver) solverAccount.getPlugin();
                    final PluginChallengeSolver<T> solver = plugin.getPluginChallengeSolver(c, solverAccount);
                    if (solver == null) {
                        /* E.g. solver cannot handle challenge it gets presented */
                        continue;
                    }
                    ret.add(solver);
                    success = true;
                } catch (final Throwable e) {
                    logger.log(e);
                } finally {
                    if (success) {
                        unavailableSolverDomains.remove(solverAccount.getHoster());
                    } else {
                        unavailableSolverDomains.add(solverAccount.getHoster());
                    }
                }
            }
            logger.info("Solver accounts that cannot be used for this challenge: " + unavailableSolverDomains);
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
