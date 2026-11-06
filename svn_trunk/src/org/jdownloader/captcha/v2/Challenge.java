package org.jdownloader.captcha.v2;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

import jd.controlling.accountchecker.AccountCheckerThread;
import jd.controlling.captcha.SkipRequest;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.LinkCrawler;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.Plugin;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.jdownloader.DomainInfo;
import org.jdownloader.captcha.v2.ChallengeSolver.SolverType;
import org.jdownloader.captcha.v2.solverjob.ResponseList;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.controlling.UniqueAlltimeID;
import org.jdownloader.gui.translate._GUI;

public abstract class Challenge<T> {
    private final UniqueAlltimeID id                 = new UniqueAlltimeID();
    private final Class<T>        resultType;
    private final long            created            = Time.systemIndependentCurrentJVMTimeMillis();
    private volatile long         lastActivity       = created;
    private int                   timeout            = -1;
    private final boolean         createdInsideAccountChecker;
    private int                   round              = -1;
    private CaptchaRequestType    captchaRequestType = null;
    private final boolean         guessAccountLogin;

    public enum CaptchaRequestType implements LabelInterface {
        HOSTER {
            @Override
            public String getLabel() {
                return _GUI.T.CaptchaRequestType_HOSTER();
            }
        },
        HOSTER_LOGIN {
            @Override
            public String getLabel() {
                return _GUI.T.CaptchaRequestType_HOSTER_LOGIN();
            }
        },
        DECRYPTER {
            @Override
            public String getLabel() {
                return _GUI.T.CaptchaRequestType_DECRYPTER();
            }
        }
    }

    public int getRound() {
        return round;
    }

    public Object getAPIStorable(String format) throws Exception {
        return null;
    }

    public boolean canBeSkippedBy(SkipRequest skipRequest, ChallengeSolver<?> solver, Challenge<?> challenge) {
        final Plugin plg = getPlugin();
        final CaptchaRequestType requestType = getCaptchaRequestType();
        if (plg == null || requestType == null) {
            return false;
        }
        switch (requestType) {
        case HOSTER:
        case HOSTER_LOGIN:
            return canBeSkippedByPluginforHost(skipRequest, solver, challenge, plg);
        case DECRYPTER:
            return canBeSkippedByPluginforDecrypt(skipRequest, solver, challenge, plg);
        default:
            return false;
        }
    }

    private boolean canBeSkippedByPluginforDecrypt(final SkipRequest skipRequest, final ChallengeSolver<?> solver, final Challenge<?> challenge, final Plugin plugin) {
        final Plugin challengePlugin = challenge.getPlugin();
        if (challengePlugin == null || challenge.getCaptchaRequestType() != CaptchaRequestType.DECRYPTER) {
            /* we only want block PluginForDecrypt captcha here */
            return false;
        }
        final PluginForDecrypt currentPlugin = (PluginForDecrypt) plugin;
        final LinkCrawler currentCrawler = currentPlugin.getCrawler();
        final CrawledLink currentOrigin = currentPlugin.getCurrentLink().getOriginLink();
        final PluginForDecrypt decrypt = (PluginForDecrypt) challengePlugin;
        if (currentCrawler != decrypt.getCrawler()) {
            /* we have a different crawler source */
            return false;
        }
        switch (skipRequest) {
        case STOP_CURRENT_ACTION:
            /* user wants to stop current action (eg crawling) */
            return true;
        case BLOCK_ALL_CAPTCHAS:
            /* user wants to block all captchas (current session) */
            return true;
        case BLOCK_HOSTER:
            /* user wants to block captchas from specific hoster */
            return StringUtils.equals(currentPlugin.getHost(), challengePlugin.getHost());
        case BLOCK_PACKAGE:
            final CrawledLink crawledLink = decrypt.getCurrentLink();
            return crawledLink != null && crawledLink.getOriginLink() == currentOrigin;
        default:
            return false;
        }
    }

    public boolean isGuessAccountLogin() {
        return guessAccountLogin;
    }

    public Boolean isAccountLogin() {
        final CaptchaRequestType requestType = getCaptchaRequestType();
        if (requestType != null) {
            return CaptchaRequestType.HOSTER_LOGIN.equals(requestType);
        }
        if (isGuessAccountLogin()) {
            return true;
        }
        if (isCreatedInsideAccountChecker()) {
            return true;
        }
        return null;
    }

    private boolean canBeSkippedByPluginforHost(final SkipRequest skipRequest, final ChallengeSolver<?> solver, final Challenge<?> challenge, final Plugin plugin) {
        if (Boolean.TRUE.equals(isAccountLogin())) {
            /* we don't want to skip login captcha inside fetchAccountInfo(Thread is AccountCheckerThread) */
            return false;
        }
        final DownloadLink challengeLink = challenge.getDownloadLink();
        if (challengeLink == null) {
            return false;
        }
        final Plugin challengePlugin = challenge.getPlugin();
        if (challengePlugin != null && challenge.getCaptchaRequestType() == CaptchaRequestType.DECRYPTER) {
            /* we only want block PluginForHost captcha here */
            return false;
        }
        final PluginForHost currentPlugin = (PluginForHost) plugin;
        final DownloadLink currentLink = currentPlugin.getDownloadLink();
        switch (skipRequest) {
        case BLOCK_ALL_CAPTCHAS:
            /* user wants to block all captchas (current session) */
            return true;
        case BLOCK_HOSTER:
            /* user wants to block captchas from specific hoster */
            return StringUtils.equals(currentPlugin.getHost(), challengePlugin.getHost());
        case BLOCK_PACKAGE:
            /* user wants to block captchas from current FilePackage */
            if (challengeLink.getDefaultPlugin() == null) {
                return false;
            }
            final FilePackage currentFilePackage = currentLink.getFilePackage();
            return !FilePackage.isDefaultFilePackage(currentFilePackage) && challengeLink.getFilePackage() == currentLink.getFilePackage();
        default:
            return false;
        }
    }

    /**
     * Can be overridden to validate a response before adding it to the job.
     *
     * @return {@link ValidationResult#VALID} or the exact reason why the response was rejected
     */
    public ValidationResult validateResponse(AbstractResponse<T> response) {
        if (response == null) {
            return ValidationResult.INVALID;
        } else if (response.getValue() == null) {
            return ValidationResult.EMPTY_RESPONSE;
        } else {
            return ValidationResult.VALID;
        }
    }

    public UniqueAlltimeID getId() {
        return id;
    }

    @SuppressWarnings("unchecked")
    public Challenge(String method, String explain2) {
        typeID = method;
        explain = explain2;
        createdInsideAccountChecker = Thread.currentThread() instanceof AccountCheckerThread;
        Type superClass = this.getClass().getGenericSuperclass();
        while (superClass instanceof Class) {
            superClass = ((Class<?>) superClass).getGenericSuperclass();
            if (superClass == null) {
                throw new IllegalArgumentException("Wrong Construct");
            }
        }
        resultType = (Class<T>) ((ParameterizedType) superClass).getActualTypeArguments()[0];
        guessAccountLogin = guessAccountLogin();
    }

    protected boolean guessAccountLogin() {
        final StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
        Boolean elementWithinPlugin = null;
        for (StackTraceElement element : stackTrace) {
            try {
                final Class<?> clazz = Class.forName(element.getClassName());
                final boolean isPluginClazz = Plugin.class.isAssignableFrom(clazz);
                if (isPluginClazz) {
                    if (elementWithinPlugin == null || elementWithinPlugin.booleanValue() == true) {
                        elementWithinPlugin = true;
                    } else {
                        return false;
                    }
                } else if (Boolean.TRUE.equals(elementWithinPlugin)) {
                    return false;
                }
            } catch (ClassNotFoundException ignore) {
            }
            if (elementWithinPlugin == null) {
                continue;
            } else if (StringUtils.startsWithCaseInsensitive(element.getMethodName(), "fetchAccountInfo")) {
                return true;
            } else if (StringUtils.startsWithCaseInsensitive(element.getMethodName(), "login")) {
                return true;
            }
        }
        return false;
    }

    public boolean isCreatedInsideAccountChecker() {
        return createdInsideAccountChecker;
    }

    public int getTimeout() {
        return Math.max(-1, timeout);
    }

    public void setTimeout(int timeout) {
        this.timeout = timeout;
    }

    public Class<T> getResultType() {
        return resultType;
    }

    /**
     * Returns the validation state of this challenge after the solvers are done: {@link ValidationResult#VALID} if there is a usable
     * result. Otherwise the exact reason why the answer(s) of the solver(s) were rejected (see {@link #validateResponse(AbstractResponse)}
     * ), or {@link ValidationResult#INVALID} if there is no more detailed reason (e.g. no solver delivered an answer).
     */
    public ValidationResult getValidationResult() {
        final ResponseList<T> results = getResult();
        if (results != null && results.getValue() != null) {
            return ValidationResult.VALID;
        }
        final SolverJob<T> job = this.job;
        final ValidationResult rejected = job == null ? null : job.getRejectedValidationResult();
        return rejected != null ? rejected : ValidationResult.INVALID;
    }

    private String typeID;
    private String explain;

    public String getTypeID() {
        return typeID;
    }

    public long getCreated() {
        return created;
    }

    public long getLastActivity() {
        return lastActivity;
    }

    public boolean keepAlive() {
        final Plugin plugin = getPlugin();
        if (plugin != null && plugin.keepAlive(this)) {
            lastActivity = Time.systemIndependentCurrentJVMTimeMillis();
            return true;
        } else {
            return false;
        }
    }

    public long getValidUntil() {
        final int timeout = getTimeout();
        if (timeout > 0) {
            return getLastActivity() + timeout;
        } else {
            return -1;
        }
    }

    public int getRemainingTimeout() {
        final long validUntil = getValidUntil();
        if (validUntil > 0) {
            return (int) Math.max(0, validUntil - Time.systemIndependentCurrentJVMTimeMillis());
        } else {
            return -1;
        }
    }

    public void setTypeID(String typeID) {
        this.typeID = typeID;
    }

    public String getExplain() {
        return explain;
    }

    public void setExplain(String explain) {
        this.explain = explain;
    }

    public ResponseList<T> getResult() {
        return result;
    }

    public void setResult(ResponseList<T> result) {
        this.result = result;
    }

    private ResponseList<T> result;
    private SolverJob<T>    job;

    public String getHost() {
        final Plugin plg = getPlugin();
        return plg == null ? null : plg.getHost();
    }

    public DownloadLink getDownloadLink() {
        final Plugin plugin = getPlugin();
        if (plugin != null && plugin instanceof PluginForHost) {
            return ((PluginForHost) plugin).getDownloadLink();
        } else {
            return null;
        }
    }

    public DomainInfo getDomainInfo() {
        final Plugin plugin = getPlugin();
        if (plugin == null) {
            throw new WTFException("no plugin for this challenge!?");
        }
        DomainInfo ret = null;
        if (plugin instanceof PluginForHost) {
            final DownloadLink dl = ((PluginForHost) plugin).getDownloadLink();
            if (dl != null) {
                ret = dl.getDomainInfo();
            }
        }
        if (ret == null) {
            ret = DomainInfo.getInstance(plugin.getHost());
        }
        if (ret != null) {
            return ret;
        }
        throw new WTFException("no domaininfo for this challenge!?");
    }

    public Plugin getPlugin() {
        return null;
    }

    public AbstractResponse<T> parseAPIAnswer(String result, String resultFormat, ChallengeSolver<?> solver) {
        return null;
    }

    public void initController(SolverJob<T> job) {
        this.job = job;
        final Plugin plg = getPlugin();
        if (plg != null) {
            round = plg.addChallenge(this);
        }
    }

    public SolverJob<T> getJob() {
        return job;
    }

    /**
     * is called from plugins after all it's challenges have been handled. NOTE: Not all plugins call this method.
     */
    public void cleanup() {
    }

    /**
     * called when the controller handled this challenge
     */
    public void onHandled() {
    }

    public void sendStatsError(ChallengeSolver<?> solver, Throwable e) {
        if (e == null || e instanceof InterruptedException) {
            return;
        } else if (solver == null || solver.getSolverType() != SolverType.EXTERNAL) {
            return;
        }
    }

    public void sendStatsSolving(ChallengeSolver<?> solver) {
        if (solver == null || solver.getSolverType() != SolverType.EXTERNAL) {
            return;
        }
    }

    public void sendStatsValidation(ChallengeSolver<?> solver, String status) {
        if (solver == null || solver.getSolverType() != SolverType.EXTERNAL) {
            return;
        }
    }

    // is called in a 1000ms interval while solvers are active. can be used to check for external success (like oauth
    public void poll(SolverJob<T> job2) {
    }

    public CaptchaRequestType getCaptchaRequestType() {
        return captchaRequestType;
    }

    public Challenge<T> setCaptchaRequestType(CaptchaRequestType captchaRequestType) {
        this.captchaRequestType = captchaRequestType;
        return this;
    }

    private volatile HTTPProxy proxy = null;

    /**
     * Returns the proxy which was used for the requests of this challenge (the plugin sets it), or null if there was none (direct
     * connection = the IP address of the user). <br>
     * Solvers which work from another IP address (external solver services) can only solve challenges which require the same IP address
     * (see {@link #isSameIPRequired()}) if they use this proxy, see {@link ChallengeSolver.SolverFeature#PROXY}.
     */
    public HTTPProxy getProxy() {
        return proxy;
    }

    public void setProxy(final HTTPProxy proxy) {
        this.proxy = proxy;
    }

    /** True if the challenge has a real proxy (not null, not a direct connection) which a solver could use. */
    public boolean hasProxy() {
        final HTTPProxy p = proxy;
        return p != null && p.isRemote();
    }

    /**
     * Returns true if all requests that belong to this challenge (loading the challenge, solving it, sending the result to the website)
     * have to come from the same IP address as the requests of the user/JDownloader. <br>
     * Some captchas bind the challenge or the solution to the IP address that requested the challenge, e.g. powCAPTCHA. A solver which
     * works from another IP address (e.g. external solver services which solve the captcha on their own servers/workers) cannot solve such
     * a challenge: the website would reject the result. <br>
     * Default: false. Challenges of captchas that are bound to the IP address have to override this. <br>
     * See {@link ChallengeSolver#getChallengeVetoReason(Challenge)}: External solvers are only used for such a challenge if the challenge
     * has a proxy ({@link #getProxy()}) and the solver supports proxies ({@link ChallengeSolver.SolverFeature#PROXY}).
     */
    public boolean isSameIPRequired() {
        return false;
    }
}
