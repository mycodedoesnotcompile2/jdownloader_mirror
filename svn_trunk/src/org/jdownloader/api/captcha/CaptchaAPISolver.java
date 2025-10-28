package org.jdownloader.api.captcha;

import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

import org.appwork.remoteapi.RemoteAPI;
import org.appwork.remoteapi.RemoteAPIRequest;
import org.appwork.remoteapi.RemoteAPIResponse;
import org.appwork.remoteapi.exceptions.InternalApiException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.StringUtils;
import org.jdownloader.api.myjdownloader.MyJDownloaderController;
import org.jdownloader.api.myjdownloader.MyJDownloaderHttpConnection;
import org.jdownloader.api.myjdownloader.MyJDownloaderRequestInterface;
import org.jdownloader.captcha.event.ChallengeResponseListener;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.JobRunnable;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.oauth.AccountLoginOAuthChallenge;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.myjdownloader.client.json.SessionInfoResponse;

import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;
import jd.plugins.DownloadLink;

public class CaptchaAPISolver extends ChallengeSolver<Object> implements CaptchaAPI, ChallengeResponseListener {
    private static final CaptchaAPISolver INSTANCE = new CaptchaAPISolver();

    public static CaptchaAPISolver getInstance() {
        return INSTANCE;
    }

    protected int getDefaultWaitForOthersTimeout() {
        return 120000;
    }

    private final CaptchaAPIEventPublisher                 eventPublisher;
    private final CaptchaMyJDownloaderRemoteSolverSettings config;

    @Override
    protected boolean isChallengeSupported(Challenge<?> c) {
        return c instanceof HCaptchaChallenge || c instanceof RecaptchaV2Challenge || c instanceof AccountLoginOAuthChallenge || c instanceof ImageCaptchaChallenge;
    }

    @Override
    public void solve(final SolverJob<Object> job) throws InterruptedException, SolverException, SkipException {
        final Challenge<?> challenge = job.getChallenge();
        job.getLogger().info("Fire MyJDownloader Captcha Event");
        if (challenge instanceof RecaptchaV2Challenge) {
            // create fallback challenge here. we do not want to block later
            try {
                ((RecaptchaV2Challenge) challenge).createBasicCaptchaChallenge(false);
            } catch (final Throwable e) {
                job.getLogger().log(e);
            }
        }
        MyJDownloaderController.getInstance().pushCaptchaFlag(true);
        try {
            while (!isJobDone(job)) {
                Thread.sleep(250);
            }
        } finally {
            job.getLogger().info("Wait done");
        }
    }

    public CaptchaAPISolver() {
        // 0: no threadpool
        super(new CaptchaAPIManualRemoteSolverService(), 0);
        config = JsonConfig.create(CaptchaMyJDownloaderRemoteSolverSettings.class);
        eventPublisher = new CaptchaAPIEventPublisher();
        ChallengeResponseController.getInstance().getEventSender().addListener(this, true);
    }

    public CaptchaAPIEventPublisher getEventPublisher() {
        return eventPublisher;
    }

    public List<CaptchaJob> list(RemoteAPIRequest request) {
        final List<CaptchaJob> ret = new ArrayList<CaptchaJob>();
        for (final SolverJob<?> entry : ChallengeResponseController.getInstance().listJobs()) {
            if (!entry.isDone()) {
                final Challenge<?> challenge = entry.getChallenge();
                if (isChallengeSupported(challenge)) {
                    final CaptchaJob captchaJob = getCaptchaJob(request, challenge.getId().getID());
                    if (captchaJob != null) {
                        ret.add(captchaJob);
                    }
                }
            }
        }
        Collections.sort(ret, new Comparator<CaptchaJob>() {
            private final int compare(long x, long y) {
                return (x < y) ? -1 : ((x == y) ? 0 : 1);
            }

            @Override
            public int compare(CaptchaJob o1, CaptchaJob o2) {
                return compare(o1.getTimeout(), o2.getTimeout());
            }
        });
        return ret;
    }

    public void get(RemoteAPIRequest request, RemoteAPIResponse response, long id) throws InternalApiException, InvalidCaptchaIDException {
        get(request, response, id, null);
    }

    public void get(RemoteAPIRequest request, RemoteAPIResponse response, long id, String format) throws InternalApiException, InvalidCaptchaIDException {
        final SolverJob<?> job = getJobByChallengeId(id);
        if (job == null || job.isDone()) {
            throw new InvalidCaptchaIDException();
        }
        try {
            final Challenge<?> challenge = job.getChallenge();
            final OutputStream out = RemoteAPI.getOutputStream(response, request, RemoteAPI.gzip(request), true);
            try {
                final HashMap<String, Object> captchaResponseData = new HashMap<String, Object>();
                captchaResponseData.put("data", challenge.getAPIStorable(format));
                if (request.getHttpRequest() instanceof MyJDownloaderRequestInterface) {
                    captchaResponseData.put("rid", ((MyJDownloaderRequestInterface) request.getHttpRequest()).getRid());
                }
                out.write(JSonStorage.serializeToJson(captchaResponseData).getBytes("UTF-8"));
            } finally {
                try {
                    out.close();
                } catch (final Throwable e) {
                }
            }
        } catch (Exception e) {
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
            throw new InternalApiException(e);
        }
    }

    private SolverJob<?> getJobByChallengeId(long id) {
        return ChallengeResponseController.getInstance().getJobByChallengeId(id);
    }

    public boolean isJobDone(final SolverJob<?> job) {
        if (isMyJDownloaderActive()) {
            return super.isJobDone(job);
        } else {
            return true;
        }
    }

    @Override
    public void enqueue(SolverJob<Object> job) {
        if (!isMyJDownloaderActive()) {
            job.setSolverDone(this);
        } else {
            super.enqueue(job);
        }
    }

    private boolean isMyJDownloaderActive() {
        return MyJDownloaderController.getInstance().isActive();
    }

    public boolean solve(long id, String result) throws InvalidCaptchaIDException, InvalidChallengeTypeException {
        return solve(id, result, null);
    }

    @SuppressWarnings("unchecked")
    public boolean solve(long id, String result, String resultFormat) throws InvalidCaptchaIDException, InvalidChallengeTypeException {
        final SolverJob<?> job = getJobByChallengeId(id);
        if (job == null || job.isDone()) {
            throw new InvalidCaptchaIDException();
        } else {
            final Challenge<?> challenge = job.getChallenge();
            final AbstractResponse<?> ret = challenge.parseAPIAnswer(result, resultFormat, this);
            if (ret == null) {
                throw new InvalidChallengeTypeException(challenge.getClass().getName());
            } else {
                ((SolverJob<Object>) job).addAnswer((AbstractResponse<Object>) ret);
                return true;
            }
        }
    }

    @Deprecated
    public boolean skip(long id) throws InvalidCaptchaIDException {
        return skip(id, SkipRequest.SINGLE);
    }

    public boolean skip(long id, SkipRequest type) throws InvalidCaptchaIDException {
        final SolverJob<?> job = getJobByChallengeId(id);
        if (job == null) {
            throw new InvalidCaptchaIDException();
        } else {
            ChallengeResponseController.getInstance().setSkipRequest(type, this, job.getChallenge());
            return true;
        }
    }

    public void kill(SolverJob<Object> job) {
        super.kill(job);
        MyJDownloaderController.getInstance().pushCaptchaFlag(hasJobs());
    }

    @Override
    public CaptchaJob getCaptchaJob(RemoteAPIRequest request, long id) {
        final SolverJob<?> entry = getJobByChallengeId(id);
        if (entry != null) {
            final CaptchaJob ret = new CaptchaJob();
            final Challenge<?> challenge = entry.getChallenge();
            Class<?> cls = challenge.getClass();
            while (cls != null && StringUtils.isEmpty(ret.getType())) {
                ret.setType(cls.getSimpleName());
                ret.setChallengeType(cls.getSimpleName());
                cls = cls.getSuperclass();
            }
            if (challenge instanceof HCaptchaChallenge) {
                final MyJDownloaderHttpConnection con = MyJDownloaderHttpConnection.getMyJDownloaderHttpConnection(request);
                final SessionInfoResponse sessionInfo = con != null ? con.getSessionInfo() : null;
                if (sessionInfo != null && StringUtils.startsWithCaseInsensitive(sessionInfo.getAppKey(), "myjd_webinterface")) {
                    // workaround, required for Webinterface to show captcha popup
                    ret.setType("RecaptchaV2Challenge");
                }
            }
            ret.setID(challenge.getId().getID());
            ret.setHoster(challenge.getHost());
            ret.setCaptchaCategory(challenge.getTypeID());
            ret.setExplain(challenge.getExplain());
            ret.setRemaining(challenge.getRemainingTimeout());
            ret.setTimeout(challenge.getTimeout());
            ret.setCreated(challenge.getCreated());
            final DownloadLink link = challenge.getDownloadLink();
            if (link != null) {
                ret.setLink(link.getUniqueID().getID());
            }
            return ret;
        } else {
            return null;
        }
    }

    @Override
    public void onNewJobAnswer(SolverJob<?> job, AbstractResponse<?> response) {
    }

    @Override
    public void onJobDone(final SolverJob<?> job) {
        getEventPublisher().fireJobDoneEvent(job);
        dispose(job);
        MyJDownloaderController.getInstance().pushCaptchaFlag(hasJobs());
    }

    public boolean hasJobs() {
        synchronized (map) {
            return map.size() > 0;
        }
    }

    protected void dispose(SolverJob<?> job) {
        final JobRunnable<Object> suc;
        synchronized (map) {
            suc = map.remove(job);
        }
        if (suc != null) {
            suc.fireDoneAndAfterSolveEvents();
        }
    }

    public long keepAlive(final long jobId) {
        final SolverJob<?> entry = getJobByChallengeId(jobId);
        if (entry == null) {
            ChallengeResponseController.getInstance().keepAlivePendingChallenges(null);
            return -1;
        } else {
            ChallengeResponseController.getInstance().keepAlivePendingChallenges(entry.getChallenge());
            return entry.getChallenge().getRemainingTimeout();
        }
    }

    @Override
    public void onNewJob(SolverJob<?> job) {
        getEventPublisher().fireNewJobEvent(job, job.getChallenge());
    }

    @Override
    public void onJobSolverEnd(ChallengeSolver<?> solver, SolverJob<?> job) {
    }

    @Override
    public void onJobSolverStart(ChallengeSolver<?> solver, SolverJob<?> job) {
    }
}
