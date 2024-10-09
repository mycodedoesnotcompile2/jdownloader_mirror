package org.jdownloader.captcha.v2.challenge.keycaptcha;

import jd.controlling.captcha.ChallengeDialogHandler;
import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;
import jd.gui.swing.jdgui.JDGui;

import org.appwork.exceptions.WTFException;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.challenge.keycaptcha.dialog.KeyCaptchaCategoryDialogHandler;
import org.jdownloader.captcha.v2.challenge.keycaptcha.dialog.KeyCaptchaPuzzleDialogHandler;
import org.jdownloader.captcha.v2.solver.gui.DialogBasicCaptchaSolver;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.captcha.v2.solver.service.DialogSolverService;
import org.jdownloader.captcha.v2.solverjob.ChallengeSolverJobListener;
import org.jdownloader.captcha.v2.solverjob.ResponseList;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.settings.staticreferences.CFG_SILENTMODE;

public class KeyCaptchaDialogSolver extends ChallengeSolver<String> {
    private static final KeyCaptchaDialogSolver INSTANCE = new KeyCaptchaDialogSolver();

    public static KeyCaptchaDialogSolver getInstance() {
        return INSTANCE;
    }

    @Override
    protected boolean isChallengeSupported(Challenge<?> c) {
        return c instanceof KeyCaptchaPuzzleChallenge || c instanceof KeyCaptchaCategoryChallenge;
    }

    private KeyCaptchaDialogSolver() {
        super(DialogSolverService.getInstance(), 1);
    }

    private volatile ChallengeDialogHandler<?, ?, String> handler;

    @Override
    public Class<String> getResultType() {
        return String.class;
    }

    public void checkSilentMode(final SolverJob<String> job) throws SkipException, InterruptedException {
        if (JDGui.getInstance().isSilentModeActive()) {
            switch (CFG_SILENTMODE.CFG.getOnCaptchaDuringSilentModeAction()) {
            case WAIT_IN_BACKGROUND_UNTIL_WINDOW_GETS_FOCUS_OR_TIMEOUT:
                break;
            case DISABLE_DIALOG_SOLVER:
                job.getEventSender().addListener(new ChallengeSolverJobListener() {
                    @Override
                    public void onSolverTimedOut(ChallengeSolver<?> parameter) {
                    }

                    @Override
                    public void onSolverStarts(ChallengeSolver<?> parameter) {
                    }

                    @Override
                    public void onSolverJobReceivedNewResponse(AbstractResponse<?> response) {
                    }

                    @Override
                    public void onSolverDone(ChallengeSolver<?> solver) {
                        if (job.isDone()) {
                            if (!job.isSolved()) {
                                ChallengeResponseController.getInstance().setSkipRequest(SkipRequest.SINGLE, KeyCaptchaDialogSolver.this, job.getChallenge());
                            }
                            job.getEventSender().removeListener(this);
                        }
                    }
                });
                return;
            case SKIP_LINK:
                throw new SkipException(job.getChallenge(), SkipRequest.SINGLE);
            }
        }
        checkInterruption();
    }

    public void requestFocus(Challenge<String> challenge) {
        final ChallengeDialogHandler<?, ?, String> handler = this.handler;
        if (handler != null) {
            handler.requestFocus();
        }
    }

    @Override
    public void solve(final SolverJob<String> job) throws InterruptedException, SolverException, SkipException {
        synchronized (DialogBasicCaptchaSolver.getInstance()) {
            checkSilentMode(job);
            if (job.getChallenge() instanceof KeyCaptchaPuzzleChallenge) {
                handler = new KeyCaptchaPuzzleDialogHandler((KeyCaptchaPuzzleChallenge) job.getChallenge());
            } else if (job.getChallenge() instanceof KeyCaptchaCategoryChallenge) {
                handler = new KeyCaptchaCategoryDialogHandler((KeyCaptchaCategoryChallenge) job.getChallenge());
            } else {
                return;
            }
            final ChallengeSolverJobListener jacListener;
            job.getEventSender().addListener(jacListener = new ChallengeSolverJobListener() {
                @Override
                public void onSolverTimedOut(ChallengeSolver<?> parameter) {
                }

                @Override
                public void onSolverStarts(ChallengeSolver<?> parameter) {
                }

                @Override
                public void onSolverJobReceivedNewResponse(AbstractResponse<?> response) {
                    final ResponseList<String> resp = job.getResponse();
                    handler.setSuggest(resp.getValue());
                    job.getLogger().info("Received Suggestion: " + resp);
                }

                @Override
                public void onSolverDone(ChallengeSolver<?> solver) {
                }
            });
            try {
                final ResponseList<String> resp = job.getResponse();
                if (resp != null) {
                    handler.setSuggest(resp.getValue());
                }
                checkInterruption();
                job.getChallenge().sendStatsSolving(this);
                handler.run();
                final String token = handler.getResult();
                if (token != null) {
                    job.addAnswer(new KeyCaptchaResponse(job.getChallenge(), this, token, 100));
                }
            } catch (InterruptedException e) {
                throw e;
            } catch (SkipException e) {
                throw e;
            } catch (Exception e) {
                job.getChallenge().sendStatsError(this, e);
                throw new WTFException(e);
            } finally {
                job.getLogger().info("Dialog closed. Response far: " + job.getResponse());
                if (jacListener != null) {
                    job.getEventSender().removeListener(jacListener);
                }
                handler = null;
            }
        }
    }
}
