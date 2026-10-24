package jd.plugins.hoster;

import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.util.EnumSet;
import java.util.Set;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.imageio.ImageIO;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeSolver.SolverFeature;
import org.jdownloader.captcha.v2.PluginChallengeSolver;
import org.jdownloader.captcha.v2.SolverStatus;
import org.jdownloader.captcha.v2.challenge.hcaptcha.HCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickedPoint;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.RecaptchaV2Challenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.CaptchaResponse;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.TokenCaptchaResponse;
import org.jdownloader.captcha.v2.solver.CESSolverJob;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfigNinekw;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;

import jd.PluginWrapper;
import jd.http.requests.FormData;
import jd.http.requests.PostFormDataRequest;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountUnavailableException;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

/**
 * Plugin for 9kw captcha solving service (https://9kw.eu/).
 */
@HostPlugin(revision = "$Revision: 53733 $", interfaceVersion = 3, names = { "9kw.eu" }, urls = { "" })
public class PluginForCaptchaSolverNineKw extends abstractPluginForCaptchaSolver {
    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.CAPTCHA_SOLVER, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION, LazyPlugin.FEATURE.API_KEY_LOGIN };
    }

    public PluginForCaptchaSolverNineKw(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public String getBuyPremiumUrl() {
        return "https://www." + getHost() + "/register.html";
    }

    @Override
    public Set<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final Set<CAPTCHA_TYPE> types = EnumSet.noneOf(CAPTCHA_TYPE.class);
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.IMAGE_SINGLE_CLICK_CAPTCHA);
        types.add(CAPTCHA_TYPE.IMAGE_MULTI_CLICK_CAPTCHA);
        /*
         * All reCaptcha variants are "interactive" captchas on 9kw (variants "recaptchav2" and "recaptchav3", see "interactive" parameter
         * in the API docs). The API has no separate Enterprise switch, so Enterprise captchas are sent like their normal counterparts.
         */
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        /* 2026-10-13: v3 and v3 enterprise does not seem to work -> Always runs into timeout? */
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3_ENTERPRISE);
        /* 2026-10-13: hCaptcha is confirmed working. */
        types.add(CAPTCHA_TYPE.HCAPTCHA);
        // types.add(CAPTCHA_TYPE.GEETEST_V1);
        // types.add(CAPTCHA_TYPE.GEETEST_V4);
        return types;
    }

    @Override
    public Set<SolverFeature> getSolverFeatures() {
        return EnumSet.of(SolverFeature.REPORT_INVALID_CAPTCHAS, SolverFeature.REPORT_VALID_CAPTCHAS, SolverFeature.ABORT_CAPTCHAS);
    }

    protected String getApiBaseV2() {
        return "https://api." + getHost();
    }

    @Override
    public String getAGBLink() {
        return getBaseURL() + "/userapi.html";
    }

    @Override
    protected boolean looksLikeValidAPIKey(final String str) {
        if (str == null) {
            return false;
        }
        return str.matches("[a-zA-Z0-9]{10,}");
    }

    @Override
    protected String getAPILoginHelpURL() {
        return getBaseURL() + "/enterpage";
    }

    private String getBaseURL() {
        return "https://www." + getHost();
    }

    @Override
    public boolean setInvalid(AbstractResponse<?> response, Account account) {
        return sendCaptchaFeedback(response.getCaptchaSolverTaskID(), account, CaptchaFeedback.INCORRECT);
    }

    @Override
    public boolean setValid(AbstractResponse<?> response, Account account) {
        return sendCaptchaFeedback(response.getCaptchaSolverTaskID(), account, CaptchaFeedback.CORRECT);
    }

    /* 9kw has no dedicated cancel action: Feedback "unused" is how a submitted captcha that is no longer needed is released. */
    @Override
    public boolean abortCaptcha(final AbstractResponse<?> response, final Account account) {
        return sendCaptchaFeedback(response.getCaptchaSolverTaskID(), account, CaptchaFeedback.UNUSED);
    }

    /** Possible values of the "correct" parameter of the API action "usercaptchacorrectback". */
    private enum CaptchaFeedback {
        /** The captcha was solved correctly. */
        CORRECT(1),
        /** The captcha was solved incorrectly. */
        INCORRECT(2),
        /** The captcha was not used (e.g. already solved by another solver or no longer needed). */
        UNUSED(3);
        private final int apiValue;

        private CaptchaFeedback(final int apiValue) {
            this.apiValue = apiValue;
        }

        private int getAPIValue() {
            return apiValue;
        }
    }

    private boolean sendCaptchaFeedback(final String captchaSolverTaskID, Account account, final CaptchaFeedback feedback) {
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("action", "usercaptchacorrectback");
        query.appendEncoded("correct", String.valueOf(feedback.getAPIValue()));
        query.appendEncoded("id", captchaSolverTaskID);
        try {
            final Map<String, Object> resp = this.callAPI(query, account);
            final Map<String, Object> status = (Map<String, Object>) resp.get("status");
            return ((Boolean) status.get("success")).booleanValue();
        } catch (final Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(Account account) throws Exception {
        final UrlQuery query = new UrlQuery();
        query.appendEncoded("action", "usercaptchaguthaben");
        final Map<String, Object> entries = this.callAPI(query, account);
        final Double credits = ((Number) entries.get("credits")).doubleValue();
        final AccountInfo ai = new AccountInfo();
        ai.setAccountBalance(credits);
        return ai;
    }

    /**
     * Docs: https://www.9kw.eu/api.html#apigeneral-tab
     *
     * @throws PluginException
     */
    private Map<String, Object> callAPI(final UrlQuery query, final Account account) throws IOException, PluginException {
        return callAPI(query, account, null);
    }

    /**
     * @param imageData
     *            If given, the request is sent as multipart/form-data POST with this image as "file-upload-01" (the docs require POST for
     *            usercaptchaupload). Sending the image as base64 inside a GET url is too large and gets the connection reset.
     */
    private Map<String, Object> callAPI(final UrlQuery query, final Account account, final byte[] imageData) throws IOException, PluginException {
        query.appendEncoded("json", "1");
        query.appendEncoded("apikey", account.getPass());
        /* Potentially unneeded params */
        /**
         * 2026-03-17: Do not add jd=2 parameter as this will make API return non-json responses for some cases but we want json whenever
         * possible. </br>
         * Known effects when this parameter is sent: <br>
         * - Sometimes non-json responses <br>
         * - "captcha_id" field instead of "captchaid" <br>
         */
        // query.appendEncoded("jd", "2");
        query.appendEncoded("source", "jd2");
        query.appendEncoded("captchaSource", "jdPlugin");
        query.appendEncoded("version", "1.2");
        if (imageData != null) {
            final PostFormDataRequest request = br.createPostFormDataRequest(getBaseURL() + "/index.cgi");
            for (final Map.Entry<String, String> entry : query.toMap(true).entrySet()) {
                request.addFormData(new FormData(entry.getKey(), entry.getValue()));
            }
            request.addFormData(new FormData("file-upload-01", "captcha.png", "image/png", imageData));
            br.getPage(request);
        } else {
            br.getPage(getBaseURL() + "/index.cgi?" + query.toString());
        }
        /* Check for non-json response. This is the best workaround I found in order to "keep things pretty". */
        /* See list of possible errors here: https://www.9kw.eu/api.html#apigeneral-tab */
        final Regex non_json_error_regex = br.getRegex("(\\d{4}) (.+)");
        if (non_json_error_regex.patternFind()) {
            final int error_code = Integer.parseInt(non_json_error_regex.getMatch(0));
            final String error_msg = non_json_error_regex.getMatch(1);
            throwForErrorCode(error_code, error_msg);
        }
        final Regex captcha_upload_success = br.getRegex("OK-(\\d+)");
        if (captcha_upload_success.patternFind()) {
            final Map<String, Object> resp = new HashMap<String, Object>();
            resp.put("captcha_id", captcha_upload_success.getMatch(0));
            return resp;
        }
        final Regex captcha_response_success = br.getRegex("OK-answered-(.+)");
        if (captcha_response_success.patternFind()) {
            final Map<String, Object> resp = new HashMap<String, Object>();
            resp.put("answer", captcha_response_success.getMatch(0));
            return resp;
        }
        if (br.getRequest().getHtmlCode().equalsIgnoreCase("OK")) {
            // TODO: Check if this case still exists
            return null;
        }
        /* Expect json response */
        final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        final Map<String, Object> status = (Map<String, Object>) entries.get("status");
        if (Boolean.TRUE.equals(status.get("success"))) {
            /* No error */
            return entries;
        }
        final String error = (String) entries.get("error");
        final String errorNumber = new Regex(error, "^(\\d{1,4})").getMatch(0);
        if (errorNumber == null) {
            throw new AccountInvalidException(error);
        }
        throwForErrorCode(Integer.parseInt(errorNumber), error);
        /* Unreachable: throwForErrorCode always throws. */
        throw new WTFException();
    }

    /**
     * Classifies a 9kw error code (see https://www.9kw.eu/api.html#apigeneral-tab) as a permanent account error, a temporary account error,
     * or a plain captcha error, and throws the matching exception. Always throws.
     */
    private void throwForErrorCode(final int errorcode, final String message) throws PluginException {
        switch (errorcode) {
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
        case 11:
        case 24:
        case 26:
        case 30:
        case 32:
        case 34:
        case 35:
        case 36:
        case 55:
            /*
             * 1-5: no/inactive/deactivated API key or no matching account found. 11 & 24: insufficient balance (two separate error codes
             * for the same underlying problem). 26: terms of service not accepted. 30: user/account not found. 32: account temporarily or
             * permanently restricted by the operator. 34/35: https/http requests are not allowed by the account settings. 36: source not
             * allowed. 55: IP not allowed. All of these need a manual change by the user (or support) -> permanent.
             */
            throw new AccountInvalidException(message);
        case 31:
            /* Account is not yet 24h old -> resolves itself, purely temporary. */
            throw new AccountUnavailableException(message, 24 * 60 * 60 * 1000L);
        case 33:
            /* This plugin/API version is not accepted anymore. Only a JDownloader update helps -> check again later. */
            throw new AccountUnavailableException(message, 60 * 60 * 1000L);
        case 56:
            /* A limit (e.g. captchas per hour/minute of the 9kw account) is reached -> resolves itself. */
            throw new AccountUnavailableException(message, 15 * 60 * 1000L);
        case 15:
            /* Captchas were submitted too quickly (min. 250ms between two requests) -> only a short pause is needed. */
            throw new AccountUnavailableException(message, 10 * 1000L);
        default:
            /* Captcha error */
            throw new PluginException(LinkStatus.ERROR_CAPTCHA, message);
        }
    }

    /**
     * The 9kw API docs recommend the first request for a solution after 5-10 seconds and further requests every few seconds, so do not poll
     * faster than every 5 seconds, no matter what the user configured.
     */
    @Override
    public long getServerSideMinPollingIntervalMillis() {
        return 5000;
    }

    /**
     * 9kw rejects images whose width + height exceeds this value (error 0010 "Size is too big", e.g. 640+640=1280>800).
     */
    private static final int MAX_IMAGE_WIDTH_PLUS_HEIGHT = 800;

    /**
     * Result of {@link #prepareImage(File)}: the PNG bytes to upload and the factors the image was scaled with per axis (1.0 = unchanged).
     */
    private static class PreparedImage {
        private final byte[] data;
        private final double scaleX;
        private final double scaleY;

        private PreparedImage(final byte[] data, final double scaleX, final double scaleY) {
            this.data = data;
            this.scaleX = scaleX;
            this.scaleY = scaleY;
        }
    }

    /**
     * Loads the image into RAM and downscales it (keeping the aspect ratio) so that width + height does not exceed the limit of the 9kw
     * API. Images that are small enough are not scaled.
     */
    private PreparedImage prepareImage(final File imageFile) throws IOException {
        final BufferedImage source = ImageIO.read(imageFile);
        if (source == null) {
            throw new IOException("Unsupported image format: " + imageFile);
        }
        final int width = source.getWidth();
        final int height = source.getHeight();
        BufferedImage target = source;
        double scaleX = 1.0;
        double scaleY = 1.0;
        if (width + height > MAX_IMAGE_WIDTH_PLUS_HEIGHT) {
            final double scale = (double) MAX_IMAGE_WIDTH_PLUS_HEIGHT / (width + height);
            final int newWidth = Math.max(1, (int) Math.floor(width * scale));
            final int newHeight = Math.max(1, (int) Math.floor(height * scale));
            /* Use the real factors per axis (rounding to whole pixels) so click coordinates can be mapped back precisely. */
            scaleX = (double) newWidth / width;
            scaleY = (double) newHeight / height;
            target = new BufferedImage(newWidth, newHeight, BufferedImage.TYPE_INT_RGB);
            final Graphics2D g = target.createGraphics();
            try {
                g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
                g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
                g.drawImage(source, 0, 0, newWidth, newHeight, null);
            } finally {
                g.dispose();
            }
            logger.info("Downscaled image from " + width + "x" + height + " to " + newWidth + "x" + newHeight + " (9kw limit: width+height <= " + MAX_IMAGE_WIDTH_PLUS_HEIGHT + ")");
        }
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        ImageIO.write(target, "png", bos);
        return new PreparedImage(bos.toByteArray(), scaleX, scaleY);
    }

    @Override
    public void solve(CESSolverJob<?> job, Account account) throws Exception {
        job.setStatus(SolverStatus.UPLOADING);
        final UrlQuery upload_query = new UrlQuery();
        upload_query.appendEncoded("action", "usercaptchaupload");
        final Challenge<?> captchachallenge = job.getChallenge();
        /* Only set for image based challenges. Sent as file upload instead of base64. */
        File imageFile = null;
        PreparedImage preparedImage = null;
        if (captchachallenge instanceof RecaptchaV2Challenge) {
            final RecaptchaV2Challenge challenge = (RecaptchaV2Challenge) captchachallenge;
            upload_query.appendEncoded("data-sitekey", challenge.getSiteKey());
            upload_query.appendEncoded("isInvisible", challenge.isInvisible() == true ? "1" : "0");
            /* Parameter "captchachoice" is not in the docs (anymore), "oldsource" below is enough. */
            final Map<String, Object> v3action = challenge.getV3Action();
            upload_query.appendEncoded("pageurl", challenge.getSiteUrl(this));
            if (v3action != null) {
                upload_query.appendEncoded("actionname", (String) v3action.get("action"));
                /* Minimal score: requested by the challenge or 0.3 by default. */
                final Double minScore = challenge.getMinScore();
                upload_query.appendEncoded("min_score", String.valueOf(minScore != null ? minScore : 0.3d));
            }
            upload_query.appendEncoded("interactive", "1");
            upload_query.appendEncoded("securetoken", challenge.getSecureToken());
            if (v3action != null || challenge.isV3()) {
                upload_query.appendEncoded("oldsource", "recaptchav3");
            } else {
                upload_query.appendEncoded("oldsource", "recaptchav2");
            }
        } else if (captchachallenge instanceof HCaptchaChallenge) {
            final HCaptchaChallenge challenge = (HCaptchaChallenge) captchachallenge;
            upload_query.appendEncoded("data-sitekey", challenge.getSiteKey());
            upload_query.appendEncoded("pageurl", challenge.getSiteUrl(this));
            upload_query.appendEncoded("oldsource", "hcaptcha");
            upload_query.appendEncoded("interactive", "1");
        } else if (captchachallenge instanceof MultiClickCaptchaChallenge) {
            /* Coordinates task: https://2captcha.com/api-docs/coordinates */
            final MultiClickCaptchaChallenge challenge = (MultiClickCaptchaChallenge) captchachallenge;
            /* Single click captchas have their own parameter. */
            upload_query.appendEncoded(challenge.isSingleClick() ? "mouse" : "multimouse", "1");
            imageFile = challenge.getImageFile();
        } else if (captchachallenge instanceof ImageCaptchaChallenge) {
            final ImageCaptchaChallenge<?> challenge = (ImageCaptchaChallenge<?>) captchachallenge;
            imageFile = challenge.getImageFile();
            if (Boolean.TRUE.equals(challenge.getCaseSensitive())) {
                /* 0 = disabled (default), 1 = case sensitive */
                upload_query.appendEncoded("case-sensitive", "1");
            }
            if (Boolean.TRUE.equals(challenge.getNumeric())) {
                /* 0 = disabled (default), 1 = digits only (0-9) */
                upload_query.appendEncoded("numeric", "1");
            }
        } else {
            throw new IllegalArgumentException("Unexpected captcha challenge type");
        }
        /*
         * maxtimeout is in SECONDS. Use the time JDownloader itself waits for this solver (see ChallengeSolver#getFinalTimeoutMillis, what
         * JobRunnable enforces), so 9kw does not give up on the captcha before we do. Limits of the 9kw API: the old JDownloader 9kw
         * setting allowed 75 - 3999 seconds, so keep the value within that range. Before, this was Math.min(60, Challenge#getTimeout()),
         * which is in milliseconds and therefore always resulted in 60 (or -1 for challenges without timeout).
         */
        final long timeoutSeconds = new PluginChallengeSolver<Object>(this, account).getFinalTimeoutMillis() / 1000;
        final long minTimeoutSeconds = 75;
        final long maxTimeoutSeconds = 3999;
        upload_query.appendEncoded("maxtimeout", Math.max(minTimeoutSeconds, Math.min(maxTimeoutSeconds, timeoutSeconds)) + "");
        if (captchachallenge.getExplain() != null) {
            upload_query.appendEncoded("textinstructions", captchachallenge.getExplain());
        }
        final CaptchaSolverPluginConfigNinekw cfg = PluginJsonConfig.get(this.getConfigInterface());
        upload_query.appendEncoded("prio", cfg.getPrio() + "");
        upload_query.appendEncoded("selfsolve", cfg.isSelfsolve() + "");
        upload_query.appendEncoded("confirm", cfg.isConfirm() + "");
        if (imageFile != null) {
            preparedImage = prepareImage(imageFile);
        }
        final Map<String, Object> uploadresp = this.callAPI(upload_query, account, preparedImage != null ? preparedImage.data : null);
        final String captcha_id = uploadresp.get("captchaid").toString();
        job.setCaptchaSolverTaskID(captcha_id);
        final UrlQuery polling_query = new UrlQuery();
        polling_query.appendEncoded("action", "usercaptchacorrectdata");
        polling_query.appendEncoded("id", captcha_id);
        /* Wait for captcha answer */
        job.setStatus(SolverStatus.SOLVING);
        while (job.getJob().isAlive() && !job.getJob().isSolved()) {
            waitDuringPolling(job.getChallenge(), account);
            final Map<String, Object> pollingresp = this.callAPI(polling_query, account);
            final Number credits = (Number) pollingresp.get("credits");
            if (credits != null) {
                try {
                    account.getAccountInfo().setAccountBalance(credits.doubleValue());
                } catch (final Exception e) {
                }
            }
            final Number try_again = (Number) pollingresp.get("try_again");
            if (try_again != null && try_again.shortValue() == 1) {
                /* {"credits":40628,"message":"OK","try_again":1,"answer":"","status":{"https":1,"success":true}} */
                logger.info("No response yet -> Retry");
                continue;
            }
            final String answer = (String) pollingresp.get("answer");
            if (StringUtils.isEmpty(answer)) {
                /* No error && no retry allowed && no answer -> Unsolved for unknown reasons */
                logger.info("No answer and no retry allowed anymore -> Stopping polling");
                job.setStatus(SolverStatus.UNSOLVED);
                return;
            }
            final AbstractResponse resp;
            if (captchachallenge instanceof RecaptchaV2Challenge || captchachallenge instanceof HCaptchaChallenge) {
                resp = new TokenCaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), answer);
            } else if (captchachallenge instanceof MultiClickCaptchaChallenge) {
                // TODO: Test this
                /* e.g. "68x149;81x192", a single click captcha has just one pair */
                final String[] pairs = answer.split(";");
                final int[] x = new int[pairs.length];
                final int[] y = new int[pairs.length];
                for (int i = 0; i < pairs.length; i++) {
                    final String[] xy = pairs[i].split("x");
                    /* Coordinates refer to the (possibly downscaled) uploaded image -> map them back to the original size. */
                    x[i] = (int) Math.round(Integer.parseInt(xy[0]) / preparedImage.scaleX);
                    y[i] = (int) Math.round(Integer.parseInt(xy[1]) / preparedImage.scaleY);
                }
                resp = new AbstractResponse<MultiClickedPoint>((MultiClickCaptchaChallenge) captchachallenge, job.getSolver(), new MultiClickedPoint(x, y));
            } else {
                resp = new CaptchaResponse((Challenge<String>) captchachallenge, job.getSolver(), answer);
            }
            resp.setCaptchaSolverTaskID(captcha_id);
            job.setAnswer(resp);
            return;
        }
    }

    @Override
    public Class<? extends CaptchaSolverPluginConfigNinekw> getConfigInterface() {
        return CaptchaSolverPluginConfigNinekw.class;
    }
}