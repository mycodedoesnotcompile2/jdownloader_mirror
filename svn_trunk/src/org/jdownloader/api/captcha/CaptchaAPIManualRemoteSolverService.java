package org.jdownloader.api.captcha;

import java.util.ArrayList;
import java.util.List;

import javax.swing.Icon;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.StringUtils;
import org.jdownloader.api.myjdownloader.MyJDownloaderController;
import org.jdownloader.api.myjdownloader.MyJDownloaderSettings.MyJDownloaderError;
import org.jdownloader.captcha.v2.ChallengeSolver.SolverType;
import org.jdownloader.captcha.v2.solver.service.AbstractSolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.settings.advanced.AdvancedConfigManager;
import org.jdownloader.settings.staticreferences.CFG_MYJD;

import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.views.myjd.MyJDownloaderView;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public class CaptchaAPIManualRemoteSolverService extends AbstractSolverService {
    private CaptchaMyJDownloaderRemoteSolverSettingsV3 config;

    public CaptchaAPIManualRemoteSolverService() {
        config = JsonConfig.create(CaptchaMyJDownloaderRemoteSolverSettingsV3.class);
        AdvancedConfigManager.getInstance().register(config);
    }

    @Override
    public SolverType getType() {
        return SolverType.JD_REMOTE_API;
    }

    @Override
    public Icon getIcon(int size) {
        return new AbstractIcon(IconKey.ICON_LOGO_MYJDOWNLOADER, size);
    }

    @Override
    public String getName() {
        return _GUI.T.CaptchaAPISolver_gettypeName();
    }

    @Override
    public String getDescription() {
        return "Manual captcha solving (NOT automatic) via the MyJDownloader web interface and mobile apps. Requires a MyJDownloader account and only makes sense if you are ready to manually solve captchas on your phone or PC.";
    }

    /**
     * The states the Status column can be in for this solver: connected (ready), no MyJDownloader account configured yet (routine
     * "Configure" case), account configured but currently not connected, or account configured but in an error state (see
     * {@link MyJDownloaderError}). The last two need the user's attention and are shown with a warning icon.
     */
    private enum Status {
        CONNECTED,
        NOT_CONFIGURED,
        NOT_CONNECTED,
        ERROR
    }

    private Status getResolvedStatus() {
        if (MyJDownloaderController.getInstance().isActive()) {
            return Status.CONNECTED;
        }
        if (StringUtils.isEmpty(CFG_MYJD.CFG.getEmail()) || StringUtils.isEmpty(CFG_MYJD.CFG.getPassword())) {
            return Status.NOT_CONFIGURED;
        }
        final MyJDownloaderError latestError = CFG_MYJD.CFG.getLatestError();
        if (latestError == null) {
            return Status.NOT_CONNECTED;
        }
        switch (latestError) {
        case NONE:
            return Status.NOT_CONNECTED;
        default:
            return Status.ERROR;
        }
    }

    @Override
    public String getStatusText() {
        switch (getResolvedStatus()) {
        case CONNECTED:
            return _GUI.T.CaptchaSolverService_status_ready();
        case NOT_CONFIGURED:
        case NOT_CONNECTED:
        case ERROR:
        default:
            /* Not ready -> an action button (see getStatusActionName()) is shown instead. */
            return null;
        }
    }

    @Override
    public String getStatusActionName() {
        switch (getResolvedStatus()) {
        case NOT_CONFIGURED:
            return _GUI.T.CaptchaSolverService_status_configure();
        case NOT_CONNECTED:
            return _GUI.T.CaptchaSolverService_status_myjd_notConnected();
        case ERROR:
            return _GUI.T.CaptchaSolverService_status_myjd_error();
        case CONNECTED:
        default:
            return null;
        }
    }

    @Override
    public boolean isStatusActionWarning() {
        switch (getResolvedStatus()) {
        case NOT_CONNECTED:
        case ERROR:
            return true;
        case NOT_CONFIGURED:
        case CONNECTED:
        default:
            /* "Configure" is a routine action, not a warning. */
            return false;
        }
    }

    @Override
    public void onStatusAction() {
        /* Open the closable My.JDownloader tab (same as the "Open My.JDownloader.org tab" button in the MyJD settings). */
        JDGui.getInstance().setContent(MyJDownloaderView.getInstance(), true);
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.IMAGE_SINGLE_CLICK_CAPTCHA);
        types.add(CAPTCHA_TYPE.IMAGE_MULTI_CLICK_CAPTCHA);
        /* Everything the browser dialog solver can do, too. */
        types.add(CAPTCHA_TYPE.HCAPTCHA);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        /* TODO: Check enterprise handling: https://svn.jdownloader.org/issues/90631 */
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        return types;
    }

    public static final String ID = "myjdremote";

    @Override
    public String getID() {
        return ID;
    }

    @Override
    public CaptchaMyJDownloaderRemoteSolverSettingsV3 getConfigV3() {
        return config;
    }

    @Override
    public String getHelpArticleURL() {
        return "https://support.jdownloader.org/knowledgebase/article/what-is-myjdownloader";
    }
}
