package org.jdownloader.api.captcha;

import java.util.ArrayList;
import java.util.List;

import javax.swing.Icon;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.views.myjd.MyJDownloaderView;

import org.appwork.storage.config.JsonConfig;
import org.jdownloader.api.myjdownloader.MyJDownloaderController;
import org.jdownloader.captcha.v2.solver.service.AbstractSolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.settings.advanced.AdvancedConfigManager;

public class CaptchaAPIManualRemoteSolverService extends AbstractSolverService {
    private CaptchaMyJDownloaderRemoteSolverSettingsV3 config;

    public CaptchaAPIManualRemoteSolverService() {
        config = JsonConfig.create(CaptchaMyJDownloaderRemoteSolverSettingsV3.class);
        AdvancedConfigManager.getInstance().register(config);
    }

    @Override
    public String getType() {
        return _GUI.T.CaptchaAPISolver_getName();
    }

    @Override
    public Icon getIcon(int size) {
        return new AbstractIcon(IconKey.ICON_LOGO_MYJDOWNLOADER, size);
    }

    @Override
    public boolean hasConfigPanel() {
        return true;
    }

    @Override
    public String getName() {
        return _GUI.T.CaptchaAPISolver_gettypeName();
    }

    @Override
    public String getDescription() {
        return "Manual captcha solving (NOT automatic) via the MyJDownloader web interface and mobile apps. Requires a MyJDownloader account and only makes sense if you are ready to manually solve captchas on your phone or PC.";
    }

    @Override
    public String getStatusText() {
        if (MyJDownloaderController.getInstance().isConnected()) {
            return "Ready";
        }
        /* No MyJDownloader account entered or not connected -> show the Configure action button instead. */
        return null;
    }

    @Override
    public String getStatusActionName() {
        if (MyJDownloaderController.getInstance().isConnected()) {
            return null;
        }
        return "Configure";
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
