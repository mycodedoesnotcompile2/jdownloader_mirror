package org.jdownloader.captcha.v2.solver.service;

import java.util.ArrayList;
import java.util.List;

import javax.swing.Icon;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.captcha.v2.solver.browser.BrowserCaptchaSolverConfigV3;
import org.jdownloader.controlling.browser.ExternalBrowserManager;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public class BrowserSolverService extends AbstractSolverService {
    public static final String                  ID       = "browser";
    private static final BrowserSolverService   INSTANCE  = new BrowserSolverService();
    private static BrowserCaptchaSolverConfigV3 config;

    public static BrowserSolverService getInstance() {
        if (config == null) {
            config = JsonConfig.create(BrowserCaptchaSolverConfigV3.class);
        }
        return INSTANCE;
    }

    public boolean isOpenBrowserSupported() {
        String[] browserCommandLine = BrowserSolverService.getInstance().getConfig().getBrowserCommandline();
        if (browserCommandLine == null || browserCommandLine.length == 0) {
            browserCommandLine = CFG_GENERAL.BROWSER_COMMAND_LINE.getValue();
        }
        return CrossSystem.isOpenBrowserSupported() || CrossSystem.buildBrowserCommandline(browserCommandLine, "https://jdownloader.org") != null;
    }

    @Override
    public String getType() {
        return _GUI.T.BrowserSolverService_getName();
    }

    @Override
    public Icon getIcon(int size) {
        return NewTheme.I().getIcon(IconKey.ICON_OCR, size);
    }

    @Override
    public String getName() {
        return "Dialog in Browser (Chrome, Firefox, Edge)";
    }

    @Override
    public String getDescription() {
        return "Manual local captcha solving in your own browser. Requires our MyJDownloader browser extension. Does NOT require a MyJDownloader account!";
    }

    @Override
    public String getStatusText() {
        final String[] commandline = getConfig().getBrowserCommandline();
        if (commandline != null && commandline.length > 0) {
            final String browserName = ExternalBrowserManager.getInstance().getLazyBrowserName(commandline);
            if (browserName != null) {
                return "Ready | using " + browserName;
            }
            return "Ready | using custom browser";
        }
        return "Ready | using OS default browser";
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.HCAPTCHA);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V3_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_INVISIBLE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2_ENTERPRISE);
        types.add(CAPTCHA_TYPE.RECAPTCHA_V2);
        return types;
    }

    @Override
    public boolean hasConfigPanel() {
        return true;
    }

    /** Typed config accessor used by the browser solver subsystem. */
    public BrowserCaptchaSolverConfigV3 getConfig() {
        if (config == null) {
            config = JsonConfig.create(BrowserCaptchaSolverConfigV3.class);
        }
        return config;
    }

    @Override
    public BrowserCaptchaSolverConfigV3 getConfigV3() {
        return getConfig();
    }

    @Override
    public String getHelpArticleURL() {
        return "https://support.jdownloader.org/de/knowledgebase/article/jd-opens-my-browser-to-display-captchas";
    }

    @Override
    public String getID() {
        return ID;
    }
}
