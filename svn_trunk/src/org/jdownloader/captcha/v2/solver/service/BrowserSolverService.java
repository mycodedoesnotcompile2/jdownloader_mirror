package org.jdownloader.captcha.v2.solver.service;

import java.util.HashMap;
import java.util.Map;

import javax.swing.Icon;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.captcha.v2.solver.browser.BrowserCaptchaSolverConfig;
import org.jdownloader.captcha.v2.solver.cheapcaptcha.CheapCaptchaSolverService;
import org.jdownloader.captcha.v2.solver.dbc.DeathByCaptchaSolverService;
import org.jdownloader.captcha.v2.solver.endcaptcha.EndCaptchaSolverService;
import org.jdownloader.captcha.v2.solver.imagetyperz.ImageTyperzSolverService;
import org.jdownloader.captcha.v2.solver.jac.JacSolverService;
import org.jdownloader.captcha.v2.solver.solver9kw.NineKwSolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;

import jd.gui.swing.jdgui.views.settings.panels.anticaptcha.AbstractCaptchaSolverConfigPanel;

public class BrowserSolverService extends AbstractSolverService {
    public static final String                ID       = "browser";
    private static final BrowserSolverService INSTANCE = new BrowserSolverService();
    private static BrowserCaptchaSolverConfig config;

    public static BrowserSolverService getInstance() {
        if (config == null) {
            config = JsonConfig.create(BrowserCaptchaSolverConfig.class);
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
        return _GUI.T.BrowserSolverService_gettypeName();
    }

    @Override
    public AbstractCaptchaSolverConfigPanel getConfigPanel() {
        AbstractCaptchaSolverConfigPanel ret = new AbstractCaptchaSolverConfigPanel() {
            {
                addHeader(getTitle(), BrowserSolverService.this.getIcon(32));
                addDescription(BrowserSolverService.this.getType());
                addBlackWhiteList(config);
            }

            @Override
            public Icon getIcon() {
                return BrowserSolverService.this.getIcon(32);
            }

            @Override
            public String getPanelID() {
                return "JAC_" + getTitle();
            }

            @Override
            public String getTitle() {
                return BrowserSolverService.this.getName();
            }

            @Override
            public void save() {
            }

            @Override
            public void updateContents() {
            }
        };
        return ret;
    }

    @Override
    public boolean hasConfigPanel() {
        return true;
    }

    @Override
    public BrowserCaptchaSolverConfig getConfig() {
        return JsonConfig.create(BrowserCaptchaSolverConfig.class);
    }

    @Override
    public Map<String, Integer> getWaitForOthersDefaultMap() {
        HashMap<String, Integer> ret = new HashMap<String, Integer>();
        // ret.put(DialogClickCaptchaSolver.ID, 0);
        // ret.put(DialogBasicCaptchaSolver.ID, 0);
        // ret.put(CaptchaAPISolver.ID, 0);
        ret.put(JacSolverService.ID, 30000);
        ret.put(NineKwSolverService.ID, 300000);
        // ret.put(CaptchaMyJDSolverService.ID, 60000);
        ret.put(DeathByCaptchaSolverService.ID, 60000);
        ret.put(ImageTyperzSolverService.ID, 60000);
        ret.put(CheapCaptchaSolverService.ID, 60000);
        ret.put(EndCaptchaSolverService.ID, 60000);
        return ret;
    }

    @Override
    public String getID() {
        return ID;
    }
}
