package org.jdownloader.captcha.v2.solver.jac;

import org.jdownloader.captcha.v2.ChallengeSolver.SolverType;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Icon;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

import org.appwork.storage.config.JsonConfig;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.captcha.v2.solver.service.AbstractSolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.advanced.AdvancedConfigManager;

public class JacSolverService extends AbstractSolverService implements SolverService {
    private JacSolverConfigV3 config;

    public JacSolverService() {
        config = JsonConfig.create(JacSolverConfigV3.class);
        AdvancedConfigManager.getInstance().register(config);
    }

    public static final String ID = "jac";

    @Override
    public Icon getIcon(int size) {
        return NewTheme.I().getIcon(IconKey.ICON_OCR, size);
    }

    @Override
    public SolverType getType() {
        return SolverType.JD_LOCAL;
    }

    @Override
    public String getID() {
        return ID;
    }

    @Override
    public String getName() {
        return _GUI.T.JACSolver_gettypeName_();
    }

    @Override
    public String getDescription() {
        return "Legacy automated local captcha solving that automatically solves a few older, specific image captcha types.";
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        /* JAC only solves plain image captchas, see JACSolver#getSupportedCaptchaTypes. */
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        return types;
    }

    @Override
    public JacSolverConfigV3 getConfigV3() {
        return config;
    }

}
