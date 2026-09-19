package org.jdownloader.captcha.v2.solver.service;

import java.util.ArrayList;
import java.util.List;

import javax.swing.Icon;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;

import org.appwork.storage.config.JsonConfig;
import org.jdownloader.captcha.v2.solver.gui.DialogCaptchaSolverConfigV3;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;

public class DialogSolverService extends AbstractSolverService {
    public static final String                 ID       = "dialog";
    private static final DialogSolverService   INSTANCE = new DialogSolverService();
    private static DialogCaptchaSolverConfigV3 config;

    public static DialogSolverService getInstance() {
        if (config == null) {
            config = JsonConfig.create(DialogCaptchaSolverConfigV3.class);
        }
        return INSTANCE;
    }

    @Override
    public String getType() {
        return _GUI.T.DialogBasicCaptchaSolver_getName();
    }

    @Override
    public Icon getIcon(int size) {
        return NewTheme.I().getIcon(IconKey.ICON_OCR, size);
    }

    @Override
    public String getName() {
        return "Dialog in JDownloader";
    }

    @Override
    public String getDescription() {
        return "Manual captcha solving inside JDownloader. Supports only static image captchas.";
    }

    @Override
    public List<CAPTCHA_TYPE> getSupportedCaptchaTypes() {
        final List<CAPTCHA_TYPE> types = new ArrayList<CAPTCHA_TYPE>();
        types.add(CAPTCHA_TYPE.IMAGE);
        types.add(CAPTCHA_TYPE.IMAGE_SINGLE_CLICK_CAPTCHA);
        types.add(CAPTCHA_TYPE.IMAGE_MULTI_CLICK_CAPTCHA);
        return types;
    }

    @Override
    public boolean hasConfigPanel() {
        return true;
    }

    @Override
    public DialogCaptchaSolverConfigV3 getConfigV3() {
        if (config == null) {
            config = JsonConfig.create(DialogCaptchaSolverConfigV3.class);
        }
        return config;
    }

    @Override
    public String getID() {
        return ID;
    }
}
