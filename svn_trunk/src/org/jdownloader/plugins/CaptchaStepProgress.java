package org.jdownloader.plugins;

import java.awt.Color;

import javax.swing.Icon;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.PluginProgress;

import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.downloads.columns.ETAColumn;
import org.jdownloader.images.NewTheme;
import org.jdownloader.translate._JDT;

public final class CaptchaStepProgress extends PluginProgress {
    private final CAPTCHA_TYPE captchaType;

    /**
     * @param captchaType
     *            The type of the captcha which is solved, may be null (then a general icon and text are shown).
     */
    public CaptchaStepProgress(long current, long total, Color color, CAPTCHA_TYPE captchaType) {
        super(current, total, color);
        this.captchaType = captchaType;
        setIcon(NewTheme.I().getIcon(IconKey.ICON_OCR, 16));
    }

    @Override
    public Icon getIcon(Object requestor) {
        if (requestor != null) {
            if (requestor instanceof ETAColumn) { return null; }
        }
        if (captchaType != null) {
            /* The icon of the captcha type (the favicon may be loaded in the background, so it is asked for every time). */
            final Icon icon = captchaType.getIcon();
            if (icon != null) {
                return icon;
            }
        }
        return super.getIcon(requestor);
    }

    @Override
    public String getMessage(Object requestor) {
        if (requestor != null) {
            if (requestor instanceof ETAColumn) { return null; }
        }
        if (captchaType != null) {
            return _JDT.T.gui_downloadview_statustext_solving(captchaType.getDisplayName());
        }
        return _JDT.T.gui_downloadview_statustext_jac();
    }

    @Override
    public PluginTaskID getID() {
        return PluginTaskID.CAPTCHA;
    }
}