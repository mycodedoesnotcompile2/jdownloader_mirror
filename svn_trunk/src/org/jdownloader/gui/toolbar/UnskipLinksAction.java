package org.jdownloader.gui.toolbar;

import java.awt.event.ActionEvent;

import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.proxy.ProxyController;

import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.captcha.blacklist.BlacklistEntry;
import org.jdownloader.captcha.blacklist.BlockAllDownloadCaptchasEntry;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByHost;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByLink;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByPackage;
import org.jdownloader.captcha.blacklist.CaptchaBlackList;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;

public class UnskipLinksAction extends CustomizableAppAction implements ActionContext {

    public UnskipLinksAction() {
        addContextSetup(this);
        setName(_GUI.T.UnskipAllSkippedLinksAction());
        setIconKey(IconKey.ICON_SKIPPED);
    }

    /**
     * @param path
     */
    public void updateIcon(String path) {
        if (StringUtils.equals(ProxyController.getInstance().getLatestProfilePath(), path) && StringUtils.isNotEmpty(path)) {
            setIconKey(IconKey.ICON_PROXY_ROTATE);
        } else {
            setIconKey(IconKey.ICON_PROXY);
        }
    }

    /**
     * Unskips everything: Removes all captcha blocks of the downloads ("Skip all captchas", ...) and resets all skipped links, whatever
     * the reason is. The blocks have to be removed first, otherwise the links would be skipped again.
     */
    public static void unSkipAll() {
        for (final BlacklistEntry entry : CaptchaBlackList.getInstance().listEntries()) {
            if (entry instanceof BlockAllDownloadCaptchasEntry || entry instanceof BlockDownloadCaptchasByHost || entry instanceof BlockDownloadCaptchasByPackage || entry instanceof BlockDownloadCaptchasByLink) {
                CaptchaBlackList.getInstance().remove(entry);
            }
        }
        DownloadWatchDog.getInstance().unSkipAllSkipped();
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        try {
            unSkipAll();

        } catch (Throwable e1) {
            Dialog.getInstance().showExceptionDialog(_GUI.T.lit_error_occured(), e1.getMessage(), e1);
        } finally {

        }
    }

}
