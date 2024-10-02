package org.jdownloader.captcha.v2.challenge.cutcaptcha;

import jd.controlling.captcha.SkipException;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.controlling.linkcrawler.LinkCrawlerThread;
import jd.http.Browser;
import jd.plugins.CaptchaException;
import jd.plugins.DownloadLink;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

import org.appwork.utils.logging2.LogSource;
import org.jdownloader.captcha.blacklist.BlacklistEntry;
import org.jdownloader.captcha.blacklist.BlockAllDownloadCaptchasEntry;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByHost;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByLink;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByPackage;
import org.jdownloader.captcha.blacklist.CaptchaBlackList;
import org.jdownloader.captcha.v2.CaptchaHosterHelperInterface;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.gui.helpdialogs.HelpDialog;
import org.jdownloader.plugins.CaptchaStepProgress;

public class CaptchaHelperHostPluginCutCaptcha extends AbstractCaptchaHelperCutCaptcha<PluginForHost> implements CaptchaHosterHelperInterface {
    public CaptchaHelperHostPluginCutCaptcha(PluginForHost plugin, Browser br, String siteKey) {
        super(plugin, br, siteKey);
    }

    public CaptchaHelperHostPluginCutCaptcha(PluginForHost plugin, Browser br) {
        this(plugin, br, null);
    }

    public String getToken() throws PluginException, InterruptedException {
        logger.info("SiteKey:" + getSiteKey());
        if (Thread.currentThread() instanceof LinkCrawlerThread) {
            logger.severe("PluginForHost.getCaptchaCode inside LinkCrawlerThread!?");
        }
        final CutCaptchaChallenge challenge = createChallenge();
        final PluginForHost plugin = getPlugin();
        final DownloadLink link = plugin.getDownloadLink();
        final CaptchaStepProgress progress = new CaptchaStepProgress(0, 1, null);
        progress.setProgressSource(this);
        progress.setDisplayInProgressColumnEnabled(false);
        try {
            if (link != null) {
                link.addPluginProgress(progress);
            }
            challenge.setTimeout(plugin.getChallengeTimeout(challenge));
            if (plugin.isAccountLoginCaptchaChallenge(link, challenge)) {
                /**
                 * account login -> do not use antiCaptcha services
                 */
                challenge.setAccountLogin(true);
            } else {
                final SingleDownloadController controller = link != null ? link.getDownloadLinkController() : null;
                if (controller != null) {
                    plugin.setHasCaptcha(link, controller.getAccount(), true);
                }
            }
            plugin.invalidateLastChallengeResponse();
            final BlacklistEntry<?> blackListEntry = CaptchaBlackList.getInstance().matches(challenge);
            if (blackListEntry != null) {
                logger.warning("Cancel. Blacklist Matching");
                throw new CaptchaException(blackListEntry);
            } else {
                ChallengeResponseController.getInstance().handle(challenge);
                if (!challenge.isSolved()) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA);
                } else if (!challenge.isCaptchaResponseValid()) {
                    throw new PluginException(LinkStatus.ERROR_CAPTCHA, "Captcha reponse value did not validate!");
                } else {
                    return challenge.getResult().getValue();
                }
            }
        } catch (InterruptedException e) {
            LogSource.exception(logger, e);
            throw e;
        } catch (SkipException e) {
            LogSource.exception(logger, e);
            if (link != null) {
                switch (e.getSkipRequest()) {
                case BLOCK_ALL_CAPTCHAS:
                    CaptchaBlackList.getInstance().add(new BlockAllDownloadCaptchasEntry());
                    HelpDialog.showCaptchaSkippedDialog();
                    break;
                case BLOCK_HOSTER:
                    CaptchaBlackList.getInstance().add(new BlockDownloadCaptchasByHost(link.getHost()));
                    HelpDialog.showCaptchaSkippedDialog();
                    break;
                case BLOCK_PACKAGE:
                    CaptchaBlackList.getInstance().add(new BlockDownloadCaptchasByPackage(link.getParentNode()));
                    HelpDialog.showCaptchaSkippedDialog();
                    break;
                case TIMEOUT:
                    plugin.onCaptchaTimeout(link, e.getChallenge());
                    // TIMEOUT may fallthrough to SINGLE
                case SINGLE:
                    CaptchaBlackList.getInstance().add(new BlockDownloadCaptchasByLink(e.getSkipRequest(), link));
                    HelpDialog.showCaptchaSkippedDialog();
                    break;
                case REFRESH:
                    break;
                case STOP_CURRENT_ACTION:
                    if (Thread.currentThread() instanceof SingleDownloadController) {
                        DownloadWatchDog.getInstance().stopDownloads();
                    }
                    break;
                }
            }
            throw new CaptchaException(e.getSkipRequest());
        } finally {
            if (link != null) {
                link.removePluginProgress(progress);
            }
        }
    }
}
