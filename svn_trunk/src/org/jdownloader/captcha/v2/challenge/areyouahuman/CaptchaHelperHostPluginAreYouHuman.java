package org.jdownloader.captcha.v2.challenge.areyouahuman;

import java.awt.Rectangle;

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
import org.jdownloader.captcha.v2.solver.browser.BrowserViewport;
import org.jdownloader.captcha.v2.solver.browser.BrowserWindow;
import org.jdownloader.gui.helpdialogs.HelpDialog;
import org.jdownloader.plugins.CaptchaStepProgress;

public class CaptchaHelperHostPluginAreYouHuman extends AbstractCaptchaHelperAreYouHuman<PluginForHost> implements CaptchaHosterHelperInterface {
    public CaptchaHelperHostPluginAreYouHuman(PluginForHost plugin, Browser br, String siteKey) {
        super(plugin, br, siteKey);
    }

    public CaptchaHelperHostPluginAreYouHuman(PluginForHost plugin, Browser br) {
        this(plugin, br, null);
    }

    public String getToken() throws PluginException, InterruptedException {
        if (Thread.currentThread() instanceof LinkCrawlerThread) {
            logger.severe("PluginForHost.getCaptchaCode inside LinkCrawlerThread!?");
        }
        String apiKey = siteKey;
        if (apiKey == null) {
            apiKey = getAreYouAHumanApiKey();
            if (apiKey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "AreYouAHuman API Key can not be found");
            }
        }
        final PluginForHost plugin = getPlugin();
        final DownloadLink link = getPlugin().getDownloadLink();
        final CaptchaStepProgress progress = new CaptchaStepProgress(0, 1, null);
        progress.setProgressSource(this);
        progress.setDisplayInProgressColumnEnabled(false);
        try {
            link.addPluginProgress(progress);
            final AreYouAHumanChallenge challenge = new AreYouAHumanChallenge(apiKey, getPlugin()) {
                @Override
                public BrowserViewport getBrowserViewport(BrowserWindow screenResource, Rectangle elementBounds) {
                    return null;
                }
            };
            challenge.setTimeout(getPlugin().getChallengeTimeout(challenge));
            if (plugin.isAccountLoginCaptchaChallenge(link, challenge)) {
                /**
                 * account login -> do not use antiCaptcha services
                 */
                challenge.setAccountLogin(true);
            } else {
                final SingleDownloadController controller = link.getDownloadLinkController();
                if (controller != null) {
                    getPlugin().setHasCaptcha(link, controller.getAccount(), true);
                }
            }
            getPlugin().invalidateLastChallengeResponse();
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
                    getPlugin().onCaptchaTimeout(link, e.getChallenge());
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
            link.removePluginProgress(progress);
        }
    }
}
