package org.jdownloader.captcha.v2.challenge.sweetcaptcha;

import java.awt.Rectangle;

import jd.controlling.captcha.SkipException;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.controlling.linkcrawler.LinkCrawlerThread;
import jd.http.Browser;
import jd.parser.html.Form;
import jd.plugins.CaptchaException;
import jd.plugins.DecrypterException;
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

public class CaptchaHelperHostPluginSweetCaptcha extends AbstractCaptchaHelperSweetCaptcha<PluginForHost> implements CaptchaHosterHelperInterface {
    public CaptchaHelperHostPluginSweetCaptcha(final PluginForHost plugin, final Browser br, final String siteKey, final String appKey) {
        super(plugin, br, siteKey, appKey);
    }

    public CaptchaHelperHostPluginSweetCaptcha(final PluginForHost plugin, final Browser br) {
        this(plugin, br, null, null);
    }

    public String getToken() throws PluginException, InterruptedException {
        if (Thread.currentThread() instanceof LinkCrawlerThread) {
            logger.severe("PluginForHost.getCaptchaCode inside LinkCrawlerThread!?");
        }
        final PluginForHost plugin = this.plugin;
        final DownloadLink link = plugin.getDownloadLink();
        String appkey = appKey;
        if (appkey == null) {
            appkey = getSweetCaptchaAppKey();
            if (appkey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "SweetCaptcha AppKey can not be found");
            }
        }
        String sitekey = siteKey;
        if (sitekey == null) {
            sitekey = getSweetCaptchaApiKey();
            if (sitekey == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "SweetCaptcha SiteKey can not be found");
            }
        }
        final CaptchaStepProgress progress = new CaptchaStepProgress(0, 1, null);
        progress.setProgressSource(this);
        progress.setDisplayInProgressColumnEnabled(false);
        try {
            link.addPluginProgress(progress);
            final SweetCaptchaChallenge challenge = new SweetCaptchaChallenge(sitekey, appkey, plugin) {
                @Override
                public BrowserViewport getBrowserViewport(BrowserWindow screenResource, Rectangle elementBounds) {
                    return null;
                }
            };
            challenge.setTimeout(plugin.getChallengeTimeout(challenge));
            if (plugin.isAccountLoginCaptchaChallenge(link, challenge)) {
                /**
                 * account login -> do not use anticaptcha services
                 */
                challenge.setAccountLogin(true);
            } else {
                final SingleDownloadController controller = link.getDownloadLinkController();
                if (controller != null) {
                    plugin.setHasCaptcha(link, controller.getAccount(), true);
                }
            }
            plugin.invalidateLastChallengeResponse();
            final BlacklistEntry<?> blackListEntry = CaptchaBlackList.getInstance().matches(challenge);
            if (blackListEntry != null) {
                logger.warning("Cancel. Blacklist Matching");
                throw new CaptchaException(blackListEntry);
            }
            ChallengeResponseController.getInstance().handle(challenge);
            if (!challenge.isSolved()) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            }
            return challenge.getResult().getValue();
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

                    break;
                case REFRESH:
                    // we should forward the refresh request to a new pluginstructure soon. For now. the plugin will just retry
                    return "";
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

    public Form setFormValues(final Form form) throws PluginException, InterruptedException, DecrypterException {
        if (form == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Form not provided");
        }
        return setFormValues(form, this.getToken());
    }
}
