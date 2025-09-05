//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.UnknownHostException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.ComboBoxModel;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.Timer;
import javax.swing.table.JTableHeader;

import jd.PluginWrapper;
import jd.captcha.JACMethod;
import jd.config.SubConfiguration;
import jd.controlling.AccountController;
import jd.controlling.captcha.CaptchaSettings;
import jd.controlling.captcha.SkipException;
import jd.controlling.captcha.SkipRequest;
import jd.controlling.downloadcontroller.AccountCache.ACCOUNTTYPE;
import jd.controlling.downloadcontroller.DiskSpaceManager.DISKSPACERESERVATIONRESULT;
import jd.controlling.downloadcontroller.DiskSpaceReservation;
import jd.controlling.downloadcontroller.DownloadSession;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.DownloadWatchDogJob;
import jd.controlling.downloadcontroller.ExceptionRunnable;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.controlling.downloadcontroller.SingleDownloadController.WaitingQueueItem;
import jd.controlling.linkchecker.LinkChecker;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcrawler.CheckableLink;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.LinkCrawler;
import jd.controlling.linkcrawler.LinkCrawlerThread;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.proxy.AbstractProxySelectorImpl;
import jd.controlling.reconnect.ipcheck.BalancedWebIPCheck;
import jd.controlling.reconnect.ipcheck.IPCheckException;
import jd.controlling.reconnect.ipcheck.OfflineException;
import jd.gui.swing.jdgui.BasicJDTable;
import jd.gui.swing.jdgui.views.settings.panels.pluginsettings.PluginConfigPanel;
import jd.http.Browser;
import jd.http.Browser.BrowserException;
import jd.http.NoGateWayException;
import jd.http.ProxySelectorInterface;
import jd.http.Request;
import jd.http.StaticProxySelector;
import jd.http.URLConnectionAdapter;
import jd.nutils.Formatter;
import jd.nutils.JDHash;
import jd.plugins.Account.AccountError;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import jd.plugins.download.DownloadInterface;
import jd.plugins.download.DownloadInterfaceFactory;
import jd.plugins.download.DownloadLinkDownloadable;
import jd.plugins.download.Downloadable;
import net.miginfocom.swing.MigLayout;

import org.appwork.exceptions.WTFException;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.config.JsonConfig;
import org.appwork.swing.MigPanel;
import org.appwork.swing.action.BasicAction;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtComponentRowHighlighter;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTable;
import org.appwork.swing.exttable.ExtTableHeaderRenderer;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtFileSizeColumn;
import org.appwork.swing.exttable.columns.ExtLongColumn;
import org.appwork.swing.exttable.columns.ExtProgressColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.timetracker.TimeTracker;
import org.appwork.timetracker.TrackerJob;
import org.appwork.uio.CloseReason;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.InputDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Files;
import org.appwork.utils.Hash;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.ProgressFeedback;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.net.httpconnection.HTTPConnection.RequestMethod;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.DispositionHeader;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.appwork.utils.swing.dialog.InputDialog;
import org.appwork.utils.swing.dialog.ProgressDialog;
import org.appwork.utils.swing.dialog.ProgressDialog.ProgressGetter;
import org.jdownloader.DomainInfo;
import org.jdownloader.DomainInfo.DomainInfoFactory;
import org.jdownloader.captcha.blacklist.BlacklistEntry;
import org.jdownloader.captcha.blacklist.BlockAllDownloadCaptchasEntry;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByHost;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByLink;
import org.jdownloader.captcha.blacklist.BlockDownloadCaptchasByPackage;
import org.jdownloader.captcha.blacklist.CaptchaBlackList;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickedPoint;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.BasicCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.ImageCaptchaChallenge;
import org.jdownloader.controlling.DefaultDownloadLinkViewImpl;
import org.jdownloader.controlling.DownloadLinkView;
import org.jdownloader.controlling.UrlProtection;
import org.jdownloader.controlling.ffmpeg.FFMpegInstallProgress;
import org.jdownloader.controlling.ffmpeg.FFmpeg;
import org.jdownloader.controlling.ffmpeg.FFmpegProvider;
import org.jdownloader.controlling.ffmpeg.FFmpegSetup;
import org.jdownloader.controlling.ffmpeg.FFprobe;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter.ExtensionsFilterInterface;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter.VideoExtensions;
import org.jdownloader.controlling.linkcrawler.GenericVariants;
import org.jdownloader.controlling.linkcrawler.LinkVariant;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.InputChangedCallbackInterface;
import org.jdownloader.gui.dialog.AskDownloadPasswordDialogInterface;
import org.jdownloader.gui.dialog.AskForDownloadLinkDialog;
import org.jdownloader.gui.dialog.AskToUsePremiumDialog;
import org.jdownloader.gui.dialog.AskToUsePremiumDialogInterface;
import org.jdownloader.gui.helpdialogs.HelpDialog;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo.PluginView;
import org.jdownloader.gui.views.linkgrabber.columns.VariantColumn;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.BadgeIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.CaptchaStepProgress;
import org.jdownloader.plugins.ConditionalSkipReasonException;
import org.jdownloader.plugins.PluginTaskID;
import org.jdownloader.plugins.SkipReason;
import org.jdownloader.plugins.SkipReasonException;
import org.jdownloader.plugins.SleepPluginProgress;
import org.jdownloader.plugins.UserIOProgress;
import org.jdownloader.plugins.WaitForAccountTrafficSkipReason;
import org.jdownloader.plugins.WaitForAccountTrafficSkipReasonMultihostLinksRequired;
import org.jdownloader.plugins.WaitForAccountTrafficSkipReasonMultihostTrafficRequired;
import org.jdownloader.plugins.WaitingSkipReasonMultihostHostUnavailable;
import org.jdownloader.plugins.accounts.AccountBuilderInterface;
import org.jdownloader.plugins.config.AccountConfigInterface;
import org.jdownloader.plugins.config.AccountJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;
import org.jdownloader.plugins.controller.PluginClassLoader.PluginClassLoaderChild;
import org.jdownloader.plugins.controller.crawler.CrawlerPluginController;
import org.jdownloader.plugins.controller.crawler.LazyCrawlerPlugin;
import org.jdownloader.plugins.controller.host.HostPluginController;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;
import org.jdownloader.plugins.controller.host.PluginFinder;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;
import org.jdownloader.translate._JDT;
import org.jdownloader.updatev2.UpdateController;
import org.jdownloader.updatev2.UpdateHandler;

/**
 * Dies ist die Oberklasse fuer alle Plugins, die von einem Anbieter Dateien herunterladen koennen
 *
 * @author astaldo
 */
public abstract class PluginForHost extends Plugin {
    private static final String    COPY_MOVE_FILE = "CopyMoveFile";
    private static final Pattern[] PATTERNS       = new Pattern[] {
        /**
         * these patterns should split filename and fileextension (extension must include the
         * point)
         */
        // multipart rar archives
        Pattern.compile("(.*)(\\.pa?r?t?\\.?[0-9]+.*?\\.rar$)", Pattern.CASE_INSENSITIVE),
        // normal files with extension
        Pattern.compile("(.*)(\\..*?$)", Pattern.CASE_INSENSITIVE) };
    private LazyHostPlugin         lazyP          = null;
    /**
     * Is true if the user has answered a captcha challenge. Does not say anything whether or not the answer was correct.
     */
    private boolean                dlSet          = false;

    public LazyHostPlugin getLazyP() {
        return lazyP;
    }

    public void runCaptchaDDosProtection(final String id) throws InterruptedException {
        final TimeTracker tracker = ChallengeResponseController.getInstance().getTracker(id);
        final Thread thread = Thread.currentThread();
        final TrackerJob trackerJob;
        if (thread instanceof SingleDownloadController) {
            trackerJob = new SingleDownloadControllerCaptchaTrackerJob(id, (SingleDownloadController) thread);
        } else {
            final DownloadLink downloadLink = getDownloadLink();
            if (downloadLink != null) {
                final SingleDownloadController controller = downloadLink.getDownloadLinkController();
                if (controller != null) {
                    trackerJob = new SingleDownloadControllerCaptchaTrackerJob(id, controller);
                } else {
                    trackerJob = new DownloadLinkCaptchaTracker(id, downloadLink);
                }
            } else {
                trackerJob = new TrackerJob(1);
            }
        }
        tracker.wait(trackerJob);
    }

    protected List<LazyCrawlerPlugin> findNextLazyCrawlerPlugins(final String url, final LazyCrawlerPlugin.FEATURE... features) {
        final List<LazyCrawlerPlugin> ret = new ArrayList<LazyCrawlerPlugin>();
        for (final LazyCrawlerPlugin lazyCrawlerPlugin : CrawlerPluginController.list(true)) {
            if ((features == null || features.length == 0 || lazyCrawlerPlugin.hasFeature(features)) && lazyCrawlerPlugin.canHandle(url)) {
                ret.add(lazyCrawlerPlugin);
            }
        }
        return ret;
    }

    public String getPluginContentURL(DownloadLink link) {
        final String ret = link.getStringProperty(DownloadLink.URL_CONTENT);
        return ret;
    }

    public boolean setPluginContentURL(String url, DownloadLink link) {
        if (!StringUtils.equals(url, getPluginContentURL(link))) {
            if (StringUtils.isEmpty(url)) {
                return link.removeProperty(DownloadLink.URL_CONTENT);
            } else {
                return link.setProperty(DownloadLink.URL_CONTENT, url);
            }
        } else {
            return false;
        }
    }

    public String getPluginCustomURL(DownloadLink link) {
        final String ret = link.getStringProperty(DownloadLink.URL_CUSTOM);
        return ret;
    }

    public boolean setPluginCustomURL(String url, DownloadLink link) {
        if (!StringUtils.equals(url, getPluginCustomURL(link))) {
            if (StringUtils.isEmpty(url)) {
                return link.removeProperty(DownloadLink.URL_CUSTOM);
            } else {
                return link.setProperty(DownloadLink.URL_CUSTOM, url);
            }
        } else {
            return false;
        }
    }

    public String getPluginContainerURL(DownloadLink link) {
        final String ret = link.getStringProperty(DownloadLink.URL_CONTAINER);
        return ret;
    }

    public boolean setPluginContainerURL(String url, DownloadLink link) {
        if (!StringUtils.equals(url, getPluginContainerURL(link))) {
            if (StringUtils.isEmpty(url)) {
                return link.removeProperty(DownloadLink.URL_CONTAINER);
            } else {
                return link.setProperty(DownloadLink.URL_CONTAINER, url);
            }
        } else {
            return false;
        }
    }

    /**
     * This method must return sort of unique identifier
     *
     * example: getHost()://fileID
     *
     * example: getHost()://fileID/quality
     *
     * example: getHost()://fileID/quality/format
     *
     * example: getHost()://fileID/quality/format/language
     *
     * example: full URL
     *
     * do NOT override this method to just return fileID only!
     *
     * @param link
     * @return
     */
    public String getLinkID(DownloadLink link) {
        final String linkID = link.getSetLinkID();
        if (StringUtils.isEmpty(linkID)) {
            return link.getPluginPatternMatcher();
        } else {
            final String orgLinkID = link.getStringProperty("ORG_LINKID");
            if (orgLinkID != null) {
                // convert old linkIDs
                return linkID.replaceFirst("_ORIGINAL$", "");
            } else {
                return linkID;
            }
        }
    }

    public void setLinkID(final DownloadLink link, final String linkID) {
        if (StringUtils.isEmpty(linkID)) {
            link.removeProperty(DownloadLink.PROPERTY_LINKDUPEID);
        } else {
            link.setProperty(DownloadLink.PROPERTY_LINKDUPEID, linkID);
        }
    }

    public AccountInfo handleAccountException(final Account account, final LogInterface logger, Throwable throwable) {
        final AccountInfo ai;
        if (account.getAccountInfo() != null) {
            ai = account.getAccountInfo();
        } else {
            ai = new AccountInfo();
            account.setAccountInfo(ai);
        }
        if (logger != null) {
            if (logger instanceof LogSource) {
                ((LogSource) logger).clear();
            }
            logger.info("PluginDetails(" + getLazyP().getClassName() + "|" + getVersion() + ")");
            logger.log(throwable);
        }
        if (throwable instanceof NoGateWayException) {
            account.setError(AccountError.TEMP_DISABLED, 5 * 60 * 1000l, _JDT.T.AccountController_updateAccountInfo_no_gateway());
            return ai;
        }
        if (throwable instanceof NullPointerException) {
            throwable = new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, null, throwable);
        }
        if (throwable instanceof PluginException) {
            final PluginException pluginException = (PluginException) throwable;
            switch (pluginException.getLinkStatus()) {
            case LinkStatus.ERROR_CAPTCHA: {
                invalidateLastChallengeResponse();
                final String errorMsg;
                if (StringUtils.isEmpty(pluginException.getLocalizedMessage())) {
                    errorMsg = _JDT.T.DownloadLink_setSkipped_statusmessage_captcha();
                } else {
                    errorMsg = pluginException.getLocalizedMessage();
                }
                account.setError(AccountError.TEMP_DISABLED, 60 * 60 * 1000l, errorMsg);
                return ai;
            }
            case LinkStatus.ERROR_PREMIUM: {
                validateLastChallengeResponse();
                if (PluginException.VALUE_ID_PREMIUM_TEMP_DISABLE == pluginException.getValue()) {
                    final String errorMsg;
                    if (StringUtils.isEmpty(pluginException.getLocalizedMessage())) {
                        if (!ai.isUnlimitedTraffic() && ai.getTrafficLeft() <= 0) {
                            errorMsg = _JDT.T.AccountController_updateAccountInfo_status_traffic_reached();
                        } else {
                            errorMsg = _JDT.T.AccountController_updateAccountInfo_status_uncheckable();
                        }
                    } else {
                        errorMsg = pluginException.getLocalizedMessage();
                    }
                    if (pluginException instanceof AccountUnavailableException) {
                        final AccountUnavailableException timeout = (AccountUnavailableException) pluginException;
                        account.setError(AccountError.TEMP_DISABLED, timeout.getTimeout(), errorMsg);
                    } else {
                        account.setError(AccountError.TEMP_DISABLED, 60 * 60 * 1000l, errorMsg);
                    }
                } else {
                    final String errorMsg;
                    if (StringUtils.isEmpty(pluginException.getLocalizedMessage())) {
                        errorMsg = _JDT.T.AccountController_updateAccountInfo_status_logins_wrong();
                    } else {
                        errorMsg = pluginException.getLocalizedMessage();
                    }
                    account.setError(AccountError.INVALID, -1, errorMsg);
                }
                return ai;
            }
            case LinkStatus.ERROR_PLUGIN_DEFECT: {
                final String errorMsg;
                if (StringUtils.isEmpty(pluginException.getLocalizedMessage())) {
                    errorMsg = _JDT.T.AccountController_updateAccountInfo_status_plugin_defect();
                } else {
                    errorMsg = pluginException.getLocalizedMessage();
                }
                account.setError(AccountError.TEMP_DISABLED, 60 * 60 * 1000l, errorMsg);
                return ai;
            }
            default: {
                final String errorMsg;
                if (StringUtils.isEmpty(pluginException.getLocalizedMessage())) {
                    errorMsg = _JDT.T.AccountController_updateAccountInfo_status_uncheckable();
                } else {
                    errorMsg = pluginException.getLocalizedMessage();
                }
                account.setError(AccountError.TEMP_DISABLED, 60 * 60 * 1000l, errorMsg);
                return ai;
            }
            }
        }
        ProxySelectorInterface proxySelector = null;
        final BrowserException browserException = Exceptions.getInstanceof(throwable, BrowserException.class);
        if (browserException != null && browserException.getRequest() != null) {
            final HTTPProxy proxy = browserException.getRequest().getProxy();
            if (proxy != null) {
                proxySelector = new StaticProxySelector(proxy);
            }
        }
        if (proxySelector == null && getBrowser() != null && getBrowser().getRequest() != null) {
            final HTTPProxy proxy = getBrowser().getRequest().getProxy();
            if (proxy != null) {
                proxySelector = new StaticProxySelector(proxy);
            }
        }
        final BalancedWebIPCheck onlineCheck = new BalancedWebIPCheck(proxySelector);
        try {
            onlineCheck.getExternalIP();
        } catch (final OfflineException e2) {
            account.setError(AccountError.TEMP_DISABLED, 5 * 60 * 1000l, "No Internet Connection");
            return ai;
        } catch (final IPCheckException e2) {
        }
        if (browserException != null && Exceptions.getInstanceof(browserException, UnknownHostException.class) != null) {
            account.setError(AccountError.TEMP_DISABLED, 5 * 60 * 1000l, "DNS issues");
            return ai;
        }
        final String errorMsg;
        if (StringUtils.isEmpty(throwable.getMessage())) {
            errorMsg = _JDT.T.AccountController_updateAccountInfo_status_uncheckable();
        } else {
            if (browserException != null && browserException.getRequest() != null) {
                errorMsg = browserException.getSuperMessage();
            } else {
                errorMsg = throwable.getMessage();
            }
        }
        account.setError(AccountError.TEMP_DISABLED, 60 * 60 * 1000l, errorMsg);
        return ai;
    }

    public void setLazyP(LazyHostPlugin lazyP) {
        this.lazyP = lazyP;
    }

    @Deprecated
    public void errLog(Throwable e, Browser br, DownloadLink link) {
        errLog(e, br, null, link, null);
    }

    public long calculateAdditionalRequiredDiskSpace(DownloadLink link) {
        return 0;
    }

    protected void checkAndReserve(final DownloadLink downloadLink, final DiskSpaceReservation reservation) throws Exception {
        final DISKSPACERESERVATIONRESULT result = DownloadWatchDog.getInstance().getSession().getDiskSpaceManager().checkAndReserve(reservation, downloadLink != null ? downloadLink.getDownloadLinkController() : null);
        switch (result) {
        case FAILED:
            throw new SkipReasonException(SkipReason.DISK_FULL);
        case INVALIDDESTINATION:
            throw new SkipReasonException(SkipReason.INVALID_DESTINATION);
        default:
            break;
        }
    }

    protected void free(final DownloadLink downloadLink, final DiskSpaceReservation reservation) {
        DownloadWatchDog.getInstance().getSession().getDiskSpaceManager().free(reservation, downloadLink != null ? downloadLink.getDownloadLinkController() : null);
    }

    public void errLog(Throwable e, Browser br, LogSource log, DownloadLink link, Account account) {
        if (e != null && e instanceof PluginException && ((PluginException) e).getLinkStatus() == LinkStatus.ERROR_PLUGIN_DEFECT) {
            final LogSource errlogger = LogController.getInstance().getLogger("PluginErrors");
            try {
                errlogger.severe("HosterPlugin out of date: " + this + " :" + getVersion());
                errlogger.severe("URL: " + link.getPluginPatternMatcher() + " | ContentUrl: " + link.getContentUrl() + " | ContainerUrl: " + link.getContainerUrl() + " | OriginUrl: " + link.getOriginUrl() + " | ReferrerUrl: " + link.getReferrerUrl());
                if (e != null) {
                    errlogger.log(e);
                }
            } finally {
                errlogger.close();
            }
        }
    }

    @Deprecated
    public PluginForHost(final PluginWrapper wrapper) {
        super(wrapper);
        final ClassLoader cl = getClass().getClassLoader();
        if (!(cl instanceof PluginClassLoaderChild)) {
            //
            throw new WTFException(this + " got loaded by non PluginClassLoaderChild!");
        }
        /* defaultPlugin does not need any Browser instance */
        br = null;
        dl = null;
        /* defaultPlugins do not have any working logger */
        /* workaround for all the lazy init issues */
        this.lazyP = (LazyHostPlugin) wrapper.getLazy();
    }

    public DownloadInterface getDownloadInterface() {
        return dl;
    }

    public String getCaptchaCode(final String captchaAddress, final DownloadLink downloadLink) throws Exception {
        return getCaptchaCode(getHost(), captchaAddress, downloadLink);
    }

    @Override
    public long getVersion() {
        return lazyP.getVersion();
    }

    @Override
    public Pattern getSupportedLinks() {
        return lazyP.getPattern();
    }

    protected String getCaptchaCode(final String method, final String captchaAddress, final DownloadLink downloadLink) throws Exception {
        if (StringUtils.isEmpty(captchaAddress)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "No captcha url given");
        }
        return getCaptchaCode(method, getCaptchaImage(captchaAddress), downloadLink);
    }

    protected MultiClickedPoint getMultiCaptchaClickedPoint(final File file, final DownloadLink link, final String explain) throws Exception {
        final File copy = copyCaptcha(this.getHost(), file);
        final MultiClickCaptchaChallenge c = new MultiClickCaptchaChallenge(copy, explain, this);
        return handleCaptchaChallenge(link, c);
    }

    protected String getCaptchaCode(final File captchaFile, final DownloadLink downloadLink) throws Exception {
        return getCaptchaCode(getHost(), captchaFile, downloadLink);
    }

    public String getCaptchaCode(final String methodname, final File captchaFile, final DownloadLink downloadLink) throws Exception {
        return getCaptchaCode(methodname, captchaFile, 0, downloadLink, null, null);
    }

    protected String getCaptchaCode(final String method, File file, final int flag, final DownloadLink link, final String defaultValue, final String explain) throws Exception {
        final String orgCaptchaImage = link.getStringProperty("orgCaptchaFile", null);
        if (orgCaptchaImage != null && new File(orgCaptchaImage).exists()) {
            file = new File(orgCaptchaImage);
        }
        final File copy = copyCaptcha(method, file);
        if (this.getDownloadLink() == null) {
            this.setDownloadLink(link);
        }
        final BasicCaptchaChallenge c = createChallenge(method, copy, flag, link, defaultValue, explain);
        return handleCaptchaChallenge(link, c);
    }

    private File copyCaptcha(String method, File file) throws Exception {
        if (file == null) {
            return null;
        }
        final File copy = Application.getResource("captchas/" + method + "/" + Hash.getMD5(file) + "." + Files.getExtension(file.getName()));
        copy.delete();
        copy.getParentFile().mkdirs();
        IO.copyFile(file, copy);
        return copy;
    }

    @Deprecated
    protected boolean isAccountLoginCaptchaChallenge(final DownloadLink link, final Challenge<?> c) {
        return isAccountLoginCaptchaChallenge(c) || link == null || FilePackage.isDefaultFilePackage(link.getFilePackage());
    }

    protected <ReturnType> ReturnType handleSkipException(final DownloadLink link, Challenge<ReturnType> c, SkipException e) throws PluginException, CaptchaException, InterruptedException {
        LogSource.exception(logger, e);
        if (link != null && !c.isAccountLogin()) {
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
                onCaptchaTimeout(link, c);
                // TIMEOUT may fallthrough to SINGLE
            case SINGLE:
                CaptchaBlackList.getInstance().add(new BlockDownloadCaptchasByLink(e.getSkipRequest(), link));
                HelpDialog.showCaptchaSkippedDialog();
                break;
            default:
                break;
            }
        }
        switch (e.getSkipRequest()) {
        case STOP_CURRENT_ACTION:
            if (Thread.currentThread() instanceof SingleDownloadController) {
                DownloadWatchDog.getInstance().stopDownloads();
            }
            break;
        case REFRESH:
            // we should forward the refresh request to a new pluginstructure soon. For now. the plugin will just retry
            return c.getRefreshTrigger();
        default:
            break;
        }
        throw new CaptchaException(e.getSkipRequest());
    }

    public <T> T handleCaptchaChallenge(final DownloadLink link, Challenge<T> c) throws CaptchaException, PluginException, InterruptedException {
        if (c instanceof ImageCaptchaChallenge) {
            final File captchaFile = ((ImageCaptchaChallenge) c).getImageFile();
            cleanUpCaptchaFiles.addIfAbsent(captchaFile);
        }
        c.setTimeout(getChallengeTimeout(c));
        invalidateLastChallengeResponse();
        PluginProgress progress = null;
        try {
            if (isAccountLoginCaptchaChallenge(link, c)) {
                /**
                 * account login -> do not use anticaptcha services
                 */
                c.setAccountLogin(true);
            } else if (link != null) {
                progress = new CaptchaStepProgress(0, 1, null);
                progress.setProgressSource(this);
                progress.setDisplayInProgressColumnEnabled(false);
                link.addPluginProgress(progress);
                final SingleDownloadController controller = link.getDownloadLinkController();
                if (controller != null) {
                    setHasCaptcha(link, controller.getAccount(), true);
                }
            }
            final BlacklistEntry<?> blackListEntry = CaptchaBlackList.getInstance().matches(c);
            if (blackListEntry != null) {
                logger.warning("Cancel. Blacklist Matching");
                throw new CaptchaException(blackListEntry);
            }
            ChallengeResponseController.getInstance().handle(c);
            if (!c.isSolved()) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            } else if (!c.isCaptchaResponseValid()) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA, "Captcha reponse value did not validate!");
            } else {
                return c.getResult().getValue();
            }
        } catch (InterruptedException e) {
            LogSource.exception(logger, e);
            throw e;
        } catch (SkipException e) {
            final Thread thread = Thread.currentThread();
            if (thread instanceof SingleDownloadController) {
                return handleSkipException(getDownloadLink(), c, e);
            }
            if (thread instanceof LinkCrawlerThread) {
                Plugin plugin = ((LinkCrawlerThread) thread).getCurrentPlugin();
                while (plugin != null) {
                    if (plugin instanceof PluginForDecrypt) {
                        final PluginForDecrypt decryptPlugin = (PluginForDecrypt) plugin;
                        return decryptPlugin.handleSkipException(c, e);
                    } else {
                        plugin = plugin.getParentPlugin();
                    }
                }
            }
            throw new CaptchaException(e.getSkipRequest());
        } finally {
            if (progress != null) {
                link.removePluginProgress(progress);
            }
        }
    }

    /** This gets executed whenever the user does not answer a captcha which then runs into timeout. */
    public void onCaptchaTimeout(final DownloadLink link, Challenge<?> challenge) throws CaptchaException, PluginException, InterruptedException {
        switch (JsonConfig.create(CaptchaSettings.class).getOnHosterCaptchaTimeoutAction()) {
        case RETRY:
            throw new PluginException(LinkStatus.ERROR_RETRY);
        case ASK:
            if (UIOManager.I().showConfirmDialog(0, _GUI.T.gui_captchaWindow_askForInput(link.getDomainInfo().getTld()), _GUI.T.StatusBarImpl_skippedLinksMarker_desc(1), new AbstractIcon(IconKey.ICON_QUESTION, 32), _GUI.T.CaptchaDialog_layoutDialogContent_refresh(), _GUI.T.AbstractCaptchaDialog_AbstractCaptchaDialog_cancel())) {
                throw new PluginException(LinkStatus.ERROR_RETRY);
            }
            break;
        case SKIP_HOSTER:
            throw new CaptchaException(SkipRequest.BLOCK_HOSTER);
        case SKIP:
            // fallthrough
        default:
            HelpDialog.showCaptchaSkippedDialog();
            break;
        }
    }

    protected String getDefaultFileName(DownloadLink link) {
        return null;
    }

    protected DownloadLinkView getDownloadLinkView(DownloadLink link) {
        return new DefaultDownloadLinkViewImpl();
    }

    protected DomainInfo getDomainInfo(DownloadLink link) {
        if (this instanceof DomainInfoFactory) {
            return DomainInfo.getInstance(getHost(link, null, true), (DomainInfoFactory) this);
        } else {
            return DomainInfo.getInstance(getHost(link, null, true));
        }
    }

    protected BasicCaptchaChallenge createChallenge(final String method, File file, final int flag, final DownloadLink link, final String defaultValue, final String explain) {
        return new BasicCaptchaChallenge(method, file, defaultValue, explain, this, flag);
    }

    protected volatile DownloadInterface dl                                           = null;
    private static final String          AUTO_FILE_NAME_CORRECTION_NAME_SPLIT         = "AUTO_FILE_NAME_CORRECTION_NAME_SPLIT";
    private static final String          AUTO_FILE_NAME_CORRECTION_NAME_SPLIT_PATTERN = "AUTO_FILE_NAME_CORRECTION_NAME_SPLIT_PATTERN";
    private long                         WAIT_BETWEEN_STARTS                          = 0;
    private boolean                      enablePremium                                = false;
    private String                       premiumurl                                   = null;
    private DownloadLink                 link                                         = null;
    protected DownloadInterfaceFactory   customizedDownloadFactory                    = null;

    public DownloadInterfaceFactory getCustomizedDownloadFactory() {
        return customizedDownloadFactory;
    }

    public void setCustomizedDownloadFactory(DownloadInterfaceFactory customizedDownloadFactory) {
        this.customizedDownloadFactory = customizedDownloadFactory;
    }

    @Override
    public String getHost() {
        return lazyP.getDisplayName();
    }

    @Override
    public LogInterface getLogger() {
        return super.getLogger();
    }

    @Override
    @Deprecated
    public SubConfiguration getPluginConfig() {
        return SubConfiguration.getConfig(lazyP.getHost());
    }

    protected PluginConfigPanelNG createConfigPanel() {
        boolean hasConfigPanel = getConfigInterface() != null;
        hasConfigPanel |= isPremiumEnabled() && getAccountConfigInterface(null) != null;
        hasConfigPanel |= hasFeature(FEATURE.MULTIHOST);
        if (!hasConfigPanel) {
            return null;
        }
        final PluginConfigPanelNG ret = new PluginConfigPanelNG() {
            private PluginConfigPanel oldStyle;

            @Override
            public void updateContents() {
            }

            @Override
            protected void initPluginSettings(Plugin plugin) {
                super.initPluginSettings(plugin);
                if (hasOldConfigContainer()) {
                    final PluginConfigPanel oldStyle = PluginConfigPanel.create(getLazyP());
                    if (oldStyle != null) {
                        add(oldStyle, "pushx,growx,spanx");
                    }
                    this.oldStyle = oldStyle;
                }
            }

            @Override
            protected void onHide() {
                super.onHide();
                final PluginConfigPanel oldStyle = this.oldStyle;
                if (oldStyle != null) {
                    oldStyle.setHidden();
                }
            }

            @Override
            protected void onShow() {
                super.onShow();
                final PluginConfigPanel oldStyle = this.oldStyle;
                if (oldStyle != null) {
                    oldStyle.setShown();
                }
            }

            @Override
            public void save() {
            }
        };
        return ret;
    }

    @Override
    public void clean() {
        try {
            try {
                final DownloadInterface dl = getDownloadInterface();
                if (dl != null) {
                    try {
                        final URLConnectionAdapter con = dl.getConnection();
                        if (con != null) {
                            con.disconnect();
                        }
                    } finally {
                        dl.close();
                    }
                }
            } catch (Throwable ignore) {
            } finally {
                setDownloadInterface(null);
            }
            try {
                final Browser br = getBrowser();
                if (br != null) {
                    br.disconnect();
                }
            } catch (Throwable ignore) {
            } finally {
                br = null;
            }
        } finally {
            super.clean();
        }
    }

    public boolean gotDownloadInterface() {
        return dlSet;
    }

    public synchronized void setDownloadInterface(DownloadInterface dl) {
        final DownloadInterface oldDl = this.dl;
        this.dl = dl;
        if (dlSet == false && dl != null) {
            dlSet = true;
        }
        if (oldDl != null && oldDl != dl) {
            try {
                oldDl.close();
            } catch (final Throwable e) {
                getLogger().log(e);
            }
        }
    }

    protected void setBrowserExclusive() {
        if (br != null) {
            if (br.setCookiesExclusive(true)) {
                br.clearCookies(getHost());
            }
        }
    }

    /** default fetchAccountInfo, set account valid to true */
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        account.setValid(true);
        return ai;
    }

    public abstract String getAGBLink();

    protected void enablePremium() {
        enablePremium(null);
    }

    protected void enablePremium(final String url) {
        premiumurl = url;
        enablePremium = true;
    }

    /**
     * Hier werden Treffer fuer Downloadlinks dieses Anbieters in diesem Text gesucht. Gefundene Links werden dann in einem ArrayList
     * zurueckgeliefert
     *
     * @param data
     *            Ein Text mit beliebig vielen Downloadlinks dieses Anbieters
     * @return Ein ArrayList mit den gefundenen Downloadlinks
     */
    public ArrayList<DownloadLink> getDownloadLinks(CrawledLink source, final String data, final FilePackage fp) {
        final String[] hits = new Regex(data, getSupportedLinks()).getColumn(-1);
        if (hits == null || hits.length == 0) {
            return null;
        }
        final ArrayList<DownloadLink> links = new ArrayList<DownloadLink>(hits.length);
        try {
            PluginForHost plugin = null;
            for (String url : hits) {
                /* remove newlines... */
                url = url.trim();
                /*
                 * this removes the " from HTMLParser.ArrayToString
                 */
                /* only 1 " at start */
                while (url.charAt(0) == '"') {
                    url = url.substring(1);
                }
                /* can have several " at the end */
                while (url.charAt(url.length() - 1) == '"') {
                    url = url.substring(0, url.length() - 1);
                }
                /*
                 * use this REGEX to cut of following http links, (?=https?:|$|\r|\n|)
                 */
                /* we use null as ClassLoader to make sure all share the same ProtoTypeClassLoader */
                if (isValidURL(url)) {
                    if (plugin == null) {
                        plugin = getLazyP().getPrototype(null);
                    }
                    final DownloadLink link = new DownloadLink(plugin, null, getHost(), url, true);
                    links.add(link);
                }
            }
        } catch (Throwable e) {
            LogSource.exception(logger, e);
        }
        if (fp != null && fp != FilePackage.getDefaultFilePackage()) {
            fp.addLinks(links);
        }
        return links;
    }

    public boolean isValidURL(String URL) {
        return true;
    }

    @Override
    public Matcher getMatcher() {
        return lazyP.getMatcher();
    }

    /**
     * OVERRIDE this function if you need to modify the link, ATTENTION: you have to use new browser instances, this plugin might not have
     * one yet!
     */
    public void correctDownloadLink(final DownloadLink link) throws Exception {
    }

    public void onPluginAssigned(final DownloadLink link) throws Exception {
    }

    /**
     * Holt Informationen zu einem Link. z.B. dateigroeße, Dateiname, verfuegbarkeit etc.
     *
     * @param parameter
     * @return true/false je nach dem ob die Datei noch online ist (verfuegbar)
     * @throws IOException
     */
    public abstract AvailableStatus requestFileInformation(DownloadLink parameter) throws Exception;

    public int getMaxSimultanFreeDownloadNum() {
        return 1;
    }

    public int getMaxSimultanPremiumDownloadNum() {
        return -1;
    }

    public boolean isResumeable(DownloadLink link, final Account account) {
        if (link != null) {
            return link.getBooleanProperty(DownloadLink.PROPERTY_RESUMEABLE, false);
        } else {
            return false;
        }
    }

    public int getMaxSimultanDownload(DownloadLink link, final Account account, AbstractProxySelectorImpl proxy) {
        return getMaxSimultanDownload(link, account);
    }

    /**
     * this method returns absolute numbers of max allowed downloads for given plugin/link/account combination
     *
     * @param link
     * @param account
     * @return
     */
    protected int getMaxSimultanDownload(final DownloadLink link, final Account account) {
        int max;
        if (account == null) {
            max = getMaxSimultanFreeDownloadNum();
            if (max >= 0) {
                /* >=0 = 0 or more downloads */
                return max;
            }
            if (max == -1) {
                /*-1 = unlimited*/
                return Integer.MAX_VALUE;
            }
            /* no downloads */
            return 0;
        } else {
            max = account.getMaxSimultanDownloads();
            if (max >= 1) {
                /* 1 or more downloads */
                return max;
            }
            if (max == -1) {
                /*-1 = unlimited*/
                return Integer.MAX_VALUE;
            }
            if (max == 0) {
                /* 0 = use deprecated getMaxSimultanPremiumDownloadNum */
                max = getMaxSimultanPremiumDownloadNum();
                if (max >= 0) {
                    /* >=0 = 0 or more downloads */
                    return max;
                }
                if (max == -1) {
                    /*-1 = unlimited*/
                    return Integer.MAX_VALUE;
                }
                /* no downloads */
                return 0;
            }
            /* no downloads */
            return 0;
        }
    }

    /**
     * returns the host of the service handling the download for link with given account
     *
     * @param link
     * @param account
     * @param includeSubdomain
     * @return
     */
    public String getHost(DownloadLink link, Account account, boolean includeSubdomain) {
        return getHost();
    }

    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    public abstract void handleFree(DownloadLink link) throws Exception;

    /**
     * return if we can download given downloadLink via given account with this pluginForHost
     *
     * @param downloadLink
     * @param account
     * @return
     * @throws Exception
     */
    public boolean canHandle(DownloadLink downloadLink, Account account) throws Exception {
        if (account != null && account.isMultiHost() && !account.getHoster().equals(downloadLink.getHost())) {
            final AccountInfo ai = account.getAccountInfo();
            if (ai == null) {
                return false;
            }
            /* Check for domain specific limits of multihost items. */
            /* Verify again if host is still supported because plugins can modify list on runtime */
            final MultiHostHost mhost = ai.getMultihostSupportedHost(downloadLink.getHost());
            if (mhost == null) {
                /* Host is not supported (anymore) */
                return false;
            } else if (!mhost.isEnabled()) {
                /* Disabled by user */
                return false;
            }
            final MultihosterHostStatus status = mhost.getStatus();
            if (status != MultihosterHostStatus.WORKING && status != MultihosterHostStatus.WORKING_UNSTABLE) {
                /* Download of that host is permanently not possible. */
                return false;
            } else if (mhost.getUnavailableTimeMillis() > 0) {
                throw new ConditionalSkipReasonException(new WaitingSkipReasonMultihostHostUnavailable(account, downloadLink.getHost(), mhost.getUnavailableStatusText(), mhost.getUnavailableUntilTimestamp()));
            }
        }
        return true;
    }

    /**
     * Returns if the given DownloadLink can be downloaded via given PluginForHost
     *
     * @param downloadLink
     * @param plugin
     * @return
     */
    public boolean allowHandle(DownloadLink downloadLink, PluginForHost plugin) {
        /**
         * example: only allow original host plugin
         *
         * return downloadLink.getHost().equalsIgnoreCase(plugin.getHost());
         */
        return true;
    }

    public AccountTrafficView getAccountTrafficView(final Account account) {
        if (account == null) {
            return null;
        }
        return account.getAccountInfo();
    }

    public boolean enoughTrafficFor(final DownloadLink link, final Account account) throws Exception {
        if (account == null) {
            return true;
        }
        final AccountInfo ai = account.getAccountInfo();
        if (ai == null) {
            return true;
        }
        final long trafficLeft = ai.getTrafficLeft();
        final long minimum = 1024;
        final long downloadSize = link.getView().getBytesTotalEstimated();
        long trafficNeeded;
        if (downloadSize > 0) {
            trafficNeeded = Math.max(minimum, downloadSize - link.getView().getBytesLoaded());
        } else {
            trafficNeeded = minimum;
        }
        if (account.isMultiHost() && !account.getHoster().equals(link.getHost())) {
            /* Check for domain specific limits of multihost items. */
            /* Verify again if host is still supported because plugins can modify list on runtime */
            final MultiHostHost mhost = ai.getMultihostSupportedHost(link.getHost());
            if (mhost == null) {
                /* Host is not supported (anymore) */
                return false;
            }
            if (!mhost.isUnlimitedLinks() && mhost.getLinksLeft() <= 0) {
                /* Max limits link is reached -> Cannot download */
                if (ai.isTrafficRefill()) {
                    throw new ConditionalSkipReasonException(new WaitForAccountTrafficSkipReasonMultihostLinksRequired(account, link.getHost(), mhost.getLinksMax()));
                } else {
                    return false;
                }
            }
            if (!mhost.isUnlimitedTraffic()) {
                /* Traffic limit exists -> Check if enough traffic is left. */
                final long host_TrafficLeft = Math.max(0, mhost.getTrafficLeft());
                if (trafficNeeded > host_TrafficLeft) {
                    /* Not enough individual file host traffic */
                    if (ai.isTrafficRefill()) {
                        final long howMuchTrafficIsMissing = trafficNeeded - host_TrafficLeft;
                        throw new ConditionalSkipReasonException(new WaitForAccountTrafficSkipReasonMultihostTrafficRequired(account, link.getHost(), howMuchTrafficIsMissing));
                    } else {
                        return false;
                    }
                }
            }
            /**
             * In some cases, individual hosts can have different traffic calculation values than 100%. <br>
             * This calculation applies for the global account-traffic and not for the individual host. </br> Example: File size is 1GB,
             * individual host traffic calculation factor is 400% <br>
             * Account traffic needed: 4GB <br>
             * Individual host traffic needed: 1GB
             */
            trafficNeeded = (trafficNeeded * mhost.getTrafficCalculationFactorPercent()) / 100;
        }
        if (!ai.isUnlimitedTraffic() && !ai.isSpecialTraffic()) {
            /* Check if enough traffic is left */
            if (trafficNeeded > trafficLeft) {
                if (ai.isTrafficRefill()) {
                    final long howMuchTrafficIsMissing = trafficNeeded - trafficLeft;
                    throw new ConditionalSkipReasonException(new WaitForAccountTrafficSkipReason(account, howMuchTrafficIsMissing));
                } else {
                    return false;
                }
            }
        }
        return true;
    }

    public FFmpeg getFFmpeg(final Browser br, final DownloadLink downloadLink) {
        return new FFmpeg(br) {
            @Override
            public LogInterface getLogger() {
                return PluginForHost.this.getLogger();
            }
        };
    }

    public FFprobe getFFProbe(final Browser br, final DownloadLink downloadLink) {
        return new FFprobe(br) {
            @Override
            public LogInterface getLogger() {
                return PluginForHost.this.getLogger();
            }
        };
    }

    public void checkFFmpeg(final DownloadLink downloadLink, final String reason) throws SkipReasonException, InterruptedException {
        final FFmpeg ffmpeg = getFFmpeg(null, downloadLink);
        if (!ffmpeg.isAvailable()) {
            final UpdateHandler handler = UpdateController.getInstance().getHandler();
            if (handler == null) {
                getLogger().warning("Please set FFmpeg: BinaryPath in advanced options");
                throw new SkipReasonException(SkipReason.FFMPEG_MISSING);
            }
            final FFMpegInstallProgress progress = new FFMpegInstallProgress();
            progress.setProgressSource(this);
            try {
                downloadLink.addPluginProgress(progress);
                FFmpegProvider.getInstance().install(progress, reason);
            } finally {
                downloadLink.removePluginProgress(progress);
            }
            ffmpeg.setPath(JsonConfig.create(FFmpegSetup.class).getBinaryPath());
            if (!ffmpeg.isAvailable()) {
                final List<String> requestedInstalls = handler.getRequestedInstalls();
                final String extensionID = org.jdownloader.controlling.ffmpeg.FFMpegInstallThread.getFFmpegExtensionName();
                if (requestedInstalls != null && extensionID != null && requestedInstalls.contains(extensionID)) {
                    throw new SkipReasonException(SkipReason.UPDATE_RESTART_REQUIRED);
                } else {
                    throw new SkipReasonException(SkipReason.FFMPEG_MISSING);
                }
            }
        } else if (!ffmpeg.isCompatible()) {
            getLogger().warning("Incompatible/non functional FFMPEG found!");
            throw new SkipReasonException(SkipReason.FFMPEG_MISSING);
        }
    }

    public void checkFFProbe(final DownloadLink downloadLink, final String reason) throws SkipReasonException, InterruptedException {
        final FFprobe ffprobe = getFFProbe(null, downloadLink);
        if (!ffprobe.isAvailable()) {
            final UpdateHandler handler = UpdateController.getInstance().getHandler();
            if (handler == null) {
                getLogger().warning("Please set FFProbe: BinaryPath in advanced options");
                throw new SkipReasonException(SkipReason.FFPROBE_MISSING);
            }
            final FFMpegInstallProgress progress = new FFMpegInstallProgress();
            progress.setProgressSource(this);
            try {
                downloadLink.addPluginProgress(progress);
                FFmpegProvider.getInstance().install(progress, reason);
            } finally {
                downloadLink.removePluginProgress(progress);
            }
            ffprobe.setPath(JsonConfig.create(FFmpegSetup.class).getBinaryPathProbe());
            if (!ffprobe.isAvailable()) {
                final List<String> requestedInstalls = handler.getRequestedInstalls();
                final String extensionID = org.jdownloader.controlling.ffmpeg.FFMpegInstallThread.getFFmpegExtensionName();
                if (requestedInstalls != null && extensionID != null && requestedInstalls.contains(extensionID)) {
                    throw new SkipReasonException(SkipReason.UPDATE_RESTART_REQUIRED);
                } else {
                    throw new SkipReasonException(SkipReason.FFPROBE_MISSING);
                }
            }
        } else if (!ffprobe.isCompatible()) {
            getLogger().warning("Incompatible/non functional FFProbe found!");
            throw new SkipReasonException(SkipReason.FFPROBE_MISSING);
        }
    }

    public void handle(final DownloadLink downloadLink, final Account account) throws Exception {
        final Account previousAccount = setCurrentAccount(account);
        ACCOUNTTYPE accountType = null;
        try {
            waitForNextStartAllowed(downloadLink, account);
            if (account != null) {
                /* with account */
                if (StringUtils.equalsIgnoreCase(account.getHoster(), downloadLink.getHost())) {
                    accountType = ACCOUNTTYPE.ORIGINAL;
                    handlePremium(downloadLink, account);
                } else {
                    accountType = ACCOUNTTYPE.MULTI;
                    handleMultiHost(downloadLink, account);
                }
            } else {
                /* without account */
                accountType = ACCOUNTTYPE.NONE;
                handleFree(downloadLink);
            }
            postHandle(downloadLink, account, this);
        } catch (final Exception e) {
            handleException(downloadLink, accountType, account, e);
            throw e;
        } finally {
            try {
                if (dl != null) {
                    downloadLink.getDownloadLinkController().getConnectionHandler().removeConnectionHandler(dl.getManagedConnetionHandler());
                }
            } catch (final Throwable e) {
                e.printStackTrace();
            }
            finalHandle(downloadLink, account, this);
            setCurrentAccount(previousAccount);
        }
    }

    protected void handleException(final DownloadLink downloadLink, final ACCOUNTTYPE accountType, final Account account, final Exception e) throws Exception {
        if (ACCOUNTTYPE.MULTI.equals(accountType) && e instanceof PluginException) {
            if (((PluginException) e).getLinkStatus() == LinkStatus.ERROR_FILE_NOT_FOUND && AvailableStatus.TRUE.equals(downloadLink.getAvailableStatus())) {
                /* File is online according to original filehoster -> Do not trust offline status from multihoster. */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Multihoster " + getHost() + " claims that this file is offline", e);
            }
        }
    }

    @Override
    public Browser createNewBrowserInstance() {
        return new PluginBrowser<PluginForHost>(this);
    }

    protected void finalHandle(DownloadLink downloadLink, Account account, PluginForHost pluginForHost) {
    }

    public long getTrafficRequired(final DownloadLink downloadLink, final Account account, long bytes) {
        if (account == null) {
            return bytes;
        }
        final AccountInfo ai = account.getAccountInfo();
        if (ai == null) {
            return bytes;
        }
        final MultiHostHost mhost = ai.getMultihostSupportedHost(downloadLink.getHost());
        if (mhost == null) {
            return bytes;
        }
        return (bytes * mhost.getTrafficCalculationFactorPercent()) / 100;
    }

    public void update(final DownloadLink downloadLink, final Account account, final long bytesTransfered) {
        if (account == null) {
            return;
        } else if (bytesTransfered == 0) {
            return;
        }
        // update the AccountInfo and NOT the AccountTrafficView
        final AccountInfo ai = account.getAccountInfo();
        if (ai == null) {
            return;
        } else if (ai.isUnlimitedTraffic()) {
            /* Unlimited traffic -> Do not deduct traffic. */
            return;
        }
        final long trafficToDeduct = getTrafficRequired(downloadLink, account, bytesTransfered);
        if (trafficToDeduct == 0) {
            return;
        }
        final long trafficLeft = Math.max(0, ai.getTrafficLeft() - trafficToDeduct);
        ai.setTrafficLeft(trafficLeft);
    }

    public void postHandle(final DownloadLink downloadLink, final Account account, final PluginForHost pluginForHost) throws Exception {
        if (pluginForHost == null) {
            return;
        } else if (downloadLink == null) {
            return;
        }
        if (StringUtils.equalsIgnoreCase(downloadLink.getHost(), pluginForHost.getHost())) {
            if (downloadLink.hasGenericVariantSupport() && downloadLink.hasVariantSupport()) {
                final GenericVariants var = downloadLink.getVariant(GenericVariants.class);
                if (var != null) {
                    var.runPostDownload(this, downloadLink, account);
                }
            }
        }
    }

    public void preHandle(final DownloadLink downloadLink, final Account account, final PluginForHost pluginForHost) throws Exception {
        if (pluginForHost == null) {
            return;
        } else if (downloadLink == null) {
            return;
        }
        if (StringUtils.equalsIgnoreCase(downloadLink.getHost(), pluginForHost.getHost())) {
            if (downloadLink.hasGenericVariantSupport() && downloadLink.hasVariantSupport()) {
                final GenericVariants var = downloadLink.getVariant(GenericVariants.class);
                if (var != null) {
                    var.runPreDownload(this, downloadLink, account);
                }
            }
        }
    }

    public void handleMultiHost(DownloadLink downloadLink, Account account) throws Exception {
        /*
         * fetchAccountInfo must fill ai.setMultiHostSupport to signal all supported multiHosts
         *
         * please synchronized on accountinfo and the ArrayList<String> when you change something in the handleMultiHost function
         *
         * in fetchAccountInfo we don't have to synchronize because we create a new instance of AccountInfo and fill it
         *
         * if you need customizable maxDownloads, please use getMaxSimultanDownload to handle this you are in multihost when account host
         * does not equal link host!
         *
         *
         *
         * will update this doc about error handling
         */
        logger.severe("invalid call to handleMultiHost: " + downloadLink.getName() + ":" + downloadLink.getHost() + " to " + getHost() + ":" + this.getVersion() + " with " + account);
        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
    }

    /**
     * Stellt das Plugin in den Ausgangszustand zurueck (variablen intialisieren etc)
     */
    public void reset() {
    }

    public final void resetLink(DownloadLink downloadLink) {
        resetDownloadlink(downloadLink);
    }

    public void resetDownloadlink(DownloadLink link) {
    }

    public List<File> listProcessFiles(DownloadLink link) {
        final HashSet<File> ret = new HashSet<File>();
        ret.add(new File(link.getFileOutputForPlugin(false, false) + ".part"));
        ret.add(new File(link.getFileOutputForPlugin(false, false)));
        ret.add(new File(link.getFileOutputForPlugin(false, true)));
        return new ArrayList<File>(ret);
    }

    public int getTimegapBetweenConnections() {
        return 50;
    }

    public String updateAccountPassword(final Account account, final String password) {
        if (password == null) {
            return null;
        }
        String ret = password.replaceAll("(?i)<" + Pattern.quote(getHost()) + ":EMPTY>", "");
        if (hasFeature(FEATURE.API_KEY_LOGIN)) {
            ret = ret.trim();
        }
        return ret;
    }

    public void setStartIntervall(long interval) {
        WAIT_BETWEEN_STARTS = Math.max(0, interval);
    }

    protected long getStartInterval(final DownloadLink downloadLink, final Account account) {
        return WAIT_BETWEEN_STARTS;
    }

    protected void waitForNextStartAllowed(final DownloadLink downloadLink, final Account account) throws PluginException, InterruptedException {
        final WaitingQueueItem queueItem = downloadLink.getDownloadLinkController().getQueueItem();
        final long wait = Math.max(0, getStartInterval(downloadLink, account));
        if (wait == 0) {
            queueItem.lastStartTimestamp.set(System.currentTimeMillis());
            return;
        }
        final PluginProgress progress = new PluginProgress(0, 0, null) {
            private String pluginMessage = null;

            @Override
            public String getMessage(Object requestor) {
                return pluginMessage;
            }

            @Override
            public PluginTaskID getID() {
                return PluginTaskID.WAIT;
            }

            @Override
            public void updateValues(long current, long total) {
                if (current > 0) {
                    pluginMessage = _JDT.T.gui_download_waittime_status2(Formatter.formatSeconds(current / 1000));
                } else {
                    pluginMessage = null;
                }
                super.updateValues(current, total);
            }
        };
        progress.setIcon(new AbstractIcon(IconKey.ICON_WAIT, 16));
        progress.setProgressSource(this);
        progress.setDisplayInProgressColumnEnabled(false);
        try {
            long lastQueuePosition = -1;
            long waitQueuePosition = -1;
            long waitMax = 0;
            long waitCur = 0;
            synchronized (queueItem) {
                if (!queueItem.lastStartTimestamp.compareAndSet(0, System.currentTimeMillis())) {
                    downloadLink.addPluginProgress(progress);
                    while ((waitQueuePosition = queueItem.indexOf(downloadLink)) >= 0 && !downloadLink.getDownloadLinkController().isAborting()) {
                        if (waitQueuePosition != lastQueuePosition) {
                            waitMax = (queueItem.lastStartTimestamp.get() - System.currentTimeMillis()) + ((waitQueuePosition + 1) * wait);
                            waitCur = waitMax;
                            lastQueuePosition = waitQueuePosition;
                        }
                        if (waitCur <= 0) {
                            break;
                        }
                        progress.updateValues(waitCur, waitMax);
                        long wTimeout = Math.min(1000, Math.max(0, waitCur));
                        queueItem.wait(wTimeout);
                        waitCur -= wTimeout;
                    }
                    if (downloadLink.getDownloadLinkController().isAborting()) {
                        throw new PluginException(LinkStatus.ERROR_RETRY);
                    }
                }
                queueItem.lastStartTimestamp.set(System.currentTimeMillis());
            }
        } catch (final InterruptedException e) {
            if (downloadLink.getDownloadLinkController().isAborting()) {
                throw new PluginException(LinkStatus.ERROR_RETRY, null, -1, e);
            } else {
                throw e;
            }
        } finally {
            downloadLink.removePluginProgress(progress);
        }
    }

    public void waitForNextConnectionAllowed(DownloadLink downloadLink) throws InterruptedException {
        final WaitingQueueItem queueItem = downloadLink.getDownloadLinkController().getQueueItem();
        long wait = getTimegapBetweenConnections();
        if (wait <= 0) {
            queueItem.lastConnectionTimestamp.set(System.currentTimeMillis());
            return;
        }
        while (true) {
            long lastConnectionTimestamp = queueItem.lastConnectionTimestamp.get();
            long waitCur = Math.max(0, lastConnectionTimestamp - System.currentTimeMillis() + wait);
            if (waitCur <= 0) {
                queueItem.lastConnectionTimestamp.set(System.currentTimeMillis());
                break;
            }
            if (downloadLink.getDownloadLinkController().isAborting()) {
                throw new InterruptedException("Controller aborted");
            }
            Thread.sleep(waitCur);
            if (queueItem.lastConnectionTimestamp.compareAndSet(lastConnectionTimestamp, System.currentTimeMillis())) {
                break;
            }
        }
        if (downloadLink.getDownloadLinkController().isAborting()) {
            throw new InterruptedException("Controller aborted");
        }
    }

    protected void sleep(final long i, final DownloadLink downloadLink) throws PluginException {
        sleep(i, downloadLink, "");
    }

    @Deprecated
    public void resetPluginGlobals() {
    }

    protected boolean isAbort() {
        final DownloadLink link = getDownloadLink();
        if (link != null) {
            final SingleDownloadController con = link.getDownloadLinkController();
            return (con != null && con.isAborting()) || Thread.currentThread().isInterrupted();
        } else {
            return super.isAbort();
        }
    }

    protected void sleep(long i, DownloadLink downloadLink, final String message) throws PluginException {
        if (isAbort()) {
            throw new PluginException(LinkStatus.ERROR_RETRY);
        }
        final PluginProgress progress = new SleepPluginProgress(i, message);
        progress.setProgressSource(this);
        progress.setDisplayInProgressColumnEnabled(false);
        try {
            downloadLink.addPluginProgress(progress);
            while (i > 0 && !isAbort()) {
                progress.setCurrent(i);
                synchronized (this) {
                    wait(Math.min(1000, Math.max(0, i)));
                }
                i -= 1000;
            }
        } catch (final InterruptedException e) {
            throw new PluginException(LinkStatus.ERROR_RETRY, null, -1, e);
        } finally {
            downloadLink.removePluginProgress(progress);
        }
        if (isAbort()) {
            throw new PluginException(LinkStatus.ERROR_RETRY);
        }
    }

    /**
     * Gibt die Url zurueck, unter welcher ein PremiumAccount gekauft werden kann
     *
     * @return
     */
    public String getBuyPremiumUrl() {
        return premiumurl;
    }

    public boolean isPremiumEnabled() {
        return enablePremium;
    }

    public DownloadLink setDownloadLink(DownloadLink link) {
        final DownloadLink ret = getDownloadLink();
        this.link = link;
        return ret;
    }

    public DownloadLink buildAccountCheckDownloadLink(final Account account) {
        String user = account.getUser();
        if (StringUtils.isEmpty(user)) {
            user = "";
        }
        return new DownloadLink(this, "Account (" + user + ")@" + account.getHoster(), getHost(), "https://" + account.getHoster(), true);
    }

    /**
     *
     * @param message
     *            The message to be displayed or <code>null</code> to display a Password prompt
     * @param link
     *            the {@link DownloadLink}
     * @return the entered password
     * @throws PluginException
     *             if the user aborts the input
     */
    public String getUserInput(final String title, String message, DownloadLink link) throws PluginException {
        if (message == null) {
            message = _GUI.T.AskForPasswordDialog_AskForPasswordDialog_title_();
        }
        if (link == null) {
            link = getDownloadLink();
        }
        final UserIOProgress prg = new UserIOProgress(message);
        prg.setProgressSource(getCurrentActivePlugin());
        prg.setDisplayInProgressColumnEnabled(false);
        try {
            link.addPluginProgress(prg);
            final AskDownloadPasswordDialogInterface handle = UIOManager.I().show(AskDownloadPasswordDialogInterface.class, new AskForDownloadLinkDialog(title, message, link));
            if (handle.getCloseReason() != CloseReason.OK) {
                throw new PluginException(LinkStatus.ERROR_FATAL, _JDT.T.plugins_errors_wrongpassword());
            }
            final String password = handle.getText();
            if (StringUtils.isEmpty(password)) {
                throw new PluginException(LinkStatus.ERROR_FATAL, _JDT.T.plugins_errors_wrongpassword());
            } else {
                return password;
            }
        } finally {
            link.removePluginProgress(prg);
        }
    }

    public String getUserInput(String message, final DownloadLink link) throws PluginException {
        return getUserInput(_GUI.T.AskForPasswordDialog_AskForPasswordDialog_title_(), message, link);
    }

    /**
     * Asks user to enter 2FA login code. <br>
     *
     * @params pattern: Allowed pattern of 2FA code. If a pattern is given and the entered code does not match the pattern, an exception
     *         will be thrown.
     */
    protected String getTwoFACode(final Account account, Object patternO) throws PluginException {
        if (account == null) {
            throw new IllegalArgumentException();
        }
        Pattern pattern = null;
        if (patternO != null) {
            if (patternO instanceof Pattern) {
                pattern = (Pattern) patternO;
            } else if (patternO instanceof String) {
                pattern = Pattern.compile(patternO.toString());
            } else {
                throw new IllegalArgumentException();
            }
        }
        final DownloadLink dl_dummy = new DownloadLink(this, "Account 2FA login", this.getHost(), "https://" + account.getHoster(), true);
        String twoFACode = getUserInput(org.jdownloader.gui.translate._GUI.T.jd_gui_swing_components_AccountDialog_2FA_login(), dl_dummy);
        if (twoFACode != null) {
            twoFACode = twoFACode.trim();
        }
        /* Validate result */
        if (StringUtils.isEmpty(twoFACode)) {
            /* This should never happen. */
            throw new AccountInvalidException("Invalid/empty 2FA result.");
        } else if (pattern != null && !new Regex(twoFACode, pattern).patternMatches()) {
            throw new AccountInvalidException(org.jdownloader.gui.translate._GUI.T.jd_gui_swing_components_AccountDialog_2FA_login_invalid_format(pattern.pattern()));
        }
        return twoFACode;
    }

    public long getAvailableStatusTimeout(DownloadLink link, AvailableStatus availableStatus) {
        if (availableStatus != null) {
            switch (availableStatus) {
            case TRUE:
            case FALSE:
                return 5 * 60 * 1000l;
            default:
                return 2 * 60 * 1000l;
            }
        } else {
            return 1 * 60 * 1000l;
        }
    }

    public DownloadLink getDownloadLink() {
        return link;
    }

    /**
     * Use this whenever you change the "main domain" of a plugin. It will then e.g. change for all existing downloadurls in a users'
     * downloadlist and also in Account Manager. To signal rewrite support this method must return destination host(eg getHost()) for
     * host==null
     *
     * see implementsRewriteHost
     *
     * @param host
     * @return
     */
    public String rewriteHost(String host) {
        if (host != null && host.equals(getHost())) {
            return getHost();
        } else {
            return null;
        }
    }

    protected String rewriteHost(List<String[]> pluginDomains, String host, String... rewriteEnabledHosts) {
        if (rewriteEnabledHosts == null || rewriteEnabledHosts.length == 0 || Arrays.asList(rewriteEnabledHosts).contains(getHost())) {
            if (host == null) {
                return getHost();
            } else {
                final String mapping = this.getMappedHost(pluginDomains, host);
                return mapping;
            }
        }
        return null;
    }

    public PluginForHost assignPlugin(PluginFinder pluginFinder, final DownloadLink link) {
        if (link == null) {
            return null;
        }
        link.setHost(getHost());
        link.setDefaultPlugin(this);
        return this;
    }

    public boolean assignPlugin(final Account account) {
        if (account == null) {
            return false;
        }
        final String oldHost = account.getHoster();
        List<String> hosterHistory = account.getHosterHistory();
        if (hosterHistory == null) {
            hosterHistory = new ArrayList<String>();
            account.setHosterHistory(hosterHistory);
        }
        if (!hosterHistory.contains(oldHost)) {
            hosterHistory.add(oldHost);
        }
        account.setHoster(getHost());
        account.setPlugin(this);
        return true;
    }

    public static boolean implementsRewriteHost(PluginForHost plugin) {
        if (plugin == null) {
            return false;
        }
        try {
            final Method method = plugin.getClass().getMethod("rewriteHost", new Class[] { String.class });
            final boolean implementsHandlePremium = method.getDeclaringClass() != PluginForHost.class;
            return implementsHandlePremium && plugin.rewriteHost((String) null) != null;
        } catch (NoSuchMethodException e) {
            LogController.CL().log(e);
            return false;
        } catch (Throwable e) {
            LogController.CL().log(e);
            return false;
        }
    }

    public static boolean implementsAllowHandle(PluginForHost plugin) {
        if (plugin == null) {
            return false;
        }
        try {
            final Method method = plugin.getClass().getMethod("allowHandle", new Class[] { DownloadLink.class, PluginForHost.class });
            final boolean implementsHandlePremium = method.getDeclaringClass() != PluginForHost.class;
            return implementsHandlePremium;
        } catch (NoSuchMethodException e) {
            return false;
        } catch (Throwable e) {
            LogController.CL().log(e);
            return false;
        }
    }

    /**
     * Determines whether or not mass- linkchecking is allowed. <br>
     * If it is always possible, simply override PluginForHost.checkLinks(final DownloadLink[] urls). <br>
     * If it is generally possible but not always e.g. depending whether an apikey is given or not, override this method. Example: <br>
     * org.jdownloader.plugins.components.XFileSharingProBasic
     */
    public boolean internal_supportsMassLinkcheck() {
        return implementsCheckLinks(this);
    }

    /** Does this plugin override the method required to do mass linkchecking? */
    public static boolean implementsCheckLinks(PluginForHost plugin) {
        if (plugin == null) {
            return false;
        }
        try {
            final Method method = plugin.getClass().getMethod("checkLinks", new DownloadLink[0].getClass());
            final boolean hasMassCheck = method.getDeclaringClass() != PluginForHost.class;
            return hasMassCheck;
        } catch (NoSuchMethodException e) {
            return false;
        } catch (Throwable e) {
            LogController.CL().log(e);
            return false;
        }
    }

    public static boolean implementsHandlePremium(PluginForHost plugin) {
        if (plugin == null) {
            return false;
        } else if (!plugin.isPremiumEnabled()) {
            return false;
        }
        try {
            final Method method = plugin.getClass().getMethod("handlePremium", new Class[] { DownloadLink.class, Account.class });
            final boolean implementsHandlePremium = method.getDeclaringClass() != PluginForHost.class;
            return implementsHandlePremium;
        } catch (NoSuchMethodException e) {
            return false;
        } catch (Throwable e) {
            LogController.CL().log(e);
            return false;
        }
    }

    public static boolean implementsSortDownloadLink(PluginForHost plugin) {
        if (plugin == null) {
            return false;
        }
        try {
            final Method method = plugin.getClass().getMethod("sortDownloadLinks", new Class[] { Account.class, List.class });
            final boolean implementsSortDownloadLink = method.getDeclaringClass() != PluginForHost.class;
            return implementsSortDownloadLink;
        } catch (NoSuchMethodException e) {
            return false;
        } catch (Throwable e) {
            LogController.CL().log(e);
            return false;
        }
    }

    /**
     *
     * Can we expect a captcha if we try to load link with/without account?
     *
     *
     * Use within plugin only.
     *
     * @param link
     * @param acc
     * @return
     */
    public boolean hasCaptcha(DownloadLink link, Account acc) {
        return false;
    }

    private static WeakHashMap<Account, HashMap<String, Boolean>> AUTOCAPTCHAMAP = new WeakHashMap<Account, HashMap<String, Boolean>>();

    public Boolean expectCaptcha(DownloadLink link, Account acc) {
        synchronized (AUTOCAPTCHAMAP) {
            final HashMap<String, Boolean> map = AUTOCAPTCHAMAP.get(acc);
            if (map != null) {
                final String ID = getHost() + "_" + (acc != null ? acc.getType() : "") + "_" + link.getHost();
                final Boolean captcha = map.get(ID);
                if (captcha != null) {
                    return captcha;
                }
            }
            return hasCaptcha(link, acc);
        }
    }

    public void setHasCaptcha(DownloadLink link, Account acc, Boolean hasCaptcha) {
        synchronized (AUTOCAPTCHAMAP) {
            if (hasCaptcha != null && hasCaptcha != hasCaptcha(link, acc)) {
                final SingleDownloadController controller = link.getDownloadLinkController();
                final LogInterface logger;
                if (controller != null) {
                    logger = controller.getLogger();
                } else {
                    logger = getLogger();
                }
                logger.info("Outdated hasCaptcha detected:" + getHost());
            }
            HashMap<String, Boolean> map = AUTOCAPTCHAMAP.get(acc);
            if (map == null && Boolean.TRUE.equals(hasCaptcha)) {
                map = new HashMap<String, Boolean>();
                AUTOCAPTCHAMAP.put(acc, map);
            }
            if (map != null) {
                final String ID = getHost() + "_" + (acc != null ? acc.getType() : "") + "_" + link.getHost();
                if (hasCaptcha == null) {
                    if (map.remove(ID) && map.size() == 0) {
                        AUTOCAPTCHAMAP.remove(acc);
                    }
                } else {
                    map.put(ID, hasCaptcha);
                }
            }
        }
    }

    /* Do we have anticaptcha available for this host? */
    /* ONLY override if you have customized this */
    public boolean hasAutoCaptcha() {
        return JACMethod.hasMethod(getHost());
    }

    /**
     * plugins may change the package identifier used for auto package matching. some hosters replace chars, shorten filenames...
     *
     * @param packageIdentifier
     * @return
     */
    public String filterPackageID(String packageIdentifier) {
        return packageIdentifier;
    }

    /**
     * Some hosters have bad filenames. Rapidshare for example replaces all special chars and spaces with _. Plugins can try to autocorrect
     * this based on other downloadlinks
     *
     * @param cache
     *            TODO
     * @param downloadable
     * @param dlinks
     * @param orgiginalfilename
     */
    // public String autoFilenameCorrection(String orgiginalfilename,
    // DownloadLink downloadLink, ArrayList<DownloadLink> dlinks) {
    // return null;
    // }
    public char[] getFilenameReplaceMap() {
        return new char[0];
    }

    protected URLConnectionAdapter checkDownloadableRequest(final DownloadLink link, final Browser br, final Request request, final long setSizeLargerThan, final boolean closeConnection) throws IOException, PluginException {
        if (request == null) {
            return null;
        }
        URLConnectionAdapter con = null;
        boolean closeFlag = true;
        try {
            request.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT_ENCODING, "identity");
            con = br.openRequestConnection(request);
            if (this.looksLikeDownloadableContent(con)) {
                final long completeContentLength = con.getCompleteContentLength();
                if (completeContentLength > setSizeLargerThan) {
                    if (con.isContentDecoded()) {
                        link.setDownloadSize(completeContentLength);
                    } else {
                        link.setVerifiedFileSize(completeContentLength);
                    }
                }
                closeFlag = closeConnection;
                return con;
            } else {
                try {
                    br.followConnection(true);
                } catch (IOException ignore) {
                    logger.log(ignore);
                }
                return null;
            }
        } finally {
            if (con != null && closeFlag) {
                try {
                    con.disconnect();
                } catch (final Throwable e) {
                }
            }
        }
    }

    public String autoFilenameCorrection(HashMap<Object, Object> cache, String originalFilename, DownloadLink downloadLink, ArrayList<DownloadLink> dlinks) {
        try {
            // cache = null;
            String MD5 = downloadLink.getMD5Hash();
            String SHA1 = downloadLink.getSha1Hash();
            String SHA256 = downloadLink.getSha256Hash();
            // auto partname correction
            /*
             * this holds the filename split into name, extension(. included)
             */
            String[] fileNameSplit = null;
            /*
             * this holds the Pattern got used to split the filename
             */
            Pattern pattern = null;
            if (cache != null) {
                /* load from cache */
                fileNameSplit = (String[]) cache.get(AUTO_FILE_NAME_CORRECTION_NAME_SPLIT + originalFilename);
                pattern = (Pattern) cache.get(AUTO_FILE_NAME_CORRECTION_NAME_SPLIT_PATTERN + originalFilename);
            }
            char[] originalReplaces = getFilenameReplaceMap();
            // find first match
            if (pattern == null) {
                for (Pattern p : PATTERNS) {
                    fileNameSplit = new Regex(originalFilename, p).getRow(0);
                    if (fileNameSplit != null) {
                        /*
                         * regex matched, so we should now have filename, extension in fileNameSplit
                         */
                        pattern = p;
                        if (cache != null) {
                            /* update cache */
                            cache.put(AUTO_FILE_NAME_CORRECTION_NAME_SPLIT + originalFilename, fileNameSplit);
                            cache.put(AUTO_FILE_NAME_CORRECTION_NAME_SPLIT_PATTERN + originalFilename, pattern);
                        }
                        break;
                    }
                }
            }
            if (fileNameSplit == null) {
                /*
                 * no valid pattern found,lets split filename into name/extension as fallback
                 */
                fileNameSplit = CrossSystem.splitFileName(originalFilename);
                pattern = null;
            }
            String filteredName = filterPackageID(fileNameSplit[0]);
            String prototypesplit;
            String newName;
            for (DownloadLink next : dlinks) {
                if (downloadLink == next) {
                    /* same link */
                    continue;
                }
                if (next.getHost().equals(getHost())) {
                    /* same host */
                    continue;
                }
                String prototypeName = next.getNameSetbyPlugin();
                if (prototypeName.equals(originalFilename)) {
                    /* same name */
                    continue;
                }
                if (prototypeName.equalsIgnoreCase(originalFilename)) {
                    /* same name but different upper/lower cases */
                    newName = fixCase(cache, originalFilename, prototypeName);
                    if (newName != null) {
                        return newName;
                    }
                }
                /*
                 * this holds the filename that got extracted with same pattern as the originalFilename
                 */
                prototypesplit = null;
                if (cache != null && pattern != null) {
                    /* load prototype split from cache if available */
                    prototypesplit = (String) cache.get(prototypeName + pattern.toString());
                }
                if (prototypesplit == null) {
                    /* no prototypesplit available yet, create new one */
                    if (pattern != null) {
                        /*
                         * a pattern does exist, we must use the same one to make sure the *filetypes* match (eg . part01.rar and .r01 with
                         * same filename
                         */
                        prototypesplit = new Regex(prototypeName, pattern).getMatch(0);
                    } else {
                        /* no pattern available, lets use fallback */
                        prototypesplit = CrossSystem.splitFileName(prototypeName)[0];
                    }
                    if (prototypesplit == null) {
                        /*
                         * regex did not match, different *filetypes*
                         */
                        continue;
                    }
                    if (cache != null && pattern != null) {
                        /* update cache */
                        cache.put(prototypeName + pattern.toString(), prototypesplit);
                    }
                }
                if (fileNameSplit[0].equals(prototypesplit)) {
                    continue;
                }
                if (isHosterManipulatesFilenames() && fileNameSplit[0].length() == prototypesplit.length() && filteredName.equalsIgnoreCase(filterPackageID(prototypesplit))) {
                    newName = getFixedFileName(cache, originalFilename, originalReplaces, prototypesplit, next.getDefaultPlugin().getFilenameReplaceMap());
                    if (newName != null) {
                        String caseFix = fixCase(cache, newName + fileNameSplit[1], prototypeName);
                        if (caseFix != null) {
                            /* we had to fix the upper/lower cases */
                            return caseFix;
                        }
                        /* we have new name, add extension to it */
                        return newName + fileNameSplit[1];
                    }
                }
                if ((!StringUtils.isEmpty(MD5) && MD5.equalsIgnoreCase(next.getMD5Hash())) || (!StringUtils.isEmpty(SHA1) && SHA1.equalsIgnoreCase(next.getSha1Hash())) || (!StringUtils.isEmpty(SHA256) && SHA256.equalsIgnoreCase(next.getSha256Hash()))) {
                    // 100% mirror! ok and now? these files should have the
                    // same filename!!
                    return next.getView().getDisplayName();
                }
            }
        } catch (Throwable e) {
            LogController.CL().log(e);
        }
        return null;
    }

    protected String getFixedFileName(HashMap<Object, Object> cache, String originalFilename, char[] originalReplaces, String prototypeName, char[] prototypeReplaces) {
        if (originalReplaces.length == 0 && prototypeReplaces.length == 0) {
            /* no replacements available */
            return null;
        }
        final Boolean original = (Boolean) cache.get(originalFilename + new String(originalReplaces));
        final Boolean prototype = (Boolean) cache.get(prototypeName + new String(prototypeReplaces));
        if (Boolean.FALSE.equals(original) && Boolean.FALSE.equals(prototype)) {
            return null;
        }
        final ArrayList<Character> foundOriginalReplaces = new ArrayList<Character>(originalReplaces.length);
        final ArrayList<Character> foundPrototypeReplaces = new ArrayList<Character>(prototypeReplaces.length);
        if (original == null) {
            for (int index = 0; index < originalReplaces.length; index++) {
                if (originalFilename.indexOf(originalReplaces[index]) >= 0) {
                    foundOriginalReplaces.add(originalReplaces[index]);
                }
            }
        }
        if (prototype == null) {
            for (int index = 0; index < prototypeReplaces.length; index++) {
                if (prototypeName.indexOf(prototypeReplaces[index]) >= 0) {
                    foundPrototypeReplaces.add(prototypeReplaces[index]);
                }
            }
        }
        if (original == null && foundOriginalReplaces.size() == 0) {
            cache.put(originalFilename + new String(originalReplaces), Boolean.FALSE);
        }
        if (prototype == null && foundPrototypeReplaces.size() == 0) {
            cache.put(prototypeName + new String(prototypeReplaces), Boolean.FALSE);
        }
        if (foundOriginalReplaces.size() == 0 && foundOriginalReplaces.size() == 0) {
            return null;
        }
        final StringBuilder sb = new StringBuilder();
        mainLoop: for (int i = 0; i < prototypeName.length(); i++) {
            char oC = originalFilename.charAt(i);
            char pC = prototypeName.charAt(i);
            if (Character.toLowerCase(oC) != Character.toLowerCase(pC)) {
                for (Character oCC : foundOriginalReplaces) {
                    /*
                     * first we check if char from Original is on replacement List, if so, we use char from prototype
                     */
                    if (oC == oCC.charValue()) {
                        sb.append(pC);
                        continue mainLoop;
                    }
                }
                for (Character pCC : foundPrototypeReplaces) {
                    /*
                     * then we check if char from prototype is on replacement List, if so, we use char from original
                     */
                    if (pC == pCC.charValue()) {
                        sb.append(oC);
                        continue mainLoop;
                    }
                }
                return null;
            } else {
                sb.append(oC);
            }
        }
        return sb.toString();
    }

    protected String fixCase(HashMap<Object, Object> cache, String originalFilename, String prototypeName) {
        if (cache != null) {
            Object ret = cache.get(originalFilename + "_" + prototypeName);
            if (ret != null) {
                return (String) ret;
            }
        }
        boolean eic = originalFilename.equals(prototypeName);
        StringBuilder sb = new StringBuilder(prototypeName.length());
        for (int i = 0; i < prototypeName.length(); i++) {
            char c = originalFilename.charAt(i);
            char correctc = prototypeName.charAt(i);
            if (Character.toLowerCase(c) == Character.toLowerCase(correctc)) {
                if (eic) {
                    sb.append(Character.isUpperCase(c) ? c : correctc);
                } else {
                    // for fixcase after rename cases
                    sb.append(correctc);
                }
                // may cause filename errors
            } else if (Character.isDigit(c) && Character.isDefined(correctc)) {
                sb.append(c);
            } else {
                return null;
            }
        }
        if (cache != null) {
            cache.put(originalFilename + "_" + prototypeName, sb.toString());
        }
        return sb.toString();
    }

    /**
     * Some hoster manipulate the filename after upload. rapidshare for example, replaces special chars and spaces with _
     *
     * @return
     */
    public boolean isHosterManipulatesFilenames() {
        return false;
    }

    /**
     * If a plugin want's to define it's one premium info dialog or premiuminfo panel. overwrite this methods
     *
     * @param dialog
     * @return
     */
    public JComponent layoutPremiumInfoPanel(AbstractDialog dialog) {
        return null;
    }

    /**
     * Can be overridden to support special accounts like login tokens instead of username/password
     *
     * @return
     */
    public AccountBuilderInterface getAccountFactory(final InputChangedCallbackInterface callback) {
        if (this.hasFeature(FEATURE.COOKIE_LOGIN_ONLY) || this.hasFeature(FEATURE.COOKIE_LOGIN_OPTIONAL)) {
            return new DefaultEditAccountPanelCookieLogin(callback, this);
        } else if (this.hasFeature(FEATURE.USERNAME_IS_EMAIL)) {
            return new DefaultEditAccountPanelCookieLogin(callback, this);
        } else if (this.hasFeature(FEATURE.API_KEY_LOGIN)) {
            return new DefaultEditAccountPanelAPIKeyLogin(callback, this);
        } else {
            return new DefaultEditAccountPanel(callback, false);
        }
    }

    public void resumeDownloadlink(DownloadLink downloadLink) {
    }

    public boolean checkLinks(final DownloadLink[] urls) {
        return false;
    }

    public AvailableStatus checkLink(DownloadLink downloadLink) throws Exception {
        return requestFileInformation(downloadLink);
    }

    public AccountConfigInterface getAccountJsonConfig(Account acc) {
        return AccountJsonConfig.get(this, acc);
    }

    @Override
    public boolean isHandlingMultipleHosts() {
        try {
            if (hasFeature(FEATURE.GENERIC)) {
                // new way to signal multiple hosts/generic support by setting FEATURE.GENERIC
                return true;
            } else {
                // old way to signal multiple hosts/generic support by overriding getHost(DownloadLink,Account,boolean) method
                final Method method = this.getClass().getMethod("getHost", new Class[] { DownloadLink.class, Account.class, boolean.class });
                final boolean ret = method.getDeclaringClass() != PluginForHost.class;
                return ret;
            }
        } catch (Throwable e) {
        }
        return super.isHandlingMultipleHosts();
    }

    @Override
    protected void displayBubbleNotification(final String title, final String text) {
        final DownloadLink link = getDownloadLink();
        displayBubbleNotification(title, text, link != null ? link.getDomainInfo().getIcon(32) : null);
    }

    @Override
    public String getCrawlerLoggerID(CrawledLink link) {
        return getHost() + "_" + getLazyP().getClassName();
    }

    public void setActiveVariantByLink(DownloadLink downloadLink, LinkVariant variant) {
        downloadLink.setVariant(variant);
        if (variant instanceof GenericVariants) {
            final GenericVariants v = (GenericVariants) variant;
            switch (v) {
            case ORIGINAL:
                downloadLink.setCustomExtension(null);
                break;
            default:
                downloadLink.setCustomExtension(v.getExtension());
                break;
            }
        }
    }

    public LinkVariant getActiveVariantByLink(DownloadLink downloadLink) {
        return downloadLink.getVariant(GenericVariants.class);
    }

    public List<? extends LinkVariant> getVariantsByLink(DownloadLink downloadLink) {
        return downloadLink.getVariants(GenericVariants.class);
    }

    public JComponent getVariantPopupComponent(DownloadLink downloadLink) {
        return null;
    }

    public boolean hasVariantToChooseFrom(DownloadLink downloadLink) {
        final List<? extends LinkVariant> variants = getVariantsByLink(downloadLink);
        return variants != null && variants.size() > 0;
    }

    protected void updateDownloadLink(final CheckableLink checkableLink, final String url) {
        final DownloadLink downloadLink = checkableLink != null ? checkableLink.getDownloadLink() : null;
        if (downloadLink == null || url == null) {
            return;
        }
        downloadLink.setPluginPatternMatcher(url);
        downloadLink.setDomainInfo(null);
        downloadLink.resume(Arrays.asList(new PluginForHost[] { this }));
        final LinkChecker<CheckableLink> linkChecker = new LinkChecker<CheckableLink>(true);
        linkChecker.check(checkableLink);
    }

    protected boolean supportsUpdateDownloadLink(final CheckableLink downloadLink) {
        return false;
    }

    /**
     * no Browser instance available!
     *
     * @param host
     * @return
     * @throws IOException
     */
    public Object getFavIcon(final String host) throws IOException {
        return null;
    }

    protected JMenuItem createChangeURLMenuItem(final CheckableLink checkableLink) {
        final DownloadLink downloadLink = checkableLink != null ? checkableLink.getDownloadLink() : null;
        if (downloadLink == null || UrlProtection.PROTECTED_CONTAINER.equals(downloadLink.getUrlProtection())) {
            return null;
        }
        return new JMenuItem(new BasicAction() {
            /**
             *
             */
            private static final long serialVersionUID = 5968961149921441923L;
            private final BadgeIcon   icon;
            {
                icon = new BadgeIcon(downloadLink.getDomainInfo(), new AbstractIcon(IconKey.ICON_URL, 16), 4, 4);
                setName(_GUI.T.lit_change_url());
                setSmallIcon(icon);
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                final InputDialogInterface ret = UIOManager.I().show(InputDialogInterface.class, new InputDialog(0, " " + downloadLink.getName(), _GUI.T.lit_change_url(), downloadLink.getPluginPatternMatcher(), icon, null, null));
                try {
                    ret.throwCloseExceptions();
                    final String url = ret.getText();
                    if (!StringUtils.equals(downloadLink.getPluginPatternMatcher(), url)) {
                        if (checkableLink instanceof CrawledLink) {
                            LinkCollector.getInstance().getQueue().add(new QueueAction<Void, RuntimeException>() {
                                @Override
                                protected Void run() throws RuntimeException {
                                    updateDownloadLink(checkableLink, url);
                                    return null;
                                }
                            });
                        } else {
                            DownloadWatchDog.getInstance().enqueueJob(new DownloadWatchDogJob() {
                                @Override
                                public boolean isHighPriority() {
                                    return false;
                                }

                                @Override
                                public void interrupt() {
                                }

                                @Override
                                public void execute(DownloadSession currentSession) {
                                    final SingleDownloadController con = downloadLink.getDownloadLinkController();
                                    if (con == null) {
                                        updateDownloadLink(checkableLink, url);
                                    } else {
                                        con.getJobsAfterDetach().add(new DownloadWatchDogJob() {
                                            @Override
                                            public void execute(DownloadSession currentSession) {
                                                updateDownloadLink(checkableLink, url);
                                            }

                                            @Override
                                            public void interrupt() {
                                            }

                                            @Override
                                            public boolean isHighPriority() {
                                                return false;
                                            }
                                        });
                                    }
                                }
                            });
                        }
                    }
                } catch (DialogNoAnswerException ignore) {
                }
            }
        });
    }

    public List<JComponent> extendLinkgrabberContextMenu(final AtomicBoolean isCancelled, final JComponent parent, final PluginView<CrawledLink> pv, Collection<PluginView<CrawledLink>> allPvs) {
        final List<JComponent> ret = new ArrayList<JComponent>();
        if (pv.size() == 1 && supportsUpdateDownloadLink(pv.get(0))) {
            final JMenuItem changeURLMenuItem = createChangeURLMenuItem(pv.get(0));
            if (changeURLMenuItem != null) {
                ret.add(changeURLMenuItem);
            }
        }
        if (allPvs.size() != 1) {
            return ret;
        }
        final JMenu setVariants = new JMenu(_GUI.T.PluginForHost_extendLinkgrabberContextMenu_generic_convert());
        final DomainInfo domainInfo = DomainInfo.getInstance(getHost());
        setVariants.setIcon(domainInfo.getFavIcon());
        setVariants.setEnabled(false);
        final JMenu addVariants = new JMenu("Add converted variant...");
        addVariants.setIcon(new BadgeIcon(domainInfo, new AbstractIcon(IconKey.ICON_ADD, 16), 4, 4));
        addVariants.setEnabled(false);
        new Thread("Collect Variants") {
            {
                setDaemon(true);
            }

            public void run() {
                try {
                    Thread.sleep(2000);
                } catch (InterruptedException e1) {
                    e1.printStackTrace();
                }
                HashSet<GenericVariants> map = new HashSet<GenericVariants>();
                final ArrayList<GenericVariants> list = new ArrayList<GenericVariants>();
                for (CrawledLink cl : pv.getChildren()) {
                    if (isCancelled.get()) {
                        break;
                    }
                    if (cl.getDownloadLink() == null || !cl.getDownloadLink().getBooleanProperty("GENERIC_VARIANTS", false) || !cl.getDownloadLink().hasVariantSupport()) {
                        continue;
                    }
                    List<GenericVariants> v = cl.getDownloadLink().getVariants(GenericVariants.class);
                    if (v != null) {
                        for (LinkVariant lv : v) {
                            if (lv instanceof GenericVariants) {
                                if (map.add((GenericVariants) lv)) {
                                    list.add((GenericVariants) lv);
                                }
                            }
                        }
                    }
                }
                if (list.size() == 0) {
                    return;
                }
                Collections.sort(list, new Comparator<GenericVariants>() {
                    @Override
                    public int compare(GenericVariants o1, GenericVariants o2) {
                        return o1.name().compareTo(o2.name());
                    }
                });
                if (isCancelled.get()) {
                    return;
                }
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        setVariants.setEnabled(true);
                        addVariants.setEnabled(true);
                        setVariants.setVisible(true);
                        addVariants.setVisible(true);
                        for (final GenericVariants gv : list) {
                            setVariants.add(new JMenuItem(new BasicAction() {
                                {
                                    setName(gv._getName(link));
                                }

                                @Override
                                public void actionPerformed(ActionEvent e) {
                                    java.util.List<CheckableLink> checkableLinks = new ArrayList<CheckableLink>(1);
                                    for (CrawledLink cl : pv.getChildren()) {
                                        // List<GenericVariants> variants = new ArrayList<GenericVariants>();
                                        for (LinkVariant v : getVariantsByLink(cl.getDownloadLink())) {
                                            if (v.equals(gv)) {
                                                LinkCollector.getInstance().setActiveVariantForLink(cl, gv);
                                                checkableLinks.add(cl);
                                                break;
                                            }
                                        }
                                    }
                                    LinkChecker<CheckableLink> linkChecker = new LinkChecker<CheckableLink>(true);
                                    linkChecker.check(checkableLinks);
                                }
                            }));
                            addVariants.add(new JMenuItem(new BasicAction() {
                                {
                                    setName(gv._getName(link));
                                }

                                @Override
                                public void actionPerformed(ActionEvent e) {
                                    java.util.List<CheckableLink> checkableLinks = new ArrayList<CheckableLink>(1);
                                    for (CrawledLink cl : pv.getChildren()) {
                                        List<GenericVariants> variants = new ArrayList<GenericVariants>();
                                        for (LinkVariant v : getVariantsByLink(cl.getDownloadLink())) {
                                            if (v.equals(gv)) {
                                                CrawledLink newLink = LinkCollector.getInstance().addAdditional(cl, gv);
                                                if (newLink != null) {
                                                    checkableLinks.add(newLink);
                                                } else {
                                                    Toolkit.getDefaultToolkit().beep();
                                                }
                                                break;
                                            }
                                        }
                                    }
                                    LinkChecker<CheckableLink> linkChecker = new LinkChecker<CheckableLink>(true);
                                    linkChecker.check(checkableLinks);
                                }
                            }));
                        }
                    }
                };
            };
        }.start();
        ret.add(setVariants);
        ret.add(addVariants);
        return ret;
    }

    public List<JComponent> extendDownloadsTableContextMenu(final AtomicBoolean isCancelled, JComponent parent, PluginView<DownloadLink> pv, Collection<PluginView<DownloadLink>> views) {
        if (pv.size() == 1 && supportsUpdateDownloadLink(pv.get(0))) {
            final JMenuItem changeURLMenuItem = createChangeURLMenuItem(pv.get(0));
            if (changeURLMenuItem != null) {
                final List<JComponent> ret = new ArrayList<JComponent>();
                ret.add(changeURLMenuItem);
                return ret;
            }
        }
        return null;
    }

    public Downloadable newDownloadable(DownloadLink downloadLink, final Browser br) {
        return new DownloadLinkDownloadable(downloadLink, br);
    }

    /**
     * sort accounts for best order to download downloadLink
     *
     * @param accounts
     * @param downloadLink
     * @return
     */
    public List<Account> sortAccounts(DownloadLink downloadLink, List<Account> accounts) {
        return accounts;
    }

    /**
     * sort downloadLinks for best order to download via account
     *
     * @param accounts
     * @param downloadLink
     * @return
     */
    public List<DownloadLink> sortDownloadLinks(Account account, List<DownloadLink> downloadLinks) {
        return downloadLinks;
    }

    protected AskToUsePremiumDialog createAskToUsePremiumDialog() {
        final AskToUsePremiumDialog dialog = new AskToUsePremiumDialog(this) {
            @Override
            public String getDontShowAgainKey() {
                return "adsPremium_" + getDomain();
            }
        };
        dialog.setTimeout(1 * 60 * 1000);
        return dialog;
    }

    @Deprecated
    protected void showFreeDialog(final String domain) throws PluginException {
        showFreeDialog();
    }

    protected void showFreeDialog() throws PluginException {
        final AskToUsePremiumDialog dialog = createAskToUsePremiumDialog();
        try {
            UIOManager.I().show(AskToUsePremiumDialogInterface.class, dialog).throwCloseExceptions();
            CrossSystem.openURL(new URL(dialog.getPremiumUrl()));
        } catch (DialogNoAnswerException e) {
            logger.log(e);
        } catch (MalformedURLException e) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, null, -1, e);
        }
    }

    private static Object CHECKSHOWFREEDIALOGLOCK = new Object();

    protected boolean checkShowFreeDialog(final String domain) {
        if (domain == null) {
            return false;
        } else if (Application.isHeadless()) {
            return false;
        }
        try {
            synchronized (CHECKSHOWFREEDIALOGLOCK) {
                final String key = JDHash.getMD5(domain) + "_05032020";
                final long TIMEOUT = 1000l * 60 * 60 * 24 * 31 * 2;
                long lastTimestamp = -1;
                SubConfiguration config = null;
                try {
                    config = getPluginConfig();
                    final Object value = config.getProperty(key, null);
                    if (value != null) {
                        try {
                            lastTimestamp = Long.parseLong(value.toString());
                        } catch (final Throwable e) {
                        }
                    }
                    if (lastTimestamp < 0 || System.currentTimeMillis() - lastTimestamp > TIMEOUT) {
                        lastTimestamp = System.currentTimeMillis();
                        return true;
                    } else {
                        config = null;
                        lastTimestamp = -1;
                    }
                } finally {
                    if (config != null && lastTimestamp > 0) {
                        config.setProperty(key, Long.toString(lastTimestamp));
                        config.save();
                    }
                }
            }
        } catch (final Throwable e) {
            logger.log(e);
        }
        return false;
    }

    public static class FilePair {
        public FilePair(File oldFile, File newFile) {
            this.oldFile = oldFile;
            this.newFile = newFile;
        }

        private final File oldFile;

        public File getOldFile() {
            return oldFile;
        }

        public File getNewFile() {
            return newFile;
        }

        private final File newFile;
    }

    /**
     * Do not call directly. This method is called from the DownloadWatchdog.rename method only. The DownloadWatchdog assures, that the
     * method is not called during a processing download, but afterwards. Avoid to override this method. if possible, try to override
     * #listFilePairsToMove instead
     *
     * @param link
     * @param string2
     * @param string
     * @param value
     */
    public void move(DownloadLink link, String currentDirectory, String currentName, String newDirectory, String newName) throws Exception {
        if (link.getView().getBytesLoaded() <= 0) {
            // nothing to rename or move. there should not be any file, and if there is, it does not belong to the link
            return;
        }
        if (StringUtils.isEmpty(newName)) {
            newName = currentName;
        }
        if (StringUtils.isEmpty(newDirectory)) {
            newDirectory = currentDirectory;
        }
        if (CrossSystem.isWindows()) {
            if (StringUtils.equalsIgnoreCase(currentDirectory, newDirectory) && StringUtils.equalsIgnoreCase(currentName, newName)) {
                return;
            }
        } else {
            if (StringUtils.equals(currentDirectory, newDirectory) && StringUtils.equals(currentName, newName)) {
                return;
            }
        }
        final ArrayList<ExceptionRunnable> revertList = new ArrayList<ExceptionRunnable>();
        final MovePluginProgress progress = new MovePluginProgress();
        try {
            link.addPluginProgress(progress);
            progress.setProgressSource(this);
            for (FilePair filesToHandle : listFilePairsToMove(link, currentDirectory, currentName, newDirectory, newName)) {
                handle(revertList, link, progress, filesToHandle.getOldFile(), filesToHandle.getNewFile());
            }
            revertList.clear();
        } catch (Exception e) {
            getLogger().log(e);
            throw e;
        } finally {
            try {
                // revert
                for (final ExceptionRunnable r : revertList) {
                    try {
                        if (r != null) {
                            r.run();
                        }
                    } catch (Throwable e1) {
                        getLogger().log(e1);
                    }
                }
            } finally {
                link.removePluginProgress(progress);
            }
        }
    }

    protected FilePair[] listFilePairsToMove(DownloadLink link, String currentDirectory, String currentName, String newDirectory, String newName) {
        FilePair[] ret = new FilePair[2];
        ret[0] = new FilePair(new File(new File(currentDirectory), currentName + ".part"), new File(new File(newDirectory), newName + ".part"));
        ret[1] = new FilePair(new File(new File(currentDirectory), currentName), new File(new File(newDirectory), newName));
        return ret;
    }

    private void handle(ArrayList<ExceptionRunnable> revertList, final DownloadLink downloadLink, final MovePluginProgress progress, final File currentFile, final File newFile) throws FileExistsException, CouldNotRenameException, IOException {
        if (!currentFile.exists()) {
            /* Do nothing */
            return;
        } else if (currentFile.equals(newFile)) {
            /* Do nothing */
            return;
        }
        progress.setFile(newFile);
        revertList.add(new ExceptionRunnable() {
            @Override
            public void run() throws Exception {
                renameOrMove(progress, downloadLink, newFile, currentFile);
            }
        });
        renameOrMove(progress, downloadLink, currentFile, newFile);
    }

    private void renameOrMove(MovePluginProgress progress, final DownloadLink downloadLink, File old, File newFile) throws FileExistsException, CouldNotRenameException, IOException {
        if (newFile.exists()) {
            throw new FileExistsException(old, newFile);
        } else if (!newFile.getParentFile().exists() && !newFile.getParentFile().mkdirs()) {
            throw new IOException("Could not create " + newFile.getParent());
        }
        try {
            getLogger().info("Move " + old + " to " + newFile);
            if (CrossSystem.isWindows() && Application.getJavaVersion() >= Application.JAVA17) {
                java.nio.file.Files.move(java.nio.file.Paths.get(old.toURI()), java.nio.file.Paths.get(newFile.toURI()), java.nio.file.StandardCopyOption.ATOMIC_MOVE);
            } else if (!old.renameTo(newFile)) {
                throw new CouldNotRenameException(old, newFile);
            }
        } catch (CouldNotRenameException e) {
            getLogger().log(e);
            copyMove(progress, downloadLink, old, newFile);
        } catch (IOException e) {
            getLogger().log(e);
            copyMove(progress, downloadLink, old, newFile);
        }
        // TODO copy optimiz
        if (!newFile.getParentFile().equals(old.getParentFile())) {
            // check if we have to delete the old path;
            if (!old.getParentFile().equals(new File(CFG_GENERAL.DEFAULT_DOWNLOAD_FOLDER.getValue()))) {
                // we ignore the dynamic tags here. if the default downloaddirectory contains dynamic tags, we can delete the folders
                // anyaway if empty.
                old.getParentFile().delete();
            }
        }
    }

    private void copyMove(final MovePluginProgress progress, final DownloadLink downloadLink, final File old, final File newFile) throws IOException {
        if (!old.exists() && newFile.exists()) {
            return;
        } else if (!old.exists()) {
            throw new IOException("Cannot move " + old + " to " + newFile + ". The File does not exist!");
        }
        // we did an file exists check earlier. so if the file exists here, the only reason is a failed rename/move;
        newFile.delete();
        Thread thread = null;
        if (JSonStorage.getPlainStorage("Dialogs").get(COPY_MOVE_FILE, -1) < 0) {
            // System.out.println("Thread start");
            thread = new Thread() {
                public void run() {
                    try {
                        Thread.sleep(3000);
                        // System.out.println("Dialog go");
                        ProgressDialog dialog = new ProgressDialog(new ProgressGetter() {
                            @Override
                            public void run() throws Exception {
                                while (true) {
                                    Thread.sleep(1000);
                                }
                            }

                            @Override
                            public String getString() {
                                return _JDT.T.lit_please_wait();
                            }

                            @Override
                            public int getProgress() {
                                double perc = progress.getPercent();
                                return Math.min(99, (int) (perc));
                            }

                            @Override
                            public String getLabelString() {
                                return null;
                            }
                        }, Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, _GUI.T.PluginForHost_copyMove_progressdialog_title(), null, new AbstractIcon(IconKey.ICON_SAVETO, 32), null, _JDT.T.lit_hide()) {
                            @Override
                            public String getDontShowAgainKey() {
                                return COPY_MOVE_FILE;
                            }

                            private Component leftLabel(String name) {
                                JLabel ret = new JLabel(name);
                                ret.setHorizontalAlignment(SwingConstants.LEFT);
                                return ret;
                            }

                            protected void extendLayout(JPanel p) {
                                if (p.getComponentCount() == 0) {
                                    final JPanel subp = new MigPanel("ins 0,wrap 1", "[]", "[][]");
                                    p.add(subp, "wrap");
                                    p = subp;
                                    String packagename = downloadLink.getParentNode().getName();
                                    p.add(SwingUtils.toBold(new JLabel(_GUI.T.lit_hoster())), "split 2,sizegroup left,alignx left");
                                    DomainInfo di = downloadLink.getDomainInfo();
                                    JLabel ret = new JLabel(di.getTld());
                                    ret.setHorizontalAlignment(SwingConstants.LEFT);
                                    ret.setIcon(di.getFavIcon());
                                    p.add(ret);
                                    if (downloadLink.getParentNode() != FilePackage.getDefaultFilePackage()) {
                                        p.add(SwingUtils.toBold(new JLabel(_GUI.T.IfFileExistsDialog_layoutDialogContent_package())), "split 2,sizegroup left,alignx left");
                                        p.add(leftLabel(packagename));
                                    }
                                    p.add(SwingUtils.toBold(new JLabel(_GUI.T.lit_filesize())), "split 2,sizegroup left,alignx left");
                                    p.add(leftLabel(SizeFormatter.formatBytes(old.length())));
                                    if (newFile.getName().equals(old.getName())) {
                                        p.add(SwingUtils.toBold(new JLabel(_GUI.T.lit_filename())), "split 2,sizegroup left,alignx left");
                                        p.add(leftLabel(newFile.getName()));
                                    } else {
                                        p.add(SwingUtils.toBold(new JLabel(_GUI.T.PLUGINFORHOST_MOVECOPY_DIALOG_OLDFILENAME())), "split 2,sizegroup left,alignx left");
                                        p.add(leftLabel(old.getName()));
                                        p.add(SwingUtils.toBold(new JLabel(_GUI.T.PLUGINFORHOST_MOVECOPY_DIALOG_NEWFILENAME())), "split 2,sizegroup left,alignx left");
                                        p.add(leftLabel(newFile.getName()));
                                    }
                                    p.add(SwingUtils.toBold(new JLabel(_GUI.T.PLUGINFORHOST_MOVECOPY_DIALOG_OLD())), "split 2,sizegroup left,alignx left");
                                    p.add(leftLabel(old.getParent()));
                                    p.add(SwingUtils.toBold(new JLabel(_GUI.T.PLUGINFORHOST_MOVECOPY_DIALOG_NEW())), "split 2,sizegroup left,alignx left");
                                    p.add(leftLabel(newFile.getParent()));
                                }
                            }
                        };
                        UIOManager.I().show(null, dialog);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            };
            thread.start();
        } else {
            // System.out.println("Do not show again " + JSonStorage.getPlainStorage("Dialogs").get(COPY_MOVE_FILE, -1));
        }
        try {
            IO.copyFile(new ProgressFeedback() {
                @Override
                public void setBytesTotal(long length) {
                    progress.setTotal(length);
                }

                @Override
                public void setBytesProcessed(long position) {
                    progress.setCurrent(position);
                }
            }, old, newFile, SYNC.META_AND_DATA);
            old.delete();
        } catch (IOException io) {
            newFile.delete();
            throw io;
        } finally {
            if (thread != null) {
                thread.interrupt();
            }
        }
    }

    public boolean isProxyRotationEnabledForLinkChecker() {
        return true;
    }

    /**
     * Used to disable slow speed warning, useful in plugins which have login services but may not provide speed increases.
     *
     * @param link
     * @param account
     * @return
     */
    public boolean isSpeedLimited(DownloadLink link, Account account) {
        if (link == null) {
            return false;
        }
        if (StringUtils.equals(link.getHost(), getHost())) {
            if (hasFeature(LazyPlugin.FEATURE.MULTIHOST)) {
                return false;
            } else {
                // link and plugin from same service
                return isPremiumEnabled() && account == null;
            }
        } else {
            return false;
        }
    }

    /**
     * plugins may set a mirrorid to help the mirror detector. You have to ensure, that two mirrors either get the same mirror id, or no
     * mirrorid(null)
     *
     * @return
     */
    public String getMirrorID(DownloadLink link) {
        return null;
    }

    /** Returns downloadurls for external services. E.g. use this in multihost plugins instead of e.g. getPluginPatternMatcher! */
    public String buildExternalDownloadURL(DownloadLink downloadLink, PluginForHost buildForThisPlugin) {
        return downloadLink.getPluginPatternMatcher();
    }

    public String buildContainerDownloadURL(DownloadLink downloadLink, PluginForHost buildForThisPlugin) {
        final String ret = downloadLink.getContentUrl();
        if (ret != null) {
            return ret;
        } else {
            return downloadLink.getPluginPatternMatcher();
        }
    }

    public void setLinkID(DownloadLink downloadLink, LinkVariant variant) {
        final boolean isOriginal = variant == null || GenericVariants.ORIGINAL.equals(variant);
        final String orgLinkID = downloadLink.getStringProperty("ORG_LINKID");
        if (isOriginal) {
            if (orgLinkID != null) {
                downloadLink.setLinkID(orgLinkID);
            }
        } else {
            if (orgLinkID == null) {
                final String linkID = downloadLink.getLinkID();
                downloadLink.setProperty("ORG_LINKID", linkID);
                downloadLink.setLinkID(linkID + "_" + variant._getUniqueId());
            } else {
                downloadLink.setLinkID(orgLinkID + "_" + variant._getUniqueId());
            }
        }
    }

    public List<GenericVariants> getGenericVariants(DownloadLink downloadLink) {
        final List<String> converts = getConvertToList(downloadLink);
        if (converts != null && converts.size() > 0) {
            final List<GenericVariants> variants = new ArrayList<GenericVariants>();
            variants.add(GenericVariants.ORIGINAL);
            for (final String v : converts) {
                try {
                    variants.add(GenericVariants.valueOf(v));
                } catch (Throwable e) {
                    e.printStackTrace();
                }
            }
            if (variants.size() > 1) {
                return variants;
            }
        } else {
            final String name = downloadLink.getName();
            final String exten = Files.getExtension(name);
            if (exten != null) {
                boolean isVideo = false;
                for (final ExtensionsFilterInterface extension : VideoExtensions.values()) {
                    final Pattern pattern = extension.getPattern();
                    if (pattern != null && pattern.matcher(exten).matches()) {
                        isVideo = true;
                        break;
                    }
                }
                if (isVideo) {
                    final List<GenericVariants> variants = new ArrayList<GenericVariants>();
                    variants.add(GenericVariants.DEMUX_GENERIC_AUDIO);
                    return variants;
                }
            }
        }
        return null;
    }

    public boolean hasFeature(final LazyPlugin.FEATURE feature) {
        if (feature == null) {
            return false;
        }
        final LazyPlugin.FEATURE[] features = getFeatures();
        if (features == null) {
            return false;
        }
        for (int i = 0; i < features.length; i++) {
            if (features[i] == feature) {
                return true;
            }
        }
        return false;
    }

    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[0];
    }

    public List<String> getConvertToList(DownloadLink downloadLink) {
        return null;
    }

    public UrlProtection getUrlProtection(List<DownloadLink> hosterLinks) {
        return null;
    }

    /**
     * Designed to return first result from decrypter task. This way we don't need to separate decrypters into separate classes like we had
     * todo with adfly just for jd.plugins.hoster.IdupIn.getDllink() rev30650
     *
     * @author raztoki
     * @param url
     * @return
     */
    public String returnDecrypterTaskResult(final String url) {
        final LinkCrawler lc = LinkCrawler.newInstance();
        lc.crawl(url);
        lc.waitForCrawling();
        return lc.getCrawledLinks().size() >= 1 ? lc.getCrawledLinks().get(0).getURL() : null;
    }

    public boolean fillVariantsPopup(VariantColumn variantColumn, JPopupMenu popup, AbstractNode value, LinkVariant selected, ComboBoxModel<LinkVariant> dm) {
        return false;
    }

    public boolean onLinkCollectorDupe(CrawledLink existingLink, CrawledLink newLink) {
        return false;
    }

    public boolean onLinkCrawlerDupeFilterEnabled(CrawledLink existingLink, CrawledLink newLink) {
        return true;
    }

    public Class<? extends AccountConfigInterface> getAccountConfigInterface(Account account) {
        Class<?> currentClass = getClass();
        while (currentClass != null && PluginForHost.class.isAssignableFrom(currentClass)) {
            for (final Class<?> cls : currentClass.getDeclaredClasses()) {
                if (AccountConfigInterface.class.isAssignableFrom(cls)) {
                    return (Class<? extends AccountConfigInterface>) cls;
                }
            }
            currentClass = currentClass.getSuperclass();
        }
        return null;
    }

    public void extendMultiHostAccountSettingsPanel(final Account acc, final PluginConfigPanelNG panel) {
        if (!acc.isEnabled()) {
            /**
             * Do not display detailed supported host information for disabled accounts because: <br>
             * - Takes away a lot of space <br>
             * - Supported host information may change frequently so chances are super high that the information we have is outdated
             */
            return;
        }
        final AccountInfo ai = acc.getAccountInfo();
        if (ai == null) {
            return;
        }
        final List<MultiHostHost> hosts = ai.getMultiHostSupportV2();
        if (hosts == null || hosts.isEmpty()) {
            return;
        }
        /* Determine default visibility states for some columns */
        boolean shouldShowLinkLimitColumns = false;
        boolean shouldShowTrafficLimitColumns = false;
        boolean shouldShowTrafficCaculationColumn = false;
        boolean shouldShowUnavailableForColumn = false;
        boolean containsItemsWithCustomStatusText = false;
        for (final MultiHostHost mhost : hosts) {
            if (!shouldShowLinkLimitColumns && !mhost.isUnlimitedLinks()) {
                shouldShowLinkLimitColumns = true;
            }
            if (!shouldShowTrafficLimitColumns && !mhost.isUnlimitedTraffic()) {
                shouldShowTrafficLimitColumns = true;
            }
            if (!shouldShowTrafficCaculationColumn && mhost.getTrafficCalculationFactorPercent() != 100) {
                shouldShowTrafficCaculationColumn = true;
            }
            if (!shouldShowUnavailableForColumn && mhost.getUnavailableTimeMillis() > 0) {
                shouldShowUnavailableForColumn = true;
            }
            if (!containsItemsWithCustomStatusText && mhost.getStatusText() != null) {
                containsItemsWithCustomStatusText = true;
            }
            if (shouldShowTrafficLimitColumns && shouldShowLinkLimitColumns && shouldShowTrafficCaculationColumn && shouldShowUnavailableForColumn) {
                break;
            }
        }
        final boolean shouldShowLinkLimitColumns_final = shouldShowLinkLimitColumns;
        final boolean shouldShowTrafficLimitColumns_final = shouldShowTrafficLimitColumns;
        final boolean shouldShowTrafficCaculationColumn_final = shouldShowTrafficCaculationColumn;
        final boolean shouldShowUnavailableForColumn_final = shouldShowUnavailableForColumn;
        final boolean shouldShowUInternalStatusColumn_final = containsItemsWithCustomStatusText;
        final Icon icon_error = NewTheme.I().getIcon(IconKey.ICON_ERROR, 16);
        final Icon icon_okay = NewTheme.I().getIcon(IconKey.ICON_OK, 16);
        final Icon icon_warning = NewTheme.I().getIcon(IconKey.ICON_WARNING, 16);
        final Icon icon_wait = NewTheme.I().getIcon(IconKey.ICON_WAIT, 16);
        final Icon icon_filler = NewTheme.I().getIcon(IconKey.ICON_BEER, 16);
        final ExtTableModel<MultiHostHost> tableModel = new ExtTableModel<MultiHostHost>("MultiHostHostTable_" + acc.getHoster()) {
            @Override
            protected void initColumns() {
                addColumn(new ExtCheckColumn<MultiHostHost>(_GUI.T.premiumaccounttablemodel_column_enabled()) {
                    public ExtTableHeaderRenderer getHeaderRenderer(final JTableHeader jTableHeader) {
                        final ExtTableHeaderRenderer ret = new ExtTableHeaderRenderer(this, jTableHeader) {
                            private final Icon ok = NewTheme.I().getIcon(IconKey.ICON_OK, 14);

                            @Override
                            public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
                                super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                                setIcon(ok);
                                setHorizontalAlignment(CENTER);
                                setText(null);
                                return this;
                            }
                        };
                        return ret;
                    }

                    @Override
                    public int getMaxWidth() {
                        return 30;
                    }

                    @Override
                    protected boolean getBooleanValue(final MultiHostHost mhost) {
                        return mhost.isEnabled();
                    }

                    @Override
                    public boolean isEditable(MultiHostHost mhost) {
                        return true;
                    }

                    @Override
                    protected void setBooleanValue(final boolean enabled, final MultiHostHost mhost) {
                        mhost.setEnabled(enabled);
                        fireTableStructureChanged();
                    }
                });
                addColumn(new ExtTextColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_domain()) {
                    @Override
                    public String getStringValue(final MultiHostHost mhost) {
                        return mhost.getDomain();
                    }

                    @Override
                    public Icon getIcon(MultiHostHost mhost) {
                        final DomainInfo di = mhost.getDomainInfo();
                        if (di != null) {
                            return di.getFavIcon(false);
                        } else {
                            return icon_filler;
                        }
                    }

                    @Override
                    public boolean onDoubleClick(MouseEvent e, MultiHostHost mhost) {
                        if (mhost.getStatus() == MultihosterHostStatus.DEACTIVATED_JDOWNLOADER_UNSUPPORTED) {
                            /* Host is not supported by JD -> It doesn't make sense to even try to open an affiliate link. */
                            return false;
                        }
                        final LazyHostPlugin lazyHostPlugin = HostPluginController.getInstance().get(mhost.getDomain());
                        if (lazyHostPlugin != null && lazyHostPlugin.isPremium()) {
                            AccountController.openAfflink(lazyHostPlugin, null, "MultiHostSupportedHostsDetailTable");
                        }
                        return false;
                    }

                    @Override
                    protected String getTooltipText(final MultiHostHost mhost) {
                        /* Display comma separated list of all known domains of this host as tooltip. */
                        final StringBuilder sb = new StringBuilder();
                        for (final String domain : mhost.getDomains()) {
                            if (sb.length() > 0) {
                                sb.append(", ");
                            }
                            sb.append(domain);
                        }
                        return sb.toString();
                    }

                    @Override
                    public boolean isEnabled(final MultiHostHost mhost) {
                        return mhost.isEnabled();
                    }
                });
                addColumn(new ExtTextColumn<MultiHostHost>("Status") {
                    @Override
                    public String getStringValue(final MultiHostHost mhost) {
                        final String text = mhost.getStatusText();
                        if (mhost.getUnavailableTimeMillis() > 0) {
                            return mhost.getUnavailableStatusText();
                        } else if (!mhost.isUnlimitedLinks() && mhost.getLinksLeft() <= 0) {
                            return _GUI.T.account_error_no_links_left();
                        } else if (!mhost.isUnlimitedTraffic() && mhost.getTrafficLeft() <= 0) {
                            return _GUI.T.account_error_no_traffic_left();
                        } else if (text != null) {
                            return text;
                        } else {
                            return mhost.getStatus().getLabel();
                        }
                    }

                    @Override
                    protected String getTooltipText(final MultiHostHost mhost) {
                        if (mhost.getUnavailableTimeMillis() > 0) {
                            // TODO: Add translation
                            return "Host temporarily unavailable because of too many wrong download attempts";
                        } else {
                            return mhost.getStatus().getLabel();
                        }
                    }

                    private final Color defaultColor;
                    {
                        renderer.setLayout(new MigLayout("ins 0", "[grow,fill][]", "[grow,fill]"));
                        defaultColor = rendererField.getForeground();
                    }

                    @Override
                    public void configureRendererComponent(MultiHostHost value, boolean isSelected, boolean hasFocus, int row, int column) {
                        super.configureRendererComponent(value, isSelected, hasFocus, row, column);
                        final MultihosterHostStatus status = value.getStatus();
                        if (status != MultihosterHostStatus.WORKING && status != MultihosterHostStatus.WORKING_UNSTABLE) {
                            rendererField.setForeground(Color.RED);
                        } else {
                            rendererField.setForeground(defaultColor);
                        }
                    }

                    @Override
                    public Icon getIcon(MultiHostHost mhost) {
                        final MultihosterHostStatus status = mhost.getStatus();
                        if (mhost.getUnavailableTimeMillis() > 0) {
                            return icon_wait;
                        } else if (!mhost.isUnlimitedLinks() && mhost.getLinksLeft() <= 0) {
                            return icon_wait;
                        } else if (!mhost.isUnlimitedTraffic() && mhost.getTrafficLeft() <= 0) {
                            return icon_wait;
                        } else if (status == MultihosterHostStatus.WORKING) {
                            return icon_okay;
                        } else if (status == MultihosterHostStatus.WORKING_UNSTABLE) {
                            return icon_warning;
                        } else {
                            return icon_error;
                        }
                    }

                    @Override
                    public boolean isEnabled(final MultiHostHost mhost) {
                        return mhost.isEnabled();
                    }
                });
                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && shouldShowUInternalStatusColumn_final) {
                    /* This column exists for debugging purposes only! */
                    addColumn(new ExtTextColumn<MultiHostHost>("Internal status") {
                        @Override
                        public String getStringValue(final MultiHostHost mhost) {
                            return mhost.getStatus().getLabel();
                        }

                        private final Color defaultColor;
                        {
                            renderer.setLayout(new MigLayout("ins 0", "[grow,fill][]", "[grow,fill]"));
                            defaultColor = rendererField.getForeground();
                        }

                        @Override
                        public void configureRendererComponent(MultiHostHost value, boolean isSelected, boolean hasFocus, int row, int column) {
                            super.configureRendererComponent(value, isSelected, hasFocus, row, column);
                            final MultihosterHostStatus status = value.getStatus();
                            if (status != MultihosterHostStatus.WORKING && status != MultihosterHostStatus.WORKING_UNSTABLE) {
                                rendererField.setForeground(Color.RED);
                            } else {
                                rendererField.setForeground(defaultColor);
                            }
                        }

                        @Override
                        public boolean isEnabled(final MultiHostHost mhost) {
                            return mhost.isEnabled();
                        }
                    });
                }
                if (shouldShowUnavailableForColumn_final) {
                    addColumn(new ExtLongColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_unavailable_for()) {
                        @Override
                        protected long getLong(MultiHostHost mhost) {
                            return mhost.getUnavailableTimeMillis();
                        }

                        private final AtomicReference<Timer> timerReference = new AtomicReference<Timer>();

                        @Override
                        public boolean isVisible(boolean savedValue) {
                            final boolean ret = super.isVisible(savedValue);
                            final ExtLongColumn<MultiHostHost> thisColumn = this;
                            if (ret && timerReference.get() == null) {
                                final Timer countdownTimer = new Timer(1000, new ActionListener() {
                                    @Override
                                    public void actionPerformed(ActionEvent e) {
                                        final ExtTable<MultiHostHost> table = getTable();
                                        if (table == null) {
                                            // no table set yet
                                            return;
                                        }
                                        if (!table.isShowing() || !getModel().isColumnVisible(thisColumn) || timerReference.get() != e.getSource()) {
                                            ((Timer) e.getSource()).stop();
                                            return;
                                        }
                                        table.getModel().fireTableDataChanged();
                                    }
                                }) {
                                    private static final long serialVersionUID = -8818019184160268747L;

                                    @Override
                                    public void stop() {
                                        super.stop();
                                        timerReference.compareAndSet(this, null);
                                    };
                                };
                                countdownTimer.start();
                                timerReference.set(countdownTimer);
                            }
                            return ret;
                        }

                        @Override
                        protected String getLongFormatted(MultiHostHost mhost) {
                            final long unavailableFor = getLong(mhost);
                            if (unavailableFor > 0) {
                                return TimeFormatter.formatMilliSeconds(unavailableFor, 0);
                            } else {
                                return "---";
                            }
                        }

                        @Override
                        public boolean isEnabled(final MultiHostHost mhost) {
                            return mhost.isEnabled();
                        }
                    });
                }
                if (shouldShowLinkLimitColumns_final) {
                    addColumn(new ExtProgressColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_links_left_max()) {
                        @Override
                        public int getMinWidth() {
                            return 50;
                        }

                        @Override
                        protected boolean isIndeterminated(final MultiHostHost value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                            return false;
                        }

                        @Override
                        protected String getString(MultiHostHost mhost, long current, long total) {
                            if (mhost.isUnlimitedLinks()) {
                                return _GUI.T.lit_unlimited();
                            } else {
                                return mhost.getLinksLeft() + "/" + mhost.getLinksMax();
                            }
                        }

                        @Override
                        protected long getMax(MultiHostHost mhost) {
                            if (mhost.isUnlimitedLinks()) {
                                return Long.MAX_VALUE;
                            } else {
                                return mhost.getLinksMax();
                            }
                        }

                        @Override
                        protected long getValue(MultiHostHost mhost) {
                            if (mhost.isUnlimitedLinks()) {
                                return Long.MAX_VALUE;
                            } else {
                                return mhost.getLinksLeft();
                            }
                        }

                        @Override
                        public boolean isEnabled(final MultiHostHost mhost) {
                            return mhost.isEnabled();
                        }
                    });
                    addColumn(new ExtLongColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_links_left()) {
                        @Override
                        protected long getLong(MultiHostHost mhost) {
                            return mhost.getLinksLeft();
                        }

                        @Override
                        protected String getLongFormatted(MultiHostHost mhost) {
                            if (mhost.isUnlimitedLinks()) {
                                return _GUI.T.lit_unlimited();
                            } else {
                                return Long.toString(getLong(mhost));
                            }
                        }

                        @Override
                        public boolean isDefaultVisible() {
                            return false;
                        }

                        @Override
                        public boolean isEnabled(final MultiHostHost mhost) {
                            return mhost.isEnabled();
                        }
                    });
                    addColumn(new ExtLongColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_links_max()) {
                        @Override
                        protected long getLong(MultiHostHost mhost) {
                            return mhost.getLinksMax();
                        }

                        @Override
                        protected String getLongFormatted(MultiHostHost mhost) {
                            if (mhost.isUnlimitedLinks()) {
                                return _GUI.T.lit_unlimited();
                            } else {
                                return Long.toString(getLong(mhost));
                            }
                        }

                        @Override
                        public boolean isDefaultVisible() {
                            return false;
                        }

                        @Override
                        public boolean isEnabled(final MultiHostHost mhost) {
                            return mhost.isEnabled();
                        }
                    });
                }
                if (shouldShowTrafficLimitColumns_final) {
                    addColumn(new ExtProgressColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_traffic_left_max()) {
                        {
                            setRowSorter(new ExtDefaultRowSorter<MultiHostHost>() {
                                @Override
                                public int compare(final MultiHostHost o1, final MultiHostHost o2) {
                                    final long v1 = getValue(o1);
                                    final long v2 = getValue(o2);
                                    if (v1 == v2) {
                                        return 0;
                                    }
                                    if (this.getSortOrderIdentifier() != ExtColumn.SORT_ASC) {
                                        return v1 > v2 ? -1 : 1;
                                    } else {
                                        return v2 > v1 ? -1 : 1;
                                    }
                                }
                            });
                        }
                        private final SIZEUNIT      maxSizeUnit = JsonConfig.create(GraphicalUserInterfaceSettings.class).getMaxSizeUnit();
                        private final DecimalFormat formatter   = new DecimalFormat();

                        @Override
                        public int getMinWidth() {
                            return 140;
                        }

                        @Override
                        protected boolean isIndeterminated(final MultiHostHost value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                            return false;
                        }

                        @Override
                        protected String getString(MultiHostHost mhost, long current, long total) {
                            if (mhost.isUnlimitedTraffic()) {
                                return _GUI.T.premiumaccounttablemodel_column_trafficleft_unlimited();
                            } else {
                                return getSizeString(mhost.getTrafficLeft()) + "/" + getSizeString(mhost.getTrafficMax());
                            }
                        }

                        private final String getSizeString(final long fileSize) {
                            return SIZEUNIT.formatValue(maxSizeUnit, formatter, fileSize);
                        }

                        @Override
                        protected long getMax(MultiHostHost mhost) {
                            if (mhost.isUnlimitedTraffic()) {
                                return Long.MAX_VALUE;
                            } else {
                                return mhost.getTrafficMax();
                            }
                        }

                        @Override
                        protected long getValue(MultiHostHost mhost) {
                            if (mhost.isUnlimitedTraffic()) {
                                return Long.MAX_VALUE;
                            } else {
                                return mhost.getTrafficLeft();
                            }
                        }

                        @Override
                        public boolean isEnabled(final MultiHostHost mhost) {
                            return mhost.isEnabled();
                        }
                    });
                    addColumn(new ExtFileSizeColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_traffic_left()) {
                        @Override
                        public String getStringValue(MultiHostHost mhost) {
                            if (mhost.isUnlimitedTraffic()) {
                                return _GUI.T.lit_unlimited();
                            } else {
                                return getSizeString(mhost.getTrafficLeft());
                            }
                        }

                        @Override
                        protected long getBytes(MultiHostHost val) {
                            return val.getTrafficLeft();
                        }

                        @Override
                        public boolean isDefaultVisible() {
                            return false;
                        }

                        @Override
                        public boolean isEnabled(final MultiHostHost mhost) {
                            return mhost.isEnabled();
                        }
                    });
                    addColumn(new ExtFileSizeColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_traffic_max()) {
                        @Override
                        public String getStringValue(MultiHostHost mhost) {
                            if (mhost.isUnlimitedTraffic()) {
                                return _GUI.T.lit_unlimited();
                            } else {
                                return getSizeString(mhost.getTrafficMax());
                            }
                        }

                        @Override
                        protected long getBytes(MultiHostHost val) {
                            return val.getTrafficMax();
                        }

                        @Override
                        public boolean isDefaultVisible() {
                            return false;
                        }

                        @Override
                        public boolean isEnabled(final MultiHostHost mhost) {
                            return mhost.isEnabled();
                        }
                    });
                }
                if (shouldShowTrafficCaculationColumn_final) {
                    addColumn(new ExtLongColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_traffic_calculation_factor_percent()) {
                        @Override
                        protected long getLong(MultiHostHost mhost) {
                            return mhost.getTrafficCalculationFactorPercent();
                        }

                        @Override
                        protected String getLongFormatted(MultiHostHost mhost) {
                            return getLong(mhost) + "%";
                        }

                        @Override
                        public boolean isEnabled(final MultiHostHost mhost) {
                            return mhost.isEnabled();
                        }
                    });
                }
            }
        };
        if (shouldShowLinkLimitColumns_final || shouldShowTrafficLimitColumns_final || shouldShowUnavailableForColumn_final) {
            /* Add highlighter if needed */
            tableModel.addExtComponentRowHighlighter(new ExtComponentRowHighlighter<MultiHostHost>(null, Color.YELLOW, null) {
                @Override
                public boolean accept(ExtColumn<MultiHostHost> column, MultiHostHost mhost, boolean selected, boolean focus, int row) {
                    if (!mhost.isUnlimitedLinks() && mhost.getLinksLeft() <= 0) {
                        return true;
                    } else if (!mhost.isUnlimitedTraffic() && mhost.getTrafficLeft() <= 0) {
                        return true;
                    } else if (mhost.getUnavailableTimeMillis() > 0) {
                        return true;
                    } else {
                        return false;
                    }
                }
            });
        }
        tableModel._fireTableStructureChanged(hosts, false);
        final BasicJDTable<MultiHostHost> table = new BasicJDTable<MultiHostHost>(tableModel);
        table.setPreferredScrollableViewportSize(new Dimension(table.getPreferredSize().width, table.getRowHeight() * table.getRowCount()));
        table.setSearchEnabled(true);
        table.addMouseWheelListener(new MouseWheelListener() {
            @Override
            public void mouseWheelMoved(MouseWheelEvent e) {
                /*
                 * Forward event to upper panel so that the scrolling happens in it and not in our table which is always full-size and has
                 * no vertical scrollbar.
                 */
                panel.dispatchEvent(e);
            }
        });
        final JScrollPane scrollPane = new JScrollPane(table);
        panel.add(scrollPane);
    }

    public void extendAccountSettingsPanel(final Account acc, final PluginConfigPanelNG panel) {
    }

    /**
     * Override this if API login is needed for this plugin. <br>
     * Return an URL which will lead the user to his API key and/or instructions on how to login via API in JDownloader. <br>
     * Example(s): <br>
     * pixeldrain.com: https://pixeldrain.com/user/connect_app?app=jdownloader <br>
     * cocoleech.com: https://members.cocoleech.com/settings
     */
    protected String getAPILoginHelpURL() {
        return null;
    }

    /** Override if API key login is used for this plugins' account functionality. */
    protected boolean looksLikeValidAPIKey(final String str) {
        return false;
    }

    /**
     * Use this to pre-validate login credentials. <br>
     * This method works locally/offline and shall not perform any http requests!
     *
     * @throws AccountInvalidException
     */
    public void validateLogins(final Account account) throws AccountInvalidException {
        if (this.hasFeature(FEATURE.USERNAME_IS_EMAIL)) {
            if (!looksLikeValidEmailAddress(account, account.getUser())) {
                throw new AccountInvalidException(_GUI.T.accountdialog_LoginValidationErrorInputIsNotEmailAddress());
            }
        }
        if (this.hasFeature(FEATURE.COOKIE_LOGIN_ONLY)) {
            /* Check for non-cookie value in password field when only cookies are allowed. */
            if (account.loadUserCookies() == null) {
                throw new AccountInvalidException(_GUI.T.accountdialog_LoginValidationErrorCookieLoginMandatoryButNoCookiesGiven());
            }
        } else if (!this.hasFeature(FEATURE.COOKIE_LOGIN_OPTIONAL)) {
            /* Check for cookies in password field when cookies are not allowed. */
            if (account.loadUserCookies() != null) {
                throw new AccountInvalidException(_GUI.T.accountdialog_LoginValidationErrorCookieLoginUnsupportedButGiven());
            }
        } else if (this.hasFeature(FEATURE.API_KEY_LOGIN)) {
            /* Check for invalid-looking API key in password field when only API key is allowed. */
            if (!this.looksLikeValidAPIKey(account.getPass())) {
                throw new AccountInvalidException(_GUI.T.accountdialog_LoginValidationErrorInvalidAPIKey());
            }
        }
    }

    public boolean looksLikeValidEmailAddress(final Account account, final String email) {
        // TODO: Move this function to a better place
        if (email == null) {
            return false;
        }
        if (email.matches("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")) {
            return true;
        } else {
            return false;
        }
    }

    public boolean isSameAccount(Account downloadAccount, AbstractProxySelectorImpl downloadProxySelector, Account candidateAccount, AbstractProxySelectorImpl candidateProxySelector) {
        return downloadProxySelector == candidateProxySelector && downloadAccount == candidateAccount;
    }

    protected URLConnectionAdapter basicLinkCheck(final Browser br, final Request request, final DownloadLink link, final String customFileName, final String defaultExtension) throws IOException, PluginException {
        return basicLinkCheck(br, request, link, customFileName, defaultExtension, FILENAME_SOURCE.values());
    }

    public static interface FilenameSourceInterface {
        public String getFilename(Plugin plugin, DownloadLink link, URLConnectionAdapter con, final String... customValues);

        public boolean setFilename(Plugin plugin, DownloadLink link, final String filename);

        public boolean setFilename(DownloadLinkDownloadable downloadable, final String filename);

    }

    /**
     * do not change order as it serves as default order for basicLinkCheck method
     *
     * @author daniel
     *
     */
    public static enum FILENAME_SOURCE implements FilenameSourceInterface {
        FORCED() {
            @Override
            public String getFilename(Plugin plugin, DownloadLink link, URLConnectionAdapter con, final String... customValues) {
                if (link == null) {
                    return null;
                }
                String ret = CrawledLink.getForcedName(link);
                if (ret != null && plugin != null) {
                    ret = plugin.correctOrApplyFileNameExtension(this, link, ret, con, customValues);
                }
                return ret;
            }

            @Override
            public boolean setFilename(DownloadLinkDownloadable downloadable, String filename) {
                if (filename == null) {
                    return false;
                }
                downloadable.setForcedFileName(filename);
                downloadable.setFinalFileName(filename);
                return true;
            }

            @Override
            public boolean setFilename(Plugin plugin, DownloadLink link, String fileName) {
                if (fileName == null) {
                    return false;
                } else if (plugin != null) {
                    return plugin.setFilename(this, link, fileName);
                } else {
                    CrawledLink.setForcedName(link, fileName);
                    link.setFinalFileName(fileName);
                    return true;
                }
            }
        },
        CUSTOM() {
            @Override
            public String getFilename(Plugin plugin, DownloadLink link, URLConnectionAdapter con, final String... customValues) {
                String ret = getCustomFilename(customValues);
                if (ret != null && plugin != null) {
                    ret = plugin.correctOrApplyFileNameExtension(this, link, ret, con, customValues);
                }
                return ret;
            }
        },
        CONNECTION() {
            @Override
            public String getFilename(Plugin plugin, DownloadLink link, URLConnectionAdapter con, final String... customValues) {
                if (con == null) {
                    return null;
                }
                String ret = HEADER.getFilename(plugin, link, con, customValues);
                if (ret == null) {
                    ret = URL.getFilename(plugin, link, con, customValues);
                }
                if (ret != null && plugin != null) {
                    ret = plugin.correctOrApplyFileNameExtension(this, link, ret, con, customValues);
                }
                return ret;
            }
        },
        FINAL() {
            @Override
            public String getFilename(Plugin plugin, DownloadLink link, URLConnectionAdapter con, final String... customValues) {
                if (link == null) {
                    return null;
                }
                String ret = link.getFinalFileName();
                if (ret != null && plugin != null) {
                    ret = plugin.correctOrApplyFileNameExtension(this, link, ret, con, customValues);
                }
                return ret;
            }
        },
        HEADER() {
            @Override
            public String getFilename(Plugin plugin, DownloadLink link, URLConnectionAdapter con, final String... customValues) {
                if (con == null) {
                    return null;
                }
                final DispositionHeader header;
                if (plugin != null) {
                    header = plugin.getDispositionHeader(con);
                } else {
                    header = Plugin.parseDispositionHeader(con);
                }
                if (header == null) {
                    return null;
                }
                String ret = header.getFilename();
                if (ret != null && plugin != null) {
                    ret = plugin.correctOrApplyFileNameExtension(this, link, ret, con, customValues);
                }
                return ret;
            }
        },
        URL() {
            @Override
            public String getFilename(Plugin plugin, DownloadLink link, URLConnectionAdapter con, final String... customValues) {
                if (con == null) {
                    return null;
                }
                String ret = getFileNameFromURL(con.getURL());
                if (ret != null && plugin != null) {
                    ret = plugin.correctOrApplyFileNameExtension(this, link, ret, con, customValues);
                }
                return ret;
            }
        },
        PLUGIN() {
            @Override
            public String getFilename(Plugin plugin, DownloadLink link, URLConnectionAdapter con, final String... customValues) {
                if (link == null) {
                    return null;
                }
                String ret = link.getNameSetbyPlugin();
                if (ret != null && plugin != null) {
                    ret = plugin.correctOrApplyFileNameExtension(this, link, ret, con, customValues);
                }
                return ret;
            }
        };

        public static String getCustomFilename(final String... customValues) {
            if (customValues != null && customValues.length > 0) {
                return customValues[0];
            }
            return null;
        };

        public static String getCustomExtension(final String... customValues) {
            if (customValues != null && customValues.length > 1) {
                return customValues[1];
            }
            return null;
        };

        public static FILENAME_SOURCE[] prefer(FILENAME_SOURCE[] list, FILENAME_SOURCE... preferSource) {
            final LinkedHashSet<FILENAME_SOURCE> ret = new LinkedHashSet<PluginForHost.FILENAME_SOURCE>();
            if (preferSource != null) {
                ret.addAll(Arrays.asList(preferSource));
            }
            if (list != null) {
                ret.addAll(Arrays.asList(list));
            }
            return ret.toArray(new FILENAME_SOURCE[0]);
        }

        @Override
        public boolean setFilename(DownloadLinkDownloadable downloadable, String filename) {
            if (filename == null) {
                return false;
            } else {
                downloadable.setFinalFileName(filename);
                return true;
            }
        }

        @Override
        public boolean setFilename(Plugin plugin, DownloadLink link, String fileName) {
            if (fileName == null) {
                return false;
            } else if (plugin != null) {
                return plugin.setFilename(this, link, fileName);
            } else {
                link.setFinalFileName(fileName);
                return true;
            }
        }
    }

    protected URLConnectionAdapter basicLinkCheck(final Browser br, final Request request, final DownloadLink link, final String customFileName, final String defaultExtension, final FILENAME_SOURCE... fileNameSource) throws IOException, PluginException {
        URLConnectionAdapter con = null;
        try {
            br.setFollowRedirects(true);
            request.getHeaders().put(HTTPConstants.HEADER_REQUEST_ACCEPT_ENCODING, "identity");
            con = br.openRequestConnection(request);
            handleConnectionErrors(br, con);
            if (con.getCompleteContentLength() > 0) {
                if (con.isContentDecoded()) {
                    link.setVerifiedFileSize(-1);
                    link.setDownloadSize(con.getCompleteContentLength());
                } else {
                    link.setVerifiedFileSize(con.getCompleteContentLength());
                }
            }
            if (fileNameSource != null && fileNameSource.length > 0) {
                for (FILENAME_SOURCE source : fileNameSource) {
                    final String filename = getFileNameFromSource(source, link, con, customFileName, defaultExtension);
                    if (source.setFilename(this, link, filename)) {
                        break;
                    }
                }
            }
            if (RequestMethod.HEAD.equals(request.getRequestMethod())) {
                br.followConnection();
            }
            return con;
        } finally {
            try {
                if (con != null) {
                    con.disconnect();
                }
            } catch (final Throwable e) {
            }
        }
    }

    /** Checks connection for "downloadable file content" and throws exception if there is no file content. */
    protected void handleConnectionErrors(final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        if (!this.looksLikeDownloadableContent(con)) {
            br.followConnection(true);
            throwConnectionExceptions(br, con);
            throwFinalConnectionException(br, con);
        }
    }

    protected void throwConnectionExceptions(final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        switch (con.getResponseCode()) {
        case 403:
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403", 60 * 60 * 1000l);
        case 404:
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 404", 60 * 60 * 1000l);
        case 416:
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 416", 60 * 60 * 1000l);
        case 429:
            throw new PluginException(LinkStatus.ERROR_IP_BLOCKED, "429 Too Many Requests", 1 * 60 * 1000l);
        case 503:
            throw new PluginException(LinkStatus.ERROR_HOSTER_TEMPORARILY_UNAVAILABLE, "Server error 503 connection limit reached", 5 * 60 * 1000l);
        }
    }

    protected void throwFinalConnectionException(final Browser br, final URLConnectionAdapter con) throws PluginException, IOException {
        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Content broken?");
    }

    protected Thread showCookieLoginInfo() {
        final String host = this.getHost();
        final Thread thread = new Thread() {
            public void run() {
                try {
                    final String help_article_url = "https://support.jdownloader.org/knowledgebase/article/account-cookie-login-instructions";
                    String message = "";
                    final String title;
                    if ("de".equalsIgnoreCase(System.getProperty("user.language"))) {
                        title = host + " - Login";
                        message += "Hallo liebe(r) " + host + " NutzerIn\r\n";
                        message += "Um deinen " + host + " Account in JDownloader verwenden zu können, musst du folgende Schritte beachten:\r\n";
                        message += "Folge der Anleitung im Hilfe-Artikel:\r\n";
                        message += help_article_url;
                    } else {
                        title = host + " - Login";
                        message += "Hello dear " + host + " user\r\n";
                        message += "In order to use an account of this service in JDownloader, please follow these instructions:\r\n";
                        message += help_article_url;
                    }
                    final ConfirmDialog dialog = new ConfirmDialog(UIOManager.LOGIC_COUNTDOWN, title, message);
                    dialog.setTimeout(3 * 60 * 1000);
                    if (CrossSystem.isOpenBrowserSupported() && !Application.isHeadless()) {
                        CrossSystem.openURL(help_article_url);
                    }
                    final ConfirmDialogInterface ret = UIOManager.I().show(ConfirmDialogInterface.class, dialog);
                    ret.throwCloseExceptions();
                } catch (final Throwable e) {
                    getLogger().log(e);
                }
            };
        };
        thread.setDaemon(true);
        thread.start();
        return thread;
    }
}