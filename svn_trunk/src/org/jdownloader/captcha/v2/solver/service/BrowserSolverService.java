package org.jdownloader.captcha.v2.solver.service;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.Icon;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.Application;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.jdownloader.captcha.v2.ChallengeSolver.SolverType;
import org.jdownloader.captcha.v2.solver.browser.BrowserCaptchaSolverConfigV3;
import org.jdownloader.controlling.browser.ExternalBrowserManager;
import org.jdownloader.controlling.browser.ExternalBrowserManager.InstalledBrowser;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;

import jd.gui.swing.jdgui.views.settings.panels.anticaptcha.SelectBrowserDialog;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public class BrowserSolverService extends AbstractSolverService {
    public static final String                  ID       = "browser";
    private static final BrowserSolverService   INSTANCE = new BrowserSolverService();
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
    public SolverType getType() {
        return SolverType.JD_LOCAL_BROWSER;
    }

    @Override
    public Icon getIcon(int size) {
        return NewTheme.I().getIcon(IconKey.ICON_BROWSE, size);
    }

    @Override
    public String getName() {
        return "Dialog in Browser (Chrome, Firefox, Edge)";
    }

    @Override
    public String getDescription() {
        return "Manual local captcha solving in your own browser. Requires our MyJDownloader browser extension. Does NOT require a MyJDownloader account!";
    }

    /**
     * Not ready if the system cannot open URLs (no OS default browser and no usable browser command line, see BrowserSolver#enqueue) or if
     * the configured browser executable does not exist.
     */
    @Override
    public boolean isReady() {
        if (Application.isHeadless()) {
            /* Guard Clause for headless installations though headless shouldn't even try to use this service anyways. */
            return false;
        }
        return isOpenBrowserSupported() && getMissingBrowserPath() == null;
    }

    /**
     * Returns the executable path of the configured browser if it is an explicit path (contains a directory part) that does not exist, else
     * null. A plain command name like "firefox" is resolved via PATH and therefore never reported.
     */
    private String getMissingBrowserPath() {
        final String[] commandline = getConfig().getBrowserCommandline();
        if (commandline == null) {
            return null;
        }
        for (final String arg : commandline) {
            if (arg != null && arg.trim().length() > 0) {
                final String path = arg.trim();
                if (path.indexOf('/') < 0 && path.indexOf('\\') < 0) {
                    return null;
                }
                /* Avoid file system access on every call: the result is cached per path. */
                final CheckedPath cached = checkedPath.get();
                if (cached != null && cached.path.equals(path)) {
                    return cached.missing ? path : null;
                }
                final boolean missing = !new File(path).exists();
                checkedPath.set(new CheckedPath(path, missing));
                return missing ? path : null;
            }
        }
        return null;
    }

    /**
     * Forgets the cached existence check of the configured browser path, so it is checked again on next access. Call this after a new
     * browser path has been set.
     */
    public void resetBrowserPathCache() {
        checkedPath.set(null);
    }

    private static final class CheckedPath {
        private final String  path;
        private final boolean missing;

        private CheckedPath(final String path, final boolean missing) {
            this.path = path;
            this.missing = missing;
        }
    }

    private final AtomicReference<CheckedPath> checkedPath = new AtomicReference<CheckedPath>();

    /**
     * Returns true if the user configured a custom browser (explicit path) which does not exist. In that case the solver must not work and
     * must not silently fall back to the OS default browser.
     */
    public boolean isCustomBrowserMissing() {
        return getMissingBrowserPath() != null;
    }

    /** The "Not ready" status text is shown as a warning, see {@link #getStatusText()}. */
    @Override
    public boolean isStatusTextWarning() {
        return !isReady();
    }

    @Override
    public String getStatusText() {
        if (isCustomBrowserMissing()) {
            /* Not ready -> an action button (see getStatusActionName()) is shown instead. */
            return null;
        }
        if (!isOpenBrowserSupported()) {
            return _GUI.T.BrowserSolverService_status_notReady_openUrlsUnsupported();
        }
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
    public String getStatusActionName() {
        if (isCustomBrowserMissing()) {
            return _GUI.T.BrowserSolverService_status_customBrowserNotFound();
        }
        return null;
    }

    @Override
    public boolean isStatusActionWarning() {
        return true;
    }

    @Override
    public void onStatusAction() {
        showSelectBrowserDialog();
    }

    /**
     * Lets the user choose the browser of this solver and stores the choice.
     *
     * @return true if the user selected a browser (and the config was changed), false if the dialog was canceled.
     */
    public boolean showSelectBrowserDialog() {
        final SelectBrowserDialog dialog = new SelectBrowserDialog(getConfig().getBrowserCommandline());
        try {
            final InstalledBrowser selected = Dialog.getInstance().showDialog(dialog);
            if (selected == null) {
                return false;
            }
            if (selected.getPath() == null) {
                /* "OS Default" */
                getConfig().setBrowserCommandline(null);
            } else {
                getConfig().setBrowserCommandline(new String[] { selected.getPath(), "%s" });
            }
            /* Check the (possibly new) browser path again on next access. */
            resetBrowserPathCache();
            return true;
        } catch (DialogClosedException e) {
            return false;
        } catch (DialogCanceledException e) {
            return false;
        }
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
