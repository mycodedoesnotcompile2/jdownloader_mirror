package org.jdownloader.captcha.v2.solver.browser;

import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.handler.BooleanKeyHandler;
import org.appwork.storage.config.handler.IntegerKeyHandler;
import org.appwork.storage.config.handler.ObjectKeyHandler;
import org.appwork.storage.config.handler.StorageHandler;
import org.appwork.storage.config.handler.StringListHandler;

public class CFG_BROWSER_CAPTCHA_SOLVER {
    // public static void main(String[] args) {
    // ConfigUtils.printStaticMappings(BrowserCaptchaSolverConfig.class);
    // }
    // Static Mappings for interface org.jdownloader.captcha.v2.solver.browser.BrowserCaptchaSolverConfigV3
    public static final BrowserCaptchaSolverConfigV3                 CFG                       = JsonConfig.create(BrowserCaptchaSolverConfigV3.class);
    public static final StorageHandler<BrowserCaptchaSolverConfigV3> SH                        = (StorageHandler<BrowserCaptchaSolverConfigV3>) CFG._getStorageHandler();
    // let's do this mapping here. If we map all methods to static handlers, access is faster, and we get an error on init if mappings are
    // wrong.
    public static final IntegerKeyHandler                           LOCAL_HTTP_PORT           = SH.getKeyHandler("LocalHttpPort", IntegerKeyHandler.class);
    public static final BooleanKeyHandler                           AUTO_OPEN_BROWSER_ENABLED = SH.getKeyHandler("AutoOpenBrowserEnabled", BooleanKeyHandler.class);
    /**
     * Example: [ "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe", "-app=%s" ]
     **/
    public static final StringListHandler                           BROWSER_COMMANDLINE       = SH.getKeyHandler("BrowserCommandline", StringListHandler.class);
    public static final BooleanKeyHandler                           ENABLED                   = SH.getKeyHandler("Enabled", BooleanKeyHandler.class);
    public static final BooleanKeyHandler                           AUTO_CLICK_ENABLED        = SH.getKeyHandler("AutoClickEnabled", BooleanKeyHandler.class);
}