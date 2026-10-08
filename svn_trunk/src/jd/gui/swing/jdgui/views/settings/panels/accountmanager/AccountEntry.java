package jd.gui.swing.jdgui.views.settings.panels.accountmanager;

import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.views.settings.ConfigurationView;
import jd.gui.swing.jdgui.views.settings.panels.anticaptcha.CaptchaConfigPanel;
import jd.gui.swing.jdgui.views.settings.panels.pluginsettings.PluginSettings;
import jd.plugins.Account;
import jd.plugins.PluginForHost;

import org.appwork.storage.config.JsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;

public class AccountEntry {
    private final Account account;

    public Account getAccount() {
        return account;
    }

    public AccountEntry(Account acc) {
        this.account = acc;
    }

    /**
     * True if the account's plugin has settings which are shown in the plugin settings panel. Same condition the plugin settings panel
     * uses to list a plugin (see PluginSettingsPanel#fillModel).
     */
    public boolean hasConfiguration() {
        final PluginForHost plugin = account != null ? account.getPlugin() : null;
        if (plugin == null) {
            return false;
        }
        final LazyHostPlugin lazy = plugin.getLazyP();
        /* Captcha solver plugins have their settings in the captcha settings panel (see showConfiguration). */
        return lazy.hasFeature(FEATURE.CAPTCHA_SOLVER) || lazy.isHasConfig() || (lazy.isPremium() && (lazy.isHasPremiumConfig() || lazy.hasFeature(FEATURE.MULTIHOST)));
    }

    /**
     * Opens the settings of the account's plugin. For captcha solver plugins these are the captcha settings, with the matching entry of the
     * solver table preselected; for all other plugins the plugin settings, scrolled to the account.
     */
    public void showConfiguration() {
        final PluginForHost solverPlugin = account != null ? account.getPlugin() : null;
        if (solverPlugin != null && solverPlugin.hasFeature(FEATURE.CAPTCHA_SOLVER)) {
            CaptchaConfigPanel.showSolver(solverPlugin.getHost());
            return;
        }
        JsonConfig.create(GraphicalUserInterfaceSettings.class).setConfigViewVisible(true);
        JDGui.getInstance().setContent(ConfigurationView.getInstance(), true);
        ConfigurationView.getInstance().setSelectedSubPanel(PluginSettings.class);
        final PluginSettings pluginSettings = ConfigurationView.getInstance().getSubPanel(PluginSettings.class);
        if (pluginSettings != null && account != null) {
            final PluginForHost plugin = account.getPlugin();
            if (plugin != null) {
                pluginSettings.setPlugin(plugin.getLazyP());
                pluginSettings.scrollToAccount(account);
            }
        }
    }
}
