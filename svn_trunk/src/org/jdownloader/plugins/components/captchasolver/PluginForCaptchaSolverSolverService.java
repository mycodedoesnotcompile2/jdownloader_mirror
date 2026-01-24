package org.jdownloader.plugins.components.captchasolver;

import java.util.List;
import java.util.Map;

import javax.swing.Icon;

import org.appwork.storage.config.JsonConfig;
import org.jdownloader.DomainInfo;
import org.jdownloader.captcha.v2.ChallengeSolverConfig;
import org.jdownloader.captcha.v2.solver.service.AbstractSolverService;
import org.jdownloader.captcha.v2.solver.twocaptcha.TwoCaptchaConfigInterface;

import jd.gui.swing.jdgui.components.premiumbar.ServiceCollection;
import jd.gui.swing.jdgui.components.premiumbar.ServicePanelExtender;
import jd.gui.swing.jdgui.views.settings.panels.anticaptcha.AbstractCaptchaSolverConfigPanel;

public class PluginForCaptchaSolverSolverService extends AbstractSolverService implements ServicePanelExtender {
    protected final abstractPluginForCaptchaSolver plugin;

    public PluginForCaptchaSolverSolverService(final abstractPluginForCaptchaSolver plugin) {
        if (plugin == null) {
            throw new IllegalArgumentException();
        }
        this.plugin = plugin;
    }

    @Override
    public String getType() {
        return this.plugin.getHost();
    }

    @Override
    public String getName() {
        return this.plugin.getHost();
    }

    @Override
    public String getID() {
        return this.plugin.getHost();
    }

    @Override
    public Icon getIcon(int size) {
        return DomainInfo.getInstance(plugin.getHost());
    }

    @Override
    public boolean hasConfigPanel() {
        return false;
    }

    @Override
    public ChallengeSolverConfig getConfig() {
        // return null;
        // TODO: Remove this dummy config
        return JsonConfig.create(TwoCaptchaConfigInterface.class);
    }

    @Override
    public AbstractCaptchaSolverConfigPanel getConfigPanel() {
        return null;
    }

    @Override
    public void extendServicePabel(List<ServiceCollection<?>> services) {
    }

    @Override
    public Map<String, Integer> getWaitForOthersDefaultMap() {
        return null;
    }
}
