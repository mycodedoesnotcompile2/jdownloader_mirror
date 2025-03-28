package org.jdownloader.plugins.components.captchasolver;

import java.util.List;
import java.util.Map;

import javax.swing.Icon;

import org.jdownloader.captcha.v2.ChallengeSolverConfig;
import org.jdownloader.captcha.v2.solver.service.AbstractSolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;

import jd.gui.swing.jdgui.components.premiumbar.ServiceCollection;
import jd.gui.swing.jdgui.components.premiumbar.ServicePanelExtender;
import jd.gui.swing.jdgui.views.settings.panels.anticaptcha.AbstractCaptchaSolverConfigPanel;

public class PluginForCaptchaSolverSolverService extends AbstractSolverService implements ServicePanelExtender {
    public PluginForCaptchaSolverSolverService() {
    }

    @Override
    public String getType() {
        return _GUI.T.CaptchaSolver_Type_paid_online();
    }

    @Override
    public Icon getIcon(int size) {
        return NewTheme.I().getIcon(IconKey.ICON_LOGO_DBC, size);
    }

    @Override
    public boolean hasConfigPanel() {
        return false;
    }

    @Override
    public String getName() {
        return "TODO_DUMMY";
    }

    @Override
    public ChallengeSolverConfig getConfig() {
        return null;
    }

    @Override
    public String getID() {
        return "TODO";
    }

    @Override
    public AbstractCaptchaSolverConfigPanel getConfigPanel() {
        return null;
    }

    @Override
    public void extendServicePabel(List<ServiceCollection<?>> services) {
    }
    // @Override
    // public Map<String, Integer> getWaitForOthersDefaultMap() {
    // HashMap<String, Integer> ret = new HashMap<String, Integer>();
    // ret.put(JacSolverService.ID, 30000);
    // ret.put(NineKwSolverService.ID, 120000);
    // ret.put(ImageTyperzSolverService.ID, 60000);
    // // ret.put(CheapCaptchaSolverService.ID, 60000);
    // // ret.put(EndCaptchaSolverService.ID, 60000);
    // // ret.put(TwoCaptchaSolverService.ID, 60000);
    // ret.put(TwoCaptchaSolverService.ID, 60000);
    // return ret;
    // }

    @Override
    public Map<String, Integer> getWaitForOthersDefaultMap() {
        return null;
    }
}
