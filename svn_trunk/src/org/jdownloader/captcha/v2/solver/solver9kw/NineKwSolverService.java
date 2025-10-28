package org.jdownloader.captcha.v2.solver.solver9kw;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.Icon;

import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.swing.components.tooltips.ExtTooltip;
import org.appwork.utils.Application;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.extmanager.LoggerFactory;
import org.jdownloader.captcha.v2.solver.jac.JacSolverService;
import org.jdownloader.captcha.v2.solver.service.AbstractSolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.settings.advanced.AdvancedConfigManager;
import org.jdownloader.settings.staticreferences.CFG_9KWCAPTCHA;

import jd.SecondLevelLaunch;
import jd.gui.swing.jdgui.components.premiumbar.ServiceCollection;
import jd.gui.swing.jdgui.components.premiumbar.ServicePanel;
import jd.gui.swing.jdgui.components.premiumbar.ServicePanelExtender;
import jd.gui.swing.jdgui.views.settings.panels.anticaptcha.AbstractCaptchaSolverConfigPanel;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;

public class NineKwSolverService extends AbstractSolverService implements ServicePanelExtender {
    private static final NineKwSolverService INSTANCE = new NineKwSolverService();
    public static final String               ID       = "9kw";

    /**
     * get the only existing instance of NineKwSolverService. This is a singleton
     *
     * @return
     */
    public static NineKwSolverService getInstance() {
        return NineKwSolverService.INSTANCE;
    }

    @Override
    public void extendServicePabel(List<ServiceCollection<?>> services) {
        if (StringUtils.isNotEmpty(config.getApiKey()) && isEnabled()) {
            services.add(new ServiceCollection<Captcha9kwSolver>() {
                /**
                 *
                 */
                private static final long serialVersionUID = 5569965026755271172L;

                @Override
                public Icon getIcon() {
                    return new AbstractIcon("logo/9kw", 18);
                }

                @Override
                public boolean isEnabled() {
                    // text or mouse or puzzle
                    return config.isEnabledGlobally() && (config.isEnabled() || config.ismouse() || config.ispuzzle());
                }

                @Override
                protected long getLastActiveTimestamp() {
                    return System.currentTimeMillis();
                }

                @Override
                protected String getName() {
                    return "9kw.eu";
                }

                @Override
                public ExtTooltip createTooltip(ServicePanel owner) {
                    return new ServicePanel9kwTooltip(owner, NineKwSolverService.this);
                }
            });
        }
    }

    public String getAPIROOT() {
        if (config.ishttps()) {
            return "https://www.9kw.eu/";
        } else {
            return "http://www.9kw.eu/";
        }
    }

    public NineKWAccount loadAccount() throws IOException {
        Browser br = new Browser();
        NineKWAccount ret = new NineKWAccount();
        String credits;
        ret.setRequests(textSolver.counter.get() + clickSolver.counter.get());
        ret.setSkipped(textSolver.counterInterrupted.get() + clickSolver.counterInterrupted.get());
        ret.setSolved(textSolver.counterSolved.get() + clickSolver.counterSolved.get());
        ret.setSend(textSolver.counterSend.get() + clickSolver.counterSend.get());
        ret.setSendError(textSolver.counterSendError.get() + clickSolver.counterSendError.get());
        ret.setOK(textSolver.counterOK.get() + clickSolver.counterOK.get());
        ret.setNotOK(textSolver.counterNotOK.get() + clickSolver.counterNotOK.get());
        ret.setUnused(textSolver.counterUnused.get() + clickSolver.counterUnused.get());
        try {
            String servercheck = br.getPage(getAPIROOT() + "grafik/servercheck.txt");
            LoggerFactory.getDefaultLogger().info("\r\n" + br.getRequest());
            ret.setWorker(Integer.parseInt(new Regex(servercheck, "worker=(\\d+)").getMatch(0)));
            ret.setAvgSolvtime(Integer.parseInt(new Regex(servercheck, "avg1h=(\\d+)").getMatch(0)));
            ret.setQueue(Integer.parseInt(new Regex(servercheck, "queue=(\\d+)").getMatch(0)));
            ret.setQueue1(Integer.parseInt(new Regex(servercheck, "queue1=(\\d+)").getMatch(0)));
            ret.setQueue2(Integer.parseInt(new Regex(servercheck, "queue2=(\\d+)").getMatch(0)));
            ret.setInWork(Integer.parseInt(new Regex(servercheck, "inwork=(\\d+)").getMatch(0)));
            ret.setWorkerMouse(Integer.parseInt(new Regex(servercheck, "workermouse=(\\d+)").getMatch(0)));
            ret.setWorkerMultiMouse(Integer.parseInt(new Regex(servercheck, "workermultimouse=(\\d+)").getMatch(0)));
            ret.setWorkerConfirm(Integer.parseInt(new Regex(servercheck, "workerconfirm=(\\d+)").getMatch(0)));
            ret.setWorkerText(Integer.parseInt(new Regex(servercheck, "workertext=(\\d+)").getMatch(0)));
            ret.setWorkerTextOnly(Integer.parseInt(new Regex(servercheck, "workertextonly=(\\d+)").getMatch(0)));
            ret.setWorkerPuzzle(Integer.parseInt(new Regex(servercheck, "workerpuzzle=(\\d+)").getMatch(0)));
            ret.setWorkerRotate(Integer.parseInt(new Regex(servercheck, "workerrotate=(\\d+)").getMatch(0)));
            ret.setWorkerAudio(Integer.parseInt(new Regex(servercheck, "workeraudio=(\\d+)").getMatch(0)));
            ret.setWorkerSpecial(Integer.parseInt(new Regex(servercheck, "workerinteractive=(\\d+)").getMatch(0)));
        } catch (NumberFormatException e) {
            ret.setError("API Error!");
        }
        if (!config.getApiKey().matches("^[a-zA-Z0-9]+$")) {
            ret.setError("API Key is not correct!");
        } else {
            credits = br.getPage(getAPIROOT() + "index.cgi?action=usercaptchaguthaben&apikey=" + Encoding.urlEncode(config.getApiKey()));
            LoggerFactory.getDefaultLogger().info("\r\n" + br.getRequest());
            try {
                ret.setCreditBalance(Integer.parseInt(credits.trim()));
                String userhistory1 = br.getPage(getAPIROOT() + "index.cgi?action=userhistory&short=1&apikey=" + Encoding.urlEncode(config.getApiKey()));
                String userhistory2 = br.getPage(getAPIROOT() + "index.cgi?action=userhistory2&short=1&apikey=" + Encoding.urlEncode(config.getApiKey()));
                ret.setAnswered9kw(Integer.parseInt(Regex.getLines(userhistory2)[0]));
                ret.setSolved9kw(Integer.parseInt(Regex.getLines(userhistory1)[0]));
            } catch (NumberFormatException e) {
                ret.setError(credits);
            }
        }
        return ret;
    }

    @Override
    public String getType() {
        return _GUI.T.Captcha9kwSolver_getName_();
    }

    @Override
    public boolean isEnabled() {
        return config.isEnabledGlobally() && (config.ispuzzle() || config.ismouse() || config.isEnabled());
    }

    @Override
    public void setEnabled(boolean b) {
        if (b) {
            config.setEnabledGlobally(true);
            if (!config.isEnabled() || config.ispuzzle() || config.ismouse()) {
                config.setEnabled(b);
                config.setmouse(b);
                config.setpuzzle(b);
            }
        } else {
            config.setEnabledGlobally(false);
        }
    }

    private Captcha9kwSettings         config;
    private Captcha9kwSolver           textSolver;
    private Captcha9kwSolverClick      clickSolver;
    private Captcha9kwSolverMultiClick multiclickSolver;

    private void updateServicePanel(boolean b) {
        if (!Application.isHeadless()) {
            ServicePanel.getInstance().requestUpdate(b);
        }
    }

    /**
     * Create a new instance of NineKwSolverService. This is a singleton class. Access the only existing instance by using
     * {@link #getInstance()}.
     */
    private NineKwSolverService() {
        config = JsonConfig.create(Captcha9kwSettings.class);
        AdvancedConfigManager.getInstance().register(config);
        if (!Application.isHeadless()) {
            SecondLevelLaunch.GUI_COMPLETE.executeWhenReached(new Runnable() {
                public void run() {
                    ServicePanel.getInstance().addExtender(NineKwSolverService.this);
                    CFG_9KWCAPTCHA.API_KEY.getEventSender().addListener(new GenericConfigEventListener<String>() {
                        @Override
                        public void onConfigValueModified(KeyHandler<String> keyHandler, String newValue) {
                            updateServicePanel(true);
                        }

                        @Override
                        public void onConfigValidatorError(KeyHandler<String> keyHandler, String invalidValue, ValidationException validateException) {
                        }
                    });
                    CFG_9KWCAPTCHA.ENABLED_GLOBALLY.getEventSender().addListener(new GenericConfigEventListener<Boolean>() {
                        @Override
                        public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
                        }

                        @Override
                        public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
                            updateServicePanel(true);
                        }
                    });
                    CFG_9KWCAPTCHA.ENABLED.getEventSender().addListener(new GenericConfigEventListener<Boolean>() {
                        @Override
                        public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
                        }

                        @Override
                        public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
                            updateServicePanel(true);
                        }
                    });
                    CFG_9KWCAPTCHA.MOUSE.getEventSender().addListener(new GenericConfigEventListener<Boolean>() {
                        @Override
                        public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
                        }

                        @Override
                        public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
                            updateServicePanel(true);
                        }
                    });
                    CFG_9KWCAPTCHA.PUZZLE.getEventSender().addListener(new GenericConfigEventListener<Boolean>() {
                        @Override
                        public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
                        }

                        @Override
                        public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
                            updateServicePanel(true);
                        }
                    });
                }
            });
        }
    }

    @Override
    public Map<String, Integer> getWaitForOthersDefaultMap() {
        HashMap<String, Integer> ret = new HashMap<String, Integer>();
        ret.put(JacSolverService.ID, 30000);
        // ret.put(DeathByCaptchaSolverService.ID, 60000);
        // ret.put(ImageTyperzSolverService.ID, 60000);
        // ret.put(CheapCaptchaSolverService.ID, 60000);
        // ret.put(EndCaptchaSolverService.ID, 60000);
        // ret.put(TwoCaptchaSolverService.ID, 60000);
        return ret;
    }

    @Override
    public Icon getIcon(int size) {
        return new AbstractIcon(IconKey.ICON_LOGO_9KW, size);
    }

    @Override
    public AbstractCaptchaSolverConfigPanel getConfigPanel() {
        return new NineKwConfigPanel(this);
    }

    @Override
    public boolean hasConfigPanel() {
        return true;
    }

    @Override
    public String getName() {
        return _GUI.T.Captcha9kwSolver_gettypeName_();
    }

    @Override
    public Captcha9kwSettings getConfig() {
        return config;
    }

    @Override
    public String getID() {
        return ID;
    }

    public void setTextSolver(Captcha9kwSolver captcha9kwSolver) {
        this.textSolver = captcha9kwSolver;
    }

    public void setClickSolver(Captcha9kwSolverClick captcha9kwSolverClick) {
        this.clickSolver = captcha9kwSolverClick;
    }

    public void setMultiClickSolver(Captcha9kwSolverMultiClick captcha9kwSolverMultiClick) {
        this.multiclickSolver = captcha9kwSolverMultiClick;
    }
}
