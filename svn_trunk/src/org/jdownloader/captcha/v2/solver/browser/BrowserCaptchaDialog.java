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
package org.jdownloader.captcha.v2.solver.browser;

import java.awt.Cursor;
import java.awt.Dialog.ModalityType;
import java.awt.Image;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;

import jd.gui.swing.dialog.AbstractImageCaptchaDialog;
import jd.gui.swing.dialog.DialogType;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.views.settings.components.Checkbox;
import net.miginfocom.swing.MigLayout;

import org.appwork.storage.config.JsonConfig;
import org.appwork.swing.components.ExtButton;
import org.appwork.uio.UIOManager;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.DefaultButtonPanel;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.appwork.utils.swing.dialog.dimensor.RememberLastDialogDimension;
import org.appwork.utils.swing.dialog.locator.RememberAbsoluteDialogLocator;
import org.appwork.utils.swing.windowmanager.WindowManager;
import org.appwork.utils.swing.windowmanager.WindowManager.FrameState;
import org.jdownloader.DomainInfo;
import org.jdownloader.actions.AppAction;
import org.jdownloader.captcha.v2.AbstractCaptchaDialog;
import org.jdownloader.captcha.v2.ChallengeResponseController;
import org.jdownloader.captcha.v2.solver.service.BrowserSolverService;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.settings.AbstractConfigPanel;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.controller.host.HostPluginController;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;
import org.jdownloader.premium.PremiumInfoDialog;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;
import org.jdownloader.settings.staticreferences.CFG_GUI;

/**
 * This Dialog is used to display a Inputdialog for the captchas
 */
public class BrowserCaptchaDialog extends AbstractCaptchaDialog<String> {

    public BrowserCaptchaDialog(int flag, DialogType type, DomainInfo domainInfo, AbstractBrowserChallenge captchaChallenge) {
        super(captchaChallenge, flag | Dialog.STYLE_HIDE_ICON, _GUI.T.gui_captchaWindow_askForInput(domainInfo.getTld()), type, domainInfo, null);
        if (JsonConfig.create(GraphicalUserInterfaceSettings.class).isCaptchaDialogUniquePositionByHosterEnabled()) {
            setLocator(new RememberAbsoluteDialogLocator("CaptchaDialog_" + domainInfo.getTld()));
        } else {
            setLocator(new RememberAbsoluteDialogLocator("CaptchaDialog"));
        }
        setDimensor(new RememberLastDialogDimension("Captcha-" + getHost() + "." + challenge.getClass().getSimpleName() + "." + challenge.getTypeID()));
    }

    public static FrameState getWindowState() {
        for (Window w : Window.getWindows()) {
            if (WindowManager.getInstance().hasFocus(w)) {
                return FrameState.TO_FRONT_FOCUSED;
            }
        }
        FrameState ret = (FrameState) CFG_GUI.NEW_DIALOG_FRAME_STATE.getValue();
        if (ret == null) {
            ret = FrameState.TO_FRONT;
        }
        switch (ret) {
        case OS_DEFAULT:
        case TO_BACK:
            JDGui.getInstance().flashTaskbar();
        }
        return ret;
    }

    @Override
    protected FrameState getWindowStateOnVisible() {
        return getWindowState();
    }

    @Override
    public void onSetVisible(boolean b) {
        super.onSetVisible(b);
        if (b) {
            AbstractImageCaptchaDialog.playCaptchaSound();
        }
    }

    @Override
    public ModalityType getModalityType() {
        return ModalityType.MODELESS;
    }

    protected AbstractConfigPanel                          iconPanel;
    protected Point                                        offset;

    protected boolean                                      refresh;
    protected final CopyOnWriteArrayList<BrowserReference> browserReferences = new CopyOnWriteArrayList<BrowserReference>();
    private volatile String                                responseCode;
    private WindowFocusListener                            openBrowserFocusListener;

    @Override
    protected String createReturnValue() {
        return responseCode;
    }

    public void dispose() {
        try {
            if (!isInitialized()) {
                return;
            }
            try {
                if (dialog != null) {
                    if (openBrowserFocusListener != null) {
                        getDialog().removeWindowFocusListener(openBrowserFocusListener);
                    }
                }
            } finally {
                super.dispose();
            }
        } finally {
            while (browserReferences.size() > 0) {
                final BrowserReference browserReference = browserReferences.remove(0);
                if (browserReference != null) {
                    browserReference.dispose();
                }
            }
        }
    }

    @Override
    protected DefaultButtonPanel getDefaultButtonPanel() {
        final DefaultButtonPanel ret = new DefaultButtonPanel("ins 0", "[]", "0[grow,fill]0") {
            @Override
            public void addCancelButton(final JButton cancelButton) {
                super.addCancelButton(cancelButton);
                final JButton bt = new JButton(new AbstractIcon(IconKey.ICON_POPDOWNSMALL, -1)) {
                    public void setBounds(int x, int y, int width, int height) {
                        int delta = 5;
                        super.setBounds(x - delta, y, width + delta, height);
                    }
                };
                bt.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        createPopup();
                    }
                });
                super.add(bt, "gapleft 0,width 8!");
            }
        };
        ret.setOpaque(false);
        ExtButton premium = new ExtButton(new AppAction() {
            /**
             *
             */
            private static final long serialVersionUID = -3551320196255605774L;
            {
                setName(_GUI.T.CaptchaDialog_getDefaultButtonPanel_premium());
            }

            public void actionPerformed(ActionEvent e) {
                cancel();
                PremiumInfoDialog d = new PremiumInfoDialog(hosterInfo, _GUI.T.PremiumInfoDialog_PremiumInfoDialog_(hosterInfo.getTld()), "CaptchaDialog");
                try {
                    Dialog.getInstance().showDialog(d);
                } catch (DialogClosedException e1) {
                    e1.printStackTrace();
                } catch (DialogCanceledException e1) {
                    e1.printStackTrace();
                }
            }
        });
        premium.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        premium.setRolloverEffectEnabled(true);
        final LazyHostPlugin plg = HostPluginController.getInstance().get(hosterInfo.getTld());
        if (plg != null && plg.isPremium() && CFG_GUI.CFG.isHateCaptchasTextInCaptchaDialogVisible()) {
            ret.add(premium);
        }
        SwingUtils.setOpaque(premium, false);
        return ret;
    }

    public Point getOffset() {
        return offset;
    }

    @Override
    protected boolean isResizable() {
        return true;
    }

    @Override
    protected MigLayout getDialogLayout() {
        return new MigLayout("ins 0,wrap 1", "[fill,grow]", "[grow,fill]10[]");
    }

    @Override
    protected JPanel createCaptchaPanel() {
        iconPanel = new AbstractConfigPanel(5) {
            @Override
            public Icon getIcon() {
                return null;
            }

            @Override
            public String getLeftGap() {
                return "0";
            }

            @Override
            public String getTitle() {
                return null;
            }

            @Override
            public void save() {
            }

            @Override
            public void updateContents() {
            }
        };
        iconPanel.addDescriptionPlain(_GUI.T.BrowserCaptchaDialog_layoutDialogContent_explain_());
        iconPanel.addPair(_GUI.T.BrowserCaptchaDialog_layoutDialogContent_autoclick(), null, new Checkbox(CFG_BROWSER_CAPTCHA_SOLVER.AUTO_CLICK_ENABLED));
        iconPanel.addPair(_GUI.T.BrowserCaptchaDialog_layoutDialogContent_autoopen(), null, new Checkbox(CFG_BROWSER_CAPTCHA_SOLVER.AUTO_OPEN_BROWSER_ENABLED));
        return iconPanel;
    }

    @Override
    public JComponent layoutDialogContent() {
        final JComponent ret = super.layoutDialogContent();
        if (BrowserSolverService.getInstance().getConfig().isAutoOpenBrowserEnabled()) {
            final Thread openBrowseThread = new Thread("BrowserCaptcha:AutoOpen:" + challenge) {
                {
                    setDaemon(true);
                }

                public void run() {
                    try {
                        autoOpenDelay();
                        if (isAutoOpenStillRequired()) {
                            openBrowser();
                        }
                    } catch (InterruptedException ignore) {
                    }
                };
            };
            if (CFG_GUI.CFG.getNewDialogFrameState() != FrameState.TO_BACK) {
                openBrowseThread.start();
            } else {
                getDialog().addWindowFocusListener(openBrowserFocusListener = new WindowFocusListener() {
                    @Override
                    public void windowLostFocus(WindowEvent e) {
                    }

                    @Override
                    public void windowGainedFocus(WindowEvent e) {
                        openBrowseThread.start();
                        getDialog().removeWindowFocusListener(this);
                    }
                });
            }
        }
        return ret;
    }

    protected boolean isAutoOpenStillRequired() {
        if (!CFG_BROWSER_CAPTCHA_SOLVER.AUTO_OPEN_BROWSER_ENABLED.isEnabled()) {
            return false;
        } else {
            final SolverJob<?> job = ChallengeResponseController.getInstance().getJobByChallengeId(challenge.getId().getID());
            if (challenge.isSolved() || job == null || job.isDone() || BrowserSolver.getInstance().isJobDone(job)) {
                return false;
            } else {
                return true;
            }
        }
    }

    protected void autoOpenDelay() throws InterruptedException {
        int autoOpenDelay = Math.max(100, BrowserSolverService.getInstance().getConfig().getAutoOpenDelay());
        while (autoOpenDelay > 0 && isAutoOpenStillRequired()) {
            Thread.sleep(100);
            autoOpenDelay = autoOpenDelay - 100;
        }
        if (!isAutoOpenStillRequired()) {
            throw new InterruptedException("Challenge is no longer required");
        }
    }

    public void actionPerformed(final ActionEvent e) {
        if (e.getSource() == this.okButton) {
            new Thread("BrowserCaptchaDialog:actionPerformed:" + challenge) {
                {
                    setDaemon(true);
                }

                public void run() {
                    openBrowser();
                };
            }.start();
        } else {
            super.actionPerformed(e);
        }
    }

    @Override
    public String getOKButtonText() {
        return _GUI.T.BrowserCaptchaDialog_getOKButtonText_open_browser();
    }

    public boolean isRefresh() {
        return refresh;
    }

    public void setRefresh(boolean refresh) {
        this.refresh = refresh;
    }

    public void mouseClicked(final MouseEvent e) {
        this.cancel();
    }

    public void mouseEntered(final MouseEvent e) {
    }

    public void mouseExited(final MouseEvent e) {
    }

    public void mousePressed(final MouseEvent e) {
        this.cancel();
    }

    public void mouseReleased(final MouseEvent e) {
        this.cancel();
    }

    public List<? extends Image> getIconList() {
        return JDGui.getInstance().getMainFrame().getIconImages();
    }

    protected void openBrowser() {
        final BrowserReference browserReference = new BrowserReference((AbstractBrowserChallenge) challenge) {
            @Override
            public void onResponse(String parameter) {
                responseCode = parameter;
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        setReturnmask(true);
                        BrowserCaptchaDialog.this.dispose();
                    }
                };
            }
        };
        browserReferences.add(browserReference);
        try {
            browserReference.open();
        } catch (Throwable e1) {
            browserReference.dispose();
            browserReferences.remove(browserReference);
            UIOManager.I().showException(e1.getMessage(), e1);
        }
    }

}