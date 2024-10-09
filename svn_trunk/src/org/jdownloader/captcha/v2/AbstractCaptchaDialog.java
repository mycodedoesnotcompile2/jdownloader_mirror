package org.jdownloader.captcha.v2;

import java.awt.Cursor;
import java.awt.Dialog.ModalExclusionType;
import java.awt.Dialog.ModalityType;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.net.URL;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.BooleanControl;
import javax.sound.sampled.Clip;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.FloatControl;
import javax.sound.sampled.LineEvent;
import javax.sound.sampled.LineEvent.Type;
import javax.sound.sampled.LineListener;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;

import jd.controlling.linkcrawler.CrawledLink;
import jd.gui.swing.dialog.DialogType;
import jd.gui.swing.jdgui.JDGui;
import jd.plugins.DownloadLink;
import jd.plugins.Plugin;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import net.miginfocom.swing.MigLayout;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.config.JsonConfig;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtButton;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.AbstractDialog;
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
import org.jdownloader.captcha.v2.solver.CESChallengeSolver;
import org.jdownloader.captcha.v2.solver.gui.Header;
import org.jdownloader.captcha.v2.solverjob.SolverJob;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.components.HeaderScrollPane;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.BadgeIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.controller.host.HostPluginController;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;
import org.jdownloader.premium.PremiumInfoDialog;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;
import org.jdownloader.settings.SoundSettings;
import org.jdownloader.settings.staticreferences.CFG_CAPTCHA;
import org.jdownloader.settings.staticreferences.CFG_GUI;
import org.jdownloader.updatev2.gui.LAFOptions;

public abstract class AbstractCaptchaDialog<T> extends AbstractDialog<T> {
    // private LocationStorage config;
    protected boolean hideCaptchasForHost = false;

    @Override
    protected FrameState getWindowStateOnVisible() {
        return AbstractCaptchaDialog.getWindowState();
    }

    @Override
    public boolean isExpired(long currentTimeout) {
        boolean is = super.isExpired(currentTimeout);
        boolean cesActive = false;
        SolverJob<?> job = challenge.getJob();
        if (challenge != null) {
            // don't autoclose captcha dialog if ces solver is still active
            for (ChallengeSolver s : challenge.getJob().getSolverList()) {
                if (s instanceof CESChallengeSolver && !challenge.getJob().isDone(s)) {
                    cesActive = true;
                }
            }
        }
        return is && !cesActive;
    }

    @Override
    public String formatCountdown(long currentTimeout) {
        if (currentTimeout < 0) {
            return "wait for C.E.S.";
        }
        return super.formatCountdown(currentTimeout);
    }

    @Override
    protected T createReturnValue() {
        return null;
    }

    protected void createPopup() {
        final JPopupMenu popup = new JPopupMenu();
        JMenuItem mi;
        if (getType() == DialogType.HOSTER) {
            mi = new JMenuItem(new AppAction() {
                {
                    setName(_GUI.T.AbstractCaptchaDialog_createPopup_skip_and_disable_all_downloads_from(getHost()));
                    try {
                        setSmallIcon(getDomainInfo().getIcon(16));
                    } catch (final Throwable e) {
                        e.printStackTrace();
                    }
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    hideCaptchasForHost = true;
                    setReturnmask(false);
                    dispose();
                }
            });
            popup.add(mi);
            mi = new JMenuItem(new AppAction() {
                {
                    setName(_GUI.T.AbstractCaptchaDialog_createPopup_skip_and_disable_package(getPackageName()));
                    setSmallIcon(new BadgeIcon(IconKey.ICON_PACKAGE_OPEN, IconKey.ICON_SKIPPED, 16));
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    hideCaptchasForPackage = true;
                    setReturnmask(false);
                    dispose();
                }
            });
            popup.add(mi);
            mi = new JMenuItem(new AppAction() {
                {
                    setName(_GUI.T.AbstractCaptchaDialog_createPopup_skip_and_hide_all_captchas_download());
                    setSmallIcon(NewTheme.I().getIcon(IconKey.ICON_CLEAR, 16));
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    hideAllCaptchas = true;
                    setReturnmask(false);
                    dispose();
                }
            });
            popup.add(mi);
            mi = new JMenuItem(new AppAction() {
                {
                    setName(_GUI.T.AbstractCaptchaDialog_createPopup_skip_and_stop_all_downloads());
                    setSmallIcon(new AbstractIcon(IconKey.ICON_STOP, 16));
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    stopDownloads = true;
                    setReturnmask(false);
                    dispose();
                }
            });
            popup.add(mi);
        } else {
            mi = new JMenuItem(new AppAction() {
                {
                    setName(_GUI.T.AbstractCaptchaDialog_createPopup_cancel_linkgrabbing());
                    setSmallIcon(new AbstractIcon(IconKey.ICON_STOP, 16));
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    stopCrawling = true;
                    setReturnmask(false);
                    dispose();
                }
            });
            popup.add(mi);
            mi = new JMenuItem(new AppAction() {
                {
                    setName(_GUI.T.AbstractCaptchaDialog_createPopup_cancel_stop_showing_crawlercaptchs());
                    setSmallIcon(new AbstractIcon(IconKey.ICON_FIND, 16));
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    stopShowingCrawlerCaptchas = true;
                    setReturnmask(false);
                    dispose();
                }
            });
            popup.add(mi);
        }
        Insets insets = LAFOptions.getInstance().getExtension().customizePopupBorderInsets();
        Dimension pref = popup.getPreferredSize();
        pref.height = popup.getComponentCount() * 24 + insets.top + insets.bottom;
        popup.setPreferredSize(pref);
        popup.show(cancelButton, +insets.left - pref.width + cancelButton.getWidth() + 8 + 5, +cancelButton.getHeight());
    }

    @Override
    public void onSetVisible(boolean b) {
        super.onSetVisible(b);
        if (b) {
            playCaptchaSound();
        }
    }

    @Override
    public ModalityType getModalityType() {
        return ModalityType.MODELESS;
    }

    protected boolean      hideCaptchasForPackage = false;
    protected final String explain;
    protected boolean      hideAllCaptchas;
    protected DomainInfo   hosterInfo;
    protected JPanel       iconPanel;

    public boolean isHideAllCaptchas() {
        return hideAllCaptchas;
    }

    protected Plugin              plugin;
    protected boolean             stopDownloads = false;
    protected DialogType          type;
    protected boolean             refresh;
    protected boolean             stopCrawling;
    protected boolean             stopShowingCrawlerCaptchas;
    protected final Challenge<T>  challenge;
    protected MouseMotionListener mouseMotionListener;
    protected MouseListener       mouseListener;

    public AbstractCaptchaDialog(Challenge<T> challenge, int flags, String title, DialogType type, DomainInfo domainInfo, String explain) {
        super(flags, title, null, _GUI.T.AbstractCaptchaDialog_AbstractCaptchaDialog_continue(), type == DialogType.CRAWLER ? _GUI.T.lit_cancel() : _GUI.T.AbstractCaptchaDialog_AbstractCaptchaDialog_cancel());
        this.challenge = challenge;
        if (JsonConfig.create(GraphicalUserInterfaceSettings.class).isCaptchaDialogUniquePositionByHosterEnabled()) {
            setLocator(new RememberAbsoluteDialogLocator("CaptchaDialog_" + domainInfo.getTld()));
        } else {
            setLocator(new RememberAbsoluteDialogLocator("CaptchaDialog"));
        }
        setDimensor(new RememberLastDialogDimension("Captcha-" + domainInfo.getTld()));
        if (explain != null) {
            this.explain = explain;
        } else {
            this.explain = challenge.getExplain();
        }
        this.hosterInfo = domainInfo;
        this.type = type;
        setPlugin(challenge.getPlugin());
        if (!challenge.keepAlive()) {
            // no reason to let the user stop the countdown if the result cannot be used after the countdown anyway
            setCountdownPausable(false);
        }
    }

    protected Challenge<T> getChallenge() {
        return challenge;
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

    public static void playCaptchaSound() {
        URL soundUrl = null;
        if (JsonConfig.create(SoundSettings.class).isCaptchaSoundEnabled() && (soundUrl = NewTheme.I().getURL("sounds/", "captcha", ".wav")) != null) {
            final URL finalSoundUrl = soundUrl;
            new Thread("Captcha Sound") {
                public void run() {
                    AudioInputStream stream = null;
                    Clip clip = null;
                    try {
                        stream = AudioSystem.getAudioInputStream(finalSoundUrl);
                        final AudioFormat format = stream.getFormat();
                        final DataLine.Info info = new DataLine.Info(Clip.class, format);
                        if (AudioSystem.isLineSupported(info)) {
                            clip = (Clip) AudioSystem.getLine(info);
                            clip.open(stream);
                            try {
                                final FloatControl gainControl = (FloatControl) clip.getControl(FloatControl.Type.MASTER_GAIN);
                                float db = (20f * (float) Math.log(JsonConfig.create(SoundSettings.class).getCaptchaSoundVolume() / 100f));
                                gainControl.setValue(Math.max(-80f, db));
                                BooleanControl muteControl = (BooleanControl) clip.getControl(BooleanControl.Type.MUTE);
                                muteControl.setValue(true);
                                muteControl.setValue(false);
                            } catch (Exception e) {
                                org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
                            }
                            final AtomicBoolean runningFlag = new AtomicBoolean(true);
                            clip.addLineListener(new LineListener() {
                                @Override
                                public void update(LineEvent event) {
                                    if (event.getType() == Type.STOP) {
                                        runningFlag.set(false);
                                    }
                                }
                            });
                            clip.start();
                            Thread.sleep(1000);
                            while (clip.isRunning() && runningFlag.get()) {
                                Thread.sleep(100);
                            }
                        }
                    } catch (Throwable e) {
                        org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
                    } finally {
                        try {
                            if (clip != null) {
                                final Clip finalClip = clip;
                                Thread thread = new Thread() {
                                    public void run() {
                                        finalClip.close();
                                    };
                                };
                                thread.setName("AudioStop");
                                thread.setDaemon(true);
                                thread.start();
                                thread.join(2000);
                            }
                        } catch (Throwable e) {
                        }
                        try {
                            if (stream != null) {
                                stream.close();
                            }
                        } catch (Throwable e) {
                        }
                    }
                }
            }.start();
        }
    }

    public boolean isStopCrawling() {
        return stopCrawling;
    }

    public boolean isStopShowingCrawlerCaptchas() {
        return stopShowingCrawlerCaptchas;
    }

    public void dispose() {
        if (!isInitialized()) {
            return;
        }
        super.dispose();
    }

    public String getCrawlerStatus() {
        switch (type) {
        case CRAWLER:
            return ((PluginForDecrypt) plugin).getCrawlerStatusString();
        default:
            return null;
        }
    }

    @Override
    protected DefaultButtonPanel getDefaultButtonPanel() {
        final DefaultButtonPanel ret = new DefaultButtonPanel("ins 0", "[]", "0[grow,fill]0") {
            @Override
            public void addOKButton(JButton okButton) {
                final ExtButton refreshBtn = new ExtButton(new AppAction() {
                    {
                        setSmallIcon(new AbstractIcon(IconKey.ICON_REFRESH, 18));
                        setTooltipText(_GUI.T.CaptchaDialog_layoutDialogContent_refresh());
                        KeyStroke ks = KeyStroke.getKeyStroke(CFG_GUI.CFG.getShortcutForCaptchaDialogRefresh());
                        if (ks == null) {
                            ks = KeyStroke.getKeyStroke("pressed F5");
                        }
                        setAccelerator(ks);
                    }

                    @Override
                    public void actionPerformed(ActionEvent e) {
                        refresh = true;
                        setReturnmask(false);
                        dispose();
                    }
                }) {
                    @Override
                    public int getTooltipDelay(Point mousePositionOnScreen) {
                        return 500;
                    }
                };
                //
                refreshBtn.setRolloverEffectEnabled(true);
                super.add(refreshBtn, "alignx right,width 28!");
                super.addOKButton(okButton);
            }

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
                if (CFG_CAPTCHA.CFG.isCancelDialogCountdownOnHateCaptchaClick()) {
                    cancel();
                }
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
        // premium.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0,
        // premium.getForeground()));
        LazyHostPlugin plg = HostPluginController.getInstance().get(hosterInfo.getTld());
        if (plg != null && plg.isPremium() && CFG_GUI.CFG.isHateCaptchasTextInCaptchaDialogVisible()) {
            ret.add(premium);
        }
        SwingUtils.setOpaque(premium, false);
        return ret;
    }

    public DomainInfo getDomainInfo() {
        return hosterInfo;
    }

    public long getFilesize() {
        switch (type) {
        case HOSTER:
            if (plugin == null || ((PluginForHost) plugin).getDownloadLink() == null) {
                return -1;
            }
            return ((PluginForHost) plugin).getDownloadLink().getView().getBytesTotal();
        default:
            return -1;
        }
    }

    public String getHelpText() {
        return explain;
    }

    public String getHost() {
        switch (type) {
        case HOSTER:
            return ((PluginForHost) plugin).getHost(((PluginForHost) plugin).getDownloadLink(), null, false);
        case ACCOUNT:
        case CRAWLER:
            return plugin.getHost();
        default:
            return challenge.getHost();
        }
    }

    protected String getPackageName() {
        switch (type) {
        case HOSTER:
            if (plugin == null || ((PluginForHost) plugin).getDownloadLink() == null) {
                return null;
            }
            return ((PluginForHost) plugin).getDownloadLink().getFilePackage().getName();
        default:
            return null;
        }
    }

    public DialogType getType() {
        return type;
    }

    public boolean isHideCaptchasForHost() {
        return hideCaptchasForHost;
    }

    public boolean isHideCaptchasForPackage() {
        return hideCaptchasForPackage;
    }

    public boolean isStopDownloads() {
        return stopDownloads;
    }

    public String getFilename() {
        switch (type) {
        case HOSTER: {
            final DownloadLink link;
            if (plugin == null || (link = ((PluginForHost) plugin).getDownloadLink()) == null) {
                return null;
            }
            return link.getView().getDisplayName();
        }
        case CRAWLER: {
            final CrawledLink link;
            if (plugin == null || (link = ((PluginForDecrypt) plugin).getCurrentLink()) == null) {
                return null;
            }
            return link.getName();
        }
        default:
            return null;
        }
    }

    protected Header createHeader() {
        final Header headerPanel;
        final String headerText;
        if (type == DialogType.ACCOUNT) {
            // TODO:
            throw new WTFException("FIXME");
        } else if (type == DialogType.HOSTER) {
            if (!JsonConfig.create(GeneralSettings.class).isShowFileNameInCaptchaDialogEnabled()) {
                return null;
            }
            final String fileName = getFilename();
            headerPanel = new Header("ins 0 0 1 0", "[grow,fill]", "[]");
            final long fileSize = getFilesize();
            if (fileSize > 0) {
                headerText = (_GUI.T.CaptchaDialog_layoutDialogContent_header(fileName, SizeFormatter.formatBytes(fileSize), hosterInfo.getTld()));
            } else {
                headerText = (_GUI.T.CaptchaDialog_layoutDialogContent_header2(fileName, hosterInfo.getTld()));
            }
        } else if (type == DialogType.CRAWLER) {
            headerPanel = new Header("ins 0 0 1 0", "[grow,fill]", "[grow,fill]");
            final String crawlerStatus = getCrawlerStatus();
            if (crawlerStatus == null) {
                headerText = (_GUI.T.CaptchaDialog_layoutDialogContent_header_crawler(hosterInfo.getTld()));
            } else {
                headerText = (_GUI.T.CaptchaDialog_layoutDialogContent_header_crawler2(crawlerStatus, hosterInfo.getTld()));
            }
        } else {
            return null;
        }

        final JLabel header = new JLabel() {
            private boolean setting = false;

            protected void paintComponent(Graphics g) {
                if (headerText != null) {
                    setting = true;
                    try {
                        setText(org.appwork.sunwrapper.sun.swing.SwingUtilities2Wrapper.clipStringIfNecessary(this, this.getFontMetrics(getFont()), headerText, getWidth() - 30));
                    } catch (Throwable e) {
                        // http://www.oracle.com/technetwork/java/faq-sun-packages-142232.html
                        e.printStackTrace();
                        setText(headerText);
                    }
                    setting = false;
                }
                super.paintComponent(g);
            }

            public void repaint() {
                if (setting) {
                    return;
                }
                super.repaint();
            }

            public void revalidate() {
                if (setting) {
                    return;
                }
                super.revalidate();
            }
        };
        header.setIcon(hosterInfo.getFavIcon());
        headerPanel.add(header);
        return headerPanel;
    }

    @Override
    public JComponent layoutDialogContent() {
        getDialog().setModalityType(getModalityType());
        final LAFOptions lafOptions = LAFOptions.getInstance();
        MigPanel field = new MigPanel("ins 0,wrap 1", "[grow,fill]", "[grow,fill]");
        SwingUtils.setOpaque(field, false);
        getDialog().setMinimumSize(new Dimension(0, 0));
        final JPanel panel = new JPanel(getDialogLayout());
        SwingUtils.setOpaque(panel, false);
        LAFOptions.applyBackground(lafOptions.getColorForPanelBackground(), field);
        getDialog().setModalExclusionType(ModalExclusionType.TOOLKIT_EXCLUDE);

        addBeforeImage(field);
        iconPanel = createCaptchaPanel();
        if (iconPanel != null) {
            SwingUtils.setOpaque(iconPanel, false);
            // iconPanel.add(refreshBtn, "alignx right,aligny bottom");
            if (CFG_CAPTCHA.CFG.isCancelDialogCountdownOnMouseMove()) {
                iconPanel.addMouseMotionListener(mouseMotionListener = new MouseMotionListener() {
                    private Point lastMouse;

                    @Override
                    public void mouseDragged(MouseEvent e) {
                        cancel();
                    }

                    @Override
                    public void mouseMoved(MouseEvent e) {
                        if (lastMouse != null && !lastMouse.equals(e.getPoint())) {
                            cancel();
                        }
                        lastMouse = e.getPoint();
                    }
                });
            }
            if (CFG_CAPTCHA.CFG.isCancelDialogCountdownOnMouseClick()) {
                iconPanel.addMouseListener(mouseListener = new MouseListener() {

                    @Override
                    public void mouseClicked(MouseEvent e) {
                        cancel();
                    }

                    @Override
                    public void mousePressed(MouseEvent e) {
                        cancel();
                    }

                    @Override
                    public void mouseReleased(MouseEvent e) {
                        cancel();
                    }

                    @Override
                    public void mouseEntered(MouseEvent e) {
                        // no cancel here. this would cancel the timeout if the dialog appears under the mouse - even if it does not move
                    }

                    @Override
                    public void mouseExited(MouseEvent e) {
                        cancel();
                    }
                });
            }
            field.add(iconPanel);
        }
        final HeaderScrollPane sp = createHeaderScrollPane(field);
        if (!CFG_GUI.CFG.isCaptchaDialogBorderAroundImageEnabled()) {
            sp.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
            SwingUtils.setOpaque(field, false);
        }
        panel.add(sp);
        final Header header = createHeader();
        if (header != null) {
            sp.setColumnHeaderView(header);
        }
        return panel;
    }

    protected void addBeforeImage(MigPanel field) {
    }

    protected HeaderScrollPane createHeaderScrollPane(MigPanel field) {
        return new HeaderScrollPane(field);
    }

    protected MigLayout getDialogLayout() {
        return new MigLayout("ins 0,wrap 1", "[fill,grow]", "[grow,fill]");
    }

    protected abstract JPanel createCaptchaPanel();

    public boolean isRefresh() {
        return refresh;
    }

    public void setRefresh(boolean refresh) {
        this.refresh = refresh;
    }

    public List<? extends Image> getIconList() {
        return JDGui.getInstance().getMainFrame().getIconImages();
    }

    @Override
    public Window getOwner() {
        return JDGui.getInstance().getMainFrame();
    }

    protected void setPlugin(Plugin plugin) {
        this.plugin = plugin;
    }
}
