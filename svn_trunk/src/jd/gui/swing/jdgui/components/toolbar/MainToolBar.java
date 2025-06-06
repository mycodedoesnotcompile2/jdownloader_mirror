//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
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
package jd.gui.swing.jdgui.components.toolbar;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import javax.swing.AbstractAction;
import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JRootPane;
import javax.swing.JSeparator;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.Timer;

import jd.SecondLevelLaunch;
import jd.controlling.downloadcontroller.DownloadLinkCandidate;
import jd.controlling.downloadcontroller.DownloadLinkCandidateResult;
import jd.controlling.downloadcontroller.DownloadSession;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.DownloadWatchDogJob;
import jd.controlling.downloadcontroller.DownloadWatchDogProperty;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.controlling.downloadcontroller.event.DownloadWatchdogListener;
import jd.gui.swing.components.SetIconInterface;
import jd.gui.swing.components.SetLabelInterface;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.components.speedmeter.SpeedMeterPanel;
import net.miginfocom.swing.MigLayout;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.swing.components.ExtButton;
import org.appwork.swing.components.ExtJToggleButton;
import org.appwork.utils.StringUtils;
import org.appwork.utils.ImageProvider.ImageProvider;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.controlling.contextmenu.MenuContainer;
import org.jdownloader.controlling.contextmenu.MenuContainerRoot;
import org.jdownloader.controlling.contextmenu.MenuItemData;
import org.jdownloader.controlling.contextmenu.MenuLink;
import org.jdownloader.controlling.contextmenu.SeparatorData;
import org.jdownloader.controlling.contextmenu.gui.ExtPopupMenu;
import org.jdownloader.controlling.contextmenu.gui.MenuBuilder;
import org.jdownloader.extensions.ExtensionNotLoadedException;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.toolbar.MenuManagerMainToolbar;
import org.jdownloader.gui.toolbar.action.AbstractToolBarAction;
import org.jdownloader.gui.views.downloads.QuickSettingsPopup;
import org.jdownloader.images.NewTheme;
import org.jdownloader.logging.LogController;
import org.jdownloader.settings.staticreferences.CFG_GUI;
import org.jdownloader.updatev2.gui.LAFOptions;

public class MainToolBar extends JToolBar implements MouseListener, DownloadWatchdogListener, GenericConfigEventListener<Boolean> {
    private static final long          serialVersionUID = 922971719957349497L;
    private static MainToolBar         INSTANCE         = null;
    private volatile SpeedMeterPanel   speedmeter;
    private JRootPane                  rootpane;
    private boolean                    initDone         = false;
    private LogSource                  logger;
    private HashMap<KeyStroke, Action> shortCutActions;

    public boolean isInitDone() {
        return initDone;
    }

    public static synchronized MainToolBar getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new MainToolBar();
        }
        return INSTANCE;
    }

    // @Override
    // public Dimension getPreferredSize() {
    // Dimension ret = super.getPreferredSize();
    // ret.height = 38;
    // return ret;
    // }
    //
    // @Override
    // public Dimension getMinimumSize() {
    //
    // Dimension ret = super.getMinimumSize();
    // ret.height = 38;
    // return ret;
    // }
    //
    // @Override
    // public Dimension getSize() {
    // Dimension ret = super.getSize();
    // ret.height = 38;
    // return ret;
    // }
    private MainToolBar() {
        super();
        logger = LogController.getInstance().getLogger("MainToolbar");
        this.addMouseListener(this);
        this.setRollover(true);
        this.setFloatable(false);
        SecondLevelLaunch.GUI_COMPLETE.executeWhenReached(new Runnable() {
            public void run() {
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        speedmeter = new SpeedMeterPanel(true, false);
                        speedmeter.addMouseListener(new MouseAdapter() {
                            @Override
                            public void mouseClicked(MouseEvent e) {
                                if (true) {
                                    QuickSettingsPopup pu = new QuickSettingsPopup();
                                    pu.show((Component) e.getSource(), e.getX(), e.getY());
                                }
                            }
                        });
                        CFG_GUI.SPEED_METER_VISIBLE.getEventSender().addListener(MainToolBar.this, false);
                    }
                };
                DownloadWatchDog.getInstance().getEventSender().addListener(MainToolBar.this);
            }
        });
        SecondLevelLaunch.EXTENSIONS_LOADED.executeWhenReached(new Runnable() {
            @Override
            public void run() {
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        if (!isInitDone()) {
                            updateToolbar();
                        }
                    }
                };
            }
        });
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.isPopupTrigger() || e.getButton() == MouseEvent.BUTTON3) {
                    QuickSettingsPopup pu = new QuickSettingsPopup();
                    pu.show((Component) e.getSource(), e.getX(), e.getY());
                }
            }
        });
        LAFOptions.getInstance().getExtension().customizeToolbar(this);
    }

    /**
     * USed to register the shortcuts to the rootpane during init
     *
     * @param jdGui
     */
    public void registerAccelerators(final JDGui jdGui) {
        this.rootpane = jdGui.getMainFrame().getRootPane();
    }

    @Override
    public void setVisible(boolean aFlag) {
        super.setVisible(aFlag);
    }

    /**
     * Updates the toolbar
     */
    public final void updateToolbar() {
        if (!SecondLevelLaunch.GUI_COMPLETE.isReached()) {
            return;
        }
        initDone = true;
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                setVisible(false);
                removeAll();
                initToolbar();
                updateSpecial();
                setVisible(true);
                revalidate();
            }
        };
    }

    protected void addImpl(Component comp, Object constraints, int index) {
        super.addImpl(comp, constraints, index);
        if (comp != speedmeter) {
            comp.removeMouseListener(this);
            comp.addMouseListener(this);
        }
    }

    private void fillActions(MenuContainer menuData) {
        if (!menuData._isValidated()) {
            return;
        }
        final InputMap input = getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
        final InputMap input2 = getInputMap(JComponent.WHEN_FOCUSED);
        final InputMap input3 = getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        final ActionMap actions = getActionMap();
        for (MenuItemData mi : menuData.getItems()) {
            if (!mi._isValidated()) {
                return;
            }
            if (mi instanceof MenuContainer) {
                fillActions((MenuContainer) mi);
            } else if (mi instanceof SeparatorData) {
                continue;
            } else if (mi instanceof MenuLink) {
                continue;
            } else {
                AppAction action;
                try {
                    if (mi.getActionData() == null || !mi.getActionData()._isValidDataForCreatingAnAction()) {
                        continue;
                    }
                    action = mi.createAction();
                    KeyStroke keystroke;
                    if (StringUtils.isNotEmpty(mi.getShortcut())) {
                        keystroke = KeyStroke.getKeyStroke(mi.getShortcut());
                        if (keystroke != null) {
                            action.setAccelerator(keystroke);
                        }
                    } else if (MenuItemData.isEmptyValue(mi.getShortcut())) {
                        action.setAccelerator(null);
                    }
                    keystroke = (KeyStroke) action.getValue(Action.ACCELERATOR_KEY);
                    linkAction(input, input2, input3, actions, action, keystroke);
                    if (action instanceof CustomizableAppAction) {
                        List<KeyStroke> moreShortCuts = ((CustomizableAppAction) action).getAdditionalShortcuts(keystroke);
                        if (moreShortCuts != null) {
                            for (KeyStroke ks : moreShortCuts) {
                                if (ks != null) {
                                    linkAction(input, input2, input3, actions, action, ks);
                                }
                            }
                        }
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
    }

    public void linkAction(final InputMap input, final InputMap input2, final InputMap input3, final ActionMap actions, AppAction action, KeyStroke keystroke) {
        if (action != null && (keystroke) != null) {
            String key = "CONTEXT_ACTION_" + keystroke;
            try {
                Object old = input.get(keystroke);
                if (old != null && action.getClass() != actions.get(old).getClass()) {
                    logger.warning("Duplicate Shortcuts: " + action + " overwrites " + actions.get(old) + "(" + old + ")" + " for keystroke " + keystroke);
                }
            } catch (Exception e) {
                logger.log(e);
            }
            try {
                Object old = input2.get(keystroke);
                if (old != null && action.getClass() != actions.get(old).getClass()) {
                    logger.warning("Duplicate Shortcuts: " + action + " overwrites " + actions.get(old) + "(" + old + ")" + " for keystroke " + keystroke);
                }
            } catch (Exception e) {
                logger.log(e);
            }
            try {
                Object old = input3.get(keystroke);
                if (old != null && action.getClass() != actions.get(old).getClass()) {
                    logger.warning("Duplicate Shortcuts: " + action + " overwrites " + actions.get(old) + "(" + old + ")" + " for keystroke " + keystroke);
                }
            } catch (Exception e) {
                logger.log(e);
            }
            logger.info(keystroke + " -> " + action);
            input.put(keystroke, key);
            input2.put(keystroke, key);
            input3.put(keystroke, key);
            actions.put(key, action);
            shortCutActions.put(keystroke, action);
        }
    }

    public void updateContextShortcuts(MenuContainerRoot container) {
        final InputMap input = getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
        final InputMap input2 = getInputMap(JComponent.WHEN_FOCUSED);
        final InputMap input3 = getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        final ActionMap actions = getActionMap();
        if (shortCutActions != null) {
            for (Entry<KeyStroke, Action> ks : shortCutActions.entrySet()) {
                Object binding = input.get(ks.getKey());
                input.remove(ks.getKey());
                input2.remove(ks.getKey());
                input3.remove(ks.getKey());
                actions.remove(binding);
            }
        }
        shortCutActions = new HashMap<KeyStroke, Action>();
        fillActions(container);
    }

    private void initToolbar() {
        MenuContainerRoot container = MenuManagerMainToolbar.getInstance().getMenuData();
        updateContextShortcuts(container);
        List<MenuItemData> list = container.getItems();
        this.setLayout(new MigLayout("ins 0 3 0 0", "[]", "[grow,32!]"));
        AbstractButton ab;
        // System.out.println(this.getColConstraints(list.length));
        MenuItemData last = null;
        for (final MenuItemData menudata : list) {
            AbstractButton bt = null;
            CustomizableAppAction action;
            try {
                if (!menudata.isVisible()) {
                    continue;
                }
                if (menudata instanceof SeparatorData) {
                    if (last != null && last instanceof SeparatorData) {
                        // no separator dupes
                        continue;
                    }
                    this.add(new JSeparator(SwingConstants.VERTICAL), "gapleft 10,gapright 10,width 2!,pushy,growy");
                    last = menudata;
                    continue;
                }
                if (menudata._getValidateException() != null) {
                    continue;
                }
                if (menudata.getType() == org.jdownloader.controlling.contextmenu.MenuItemData.Type.CONTAINER) {
                    bt = new ExtButton(new AppAction() {
                        private ExtPopupMenu root = null;
                        {
                            setTooltipText(menudata.getName());
                            setName(menudata.getName());
                            putValue(AbstractAction.LARGE_ICON_KEY, createDropdownImage(validateIconKey(menudata.getIconKey())));
                        }

                        @Override
                        public void actionPerformed(final ActionEvent e) {
                            ExtPopupMenu lroot = root;
                            if (lroot != null && lroot.isShowing()) {
                                return;
                            }
                            Object src = e.getSource();
                            if (e.getSource() instanceof Component) {
                                lroot = new ExtPopupMenu();
                                final ExtPopupMenu finalMenu = lroot;
                                final MouseListener ml = new MouseListener() {
                                    private Timer timer;
                                    {
                                        final MouseListener ml = this;
                                        timer = new Timer(1000, new ActionListener() {
                                            @Override
                                            public void actionPerformed(ActionEvent e2) {
                                                finalMenu.setVisible(false);
                                                ((JComponent) e.getSource()).removeMouseListener(ml);
                                            }
                                        });
                                        timer.setRepeats(false);
                                    }

                                    @Override
                                    public void mouseClicked(MouseEvent e) {
                                    }

                                    @Override
                                    public void mousePressed(MouseEvent e) {
                                    }

                                    @Override
                                    public void mouseReleased(MouseEvent e) {
                                    }

                                    @Override
                                    public void mouseEntered(MouseEvent e) {
                                        timer.stop();
                                    }

                                    @Override
                                    public void mouseExited(MouseEvent e) {
                                        timer.stop();
                                        timer.start();
                                    }
                                };
                                ((JComponent) e.getSource()).addMouseListener(ml);
                                new MenuBuilder(MenuManagerMainToolbar.getInstance(), lroot, (MenuContainer) menudata) {
                                    @Override
                                    protected void addContainer(JComponent root, MenuItemData inst, int index, int size) throws InstantiationException, IllegalAccessException, InvocationTargetException, ClassNotFoundException, NoSuchMethodException, ExtensionNotLoadedException {
                                        final JMenu submenu = (JMenu) inst.addTo(root, this);
                                        if (submenu == null) {
                                            return;
                                        }
                                        submenu.addMouseListener(ml);
                                        createLayer(submenu, (MenuContainer) inst);
                                        if (submenu.getMenuComponentCount() == 0) {
                                            root.remove(submenu);
                                        }
                                    }

                                    @Override
                                    protected void addAction(JComponent root, MenuItemData inst, int index, int size) throws InstantiationException, IllegalAccessException, InvocationTargetException, ClassNotFoundException, NoSuchMethodException, ExtensionNotLoadedException {
                                        JComponent ret = inst.addTo(root, this);
                                        if (ret != null) {
                                            ret.addMouseListener(ml);
                                        }
                                    }
                                }.run();
                                Component button = (Component) e.getSource();
                                Dimension prefSize = lroot.getPreferredSize();
                                Insets insets = LAFOptions.getInstance().getExtension().customizePopupBorderInsets();
                                root = lroot;
                                lroot.show(button, -insets.left, button.getHeight() - insets.top);
                            }
                        }
                    });
                    last = menudata;
                    final AbstractButton finalBt = bt;
                    bt.addMouseListener(new MouseListener() {
                        private Timer timer;

                        @Override
                        public void mouseReleased(MouseEvent e) {
                            Timer ltimer = timer;
                            timer = null;
                            if (ltimer != null) {
                                ltimer.stop();
                            }
                        }

                        @Override
                        public void mousePressed(MouseEvent e) {
                            Timer ltimer = timer;
                            timer = null;
                            if (ltimer != null) {
                                ltimer.stop();
                            }
                        }

                        @Override
                        public void mouseExited(MouseEvent e) {
                            Timer ltimer = timer;
                            timer = null;
                            if (ltimer != null) {
                                ltimer.stop();
                            }
                        }

                        @Override
                        public void mouseEntered(MouseEvent e) {
                            Timer ltimer = timer;
                            timer = null;
                            if (ltimer != null) {
                                ltimer.stop();
                            }
                            ltimer = new Timer(200, new ActionListener() {
                                @Override
                                public void actionPerformed(ActionEvent e) {
                                    e.setSource(finalBt);
                                    finalBt.getAction().actionPerformed(e);
                                }
                            });
                            ltimer.setRepeats(false);
                            ltimer.start();
                            timer = ltimer;
                        }

                        @Override
                        public void mouseClicked(MouseEvent e) {
                            Timer ltimer = timer;
                            timer = null;
                            if (ltimer != null) {
                                ltimer.stop();
                            }
                        }
                    });
                    add(bt, "width 32!,height 32!,hidemode 3");
                    bt.setHideActionText(true);
                    continue;
                } else if (menudata instanceof MenuLink) {
                    final JComponent item = menudata.createItem(null);
                    if (StringUtils.isNotEmpty(menudata.getIconKey())) {
                        if (item instanceof AbstractButton) {
                            ((AbstractButton) item).setIcon(NewTheme.I().getIcon(validateIconKey(menudata.getIconKey()), 24));
                        } else if (item instanceof SetIconInterface) {
                            ((SetIconInterface) item).setIcon(NewTheme.I().getIcon(validateIconKey(menudata.getIconKey()), 24));
                        }
                    }
                    if (StringUtils.isNotEmpty(menudata.getName())) {
                        if (item instanceof SetLabelInterface) {
                            ((SetLabelInterface) item).setText(menudata.getName());
                        }
                    }
                    if (item instanceof JMenu) {
                        bt = new ExtButton(new AppAction() {
                            {
                                setName(((JMenu) item).getText());
                                Icon ico = ((JMenu) item).getIcon();
                                if (ico == null || Math.max(ico.getIconHeight(), ico.getIconWidth()) < 24) {
                                    ico = createDropdownImage("menu");
                                    putValue(AbstractAction.LARGE_ICON_KEY, ico);
                                    setSmallIcon(ico);
                                } else if (ico instanceof ImageIcon) {
                                    if (Math.max(ico.getIconHeight(), ico.getIconWidth()) != 24) {
                                        ico = ImageProvider.scaleImageIcon((ImageIcon) ico, 24, 24);
                                    }
                                    ico = createDropdownImage(((ImageIcon) ico).getImage());
                                    putValue(AbstractAction.LARGE_ICON_KEY, ico);
                                    setSmallIcon(ico);
                                } else {
                                    putValue(AbstractAction.LARGE_ICON_KEY, ico);
                                    setSmallIcon(ico);
                                }
                            }

                            @Override
                            public void actionPerformed(ActionEvent e) {
                                ExtPopupMenu root = new ExtPopupMenu();
                                for (Component c : ((JMenu) item).getMenuComponents()) {
                                    root.add(c);
                                }
                                Object src = e.getSource();
                                if (e.getSource() instanceof Component) {
                                    Component button = (Component) e.getSource();
                                    Dimension prefSize = root.getPreferredSize();
                                    Insets insets = LAFOptions.getInstance().getExtension().customizePopupBorderInsets();
                                    root.show(button, -insets.left, button.getHeight() - insets.top);
                                }
                            }
                        });
                        add(bt, "width 32!,height 32!,hidemode 3");
                        bt.setHideActionText(true);
                    } else {
                        add(item, "aligny center,hidemode 3");
                    }
                } else if (menudata.getActionData() != null && menudata.getActionData()._isValidDataForCreatingAnAction()) {
                    action = menudata.createAction();
                    // if (StringUtils.isNotEmpty(menudata.getShortcut()) && KeyStroke.getKeyStroke(menudata.getShortcut()) != null) {
                    // action.setAccelerator(KeyStroke.getKeyStroke(menudata.getShortcut()));
                    // } else if (MenuItemData.isEmptyValue(menudata.getShortcut())) {
                    // action.setAccelerator(null);
                    // }
                    bt = null;
                    if (action instanceof AbstractToolBarAction) {
                        action.requestUpdate(MainToolBar.this);
                        bt = ((AbstractToolBarAction) action).createButton();
                    }
                    if (bt == null) {
                        if (action.isToggle()) {
                            action.requestUpdate(MainToolBar.this);
                            bt = new ExtJToggleButton(action);
                            Icon icon;
                            final String iconKey = validateIconKey(action.getIconKey());
                            bt.setIcon(icon = NewTheme.I().getCheckBoxImage(iconKey, false, 24));
                            bt.setRolloverIcon(icon);
                            bt.setSelectedIcon(icon = NewTheme.I().getCheckBoxImage(iconKey, true, 24));
                            bt.setRolloverSelectedIcon(icon);
                        } else {
                            action.requestUpdate(MainToolBar.this);
                            bt = new ExtButton(action);
                            bt.setIcon(NewTheme.I().getIcon(validateIconKey(action.getIconKey()), 24));
                        }
                        bt.setHideActionText(true);
                    }
                    add(bt, "width 32!,height 32!,hidemode 3");
                    action.addVisibilityPropertyChangeListener(bt);
                    bt.setVisible(action.isVisible());
                    last = menudata;
                    final Object value = action.getValue(Action.ACCELERATOR_KEY);
                    if (value == null) {
                        continue;
                    }
                    final KeyStroke ks = (KeyStroke) value;
                    this.rootpane.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(ks, action);
                    this.rootpane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(ks, action);
                    this.rootpane.getActionMap().put(action, action);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        add(Box.createHorizontalGlue(), "pushx,growx");
    }

    public String validateIconKey(String key) {
        if (StringUtils.isEmpty(key) || !NewTheme.I().hasIcon(key)) {
            return IconKey.ICON_QUESTION;
        }
        return key;
    }

    protected ImageIcon createDropdownImage(String iconKey) {
        if (MenuItemData.isEmptyValue(iconKey)) {
            iconKey = IconKey.ICON_QUESTION;
        }
        Image back = NewTheme.I().getImage(iconKey, 20);
        return createDropdownImage(back);
    }

    protected ImageIcon createDropdownImage(Image back) {
        Image checkBox = NewTheme.I().getImage("popDownSmall", -1);
        back = ImageProvider.merge(back, checkBox, 0, 0, 24 - checkBox.getWidth(null), 24 - checkBox.getHeight(null));
        return new ImageIcon(back);
    }

    protected void updateSpecial() {
        if (speedmeter != null && CFG_GUI.SPEED_METER_VISIBLE.isEnabled()) {
            add(speedmeter, "width 32:300:300,pushy,growy");
        }
    }

    // made speedMeter Instance public. Used in remote API
    public SpeedMeterPanel getSpeedMeter() {
        return speedmeter;
    }

    @Override
    public void mouseClicked(MouseEvent e) {
    }

    @Override
    public void mousePressed(MouseEvent e) {
    }

    @Override
    public void mouseReleased(MouseEvent e) {
    }

    @Override
    public void mouseEntered(MouseEvent e) {
    }

    @Override
    public void mouseExited(MouseEvent e) {
    }

    @Override
    public void onDownloadWatchdogDataUpdate() {
    }

    @Override
    public void onDownloadWatchdogStateIsIdle() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                if (speedmeter != null) {
                    speedmeter.stop();
                }
            }
        };
    }

    @Override
    public void onDownloadWatchdogStateIsPause() {
    }

    @Override
    public void onDownloadWatchdogStateIsRunning() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                if (speedmeter != null) {
                    speedmeter.start();
                }
            }
        };
    }

    @Override
    public void onDownloadWatchdogStateIsStopped() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                if (speedmeter != null) {
                    speedmeter.stop();
                }
            }
        };
    }

    @Override
    public void onDownloadWatchdogStateIsStopping() {
    }

    @Override
    public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
    }

    @Override
    public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
        DownloadWatchDog.getInstance().enqueueJob(new DownloadWatchDogJob() {
            @Override
            public void execute(DownloadSession currentSession) {
                final boolean running = DownloadWatchDog.getInstance().isRunning();
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        if (CFG_GUI.SPEED_METER_VISIBLE.isEnabled()) {
                            if (speedmeter != null && running) {
                                speedmeter.start();
                            }
                        } else {
                            if (speedmeter != null) {
                                speedmeter.stop();
                            }
                        }
                    }
                };
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

    @Override
    public void onDownloadControllerStart(SingleDownloadController downloadController, DownloadLinkCandidate candidate) {
    }

    @Override
    public void onDownloadControllerStopped(SingleDownloadController downloadController, DownloadLinkCandidate candidate, DownloadLinkCandidateResult result) {
    }

    @Override
    public void onDownloadWatchDogPropertyChange(DownloadWatchDogProperty propertyChange) {
    }
}