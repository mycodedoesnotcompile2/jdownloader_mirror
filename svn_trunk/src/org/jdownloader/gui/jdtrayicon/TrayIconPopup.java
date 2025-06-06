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
package org.jdownloader.gui.jdtrayicon;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dialog.ModalExclusionType;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.lang.reflect.InvocationTargetException;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JToggleButton;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.Timer;

import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.components.toolbar.actions.ExitToolbarAction;
import jd.utils.JDUtilities;
import net.miginfocom.swing.MigLayout;

import org.appwork.swing.ExtJFrame;
import org.appwork.swing.components.ExtJToggleButton;
import org.appwork.swing.components.tooltips.ToolTipController;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTHelper;
import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.controlling.contextmenu.MenuContainer;
import org.jdownloader.controlling.contextmenu.MenuItemData;
import org.jdownloader.controlling.contextmenu.MenuLink;
import org.jdownloader.controlling.contextmenu.SeparatorData;
import org.jdownloader.controlling.contextmenu.gui.ExtPopupMenu;
import org.jdownloader.controlling.contextmenu.gui.MenuBuilder;
import org.jdownloader.extensions.ExtensionNotLoadedException;
import org.jdownloader.gui.toolbar.MenuManagerMainToolbar;
import org.jdownloader.images.NewTheme;
import org.jdownloader.updatev2.gui.LAFOptions;

//final, because the constructor calls Thread.start(),
//see http://findbugs.sourceforge.net/bugDescriptions.html#SC_START_IN_CTOR
public final class TrayIconPopup extends ExtJFrame implements MouseListener {
    private static final long serialVersionUID  = 2623190748929934409L;
    // private JPanel entryPanel;
    // private JPanel quickConfigPanel;
    // private JPanel bottomPanel;
    private boolean           enteredPopup;
    private boolean           hideThreadrunning = false;
    // private JPanel exitPanel;
    // private java.util.List<AbstractButton> resizecomps;
    private transient Thread  hideThread;
    private TrayExtension     extension;
    private static final int  ICON_SIZE         = 20;
    private long              visibleUntil      = -1;

    public void setVisible(boolean b) {
        if (isVisible() && !b) {
            visibleUntil = System.currentTimeMillis();
        }
        super.setVisible(b);
    }

    public void dispose() {
        visibleUntil = System.currentTimeMillis();
        super.dispose();
    }

    public TrayIconPopup(TrayExtension trayExtension) {
        super();
        setTitle("JDownloader");
        setModalExclusionType(ModalExclusionType.TOOLKIT_EXCLUDE);
        this.extension = trayExtension;
        // resizecomps = new ArrayList<AbstractButton>();
        setVisible(false);
        setLayout(new MigLayout("ins 0", "[grow,fill]", "[grow,fill]"));
        addMouseListener(this);
        this.setUndecorated(true);
        // initEntryPanel();
        // initQuickConfigPanel();
        // initBottomPanel();
        // initExitPanel();
        JPanel content = new JPanel(new MigLayout("ins 5, wrap 1", "[fill]", "[]5[]"));
        add(content);
        JButton header;
        content.add(header = new JButton("<html><b>" + JDUtilities.getJDTitle(0) + "</b></html>"), "align center");
        header.setBorderPainted(false);
        header.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                JDGui.getInstance().setWindowToTray(false);
                dispose();
            }
        });
        AbstractButton ab;
        // System.out.println(this.getColConstraints(list.length));
        MenuItemData last = null;
        for (final MenuItemData menudata : MenuManagerTrayIcon.getInstance().getMenuData().getItems()) {
            AbstractButton bt = null;
            AppAction action;
            try {
                if (!menudata.isVisible()) {
                    continue;
                }
                if (menudata instanceof SeparatorData) {
                    if (last != null && last instanceof SeparatorData) {
                        // no separator dupes
                        continue;
                    }
                    content.add(new JSeparator(SwingConstants.HORIZONTAL), "growx,spanx");
                    last = menudata;
                    continue;
                }
                if (menudata._getValidateException() != null) {
                    continue;
                }
                if (menudata.getType() == org.jdownloader.controlling.contextmenu.MenuItemData.Type.CONTAINER) {
                    bt = new JToggleButton() {
                        protected void paintComponent(Graphics g) {
                            super.paintComponent(g);
                            Graphics2D g2 = (Graphics2D) g;
                            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
                            g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
                            g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.8f));
                            g2.setColor(Color.BLACK);
                            g2.fillPolygon(new int[] { getWidth() - 5, getWidth() - 5 - 6, getWidth() - 5 - 6 }, new int[] { getHeight() / 2, getHeight() / 2 - 4, getHeight() / 2 + 4 }, 3);
                        }
                    };
                    bt.setText(menudata.getName());
                    bt.setOpaque(false);
                    bt.setContentAreaFilled(false);
                    bt.setBorderPainted(false);
                    bt.addActionListener(new ActionListener() {
                        private ExtPopupMenu root = null;

                        public void actionPerformed(ActionEvent e) {
                            hideThreadrunning = false;
                            if (root != null && root.isShowing()) {
                                return;
                            }
                            root = new ExtPopupMenu();
                            new MenuBuilder(MenuManagerMainToolbar.getInstance(), root, (MenuContainer) menudata) {
                                protected void addAction(final JComponent root, final MenuItemData inst, int index, int size) throws InstantiationException, IllegalAccessException, InvocationTargetException, ClassNotFoundException, NoSuchMethodException, ExtensionNotLoadedException {
                                    final JComponent ret = inst.addTo(root, this);
                                    if (ret instanceof AbstractButton) {
                                        ((AbstractButton) ret).addActionListener(new ActionListener() {
                                            public void actionPerformed(ActionEvent evt) {
                                                ((AbstractButton) ret).getAction().actionPerformed(evt);
                                                TrayIconPopup.this.dispose();
                                            }
                                        });
                                    }
                                }
                            }.run();
                            Object src = e.getSource();
                            if (e.getSource() instanceof Component) {
                                Component button = (Component) e.getSource();
                                Dimension prefSize = root.getPreferredSize();
                                Insets insets = LAFOptions.getInstance().getExtension().customizePopupBorderInsets();
                                root.show(button, button.getWidth(), -insets.top);
                            }
                        }
                    });
                    bt.setIcon(MenuItemData.getIcon(menudata.getIconKey(), ICON_SIZE));
                    bt.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
                    bt.setFocusPainted(false);
                    bt.setHorizontalAlignment(JButton.LEFT);
                    bt.setIconTextGap(5);
                    bt.addMouseListener(new HoverEffect(bt));
                    final AbstractButton finalBt = bt;
                    bt.addMouseListener(new MouseListener() {
                        private Timer timer;

                        @Override
                        public void mouseReleased(MouseEvent e) {
                            if (timer != null) {
                                timer.stop();
                                timer = null;
                            }
                        }

                        @Override
                        public void mousePressed(MouseEvent e) {
                            if (timer != null) {
                                timer.stop();
                                timer = null;
                            }
                        }

                        @Override
                        public void mouseExited(MouseEvent e) {
                            if (timer != null) {
                                timer.stop();
                                timer = null;
                            }
                        }

                        @Override
                        public void mouseEntered(MouseEvent e) {
                            timer = new Timer(500, new ActionListener() {
                                @Override
                                public void actionPerformed(ActionEvent e) {
                                    finalBt.doClick();
                                }
                            });
                            timer.setRepeats(false);
                            timer.start();
                        }

                        @Override
                        public void mouseClicked(MouseEvent e) {
                            if (timer != null) {
                                timer.stop();
                                timer = null;
                            }
                        }
                    });
                    content.add(bt);
                    continue;
                } else if (menudata instanceof MenuLink) {
                    final JComponent item = menudata.createItem(null);
                    if (StringUtils.isNotEmpty(menudata.getIconKey())) {
                        if (item instanceof AbstractButton) {
                            ((AbstractButton) item).setIcon(MenuItemData.getIcon(menudata.getIconKey(), ICON_SIZE));
                        }
                    }
                    content.add(item, "");
                } else if (menudata.getActionData() != null && menudata.getActionData()._isValidDataForCreatingAnAction()) {
                    action = menudata.createAction();
                    if (!action.isVisible()) {
                        continue;
                    }
                    if (StringUtils.isNotEmpty(menudata.getShortcut()) && KeyStroke.getKeyStroke(menudata.getShortcut()) != null) {
                        action.setAccelerator(KeyStroke.getKeyStroke(menudata.getShortcut()));
                    } else if (MenuItemData.isEmptyValue(menudata.getShortcut())) {
                        action.setAccelerator(null);
                    }
                    content.add(getMenuEntry(action));
                    last = menudata;
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        // content.add(new JSeparator(), "growx, spanx");
        // content.add(entryPanel);
        // content.add(new JSeparator(), "growx, spanx");
        // content.add(quickConfigPanel);
        // content.add(new JSeparator(), "growx, spanx");
        // content.add(bottomPanel, "pushx,growx");
        // content.add(new JSeparator(), "growx, spanx");
        // content.add(exitPanel);
        //
        content.setBorder(BorderFactory.createLineBorder(content.getBackground().darker()));
        // Dimension size = new Dimension(getPreferredSize().width, resizecomps.get(0).getPreferredSize().height);
        // for (AbstractButton c : resizecomps) {
        // c.setPreferredSize(size);
        // c.setMinimumSize(size);
        // c.setMaximumSize(size);
        // }
        setAlwaysOnTop(true);
        pack();
        hideThread = new Thread() {
            /*
             * this thread handles closing of popup because enter/exit/move events are too slow and can miss the exitevent
             */
            public void run() {
                while (true && hideThreadrunning) {
                    try {
                        sleep(500);
                    } catch (InterruptedException e) {
                    }
                    if (enteredPopup && hideThreadrunning) {
                        final Point mouse = ToolTipController.getMouseLocation();
                        if (mouse == null) {
                            dispose();
                            break;
                        }
                        final Point current = TrayIconPopup.this.getLocation();
                        if (mouse.getLocation().x < current.x || mouse.getLocation().x > current.x + TrayIconPopup.this.getSize().width) {
                            dispose();
                            break;
                        } else if (mouse.getLocation().y < current.y || mouse.getLocation().y > current.y + TrayIconPopup.this.getSize().height) {
                            dispose();
                            break;
                        }
                    }
                }
            }
        };
        hideThreadrunning = true;
        hideThread.start();
    }

    /**
     * start autohide in 3 secs if mouse did not enter popup before
     */
    public void startAutoHide() {
        new Thread() {
            @Override
            public void run() {
                try {
                    Thread.sleep(3000);
                } catch (InterruptedException e) {
                }
                if (!enteredPopup) {
                    new EDTHelper<Object>() {
                        @Override
                        public Object edtRun() {
                            hideThreadrunning = false;
                            dispose();
                            return null;
                        }
                    }.start();
                }
            }
        }.start();
    }

    private AbstractButton getMenuEntry(AppAction action) {
        AbstractButton b = createButton(action);
        if (action instanceof ExitToolbarAction) {
            b.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    hideThreadrunning = false;
                    TrayIconPopup.this.dispose();
                }
            });
        }
        action.addVisibilityPropertyChangeListener(b);
        // resizecomps.add(b);
        return b;
    }

    private AbstractButton createButton(AppAction action) {
        if (action instanceof CustomizableAppAction) {
            ((CustomizableAppAction) action).requestUpdate(this);
        }
        if (action.isToggle()) {
            ExtJToggleButton bt = new ExtJToggleButton(action);
            bt.setOpaque(false);
            bt.setContentAreaFilled(false);
            bt.setBorderPainted(false);
            bt.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    hideThreadrunning = false;
                    TrayIconPopup.this.dispose();
                }
            });
            Icon icon;
            bt.setIcon(icon = NewTheme.I().getCheckBoxImage(action.getIconKey(), false, ICON_SIZE));
            bt.setRolloverIcon(icon);
            bt.setSelectedIcon(icon = NewTheme.I().getCheckBoxImage(action.getIconKey(), true, ICON_SIZE));
            bt.setRolloverSelectedIcon(icon);
            bt.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            bt.setFocusPainted(false);
            bt.setHorizontalAlignment(JButton.LEFT);
            bt.setIconTextGap(5);
            bt.addMouseListener(new HoverEffect(bt));
            return bt;
        } else {
            // we use a JToggleButton here, because JToggle buttons seem to have a different left icon gap the jbuttons
            ExtJToggleButton bt = new ExtJToggleButton(action);
            bt.setOpaque(false);
            bt.setContentAreaFilled(false);
            bt.setBorderPainted(false);
            bt.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    hideThreadrunning = false;
                    TrayIconPopup.this.dispose();
                }
            });
            Icon actionIcon = action.getIcon(ICON_SIZE);
            if (actionIcon != null) {
                bt.setIcon(actionIcon);
            }
            bt.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            bt.setFocusPainted(false);
            bt.setHorizontalAlignment(JButton.LEFT);
            bt.setIconTextGap(5);
            bt.addMouseListener(new HoverEffect(bt));
            return bt;
        }
    }

    public void mouseClicked(MouseEvent e) {
    }

    public void mouseEntered(MouseEvent e) {
        enteredPopup = true;
    }

    public void mouseExited(MouseEvent e) {
    }

    public void mousePressed(MouseEvent e) {
    }

    public void mouseReleased(MouseEvent e) {
    }

    /**
     * When we perform a tray action, we need a away to ask in the silentmode controller (JDGui) is the action has been invoked from the
     * tray
     *
     * @return
     */
    public boolean hasBeenRecentlyActive() {
        return isVisible() || System.currentTimeMillis() - visibleUntil < 10 * 1000;
    }
}