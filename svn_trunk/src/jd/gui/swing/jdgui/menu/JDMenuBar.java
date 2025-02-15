package jd.gui.swing.jdgui.menu;

import java.awt.Component;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.lang.reflect.InvocationTargetException;

import javax.swing.AbstractButton;
import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.KeyStroke;

import jd.SecondLevelLaunch;

import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.controlling.contextmenu.MenuContainer;
import org.jdownloader.controlling.contextmenu.MenuItemData;
import org.jdownloader.controlling.contextmenu.gui.MenuBuilder;
import org.jdownloader.extensions.ExtensionNotLoadedException;
import org.jdownloader.gui.mainmenu.DonateAction;
import org.jdownloader.gui.mainmenu.MenuManagerMainmenu;
import org.jdownloader.gui.views.downloads.bottombar.HorizontalBoxItem;
import org.jdownloader.images.NewTheme;
import org.jdownloader.logging.LogController;
import org.jdownloader.updatev2.gui.LAFOptions;

public class JDMenuBar extends JMenuBar implements MouseListener {
    private static final JDMenuBar INSTANCE = new JDMenuBar();

    /**
     * get the only existing instance of JDMenuBar. This is a singleton
     *
     * @return
     */
    public static JDMenuBar getInstance() {
        return JDMenuBar.INSTANCE;
    }

    private static final long serialVersionUID = 6758718947311901334L;
    private LogSource         logger;

    /**
     * Create a new instance of JDMenuBar. This is a singleton class. Access the only existing instance by using {@link #getInstance()}.
     */
    private JDMenuBar() {
        super();
        SecondLevelLaunch.EXTENSIONS_LOADED.executeWhenReached(new Runnable() {

            @Override
            public void run() {
                new EDTRunner() {

                    @Override
                    protected void runInEDT() {
                        updateLayout();
                    }
                };
            }
        });

        // MenuManagerMainmenu.getInstance().refresh();

        this.addMouseListener(this);
        logger = LogController.getInstance().getLogger(JDMenuBar.class.getName());
        LAFOptions.getInstance().getExtension().customizeMenuBar(this);
        // add(new JMenu("Menu is loading...."));

    }

    // @Override
    // public Dimension getPreferredSize() {
    // Dimension ret = super.getPreferredSize();
    // // ret.height = 24;
    // return ret;
    // }

    protected void addImpl(Component comp, Object constraints, int index) {
        super.addImpl(comp, constraints, index);
        comp.addMouseListener(this);
    }

    public void updateLayout() {

        removeAll();

        new MenuBuilder(MenuManagerMainmenu.getInstance(), this, MenuManagerMainmenu.getInstance().getMenuData()) {

            @Override
            protected void addContainer(JComponent root, MenuItemData inst, int index, int size) throws InstantiationException, IllegalAccessException, InvocationTargetException, ClassNotFoundException, NoSuchMethodException, ExtensionNotLoadedException {
                if (!inst.isVisible()) {
                    return;
                }
                final JMenu submenu = (JMenu) inst.addTo(root, this);

                if (submenu != null) {
                    applyMnemonic(root, submenu);
                    if (root == JDMenuBar.this) {
                        submenu.setIcon(null);
                    }
                    createLayer(submenu, (MenuContainer) inst);
                }
            }

            @Override
            protected void addAction(JComponent root, MenuItemData inst, int index, int size) throws InstantiationException, IllegalAccessException, InvocationTargetException, ClassNotFoundException, NoSuchMethodException, ExtensionNotLoadedException {

                if (!inst.isVisible()) {
                    return;
                }
                try {
                    if (root instanceof JDMenuBar && inst instanceof HorizontalBoxItem) {
                        ((JDMenuBar) root).add(Box.createHorizontalGlue());
                        return;
                    }
                } catch (Throwable e) {
                    logger.log(e);
                }
                if (root instanceof JMenu) {
                    JComponent comp = inst.addTo(root, this);
                    if (comp == null) {
                        return;
                    }
                    if (comp instanceof AbstractButton) {
                        applyMnemonic(root, (AbstractButton) comp);
                    }
                } else {

                    // WAHHHHHH this is an ugly gui hack!
                    // this peace of code loads the synthetica MenuPainter given in the LAFOptions and paints a JMenu Mouseover Background
                    // to a JButton or a jToggleBUtton
                    CustomizableAppAction action = inst.createAction();

                    if (StringUtils.isNotEmpty(inst.getShortcut()) && KeyStroke.getKeyStroke(inst.getShortcut()) != null) {
                        action.setAccelerator(KeyStroke.getKeyStroke(inst.getShortcut()));
                    } else if (MenuItemData.isEmptyValue(inst.getShortcut())) {
                        action.setAccelerator(null);
                    }

                    AbstractButton ret;
                    if (action.isToggle()) {
                        action.requestUpdate(JDMenuBar.this);
                        ret = new MenuJToggleButton(action);
                        Icon icon;

                        ret.setIcon(icon = NewTheme.I().getCheckBoxImage(action.getIconKey(), false, 18));
                        ret.setRolloverIcon(icon);
                        ret.setSelectedIcon(icon = NewTheme.I().getCheckBoxImage(action.getIconKey(), true, 18));
                        ret.setRolloverSelectedIcon(icon);

                    } else {
                        action.requestUpdate(JDMenuBar.this);

                        if (action instanceof DonateAction && index == size - 1 && !CrossSystem.isMac()) {// only if the donation button is
                            // the very right in the
                            // menu bar
                            ret = new ExtDonateMenuButton(action);
                        } else {
                            ret = new ExtMenuButton(action);
                        }
                        ret.setIcon(NewTheme.I().getIcon(action.getIconKey(), 18));

                    }
                    // JMenuItem
                    // ret.setBorderPainted(false);
                    final AbstractButton bt = ret;
                    action.addVisibilityPropertyChangeListener(bt);
                    bt.setBorderPainted(false);
                    bt.setOpaque(false);

                    // final Object value = action.getValue(Action.ACCELERATOR_KEY);
                    // if (value == null) {
                    // continue;
                    // }
                    // final KeyStroke ks = (KeyStroke) value;
                    //
                    // this.rootpane.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(ks, action);
                    // this.rootpane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(ks, action);
                    // this.rootpane.getActionMap().put(action, action);
                    //
                    // final String shortCut = action.getShortCutString();
                    // if (bt != null) {
                    // if (StringUtils.isEmpty(action.getTooltipText())) {
                    // ret.setToolTipText(shortCut != null ? " [" + shortCut + "]" : null);
                    // } else {
                    // ret.setToolTipText(action.getTooltipText() + (shortCut != null ? " [" + shortCut + "]" : ""));
                    // }
                    //
                    // }

                    ret.getAccessibleContext().setAccessibleName(action.getName());
                    ret.getAccessibleContext().setAccessibleDescription(action.getTooltipText());
                    if (StringUtils.isNotEmpty(inst.getName())) {
                        ret.setText(inst.getName());
                    }

                    ret.setIcon(MenuItemData.getIcon(inst.getIconKey(), 20));

                    if (ret instanceof AbstractButton) {
                        applyMnemonic(root, ret);
                    }

                    root.add(ret);
                }
            }

        }.run();

        if (getMenuCount() == 0) {

            new MenuBuilder(MenuManagerMainmenu.getInstance(), this, MenuManagerMainmenu.getInstance().createDefaultStructure()) {

                @Override
                protected void addContainer(JComponent root, MenuItemData inst, int index, int size) throws InstantiationException, IllegalAccessException, InvocationTargetException, ClassNotFoundException, NoSuchMethodException, ExtensionNotLoadedException {
                    final JMenu submenu = (JMenu) inst.addTo(root, this);
                    if (root == JDMenuBar.this) {
                        submenu.setIcon(null);
                    }
                    createLayer(submenu, (MenuContainer) inst);
                }

                @Override
                protected void addAction(JComponent root, MenuItemData inst, int index, int size) throws InstantiationException, IllegalAccessException, InvocationTargetException, ClassNotFoundException, NoSuchMethodException, ExtensionNotLoadedException {
                    inst.addTo(root, this);
                }

            }.run();
        }
        ;
        // MenuContainerRoot dret.nMenuManager.getInstance().getMenuData();
        // for(MenuItemData mid:dat.getItems()){
        // if(!mid.showItem(null)) continue;
        repaint();

    }

    @Override
    public void mouseClicked(MouseEvent e) {
    }

    @Override
    public void mousePressed(MouseEvent e) {
        if (e.isPopupTrigger() || e.getButton() == 3) {

            MenuManagerMainmenu.getInstance().openGui();
        }
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

}
