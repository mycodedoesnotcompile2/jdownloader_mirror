package org.jdownloader.controlling.contextmenu.gui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.lang.reflect.Constructor;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.TreeMap;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.Scrollable;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtButton;
import org.appwork.swing.components.ExtTextField;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.DebugMode;
import org.appwork.utils.FileHandler;
import org.appwork.utils.Files;
import org.appwork.utils.GetterSetter;
import org.appwork.utils.KeyUtils;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;
import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.ActionData;
import org.jdownloader.controlling.contextmenu.CustomSettingsPanelInterface;
import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.controlling.contextmenu.MenuContainer;
import org.jdownloader.controlling.contextmenu.MenuItemData;
import org.jdownloader.controlling.contextmenu.MenuLink;
import org.jdownloader.controlling.contextmenu.SeparatorData;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.config.Order;

import net.miginfocom.swing.MigLayout;

public class InfoPanel extends MigPanel implements ActionListener, Scrollable {
    public Dimension getPreferredScrollableViewportSize() {
        return this.getPreferredSize();
    }

    public int getScrollableBlockIncrement(final Rectangle visibleRect, final int orientation, final int direction) {
        return Math.max(visibleRect.height * 9 / 10, 1);
    }

    public boolean getScrollableTracksViewportHeight() {
        return false;
    }

    public boolean getScrollableTracksViewportWidth() {
        return true;
    }

    public int getScrollableUnitIncrement(final Rectangle visibleRect, final int orientation, final int direction) {
        return Math.max(visibleRect.height / 10, 1);
    }

    private JLabel            label;
    private MenuItemData      item;
    private ExtTextField      name;
    private ExtButton         iconChange;
    private MenuManagerDialog managerFrame;
    private JCheckBox         visibleBox;
    private ExtTextField      shortcut;
    private CustomPanel       customPanel;
    private JLabel            namelabel;
    private JButton           iconKeyReset;
    private JButton           nameReset;
    private JButton           shortCutReset;
    private JLabel            shortcutLabel;
    private JCheckBox         shortcutEnabled;

    public Dimension getPreferredSize() {
        Dimension ret = super.getPreferredSize();
        ret.width = Math.max(ret.width, 300);
        return ret;
        // return super.getPreferredSize();
    }

    public InfoPanel(MenuManagerDialog m) {
        super("ins 5,wrap 2", "[grow,fill][]", "[22!][]");
        this.managerFrame = m;
        label = SwingUtils.toBold(new JLabel());
        add(label);
        add(new JSeparator(), "spanx");
        add(SwingUtils.toBold(new JLabel(_GUI.T.InfoPanel_InfoPanel_properties_())), "spanx");
        // MenuItemProperty.HIDE_IF_DISABLED;
        // MenuItemProperty.HIDE_IF_OPENFILE_IS_UNSUPPORTED;
        // MenuItemProperty.HIDE_IF_OUTPUT_NOT_EXISTING;
        visibleBox = new JCheckBox();
        visibleBox.addActionListener(this);
        name = new ExtTextField() {
            @Override
            public void onChanged() {
                if (item == null) {
                    return;
                }
                item.setName(name.getText());
                updateResetButtons(item);
                updateHeaderLabel(item);
                managerFrame.fireUpdate();
            }
        };
        name.setHelpText(_GUI.T.InfoPanel_InfoPanel_customname_help());
        iconChange = new ExtButton(new AppAction() {
            {
                setName(_GUI.T.InfoPanel_changeicon());
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                final JPopupMenu p = new JPopupMenu();
                final File imagesDir = NewTheme.I().getImagesDirectory();
                final ArrayList<File> files = new ArrayList<File>();
                Files.internalWalkThroughStructure(new FileHandler<RuntimeException>() {
                    @Override
                    public void intro(File f) throws RuntimeException {
                    }

                    @Override
                    public boolean onFile(File f, int depths) throws RuntimeException {
                        final String name = f.getName().toLowerCase(Locale.ENGLISH);
                        if ("fav".equals(name) && f.isDirectory()) {
                            return false;
                        } else {
                            if (name.endsWith(".png") || (IconIO.getSvgFactory() != null && name.endsWith(".svg"))) {
                                files.add(f);
                            }
                            return true;
                        }
                    }

                    @Override
                    public void outro(File f) throws RuntimeException {
                    }
                }, imagesDir, 5);
                final JList list = new JList(files.toArray(new File[] {}));
                list.setLayoutOrientation(JList.VERTICAL_WRAP);
                list.setVisibleRowCount(30);
                final ListCellRenderer org = list.getCellRenderer();
                list.setCellRenderer(new ListCellRenderer() {
                    public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
                        File f = (File) value;
                        // String key = value.toString().substring(0, value.toString().length() - 4);
                        JLabel ret = (JLabel) org.getListCellRendererComponent(list, "", index, isSelected, cellHasFocus);
                        try {
                            ret.setIcon(IconIO.getIcon(f.toURI().toURL(), 20, 20));
                        } catch (MalformedURLException e) {
                            e.printStackTrace();
                        }
                        return ret;
                    }
                });
                list.setFixedCellHeight(24);
                list.setFixedCellWidth(24);
                list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                list.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
                    public void valueChanged(ListSelectionEvent e) {
                        File v = (File) list.getSelectedValue();
                        String rel = Files.getRelativePath(imagesDir, v);
                        rel = rel.substring(0, rel.length() - 4);
                        item.setIconKey(rel);
                        updateInfo(item);
                        p.setVisible(false);
                        managerFrame.fireUpdate();
                    }
                });
                // list.setMinimumSize(new Dimension(64, 64));
                p.setLayout(new MigLayout("ins 5", "[grow,fill]", "[grow,fill]"));
                p.add(list, "width 32:n:n");
                p.show(iconChange, 0, iconChange.getHeight());
            }
        });
        // icon=new JLabel(9)
        add(namelabel = label(_GUI.T.InfoPanel_InfoPanel_itemname()));
        add(name, "newline");
        add(nameReset = new JButton(new AppAction() {
            {
                setIconKey(IconKey.ICON_RESET);
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        // String newName = null;
                        // String oldName = item.getName();
                        // if (StringUtils.isNotEmpty(oldName)) {
                        // if (MenuItemData.isEmptyValue(oldName)) {
                        // newName = null;
                        // } else {
                        // newName = MenuItemData.EMPTY;
                        // }
                        // } else {
                        // newName = MenuItemData.EMPTY;
                        // }
                        name.setText(resetName);
                        item.setName(resetName);
                        updateInfo(item);
                        managerFrame.fireUpdate();
                    }
                };
            }
        }), "width 22!,height 22!");
        add(iconChange, "newline");
        add(iconKeyReset = new JButton(new AppAction() {
            {
                setIconKey(IconKey.ICON_RESET);
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        /*
                         * Two-state toggle: when the icon is at its default, resetIconKey is EMPTY and this removes the icon;
                         * when the icon is removed or custom, resetIconKey is the default and this restores it.
                         */
                        item.setIconKey(resetIconKey);
                        updateInfo(item);
                        managerFrame.fireUpdate();
                    }
                };
            }
        }), "width 22!,height 22!");
        shortcut = new ExtTextField();
        shortcut.setHelpText(_GUI.T.InfoPanel_InfoPanel_shortcuthelp2());
        shortcut.setEditable(false);
        shortcut.addKeyListener(new KeyListener() {
            @Override
            public void keyTyped(KeyEvent e) {
            }

            @Override
            public void keyReleased(KeyEvent e) {
            }

            @Override
            public void keyPressed(KeyEvent event) {
                String msg1 = KeyUtils.getShortcutString(event, true);
                KeyStroke currentShortcut = KeyStroke.getKeyStroke(event.getKeyCode(), event.getModifiersEx());
                shortcut.setText(msg1);
                /* Assigning a shortcut implies the hotkey is enabled (the field is only editable while the checkbox is ticked). */
                item.setShortcutDisabled(false);
                item.setShortcut(currentShortcut == null ? null : currentShortcut.toString());
                // managerFrame.repaint();
                updateResetButtons(item);
            }
        });
        if (managerFrame.getManager().isAcceleratorsEnabled()) {
            add(shortcutLabel = label(_GUI.T.InfoPanel_InfoPanel_shortcuts()), "hidemode 3");
            shortcutEnabled = new JCheckBox();
            shortcutEnabled.setToolTipText(_GUI.T.InfoPanel_shortcut_enabled_tooltip());
            shortcutEnabled.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    if (item == null) {
                        return;
                    }
                    final boolean on = shortcutEnabled.isSelected();
                    if (on) {
                        /*
                         * Enable the hotkey. The stored (custom) shortcut value is kept, so it survives a disable/enable cycle.
                         * Legacy data may have stored the EMPTY sentinel as "removed"; normalize that to inherit-default.
                         */
                        item.setShortcutDisabled(false);
                        if (MenuItemData.isEmptyValue(item.getShortcut())) {
                            item.setShortcut(null);
                        }
                    } else {
                        /* Disable the hotkey but keep the stored (custom) shortcut value. */
                        item.setShortcutDisabled(true);
                    }
                    /*
                     * Reflect the new state directly instead of calling updateInfo(), so that a "checked but not-yet-assigned"
                     * state on an action without a default hotkey is not immediately reverted to unchecked by the derivation.
                     * The field always shows the effective (custom or default) hotkey, greyed out while disabled.
                     */
                    shortcut.setEnabled(on);
                    final KeyStroke shown = getEffectiveAccelerator(item);
                    shortcut.setText(shown != null ? KeyUtils.getShortcutString(shown, true) : "");
                    updateResetButtons(item);
                    managerFrame.fireUpdate();
                }
            });
            add(shortcutEnabled, "newline,split 2,hidemode 3");
            add(shortcut, "growx,hidemode 3");
            add(shortCutReset = new JButton(new AppAction() {
                {
                    setIconKey(IconKey.ICON_RESET);
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            /*
                             * Reset to default restores the built-in hotkey (null = inherit) and the checkbox default (enabled);
                             * it never disables the hotkey.
                             */
                            item.setShortcut(null);
                            item.setShortcutDisabled(false);
                            updateInfo(item);
                            managerFrame.fireUpdate();
                        }
                    };
                }
            }), "width 22!,height 22!,hidemode 3");
        }
        add(label(_GUI.T.InfoPanel_InfoPanel_hidden_2()));
        add(visibleBox, "spanx");
        add(new JSeparator(), "spanx");
        customPanel = new CustomPanel(managerFrame);
        add(customPanel, "spanx,growx,pushx");
    }

    private String resetName;
    private String resetIconKey;

    private JLabel label(String infoPanel_InfoPanel_hideIfDisabled) {
        return new JLabel(infoPanel_InfoPanel_hideIfDisabled);
    }

    private class Entry {
        private MenuItemData  mid;
        private ActionContext so;
        private GetterSetter  gs;

        public Entry(MenuItemData mid, ActionContext so, GetterSetter gs) {
            this.mid = mid;
            this.so = so;
            this.gs = gs;
        }
    }

    public void updateResetButtons(final MenuItemData value) {
        if (value == null) {
            iconKeyReset.setEnabled(false);
            iconKeyReset.setToolTipText(null);
            nameReset.setEnabled(false);
            nameReset.setToolTipText(null);
            if (shortCutReset != null) {
                shortCutReset.setEnabled(false);
                shortCutReset.setToolTipText(null);
            }
        } else {
            try {
                if (value.getActionData() == null || !value.getActionData()._isValidDataForCreatingAnAction() || (value instanceof MenuLink)) {
                    return;
                }
                ActionData actionData = value.getActionData();
                Class<?> clazz = actionData._getClazz();
                Constructor<?> c = clazz.getConstructor(new Class[] {});
                CustomizableAppAction ret = (CustomizableAppAction) c.newInstance(new Object[] {});
                ret.setMenuItemData(value);
                // do not apply to get the defaults
                // ret.applyMenuItemData();
                ret.initContextDefaults();
                ret.loadContextSetups();
                iconKeyReset.setEnabled(true);
                nameReset.setEnabled(true);
                if (shortCutReset != null) {
                    shortCutReset.setEnabled(true);
                }
                if (StringUtils.equals(name.getText(), ret.getName())) {
                    resetName = MenuItemData.EMPTY;
                } else {
                    resetName = ret.getName();
                }
                resetIconKey = computeResetTarget(value.getIconKey(), ret.getIconKey());
                nameReset.setToolTipText(_GUI.T.ManagerFrame_layoutPanel_resettodefault_parametered(resetName));
                /*
                 * When the icon already equals the default (resetIconKey == EMPTY), the next click removes it: show a trash icon.
                 * Otherwise (removed or custom) show the "reset to default" icon.
                 */
                if (MenuItemData.isEmptyValue(resetIconKey)) {
                    iconKeyReset.setIcon(NewTheme.I().getIcon(IconKey.ICON_TRASH, 18));
                    iconKeyReset.setToolTipText(_GUI.T.InfoPanel_deleteicon_tooltip());
                } else {
                    iconKeyReset.setIcon(NewTheme.I().getIcon(IconKey.ICON_RESET, 18));
                    iconKeyReset.setToolTipText(_GUI.T.InfoPanel_reseticon_tooltip());
                }
                /* The shortcut reset only restores the built-in default (the checkbox handles disabling) and never removes it. */
                if (shortCutReset != null) {
                    shortCutReset.setIcon(NewTheme.I().getIcon(IconKey.ICON_RESET, 18));
                    shortCutReset.setToolTipText(_GUI.T.ManagerFrame_layoutPanel_resettodefault());
                }
            } catch (Throwable e) {
                LogController.CL().log(e);
            }
        }
    }

    /**
     * Computes the value the icon reset button applies on its next click and, implicitly, which icon it shows: returning
     * MenuItemData.EMPTY means the icon currently equals its default and the next click removes it (trash icon); returning the
     * default value means the icon is currently removed or customized and the next click restores the default (reset icon).
     * currentStored is the value stored on the menu item (null/empty when the default is inherited), defaultValue is the
     * built-in default of the action.
     */
    private String computeResetTarget(final String currentStored, final String defaultValue) {
        if (MenuItemData.isEmptyValue(currentStored)) {
            /* Currently removed by the user: offer to restore the default. */
            return defaultValue;
        }
        if (StringUtils.isEmpty(currentStored) || StringUtils.equals(currentStored, defaultValue)) {
            /* Implicitly (inherited) or explicitly at the default: offer to delete it. */
            return MenuItemData.EMPTY;
        }
        /* A custom value: offer to restore the default first. */
        return defaultValue;
    }

    /**
     * Reads the built-in default hotkey of the given menu item's action, independent of any custom/removed override stored on
     * the item. A fresh action instance without a menuItemData is used, so its accelerator is the one set in the action
     * constructor. Returns null when the action has no default hotkey.
     */
    private KeyStroke getDefaultAccelerator(final MenuItemData mid) {
        try {
            final Class<?> clazz = mid.getActionData()._getClazz();
            final Constructor<?> c = clazz.getConstructor(new Class[] {});
            final CustomizableAppAction fresh = (CustomizableAppAction) c.newInstance(new Object[] {});
            try {
                /* No menuItemData is set on purpose; some actions set their default accelerator here, others in the constructor. */
                fresh.initContextDefaults();
            } catch (Throwable ignore) {
            }
            final Object ks = fresh.getValue(Action.ACCELERATOR_KEY);
            if (ks instanceof KeyStroke) {
                return (KeyStroke) ks;
            }
        } catch (Throwable t) {
            LogController.CL().log(t);
        }
        return null;
    }

    /**
     * Returns the hotkey that currently applies to the given menu item: the custom hotkey if one is set, otherwise the built-in
     * default. Returns null when neither exists.
     */
    private KeyStroke getEffectiveAccelerator(final MenuItemData mid) {
        final String stored = mid.getShortcut();
        if (StringUtils.isNotEmpty(stored) && !MenuItemData.isEmptyValue(stored)) {
            return KeyStroke.getKeyStroke(stored);
        }
        return getDefaultAccelerator(mid);
    }

    /**
     * Updates the shortcut checkbox and field from the menu item state: the checkbox is ticked by default (even when no hotkey
     * is set, so the field is immediately editable to assign one with a single click) and only unticked when the user disabled
     * the hotkey (via the disabled flag or the legacy EMPTY value). The field shows the effective (custom or default) hotkey and
     * is greyed out together with the checkbox while disabled, so a custom hotkey stays visible and is preserved across a
     * disable/enable cycle.
     */
    private void updateShortcutControls(final MenuItemData mid) {
        if (shortcutEnabled == null || shortcut == null) {
            return;
        }
        /* isEmptyValue covers legacy data where "removed" was stored as the EMPTY sentinel instead of the disabled flag. */
        final boolean disabled = mid.isShortcutDisabled() || MenuItemData.isEmptyValue(mid.getShortcut());
        final KeyStroke shown = getEffectiveAccelerator(mid);
        final boolean checked = !disabled;
        shortcutEnabled.setSelected(checked);
        shortcut.setEnabled(checked);
        /* Always show the effective (custom or default) hotkey; it is greyed out together with the field while disabled. */
        shortcut.setText(shown != null ? KeyUtils.getShortcutString(shown, true) : "");
    }

    /**
     * Returns true when every customizable setting of the given menu item still equals its built-in default, so there is
     * nothing to reset. A fresh action instance without a menuItemData is used to read the defaults, because setup overrides
     * are only applied while a menuItemData is present (see CustomizableAppAction.fill).
     */
    private boolean areSettingsAtDefault(final MenuItemData mid, final ArrayList<Entry> entries) {
        try {
            final Class<?> clazz = mid.getActionData()._getClazz();
            final Constructor<?> c = clazz.getConstructor(new Class[] {});
            final CustomizableAppAction defaults = (CustomizableAppAction) c.newInstance(new Object[] {});
            try {
                /*
                 * No menuItemData is set on this instance on purpose, so the setup overrides are not applied and the setup
                 * objects keep their built-in defaults. Some actions compute defaults from a menuItemData though; in that case
                 * fall back to the plain constructor defaults instead of failing the whole comparison.
                 */
                defaults.initContextDefaults();
            } catch (Throwable ignore) {
            }
            final List<ActionContext> defaultSetups = defaults.getSetupObjects();
            if (defaultSetups == null) {
                return false;
            }
            for (final Entry e : entries) {
                final ActionContext defaultSetup = findSetupOfSameClass(defaultSetups, e.so);
                if (defaultSetup == null) {
                    continue;
                }
                final Object current = e.gs.get(e.so);
                final Object dflt = e.gs.get(defaultSetup);
                if (!isEqualValue(current, dflt)) {
                    return false;
                }
            }
            return true;
        } catch (Throwable t) {
            LogController.CL().log(t);
            /* On any error keep the button usable rather than hiding a working reset. */
            return false;
        }
    }

    private ActionContext findSetupOfSameClass(final List<ActionContext> setups, final ActionContext like) {
        if (setups != null) {
            for (final ActionContext setup : setups) {
                if (setup.getClass() == like.getClass()) {
                    return setup;
                }
            }
        }
        return null;
    }

    private boolean isEqualValue(final Object a, final Object b) {
        if (a == b) {
            return true;
        }
        if (a == null || b == null) {
            return false;
        }
        if (a.equals(b)) {
            return true;
        }
        /* Fallback for value types that do not implement equals (e.g. Modifier): compare their string forms. */
        return String.valueOf(a).equals(String.valueOf(b));
    }

    /**
     * @param lastPathComponent
     */
    public void updateInfo(final MenuItemData value) {
        // getParent().revalidate();
        // Component.revalidate is 1.7 only - that's why we have to cast
        JComponent p = (JComponent) getParent().getParent().getParent();
        updateResetButtons(value);
        p.revalidate();
        this.item = value;
        if (value == null) {
            label.setText("");
            return;
        }
        visibleBox.setSelected(value.isVisible());
        MenuItemData mid = (value);
        Rectangle bounds = null;
        String n = mid.getName();
        name.setText(n);
        // renderer.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, Color.RED));
        updateHeaderLabel(mid);
        customPanel.removeAll();
        CustomizableAppAction action = null;
        try {
            if (mid.getActionData() != null && mid.getActionData()._isValidDataForCreatingAnAction() && !(mid instanceof MenuLink)) {
                if (shortcutLabel != null) {
                    shortcutLabel.setVisible(true);
                }
                if (shortcut != null) {
                    shortcut.setVisible(true);
                }
                if (shortcutEnabled != null) {
                    shortcutEnabled.setVisible(true);
                }
                if (shortCutReset != null) {
                    shortCutReset.setVisible(true);
                }
                action = mid.createAction();
                name.setText(action.getName());
                if (StringUtils.isEmpty(action.getName())) {
                    name.setText(MenuItemData.EMPTY);
                }
                updateShortcutControls(mid);
                final List<ActionContext> sos = action.getSetupObjects();
                if (sos != null) {
                    final ArrayList<Entry> entries = new ArrayList<Entry>();
                    for (ActionContext so : sos) {
                        final ArrayList<GetterSetter> gss = new ArrayList<GetterSetter>(ReflectionUtils.getGettersSetteres(so.getClass()));
                        final ArrayList<Entry> toSort = new ArrayList<Entry>();
                        for (GetterSetter gs : gss) {
                            if (gs.hasGetter() && gs.hasSetter()) {
                                if (gs.hasAnnotation(Customizer.class)) {
                                    toSort.add(new Entry(mid, so, gs));
                                }
                            }
                        }
                        if (toSort.size() > 0) {
                            Collections.sort(toSort, new Comparator<Entry>() {
                                @Override
                                public int compare(Entry o1, Entry o2) {
                                    try {
                                        final Order orderAn1 = o1.gs.getAnnotation(Order.class);
                                        final Order orderAn2 = o2.gs.getAnnotation(Order.class);
                                        final int order1 = orderAn1 == null ? Integer.MAX_VALUE : orderAn1.value();
                                        final int order2 = orderAn2 == null ? Integer.MAX_VALUE : orderAn2.value();
                                        final int ret = CompareUtils.compareInt(order1, order2);
                                        if (ret != 0) {
                                            return ret;
                                        }
                                        final Customizer oc1 = o1.gs.getAnnotation(Customizer.class);
                                        final String lbl1;
                                        if (oc1 != null) {
                                            lbl1 = CustomPanel.getNameForCustomizer(o1.gs);
                                        } else {
                                            lbl1 = o1.gs.getKey();
                                        }
                                        final Customizer oc2 = o2.gs.getAnnotation(Customizer.class);
                                        final String lbl2;
                                        if (oc2 != null) {
                                            lbl2 = CustomPanel.getNameForCustomizer(o2.gs);
                                        } else {
                                            lbl2 = o2.gs.getKey();
                                        }
                                        return lbl1.compareToIgnoreCase(lbl2);
                                    } catch (Throwable e) {
                                        e.printStackTrace();
                                        return 0;
                                    }
                                }
                            });
                            entries.addAll(toSort);
                        }
                    }
                    if (entries.size() > 0 && DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                        /*
                         * Offer a button to reset all customizable action settings (the Customizer fields below) back to their
                         * built-in defaults. It only removes the setup overrides of the shown Customizer fields, so structural
                         * setups that are not exposed as Customizer stay untouched.
                         *
                         * TODO: The "at default" reference is currently the action class defaults, which is wrong for
                         * pre-configured menu variants (e.g. the "Delete All" entry of GenericDeleteFromDownloadlistAction, whose
                         * default is defined via a setup override, not the class default). Until this is measured against the
                         * built-in default menu structure instead, the button is only shown while running from the IDE.
                         */
                        final ActionData resetTarget = mid.getActionData();
                        final ArrayList<String> customizerKeys = new ArrayList<String>();
                        for (Entry e : entries) {
                            customizerKeys.add(e.gs.getKey());
                        }
                        final AppAction resetSettingsAction = new AppAction() {
                            {
                                setIconKey(IconKey.ICON_RESET);
                                setTooltipText(_GUI.T.InfoPanel_resetsettings_tooltip());
                            }

                            @Override
                            public void actionPerformed(ActionEvent ae) {
                                final TreeMap<String, Object> setup = resetTarget.getSetup();
                                if (setup != null) {
                                    for (final String key : customizerKeys) {
                                        setup.remove(StringUtils.toUpperCaseOrNull(key));
                                    }
                                }
                                updateInfo(item);
                                managerFrame.fireUpdate();
                            }
                        };
                        /* Grey the button out while every customizer field still equals its built-in default. */
                        resetSettingsAction.setEnabled(!areSettingsAtDefault(mid, entries));
                        final JButton resetSettings = new JButton(resetSettingsAction);
                        customPanel.add(new JLabel(_GUI.T.InfoPanel_resetsettings_tooltip()), "pushx,growx");
                        customPanel.add(resetSettings, "width 22!,height 22!,wrap");
                    }
                    for (Entry e : entries) {
                        customPanel.add(e.mid.getActionData(), action, e.so, e.gs);
                    }
                }
            } else {
                shortcut.setText("");
                shortcutLabel.setVisible(false);
                shortcut.setVisible(false);
                if (shortcutEnabled != null) {
                    shortcutEnabled.setVisible(false);
                }
                if (shortCutReset != null) {
                    shortCutReset.setVisible(false);
                }
            }
            if (mid instanceof CustomSettingsPanelInterface) {
                JComponent panel = ((CustomSettingsPanelInterface) mid).createSettingsPanel();
                if (panel != null) {
                    customPanel.add(panel, "pushx,growx");
                }
            } else if (action != null && action instanceof CustomSettingsPanelInterface) {
                JComponent panel = ((CustomSettingsPanelInterface) action).createSettingsPanel();
                if (panel != null) {
                    customPanel.add(panel, "pushx,growx");
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        revalidate();
    }

    public void updateHeaderLabel(MenuItemData mid) {
        String type = null;
        String name = mid.getName();
        Icon icon = null;
        if (mid.getIconKey() != null) {
            icon = (MenuItemData.getIcon(mid.getIconKey(), 20));
        }
        if (mid instanceof MenuContainer) {
            type = _GUI.T.InfoPanel_update_submenu();
            // label.setText(_GUI.T.InfoPanel_updateInfo_header_actionlabel(, ));
        } else if (mid instanceof SeparatorData) {
            name = _GUI.T.Renderer_getTreeCellRendererComponent_separator();
        } else {
            if (mid instanceof MenuLink) {
                type = _GUI.T.InfoPanel_update_link();
            } else {
                if (mid._isValidated()) {
                    try {
                        AppAction action = mid.createAction();
                        if (StringUtils.isEmpty(name)) {
                            name = action.getName();
                        }
                        type = _GUI.T.InfoPanel_update_action();
                        if (icon == null) {
                            icon = action.getSmallIcon();
                        }
                    } catch (Exception e) {
                    }
                }
                if (StringUtils.isEmpty(name)) {
                    name = mid.getActionData().getName();
                }
                if (icon == null) {
                    if (mid.getActionData().getIconKey() != null) {
                        icon = NewTheme.I().getIcon(mid.getActionData().getIconKey(), 18);
                    }
                }
                if (StringUtils.isEmpty(name)) {
                    name = mid.getActionData().getClazzName();
                    name = name.substring(name.lastIndexOf(".") + 1);
                }
                if (MenuItemData.isEmptyValue(mid.getIconKey())) {
                    icon = null;
                }
                if (MenuItemData.isEmptyValue(name)) {
                    name = mid.getActionData().getClazzName();
                    name = name.substring(name.lastIndexOf(".") + 1);
                    name += "(" + MenuItemData.EMPTY + ")";
                }
            }
        }
        if (StringUtils.isNotEmpty(type)) {
            label.setText(_GUI.T.InfoPanel_updateInfo_header_actionlabel(name, type));
        } else {
            label.setText(name);
        }
        label.setIcon(icon);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        item.setVisible(visibleBox.isSelected());
        managerFrame.repaint();
    }
}
