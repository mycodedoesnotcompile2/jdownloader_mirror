package org.jdownloader.controlling.contextmenu.gui;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyListener;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.Scrollable;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import jd.gui.swing.jdgui.JDGui;

import org.appwork.storage.config.annotations.EnumLabel;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.CheckBoxIcon;
import org.appwork.swing.components.ExtSpinner;
import org.appwork.swing.components.ExtTextField;
import org.appwork.utils.GetterSetter;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.reflection.Clazz;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.ActionData;
import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.controlling.contextmenu.Replacer;
import org.jdownloader.controlling.contextmenu.ReplacerProvider;
import org.jdownloader.controlling.contextmenu.Tag;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.components.PseudoCombo;
import org.jdownloader.gui.views.downloads.action.ByPassDialogSetup;
import org.jdownloader.gui.views.downloads.action.Modifier;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.staticreferences.CFG_GUI;

public class CustomPanel extends MigPanel {

    private MenuManagerDialog managerFrame;
    private LogSource         logger;

    public CustomPanel(MenuManagerDialog managerFrame) {
        super("ins 0", "[]", "[]");
        this.managerFrame = managerFrame;
        logger = managerFrame.getLogger();
        setOpaque(false);
    }

    public void add(final ActionData actionData, final CustomizableAppAction action, final ActionContext actionClass, final GetterSetter gs) {
        try {

            if (Clazz.isBoolean(gs.getType())) {
                JLabel lbl;
                add(lbl = new JLabel(getNameForCustomizer(gs)), "pushx");
                final JCheckBox jb = new JCheckBox();
                add(jb, "wrap,alignx right");
                try {
                    jb.setSelected((Boolean) actionData.fetchSetup(gs.getKey()));
                } catch (Exception e) {
                    try {
                        jb.setSelected((Boolean) gs.get(actionClass));
                    } catch (IllegalArgumentException e1) {
                        e1.printStackTrace();
                    } catch (IllegalAccessException e1) {
                        e1.printStackTrace();
                    } catch (InvocationTargetException e1) {
                        e1.printStackTrace();
                    }
                }
                jb.addActionListener(new ActionListener() {

                    @Override
                    public void actionPerformed(ActionEvent e) {
                        actionData.putSetup(gs.getKey(), jb.isSelected());
                        if (action instanceof CustomizableAppAction) {
                            action.loadContextSetups();
                        }
                        managerFrame.repaint();

                    }
                });
                if (gs.getKey().equals("BypassDialog") && gs.getGetter().getDeclaringClass().isAssignableFrom(ByPassDialogSetup.class) && CFG_GUI.CFG.isBypassAllRlyDeleteDialogsEnabled()) {
                    jb.setSelected(true);
                    jb.setEnabled(false);
                    lbl.setEnabled(false);
                }
            } else if (gs.getType() == Modifier.class) {
                final ExtTextField shortcut = new ExtTextField();
                shortcut.setHelpText(_GUI.T.InfoPanel_InfoPanel_shortcuthelp2());
                shortcut.setEditable(false);
                String value = (String) actionData.fetchSetup(gs.getKey());
                Modifier mod = null;

                if (value != null) {
                    mod = Modifier.create(value);
                }
                if (mod == null) {
                    mod = (Modifier) gs.get(actionClass);

                }
                if (mod != null) {

                    shortcut.setText(KeyEvent.getKeyModifiersText(mod.getKeyStroke().getModifiers()));
                }
                shortcut.addKeyListener(new KeyListener() {

                    @Override
                    public void keyTyped(KeyEvent e) {
                    }

                    @Override
                    public void keyReleased(KeyEvent e) {
                    }

                    @Override
                    public void keyPressed(KeyEvent event) {
                        KeyStroke ks = KeyStroke.getKeyStroke(event.getKeyCode(), event.getModifiersEx());

                        Modifier mod = Modifier.create(ks.toString());
                        System.out.println(ks + " - " + ks.getModifiers());
                        if (mod != null) {
                            shortcut.setText(KeyEvent.getKeyModifiersText(mod.getKeyStroke().getModifiers()));
                            actionData.putSetup(gs.getKey(), mod.toString());
                        } else {
                            shortcut.setText("");
                            actionData.putSetup(gs.getKey(), null);
                        }
                        if (action instanceof CustomizableAppAction) {
                            action.loadContextSetups();
                        }

                        managerFrame.repaint();
                    }

                });
                add(new JLabel(getNameForCustomizer(gs)), "pushx,growx");
                add(shortcut, "spanx,width 20:120:n");
            } else if (Clazz.isEnum(gs.getType())) {

                final Object[] values = ReflectionUtils.getEnumValues((Class<? extends Enum>) gs.getType());
                String value = (String) actionData.fetchSetup(gs.getKey());
                Object myValue = value == null ? null : ReflectionUtils.getEnumValueOf((Class<? extends Enum>) gs.getType(), value);

                if (value == null || myValue == null) {
                    myValue = gs.get(actionClass);

                    value = myValue.toString();
                }

                int index = 0;
                for (int i = 0; i < values.length; i++) {
                    if (values[i].toString().equals(value)) {
                        myValue = values[i];
                    }

                    if (myValue == values[i]) {
                        index = i;
                    }

                }
                final PseudoCombo cb = new PseudoCombo(values) {
                    protected boolean isHideSelf() {
                        return false;
                    }

                    protected javax.swing.Icon getIcon(Object v, boolean closed) {
                        if (closed) {
                            return null;
                        }
                        if (getSelectedItem() == v) {
                            return CheckBoxIcon.TRUE;
                        } else {
                            return CheckBoxIcon.FALSE;
                        }

                    };

                    public void onChanged(Object newValue) {
                        actionData.putSetup(gs.getKey(), getSelectedItem().toString());
                        if (action instanceof CustomizableAppAction) {
                            action.loadContextSetups();
                        }
                        managerFrame.repaint();
                    };

                    protected javax.swing.Icon getPopIcon(boolean closed) {

                        if (closed) {
                            if (isPopDown()) {
                                return NewTheme.I().getIcon(IconKey.ICON_POPDOWNLARGE, -1);
                            } else {
                                return NewTheme.I().getIcon(IconKey.ICON_POPUPLARGE, -1);
                            }
                        } else {
                            if (isPopDown()) {
                                return NewTheme.I().getIcon(IconKey.ICON_POPUPLARGE, -1);
                            } else {
                                return NewTheme.I().getIcon(IconKey.ICON_POPDOWNLARGE, -1);

                            }
                        }
                    };

                    protected String getLabel(Object v, boolean closed) {
                        String ret = null;
                        EnumLabel lbl;
                        try {
                            lbl = ((Class) gs.getType()).getDeclaredField(v.toString()).getAnnotation(EnumLabel.class);

                            if (lbl != null) {
                                ret = lbl.value();
                            } else if (v instanceof LabelInterface) {
                                ret = ((LabelInterface) v).getLabel();

                            }
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                        if (StringUtils.isEmpty(ret)) {
                            ret = v.toString();
                        }
                        if (closed && ret.length() > 50) {
                            ret = ret.substring(0, 50) + "...";
                        }
                        return ret;

                    };
                };
                cb.setSelectedItem(values[index]);
                add(new JLabel(getNameForCustomizer(gs)), "growx,spanx,wrap");
                add(cb, "growx,spanx,wrap");
            } else if (Clazz.isString(gs.getType())) {
                String value = StringUtils.valueOfOrNull(actionData.fetchSetup(gs.getKey()));
                if (value == null) {
                    value = (String) gs.get(actionClass);
                }
                final ExtTextField cb = new ExtTextField() {

                    @Override
                    public void onChanged() {
                        actionData.putSetup(gs.getKey(), getText());
                        if (action instanceof CustomizableAppAction) {
                            action.loadContextSetups();
                        }
                        managerFrame.repaint();
                    }
                };

                cb.setText(value);
                add(new JLabel(getNameForCustomizer(gs)), "growx,spanx,wrap");
                add(cb, "growx,spanx,wrap,pushx,growx");
                if (actionClass instanceof ReplacerProvider) {
                    final List<Replacer> tags = ((ReplacerProvider) actionClass).getReplacers(gs.getKey());
                    if (tags != null && tags.size() > 0) {
                        add(createTagList(tags, cb), "growx,spanx,wrap,pushx,growx");
                    }
                }
            } else if (Clazz.isFixedPointNumber(gs.getType())) {
                Number value = (Number) actionData.fetchSetup(gs.getKey());
                if (value == null) {
                    value = (Number) gs.get(actionClass);
                }
                final SpinnerValidator spinnerValidator = gs.getAnnotation(SpinnerValidator.class);
                int min = -1;
                int max = 10000;
                int step = 1;
                if (spinnerValidator != null) {
                    min = (int) spinnerValidator.min();
                    max = (int) spinnerValidator.max();
                    step = (int) Math.max(1, spinnerValidator.step());
                }
                final ExtSpinner spinner = new ExtSpinner(new SpinnerNumberModel(value.intValue(), min, max, step));
                spinner.addChangeListener(new ChangeListener() {

                    @Override
                    public void stateChanged(ChangeEvent e) {
                        actionData.putSetup(gs.getKey(), ((Number) spinner.getValue()).intValue());
                        if (action instanceof CustomizableAppAction) {
                            action.loadContextSetups();
                        }
                        managerFrame.repaint();
                    }
                });

                add(new JLabel(getNameForCustomizer(gs)), "growx,spanx,wrap");
                add(spinner, "growx,spanx,wrap,pushx,growx");
            }
        } catch (Exception e) {
            logger.log(e);
        }
    }

    /**
     * Creates a scrollable list with a header and one row per tag: a button that appends the tag to the given text field, a dropdown with
     * the possible replaces of the tag and the description.
     */
    private static JScrollPane createTagList(final List<Replacer> replacers, final ExtTextField target) {
        final List<Tag> tags = new ArrayList<Tag>();
        for (final Replacer replacer : replacers) {
            tags.addAll(replacer.getTags());
        }
        /* The tag column is as wide as the widest tag needs: The longest key of all tags, plus the arrow if there is a dropdown */
        final JComboBox<String> measureBox = new JComboBox<String>();
        int tagColumnWidth = 0;
        for (final Tag tag : tags) {
            for (final String key : tag.getKeys()) {
                final int width = measureBox.getFontMetrics(measureBox.getFont()).stringWidth("{" + key + "}") + (tag.getKeys().size() > 1 ? 40 : 8);
                tagColumnWidth = Math.max(tagColumnWidth, width);
            }
        }
        final String columns = "[26!][" + tagColumnWidth + "!][grow,fill]";
        /* The list always has the width of the visible area, so that the description column wraps instead of requiring a horizontal scrollbar */
        final MigPanel list = new TagListPanel("ins 2 4 2 4", columns, "[]");
        list.setOpaque(false);
        /* Column header. Uses the same columns as the list, the header of a scroll pane is as wide as its viewport. */
        final MigPanel header = new MigPanel("ins 2 4 2 4", columns, "[]");
        header.setOpaque(false);
        header.add(new JLabel(""));
        header.add(createHeaderLabel("Tag"), "gapleft 4");
        header.add(createHeaderLabel("Description"), "gapleft 8");
        for (final Tag tag : tags) {
            final List<String> replaces = tag.getKeys();
            final String[] labels = new String[replaces.size()];
            for (int i = 0; i < labels.length; i++) {
                labels[i] = "{" + replaces.get(i) + "}";
            }
            /* Tags with only one variant have no dropdown, so that it is obvious that there are no other variants */
            final JComboBox<String> replaceBox;
            final JComponent tagComponent;
            if (replaces.size() > 1) {
                /* The combo box itself is narrow, the popup is made as wide as the longest entry */
                replaceBox = new JComboBox<String>(labels);
                replaceBox.setEditable(false);
                replaceBox.setToolTipText(labels[0]);
                int widestLabel = 0;
                for (final String label : labels) {
                    widestLabel = Math.max(widestLabel, replaceBox.getFontMetrics(replaceBox.getFont()).stringWidth(label));
                }
                final int popupWidth = widestLabel + 40;
                replaceBox.addPopupMenuListener(new PopupMenuListener() {
                    @Override
                    public void popupMenuWillBecomeVisible(final PopupMenuEvent e) {
                        final Object popup = replaceBox.getUI().getAccessibleChild(replaceBox, 0);
                        if (popup instanceof JPopupMenu && ((JPopupMenu) popup).getComponentCount() > 0 && ((JPopupMenu) popup).getComponent(0) instanceof JScrollPane) {
                            final JScrollPane popupScroll = (JScrollPane) ((JPopupMenu) popup).getComponent(0);
                            final Dimension size = new Dimension(Math.max(replaceBox.getWidth(), popupWidth), popupScroll.getPreferredSize().height);
                            popupScroll.setPreferredSize(size);
                            popupScroll.setMaximumSize(size);
                            popupScroll.revalidate();
                        }
                    }

                    @Override
                    public void popupMenuWillBecomeInvisible(final PopupMenuEvent e) {
                    }

                    @Override
                    public void popupMenuCanceled(final PopupMenuEvent e) {
                    }
                });
                tagComponent = replaceBox;
            } else {
                replaceBox = null;
                final JLabel tagLabel = new JLabel(labels[0]);
                tagLabel.setToolTipText(labels[0]);
                tagComponent = tagLabel;
            }
            if (replaceBox != null) {
                replaceBox.addItemListener(new ItemListener() {
                    @Override
                    public void itemStateChanged(final ItemEvent e) {
                        if (e.getStateChange() == ItemEvent.SELECTED) {
                            replaceBox.setToolTipText(String.valueOf(replaceBox.getSelectedItem()));
                        }
                    }
                });
            }
            final JButton add = new JButton(NewTheme.I().getIcon(IconKey.ICON_ADD, 14));
            add.setMargin(new Insets(1, 3, 1, 3));
            add.setFocusable(false);
            add.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    final String newTag = Tag.toPattern(getSelectedReplace(replaceBox, replaces));
                    final String text = target.getText();
                    /* Plausibility check: do not add the tag if the pattern already ends with it */
                    if (text == null || !text.endsWith(newTag)) {
                        target.setText((text == null ? "" : text) + newTag);
                    }
                }
            });
            /* Wraps at the width of the column */
            final JTextArea description = new JTextArea(tag.getDescription());
            description.setEditable(false);
            description.setFocusable(false);
            description.setLineWrap(true);
            description.setWrapStyleWord(true);
            description.setOpaque(false);
            description.setBorder(null);
            description.setFont(new JLabel().getFont());
            description.setForeground(new JLabel().getForeground());
            list.add(add, "aligny center");
            list.add(tagComponent, "gapleft 4,growx,aligny center");
            list.add(description, "gapleft 8,aligny center,growx,wmin 50,wrap");
        }
        final JScrollPane scroll = new JScrollPane(list);
        scroll.setColumnHeaderView(header);
        scroll.setBorder(null);
        scroll.setOpaque(false);
        scroll.getViewport().setOpaque(false);
        scroll.getVerticalScrollBar().setUnitIncrement(16);
        scroll.setPreferredSize(new Dimension(400, 200));
        return scroll;
    }

    /**
     * Panel that always has the width of the viewport of the scroll pane it is in.
     */
    private static class TagListPanel extends MigPanel implements Scrollable {
        private static final long serialVersionUID = 1L;

        private TagListPanel(final String layoutConstraints, final String columnConstraints, final String rowConstraints) {
            super(layoutConstraints, columnConstraints, rowConstraints);
        }

        @Override
        public Dimension getPreferredScrollableViewportSize() {
            return getPreferredSize();
        }

        @Override
        public int getScrollableUnitIncrement(final Rectangle visibleRect, final int orientation, final int direction) {
            return 16;
        }

        @Override
        public int getScrollableBlockIncrement(final Rectangle visibleRect, final int orientation, final int direction) {
            return Math.max(16, visibleRect.height - 16);
        }

        @Override
        public boolean getScrollableTracksViewportWidth() {
            return true;
        }

        @Override
        public boolean getScrollableTracksViewportHeight() {
            return false;
        }
    }

    /**
     * @param replaceBox
     *            The dropdown or null if the tag has only one replace
     */
    private static String getSelectedReplace(final JComboBox<String> replaceBox, final List<String> replaces) {
        if (replaceBox == null) {
            return replaces.get(0);
        } else {
            return replaces.get(Math.max(0, replaceBox.getSelectedIndex()));
        }
    }

    private static JLabel createHeaderLabel(final String text) {
        final JLabel label = new JLabel(text);
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        return label;
    }

    public static String getNameForCustomizer(GetterSetter gs) {
        String ret = gs.getAnnotation(Customizer.class).name();
        String link = gs.getAnnotation(Customizer.class).link();

        // resolve the linked static method
        if (StringUtils.isNotEmpty(link)) {
            try {
                Class<?> cls = gs.getGetter().getDeclaringClass();
                if (link.startsWith("#")) {
                    // relative
                    link = link.substring(1);
                } else {
                    int index = link.lastIndexOf(".");
                    cls = Class.forName(link.substring(0, index));
                    link = link.substring(index + 1);
                }
                Method method = cls.getMethod(link, new Class[] {});
                Object translation = method.invoke(null, new Object[] {});
                if (translation != null && translation instanceof String) {
                    return (String) translation;
                }
            } catch (Throwable e) {
                JDGui.getInstance().getLogger().log(e);
                return link;
            }
        }

        return ret;
    }
}
