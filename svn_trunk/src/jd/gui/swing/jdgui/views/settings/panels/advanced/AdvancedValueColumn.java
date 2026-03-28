package jd.gui.swing.jdgui.views.settings.panels.advanced;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.Icon;
import javax.swing.JColorChooser;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;

import jd.gui.swing.jdgui.views.settings.components.MultiComboBox;
import net.miginfocom.swing.MigLayout;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.annotations.EnumLabel;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.swing.components.CheckBoxIcon;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtCompoundColumn;
import org.appwork.swing.exttable.columns.ExtSpinnerColumn;
import org.appwork.swing.exttable.columns.ExtTextAreaColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.event.DefaultEvent;
import org.appwork.utils.event.DontThrowFromCurrentThreadEventSuppressor;
import org.appwork.utils.event.EventSuppressor;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.contextmenu.gui.ExtPopupMenu;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.logging.LogController;
import org.jdownloader.settings.advanced.AdvancedConfigEntry;
import org.jdownloader.settings.advanced.RangeValidator;
import org.jdownloader.updatev2.gui.LAFOptions;

public class AdvancedValueColumn extends ExtCompoundColumn<AdvancedConfigEntry> {
    private static final long                              serialVersionUID = 1L;
    private ExtTextColumn<AdvancedConfigEntry>             stringColumn;
    private ExtCheckColumn<AdvancedConfigEntry>            booleanColumn;
    private ExtTextAreaColumn<AdvancedConfigEntry>         defaultColumn;
    private java.util.List<ExtColumn<AdvancedConfigEntry>> columns;
    private ExtSpinnerColumn<AdvancedConfigEntry>          numberColumn;
    private ExtTextColumn<AdvancedConfigEntry>             enumColumn;
    private ExtTextColumn<AdvancedConfigEntry>             enumSetColumn;
    private ExtTextColumn<AdvancedConfigEntry>             colorColumn;

    // private ExtComponentColumn<AdvancedConfigEntry> actionColumn;
    public AdvancedValueColumn() {
        super(_GUI.T.AdvancedValueColumn_AdvancedValueColumn_object_());
        columns = new ArrayList<ExtColumn<AdvancedConfigEntry>>();
        initColumns();
    }

    @Override
    public boolean isEnabled(AdvancedConfigEntry obj) {
        return true;
    }

    @Override
    public boolean isHidable() {
        return false;
    }

    private void initColumns() {
        stringColumn = new ExtTextColumn<AdvancedConfigEntry>(getName()) {
            private static final long serialVersionUID = 1L;
            {
                rendererField.setHorizontalAlignment(SwingConstants.RIGHT);
            }

            @Override
            public boolean isEditable(final AdvancedConfigEntry obj) {
                return obj.isEditable();
            }

            @Override
            public String getStringValue(AdvancedConfigEntry value) {
                return value.getValue() + "";
            }

            @Override
            protected String getTooltipText(AdvancedConfigEntry obj) {
                return obj.getDescription();
            }

            @Override
            protected void setStringValue(String value, AdvancedConfigEntry object) {
                object.setValue(value);
                AdvancedValueColumn.this.getModel().getTable().repaint();
            }
        };
        register(stringColumn);
        colorColumn = new ExtTextColumn<AdvancedConfigEntry>(getName()) {
            private static final long serialVersionUID = 1L;
            {
                rendererField.setHorizontalAlignment(SwingConstants.RIGHT);
            }

            public boolean onDoubleClick(final MouseEvent e, final AdvancedConfigEntry value) {
                Object v = value.getValue();
                Color c = v == null ? null : LAFOptions.createColor(v.toString());
                Color newColor = JColorChooser.showDialog(getModel().getTable(), _GUI.T.AdvancedValueColumn_onSingleClick_colorchooser_title_(), c);
                if (newColor != null) {
                    setStringValue("#" + LAFOptions.toHex(newColor), value);
                }
                return true;
            }

            @Override
            public boolean isEditable(final AdvancedConfigEntry obj) {
                return false;
            }

            public Color getContrastColor(Color color) {
                double y = (299 * color.getRed() + 587 * color.getGreen() + 114 * color.getBlue()) / 1000;
                if (color.getAlpha() < 128) {
                    return Color.BLACK;
                }
                return y >= 128 ? Color.black : Color.white;
            }

            @Override
            public void configureEditorComponent(final AdvancedConfigEntry value, final boolean isSelected, final int row, final int column) {
                super.configureEditorComponent(value, isSelected, row, column);
                Object v = value.getValue();
                Color c = v == null ? null : LAFOptions.createColor(v.toString());
                if (c != null) {
                    editorField.setBackground(c);
                    editorField.setForeground(getContrastColor(c));
                    editorField.setOpaque(true);
                } else {
                    editorField.setBackground(null);
                    editorField.setForeground(null);
                    editorField.setOpaque(false);
                }
            }

            @Override
            public void configureRendererComponent(final AdvancedConfigEntry value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                super.configureRendererComponent(value, isSelected, hasFocus, row, column);
                Object v = value.getValue();
                Color c = v == null ? null : LAFOptions.createColor(v.toString());
                if (c != null) {
                    rendererField.setBackground(c);
                    rendererField.setForeground(getContrastColor(c));
                    rendererField.setOpaque(true);
                } else {
                    rendererField.setBackground(null);
                    rendererField.setForeground(null);
                    rendererField.setOpaque(false);
                }
            }

            @Override
            public String getStringValue(AdvancedConfigEntry value) {
                if (value.getValue() == null || (value.getValue().toString()) == null) {
                    return null;
                }
                return value.getValue() + "";
            }

            @Override
            protected String getTooltipText(AdvancedConfigEntry obj) {
                return obj.getDescription();
            }

            @Override
            protected void setStringValue(String value, AdvancedConfigEntry object) {
                Color c = value == null ? null : LAFOptions.createColor(value);
                if (c != null) {
                    object.setValue(value);
                    AdvancedValueColumn.this.getModel().getTable().repaint();
                }
            }
        };
        register(colorColumn);
        defaultColumn = new ExtTextAreaColumn<AdvancedConfigEntry>(getName()) {
            private static final long serialVersionUID = 1L;
            {
                renderer.setHorizontalAlignment(SwingConstants.RIGHT);
            }

            @Override
            protected String getTooltipText(AdvancedConfigEntry obj) {
                return obj.getDescription();
            }

            @Override
            public boolean isEditable(final AdvancedConfigEntry obj) {
                return obj.isEditable();
            }

            @Override
            public String getStringValue(AdvancedConfigEntry value) {
                return JSonStorage.toString(value.getValue());
            }

            @Override
            protected void setStringValue(String value, AdvancedConfigEntry object) {
                if (object.getType() instanceof Class) {
                    final Class<?> clazz = (Class<?>) object.getType();
                    if (clazz.isArray() || clazz.isAssignableFrom(List.class) || clazz.isAssignableFrom(Set.class)) {
                        if (value != null && !"null".equalsIgnoreCase(value.trim()) && !value.matches("(?s)^\\s*\\[.+\\]\\s*$")) {
                            value = "[" + value + "]";
                        }
                    }
                }
                try {
                    final Object newValue = JSonStorage.restoreFromString(value, new TypeRef<Object>(object.getType()) {
                    });
                    object.setValue(newValue);
                    AdvancedValueColumn.this.getModel().getTable().repaint();
                } catch (Throwable e) {
                    Dialog.getInstance().showErrorDialog("'" + value + "'\r\nis not a valid\r\n" + object.getTypeString());
                }
            }
        };
        register(defaultColumn);
        booleanColumn = new ExtCheckColumn<AdvancedConfigEntry>(getName()) {
            private static final long serialVersionUID = 1L;

            @Override
            protected boolean getBooleanValue(AdvancedConfigEntry value) {
                return (Boolean) value.getValue();
            }

            @Override
            public boolean isEditable(final AdvancedConfigEntry obj) {
                return obj.isEditable();
            }

            {
                this.renderer.setHorizontalAlignment(SwingConstants.RIGHT);
                renderer.setHorizontalTextPosition(SwingConstants.LEFT);
                this.editor.setHorizontalAlignment(SwingConstants.RIGHT);
            }

            @Override
            protected void setBooleanValue(boolean value, AdvancedConfigEntry object) {
                object.setValue(value);
                AdvancedValueColumn.this.getModel().getTable().repaint();
            }
        };
        register(booleanColumn);
        numberColumn = new ExtSpinnerColumn<AdvancedConfigEntry>(getName()) {
            private static final long serialVersionUID = 1L;

            @Override
            public boolean isEditable(final AdvancedConfigEntry obj) {
                return obj.isEditable();
            }

            @Override
            protected SpinnerNumberModel getModel(AdvancedConfigEntry value, Number n) {
                final SpinnerNumberModel ret = super.getModel(value, n);
                if (value.getValidator() != null) {
                    if (value.getValidator() instanceof RangeValidator) {
                        if (Clazz.isDouble(n.getClass())) {
                            ret.setMaximum(((RangeValidator) value.getValidator()).getMax().doubleValue());
                            ret.setMinimum(((RangeValidator) value.getValidator()).getMin().doubleValue());
                            ret.setStepSize(((RangeValidator) value.getValidator()).getSteps().doubleValue());
                        } else if (Clazz.isFloat(n.getClass())) {
                            ret.setMaximum(((RangeValidator) value.getValidator()).getMax().floatValue());
                            ret.setMinimum(((RangeValidator) value.getValidator()).getMin().floatValue());
                            ret.setStepSize(((RangeValidator) value.getValidator()).getSteps().floatValue());
                        } else if (Clazz.isLong(n.getClass())) {
                            ret.setMaximum(((RangeValidator) value.getValidator()).getMax().longValue());
                            ret.setMinimum(((RangeValidator) value.getValidator()).getMin().longValue());
                            ret.setStepSize(((RangeValidator) value.getValidator()).getSteps().longValue());
                        } else if (Clazz.isInteger(n.getClass())) {
                            ret.setMaximum(((RangeValidator) value.getValidator()).getMax().intValue());
                            ret.setMinimum(((RangeValidator) value.getValidator()).getMin().intValue());
                            ret.setStepSize(((RangeValidator) value.getValidator()).getSteps().intValue());
                        } else if (Clazz.isShort(n.getClass())) {
                            ret.setMaximum(((RangeValidator) value.getValidator()).getMax().shortValue());
                            ret.setMinimum(((RangeValidator) value.getValidator()).getMin().shortValue());
                            ret.setStepSize(((RangeValidator) value.getValidator()).getSteps().shortValue());
                        } else if (Clazz.isByte(n.getClass())) {
                            ret.setMaximum(((RangeValidator) value.getValidator()).getMax().byteValue());
                            ret.setMinimum(((RangeValidator) value.getValidator()).getMin().byteValue());
                            ret.setStepSize(((RangeValidator) value.getValidator()).getSteps().byteValue());
                        }
                    }
                }
                return ret;
            }

            @Override
            protected Number getNumber(AdvancedConfigEntry value) {
                return (Number) value.getValue();
            }

            @Override
            protected void setNumberValue(Number value, AdvancedConfigEntry object) {
                object.setValue(value);
                AdvancedValueColumn.this.getModel().getTable().repaint();
            }

            @Override
            public String getStringValue(AdvancedConfigEntry value) {
                return value.getValue() + "";
            }
        };
        register(numberColumn);
        {
            enumColumn = new ExtTextColumn<AdvancedConfigEntry>(getName(), null) {
                private static final long serialVersionUID = 1L;
                {
                    renderer.removeAll();
                    renderer.setLayout(new MigLayout("ins 0", "[grow,fill]0[12]5", "[grow,fill]"));
                    renderer.add(rendererField);
                    renderer.add(rendererIcon);
                }

                @Override
                public void configureRendererComponent(final AdvancedConfigEntry value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                    Icon icon;
                    this.rendererIcon.setIcon(icon = this.getIcon(value));
                    String str = this.getStringValue(value);
                    if (str == null) {
                        // under substance, setting setText(null) somehow sets the label
                        // opaque.
                        str = "";
                    }
                    if (this.getTableColumn() != null) {
                        try {
                            this.rendererField.setText(org.appwork.sunwrapper.sun.swing.SwingUtilities2Wrapper.clipStringIfNecessary(this.rendererField, this.rendererField.getFontMetrics(this.rendererField.getFont()), str, this.getTableColumn().getWidth() - 18 - (icon != null ? icon.getIconWidth() : 0)));
                        } catch (Throwable e) {
                            // fallback if org.appwork.swing.sunwrapper.SwingUtilities2 disappears someday
                            e.printStackTrace();
                            this.rendererField.setText(str);
                        }
                    } else {
                        this.rendererField.setText(str);
                    }
                }

                public boolean onSingleClick(final MouseEvent e, final AdvancedConfigEntry value) {
                    ExtPopupMenu popup = new ExtPopupMenu();
                    try {
                        Object[] values = (Object[]) ((Class) value.getClazz()).getMethod("values", new Class[] {}).invoke(null, new Object[] {});
                        for (final Object o : values) {
                            popup.add(new JMenuItem(new AppAction() {
                                {
                                    EnumLabel lbl = value.getClazz().getDeclaredField(o.toString()).getAnnotation(EnumLabel.class);
                                    if (lbl != null) {
                                        setName(lbl.value());
                                    } else {
                                        if (o instanceof LabelInterface) {
                                            setName(((LabelInterface) o).getLabel());
                                        } else {
                                            setName(o.toString());
                                        }
                                    }
                                    if (value.getValue() == o) {
                                        setSmallIcon(CheckBoxIcon.TRUE);
                                    } else {
                                        setSmallIcon(CheckBoxIcon.FALSE);
                                    }
                                }

                                @Override
                                public void actionPerformed(ActionEvent e) {
                                    value.setValue(o);
                                    AdvancedValueColumn.this.getModel().getTable().repaint();
                                }
                            }));
                        }
                        Rectangle bounds = getModel().getTable().getCellRect(getModel().getTable().rowAtPoint(new Point(e.getX(), e.getY())), getModel().getTable().columnAtPoint(new Point(e.getX(), e.getY())), true);
                        Dimension pref = popup.getPreferredSize();
                        popup.setPreferredSize(new Dimension(Math.max(pref.width, bounds.width), pref.height));
                        popup.show(getModel().getTable(), bounds.x, bounds.y + bounds.height);
                        return true;
                    } catch (Exception e1) {
                        e1.printStackTrace();
                    }
                    return false;
                }

                protected int getSelectedIndex(AdvancedConfigEntry value) {
                    return ((Enum<?>) value.getValue()).ordinal();
                }

                protected void setSelectedIndex(int value, AdvancedConfigEntry object) {
                    Object[] values;
                    try {
                        values = (Object[]) object.getClazz().getMethod("values", new Class[] {}).invoke(null, new Object[] {});
                        object.setValue(values[value]);
                        AdvancedValueColumn.this.getModel().getTable().repaint();
                    } catch (IllegalArgumentException e) {
                        e.printStackTrace();
                    } catch (SecurityException e) {
                        e.printStackTrace();
                    } catch (IllegalAccessException e) {
                        e.printStackTrace();
                    } catch (InvocationTargetException e) {
                        e.printStackTrace();
                    } catch (NoSuchMethodException e) {
                        e.printStackTrace();
                    }
                }

                protected Icon getIcon(final AdvancedConfigEntry value) {
                    return NewTheme.I().getIcon(IconKey.ICON_POPDOWNLARGE, -1);
                }

                @Override
                public String getStringValue(AdvancedConfigEntry value) {
                    try {
                        EnumLabel lbl = value.getClazz().getDeclaredField(value.getValue().toString()).getAnnotation(EnumLabel.class);
                        if (lbl != null) {
                            return lbl.value();
                        }
                        if (value.getValue() instanceof LabelInterface) {
                            return ((LabelInterface) value.getValue()).getLabel();
                        }
                        if (value instanceof LabelInterface) {
                            return ((LabelInterface) value).getLabel();
                        }
                    } catch (Exception e) {
                    }
                    return value.getValue().toString();
                }
            };
            register(enumColumn);
        }
        {
            enumSetColumn = new ExtTextColumn<AdvancedConfigEntry>(getName(), null) {
                private static final long serialVersionUID = 1L;
                {
                    renderer.removeAll();
                    renderer.setLayout(new MigLayout("ins 0", "[grow,fill]0[12]5", "[grow,fill]"));
                    renderer.add(rendererField);
                    renderer.add(rendererIcon);
                }

                @Override
                public void configureRendererComponent(final AdvancedConfigEntry value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                    Icon icon;
                    this.rendererIcon.setIcon(icon = this.getIcon(value));
                    String str = this.getStringValue(value);
                    if (str == null) {
                        // under substance, setting setText(null) somehow sets the label
                        // opaque.
                        str = "";
                    }
                    if (this.getTableColumn() != null) {
                        try {
                            this.rendererField.setText(org.appwork.sunwrapper.sun.swing.SwingUtilities2Wrapper.clipStringIfNecessary(this.rendererField, this.rendererField.getFontMetrics(this.rendererField.getFont()), str, this.getTableColumn().getWidth() - 18 - (icon != null ? icon.getIconWidth() : 0)));
                        } catch (Throwable e) {
                            // fallback if org.appwork.swing.sunwrapper.SwingUtilities2 disappears someday
                            e.printStackTrace();
                            this.rendererField.setText(str);
                        }
                    } else {
                        this.rendererField.setText(str);
                    }
                }

                public boolean onSingleClick(final MouseEvent e, final AdvancedConfigEntry configEntry) {
                    try {
                        final KeyHandler<Set<Enum>> m = (KeyHandler<Set<Enum>>) configEntry.getKeyHandler();
                        final Type[] types = ((ParameterizedType) configEntry.getType()).getActualTypeArguments();
                        final MultiComboBox<Object> comp = new MultiComboBox<Object>(((Class) types[0]).getEnumConstants()) {
                            private final GenericConfigEventListener<Set<Enum>> listener = new GenericConfigEventListener<Set<Enum>>() {
                                @Override
                                public void onConfigValidatorError(KeyHandler<Set<Enum>> keyHandler, Set<Enum> invalidValue, ValidationException validateException) {
                                }

                                @Override
                                public void onConfigValueModified(KeyHandler<Set<Enum>> keyHandler, Set<Enum> newValue) {
                                    updateModel(newValue);
                                }
                            };
                            {
                                Set<Enum> value = (Set<Enum>) configEntry.getValue();
                                if (value == null) {
                                    value = newSetInstance(m);
                                    for (Object e : ((Class) types[0]).getEnumConstants()) {
                                        value.add((Enum) e);
                                    }
                                }
                                m.getEventSender().addListener(listener, true);
                                updateModel(value);
                            }

                            @Override
                            protected String getLabel(List<Object> list) {
                                return "[" + list.size() + "/" + getValues().size() + "] " + super.getLabel(list);
                            }

                            protected Set<Enum> newSetInstance(KeyHandler m) throws InstantiationException, IllegalAccessException {
                                Class raw = ReflectionUtils.getRaw(m.getTypeRef().getType());
                                if (raw.isInterface()) {
                                    raw = HashSet.class;
                                }
                                final Set<Enum> value = (Set<Enum>) raw.newInstance();
                                return value;
                            }

                            protected void updateModel(final Set<Enum> newValue) {
                                if (newValue == null) {
                                    setSelectedItems(((Class) types[0]).getEnumConstants());
                                } else {
                                    setSelectedItems((Object[]) newValue.toArray(new Enum[0]));
                                }
                            }

                            @Override
                            public void setVisible(boolean aFlag) {
                                super.setVisible(aFlag);
                                if (!aFlag) {
                                    m.getEventSender().removeListener(listener);
                                }
                            };

                            @Override
                            public void onChanged() {
                                super.onChanged();
                                final EventSuppressor added = new DontThrowFromCurrentThreadEventSuppressor<DefaultEvent>();
                                m.getEventSender().addEventSuppressor(added);
                                try {
                                    final Set<Enum> set = newSetInstance(m);
                                    for (Object e : getSelectedItems()) {
                                        set.add((Enum) e);
                                    }
                                    m.setValue(set);
                                } catch (InstantiationException e1) {
                                    LogController.CL().log(e1);
                                } catch (IllegalAccessException e1) {
                                    LogController.CL().log(e1);
                                } finally {
                                    m.getEventSender().removeEventSuppressor(added);
                                }
                            }
                        };
                        final JPopupMenu popup = comp.getPopup();
                        final Rectangle bounds = getModel().getTable().getCellRect(getModel().getTable().rowAtPoint(new Point(e.getX(), e.getY())), getModel().getTable().columnAtPoint(new Point(e.getX(), e.getY())), true);
                        final Dimension pref = popup.getPreferredSize();
                        popup.setPreferredSize(new Dimension(Math.max(pref.width, bounds.width), pref.height));
                        popup.show(getModel().getTable(), bounds.x, bounds.y + bounds.height);
                        return true;
                    } catch (Throwable ign) {
                        LogController.CL().log(ign);
                    }
                    return false;
                }

                protected Icon getIcon(final AdvancedConfigEntry value) {
                    return NewTheme.I().getIcon(IconKey.ICON_POPDOWNLARGE, -1);
                }

                @Override
                public String getStringValue(AdvancedConfigEntry configEntry) {
                    final Type[] types = ((ParameterizedType) configEntry.getType()).getActualTypeArguments();
                    final List<String> list = new ArrayList<String>();
                    labels: try {
                        final Set<Enum> set = (Set<Enum>) configEntry.getValue();
                        if (set == null || set.size() == 0) {
                            break labels;
                        }
                        for (Enum entry : set) {
                            String label = null;
                            try {
                                final EnumLabel lbl = entry.getClass().getDeclaredField(entry.name()).getAnnotation(EnumLabel.class);
                                if (lbl != null) {
                                    label = lbl.value();
                                }
                            } catch (Exception e) {
                            }
                            if (label == null && entry instanceof LabelInterface) {
                                label = ((LabelInterface) entry).getLabel();
                            }
                            if (label == null) {
                                label = entry.name();
                            }
                            list.add(label);
                        }

                    } catch (Exception e) {
                    }
                    return "[" + list.size() + "/" + ((Class) types[0]).getEnumConstants().length + "] " + list.toString();
                }
            };
            register(enumSetColumn);
        }
    }

    private void register(ExtColumn<AdvancedConfigEntry> col) {
        col.setClickcount(1);// edit mode on first click
        columns.add(col);
    }

    @Override
    public String getSortString(AdvancedConfigEntry o1) {
        return null;
    }

    @Override
    public ExtColumn<AdvancedConfigEntry> selectColumn(AdvancedConfigEntry object) {
        if (object == null) {
            return defaultColumn;
        }
        // if (object.getKeyHandler().getAnnotation(ActionClass.class) != null) {
        // return actionColumn;
        // } else
        if (Clazz.isBoolean(object.getType())) {
            return booleanColumn;
        } else if (object.getType() == String.class) {
            if (object.hasHexColorString()) {
                return colorColumn;
            } else if (object.isMultiLineString()) {
                return defaultColumn;
            } else {
                return stringColumn;
            }
        } else if (Clazz.isDouble(object.getType()) || Clazz.isFloat(object.getType()) || Clazz.isLong(object.getType()) || Clazz.isInteger(object.getType()) || Clazz.isByte(object.getType())) {
            return numberColumn;
        } else if (Enum.class.isAssignableFrom(object.getClazz())) {
            return enumColumn;
        } else if (Set.class.isAssignableFrom(object.getClazz())) {
            final Type[] types = ((ParameterizedType) object.getType()).getActualTypeArguments();
            if (types[0] instanceof Class && ((Class) types[0]).isEnum()) {
                return enumSetColumn;
            }
        }
        return defaultColumn;
    }
}
