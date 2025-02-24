package jd.gui.swing.jdgui.views.settings.panels.advanced;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.swing.Icon;
import javax.swing.JColorChooser;
import javax.swing.JMenuItem;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.annotations.EnumLabel;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.swing.components.CheckBoxIcon;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtCompoundColumn;
import org.appwork.swing.exttable.columns.ExtSpinnerColumn;
import org.appwork.swing.exttable.columns.ExtTextAreaColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.contextmenu.gui.ExtPopupMenu;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.advanced.AdvancedConfigEntry;
import org.jdownloader.settings.advanced.RangeValidator;
import org.jdownloader.updatev2.gui.LAFOptions;

import net.miginfocom.swing.MigLayout;

public class AdvancedValueColumn extends ExtCompoundColumn<AdvancedConfigEntry> {
    private static final long                              serialVersionUID = 1L;
    private ExtTextColumn<AdvancedConfigEntry>             stringColumn;
    private ExtCheckColumn<AdvancedConfigEntry>            booleanColumn;
    private ExtTextAreaColumn<AdvancedConfigEntry>         defaultColumn;
    private java.util.List<ExtColumn<AdvancedConfigEntry>> columns;
    private ExtSpinnerColumn<AdvancedConfigEntry>          longColumn;
    private ExtTextColumn<AdvancedConfigEntry>             enumColumn;
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
                    Dialog.getInstance().showErrorDialog("'" + value + "' is not a valid '" + object.getTypeString() + "'");
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
        longColumn = new ExtSpinnerColumn<AdvancedConfigEntry>(getName()) {
            private static final long serialVersionUID = 1L;

            @Override
            public boolean isEditable(final AdvancedConfigEntry obj) {
                return obj.isEditable();
            }

            @Override
            protected SpinnerNumberModel getModel(AdvancedConfigEntry value, Number n) {
                SpinnerNumberModel ret = super.getModel(value, n);
                if (value.getValidator() != null) {
                    if (value.getValidator() instanceof RangeValidator) {
                        if (Clazz.isDouble(n.getClass())) {
                            ret.setMaximum((double) ((RangeValidator) value.getValidator()).getMax());
                            ret.setMinimum((double) ((RangeValidator) value.getValidator()).getMin());
                            ret.setStepSize((double) ((RangeValidator) value.getValidator()).getSteps());
                        } else if (Clazz.isFloat(n.getClass())) {
                            ret.setMaximum((float) ((RangeValidator) value.getValidator()).getMax());
                            ret.setMinimum((float) ((RangeValidator) value.getValidator()).getMin());
                            ret.setStepSize((float) ((RangeValidator) value.getValidator()).getSteps());
                        } else if (Clazz.isLong(n.getClass())) {
                            ret.setMaximum(((RangeValidator) value.getValidator()).getMax());
                            ret.setMinimum(((RangeValidator) value.getValidator()).getMin());
                            ret.setStepSize(((RangeValidator) value.getValidator()).getSteps());
                        } else if (Clazz.isInteger(n.getClass())) {
                            ret.setMaximum((int) ((RangeValidator) value.getValidator()).getMax());
                            ret.setMinimum((int) ((RangeValidator) value.getValidator()).getMin());
                            ret.setStepSize((int) ((RangeValidator) value.getValidator()).getSteps());
                        } else if (Clazz.isShort(n.getClass())) {
                            ret.setMaximum((short) ((RangeValidator) value.getValidator()).getMax());
                            ret.setMinimum((short) ((RangeValidator) value.getValidator()).getMin());
                            ret.setStepSize((short) ((RangeValidator) value.getValidator()).getSteps());
                        } else if (Clazz.isByte(n.getClass())) {
                            ret.setMaximum((byte) ((RangeValidator) value.getValidator()).getMax());
                            ret.setMinimum((byte) ((RangeValidator) value.getValidator()).getMin());
                            ret.setStepSize((byte) ((RangeValidator) value.getValidator()).getSteps());
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
        register(longColumn);
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
            }
            if (object.isMultiLineString()) {
                return defaultColumn;
            }
            return stringColumn;
        } else if (Clazz.isDouble(object.getType()) || Clazz.isFloat(object.getType()) || Clazz.isLong(object.getType()) || Clazz.isInteger(object.getType()) || Clazz.isByte(object.getType())) {
            return longColumn;
        } else if (Enum.class.isAssignableFrom(object.getClazz())) {
            return enumColumn;
        } else {
            return defaultColumn;
        }
    }
}
