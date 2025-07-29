/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.swing.exttable.columns;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.AbstractButton;
import javax.swing.ComboBoxModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.appwork.resources.AWUTheme;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.swing.MigPanel;
import org.appwork.swing.action.BasicAction;
import org.appwork.swing.components.RadioBoxIcon;
import org.appwork.swing.exttable.ExtTableIcon;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.swing.renderer.RendererMigPanel;

public abstract class ExtComboColumn<E, ModelType> extends ExtTextColumn<E> implements ActionListener {
    /**
     * @author Thomas
     *
     */
    public static class RendererPanel extends RendererMigPanel {
        private boolean editable = false;
        private Icon    downIcon;

        /**
         * @param downIcon
         */
        public RendererPanel() {
            super("ins 0", "[]0[grow,fill]", "[grow,fill]");
        }

        /**
         * @return
         */
        public Icon getIcon() {
            return downIcon;
        }

        /*
         * (non-Javadoc)
         *
         * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
         */
        @Override
        protected void paintComponent(final Graphics g) {
            super.paintComponent(g);
            Icon ico = getIcon();
            if (editable && ico != null) {
                if (isEnabled()) {
                    ico.paintIcon(this, g, getWidth() - 5 - ico.getIconWidth(), (getHeight() - ico.getIconHeight()) / 2);
                } else {
                    if (ico instanceof ImageIcon) {
                        ico = AWUTheme.I().getDisabledIcon(ico);
                    }
                    ico.paintIcon(this, g, getWidth() - 5 - ico.getIconWidth(), (getHeight() - ico.getIconHeight()) / 2);
                }
            }
        }

        /**
         * @param editable
         */
        public void setEditable(final boolean editable) {
            this.editable = editable;
        }

        /**
         * @param object
         */
        public void setIcon(final Icon object) {
            downIcon = object;
        }
    }

    private static final long              serialVersionUID = 2114805529462086691L;
    private final ComboBoxModel<ModelType> dataModel;
    protected RendererPanel                rendererPanel;
    private Icon                           iconDown;
    private Icon                           iconUp;

    public ExtComboColumn(final String name, final ComboBoxModel<ModelType> model) {
        this(name, null, model);
    }

    protected Icon createDropUpIcon() {
        return ExtTableIcon.TABLE_COLUMN_COMBO_popUpLarge.get(getPopIconSize());
    }

    protected Icon createDropDownIcon() {
        return ExtTableIcon.TABLE_COLUMN_COMBO_popDownLarge.get(getPopIconSize());
    }

    /**
     * @return
     */
    protected int getPopIconSize() {
        return 8;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.exttable.ExtColumn#adjustWidth(int)
     */
    @Override
    protected int adjustWidth(int w) {
        final Dimension estimated = getEstimatedPopupDimensions();
        if (estimated == null) {
            return w;
        } else {
            return Math.max(w, estimated.width);
        }
    }

    /**
     * @return
     */
    protected Dimension getEstimatedPopupDimensions() {
        if (dataModel != null && dataModel.getSize() > 0) {
            final JPopupMenu opup = createPopupMenu();
            fillPopup(opup, null, dataModel.getElementAt(0), dataModel);
            return opup.getPreferredSize();
        } else {
            return null;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.exttable.columns.ExtTextColumn#createEditorPanel()
     */
    @Override
    protected MigPanel createEditorPanel() {
        return new MigPanel("ins 0", "[]5[grow,fill]", "[grow,fill]") {
            /*
             * (non-Javadoc)
             *
             * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
             */
            @Override
            protected void paintComponent(final Graphics g) {
                super.paintComponent(g);
            }

            @Override
            public void requestFocus() {
                editorField.requestFocus();
            }
        };
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.exttable.columns.ExtTextColumn#createRendererPanel()
     */
    @Override
    protected RendererMigPanel createRendererPanel() {
        iconDown = createDropDownIcon();
        iconUp = createDropUpIcon();
        return this.rendererPanel = new RendererPanel() {
            public Icon getIcon() {
                if (popup == null || !popup.isVisible()) {
                    //
                    return iconDown;
                }
                return super.getIcon();
            }
        };
    }

    public ExtComboColumn(final String name, final ExtTableModel<E> table, final ComboBoxModel<ModelType> model) {
        super(name, table);
        this.dataModel = model;
    }

    @Override
    public void configureRendererComponent(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        final ModelType selected = getSelectedItem(value);
        rendererPanel.setEditable(isEditable(value));
        rendererPanel.setIcon(editing == value ? iconUp : iconDown);
        Icon icon = getIconForRenderer(value, selected);
        rendererIcon.setIcon(icon);
        String str = getStringForRenderer(value, selected);
        if (str == null) {
            // under substance, setting setText(null) somehow sets the label
            // opaque.
            str = "";
        }
        if (getTableColumn() != null && str.length() > 0) {
            try {
                int upDownIconWidth = rendererPanel.getIcon() != null ? rendererPanel.getIcon().getIconWidth() : 0;
                int iconWidth = icon != null ? icon.getIconWidth() : 0;
                int availableWidth = getTableColumn().getWidth() - upDownIconWidth - iconWidth - 5 - 10;
                String clipped = org.appwork.sunwrapper.sun.swing.SwingUtilities2Wrapper.clipStringIfNecessary(rendererField, rendererField.getFontMetrics(rendererField.getFont()), str, availableWidth);
                rendererField.setText(clipped);
            } catch (Throwable e) {
                // fallback if org.appwork.swing.sunwrapper.SwingUtilities2 disappears someday
                e.printStackTrace();
                rendererField.setText(str);
            }
        } else {
            rendererField.setText(str);
        }
    }

    public Icon getIconForRenderer(final E value, final ModelType selected) {
        return this.modelItemToIcon(selected, value);
    }

    public String getStringForRenderer(final E value, final ModelType selected) {
        return modelItemToString(selected, value);
    }

    /**
     * @param selected
     * @param value
     *            TODO
     * @return
     */
    protected Icon modelItemToIcon(final ModelType selected, E value) {
        return null;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.exttable.ExtColumn#onMousePressed(java.awt.event.MouseEvent, java.lang.Object)
     */
    @Override
    public boolean onMousePressed(MouseEvent e, E value) {
        if (!isEditable(value, isEnabled(value))) {
            return false;
        } else {
            final int row = getModel().getTable().rowAtPoint(new Point(e.getX(), e.getY()));
            // int column = getModel().getTable().columnAtPoint(new Point(e.getX(),
            // e.getY()));
            return startEdit(value, row);
        }
    }

    /**
     * @param value
     * @param row
     * @return
     */
    protected long     lastHide = 0;
    private E          editing  = null;
    private JPopupMenu popup;

    protected boolean startEdit(final E value, final int row) {
        final long timeSinceLastHide = System.currentTimeMillis() - lastHide;
        if (timeSinceLastHide < 250 && editing == value) {
            //
            editing = null;
            repaint();
            return true;
        }
        editing = value;
        popup = createPopupMenu();
        try {
            final ModelType selected = getSelectedItem(value);
            final ComboBoxModel<ModelType> dm = updateModel(dataModel, value);
            fillPopup(popup, value, selected, dm);
            final Rectangle bounds = getModel().getTable().getCellRect(row, getIndex(), true);
            final Dimension pref = popup.getPreferredSize();
            popup.setPreferredSize(new Dimension(Math.max(pref.width, bounds.width), pref.height));
            popup.show(getModel().getTable(), bounds.x, bounds.y + bounds.height);
            return true;
        } catch (final Exception e1) {
            e1.printStackTrace();
        }
        return false;
    }

    /**
     * @return
     */
    public JPopupMenu createPopupMenu() {
        return new JPopupMenu() {
            public void setVisible(final boolean b) {
                super.setVisible(b);
                if (!b) {
                    lastHide = System.currentTimeMillis();
                    // editing = null;
                    // updateIcon(true);
                } else {
                    // updateIcon(false);
                }
            };
        };
    }

    protected void fillPopup(final JPopupMenu popup, final E value, final ModelType selected, final ComboBoxModel<ModelType> dm) {
        for (int i = 0; i < dm.getSize(); i++) {
            final ModelType o = dm.getElementAt(i);
            final JComponent bt = getPopupElement(o, CompareUtils.equals(selected, o), value);
            if (bt != null) {
                if (bt instanceof AbstractButton) {
                    ((AbstractButton) bt).addActionListener(new ActionListener() {
                        @Override
                        public void actionPerformed(final ActionEvent e) {
                            setValue(o, value);
                            popup.setVisible(false);
                        }
                    });
                }
                popup.add(bt);
            }
        }
    }

    /**
     * @param value
     *            TODO
     * @param b
     * @param o2
     * @return
     */
    protected JComponent getPopupElement(final ModelType o, final boolean selected, final E value) {
        return new JMenuItem(new BasicAction("") {
            {
                setName(getStringForRenderer(value, o));
                if (selected) {
                    setSmallIcon(RadioBoxIcon.TRUE);
                } else {
                    setSmallIcon(RadioBoxIcon.FALSE);
                }
            }

            @Override
            public void actionPerformed(final ActionEvent e) {
            }
        });
    }

    final protected Icon getIcon(final E value) {
        return modelItemToIcon(getSelectedItem(value), value);
    }

    @Override
    public String getStringValue(final E value) {
        return modelItemToString(getSelectedItem(value), value);
    }

    /**
     * @param value
     * @param comboBoxModel
     * @return
     */
    protected abstract ModelType getSelectedItem(final E object);

    protected abstract void setSelectedItem(final E object, final ModelType value);

    /**
     * @param selectedItem
     * @param value
     *            TODO
     * @return
     */
    protected String modelItemToString(final ModelType selectedItem, E value) {
        if (selectedItem == null) {
            return null;
        } else {
            if (value instanceof LabelInterface) {
                return ((LabelInterface) value).getLabel();
            }
            return selectedItem.toString();
        }
    }

    @Override
    protected String getTooltipText(final E obj) {
        return super.getTooltipText(obj);
    }

    @Override
    public boolean isEditable(final E obj) {
        final ComboBoxModel<ModelType> mod = updateModel(dataModel, obj);
        return mod != null && mod.getSize() > 1;
    }

    @Override
    public boolean isEnabled(final E obj) {
        return true;
    }

    public boolean isCellEditable(final int rowIndex, final int columnIndex) {
        return false;
    }

    @Override
    public boolean isSortable(final E obj) {
        return true;
    }

    @Override
    final public void setValue(final Object value, final E object) {
        try {
            setSelectedItem(object, (ModelType) value);
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * overwrite this method to implement different dropdown boxes
     *
     * @param dataModel
     */
    public ComboBoxModel<ModelType> updateModel(final ComboBoxModel<ModelType> dataModel, final E value) {
        return dataModel;
    }
}
