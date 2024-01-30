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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;

import org.appwork.swing.MigPanel;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.renderer.RenderLabel;
import org.appwork.utils.swing.renderer.RendererMigPanel;

public abstract class ExtTextColumn<E> extends ExtColumn<E> implements ActionListener, FocusListener {
    private static final long serialVersionUID = 2114805529462086691L;
    protected JLabel          rendererField;

    public JLabel getRendererField() {
        return rendererField;
    }

    protected JTextField          editorField;
    private final Border          defaultBorder = BorderFactory.createEmptyBorder(0, 5, 0, 5);
    private Font                  rendererFont;
    private Font                  editorFont;
    protected JPanel              editor;
    protected JLabel              rendererIcon;
    protected JPanel              renderer;
    protected JLabel              editorIconLabel;
    protected final AtomicInteger noSet         = new AtomicInteger(0);

    public void setNoSet(boolean b) {
        if (b) {
            noSet.incrementAndGet();
        } else if (noSet.get() > 0) {
            noSet.decrementAndGet();
        }
    }

    private boolean clipingEnabled = true;

    public boolean isClipingEnabled() {
        return clipingEnabled;
    }

    public void setClipingEnabled(boolean clipingEnabled) {
        this.clipingEnabled = clipingEnabled;
    }

    /**
     * @param string
     */
    public ExtTextColumn(final String name) {
        this(name, null);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.exttable.ExtColumn#getCellSizeEstimation(java.lang.Object, int)
     */
    @Override
    public Dimension getCellSizeEstimation(E element, int row) {
        boolean clip = isClipingEnabled();
        try {
            setClipingEnabled(false);
            Component c = getTableCellRendererComponent(getModel().getTable(), element, false, false, row, 1);
            c.invalidate();
            Dimension ret = c.getPreferredSize();
            return ret;
        } finally {
            setClipingEnabled(clip);
        }
    }

    public ExtTextColumn(final String name, final ExtTableModel<E> table) {
        super(name, table);
        this.editorField = new JTextField();
        this.editorField.addFocusListener(this);
        this.editorField.setBorder(null);
        this.rendererIcon = new RenderLabel() {
            private static final long serialVersionUID = 1L;

            @Override
            public void setIcon(final Icon icon) {
                this.setVisible(icon != null);
                if (icon != this.getIcon()) {
                    super.setIcon(icon);
                }
            }
        };
        this.editorField.addKeyListener(new KeyListener() {
            public void keyPressed(final KeyEvent e) {
            }

            public void keyReleased(final KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                    setNoSet(true);
                    try {
                        ExtTextColumn.this.stopCellEditing();
                    } finally {
                        setNoSet(false);
                    }
                }
            }

            public void keyTyped(final KeyEvent e) {
            }
        });
        this.editorIconLabel = new RenderLabel() {
            /**
             *
             */
            private static final long serialVersionUID = 1L;

            @Override
            public void setIcon(final Icon icon) {
                this.setVisible(icon != null);
                super.setIcon(icon);
            }
        };
        this.rendererField = createRendererField();
        this.rendererFont = this.rendererField.getFont();
        this.editorFont = this.editorField.getFont();
        this.editor = this.createEditorPanel();
        this.renderer = this.createRendererPanel();
        this.layoutEditor(this.editor, this.editorIconLabel, this.editorField);
        this.layoutRenderer(this.renderer, this.rendererIcon, this.rendererField);
        this.setRowSorter(new ExtDefaultRowSorter<E>() {
            @Override
            public int compare(final E o1, final E o2) {
                String o1s = ExtTextColumn.this.getStringValue(o1);
                String o2s = ExtTextColumn.this.getStringValue(o2);
                if (o1s == null) {
                    o1s = "";
                }
                if (o2s == null) {
                    o2s = "";
                }
                if (this.getSortOrderIdentifier() == ExtColumn.SORT_ASC) {
                    return o1s.compareToIgnoreCase(o2s);
                } else {
                    return o2s.compareToIgnoreCase(o1s);
                }
            }
        });
    }

    protected RenderLabel createRendererField() {
        return new RenderLabel() {
            /**
             *
             */
            private static final long serialVersionUID = 1L;

            @Override
            public void setIcon(final Icon icon) {
                ExtTextColumn.this.rendererIcon.setIcon(icon);
            }

            @Override
            public void setText(final String text) {
                if (text != null && text.equals(this.getText())) {
                    return;
                }
                if (text == null && this.getText() == null) {
                    return;
                }
                super.setText(text);
            }
        };
    }

    public void actionPerformed(final ActionEvent e) {
        this.editorField.removeActionListener(this);
        this.fireEditingStopped();
    }

    @Override
    protected void configureCurrentlyEditingComponent(final E value, final boolean isSelected, final int row, final int column) {
        this.editorIconLabel.setIcon(this.getIcon(value));
    }

    @Override
    public void configureEditorComponent(final E value, final boolean isSelected, final int row, final int column) {
        this.prepareColumn(value);
        this.editorField.removeActionListener(this);
        String str = this.getStringValue(value);
        if (str == null) {
            // under substance, setting setText(null) somehow sets the label
            // opaque.
            str = "";
        }
        this.editorField.setText(str);
        this.editorField.addActionListener(this);
        this.editorIconLabel.setIcon(this.getIcon(value));
    }

    @Override
    public void configureRendererComponent(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        this.prepareColumn(value);
        this.rendererIcon.setIcon(this.getIcon(value));
        String str = this.getStringValue(value);
        if (str == null) {
            // under substance, setting setText(null) somehow sets the label
            // opaque.
            str = "";
        }
        if (this.getTableColumn() != null) {
            try {
                this.rendererField.setText(clip(this.rendererField, this.rendererField.getFontMetrics(this.rendererField.getFont()), str, calculateClippingWidth(value)));
            } catch (Throwable e) {
                // fallback if org.appwork.swing.sunwrapper.SwingUtilities2
                // disappears someday
                e.printStackTrace();
                this.rendererField.setText(str);
            }
        } else {
            this.rendererField.setText(str);
        }
    }

    protected int calculateClippingWidth(E value) {
        return this.getTableColumn().getWidth() - this.rendererIcon.getPreferredSize().width - 5;
    }

    public JLabel getRendererIcon() {
        return rendererIcon;
    }

    /**
     * @param rendererField2
     * @param fontMetrics
     * @param str
     * @param i
     * @return
     */
    protected String clip(JComponent label, FontMetrics fontMetrics, String str, int width) {
        if (!clipingEnabled) {
            return str;
        }
        return org.appwork.sunwrapper.sun.swing.SwingUtilities2Wrapper.clipStringIfNecessary(label, fontMetrics, str, width);
    }

    protected MigPanel createEditorPanel() {
        return new MigPanel("ins 0 0 0 0", "[]5[grow,fill]", "[grow,fill]") {
            @Override
            public void requestFocus() {
                ExtTextColumn.this.editorField.requestFocus();
            }
        };
    }

    protected MigPanel createRendererPanel() {
        return new RendererMigPanel("ins 0 0 0 0", "[]0[grow,fill]", "[grow,fill]");
    }

    @Override
    public void focusGained(final FocusEvent e) {
        this.editorField.selectAll();
    }

    @Override
    public void focusLost(final FocusEvent e) {
        if (!e.isTemporary() || e.getOppositeComponent() == null) {
            /*
             * we check for temporary , because a rightclick menu will cause focus lost but editing should not stop
             *
             * we also check for oppositeComponent to stopEditing when we click outside the window
             */
            // do not save edits on focus lost.
            setNoSet(true);
            try {
                ExtTextColumn.this.stopCellEditing();
            } finally {
                setNoSet(false);
            }
        }
    }

    @Override
    public Object getCellEditorValue() {
        return this.editorField.getText();
    }

    /**
     * @return
     */
    @Override
    public JComponent getEditorComponent(final E value, final boolean isSelected, final int row, final int column) {
        return this.editor;
    }

    public JTextField getEditorField() {
        return this.editorField;
    }

    /*
     * @param value
     *
     * @return
     */
    protected Icon getIcon(final E value) {
        return null;
    }

    /**
     * @return
     */
    @Override
    public JComponent getRendererComponent(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        return this.renderer;
    }

    public abstract String getStringValue(E value);

    @Override
    protected String getTooltipText(final E obj) {
        return this.getStringValue(obj);
    }

    @Override
    public boolean isEditable(final E obj) {
        return false;
    }

    @Override
    public boolean isEnabled(final E obj) {
        return true;
    }

    @Override
    public boolean isSortable(final E obj) {
        return true;
    }

    /**
     * @param editor2
     * @param editorIconLabel2
     * @param editorField2
     */
    protected void layoutEditor(final JPanel editor, final JLabel editorIconLabel, final JTextField editorField) {
        editor.add(editorIconLabel, "hidemode 2");
        editor.add(editorField);
    }

    /**
     * @param rendererField
     * @param rendererIco
     * @param renderer2
     */
    protected void layoutRenderer(final JPanel renderer, final JLabel rendererIcon, final JLabel rendererField) {
        renderer.add(rendererIcon, "hidemode 2");
        renderer.add(rendererField);
    }

    @Override
    public boolean matchSearch(final E object, final Pattern pattern) {
        final String stringValue = this.getStringValue(object);
        if (stringValue == null) {
            return false;
        } else {
            return pattern.matcher(stringValue).matches();
        }
    }

    @Override
    public boolean onRenameClick(final MouseEvent e, final E obj) {
        if (this.isEditable(obj)) {
            this.startEditing(obj);
            return true;
        } else {
            return super.onRenameClick(e, obj);
        }
    }

    protected void prepareColumn(final E value) {
    }

    @Override
    public void resetEditor() {
        this.editor.setEnabled(true);
        this.editorField.setFont(this.editorFont);
        this.editorField.setBackground(null);
        this.editorIconLabel.setIcon(null);
        SwingUtils.setOpaque(this.editorIconLabel, false);
        SwingUtils.setOpaque(this.editorField, false);
    }

    @Override
    public void resetRenderer() {
        this.renderer.setEnabled(true);
        this.rendererField.setBorder(this.defaultBorder);
        this.rendererField.setOpaque(false);
        this.rendererField.setBackground(null);
        this.rendererField.setFont(this.rendererFont);
        this.renderer.setOpaque(false);
        this.rendererIcon.setIcon(null);
        SwingUtils.setOpaque(this.rendererIcon, false);
        SwingUtils.setOpaque(this.rendererField, false);
    }

    // /**
    // * @param value
    // */
    // protected void prepareLabel(final E value) {
    // }
    // /**
    // * @param label2
    // */
    // protected void prepareLabelForHelpText(final JLabel label) {
    //
    // label.setForeground(Color.lightGray);
    //
    // }
    // /**
    // * Should be overwritten to prepare the component for the TableCellEditor
    // * (e.g. setting tooltips)
    // */
    // protected void prepareTableCellEditorComponent(final JTextField text) {
    // }
    // protected void prepareTextfieldForHelpText(final JTextField tf) {
    //
    // tf.setForeground(Color.lightGray);
    //
    // }
    /**
     * Override to save value after editing
     *
     * @param value
     * @param object
     */
    protected void setStringValue(final String value, final E object) {
    }

    protected boolean noSet() {
        return noSet.get() > 0;
    }

    @Override
    public void setValue(final Object value, final E object) {
        if (!noSet()) {
            this.setStringValue((String) value, object);
        }
    }
}
