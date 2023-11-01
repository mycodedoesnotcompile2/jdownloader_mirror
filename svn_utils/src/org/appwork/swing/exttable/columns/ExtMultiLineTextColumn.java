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

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.border.Border;

import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtTextArea;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.utils.swing.renderer.RendererMigPanel;

import net.miginfocom.swing.MigLayout;

public abstract class ExtMultiLineTextColumn<E> extends ExtColumn<E> implements ActionListener, FocusListener, CellHeightProvider {
    private static final long         serialVersionUID = 2114805529462086691L;
    protected RendererMigPanel        renderer;
    protected MigPanel                editor;
    private final Border              defaultBorder    = BorderFactory.createEmptyBorder(0, 5, 0, 5);
    private Color                     rendererForeground;
    private Color                     editorForeground;
    private Font                      rendererFont;
    private Font                      editorFont;
    protected JTextArea               txt;
    protected final AtomicInteger     noSet            = new AtomicInteger(0);
    private int                       lastRow;
    private HashMap<Integer, Integer> heights;
    private ExtTextArea               rendererLabel;
    private E                         lastValue;
    private JScrollPane               scrollPane;
    private int                       lastEditorRow;
    private E                         lastEditorValue;
    private Border                    editorBorder;
    private Color                     editorBackground;

    public void setNoSet(boolean b) {
        if (b) {
            noSet.incrementAndGet();
        } else if (noSet.get() > 0) {
            noSet.decrementAndGet();
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.exttable.columns.CellHeightProvider#adjustRowHeight(int, int)
     */
    @Override
    public int adjustRowHeight(int row, int height) {
        Integer ret = heights.get(row);
        if (ret != null) {
            return Math.max(ret.intValue(), height);
        }
        return height;
    }

    /**
     * @param string
     */
    public ExtMultiLineTextColumn(final String name) {
        this(name, null);
    }

    public ExtMultiLineTextColumn(final String name, final ExtTableModel<E> table) {
        super(name, table);
        heights = new HashMap<Integer, Integer>();
        this.txt = new JTextArea();
        final InputMap input = txt.getInputMap();
        final KeyStroke enter = KeyStroke.getKeyStroke("ENTER");
        final KeyStroke shiftEnter = KeyStroke.getKeyStroke("shift ENTER");
        Object oldEnter = input.get(enter);
        input.put(enter, "STOP_EDIT");
        input.put(shiftEnter, oldEnter);
        input.put(KeyStroke.getKeyStroke("ESCAPE"), "RESET_EDIT");
        final ActionMap actions = txt.getActionMap();
        actions.put("RESET_EDIT", new AbstractAction() {
            /**
             *
             */
            private static final long serialVersionUID = 1L;

            @Override
            public void actionPerformed(ActionEvent e) {
                setNoSet(true);
                try {
                    ExtMultiLineTextColumn.this.stopCellEditing();
                } finally {
                    setNoSet(false);
                }
            }
        });
        txt.setFocusTraversalKeysEnabled(false);
        this.scrollPane = new JScrollPane(this.txt);
        JTextField dummy = new JTextField();
        editorBorder = dummy.getBorder();
        editorBackground = dummy.getBackground();
        this.txt.addFocusListener(this);
        this.editor = new MigPanel(getEditorPanelLayout()) {
            public void validate() {
                super.validate();
                heights.put(lastEditorRow, Math.min(getMaxHeight(lastEditorValue), txt.getPreferredSize().height + 40));
            }

            @Override
            public void setBounds(int x, int y, int width, int height) {
                super.setBounds(x, y, width, height);
                scrollPane.setBounds(x, y, width, height);
            }
        };
        editor.add(scrollPane);
        this.renderer = new RendererMigPanel(getRendererPanelLayout()) {
            public void validate() {
                super.validate();
                heights.put(lastRow, Math.min(getMaxHeight(lastValue), getPreferredSize().height));
            }

            @Override
            public void setBounds(int x, int y, int width, int height) {
                super.setBounds(x, y, width, height);
                rendererLabel.setBounds(x, y, width, height);
            }
        };
        rendererLabel = new ExtTextArea();
        rendererLabel.setEditable(false);
        rendererLabel.setBorder(null);
        rendererLabel.setFocusable(false);
        rendererLabel.setLineWrap(true);
        rendererLabel.setWrapStyleWord(true);
        rendererLabel.setLabelMode(true);
        renderer.add(rendererLabel);
        rendererLabel.setOpaque(false);
        this.rendererForeground = this.rendererLabel.getForeground();
        this.rendererFont = this.rendererLabel.getFont();
        this.editorFont = this.txt.getFont();
        this.editorForeground = this.txt.getForeground();
        this.setRowSorter(new ExtDefaultRowSorter<E>() {
            @Override
            public int compare(final E o1, final E o2) {
                String o1s = ExtMultiLineTextColumn.this.getStringValue(o1);
                String o2s = ExtMultiLineTextColumn.this.getStringValue(o2);
                if (o1s == null) {
                    o1s = "";
                }
                if (o2s == null) {
                    o2s = "";
                }
                if (this.getSortOrderIdentifier() == ExtColumn.SORT_ASC) {
                    return o1s.compareTo(o2s);
                } else {
                    return o2s.compareTo(o1s);
                }
            }
        });
    }

    /**
     * @return
     */
    protected MigLayout getEditorPanelLayout() {
        // 7 pixel top boarder to have the same y position as buttons or single line text in the same row
        return new MigLayout("ins 0 0 0 0", "[grow,fill]", "4[grow,fill]1");
    }

    protected MigLayout getRendererPanelLayout() {
        // 7 pixel top boarder to have the same y position as buttons or single line text in the same row
        return new MigLayout("ins 0 0 0 0", "0[grow,fill]0", "7[]7");
    }

    public void actionPerformed(final ActionEvent e) {
        // this.txt.removeActionListener(this);
        this.fireEditingStopped();
    }

    @Override
    public void configureEditorComponent(final E value, final boolean isSelected, final int row, final int column) {
        // this.editor.removeActionListener(this);
        String str = this.getStringValue(value);
        if (str == null) {
            // under substance, setting setText(null) somehow sets the label
            // opaque.
            str = "";
        }
        lastEditorRow = row;
        lastEditorValue = value;
        this.txt.setText(str);
        // this.editor.addActionListener(this);
    }

    @Override
    public void configureRendererComponent(final E value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        String str = this.getStringValue(value);
        if (str == null) {
            // under substance, setting setText(null) somehow sets the label
            // opaque.
            str = "";
        }
        lastRow = row;
        lastValue = value;
        this.rendererLabel.setText(str);
    }

    @Override
    public void focusGained(final FocusEvent e) {
    }

    @Override
    public void focusLost(final FocusEvent e) {
        if (!e.isTemporary() || e.getOppositeComponent() == null) {
            /*
             * we check for temporary , because a rightclick menu will cause focus lost but editing should not stop
             *
             * we also check for oppositeComponent to stopEditing when we click outside the window
             */
            setNoSet(true);
            try {
                ExtMultiLineTextColumn.this.stopCellEditing();
            } finally {
                setNoSet(false);
            }
        }
    }

    @Override
    public Object getCellEditorValue() {
        return this.txt.getText();
    }

    /**
     * @return
     */
    @Override
    public JComponent getEditorComponent(final E value, final boolean isSelected, final int row, final int column) {
        return this.editor;
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
     * @param value
     * @return
     */
    private int getMaxHeight(final E value) {
        return 300;
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
        final String v = this.getStringValue(obj);
        if (v != null && v.length() > 0) {
            return v;
        } else {
            return null;
        }
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

    @Override
    public boolean matchSearch(final E object, final Pattern pattern) {
        final String value = this.getStringValue(object);
        if (value == null) {
            return false;
        } else {
            return pattern.matcher(value).matches();
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

    @Override
    public void resetEditor() {
        scrollPane.setBorder(editorBorder);
        scrollPane.setBackground(editorBackground);
        scrollPane.setOpaque(true);
        this.txt.setFont(this.editorFont);
        this.txt.setForeground(this.editorForeground);
        this.editor.setOpaque(false);
        this.editor.setBackground(null);
        this.editor.putClientProperty("Synthetica.opaque", Boolean.FALSE);
        this.txt.setOpaque(false);
        this.txt.putClientProperty("Synthetica.opaque", Boolean.FALSE);
        this.txt.setBackground(null);
    }

    @Override
    public void resetRenderer() {
        this.renderer.setBorder(this.defaultBorder);
        this.renderer.setOpaque(false);
        this.renderer.setBackground(null);
        this.renderer.setFont(this.rendererFont);
        this.renderer.setForeground(this.rendererForeground);
    }

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
