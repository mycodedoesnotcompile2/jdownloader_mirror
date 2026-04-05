/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
package org.appwork.utils.swing.dialog;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.appwork.swing.MigPanel;

/**
 * Modal choice dialog: options in a scrollable single-column list sized to show as many rows as fit on screen (scrollbar when needed).
 * Same {@link ComboBoxDialogInterface} contract as {@link ComboBoxDialog}.
 */
public class ChooseFromListDialog extends AbstractChooseFromOptionsDialog {
    private static final int ROW_HEIGHT               = 26;
    private static final int MIN_LIST_WIDTH         = 320;
    private static final int DIALOG_HEIGHT_HEADROOM = 220;

    private JList<String> list;
    /**
     * When {@code closeOnOptionActivated}: {@link JList#getSelectedIndex()} at {@code mousePressed} (some LAFs briefly clear selection on press).
     */
    private int             listPressAnchorSelection = -1;
    /**
     * When {@code closeOnOptionActivated}: {@link JList#locationToIndex(java.awt.Point)} at {@code mousePressed}.
     */
    private int             listPressCellIndex       = -1;

    public ChooseFromListDialog(final int flag, final String title, final String question, final Object[] options, final int defaultSelection, final Icon icon, final String okText, final String cancelText) {
        this(flag, title, question, options, defaultSelection, icon, okText, cancelText, false);
    }

    public ChooseFromListDialog(final int flag, final String title, final String question, final Object[] options, final int defaultSelection, final Icon icon, final String okText, final String cancelText, final boolean closeOnOptionActivated) {
        super(flag, title, question, options, defaultSelection, icon, okText, cancelText, closeOnOptionActivated);
    }

    /**
     * For automation (e.g. console): fix choice without displaying OK. See {@link AbstractChooseFromOptionsDialog#applyHeadlessCommit(int)}.
     */
    public void setHeadlessCommittedIndex(final int index) {
        this.applyHeadlessCommit(index);
    }

    /** Raw option objects (same order as indices). */
    public Object[] getChooserOptions() {
        return this.options;
    }

    @Override
    public JComponent layoutDialogContent() {
        final JPanel contentpane = new MigPanel("ins 0,wrap 1", "[fill,grow]", "[][]");
        final JTextPane textpane = new JTextPane();
        textpane.setBorder(null);
        textpane.setBackground(null);
        textpane.setOpaque(false);
        textpane.setForeground(new JLabel().getForeground());
        textpane.putClientProperty("Synthetica.opaque", Boolean.FALSE);
        textpane.setText(this.message != null ? this.message : "");
        textpane.setEditable(false);
        contentpane.add(textpane);

        final String[] labels = new String[this.options.length];
        for (int i = 0; i < this.options.length; i++) {
            labels[i] = this.elementToString(this.options[i]);
        }
        this.list = new JList<String>(labels);
        this.list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        final int rowH = this.computeListRowHeight();
        this.list.setFixedCellHeight(rowH);
        this.list.setVisibleRowCount(computePreferredVisibleRows(this.options.length, rowH));
        this.list.setCellRenderer(new DefaultListCellRenderer() {
            private static final long serialVersionUID = 1L;

            @Override
            public java.awt.Component getListCellRendererComponent(final JList<?> list, final Object value, final int index, final boolean isSelected, final boolean cellHasFocus) {
                final JLabel c = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (index >= 0 && index < ChooseFromListDialog.this.options.length) {
                    final String plain = ChooseFromListDialog.this.elementToString(ChooseFromListDialog.this.options[index]);
                    Color fg = isSelected ? list.getSelectionForeground() : list.getForeground();
                    if (fg == null) {
                        fg = new JLabel().getForeground();
                    }
                    c.setForeground(fg);
                    c.setText(ChooseFromListDialog.listCellDisplayText(plain, fg));
                    c.setIcon(ChooseFromListDialog.this.elementToIcon(ChooseFromListDialog.this.options[index]));
                    c.setIconTextGap(10);
                    c.setHorizontalTextPosition(SwingConstants.RIGHT);
                    final String tip = ChooseFromListDialog.this.elementToTooltip(ChooseFromListDialog.this.options[index]);
                    c.setToolTipText(tip != null && tip.length() > 0 ? tip : null);
                }
                return c;
            }
        });
        this.list.setSelectedIndex(this.selectionIndex);
        this.list.addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(final ListSelectionEvent e) {
                if (e.getValueIsAdjusting()) {
                    return;
                }
                final int i = ChooseFromListDialog.this.list.getSelectedIndex();
                if (i >= 0) {
                    ChooseFromListDialog.this.selectionIndex = i;
                    ChooseFromListDialog.this.list.repaint();
                    if (ChooseFromListDialog.this.closeOnOptionActivated && ChooseFromListDialog.this.okButton != null) {
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                if (ChooseFromListDialog.this.okButton != null) {
                                    ChooseFromListDialog.this.okButton.doClick();
                                }
                            }
                        });
                    }
                } else {
                    ChooseFromListDialog.this.selectionIndex = -1;
                }
            }
        });
        if (this.closeOnOptionActivated) {
            this.list.addMouseListener(new MouseAdapter() {
                @Override
                public void mousePressed(final MouseEvent e) {
                    if (!SwingUtilities.isLeftMouseButton(e)) {
                        ChooseFromListDialog.this.listPressAnchorSelection = -1;
                        ChooseFromListDialog.this.listPressCellIndex = -1;
                        return;
                    }
                    ChooseFromListDialog.this.listPressAnchorSelection = ChooseFromListDialog.this.list.getSelectedIndex();
                    ChooseFromListDialog.this.listPressCellIndex = ChooseFromListDialog.this.list.locationToIndex(e.getPoint());
                }

                @Override
                public void mouseReleased(final MouseEvent e) {
                    if (!SwingUtilities.isLeftMouseButton(e)) {
                        return;
                    }
                    final int releaseCell = ChooseFromListDialog.this.list.locationToIndex(e.getPoint());
                    if (releaseCell < 0) {
                        return;
                    }
                    final int selected = ChooseFromListDialog.this.list.getSelectedIndex();
                    if (releaseCell != selected) {
                        return;
                    }
                    final int pressCell = ChooseFromListDialog.this.listPressCellIndex;
                    if (pressCell >= 0 && pressCell != releaseCell) {
                        return;
                    }
                    final int anchorSel = ChooseFromListDialog.this.listPressAnchorSelection;
                    if (releaseCell != anchorSel && anchorSel >= 0) {
                        return;
                    }
                    ChooseFromListDialog.this.selectionIndex = releaseCell;
                    SwingUtilities.invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            if (ChooseFromListDialog.this.okButton != null) {
                                ChooseFromListDialog.this.okButton.doClick();
                            }
                        }
                    });
                }
            });
        } else {
            this.list.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(final MouseEvent e) {
                    final int i = ChooseFromListDialog.this.list.locationToIndex(e.getPoint());
                    if (i < 0) {
                        return;
                    }
                    ChooseFromListDialog.this.list.setSelectedIndex(i);
                    ChooseFromListDialog.this.selectionIndex = i;
                    if (e.getClickCount() == 2 && ChooseFromListDialog.this.okButton != null) {
                        ChooseFromListDialog.this.okButton.doClick();
                    }
                }
            });
        }

        final JScrollPane scroll = new JScrollPane(this.list);
        scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        scroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        scroll.setBorder(BorderFactory.createLineBorder(new java.awt.Color(0xd0, 0xd0, 0xd0)));

        final Dimension viewport = this.list.getPreferredScrollableViewportSize();
        final int w = Math.max(MIN_LIST_WIDTH, viewport.width + 24);
        final Insets bi = scroll.getBorder() != null ? scroll.getBorder().getBorderInsets(scroll) : new Insets(0, 0, 0, 0);
        scroll.setPreferredSize(new Dimension(w, viewport.height + bi.top + bi.bottom));
        contentpane.add(scroll, "growx,aligny top");
        return contentpane;
    }

    @Override
    protected void initFocus(final JComponent focus) {
        if (this.list != null) {
            this.list.requestFocusInWindow();
        } else {
            super.initFocus(focus);
        }
    }

    /**
     * Number of rows to show without inner scrolling when possible; capped by screen height so the dialog stays usable.
     */
    public static int computePreferredVisibleRows(final int itemCount) {
        return computePreferredVisibleRows(itemCount, ROW_HEIGHT);
    }

    /**
     * @param rowHeightPx
     *            {@link JList#setFixedCellHeight(int)} used for this list (icons may increase it)
     */
    public static int computePreferredVisibleRows(final int itemCount, final int rowHeightPx) {
        if (itemCount <= 0) {
            return 6;
        }
        final int rh = rowHeightPx > 0 ? rowHeightPx : ROW_HEIGHT;
        int maxByScreen = 18;
        try {
            final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
            final Rectangle bounds = ge.getMaximumWindowBounds();
            final int avail = bounds.height - DIALOG_HEIGHT_HEADROOM;
            maxByScreen = Math.max(6, avail / rh);
        } catch (final Throwable e) {
            // ignore
        }
        return Math.min(itemCount, maxByScreen);
    }

    private int computeListRowHeight() {
        int h = ROW_HEIGHT;
        for (final Object o : this.options) {
            final Icon ic = this.elementToIcon(o);
            if (ic != null) {
                h = Math.max(h, ic.getIconHeight() + 10);
            }
            final String t = this.elementToString(o);
            if (t != null && t.indexOf('\n') >= 0) {
                h = Math.max(h, ROW_HEIGHT + 18);
            }
        }
        return h;
    }

    /**
     * @param fg
     *            when non-null and {@code plain} is multi-line, embedded in HTML so selection styling does not leave white-on-white text.
     */
    private static String listCellDisplayText(final String plain, final Color fg) {
        if (plain == null) {
            return "";
        }
        if (plain.indexOf('\n') < 0) {
            return plain;
        }
        final String[] lines = plain.split("\n", -1);
        final StringBuilder inner = new StringBuilder();
        for (int i = 0; i < lines.length; i++) {
            if (i > 0) {
                inner.append("<br/>");
            }
            inner.append(htmlEscapeMinimal(lines[i]));
        }
        if (fg != null) {
            final String hx = String.format("#%06x", fg.getRGB() & 0xffffff);
            return "<html><font color=\"" + hx + "\">" + inner.toString() + "</font></html>";
        }
        return "<html>" + inner.toString() + "</html>";
    }

    private static String htmlEscapeMinimal(final String s) {
        if (s == null) {
            return "";
        }
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            final char c = s.charAt(i);
            if (c == '&') {
                sb.append("&amp;");
            } else if (c == '<') {
                sb.append("&lt;");
            } else if (c == '>') {
                sb.append("&gt;");
            } else if (c == '"') {
                sb.append("&quot;");
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }
}
