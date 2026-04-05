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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.appwork.swing.MigPanel;

/**
 * Modal choice dialog: options as tiles in a wrapping grid backed by {@link JList} ({@link JList#HORIZONTAL_WRAP}). Preferred size fits the tile
 * grid (no extra empty area); beyond {@link #getMaxTileViewportHeight()} or screen width, scrollbars appear. Same {@link ComboBoxDialogInterface}
 * contract as {@link ComboBoxDialog}.
 */
public class ChooseFromTilesDialog extends AbstractChooseFromOptionsDialog {
    /**
     * Default upper bound for tile columns when estimating layout before a real viewport width exists (wrapping still adapts to width at runtime).
     */
    public static final int DEFAULT_MAX_COLUMNS = 5;

    private static final int   MIN_INNER_TEXT_WIDTH = 72;
    private static final int   MAX_INNER_TEXT_WIDTH = 360;
    private static final Color BORDER_NORMAL        = new Color(0xc0, 0xc0, 0xc0);
    private static final Color BORDER_SELECTED      = new Color(0x1a, 0x73, 0xe8);

    /** Tile list; {@link JList#HORIZONTAL_WRAP} with fixed cell size. */
    private JList              tileList;
    private JScrollPane        tileScrollPane;
    /**
     * When {@code closeOnOptionActivated} is true: {@link JList#getSelectedIndex()} at {@code mousePressed} (some LAFs briefly clear selection on
     * press; then this can be {@code -1}).
     */
    private int                tileListPressAnchorIndex = -1;
    /**
     * When {@code closeOnOptionActivated} is true: {@link JList#locationToIndex(java.awt.Point)} at {@code mousePressed} for the same event.
     */
    private int                tileListPressCellIndex   = -1;

    public ChooseFromTilesDialog(final int flag, final String title, final String question, final Object[] options, final int defaultSelection, final Icon icon, final String okText, final String cancelText) {
        this(flag, title, question, options, defaultSelection, icon, okText, cancelText, false);
    }

    public ChooseFromTilesDialog(final int flag, final String title, final String question, final Object[] options, final int defaultSelection, final Icon icon, final String okText, final String cancelText, final boolean closeOnOptionActivated) {
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

    /**
     * Maximum preferred viewport height for the tile area (vertical scroll beyond that). {@code <= 0} means unbounded (no height cap).
     */
    protected int getMaxTileViewportHeight() {
        try {
            final Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
            return Math.max(200, Math.min(640, (int) (screen.height * 0.55)));
        } catch (final Throwable e) {
            return 420;
        }
    }

    /**
     * Number of columns used for preferred width and row-count estimates. Default {@code min(optionCount, DEFAULT_MAX_COLUMNS)}. Override for a
     * single horizontal row of all tiles (e.g. OAuth provider strip).
     */
    protected int getTileColumnCountForLayout() {
        return Math.max(1, Math.min(this.options.length, DEFAULT_MAX_COLUMNS));
    }

    /**
     * When {@code true}, {@link JList#setVisibleRowCount(int)} is set to {@code 1} so the list prefers one horizontal row (may scroll horizontally
     * if the viewport is narrower).
     */
    protected boolean preferSingleTileRow() {
        return false;
    }

    /**
     * Preferred width hint for the tile scroll area before the dialog receives a real width ({@code <= 0} to derive from tile count and
     * {@link #getTileColumnCountForLayout()}).
     */
    protected int getPreferredTileViewportWidth() {
        final int cols = Math.max(1, this.getTileColumnCountForLayout());
        try {
            final Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
            return Math.max(280, Math.min(screen.width - 80, cols * 168 + 32));
        } catch (final Throwable e) {
            return Math.max(280, cols * 168 + 32);
        }
    }

    @Override
    public JComponent layoutDialogContent() {
        final JPanel contentpane = new MigPanel("ins 0,wrap 1", "[fill,grow]", "[][]");
        contentpane.add(this.createQuestionTextComponent(), "growx");
        contentpane.add(this.createTileGridPanel(), "growx,aligny top,wmin 0");
        return contentpane;
    }

    /**
     * Optional message above the tiles (same as {@link #getMessage()} / constructor {@code question}).
     */
    protected JComponent createQuestionTextComponent() {
        final JTextPane textpane = new JTextPane();
        textpane.setBorder(null);
        textpane.setBackground(null);
        textpane.setOpaque(false);
        textpane.setForeground(new JLabel().getForeground());
        textpane.putClientProperty("Synthetica.opaque", Boolean.FALSE);
        textpane.setText(this.message != null ? this.message : "");
        textpane.setEditable(false);
        return textpane;
    }

    /**
     * Scrollable wrapping tile grid backed by {@link JList}; assigns {@link #tileList} and {@link #tileScrollPane}.
     */
    protected JComponent createTileGridPanel() {
        final Font tileFont = UIManager.getFont("Button.font");
        final JLabel fmProbe = new JLabel();
        fmProbe.setFont(tileFont != null ? tileFont : new JLabel().getFont());
        final FontMetrics fm = fmProbe.getFontMetrics(fmProbe.getFont());
        final int innerTextWidthPx = this.computeInnerTextWidthPx(fm);

        final DefaultListModel model = new DefaultListModel();
        for (int i = 0; i < this.options.length; i++) {
            model.addElement(this.options[i]);
        }
        final TileListCellRenderer renderer = new TileListCellRenderer(innerTextWidthPx, tileFont);
        final JList measureList = new JList();
        int uniformW = 64;
        int uniformH = 48;
        for (int i = 0; i < this.options.length; i++) {
            final Component c = renderer.getListCellRendererComponent(measureList, this.options[i], i, false, false);
            final Dimension d = c.getPreferredSize();
            if (d.width > uniformW) {
                uniformW = d.width;
            }
            if (d.height > uniformH) {
                uniformH = d.height;
            }
        }
        uniformW = Math.max(64, uniformW);
        uniformH = Math.max(48, uniformH);

        this.tileList = new JList(model);
        this.tileList.setFont(tileFont != null ? tileFont : this.tileList.getFont());
        this.tileList.setLayoutOrientation(JList.HORIZONTAL_WRAP);
        this.tileList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        this.tileList.setCellRenderer(renderer);
        this.tileList.setFixedCellWidth(uniformW);
        this.tileList.setFixedCellHeight(uniformH);
        this.tileList.setOpaque(true);
        Color listBg = UIManager.getColor("List.background");
        if (listBg == null) {
            listBg = UIManager.getColor("Panel.background");
        }
        if (listBg != null) {
            this.tileList.setBackground(listBg);
        }
        this.tileList.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
        if (this.preferSingleTileRow()) {
            this.tileList.setVisibleRowCount(1);
        }

        if (this.selectionIndex >= 0 && this.selectionIndex < this.options.length) {
            this.tileList.setSelectedIndex(this.selectionIndex);
        }

        this.tileList.addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(final ListSelectionEvent e) {
                if (e.getValueIsAdjusting()) {
                    return;
                }
                final int i = ChooseFromTilesDialog.this.tileList.getSelectedIndex();
                if (i >= 0) {
                    ChooseFromTilesDialog.this.selectionIndex = i;
                    ChooseFromTilesDialog.this.tileList.repaint();
                    ChooseFromTilesDialog.this.onTileSelectionIndexChanged(i);
                    if (ChooseFromTilesDialog.this.closeOnOptionActivated && ChooseFromTilesDialog.this.okButton != null) {
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                if (ChooseFromTilesDialog.this.okButton != null) {
                                    ChooseFromTilesDialog.this.okButton.doClick();
                                }
                            }
                        });
                    }
                } else {
                    ChooseFromTilesDialog.this.selectionIndex = -1;
                }
            }
        });

        if (this.closeOnOptionActivated) {
            this.tileList.addMouseListener(new MouseAdapter() {
                @Override
                public void mousePressed(final MouseEvent e) {
                    if (!SwingUtilities.isLeftMouseButton(e)) {
                        ChooseFromTilesDialog.this.tileListPressAnchorIndex = -1;
                        ChooseFromTilesDialog.this.tileListPressCellIndex = -1;
                        return;
                    }
                    ChooseFromTilesDialog.this.tileListPressAnchorIndex = ChooseFromTilesDialog.this.tileList.getSelectedIndex();
                    ChooseFromTilesDialog.this.tileListPressCellIndex = ChooseFromTilesDialog.this.tileList.locationToIndex(e.getPoint());
                }

                @Override
                public void mouseReleased(final MouseEvent e) {
                    if (!SwingUtilities.isLeftMouseButton(e)) {
                        return;
                    }
                    final int releaseCell = ChooseFromTilesDialog.this.tileList.locationToIndex(e.getPoint());
                    if (releaseCell < 0) {
                        return;
                    }
                    final int selected = ChooseFromTilesDialog.this.tileList.getSelectedIndex();
                    if (releaseCell != selected) {
                        return;
                    }
                    final int pressCell = ChooseFromTilesDialog.this.tileListPressCellIndex;
                    if (pressCell >= 0 && pressCell != releaseCell) {
                        return;
                    }
                    final int anchorSel = ChooseFromTilesDialog.this.tileListPressAnchorIndex;
                    // Re-click on already selected row: no ListSelectionEvent. Some LAFs clear selection briefly on press (anchorSel == -1).
                    if (releaseCell != anchorSel && anchorSel >= 0) {
                        return;
                    }
                    ChooseFromTilesDialog.this.selectionIndex = releaseCell;
                    ChooseFromTilesDialog.this.onTileSelectionIndexChanged(releaseCell);
                    SwingUtilities.invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            if (ChooseFromTilesDialog.this.okButton != null) {
                                ChooseFromTilesDialog.this.okButton.doClick();
                            }
                        }
                    });
                }
            });
        } else {
            this.tileList.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(final MouseEvent e) {
                    if (e.getClickCount() == 2 && ChooseFromTilesDialog.this.okButton != null) {
                        final int idx = ChooseFromTilesDialog.this.tileList.locationToIndex(e.getPoint());
                        if (idx >= 0 && idx == ChooseFromTilesDialog.this.tileList.getSelectedIndex()) {
                            ChooseFromTilesDialog.this.okButton.doClick();
                        }
                    }
                }
            });
        }

        this.tileScrollPane = new JScrollPane(this.tileList, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        this.tileScrollPane.setBorder(BorderFactory.createEmptyBorder());
        this.tileScrollPane.getViewport().setOpaque(true);
        if (listBg != null) {
            this.tileScrollPane.getViewport().setBackground(listBg);
        }
        this.tileScrollPane.setMinimumSize(new Dimension(0, 0));

        final int cols = Math.max(1, this.getTileColumnCountForLayout());
        final int rows = this.options.length == 0 ? 0 : (this.options.length + cols - 1) / cols;
        /** Matches {@link javax.swing.JList} empty border 4+4 in {@link #createTileGridPanel()}. */
        final int listPad = 8;
        final int contentH = rows * uniformH + listPad;
        final int contentW = cols * uniformW + listPad;
        final int maxH = this.getMaxTileViewportHeight();
        int viewH = maxH > 0 && contentH > maxH ? maxH : contentH;
        if (this.options.length > 0 && viewH < uniformH + listPad) {
            viewH = uniformH + listPad;
        }
        int viewW = contentW;
        try {
            final int screenMaxW = Toolkit.getDefaultToolkit().getScreenSize().width - 80;
            if (viewW > screenMaxW) {
                viewW = screenMaxW;
            }
        } catch (final Throwable e) {
            final int fallbackCap = this.getPreferredTileViewportWidth();
            if (fallbackCap > 0 && viewW > fallbackCap) {
                viewW = fallbackCap;
            }
        }
        this.tileScrollPane.setPreferredSize(new Dimension(viewW, viewH));

        return this.tileScrollPane;
    }

    /**
     * Called after a tile becomes selected and {@link #selectionIndex} is updated (embed/host use).
     */
    protected void onTileSelectionIndexChanged(final int index) {
    }

    /**
     * Clears tile highlight and sets {@link #selectionIndex} to {@code -1} (embed mode).
     */
    public void clearTileSelection() {
        this.selectionIndex = -1;
        if (this.tileList != null) {
            this.tileList.clearSelection();
            this.tileList.repaint();
        }
    }

    /**
     * Selects one tile by index and updates display.
     */
    public void setSelectedOptionIndex(final int index) {
        if (this.tileList == null || index < 0 || index >= this.options.length) {
            return;
        }
        this.tileList.setSelectedIndex(index);
        this.selectionIndex = index;
        this.tileList.ensureIndexIsVisible(index);
        this.tileList.repaint();
    }

    /**
     * Legacy: tile chooser used {@link javax.swing.JToggleButton}; the implementation now uses {@link JList}. Use {@link #setTileChoicesEnabled(boolean)}.
     *
     * @return always {@code null}
     */
    public javax.swing.JToggleButton[] getTileButtons() {
        return null;
    }

    /**
     * Enables or disables choosing a tile (e.g. endpoint locked in a host dialog).
     */
    public void setTileChoicesEnabled(final boolean enabled) {
        if (this.tileList != null) {
            this.tileList.setEnabled(enabled);
        }
    }

    /**
     * Focus the currently selected tile, or the first tile (embed mode when the dialog frame is not shown).
     */
    public void focusDefaultTile() {
        if (this.tileList == null || this.options.length == 0) {
            return;
        }
        if (this.selectionIndex >= 0 && this.selectionIndex < this.options.length) {
            this.tileList.setSelectedIndex(this.selectionIndex);
            this.tileList.ensureIndexIsVisible(this.selectionIndex);
        }
        this.tileList.requestFocusInWindow();
    }

    @Override
    protected void initFocus(final JComponent focus) {
        if (this.tileList != null && this.options.length > 0) {
            this.focusDefaultTile();
        } else {
            super.initFocus(focus);
        }
    }

    private static void applyTileBorder(final JComponent b, final boolean selected) {
        final Border outer = BorderFactory.createLineBorder(selected ? BORDER_SELECTED : BORDER_NORMAL, selected ? 2 : 1);
        final Border pad = BorderFactory.createEmptyBorder(6, 8, 6, 8);
        b.setBorder(BorderFactory.createCompoundBorder(outer, pad));
    }

    /**
     * Inner HTML width so text can wrap; at least the widest word (capped at 360px) so one-line labels stay readable.
     */
    private int computeInnerTextWidthPx(final FontMetrics fm) {
        int maxWord = MIN_INNER_TEXT_WIDTH;
        for (int o = 0; o < this.options.length; o++) {
            final String s = this.elementToString(this.options[o]);
            if (s == null) {
                continue;
            }
            final String[] lines = s.split("\n", -1);
            for (int L = 0; L < lines.length; L++) {
                final String[] parts = lines[L].split("\\s+");
                for (int p = 0; p < parts.length; p++) {
                    if (parts[p].length() == 0) {
                        continue;
                    }
                    final int w = fm.stringWidth(parts[p]) + 10;
                    if (w > maxWord) {
                        maxWord = w;
                    }
                }
            }
        }
        if (maxWord > MAX_INNER_TEXT_WIDTH) {
            return MAX_INNER_TEXT_WIDTH;
        }
        return maxWord;
    }

    /**
     * HTML for tile label: centered; {@code innerWidthPx} controls wrapping.
     */
    private static String htmlEscapeCenteredMultiline(final String text, final int innerWidthPx, final Color fg) {
        if (text == null) {
            return "<html><table cellpadding=\"0\" cellspacing=\"0\" width=\"" + innerWidthPx + "\"><tr><td align=\"center\" valign=\"middle\"> </td></tr></table></html>";
        }
        final String[] lines = text.split("\n", -1);
        final StringBuilder inner = new StringBuilder();
        for (int L = 0; L < lines.length; L++) {
            if (L > 0) {
                inner.append("<br/>");
            }
            inner.append(htmlEscapeLine(lines[L]));
        }
        final String innerHtml = inner.toString();
        final String body;
        if (fg != null) {
            final String hx = String.format("#%06x", fg.getRGB() & 0xffffff);
            body = "<font color=\"" + hx + "\">" + innerHtml + "</font>";
        } else {
            body = innerHtml;
        }
        return "<html><table cellpadding=\"2\" cellspacing=\"0\" border=\"0\" width=\"" + innerWidthPx + "\"><tr><td align=\"center\" valign=\"middle\">" + body + "</td></tr></table></html>";
    }

    private static String htmlEscapeLine(final String text) {
        if (text == null) {
            return "";
        }
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < text.length(); i++) {
            final char c = text.charAt(i);
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

    /**
     * Renders one option as a tile (icon + HTML label, border by selection).
     */
    private final class TileListCellRenderer extends DefaultListCellRenderer {
        private static final long serialVersionUID = 1L;
        private final int         innerTextWidthPx;
        private final Font        tileFont;

        private TileListCellRenderer(final int innerTextWidthPx, final Font tileFont) {
            this.innerTextWidthPx = innerTextWidthPx;
            this.tileFont = tileFont;
        }

        @Override
        public Component getListCellRendererComponent(final JList list, final Object value, final int index, final boolean isSelected, final boolean cellHasFocus) {
            final JLabel c = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
            c.setHorizontalAlignment(SwingConstants.CENTER);
            c.setVerticalAlignment(SwingConstants.CENTER);
            final String raw = ChooseFromTilesDialog.this.elementToString(value);
            Color fg = UIManager.getColor("Label.foreground");
            if (fg == null && list != null) {
                fg = list.getForeground();
            }
            if (fg == null) {
                fg = Color.DARK_GRAY;
            }
            c.setForeground(fg);
            c.setText(htmlEscapeCenteredMultiline(raw, this.innerTextWidthPx, fg));
            final Icon tileIcon = ChooseFromTilesDialog.this.elementToIcon(value);
            c.setIcon(tileIcon);
            if (this.tileFont != null) {
                c.setFont(this.tileFont);
            }
            if (tileIcon != null) {
                c.setVerticalTextPosition(SwingConstants.BOTTOM);
                c.setHorizontalTextPosition(SwingConstants.CENTER);
            } else {
                c.setVerticalTextPosition(SwingConstants.CENTER);
                c.setHorizontalTextPosition(SwingConstants.CENTER);
            }
            c.setOpaque(true);
            Color bg = UIManager.getColor("Button.background");
            if (bg == null) {
                bg = list != null ? list.getBackground() : UIManager.getColor("Panel.background");
            }
            c.setBackground(bg != null ? bg : Color.LIGHT_GRAY);
            ChooseFromTilesDialog.applyTileBorder(c, isSelected);
            final String tip = ChooseFromTilesDialog.this.elementToTooltip(value);
            if (tip != null && tip.length() > 0) {
                c.setToolTipText(tip);
            } else {
                c.setToolTipText(null);
            }
            return c;
        }
    }
}
