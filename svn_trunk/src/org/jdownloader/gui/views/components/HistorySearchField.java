package org.jdownloader.gui.views.components;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.FocusEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.CaretEvent;
import javax.swing.text.BadLocationException;

import org.appwork.swing.components.ExtTextField;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.packagehistorycontroller.AbstractSearchHistoryManager;
import org.jdownloader.images.NewTheme;
import org.jdownloader.updatev2.gui.LAFOptions;

/**
 * Text field with a search history: A popup with matching history entries, inline completion (like the address bar of a browser) and
 * the keyboard/mouse handling for both.
 *
 * Subclasses provide the history ({@link #getSearchHistoryManager()}), decide in which direction the popup opens
 * ({@link #isHistoryPopupUpwards()}) and react on changed text ({@link #onSearchTextChanged()}).
 */
public abstract class HistorySearchField extends ExtTextField {
    private static final long serialVersionUID        = 1L;
    private static final int  HISTORY_MAX_VISIBLE_ROWS = 10;
    private JPopupMenu        historyPopup;
    @SuppressWarnings("rawtypes")
    private JList             historyList;
    @SuppressWarnings("rawtypes")
    private DefaultListModel  historyModel;
    private HistoryCellRenderer historyRenderer;
    /* True if the last ESC key press only closed the history popup -> Do not clear the search field on key release */
    private boolean           escapeClosedHistoryPopup = false;
    /*
     * Text typed by the user while an inline completion is pending (completed part is appended and selected), null if there is no pending
     * completion. Filtering uses only this typed part until the completion gets accepted.
     */
    private volatile String   inlineCompletionPrefix   = null;
    private boolean           applyingInlineCompletion = false;
    /* Clear button (X) at the right end of the field, only visible while the field contains text */
    private final Image       clearImage               = NewTheme.I().getImage(IconKey.ICON_CLOSE, -1);
    private int               clearXPos                = -1;

    public HistorySearchField() {
        super();
        addMouseMotionListener(new MouseMotionAdapter() {
            @Override
            public void mouseMoved(MouseEvent e) {
                setCursor(Cursor.getPredefinedCursor(isOverClearButton(e) ? Cursor.HAND_CURSOR : Cursor.TEXT_CURSOR));
            }
        });
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (SwingUtilities.isLeftMouseButton(e) && isOverClearButton(e)) {
                    setText("");
                }
            }
        });
        addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                switch (e.getKeyCode()) {
                case KeyEvent.VK_DOWN:
                case KeyEvent.VK_UP:
                    if (!isHistoryPopupVisible()) {
                        updateHistoryPopup(true);
                    }
                    /* Opening via arrow key directly selects the entry closest to the field */
                    if (isHistoryPopupVisible()) {
                        moveHistorySelection(e.getKeyCode() == KeyEvent.VK_DOWN ? 1 : -1);
                    }
                    e.consume();
                    break;
                case KeyEvent.VK_DELETE:
                    /* Removes the selected history entry instead of deleting text */
                    if (getSelectedHistoryEntry() != null) {
                        removeHistoryEntry(historyList.getSelectedIndex());
                        e.consume();
                    }
                    break;
                case KeyEvent.VK_ENTER:
                    if (inlineCompletionPrefix != null) {
                        acceptInlineCompletion();
                        setCaretPosition(getDocument().getLength());
                    }
                    final String selected = getSelectedHistoryEntry();
                    if (selected != null) {
                        applyHistoryEntry(selected);
                    } else {
                        hideHistoryPopup();
                        addToSearchHistory();
                    }
                    e.consume();
                    break;
                case KeyEvent.VK_ESCAPE:
                    /* First ESC only closes the popup and removes a pending inline completion, next ESC clears the field */
                    final boolean popupWasVisible = isHistoryPopupVisible();
                    hideHistoryPopup();
                    final boolean completionRemoved = removeInlineCompletion();
                    escapeClosedHistoryPopup = popupWasVisible || completionRemoved;
                    break;
                default:
                    break;
                }
            }

            @Override
            public void keyReleased(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                    if (escapeClosedHistoryPopup) {
                        escapeClosedHistoryPopup = false;
                    } else {
                        setText("");
                    }
                }
            }

            @Override
            public void keyTyped(KeyEvent e) {
                final char c = e.getKeyChar();
                if (e.isControlDown() || e.isAltDown() || e.isMetaDown()) {
                    return;
                } else if (Character.isISOControl(c) && c != '\b' && c != KeyEvent.VK_DELETE) {
                    return;
                }
                /* Same as in browser address bars: No inline completion when deleting text */
                final boolean complete = c != '\b' && c != KeyEvent.VK_DELETE;
                /* The document has not been updated yet -> Complete and update popup afterwards */
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        if (complete) {
                            applyInlineCompletion();
                        }
                        updateHistoryPopup(true);
                    }
                });
            }
        });
    }

    /** The history used for the popup and the inline completion. */
    protected abstract AbstractSearchHistoryManager getSearchHistoryManager();

    /**
     * Fixed design decision per search field, derived from where the field is placed: true if the field sits at the bottom edge of the
     * window (popup opens above the field, oldest entry at the top, newest directly above the field), false if it sits at the top or in
     * the middle (popup opens below the field, newest entry directly below the field, oldest at the bottom). In both cases the newest
     * entry is the one closest to the field.
     */
    protected abstract boolean isHistoryPopupUpwards();

    /**
     * Override and return false if the subclass draws its own clear button.
     */
    protected boolean isClearButtonEnabled() {
        return true;
    }

    private boolean isClearButtonVisible() {
        return isClearButtonEnabled() && getText().length() > 0;
    }

    private boolean isOverClearButton(final MouseEvent e) {
        return isClearButtonVisible() && clearXPos > 0 && e.getX() >= clearXPos;
    }

    @Override
    protected void paintComponent(final Graphics g) {
        super.paintComponent(g);
        if (isClearButtonVisible()) {
            final int y = (getHeight() - clearImage.getHeight(null)) / 2;
            clearXPos = getWidth() - clearImage.getWidth(null) - y;
            g.drawImage(clearImage, clearXPos, y, null);
        } else {
            clearXPos = -1;
        }
    }

    /** Called whenever the text has been changed, by the user or by code. A pending inline completion has been handled before. */
    protected abstract void onSearchTextChanged();

    @Override
    public void onChanged() {
        if (!applyingInlineCompletion) {
            /* Text has been modified by the user -> Pending inline completion is gone */
            inlineCompletionPrefix = null;
        }
        onSearchTextChanged();
        if (isHistoryPopupVisible() && SwingUtilities.isEventDispatchThread()) {
            updateHistoryPopup(false);
        }
    }

    /** Returns the text used for filtering: Only the typed part while an inline completion is pending, else the whole text. */
    public String getFilterText() {
        final String prefix = inlineCompletionPrefix;
        return prefix != null ? prefix : getText();
    }

    /**
     * Browser address bar like inline completion: Appends the rest of the newest matching history entry and selects it, so typing on
     * replaces it. Only done if the caret is at the end of the text.
     */
    private void applyInlineCompletion() {
        if (inlineCompletionPrefix != null) {
            /* Completion is already pending */
            return;
        }
        final String text = getText();
        final int length = text.length();
        if (length == 0 || getCaretPosition() != length || getSelectionStart() != getSelectionEnd()) {
            return;
        }
        final String completion = getSearchHistoryManager().getCompletion(text);
        if (completion == null) {
            return;
        }
        applyingInlineCompletion = true;
        try {
            inlineCompletionPrefix = text;
            /* Keep the spelling typed by the user, only append the rest */
            getDocument().insertString(length, completion.substring(length), null);
            select(length, getDocument().getLength());
        } catch (BadLocationException e) {
            inlineCompletionPrefix = null;
        } finally {
            applyingInlineCompletion = false;
        }
    }

    /** Accepts a pending inline completion: The whole text gets used for filtering. */
    private void acceptInlineCompletion() {
        if (inlineCompletionPrefix == null) {
            return;
        }
        inlineCompletionPrefix = null;
        onSearchTextChanged();
        updateHistoryPopup(false);
    }

    /** Removes the completed part of a pending inline completion. Returns true if there was a pending completion. */
    protected boolean removeInlineCompletion() {
        final String prefix = inlineCompletionPrefix;
        if (prefix == null) {
            return false;
        }
        inlineCompletionPrefix = null;
        try {
            getDocument().remove(prefix.length(), getDocument().getLength() - prefix.length());
        } catch (BadLocationException e) {
            /* Document has been modified in the meantime */
        }
        return true;
    }

    @Override
    public void caretUpdate(final CaretEvent e) {
        super.caretUpdate(e);
        if (inlineCompletionPrefix == null || applyingInlineCompletion) {
            return;
        }
        /* Completion stays pending as long as exactly the completed part is selected, e.g. caret moved by arrow keys/End/click -> Accept */
        if (getSelectionStart() != inlineCompletionPrefix.length() || getSelectionEnd() != getDocument().getLength()) {
            acceptInlineCompletion();
        }
    }

    @Override
    public void focusLost(final FocusEvent arg0) {
        /* arg0 is null if focusLost is called by a subclass to update the help text only */
        if (arg0 != null) {
            hideHistoryPopup();
            if (!arg0.isTemporary()) {
                /* Not accepted completion must not end up in the history */
                removeInlineCompletion();
                addToSearchHistory();
            }
        }
        super.focusLost(arg0);
    }

    /** Adds the current search text to the search history. */
    protected void addToSearchHistory() {
        getSearchHistoryManager().add(getText());
    }

    protected boolean isHistoryPopupVisible() {
        return historyPopup != null && historyPopup.isVisible();
    }

    protected void hideHistoryPopup() {
        if (isHistoryPopupVisible()) {
            historyPopup.setVisible(false);
        }
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    private void createHistoryPopup() {
        historyModel = new DefaultListModel();
        historyList = new JList(historyModel) {
            @Override
            public boolean getScrollableTracksViewportWidth() {
                /* Long entries get truncated instead of hiding the remove icon */
                return true;
            }
        };
        historyList.setFocusable(false);
        historyList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        historyRenderer = new HistoryCellRenderer();
        historyList.setCellRenderer(historyRenderer);
        final MouseAdapter mouseAdapter = new MouseAdapter() {
            @Override
            public void mouseMoved(MouseEvent e) {
                final int index = getHistoryIndexAt(e);
                if (index >= 0 && index != historyList.getSelectedIndex()) {
                    historyList.setSelectedIndex(index);
                }
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                if (!SwingUtilities.isLeftMouseButton(e)) {
                    return;
                }
                final int index = getHistoryIndexAt(e);
                if (index < 0) {
                    return;
                }
                final String entry = String.valueOf(historyModel.getElementAt(index));
                final Rectangle bounds = historyList.getCellBounds(index, index);
                if (e.getX() >= bounds.x + bounds.width - historyRenderer.getRemoveAreaWidth()) {
                    removeHistoryEntry(index);
                } else {
                    applyHistoryEntry(entry);
                }
            }
        };
        historyList.addMouseListener(mouseAdapter);
        historyList.addMouseMotionListener(mouseAdapter);
        final JScrollPane scrollPane = new JScrollPane(historyList);
        scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.setFocusable(false);
        scrollPane.getVerticalScrollBar().setFocusable(false);
        historyPopup = new JPopupMenu();
        /* Non focusable popup -> Focus and key events stay in the search field (same as combobox popups) */
        historyPopup.setFocusable(false);
        historyPopup.setLayout(new BorderLayout());
        historyPopup.add(scrollPane, BorderLayout.CENTER);
    }

    private int getHistoryIndexAt(MouseEvent e) {
        final int index = historyList.locationToIndex(e.getPoint());
        if (index < 0) {
            return -1;
        }
        final Rectangle bounds = historyList.getCellBounds(index, index);
        if (bounds == null || !bounds.contains(e.getPoint())) {
            return -1;
        }
        return index;
    }

    /**
     * Index of the popup row closest to the search field, which is always the newest entry: The last row if the popup opens above the
     * field, else the first row.
     */
    private int getClosestHistoryIndex(final int size) {
        return isHistoryPopupUpwards() ? size - 1 : 0;
    }

    /**
     * Shows/refreshes the history popup with all history entries matching the current search text. Hides the popup if there are no
     * matching entries.
     *
     * @param open
     *            true = open the popup if it is not yet visible, false = only refresh an already visible popup
     */
    @SuppressWarnings("unchecked")
    protected void updateHistoryPopup(final boolean open) {
        if (!open && !isHistoryPopupVisible()) {
            return;
        } else if (!isShowing()) {
            hideHistoryPopup();
            return;
        }
        /* Newest entry first */
        final List<String> entries = getSearchHistoryManager().listMatching(getFilterText());
        if (entries.size() == 0) {
            hideHistoryPopup();
            return;
        }
        if (historyPopup == null) {
            createHistoryPopup();
        }
        final boolean upwards = isHistoryPopupUpwards();
        final Object selected = historyList.getSelectedValue();
        historyModel.clear();
        if (upwards) {
            /* Popup opens above the search field -> Oldest entry at the top, newest entry at the bottom (next to the search field) */
            for (int i = entries.size() - 1; i >= 0; i--) {
                historyModel.addElement(entries.get(i));
            }
        } else {
            /* Popup opens below the search field -> Newest entry at the top (next to the search field), oldest entry at the bottom */
            for (int i = 0; i < entries.size(); i++) {
                historyModel.addElement(entries.get(i));
            }
        }
        final int selectedIndex = selected != null ? historyModel.indexOf(selected) : -1;
        if (selectedIndex >= 0) {
            historyList.setSelectedIndex(selectedIndex);
        } else {
            historyList.clearSelection();
        }
        historyList.setVisibleRowCount(Math.min(entries.size(), HISTORY_MAX_VISIBLE_ROWS));
        final Insets insets = LAFOptions.getInstance().getExtension().customizePopupBorderInsets();
        historyPopup.setPreferredSize(null);
        final Dimension pref = historyPopup.getPreferredSize();
        historyPopup.setPreferredSize(new Dimension(getWidth() + insets.left + insets.right, pref.height));
        if (upwards) {
            historyPopup.show(this, -insets.left, -pref.height + insets.bottom);
        } else {
            historyPopup.show(this, -insets.left, getHeight() - insets.top);
        }
        /* Scroll after the popup has been laid out: to the selected entry, else to the newest entries next to the search field */
        final int scrollToIndex = selectedIndex >= 0 ? selectedIndex : getClosestHistoryIndex(historyModel.getSize());
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                if (scrollToIndex < historyModel.getSize()) {
                    historyList.ensureIndexIsVisible(scrollToIndex);
                }
            }
        });
    }

    /**
     * Moves the selection within the history popup. Moving beyond the first/last entry clears the selection, so the user can get back to
     * the typed text.
     */
    private void moveHistorySelection(final int delta) {
        final int size = historyModel.getSize();
        if (size == 0) {
            return;
        }
        int index;
        if (historyList.getSelectedIndex() < 0) {
            /* First selection is always the entry closest to the search field (newest), no matter which direction */
            index = getClosestHistoryIndex(size);
        } else {
            index = historyList.getSelectedIndex() + delta;
            if (index >= size) {
                index = -1;
            }
        }
        if (index < 0) {
            historyList.clearSelection();
        } else {
            historyList.setSelectedIndex(index);
            historyList.ensureIndexIsVisible(index);
        }
    }

    /**
     * Removes the history entry at the given popup index and moves the selection to its neighbor, so multiple entries can be removed in a
     * row. The neighbor is chosen by age: The next newer entry (towards the search field) if there is one, else the next older one.
     */
    private void removeHistoryEntry(final int index) {
        final int size = historyModel.getSize();
        if (index < 0 || index >= size) {
            return;
        }
        final String entry = String.valueOf(historyModel.getElementAt(index));
        /* Newer entries are closer to the search field: Below the entry if the popup opens upwards, else above it */
        final int newer = isHistoryPopupUpwards() ? index + 1 : index - 1;
        final int older = isHistoryPopupUpwards() ? index - 1 : index + 1;
        if (newer >= 0 && newer < size) {
            historyList.setSelectedIndex(newer);
        } else if (older >= 0 && older < size) {
            historyList.setSelectedIndex(older);
        } else {
            historyList.clearSelection();
        }
        getSearchHistoryManager().remove(entry);
        /* Refresh restores the selection of the neighbor by its value */
        updateHistoryPopup(false);
    }

    private String getSelectedHistoryEntry() {
        if (!isHistoryPopupVisible()) {
            return null;
        }
        final Object selected = historyList.getSelectedValue();
        return selected != null ? String.valueOf(selected) : null;
    }

    private void applyHistoryEntry(final String entry) {
        hideHistoryPopup();
        setText(entry);
        setCaretPosition(getDocument().getLength());
        getSearchHistoryManager().add(entry);
    }

    @SuppressWarnings("rawtypes")
    private static class HistoryCellRenderer extends JPanel implements ListCellRenderer {
        private static final long serialVersionUID = 1L;
        private final JLabel      apply            = new JLabel(NewTheme.I().getIcon(IconKey.ICON_WAIT, 14));
        private final JLabel      text             = new JLabel();
        private final Icon        removeIcon       = NewTheme.I().getIcon(IconKey.ICON_CLOSE, 10);
        private final JLabel      remove           = new JLabel(removeIcon);

        private HistoryCellRenderer() {
            super(new BorderLayout());
            setOpaque(true);
            setBorder(BorderFactory.createEmptyBorder(2, 5, 2, 5));
            apply.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 5));
            text.setHorizontalAlignment(SwingConstants.LEADING);
            remove.setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 0));
            /* Keep the space reserved while the remove icon is hidden so the text does not jump on hover */
            remove.setPreferredSize(remove.getPreferredSize());
            add(apply, BorderLayout.WEST);
            add(text, BorderLayout.CENTER);
            add(remove, BorderLayout.EAST);
        }

        /** Width of the clickable area at the right side of a row which removes the entry. */
        private int getRemoveAreaWidth() {
            return remove.getPreferredSize().width + getInsets().right;
        }

        @Override
        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
            text.setText(String.valueOf(value));
            /* Remove icon is only shown for the highlighted entry */
            remove.setIcon(isSelected ? removeIcon : null);
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                text.setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                text.setForeground(list.getForeground());
            }
            return this;
        }
    }
}
