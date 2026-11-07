package org.jdownloader.gui.views.components.packagetable;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPopupMenu;
import javax.swing.JRootPane;
import javax.swing.JViewport;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.plaf.ButtonUI;
import javax.swing.plaf.synth.SynthButtonUI;
import javax.swing.plaf.synth.SynthContext;
import javax.swing.plaf.synth.SynthLookAndFeel;
import javax.swing.plaf.synth.SynthPainter;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.NullsafeAtomicReference;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTHelper;
import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.filter.LinkgrabberFilterRuleWrapper;
import org.jdownloader.controlling.filter.RuleWrapper;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.packagehistorycontroller.AbstractSearchHistoryManager;
import org.jdownloader.gui.packagehistorycontroller.SearchHistoryManager;
import org.jdownloader.gui.views.components.HistorySearchField;
import org.jdownloader.gui.views.components.SearchCatInterface;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.updatev2.gui.LAFOptions;

import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;

public class SearchField<SearchCat extends SearchCatInterface, PackageType extends AbstractPackageNode<ChildType, PackageType>, ChildType extends AbstractPackageChildrenNode<PackageType>> extends HistorySearchField implements MouseMotionListener, MouseListener {
    /**
     *
     */
    private static final long                                                                  serialVersionUID        = -8079363840549073686L;
    private static final int                                                                   SIZE                    = 20;
    private Image                                                                              img;
    private DelayedRunnable                                                                    delayedFilter;
    private PackageControllerTable<PackageType, ChildType>                                     table2Filter;
    private volatile JLabel                                                                    label;
    private int                                                                                labelWidth;
    private Color                                                                              bgColor;
    private volatile SearchCat[]                                                               searchCategories;
    private Image                                                                              popIcon;
    private int                                                                                iconGap                 = 38;
    private Border                                                                             orgBorder;
    private Image                                                                              close;
    private int                                                                                closeXPos               = -1;
    private boolean                                                                            mouseoverClose          = false;
    private volatile boolean                                                                   closeEnabled            = false;
    private NullsafeAtomicReference<PackageControllerTableModelFilter<PackageType, ChildType>> appliedFilter           = new NullsafeAtomicReference<PackageControllerTableModelFilter<PackageType, ChildType>>(null);
    private AppAction                                                                          focusAction;
    private JButton                                                                            button;
    private boolean                                                                            synthButtonUIAccessable = Application.getJavaVersion() >= Application.JAVA17;

    public boolean isEmpty() {
        return appliedFilter.get() == null;
    }

    public SearchField(final PackageControllerTable<PackageType, ChildType> table2Filter, SearchCat defCategory) {
        super();
        button = new JButton();
        this.table2Filter = table2Filter;
        img = NewTheme.I().getImage(IconKey.ICON_SEARCH, SIZE);
        close = NewTheme.I().getImage(IconKey.ICON_CLOSE, -1);
        LAFOptions lafo = LAFOptions.getInstance();
        bgColor = (lafo.getColorForPanelHeaderBackground());
        setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        popIcon = NewTheme.I().getImage(IconKey.ICON_POPUPSMALL, -1);
        delayedFilter = new DelayedRunnable(150l, 2000l) {
            @Override
            public String getID() {
                return "SearchField" + table2Filter.getModel().getModelID();
            }

            @Override
            public void delayedrun() {
                updateFilter();
            }
        };
        orgBorder = getBorder();
        setBorder(BorderFactory.createCompoundBorder(orgBorder, BorderFactory.createEmptyBorder(0, 28, 0, 18)));
        addMouseMotionListener(this);
        addMouseListener(this);
        // setSelectedCategory(defCategory);,
        focusAction = new AppAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                requestFocus();
                selectAll();
            }
        };
    }

    public AppAction getFocusAction() {
        return focusAction;
    }

    public Image getPopIcon() {
        return popIcon;
    }

    public void setPopIcon(Image popIcon) {
        this.popIcon = popIcon;
    }

    @Override
    protected void onSearchTextChanged() {
        final String text = getText();
        closeEnabled = text != null && text.length() > 0;
        delayedFilter.run();
    }

    @Override
    protected AbstractSearchHistoryManager getSearchHistoryManager() {
        return SearchHistoryManager.getInstance();
    }

    /* The search fields of the download and linkgrabber tab are always at the very bottom of the JD window -> popup opens upwards */
    @Override
    protected boolean isClearButtonEnabled() {
        /* Draws its own clear button */
        return false;
    }

    @Override
    protected boolean isHistoryPopupUpwards() {
        return true;
    }

    protected final boolean isSynthButtonUIAvailable(ButtonUI buttonUI) {
        if (synthButtonUIAccessable) {
            return buttonUI instanceof SynthButtonUI;
        } else {
            return false;
        }
    }

    private Method synthLookAndFeelUpdateMethod = null;

    protected void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g;
        Composite comp = g2.getComposite();
        super.paintComponent(g);
        if (label != null) {
            if (isSynthButtonUIAvailable(button.getUI())) {
                try {
                    g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f));
                    button.setOpaque(false);
                    button.setBackground(null);
                    button.setSize(labelWidth + 5 + iconGap + 8 - 1, getHeight());
                    SynthContext context = ((SynthButtonUI) button.getUI()).getContext(button);
                    if (synthLookAndFeelUpdateMethod == null) {
                        synthLookAndFeelUpdateMethod = SynthLookAndFeel.class.getDeclaredMethod("update", new Class[] { SynthContext.class, Graphics.class });
                        synthLookAndFeelUpdateMethod.setAccessible(true);
                    }
                    synthLookAndFeelUpdateMethod.invoke(null, new Object[] { context, g2 });
                    SynthPainter painter = context.getStyle().getPainter(context);
                    g2.setClip(0, 0, labelWidth + 5 + iconGap + 8 + 1, getHeight());
                    painter.paintButtonBackground(context, g2, 0, 0, labelWidth + 5 + iconGap + 8 - 1 + 4, getHeight());
                    g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.4f));
                    g2.setClip(null);
                    g2.setColor(getBackground().darker().darker());
                    g2.drawLine(labelWidth + 5 + iconGap + 8, 1, labelWidth + iconGap + 5 + 8, getHeight() - 2);
                } catch (Throwable e) {
                    e.printStackTrace();
                    synthButtonUIAccessable = false;
                } finally {
                    g2.setComposite(comp);
                }
            } else {
                g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f));
                g2.setColor(bgColor);
                g2.fillRect(1, 1, labelWidth + 5 + iconGap + 8 - 1, getHeight() - 1);
                g2.setColor(getBackground().darker());
                g2.drawLine(labelWidth + 5 + iconGap + 8, 1, labelWidth + iconGap + 5 + 8, getHeight() - 1);
                g2.setComposite(comp);
            }
            final SearchCat cat = getSelectedCategory();
            if (cat != null) {
                cat.getIcon().paintIcon(this, g2, iconGap - 24, 3);
            }
            g2.translate(iconGap + 1, 0);
            label.getUI().paint(g2, label);
            g2.drawImage(popIcon, labelWidth + 3, (getHeight() - popIcon.getHeight(null)) / 2, null);
            // label.paintComponents(g2);
            g2.translate(-iconGap - 1, 0);
        } else {
            if (isSynthButtonUIAvailable(button.getUI())) {
                try {
                    g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f));
                    button.setOpaque(false);
                    button.setBackground(null);
                    button.setSize(labelWidth + 5 + iconGap + 8 - 1, getHeight());
                    SynthContext context = ((SynthButtonUI) button.getUI()).getContext(button);
                    if (synthLookAndFeelUpdateMethod == null) {
                        synthLookAndFeelUpdateMethod = SynthLookAndFeel.class.getDeclaredMethod("update", new Class[] { SynthContext.class, Graphics.class });
                        synthLookAndFeelUpdateMethod.setAccessible(true);
                    }
                    synthLookAndFeelUpdateMethod.invoke(null, new Object[] { context, g2 });
                    SynthPainter painter = context.getStyle().getPainter(context);
                    g2.setClip(0, 0, 26, getHeight());
                    painter.paintButtonBackground(context, g2, 0, 0, 26 + 3, getHeight());
                    g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.4f));
                    g2.setClip(null);
                    g2.setColor(getBackground().darker().darker());
                    g2.drawLine(26, 1, 26, getHeight() - 1);
                } catch (Throwable e) {
                    e.printStackTrace();
                    synthButtonUIAccessable = false;
                } finally {
                    g2.setComposite(comp);
                }
            } else {
                g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f));
                g2.setColor(bgColor);
                g2.fillRect(0, 0, 26, getHeight());
                g2.setColor(getBackground().darker());
                g2.drawLine(26, 1, 26, getHeight() - 1);
                g2.setComposite(comp);
            }
            g2.drawImage(img, 3, 3, 3 + SIZE, 3 + SIZE, 0, 0, SIZE, SIZE, null);
        }
        if (closeEnabled) {
            closeXPos = getWidth() - close.getWidth(null) - (getHeight() - close.getHeight(null)) / 2;
            g2.drawImage(close, closeXPos, (getHeight() - close.getHeight(null)) / 2, close.getWidth(null), close.getHeight(null), null);
        }
        // g2.dispose();
    }

    protected final boolean isMatching(List<Pattern> pattern, String string) {
        if (string != null) {
            for (final Pattern filterPattern : pattern) {
                if (filterPattern.matcher(string).find()) {
                    return true;
                }
            }
        }
        return false;
    }

    private final static String REGEX_FILTER = "regex:";
    /* True if the entered filter text is not a valid regular expression. The text is then shown in red. */
    private volatile boolean    invalidFilterPattern = false;

    private void setInvalidFilterPattern(final boolean invalid) {
        if (invalidFilterPattern != invalid) {
            invalidFilterPattern = invalid;
            repaint();
        }
    }

    /*
     * ExtTextField resets the foreground color on focus changes, therefore the color is overridden here instead of being set.
     */
    @Override
    public Color getForeground() {
        if (invalidFilterPattern) {
            return Color.RED;
        }
        return super.getForeground();
    }

    private void updateFilter() {
        String filterRegex = getFilterText();
        final boolean enabled = filterRegex != null && filterRegex.length() > 0;
        /*
         * Restore selection and scroll position after a search: Both are lost when the filter gets removed, because the table model is
         * rebuilt. Therefore the state is remembered when the search starts (no search -> search) and restored when it ends (search ->
         * no search). Changing the search text while searching, or switching the category, does not touch the snapshot.
         */
        if (enabled && !searchActive && beforeSearch == null) {
            /*
             * Search starts: Remember the table as it is right now, before the filter is applied. beforeSearch is only null here, if a
             * previous snapshot has not been restored yet (user started a new search too fast), then the older one is kept.
             */
            beforeSearch = createSearchSnapshot();
        }
        /* Search ends: The text has been cleared (ESC, close button, deleted manually) */
        final boolean searchEnded = !enabled && searchActive;
        searchActive = enabled;
        PackageControllerTableModelFilter<PackageType, ChildType> newFilter = null;
        if (enabled) {
            final List<Pattern> list = new ArrayList<Pattern>();
            try {
                setInvalidFilterPattern(false);
                if (JsonConfig.create(GeneralSettings.class).isFilterRegex() || filterRegex.startsWith(REGEX_FILTER)) {
                    if (filterRegex.startsWith(REGEX_FILTER)) {
                        filterRegex = filterRegex.substring(REGEX_FILTER.length());
                    }
                    list.add(LinkgrabberFilterRuleWrapper.createPattern(filterRegex, true, null));
                } else {
                    final String[] filters = filterRegex.split("\\|");
                    for (String filter : filters) {
                        list.add(LinkgrabberFilterRuleWrapper.createPattern(filter, false, RuleWrapper.AUTO_PATTERN_MODE.WILDCARD));
                    }
                }
                newFilter = getFilter(list, getSelectedCategory());
            } catch (final PatternSyntaxException e) {
                /* Invalid regular expression: no filter is applied, show the text in red */
                setInvalidFilterPattern(true);
            } catch (final Throwable e) {
                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                    e.printStackTrace();
                }
            }
        } else {
            setInvalidFilterPattern(false);
        }
        final PackageControllerTableModelFilter<PackageType, ChildType> oldFilter = appliedFilter.getAndSet(newFilter);
        if (oldFilter != null) {
            table2Filter.getModel().removeFilter(oldFilter);
        }
        if (newFilter != null) {
            table2Filter.getModel().addFilter(newFilter);
        }
        if (searchEnded) {
            /*
             * The filter has been removed above, but the table still shows the filtered rows until the model is recreated. Register the
             * restore now, before recreateModel, otherwise the event of the rebuild could be missed.
             */
            restoreAfterNextStructureChange();
        }
        table2Filter.getModel().recreateModel(true);
    }

    /* True while a search text is entered. Used to detect the start and the end of a search in updateFilter. */
    private volatile boolean        searchActive = false;
    /*
     * Selection and scroll position of the table from before the search started, null if nothing is remembered. Set when a search starts,
     * cleared when it has been restored.
     */
    private volatile SearchSnapshot beforeSearch = null;

    /**
     * State of the table from before a search. The scroll position is stored as the first visible node plus the pixel offset of its row,
     * not as plain y value, because the rows may change while searching (nodes added or removed).
     */
    private static class SearchSnapshot {
        /* Selected nodes. Nodes which do not exist anymore when restoring are ignored by the table model. */
        private final List<AbstractNode> selection;
        /* Node of the first (partly) visible row, null if the table was empty or not inside a scroll pane */
        private final AbstractNode       firstVisible;
        /* Pixels by which the first visible row was already scrolled out of the viewport at the top */
        private final int                firstVisibleOffset;

        private SearchSnapshot(final List<AbstractNode> selection, final AbstractNode firstVisible, final int firstVisibleOffset) {
            this.selection = selection;
            this.firstVisible = firstVisible;
            this.firstVisibleOffset = firstVisibleOffset;
        }
    }

    /**
     * Remembers selection and scroll position of the table. Called from updateFilter, which does not run in the EDT, but the table may
     * only be accessed in the EDT, therefore the values are read via EDTHelper (blocks until done).
     */
    private SearchSnapshot createSearchSnapshot() {
        return new EDTHelper<SearchSnapshot>() {
            @Override
            public SearchSnapshot edtRun() {
                final PackageControllerTableModel<PackageType, ChildType> model = table2Filter.getModel();
                /* Copy, so the snapshot does not depend on the list instance of the model */
                final List<AbstractNode> selection = new ArrayList<AbstractNode>(model.getSelectedObjects());
                AbstractNode firstVisible = null;
                int offset = 0;
                if (table2Filter.getParent() instanceof JViewport) {
                    final Point position = ((JViewport) table2Filter.getParent()).getViewPosition();
                    /* Row at the top edge of the viewport, may be only partly visible */
                    final int row = table2Filter.rowAtPoint(new Point(0, position.y));
                    if (row >= 0) {
                        firstVisible = model.getObjectbyRow(row);
                        offset = position.y - table2Filter.getCellRect(row, 0, true).y;
                    }
                }
                return new SearchSnapshot(selection, firstVisible, offset);
            }
        }.getReturnValue();
    }

    /**
     * Restores the remembered state as soon as the table has been rebuilt without the search filter.
     *
     * recreateModel only requests the rebuild, which then happens asynchronously. Restoring right away would therefore still act on the
     * filtered rows. The model fires the table property "structureChangedFired" after it has scheduled the replacement of the table data
     * in the EDT, so a task queued from that event runs after the new data is in place.
     */
    private void restoreAfterNextStructureChange() {
        final SearchSnapshot snapshot = beforeSearch;
        if (snapshot == null) {
            return;
        }
        table2Filter.addPropertyChangeListener("structureChangedFired", new PropertyChangeListener() {
            @Override
            public void propertyChange(final PropertyChangeEvent evt) {
                /* One shot: Only the first rebuild after the search ended is of interest */
                table2Filter.removePropertyChangeListener("structureChangedFired", this);
                /* The event is not fired in the EDT. Queued behind the table data replacement, which is already scheduled there. */
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        if (searchActive || beforeSearch != snapshot) {
                            /* A new search has been started in the meantime: Keep the snapshot for the next reset */
                            return;
                        }
                        beforeSearch = null;
                        restoreSearchSnapshot(snapshot);
                    }
                });
            }
        });
    }

    /**
     * Restores selection and scroll position. Has to run in the EDT after the table has been rebuilt. Whatever does not exist anymore
     * (removed or finished nodes) is skipped, the remaining state is restored as far as possible.
     */
    private void restoreSearchSnapshot(final SearchSnapshot snapshot) {
        final PackageControllerTableModel<PackageType, ChildType> model = table2Filter.getModel();
        /* Also clears the selection if nothing was selected before the search */
        model.setSelectedObjects(snapshot.selection);
        if (snapshot.firstVisible == null || !(table2Filter.getParent() instanceof JViewport)) {
            return;
        }
        final int row = model.getRowforObject(snapshot.firstVisible);
        if (row < 0) {
            /* The node is gone (or hidden in a collapsed package): Leave the scroll position as it is */
            return;
        }
        final JViewport viewport = (JViewport) table2Filter.getParent();
        final Point position = viewport.getViewPosition();
        /* Do not scroll beyond the end if the table has become shorter */
        final int maxY = Math.max(0, table2Filter.getPreferredSize().height - viewport.getExtentSize().height);
        position.y = Math.min(maxY, Math.max(0, table2Filter.getCellRect(row, 0, true).y + snapshot.firstVisibleOffset));
        viewport.setViewPosition(position);
    }

    protected PackageControllerTableModelFilter<PackageType, ChildType> getFilter(List<Pattern> pattern, SearchCat searchCat) {
        return null;
    }

    public void mouseDragged(MouseEvent e) {
    }

    public void mouseMoved(MouseEvent e) {
        updateCursor(e);
    }

    private void updateCursor(MouseEvent e) {
        if (!hasFocus()) {
            return;
        }
        if (label != null && e.getX() < labelWidth + 5 + iconGap + 8) {
            setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            setCaretColor(getBackground());
            focusLost(null);
            mouseoverClose = false;
        } else if (closeXPos > 0 && e.getX() > closeXPos) {
            mouseoverClose = true;
            setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            setCaretColor(getBackground());
            focusLost(null);
        } else {
            setCursor(Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR));
            setCaretColor(null);
            focusGained(null);
            mouseoverClose = false;
        }
    }

    public void focusGained(final FocusEvent arg0) {
        if (arg0 != null && arg0.getOppositeComponent() instanceof JRootPane) {
            return;
        }
        super.focusGained(arg0);
    }

    public void mouseClicked(MouseEvent e) {
        if (mouseoverClose && closeEnabled) {
            onResetPerformed();
        } else if (label != null && e.getX() < labelWidth + 5 + iconGap + 8) {
            onCategoryPopup();
        } else if (StringUtils.isEmpty(getText()) && SwingUtilities.isLeftMouseButton(e)) {
            updateHistoryPopup(true);
        }
    }

    private void onResetPerformed() {
        setText(null);
        onChanged();
    }

    private void onCategoryPopup() {
        JPopupMenu popup = new JPopupMenu();
        final SearchCat selectedCat = getSelectedCategory();
        for (final SearchCat sc : searchCategories) {
            popup.add(new AppAction() {
                private final SearchCat category = sc;
                {
                    setName(sc.getLabel());
                    setSmallIcon(sc.getIcon());
                }

                public void actionPerformed(ActionEvent e) {
                    if (category != selectedCat) {
                        setSelectedCategory(category);
                        focusLost(null);
                    }
                }
            });
        }
        Insets insets = LAFOptions.getInstance().getExtension().customizePopupBorderInsets();
        Dimension pref = popup.getPreferredSize();
        // pref.width = positionComp.getWidth() + ((Component)
        // e.getSource()).getWidth() + insets[1] + insets[3];
        popup.setPreferredSize(new Dimension(labelWidth + 5 + iconGap + 8 + insets.left + insets.left + insets.right, (int) pref.getHeight()));
        popup.show(this, -insets.left, -popup.getPreferredSize().height + insets.bottom);
    }

    public void mousePressed(MouseEvent e) {
    }

    public void mouseReleased(MouseEvent e) {
    }

    public void mouseEntered(MouseEvent e) {
        updateCursor(e);
    }

    public void mouseExited(MouseEvent e) {
        mouseoverClose = false;
    }

    protected final AtomicReference<SearchCat> selectedCategory = new AtomicReference<SearchCat>(null);

    public SearchCat getSelectedCategory() {
        return selectedCategory.get();
    }

    public void setSelectedCategory(SearchCat selectedCategory) {
        if (selectedCategory == null) {
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(new NullPointerException("selectedCategory null"));
        }
        final SearchCat old = this.selectedCategory.getAndSet(selectedCategory);
        if (old != selectedCategory) {
            onChanged();
        }
        if (label != null) {
            label.setText(selectedCategory.getLabel());
            setHelpText(selectedCategory.getHelpText());
        }
    }

    public void setCategories(SearchCat[] searchCategories) {
        this.searchCategories = searchCategories;
        label = new JLabel() {
            public boolean isShowing() {
                return true;
            }

            public boolean isVisible() {
                return true;
            }
        };
        final SearchCat preSel = getSelectedCategory();
        boolean found = false;
        for (SearchCat sc : searchCategories) {
            if (sc == preSel) {
                found = true;
            }
            label.setText(sc.getLabel());
            labelWidth = Math.max(label.getPreferredSize().width, labelWidth);
        }
        if (!found) {
            final SearchCat old = this.selectedCategory.getAndSet(searchCategories[0]);
            if (old != searchCategories[0]) {
                onChanged();
            }
        }
        label.setSize(labelWidth, 24);
        // label.setEnabled(false);
        setBorder(BorderFactory.createCompoundBorder(orgBorder, BorderFactory.createEmptyBorder(0, labelWidth + 14 + iconGap, 0, 18)));
        final SearchCat sel = getSelectedCategory();
        label.setText(sel.getLabel());
        setHelpText(sel.getHelpText());
    }
}
