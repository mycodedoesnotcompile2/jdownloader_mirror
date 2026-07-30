package org.jdownloader.gui.views.linkgrabber;

import java.awt.AWTKeyStroke;
import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.LayoutManager;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.DropMode;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JPopupMenu;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.TransferHandler;

import org.appwork.swing.MigPanel;
import org.appwork.swing.components.circlebar.CircledProgressBar;
import org.appwork.swing.components.circlebar.ImagePainter;
import org.appwork.swing.exttable.DropHighlighter;
import org.appwork.swing.exttable.ExtCheckBoxMenuItem;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtOverlayRowHighlighter;
import org.appwork.swing.exttable.ExtTable;
import org.appwork.uio.UIOManager;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.queue.Queue;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.contextmenu.CustomizableAppAction;
import org.jdownloader.controlling.contextmenu.MenuContainer;
import org.jdownloader.controlling.contextmenu.MenuContainerRoot;
import org.jdownloader.controlling.contextmenu.MenuItemData;
import org.jdownloader.controlling.contextmenu.MenuLink;
import org.jdownloader.controlling.contextmenu.SeparatorData;
import org.jdownloader.controlling.contextmenu.gui.ExtPopupMenu;
import org.jdownloader.controlling.contextmenu.gui.MenuBuilder;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTableModelData;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTableModelFilter;
import org.jdownloader.gui.views.downloads.table.HorizontalScrollbarAction;
import org.jdownloader.gui.views.linkgrabber.bottombar.MenuManagerLinkgrabberTabBottombar;
import org.jdownloader.gui.views.linkgrabber.contextmenu.ConfirmLinksContextAction;
import org.jdownloader.gui.views.linkgrabber.contextmenu.MenuManagerLinkgrabberTableContext;
import org.jdownloader.gui.views.linkgrabber.quickfilter.FilterTable;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.logging.LogController;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;
import org.jdownloader.settings.staticreferences.CFG_GUI;
import org.jdownloader.translate._JDT;
import org.jdownloader.updatev2.gui.LAFOptions;

import jd.controlling.TaskQueue;
import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcollector.LinkCollector.ConfirmLinksSettings;
import jd.controlling.linkcollector.LinkCollector.ConfirmLinksSettings.SwitchToDownloadlistBehavior;
import jd.controlling.linkcollector.LinkCollector.MoveLinksMode;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.linkcrawler.CrawledPackage.TYPE;
import jd.controlling.packagecontroller.AbstractNode;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.WarnLevel;
import jd.plugins.DownloadLink.AvailableStatus;
import net.miginfocom.swing.MigLayout;

public class LinkGrabberTable extends PackageControllerTable<CrawledPackage, CrawledLink> {
    private static final long          serialVersionUID   = 8843600834248098174L;
    private HashMap<KeyStroke, Action> shortCutActions;
    private LogSource                  logger;
    private static LinkGrabberTable    INSTANCE;
    private final boolean              dupeManagerEnabled = CFG_GENERAL.CFG.isDupeManagerEnabled();

    public LinkGrabberTable(LinkGrabberPanel linkGrabberPanel, final LinkGrabberTableModel tableModel) {
        super(tableModel);
        INSTANCE = this;
        this.addRowHighlighter(new DropHighlighter(null, new Color(27, 164, 191, 75)));
        if (dupeManagerEnabled) {
            this.addRowHighlighter(new ExtOverlayRowHighlighter(null, LAFOptions.getInstance().getColorForLinkgrabberDupeHighlighter()) {
                @Override
                public boolean doHighlight(ExtTable<?> extTable, int row) {
                    final AbstractNode object = tableModel.getObjectbyRow(row);
                    if (object != null && object instanceof CrawledLink) {
                        return DownloadController.getInstance().hasDownloadLinkByID(((CrawledLink) object).getLinkID());
                    }
                    return false;
                }
            });
        }
        this.setTransferHandler(new LinkGrabberTableTransferHandler(this));
        this.setDragEnabled(true);
        this.setDropMode(DropMode.ON_OR_INSERT_ROWS);
        logger = LogController.getInstance().getLogger(LinkGrabberTable.class.getName());
        final MigPanel loaderPanel = new MigPanel("ins 0,wrap 1", "[grow,fill]", "[grow,fill][]");
        // loaderPanel.setPreferredSize(new Dimension(200, 200));
        loaderPanel.setOpaque(false);
        loaderPanel.setBackground(null);
        final CircledProgressBar loader = new CircledProgressBar() {
            public int getAnimationFPS() {
                return 25;
            }
        };
        loader.setValueClipPainter(new ImagePainter(new AbstractIcon(IconKey.ICON_BOTTY_ROBOT, 256), 1.0f));
        loader.setNonvalueClipPainter(new ImagePainter(new AbstractIcon(IconKey.ICON_BOTTY_ROBOT, 256), 0.1f));
        ((ImagePainter) loader.getValueClipPainter()).setBackground(null);
        ((ImagePainter) loader.getValueClipPainter()).setForeground(null);
        loader.setIndeterminate(true);
        loaderPanel.add(loader);
        final JProgressBar ph = new JProgressBar();
        ph.setString(_GUI.T.DownloadsTable_DownloadsTable_init_plugins());
        LinkCollector.CRAWLERLIST_LOADED.executeWhenReached(new Runnable() {
            @Override
            public void run() {
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        ph.setString(_GUI.T.LinkGrabberTable_LinkGrabberTable_object_wait_for_loading_links());
                    }
                };
            }
        });
        ph.setStringPainted(true);
        ph.setIndeterminate(true);
        loaderPanel.add(ph, "alignx center");
        // loaderPanel.setSize(400, 400);
        final LayoutManager orgLayout = getLayout();
        final Component rendererPane = getComponent(0);
        setLayout(new MigLayout("ins 0", "[grow]", "[grow]"));
        removeAll();
        add(loaderPanel, "alignx center,aligny 20%");
        LinkCollector.CRAWLERLIST_LOADED.executeWhenReached(new Runnable() {
            @Override
            public void run() {
                removeLoaderPanel(loaderPanel, orgLayout, rendererPane);
            }
        });
    }

    protected void processMouseEvent(final MouseEvent e) {
        // a left-click with mouse on empty space under the rows selects last row to improve user experience
        // like dragging the mouse to select rows
        if (e.getID() == MouseEvent.MOUSE_PRESSED) {
            if (SwingUtilities.isLeftMouseButton(e) && !isExpandToggleEvent(e)) {
                if (getSelectionModel().getValueIsAdjusting()) {
                    if (rowAtPoint(e.getPoint()) < 0) {
                        final int rowCount = this.getRowCount();
                        if (rowCount > 0) {
                            this.getSelectionModel().setAnchorSelectionIndex(rowCount - 1);
                        }
                    }
                }
            }
        } else if (e.getID() == MouseEvent.MOUSE_RELEASED) {
            if (e.getButton() == MouseEvent.BUTTON2) {
                if ((e.getModifiers() & InputEvent.CTRL_MASK) == 0) {
                    if ((e.getModifiers() & InputEvent.SHIFT_MASK) == 0) {
                        final ConfirmLinksSettings cls = new ConfirmLinksSettings(MoveLinksMode.MANUAL);
                        cls.setClearLinkgrabberlistOnConfirm(false);
                        cls.setSwitchToDownloadlistBehavior(SwitchToDownloadlistBehavior.NEVER);
                        cls.setForceDownloads(Boolean.FALSE);
                        final int row = rowAtPoint(e.getPoint());
                        final AbstractNode obj = this.getModel().getObjectbyRow(row);
                        final SelectionInfo<CrawledPackage, CrawledLink> si;
                        if (LinkGrabberTable.this.isRowSelected(row)) {
                            // clicked on a selected row. let's confirm them all
                            si = getSelectionInfo(true, true);
                        } else {
                            // clicked on a not-selected row. only add the context item
                            si = new SelectionInfo<CrawledPackage, CrawledLink>(obj);
                        }
                        ConfirmLinksContextAction.confirmSelection(si, cls);
                    }
                }
            }
        }
        super.processMouseEvent(e);
    }

    protected void fireColumnModelUpdate() {
        super.fireColumnModelUpdate();
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                boolean alllocked = true;
                for (ExtColumn<?> c : getModel().getColumns()) {
                    if (c.isResizable()) {
                        alllocked = false;
                        break;
                    }
                }
                if (alllocked) {
                    JScrollPane sp = (JScrollPane) getParent().getParent();
                    CFG_GUI.HORIZONTAL_SCROLLBARS_IN_LINKGRABBER_TABLE_ENABLED.setValue(true);
                    setColumnSaveID("hBAR");
                    setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                    sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
                    sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                }
            }
        };
    }

    protected void removeLoaderPanel(final MigPanel loaderPanel, final LayoutManager orgLayout, final Component rendererPane) {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                remove(loaderPanel);
                setLayout(orgLayout);
                loaderPanel.setVisible(false);
                add(rendererPane);
                revalidate();
                repaint();
            }
        };
    }

    public void sortPackageChildren(ExtDefaultRowSorter<AbstractNode> rowSorter, String nextSortIdentifier) {
        // TODO:
        // set LinkGrabberTableModel.setTRistate....to false and implement sorter here
    }

    @Override
    protected boolean onSingleClick(MouseEvent e, AbstractNode obj) {
        if (dupeManagerEnabled && obj != null && obj instanceof CrawledLink && DownloadController.getInstance().hasDownloadLinkByID(((CrawledLink) obj).getLinkID())) {
            JDGui.help(_GUI.T.LinkGrabberTable_onSingleClick_dupe_title(), _GUI.T.LinkGrabberTable_onSingleClick_dupe_msg(), new AbstractIcon(IconKey.ICON_COPY, 32));
        }
        return super.onSingleClick(e, obj);
    }

    protected boolean onHeaderSortClick(final MouseEvent event, final ExtColumn<AbstractNode> oldColumn, final String oldIdentifier, ExtColumn<AbstractNode> newColumn) {
        if (((LinkGrabberTableModel) getModel()).isTristateSorterEnabled()) {
            return false;
        }
        //
        if (JDGui.bugme(WarnLevel.NORMAL)) {
            UIOManager.I().showConfirmDialog(UIOManager.LOGIC_DONT_SHOW_AGAIN_IGNORES_CANCEL | Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, _JDT.T.getNextSortIdentifier_sort_warning_rly_title_(), _JDT.T.getNextSortIdentifier_sort_warning_rly_msg(newColumn.getName()), new AbstractIcon(IconKey.ICON_HELP, 32), _JDT.T.basics_yes(), _JDT.T.basics_no(), "org.jdownloader.gui.views.linkgrabber.LinkGrabberTable");
        }
        sortPackageChildren(newColumn.getRowSorter(), getModel().getNextSortIdentifier(newColumn.getSortOrderIdentifier()));
        return true;
    }

    @Override
    public boolean isSearchEnabled() {
        return false;
    }

    @Override
    protected boolean onDoubleClick(final MouseEvent e, final AbstractNode obj) {
        return false;
    }

    @Override
    protected JPopupMenu onContextMenu(final JPopupMenu popup, final AbstractNode contextObject, final java.util.List<AbstractNode> selection, final ExtColumn<AbstractNode> column, MouseEvent ev) {
        ExtPopupMenu root = new ExtPopupMenu();
        MenuContainerRoot md = MenuManagerLinkgrabberTableContext.getInstance().getMenuData();
        new MenuBuilder(MenuManagerLinkgrabberTableContext.getInstance(), root, md).setHideOnClick(!ev.isShiftDown()).run();
        return root;
    }

    protected JPopupMenu columnControlMenu(final ExtColumn<AbstractNode> extColumn) {
        JPopupMenu popup = super.columnControlMenu(extColumn);
        popup.add(new JSeparator());
        popup.add(new ExtCheckBoxMenuItem(new HorizontalScrollbarAction(this, CFG_GUI.HORIZONTAL_SCROLLBARS_IN_LINKGRABBER_TABLE_ENABLED)));
        return popup;
    }

    @Override
    protected boolean onShortcutDelete(final java.util.List<AbstractNode> selectedObjects, final KeyEvent evt, final boolean direct) {
        getSelectionInfo(new QueueSelectionInfoCallback<CrawledPackage, CrawledLink>() {
            @Override
            public void onSelectionInfo(final SelectionInfo<CrawledPackage, CrawledLink> selectionInfo) {
                final List<CrawledLink> nodesToDelete = new ArrayList<CrawledLink>();
                boolean containsOnline = false;
                for (final CrawledLink dl : selectionInfo.getChildren()) {
                    final CrawledPackage parentNode = dl.getParentNode();
                    if (parentNode != null) {
                        nodesToDelete.add(dl);
                        if ((TYPE.OFFLINE == parentNode.getType() || TYPE.POFFLINE == parentNode.getType())) {
                            continue;
                        }
                        if (dl.getDownloadLink().getAvailableStatus() != AvailableStatus.FALSE) {
                            containsOnline = true;
                        }
                    }
                }
                if (nodesToDelete.size() > 0) {
                    LinkCollector.requestDeleteLinks(nodesToDelete, containsOnline, _GUI.T.GenericDeleteSelectedToolbarAction_updateName_object_selected_all(), evt.isControlDown(), false, false, false, false);
                }
            }

            @Override
            public boolean isCancelled() {
                return false;
            }

            @Override
            public Queue getQueue() {
                return TaskQueue.getQueue();
            }
        }, SelectionType.SELECTED);
        return true;
    }

    @Override
    protected boolean updateMoveButtonEnabledStatus() {
        return super.updateMoveButtonEnabledStatus();
    }

    @Override
    protected boolean onShortcutCopy(java.util.List<AbstractNode> selectedObjects, KeyEvent evt) {
        if (evt.isAltDown() || evt.isMetaDown() || evt.isAltGraphDown() || evt.isShiftDown()) {
            return false;
        }
        TransferHandler.getCopyAction().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "copy"));
        return true;
    }

    @Override
    protected boolean onShortcutCut(java.util.List<AbstractNode> selectedObjects, KeyEvent evt) {
        if (evt.isAltDown() || evt.isMetaDown() || evt.isAltGraphDown() || evt.isShiftDown()) {
            return false;
        }
        TransferHandler.getCutAction().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "cut"));
        return true;
    }

    @Override
    protected boolean onShortcutPaste(java.util.List<AbstractNode> selectedObjects, KeyEvent evt) {
        if (evt.isAltDown() || evt.isMetaDown() || evt.isAltGraphDown() || evt.isShiftDown()) {
            return false;
        }
        TransferHandler.getPasteAction().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "paste"));
        return true;
    }

    @Override
    public ExtColumn<AbstractNode> getExpandCollapseColumn() {
        return LinkGrabberTableModel.getInstance().expandCollapse;
    }

    @Override
    public Set<AWTKeyStroke> getFocusTraversalKeys(int id) {
        // important to make ctrl+tab and ctrl+shift+tab work for the main tabbed pane
        return new HashSet<AWTKeyStroke>();
    }

    @Override
    protected boolean processKeyBinding(KeyStroke stroke, KeyEvent evt, int condition, boolean pressed) {
        boolean actionNotified = false;
        try {
            final InputMap map = getInputMap(condition);
            final ActionMap am = getActionMap();
            if (map != null && am != null && isEnabled()) {
                final Object binding = map.get(stroke);
                final Action action = (binding == null) ? null : am.get(binding);
                if (action != null) {
                    if (action instanceof CustomizableAppAction) {
                        ((CustomizableAppAction) action).requestUpdate(this);
                    }
                    if (!action.isEnabled()) {
                        Toolkit.getDefaultToolkit().beep();
                    } else {
                        actionNotified = SwingUtilities.notifyAction(action, stroke, evt, this, evt.getModifiers());
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return actionNotified || super.processKeyBinding(stroke, evt, condition, pressed);
    }

    public void updateContextShortcuts() {
        final InputMap input = getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
        final InputMap input2 = getInputMap(JComponent.WHEN_FOCUSED);
        final InputMap input3 = getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        final ActionMap actions = getActionMap();
        if (shortCutActions != null) {
            for (Entry<KeyStroke, Action> ks : shortCutActions.entrySet()) {
                Object binding = input.get(ks.getKey());
                input.remove(ks.getKey());
                input2.remove(ks.getKey());
                input3.remove(ks.getKey());
                actions.remove(binding);
            }
        }
        shortCutActions = new HashMap<KeyStroke, Action>();
        fillActions(MenuManagerLinkgrabberTableContext.getInstance().getMenuData());
        fillActions(MenuManagerLinkgrabberTabBottombar.getInstance().getMenuData());
    }

    private void fillActions(MenuContainer menuData) {
        if (!menuData._isValidated()) {
            return;
        }
        final InputMap input = getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
        final InputMap input2 = getInputMap(JComponent.WHEN_FOCUSED);
        final InputMap input3 = getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        final ActionMap actions = getActionMap();
        for (MenuItemData mi : menuData.getItems()) {
            if (!mi._isValidated()) {
                continue;
            }
            if (mi instanceof MenuContainer) {
                fillActions((MenuContainer) mi);
            } else if (mi instanceof SeparatorData) {
                continue;
            } else if (mi instanceof MenuLink) {
                List<AppAction> actionsList = ((MenuLink) mi).createActionsToLink();
                if (actionsList != null) {
                    for (AppAction action : actionsList) {
                        KeyStroke keystroke = (KeyStroke) action.getValue(Action.ACCELERATOR_KEY);
                        if (keystroke != null) {
                            linkAction(input, input2, input3, actions, action, keystroke);
                        }
                    }
                }
                continue;
            } else {
                AppAction action;
                try {
                    if (mi.getActionData() == null || !mi.getActionData()._isValidDataForCreatingAnAction()) {
                        continue;
                    }
                    action = mi.createAction();
                    KeyStroke keystroke;
                    if (StringUtils.isNotEmpty(mi.getShortcut())) {
                        keystroke = KeyStroke.getKeyStroke(mi.getShortcut());
                        if (keystroke != null) {
                            action.setAccelerator(keystroke);
                        }
                    } else if (MenuItemData.isEmptyValue(mi.getShortcut())) {
                        action.setAccelerator(null);
                    }
                    keystroke = (KeyStroke) action.getValue(Action.ACCELERATOR_KEY);
                    linkAction(input, input2, input3, actions, action, keystroke);
                    if (action instanceof CustomizableAppAction) {
                        List<KeyStroke> moreShortCuts = ((CustomizableAppAction) action).getAdditionalShortcuts(keystroke);
                        if (moreShortCuts != null) {
                            for (KeyStroke ks : moreShortCuts) {
                                if (ks != null) {
                                    linkAction(input, input2, input3, actions, action, ks);
                                }
                            }
                        }
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
    }

    protected void linkAction(final InputMap input, final InputMap input2, final InputMap input3, final ActionMap actions, AppAction action, KeyStroke keystroke) {
        if (action != null && keystroke != null) {
            String key = "CONTEXT_ACTION_" + keystroke;
            try {
                Object old = input.get(keystroke);
                if (old != null && action.getClass() != actions.get(old).getClass()) {
                    logger.warning("Duplicate Shortcuts: " + action + " overwrites " + actions.get(old) + "(" + old + ")" + " for keystroke " + keystroke);
                }
            } catch (Exception e) {
                logger.log(e);
            }
            try {
                Object old = input2.get(keystroke);
                if (old != null && action.getClass() != actions.get(old).getClass()) {
                    logger.warning("Duplicate Shortcuts: " + action + " overwrites " + actions.get(old) + "(" + old + ")" + " for keystroke " + keystroke);
                }
            } catch (Exception e) {
                logger.log(e);
            }
            try {
                Object old = input3.get(keystroke);
                if (old != null && action.getClass() != actions.get(old).getClass()) {
                    logger.warning("Duplicate Shortcuts: " + action + " overwrites " + actions.get(old) + "(" + old + ")" + " for keystroke " + keystroke);
                }
            } catch (Exception e) {
                logger.log(e);
            }
            logger.info(keystroke + " -> " + action);
            input.put(keystroke, key);
            input2.put(keystroke, key);
            input3.put(keystroke, key);
            actions.put(key, action);
            shortCutActions.put(keystroke, action);
        }
    }

    public static LinkGrabberTable getInstance() {
        return INSTANCE;
    }

    public void linkAction(AppAction focusAction, KeyStroke ks) {
        if (focusAction == null) {
            return;
        }
        final InputMap input = getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
        final InputMap input2 = getInputMap(JComponent.WHEN_FOCUSED);
        final InputMap input3 = getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        final ActionMap actions = getActionMap();
        this.linkAction(input, input2, input3, actions, focusAction, ks);
    }

    @Override
    public void paintComponent(final Graphics g) {
        super.paintComponent(g);
        paintAllHiddenWarning(g);
    }

    /**
     * When the table is empty although links are loaded, all of them got hidden by the active filters. In that case a large,
     * semi-transparent red hint is painted into the background of the (otherwise empty) table so the user understands why nothing
     * shows up. Search filtering takes precedence over linkgrabber views: only one of the two messages is ever shown.
     */
    private void paintAllHiddenWarning(final Graphics g) {
        final LinkGrabberTableModel model = (LinkGrabberTableModel) getModel();
        if (model.getRowCount() > 0) {
            // something is visible, nothing to warn about
            return;
        }
        final int total = LinkCollector.getInstance().getAllChildren().size();
        if (total <= 0) {
            // linkgrabber is genuinely empty (no links loaded), not a filtering issue
            return;
        }
        final String message;
        if (model.isFilteredView()) {
            // a non-view filter (i.e. the search field) is active -> search wins over views
            message = _GUI.T.LinkGrabberTable_allLinksHiddenBySearch(Integer.toString(total));
        } else if (hasActiveViewFilter(model)) {
            message = _GUI.T.LinkGrabberTable_allLinksHiddenByViews(Integer.toString(total));
        } else {
            // empty for some other reason (should not happen), don't paint a misleading hint
            return;
        }
        drawCenteredWarning((Graphics2D) g, message);
    }

    /** Whether at least one linkgrabber view (FilterTable) is currently applied to the model. */
    private boolean hasActiveViewFilter(final LinkGrabberTableModel model) {
        final PackageControllerTableModelData<CrawledPackage, CrawledLink> data = model.getTableData();
        if (data == null) {
            return false;
        }
        return containsViewFilter(data.getChildrenFilters()) || containsViewFilter(data.getPackageFilters());
    }

    private boolean containsViewFilter(final List<PackageControllerTableModelFilter<CrawledPackage, CrawledLink>> filters) {
        if (filters == null) {
            return false;
        }
        for (final PackageControllerTableModelFilter<CrawledPackage, CrawledLink> filter : filters) {
            if (filter instanceof FilterTable) {
                return true;
            }
        }
        return false;
    }

    private void drawCenteredWarning(final Graphics2D g2, final String message) {
        final Rectangle visibleRect = getVisibleRect();
        if (visibleRect.width <= 0 || visibleRect.height <= 0) {
            return;
        }
        final Composite orgComposite = g2.getComposite();
        final Font orgFont = g2.getFont();
        final Color orgColor = g2.getColor();
        final Object orgAA = g2.getRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING);
        try {
            g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
            // wrap into lines, then scale the font down until every line fits the available width
            final int maxWidth = Math.max(1, visibleRect.width - 40);
            int fontSize = Math.max(14, visibleRect.height / 12);
            Font font;
            List<String> lines;
            FontMetrics fm;
            while (true) {
                font = orgFont.deriveFont(Font.BOLD, (float) fontSize);
                fm = g2.getFontMetrics(font);
                lines = wrapText(message, fm, maxWidth);
                int widest = 0;
                for (final String line : lines) {
                    widest = Math.max(widest, fm.stringWidth(line));
                }
                if (widest <= maxWidth || fontSize <= 14) {
                    break;
                }
                fontSize -= 2;
            }
            g2.setFont(font);
            g2.setColor(Color.RED);
            // real red, partly transparent as requested
            g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f));
            final int lineHeight = fm.getHeight();
            final int totalHeight = lineHeight * lines.size();
            int y = visibleRect.y + (visibleRect.height - totalHeight) / 2 + fm.getAscent();
            for (final String line : lines) {
                final int x = visibleRect.x + (visibleRect.width - fm.stringWidth(line)) / 2;
                g2.drawString(line, x, y);
                y += lineHeight;
            }
        } finally {
            g2.setComposite(orgComposite);
            g2.setFont(orgFont);
            g2.setColor(orgColor);
            if (orgAA != null) {
                g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, orgAA);
            }
        }
    }

    /** Greedy word wrap so the warning stays inside the given width. */
    private List<String> wrapText(final String text, final FontMetrics fm, final int maxWidth) {
        final List<String> lines = new ArrayList<String>();
        final String[] words = text.split(" ");
        StringBuilder current = new StringBuilder();
        for (final String word : words) {
            final String candidate = current.length() == 0 ? word : current.toString() + " " + word;
            if (fm.stringWidth(candidate) > maxWidth && current.length() > 0) {
                lines.add(current.toString());
                current = new StringBuilder(word);
            } else {
                current = new StringBuilder(candidate);
            }
        }
        if (current.length() > 0) {
            lines.add(current.toString());
        }
        if (lines.size() == 0) {
            lines.add(text);
        }
        return lines;
    }
}
