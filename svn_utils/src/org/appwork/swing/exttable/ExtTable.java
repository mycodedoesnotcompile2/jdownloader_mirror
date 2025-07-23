/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.swing.exttable;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.EventObject;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

import javax.swing.CellEditor;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.storage.Storage;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.tooltips.ExtTooltip;
import org.appwork.swing.components.tooltips.ToolTipController;
import org.appwork.swing.components.tooltips.ToolTipHandler;
import org.appwork.swing.exttable.columnmenu.ResetColumns;
import org.appwork.swing.exttable.columnmenu.SearchContextAction;
import org.appwork.swing.exttable.columns.CellHeightProvider;
import org.appwork.utils.Application;
import org.appwork.utils.BinaryLogic;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTHelper;
import org.appwork.utils.swing.EDTRunner;

/**
 * The ExtTable package is a tableframework that follows two main tasks:<br>
 * 1. Easy creating of tables<br>
 * 2. Implement extended features like column selection, database connection, editing, easy rendering, sorting etc.
 *
 * @author $Author: unknown$
 */
public class ExtTable<E> extends JTable implements ToolTipHandler, PropertyChangeListener {
    /**
     *
     */
    private static final String                            DEFAULT_COLUMN_STORE = "";
    private static final long                              serialVersionUID     = 2822230056021924679L;
    // executer for renameclicks
    private static final ScheduledExecutorService          EXECUTER             = Executors.newSingleThreadScheduledExecutor();
    /**
     * Column background color if column is NOT selected
     */
    private final Color                                    columnBackground;
    /**
     * Column background color if column is selected
     */
    private final Color                                    columnBackgroundSelected;
    /**
     * Column textcolor if column is NOT selected
     */
    private final Color                                    columnForeground;
    /**
     * Column textcolor if column is selected
     */
    private final Color                                    columnForegroundSelected;
    /**
     * The underlaying datamodel
     */
    private final ExtTableModel<E>                         model;
    final private java.util.List<ExtOverlayRowHighlighter> rowHighlighters;
    /**
     * true if search is enabled
     */
    private boolean                                        searchEnabled        = false;
    protected SearchDialog                                 searchDialog;
    private final ExtTableEventSender                      eventSender;
    private JComponent                                     columnButton         = null;
    private boolean                                        columnButtonVisible  = true;
    private int                                            verticalScrollPolicy;
    protected boolean                                      headerDragging;
    private ExtColumn<E>                                   lastTooltipCol;
    private int                                            lastTooltipRow;
    private ExtDataFlavor<E>                               flavor;
    private DelayedRunnable                                renameClickDelayer;
    private Runnable                                       clickDelayerRunable;
    private String                                         columnSaveID         = ExtTable.DEFAULT_COLUMN_STORE;
    private static final KeyStroke                         KEY_STROKE_CTRL_HOME = org.appwork.utils.Application.isHeadless() ? null : KeyStroke.getKeyStroke(KeyEvent.VK_HOME, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
    /**
     *
     */
    private static final KeyStroke                         KEY_STROKE_END       = org.appwork.utils.Application.isHeadless() ? null : KeyStroke.getKeyStroke(KeyEvent.VK_END, 0);
    private static final KeyStroke                         KEY_STROKE_HOME      = org.appwork.utils.Application.isHeadless() ? null : KeyStroke.getKeyStroke(KeyEvent.VK_HOME, 0);
    /**
     *
     */
    private static final KeyStroke                         KEY_STROKE_CTRL_END  = org.appwork.utils.Application.isHeadless() ? null : KeyStroke.getKeyStroke(KeyEvent.VK_END, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());

    /**
     * Create an Extended Table instance
     *
     * @param model
     *            Databsemodel
     * @param database
     *            Information storage interface.
     * @param id
     *            Tableid used for storage
     */
    public ExtTable(final ExtTableModel<E> model) {
        super(model);
        if (model == null) {
            throw new NullPointerException("Model must not be null");
        }
        this.flavor = new ExtDataFlavor<E>(this.getClass());
        this.eventSender = new ExtTableEventSender();
        initTooltipHandler();
        this.rowHighlighters = new ArrayList<ExtOverlayRowHighlighter>();
        this.model = model;
        // workaround
        this.setColumnModel(new ExtColumnModel(this.getColumnModel()));
        model.setTable(this);
        this.setRowHeight(22);
        this.renameClickDelayer = new DelayedRunnable(ExtTable.EXECUTER, this.setupRenameClickInterval()) {
            @Override
            public void delayedrun() {
                if (ExtTable.this.clickDelayerRunable != null) {
                    if (ExtTable.this.getDropLocation() == null) {
                        ExtTable.this.clickDelayerRunable.run();
                    }
                    ExtTable.this.clickDelayerRunable = null;
                }
            }

            @Override
            public String getID() {
                return "renameClickDelayer_" + ExtTable.this.getModel().getModelID();
            }
        };
        this.setTableHeader(new JTableHeader(this.getColumnModel()) {
            /**
             *
             */
            private static final long serialVersionUID = 6099615257824836337L;

            @Override
            public Dimension getPreferredSize() {
                final Dimension ret = super.getPreferredSize();
                ret.height = 19;
                return ret;
            }
        });
        this.createColumns();
        getModel().autoColumnWidth();
        // get defaultbackground and Foregroundcolors
        final TableCellRenderer renderer = new DefaultTableCellRenderer();
        Component c = renderer.getTableCellRendererComponent(this, null, true, false, 0, 0);
        this.columnBackgroundSelected = c.getBackground();
        this.columnForegroundSelected = c.getForeground();
        c = renderer.getTableCellRendererComponent(this, null, false, false, 0, 0);
        this.columnBackground = c.getBackground();
        this.columnForeground = c.getForeground();
        this.addPropertyChangeListener("dropLocation", this);
        // Mouselistener for columnselection Menu and sort on click
        this.getTableHeader().addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(final MouseEvent e) {
                if (e.getButton() == MouseEvent.BUTTON1) {
                    ExtTable.this.onSortHeaderClick(e);
                }
            }

            @Override
            public void mousePressed(final MouseEvent e) {
                ExtTable.this.headerDragging = true;
                // only if we are not in resize mode
                if (ExtTable.this.getTableHeader().getCursor().getType() == Cursor.getDefaultCursor().getType()) {
                    if (e.getButton() == MouseEvent.BUTTON3) {
                        final JPopupMenu ccm = ExtTable.this.columnControlMenu(ExtTable.this.getExtColumnAtPoint(e.getPoint()));
                        if (ccm == null) {
                            return;
                        }
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                Point point = e.getPoint();
                                point = SwingUtilities.convertPoint(e.getComponent(), point, ExtTable.this);
                                showPopup(ccm, point);
                                if (ccm.getComponentCount() == 0) {
                                    Toolkit.getDefaultToolkit().beep();
                                }
                            }
                        });
                    }
                }
            }

            @Override
            public void mouseReleased(final MouseEvent e) {
                ExtTable.this.headerDragging = false;
                try {
                    // this is a workaround. we dis
                    if (ExtTable.this.getTableHeader().getCursor().getType() == Cursor.getDefaultCursor().getType()) {
                        for (final MouseListener ms : ExtTable.this.getTableHeader().getMouseListeners()) {
                            if (ms instanceof javax.swing.plaf.basic.BasicTableHeaderUI.MouseInputHandler) {
                                // java.lang.reflect.InaccessibleObjectException: Unable to make field private java.awt.Cursor
                                // javax.swing.plaf.basic.BasicTableHeaderUI$MouseInputHandler.otherCursor accessible: module java.desktop
                                // does not "opens javax.swing.plaf.basic" to unnamed module @5677323c
                                // at
                                // java.base/java.lang.reflect.AccessibleObject.throwInaccessibleObjectException(AccessibleObject.java:391)
                                // at java.base/java.lang.reflect.AccessibleObject.checkCanSetAccessible(AccessibleObject.java:367)
                                // at java.base/java.lang.reflect.AccessibleObject.checkCanSetAccessible(AccessibleObject.java:315)
                                // at java.base/java.lang.reflect.Field.checkCanSetAccessible(Field.java:183)
                                // at java.base/java.lang.reflect.Field.setAccessible(Field.java:177)
                                // at org.appwork.swing.exttable.ExtTable$3.mouseReleased(ExtTable.java:276)
                                if (JavaVersion.getVersion().isLowerThan(JavaVersion.JVM_16_0)) {
                                    Field field;
                                    field = javax.swing.plaf.basic.BasicTableHeaderUI.MouseInputHandler.class.getDeclaredField("otherCursor");
                                    field.setAccessible(true);
                                    field.set(ms, Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR));
                                } else {
                                    // todo...
                                }
                            }
                        }
                    }
                } catch (final Throwable e1) {
                    org.appwork.loggingv3.LogV3.log(e1);
                }
                // BasicTableHeaderUI.class.getField(name)
                // ((BasicTableHeaderUI) getTableHeader())
            }
        });
        // mouselistener to display column header tooltips
        this.getTableHeader().addMouseMotionListener(new MouseAdapter() {
            @Override
            public void mouseMoved(final MouseEvent e) {
                final int col = ExtTable.this.getExtColumnModelIndexByPoint(e.getPoint());
                if (col >= 0) {
                    ExtTable.this.getTableHeader().setToolTipText(ExtTable.this.getModel().getExtColumnByModelIndex(col).getHeaderTooltip());
                }
            }
        });
        this.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(final ListSelectionEvent e) {
                if (e == null || e.getValueIsAdjusting() || ExtTable.this.getModel().isTableSelectionClearing()) {
                    return;
                }
                ExtTable.this.onSelectionChanged();
                ExtTable.this.eventSender.fireEvent(new ExtTableEvent<ArrayList<E>>(ExtTable.this, ExtTableEvent.Types.SELECTION_CHANGED));
            }
        });
        this.getTableHeader().setReorderingAllowed(true);
        this.getTableHeader().setResizingAllowed(true);
        this.setAutoResizeMode(JTable.AUTO_RESIZE_SUBSEQUENT_COLUMNS);
        this.setAutoscrolls(true);
        // getTableHeader().setPreferredSize(new
        // Dimension(getColumnModel().getTotalColumnWidth(), 19));
        // assures that the table is painted over the complete available high
        // This method is 1.6 only
        if (Application.getJavaVersion() >= Application.JAVA16) {
            this.setFillsViewportHeight(true);
        }
        // table should always try to get the full available height
        // this will cause Problems in dialogs. decrease this value if tables
        // are layouted too height
        this.setPreferredScrollableViewportSize(new Dimension(450, 20000));
        this.getColumnModel().addColumnModelListener(new TableColumnModelListener() {
            public void columnAdded(final TableColumnModelEvent e) {
            }

            public void columnMarginChanged(final ChangeEvent e) {
            }

            public void columnMoved(final TableColumnModelEvent e) {
                if (e == null) {
                    return;
                }
                if (e.getFromIndex() == e.getToIndex()) {
                    return;
                }
                final TableColumnModel tcm = ExtTable.this.getColumnModel();
                for (int i = 0; i < tcm.getColumnCount(); i++) {
                    try {
                        ExtTable.this.getStorage().put(ExtTable.this.getColumnStoreKey("POS_COL_", i), ExtTable.this.getModel().getExtColumnByModelIndex(tcm.getColumn(i).getModelIndex()).getID());
                    } catch (final Exception e1) {
                        org.appwork.loggingv3.LogV3.log(e1);
                    }
                }
            }

            public void columnRemoved(final TableColumnModelEvent e) {
            }

            public void columnSelectionChanged(final ListSelectionEvent e) {
            }
        });
    }

    protected void initTooltipHandler() {
        ToolTipController.getInstance().register(this);
        ToolTipManager.sharedInstance().unregisterComponent(this);
    }

    private boolean                       dontDoRightNow = false;
    private int                           columModifications;
    private ArrayList<CellHeightProvider> cellHeightProviders;
    private int                           defaultRowHeight;

    @Override
    protected void resizeAndRepaint() {
        if (this.dontDoRightNow) {
            return;
        }
        super.resizeAndRepaint();
    }

    @Override
    public void setRowHeight(final int rowHeight) {
        this.defaultRowHeight = rowHeight;
        super.setRowHeight(rowHeight);
    }

    private ArrayList<CellHeightProvider> ensureCellHeightProviders() {
        if (this.getModel().getColumnModifications() == this.columModifications) {
            // nothing changed
            return this.cellHeightProviders;
        }
        ArrayList<CellHeightProvider> providers = null;
        for (final ExtColumn<E> c : this.getModel().getColumns()) {
            if (c instanceof CellHeightProvider) {
                if (providers == null) {
                    providers = new ArrayList<CellHeightProvider>();
                }
                providers.add((CellHeightProvider) c);
            }
        }
        this.cellHeightProviders = providers;
        this.columModifications = this.getModel().getColumnModifications();
        return this.cellHeightProviders;
    }

    /**
     * adds a row highlighter
     *
     * @param highlighter
     */
    public void addRowHighlighter(final ExtOverlayRowHighlighter highlighter) {
        this.removeRowHighlighter(highlighter);
        this.rowHighlighters.add(highlighter);
    }

    /**
     * create Columnselection popupmenu. It contains all available columns and let's the user select. The menu does not autoclose on click.
     *
     * @param extColumn
     *
     * @return
     */
    protected JPopupMenu columnControlMenu(final ExtColumn<E> extColumn) {
        JPopupMenu popup;
        if (extColumn == null) {
            // controlbutton
            popup = new JPopupMenu();
            for (int i = 0; i < this.getModel().getColumnCount(); i++) {
                this.getModel().getExtColumnByModelIndex(i).extendControlButtonMenu(popup);
            }
        } else {
            popup = extColumn.createHeaderPopup();
            if (popup == null) {
                popup = new JPopupMenu();
            }
        }
        for (int i = 0; i < this.getModel().getColumnCount(); ++i) {
            final int j = i;
            if (this.getModel().isHidable(i)) {
                final ExtCheckBoxMenuItem mi = new ExtCheckBoxMenuItem(this.getModel().getColumnName(i));
                mi.setHideOnClick(false);
                // mis[i] = mi;
                mi.setSelected(this.getModel().isColumnVisible(i));
                mi.addActionListener(new ActionListener() {
                    public void actionPerformed(final ActionEvent e) {
                        ExtTable.this.getModel().setColumnVisible(j, mi.isSelected());
                    }
                });
                popup.add(mi);
            }
        }
        popup.add(new JSeparator());
        if (this.isSearchEnabled()) {
            popup.add(new JMenuItem(new SearchContextAction(this)));
        }
        popup.add(new JMenuItem(new ResetColumns(this)));
        return popup;
    }

    @Override
    protected void configureEnclosingScrollPane() {
        super.configureEnclosingScrollPane();
        this.reconfigureColumnButton();
    }

    /**
     * Creates the columns based on the model
     */
    void createColumns() {
        final TableColumnModel tcm = this.getColumnModel();
        while (tcm.getColumnCount() > 0) {
            tcm.removeColumn(tcm.getColumn(0));
        }
        final LinkedHashMap<String, TableColumn> columns = new LinkedHashMap<String, TableColumn>();
        for (int i = 0; i < this.getModel().getColumnCount(); ++i) {
            final int j = i;
            ExtColumn<E> ext = this.model.getExtColumnByModelIndex(j);
            final TableColumn tableColumn = new CustomOriginalTableColumn(ext, i);
            ext.setTableColumn(tableColumn, true);
            final ExtColumn<E> column = this.model.getExtColumnByModelIndex(j);
            final ExtTableHeaderRenderer customRenderer = column.getHeaderRenderer(this.getTableHeader());
            tableColumn.setHeaderRenderer(customRenderer != null ? customRenderer : this.createDefaultHeaderRenderer(column));
            // Save column width
            if (!this.model.isColumnVisible(i) && column.isHidable()) {
                continue;
            }
            columns.put(this.model.getExtColumnByModelIndex(j).getID(), tableColumn);
            // addColumn(tableColumn);
        }
        // restore column position
        int index = 0;
        while (true) {
            if (columns.isEmpty()) {
                break;
            }
            if (index < this.getModel().getColumnCount()) {
                String id;
                try {
                    id = this.getColumnStore("POS_COL_", index, "");
                    index++;
                    if (id != null) {
                        final TableColumn item = columns.remove(id);
                        if (item != null) {
                            this.addColumn(item);
                        }
                    }
                } catch (final Exception e) {
                    org.appwork.loggingv3.LogV3.log(e);
                }
            } else {
                for (final TableColumn ritem : columns.values()) {
                    this.addColumn(ritem);
                }
                break;
            }
        }
    }

    // public boolean editCellAt(int row, int column, EventObject e){
    // if( super.editCellAt(row, column, e)){
    //
    // return true;
    // }
    // return false;
    // }
    private JComponent createDefaultColumnButton() {
        final MigPanel p = new MigPanel("ins 0 2 0 0", "[grow,fill]", "[grow,fill]");
        final JButton button;
        button = new JButton(ExtTableIcon.TABLE_COLUMN_BUTTON.get(10));
        button.setBorderPainted(false);
        button.setContentAreaFilled(false);
        p.setBackground(null);
        p.setOpaque(false);
        button.addActionListener(new ActionListener() {
            public void actionPerformed(final ActionEvent event) {
                final JButton source = (JButton) event.getSource();
                final int x = source.getLocation().x;
                final int y = source.getLocation().y;
                final JPopupMenu ccm = ExtTable.this.columnControlMenu(null);
                if (ccm == null) {
                    Toolkit.getDefaultToolkit().beep();
                    return;
                }
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        Point point = new Point(x, y);
                        point = SwingUtilities.convertPoint(source, point, ExtTable.this);
                        showPopup(ccm, point);
                        if (ccm.getComponentCount() == 0) {
                            Toolkit.getDefaultToolkit().beep();
                        }
                    }
                });
            }
        });
        p.add(button, "width 12!,height 12!");
        return p;
    }

    protected ExtTableHeaderRenderer createDefaultHeaderRenderer(final ExtColumn<E> column) {
        return new ExtTableHeaderRenderer(column, this.getTableHeader());
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.tooltips.ToolTipHandler#createExtTooltip()
     */
    @Override
    public ExtTooltip createExtTooltip(Point position) {
        if (position == null) {
            position = ToolTipController.getMouseLocation();
            if (position == null) {
                return null;
            }
            position = new Point(position);
            SwingUtilities.convertPointFromScreen(position, this);
        }
        final int row = this.getRowIndexByPoint(position);
        final ExtColumn<E> col = this.getExtColumnAtPoint(position);
        this.lastTooltipCol = col;
        this.lastTooltipRow = row;
        ExtTooltip ret = this.createToolTip(col, row, position, this.getModel().getElementAt(row));
        return ret;
    }

    /**
     * @param col
     * @param row
     * @param position
     * @param elementAt
     * @return
     */
    protected ExtTooltip createToolTip(final ExtColumn<E> col, final int row, final Point position, final E elementAt) {
        if (row < 0) {
            return null;
        }
        return col.createToolTip(position, elementAt);
    }

    /**
     * By using {@link ExtColumn#setResizable(boolean)} you can lock the widths of a column. Locked Columns can fu%& up resizing.( Imagine,
     * you have resizemode to LAST_column, and last_column is locked..) this doLayout method checks if resizing of the resize column worked.
     * If not, we temporarily switch resizemode to AUTO_RESIZE_SUBSEQUENT_COLUMNS and finally AUTO_RESIZE_ALL_COLUMNS. <br>
     * All in all, this helps to get a much better resizing
     */
    @Override
    public void doLayout() {
        final TableColumn resizeColumn = this.getTableHeader().getResizingColumn();
        // for(ExtColumn<E> c:getModel().getColumns()){
        // if(!c.isResizable()){
        // c.getInternalColumn().setMinWidth(c.getInternalColumn().getPreferredWidth());
        // c.getInternalColumn().setMaxWidth(c.getInternalColumn().getPreferredWidth())
        //
        // }
        // }
        if (resizeColumn == null) {
            super.doLayout();
            return;
        } else {
            final int orgResizeMode = this.getAutoResizeMode();
            final int beforeWidth = resizeColumn.getWidth();
            super.doLayout();
            if (resizeColumn.getWidth() - beforeWidth != 0) {
                this.setAutoResizeMode(JTable.AUTO_RESIZE_SUBSEQUENT_COLUMNS);
                resizeColumn.setWidth(beforeWidth);
                super.doLayout();
                if (resizeColumn.getWidth() - beforeWidth != 0) {
                    this.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
                    resizeColumn.setWidth(beforeWidth);
                    super.doLayout();
                    if (this.headerDragging && resizeColumn.getWidth() - beforeWidth != 0) {
                        Toolkit.getDefaultToolkit().beep();
                        this.getTableHeader().setCursor(null);
                        this.headerDragging = false;
                    }
                }
            } else {
            }
            this.saveWidthsRatio();
            this.setAutoResizeMode(orgResizeMode);
        }
    }

    /**
     * @param e
     * @param extColumnByModelIndex
     */
    protected void doSortOnColumn(final ExtColumn<E> column, final MouseEvent e) {
        column.doSort();
    }

    private boolean dropLocationSame(final JTable.DropLocation a, final JTable.DropLocation b) {
        if (a == null && b == null) {
            return true;
        }
        if (a == null && b != null) {
            return false;
        }
        if (b == null && a != null) {
            return false;
        }
        if (a.isInsertColumn() != b.isInsertColumn()) {
            return false;
        }
        if (a.isInsertRow() != b.isInsertRow()) {
            return false;
        }
        if (a.getColumn() != b.getColumn()) {
            return false;
        }
        if (a.getRow() != b.getRow()) {
            return false;
        }
        return true;
    }

    @Override
    public boolean editCellAt(final int row, final int column, final EventObject e) {
        if (e != null && e instanceof KeyEvent) {
            final int code = ((KeyEvent) e).getKeyCode();
            if (code != KeyEvent.VK_ENTER && this.isAutoEditLimitedOnEnter()) {
                return false;
            }
            if (((KeyEvent) e).isControlDown() && this.isAutoEditDisabledForCtrl()) {
                return false;
            }
        }
        final boolean ret = super.editCellAt(row, column, e);
        if (ret) {
            if (this.renameClickDelayer != null) {
                this.renameClickDelayer.stop();
            }
            // we want focus in the editor
            this.transferFocus();
        }
        return ret;
    }

    /**
     *
     */
    protected void fireColumnModelUpdate() {
        getModel().autoColumnWidth();
        ExtTable.this.eventSender.fireEvent(new ExtTableEvent<MouseEvent>(ExtTable.this, ExtTableEvent.Types.COLUMN_MODEL_UPDATE));
    }

    /* we do always create columsn ourself */
    @Override
    public boolean getAutoCreateColumnsFromModel() {
        return false;
    }

    /**
     * converts the colum index to model and returns the column's cell editor
     */
    @Override
    public TableCellEditor getCellEditor(final int row, final int columnIndex) {
        return this.model.getCelleditorByColumn(this.convertColumnIndexToModel(columnIndex));
    }

    /**
     * COnverts ther colum index to the current model and returns the column's cellrenderer
     */
    @Override
    public TableCellRenderer getCellRenderer(final int row, final int column) {
        return this.model.getCellrendererByColumn(this.convertColumnIndexToModel(column));
    }

    /**
     * @return the {@link ExtTable#columnBackground}
     * @see ExtTable#columnBackground
     */
    public Color getColumnBackground() {
        return this.columnBackground;
    }

    /**
     * @return the {@link ExtTable#columnBackgroundSelected}
     * @see ExtTable#columnBackgroundSelected
     */
    public Color getColumnBackgroundSelected() {
        return this.columnBackgroundSelected;
    }

    public JComponent getColumnButton() {
        if (this.columnButton == null) {
            this.columnButton = this.createDefaultColumnButton();
        }
        return this.columnButton;
    }

    /**
     * @return the {@link ExtTable#columnForeground}
     * @see ExtTable#columnForeground
     */
    public Color getColumnForeground() {
        return this.columnForeground;
    }

    /**
     * @return the {@link ExtTable#columnForegroundSelected}
     * @see ExtTable#columnForegroundSelected
     */
    public Color getColumnForegroundSelected() {
        return this.columnForegroundSelected;
    }

    /**
     * @return
     */
    public String getColumnSaveID() {
        return this.columnSaveID;
    }

    /**
     * @param <T>
     * @param string
     * @param index
     * @param string2
     * @return
     */
    public <T> T getColumnStore(final String key, final Object key2, final T string2) {
        return this.getStorage().get(this.getColumnStoreKey(key, key2), this.getStorage().get(key + ExtTable.DEFAULT_COLUMN_STORE + key2, string2));
    }

    public String getColumnStoreKey(final String key1, final Object key2) {
        return key1 + this.getColumnSaveID() + key2;
    }

    /**
     * @return the size of the contextmenu icons
     */
    public int getContextIconSize() {
        return 22;
    }

    /**
     * @return
     */
    public ExtDataFlavor<E> getDataFlavor() {
        return this.flavor;
    }

    @Override
    @SuppressWarnings("unchecked")
    public Component getEditorComponent() {
        // update cell editor.
        if (this.getCellEditor() != null && this.getCellEditor() instanceof ExtColumn) {
            ((ExtColumn) this.getCellEditor()).getTableCellEditorComponent(this, this.getValueAt(this.getEditingRow(), this.getEditingColumn()), this.isCellSelected(this.getEditingRow(), this.getEditingColumn()), this.getEditingRow(), this.getEditingColumn(), true);
        }
        return this.editorComp;
    }

    /**
     * @return the eventSender
     */
    public ExtTableEventSender getEventSender() {
        return this.eventSender;
    }

    /**
     * Returns the real column index at this point
     *
     * @param point
     */
    public ExtColumn<E> getExtColumnAtPoint(final Point point) {
        final int x = this.getExtColumnModelIndexByPoint(point);
        return this.getModel().getExtColumnByModelIndex(x);
    }

    /**
     *
     * @param point
     * @return columnModel Index. use {@link ExtTableModel#getExtColumnByModelIndex(int)} to get the columninstance
     */
    public int getExtColumnModelIndexByPoint(final Point point) {
        final int x = this.columnAtPoint(point);
        // this.getColumnModel().get
        return this.convertColumnIndexToModel(x);
    }

    /**
     * Returns the original Celleditor given by the current LAF UI. Used to have an reference to the LAF's default editor
     *
     * @param row
     * @param column
     * @return
     */
    public TableCellEditor getLafCellEditor(final int row, final int column) {
        return super.getCellEditor(row, column);
    }

    // @Override
    // public Point getToolTipLocation(final MouseEvent event) {
    // // this.toolTipPosition = event.getPoint();
    // return super.getToolTipLocation(event);
    // }
    /**
     * Returns the original Cellrenderer given bei the current LAF UI Used to have an reference to the LAF's default renderer
     *
     * @param row
     * @param column
     * @return
     */
    public TableCellRenderer getLafCellRenderer(final int row, final int column) {
        return super.getCellRenderer(row, column);
    }

    @Override
    @SuppressWarnings("unchecked")
    public ExtTableModel<E> getModel() {
        return (ExtTableModel<E>) super.getModel();
    }

    /**
     * @return the rowHighlighters
     */
    public java.util.List<ExtOverlayRowHighlighter> getRowHighlighters() {
        return this.rowHighlighters;
    }

    /**
     * @param point
     * @return
     */
    public int getRowIndexByPoint(final Point point) {
        return this.rowAtPoint(point);
    }

    /**
     * @return
     */
    public Storage getStorage() {
        if (this.getModel() == null) {
            new RuntimeException("TableID has to be initialized here");
        }
        return this.getModel().getStorage();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.tooltips.ToolTipHandler#updateTooltip(org .appwork.swing.components.tooltips.ExtTooltip,
     * java.awt.event.MouseEvent)
     */
    @Override
    public int getTooltipDelay(final Point mousePositionOnScreen) {
        return 0;
    }

    @Override
    public String getToolTipText(final MouseEvent event) {
        return null;
    }

    /**
     * @return
     */
    protected boolean isAutoEditDisabledForCtrl() {
        return true;
    }

    /**
     * @return
     */
    protected boolean isAutoEditLimitedOnEnter() {
        return true;
    }

    /**
     * @param ks
     * @return
     */
    protected boolean isClearSelectionTrigger(final KeyStroke ks) {
        return CrossSystem.isClearSelectionTrigger(ks);
    }

    public boolean isColumnButtonVisible() {
        return this.columnButtonVisible;
    }

    /**
     * @param ks
     * @return
     */
    protected boolean isCopySelectionTrigger(final KeyStroke ks) {
        return CrossSystem.isCopySelectionTrigger(ks);
    }

    /**
     * @param ks
     * @return
     */
    protected boolean isCutSelectionTrigger(final KeyStroke ks) {
        return CrossSystem.isCutSelectionTrigger(ks);
    }

    /**
     * @param ks
     * @return
     */
    protected boolean isDeleteFinalSelectionTrigger(final KeyStroke ks) {
        return CrossSystem.isDeleteFinalSelectionTrigger(ks);
    }

    /**
     * @param ks
     * @return
     */
    protected boolean isDeleteSelectionTrigger(final KeyStroke ks) {
        return CrossSystem.isDeleteSelectionTrigger(ks);
    }

    /**
     * @param ks
     * @return
     */
    protected boolean isPasteSelectionTrigger(final KeyStroke ks) {
        return CrossSystem.isPasteSelectionTrigger(ks);
    }

    /**
     * @return the searchEnabled
     */
    public boolean isSearchEnabled() {
        return this.searchEnabled;
    }

    /**
     * @param ks
     * @return
     */
    protected boolean isSearchTrigger(final KeyStroke ks) {
        return CrossSystem.isSearchTrigger(ks);
    }

    /**
     * @param ks
     * @return
     */
    protected boolean isSelectionAllTrigger(final KeyStroke ks) {
        return CrossSystem.isSelectionAllTrigger(ks);
    }

    /**
     * @param ks
     * @return
     */
    protected boolean isSelectionDownTrigger(final KeyStroke ks) {
        return CrossSystem.isSelectionDownTrigger(ks);
    }

    /**
     * @param ks
     * @return
     */
    protected boolean isSelectionUpTrigger(final KeyStroke ks) {
        return CrossSystem.isSelectionUpTrigger(ks);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.tooltips.ToolTipHandler# isTooltipDisabledUntilNextRefocus()
     */
    @Override
    public boolean isTooltipDisabledUntilNextRefocus() {
        // table has handle ech cell as an own component.
        return false;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.tooltips.ToolTipHandler# isTooltipWithoutFocusEnabled()
     */
    @Override
    public boolean isTooltipWithoutFocusEnabled() {
        return false;
    }

    protected JPopupMenu onContextMenu(final JPopupMenu popup, final E contextObject, final List<E> selection, final ExtColumn<E> column, final MouseEvent mouseEvent) {
        return null;
    }

    /**
     * This method will be called when a doubleclick is performed on the object <code>obj</code>
     *
     * @param obj
     */
    protected boolean onDoubleClick(final MouseEvent e, final E obj) {
        return false;
    }

    /**
     * Override this to handle header sort clicks
     *
     * @param e
     * @param oldIdentifier
     * @param oldColumn
     * @param newColumn
     * @param col
     * @return
     */
    protected boolean onHeaderSortClick(final MouseEvent e, final ExtColumn<E> oldColumn, final String oldIdentifier, final ExtColumn<E> newColumn) {
        return false;
    }

    /**
     * This method will be called if the user does a windows typic rename click order. Means: click on a already selected single row
     *
     * @param e
     * @param obj
     * @return
     */
    protected boolean onRenameClick(final MouseEvent e, final E obj) {
        return false;
    }

    /**
     *
     */
    protected void onSelectionChanged() {
    }

    protected boolean isWrapAroundEnabled() {
        return true;
    }

    /**
     * @param selectedObjects
     * @param evt
     * @return
     */
    protected boolean onShortcutCopy(final List<E> selectedObjects, final KeyEvent evt) {
        return false;
    }

    /**
     * @param selectedObjects
     * @param evt
     * @return
     */
    protected boolean onShortcutCut(final List<E> selectedObjects, final KeyEvent evt) {
        return false;
    }

    /**
     * @param selectedObjects
     * @param evt
     * @param direct
     *            TODO
     * @return
     */
    protected boolean onShortcutDelete(final List<E> selectedObjects, final KeyEvent evt, final boolean direct) {
        return false;
    }

    /**
     * @param selectedObjects
     * @param evt
     * @return
     */
    protected boolean onShortcutPaste(final List<E> selectedObjects, final KeyEvent evt) {
        return false;
    }

    /**
     * @param selectedObjects
     * @param evt
     * @return
     */
    protected boolean onShortcutSearch(final List<E> selectedObjects, final KeyEvent evt) {
        if (this.isSearchEnabled() && this.hasFocus()) {
            this.startSearch();
            return true;
        }
        return false;
    }

    /**
     * @param e
     * @param obj
     */
    protected boolean onSingleClick(final MouseEvent e, final E obj) {
        return false;
    }

    /**
     * @param e
     */
    protected void onSortHeaderClick(final MouseEvent e) {
        final int col = ExtTable.this.getExtColumnModelIndexByPoint(e.getPoint());
        if (col == -1) {
            return;
        }
        final ExtColumn<E> oldColumn = ExtTable.this.getModel().getSortColumn();
        final String oldIdentifier = oldColumn == null ? null : oldColumn.getSortOrderIdentifier();
        if (!ExtTable.this.onHeaderSortClick(e, oldColumn, oldIdentifier, ExtTable.this.getModel().getExtColumnByModelIndex(col))) {
            if (ExtTable.this.getModel().getExtColumnByModelIndex(col).isSortable(null)) {
                this.doSortOnColumn(ExtTable.this.getModel().getExtColumnByModelIndex(col), e);
            }
        }
        ExtTable.this.eventSender.fireEvent(new ExtTableEvent<MouseEvent>(ExtTable.this, ExtTableEvent.Types.SORT_HEADER_CLICK, e));
    }

    /**
     * @return
     */
    public boolean isResizeableColumns() {
        return true;
    }

    @Override
    public void paintComponent(final Graphics g) {
        super.paintComponent(g);
        if (!this.handleCellHeightProviders(g)) {
            this.paintHighlighters(g);
        }
    }

    /**
     * @param g
     *            TODO
     *
     */
    protected boolean handleCellHeightProviders(Graphics g) {
        final ArrayList<CellHeightProvider> providers = this.ensureCellHeightProviders();
        if (providers == null) {
            if (getRowHeight() != defaultRowHeight) {
                this.setRowHeight(this.defaultRowHeight);
            }
            return false;
        }
        this.dontDoRightNow = true;
        boolean repaintRequired = false;
        try {
            boolean anyThingNonDefault = false;
            for (int i = 0; i < this.getRowCount(); i++) {
                int height = this.defaultRowHeight;
                for (final CellHeightProvider p : providers) {
                    if (getModel().isColumnVisible((ExtColumn<E>) p)) {
                        height = p.adjustRowHeight(i, height);
                    }
                }
                final int is = this.getRowHeight(i);
                if (height == is) {
                    if (height != this.defaultRowHeight) {
                        anyThingNonDefault = true;
                    }
                    continue;
                }
                repaintRequired = true;
                if (height != this.defaultRowHeight) {
                    anyThingNonDefault = true;
                }
                this.setRowHeight(i, height);
            }
            if (!anyThingNonDefault) {
                // Set Default height for all
                this.setRowHeight(this.defaultRowHeight);
            }
            return repaintRequired;
        } finally {
            this.dontDoRightNow = false;
            if (repaintRequired) {
                this.resizeAndRepaint();
            }
        }
    }

    /**
     * @param g
     */
    private void paintHighlighters(final Graphics g) {
        if (this.getRowCount() == 0) {
            return;
        }
        final Rectangle visibleRect = this.getVisibleRect();
        Rectangle first, last;
        // get current width;
        first = this.getCellRect(0, 0, true);
        last = this.getCellRect(0, this.getColumnCount() - 1, true);
        final int width = last.x + last.width - first.x;
        int firstVisibleRow = rowAtPoint(new Point(0, visibleRect.y));
        int lastVisibleRow = rowAtPoint(new Point(0, visibleRect.y + visibleRect.height - 1));
        if (lastVisibleRow == -1) {
            lastVisibleRow = getRowCount() - 1;
        }
        for (final ExtOverlayRowHighlighter rh : this.rowHighlighters) {
            for (int i = firstVisibleRow; i <= lastVisibleRow; i++) {
                if (rh.doHighlight(this, i)) {
                    first = this.getCellRect(i, 0, true);
                    rh.paint((Graphics2D) g, 0, first.y, width, first.height - 1);
                }
            }
        }
    }

    /**
     * Key selection
     */
    @SuppressWarnings("unchecked")
    @Override
    protected boolean processKeyBinding(final KeyStroke stroke, final KeyEvent evt, final int condition, final boolean pressed) {
        if (evt.getModifiers() == 0 && evt.getKeyCode() == KeyEvent.VK_CONTEXT_MENU && !pressed) {
            // show context menu (horizontally centered in first column)
            final E contextObject = this.getModel().getObjectbyRow(this.getSelectedRow());
            final List<E> selectedObjects = this.getModel().getSelectedObjects();
            final ExtColumn<E> firstColumn = this.getModel().getExtColumnByModelIndex(0);
            final Point point = new Point(firstColumn.getWidth() / 2, 0);
            final int scrollPosition = -this.getY();
            final JViewport viewport = getParent() instanceof JViewport ? (JViewport) this.getParent() : null;
            final int viewportHeight = viewport == null ? this.getHeight() : viewport.getHeight();
            if (this.getSelectedRow() == -1) {
                // find vertical position for no selection
                point.y = this.getRowCount() * rowHeight;
                if (point.y + (rowHeight - 1) / 2 < viewportHeight) {
                    // center in next possible row (non-existent)
                    point.y += (rowHeight - 1) / 2;
                } else if (point.y < viewportHeight) {
                    // center in remaining space not covered by rows
                    point.y += (viewportHeight - point.y) / 2;
                } else {
                    // center in table
                    point.y = scrollPosition + (viewportHeight - 1) / 2;
                }
            } else {
                // find vertical position with rows selected (centered between first and last selected row)
                int[] selectedRows = this.getSelectedRows();
                int upperY = Math.max(selectedRows[0] * rowHeight, scrollPosition);
                upperY = Math.min(upperY, scrollPosition + viewportHeight - 1);
                int lowerY = Math.max((selectedRows[selectedRows.length - 1] + 1) * rowHeight - 1, scrollPosition);
                lowerY = Math.min(lowerY, scrollPosition + viewportHeight - 1);
                point.y = (upperY + lowerY) / 2;
            }
            final Point absolutePoint = (Point) point.clone();
            SwingUtilities.convertPointToScreen(absolutePoint, this);
            final MouseEvent mouseEvent = new MouseEvent(this, MouseEvent.MOUSE_RELEASED, evt.getWhen(), 0, point.x, point.y, absolutePoint.x, absolutePoint.y, 1, true, MouseEvent.BUTTON3);
            final JPopupMenu menu = this.onContextMenu(new JPopupMenu(), contextObject, selectedObjects, firstColumn, mouseEvent);
            this.showPopup(menu, point);
            return true;
        }
        if (evt.isControlDown() && evt.getKeyCode() == KeyEvent.VK_HOME) {
            return false;
        }
        if (evt.isControlDown() && evt.getKeyCode() == KeyEvent.VK_END) {
            return false;
        }
        if (evt.getModifiers() == KeyEvent.SHIFT_MASK && evt.getKeyCode() == KeyEvent.VK_HOME && pressed && this.getSelectionModel().getSelectionMode() != ListSelectionModel.SINGLE_SELECTION) {
            // extend selection to beginning of list
            this.changeSelection(0, 0, false, true);
            return true;
        }
        if (evt.getModifiers() == KeyEvent.SHIFT_MASK && evt.getKeyCode() == KeyEvent.VK_END && pressed && this.getSelectionModel().getSelectionMode() != ListSelectionModel.SINGLE_SELECTION) {
            // extend selection to end of list
            this.changeSelection(this.getRowCount() - 1, 0, false, true);
            return true;
        }
        if (!evt.isControlDown() && evt.getKeyCode() == KeyEvent.VK_ENTER && pressed) {
            // edit
            final int leadRow = this.getSelectionModel().getLeadSelectionIndex();
            final int leadColumn = this.getColumnModel().getSelectionModel().getLeadSelectionIndex();
            if (leadRow != -1 && leadColumn != -1 && !this.isEditing()) {
                if (!this.editCellAt(leadRow, leadColumn, evt)) {
                    return false;
                } else {
                    return true;
                }
            }
        }
        if (!pressed) {
            return super.processKeyBinding(stroke, evt, condition, pressed);
        }
        final KeyStroke ks = stroke;
        if (ks != null) {
            if (this.isClearSelectionTrigger(ks)) {
                this.clearSelection();
                return true;
            }
            if (this.isCutSelectionTrigger(ks)) {
                ExtTable.this.eventSender.fireEvent(new ExtTableEvent<List<E>>(ExtTable.this, ExtTableEvent.Types.SHORTCUT_CUT, this.getModel().getSelectedObjects()));
                return this.onShortcutCut(this.getModel().getSelectedObjects(), evt);
            }
            if (this.isPasteSelectionTrigger(ks)) {
                ExtTable.this.eventSender.fireEvent(new ExtTableEvent<List<E>>(ExtTable.this, ExtTableEvent.Types.SHORTCUT_PASTE, this.getModel().getSelectedObjects()));
                return this.onShortcutPaste(this.getModel().getSelectedObjects(), evt);
            }
            if (this.isCopySelectionTrigger(ks)) {
                ExtTable.this.eventSender.fireEvent(new ExtTableEvent<List<E>>(ExtTable.this, ExtTableEvent.Types.SHORTCUT_COPY, this.getModel().getSelectedObjects()));
                return this.onShortcutCopy(this.getModel().getSelectedObjects(), evt);
            }
            if (this.isDeleteFinalSelectionTrigger(ks)) {
                ExtTable.this.eventSender.fireEvent(new ExtTableEvent<Object>(ExtTable.this, ExtTableEvent.Types.SHORTCUT_DELETE, this.getModel().getSelectedObjects(), BinaryLogic.containsSome(evt.getModifiers(), ActionEvent.SHIFT_MASK)));
                return this.onShortcutDelete(this.getModel().getSelectedObjects(), evt, true);
            }
            if (this.isDeleteSelectionTrigger(ks)) {
                ExtTable.this.eventSender.fireEvent(new ExtTableEvent<Object>(ExtTable.this, ExtTableEvent.Types.SHORTCUT_DELETE, this.getModel().getSelectedObjects(), BinaryLogic.containsSome(evt.getModifiers(), ActionEvent.SHIFT_MASK)));
                return this.onShortcutDelete(this.getModel().getSelectedObjects(), evt, false);
            }
            if (this.isSearchTrigger(ks)) {
                ExtTable.this.eventSender.fireEvent(new ExtTableEvent<List<E>>(ExtTable.this, ExtTableEvent.Types.SHORTCUT_SEARCH, this.getModel().getSelectedObjects()));
                return this.onShortcutSearch(this.getModel().getSelectedObjects(), evt);
            }
            if (this.isSelectionUpTrigger(ks) && this.isWrapAroundEnabled()) {
                if (this.getSelectedRow() == 0) {
                    if (this.getCellEditor() != null) {
                        this.getCellEditor().stopCellEditing();
                    }
                    this.changeSelection(this.getRowCount() - 1, 0, false, false);
                    return true;
                }
            }
            if (this.isSelectionDownTrigger(ks) && this.isWrapAroundEnabled()) {
                if (this.getSelectedRow() == this.getRowCount() - 1) {
                    if (this.getCellEditor() != null) {
                        this.getCellEditor().stopCellEditing();
                    }
                    this.changeSelection(0, 0, false, false);
                    return true;
                }
            }
            if (this.isSelectionAllTrigger(ks)) {
                this.onShortcutSelectAll();
                return true;
            }
            if (ks.equals(ExtTable.KEY_STROKE_CTRL_HOME)) {
                if (this.getCellEditor() != null) {
                    this.getCellEditor().stopCellEditing();
                }
                if (this.getSelectedRow() != -1 && this.getRowCount() != 0) {
                    this.getSelectionModel().setSelectionInterval(0, this.getSelectedRows()[this.getSelectedRows().length - 1]);
                    /* to avoid selection by super.processKeyBinding */
                    return true;
                }
            }
            if (ks.equals(ExtTable.KEY_STROKE_CTRL_END)) {
                if (this.getCellEditor() != null) {
                    this.getCellEditor().stopCellEditing();
                }
                if (this.getSelectedRow() != -1 && this.getRowCount() != 0) {
                    this.getSelectionModel().setSelectionInterval(this.getSelectedRow(), this.getRowCount() - 1);
                    /* to avoid selection by super.processKeyBinding */
                    return true;
                }
            }
            if (ks.equals(ExtTable.KEY_STROKE_END)) {
                if (this.getCellEditor() != null) {
                    this.getCellEditor().stopCellEditing();
                }
                if (this.getRowCount() != 0) {
                    final int index = this.getRowCount() - 1;
                    this.getSelectionModel().setSelectionInterval(index, index);
                }
            }
            if (ks.equals(ExtTable.KEY_STROKE_HOME)) {
                if (this.getCellEditor() != null) {
                    this.getCellEditor().stopCellEditing();
                }
                if (this.getRowCount() != 0) {
                    this.getSelectionModel().setSelectionInterval(0, 0);
                }
            }
        }
        return super.processKeyBinding(stroke, evt, condition, pressed);
    }

    public void onShortcutSelectAll() {
        if (this.getCellEditor() != null) {
            this.getCellEditor().stopCellEditing();
        }
        this.getSelectionModel().setSelectionInterval(0, this.getRowCount() - 1);
    }

    // /**
    // * @param b
    // */
    // public synchronized void setAutoResizeFallbackEnabled(final boolean b) {
    //
    // if (b == this.autoResizeFallback) { return; }
    //
    // if (b) {
    // this.orgAutoResizeMode = this.getAutoResizeMode();
    // this.setAutoResizeMode(JTable.AUTO_RESIZE_SUBSEQUENT_COLUMNS);
    // this.autoResizeFallback = b;
    // } else {
    // this.autoResizeFallback = b;
    // if (this.orgAutoResizeMode >= 0) {
    // this.setAutoResizeMode(this.orgAutoResizeMode);
    // }
    // this.orgAutoResizeMode = -1;
    //
    // }
    //
    // }
    // @Override
    // public void setAutoResizeMode(final int mode) {
    //
    // if (this.autoResizeFallback) {
    // System.out.println("Keep mode: " + mode);
    // this.orgAutoResizeMode = mode;
    // } else {
    // System.out.println("Mode: " + mode);
    // super.setAutoResizeMode(mode);
    // }
    //
    // }
    protected void showPopup(final JPopupMenu popup, final Point p) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                popup.show(ExtTable.this, p.x, p.y);
            }
        });
        return;
    }

    @SuppressWarnings("unchecked")
    @Override
    protected void processMouseEvent(final MouseEvent e) {
        if (e.getID() == MouseEvent.MOUSE_RELEASED) {
            if (isContextMenuTrigger(e)) {
                final int row = this.rowAtPoint(e.getPoint());
                final E obj = this.getModel().getObjectbyRow(row);
                final ExtColumn<E> col = this.getExtColumnAtPoint(e.getPoint());
                if (obj == null || row == -1) {
                    /* no object under mouse, lets clear the selection */
                    this.clearSelection();
                    final JPopupMenu popup = this.onContextMenu(new JPopupMenu(), null, null, col, e);
                    if (popup != null) {
                        this.eventSender.fireEvent(new ExtTableEvent<JPopupMenu>(this, ExtTableEvent.Types.CONTEXTMENU, popup));
                        if (popup.getComponentCount() > 0) {
                            showPopup(popup, e.getPoint());
                            return;
                        }
                    }
                } else {
                    /* check if we need to select object */
                    if (!this.isRowSelected(row)) {
                        this.clearSelection();
                        this.addRowSelectionInterval(row, row);
                    }
                    final List<E> selected = this.getModel().getSelectedObjects();
                    final JPopupMenu popup = this.onContextMenu(new JPopupMenu(), obj, selected, col, e);
                    if (popup != null) {
                        this.eventSender.fireEvent(new ExtTableEvent<JPopupMenu>(this, ExtTableEvent.Types.CONTEXTMENU, popup));
                        if (popup.getComponentCount() > 0) {
                            showPopup(popup, e.getPoint());
                            return;
                        }
                    }
                }
            }
        } else if (e.getID() == MouseEvent.MOUSE_CLICKED) {
            if (e.getButton() == MouseEvent.BUTTON1 && e.getClickCount() == 2) {
                final int row = this.rowAtPoint(e.getPoint());
                final E obj = this.getModel().getObjectbyRow(row);
                this.renameClickDelayer.stop();
                boolean ret = false;
                if (obj != null) {
                    final ExtColumn<E> col = this.getExtColumnAtPoint(e.getPoint());
                    if (col != null) {
                        ret = col.onDoubleClick(e, obj);
                    }
                    if (ret == false) {
                        ret = this.onDoubleClick(e, obj);
                        this.eventSender.fireEvent(new ExtTableEvent<E>(this, ExtTableEvent.Types.DOUBLECLICK, obj));
                    }
                }
                if (ret == true) {
                    return;
                }
            } else if (e.getButton() == MouseEvent.BUTTON1 && e.getClickCount() == 1) {
                final int row = this.rowAtPoint(e.getPoint());
                final E obj = this.getModel().getObjectbyRow(row);
                boolean ret = false;
                if (obj != null) {
                    final ExtColumn<E> col = this.getExtColumnAtPoint(e.getPoint());
                    if (col != null) {
                        ret = col.onSingleClick(e, obj);
                    }
                    if (ret == false) {
                        ret = this.onSingleClick(e, obj);
                    }
                    if (ret == true) {
                        return;
                    }
                    if (this.clickDelayerRunable != null) {
                        this.renameClickDelayer.resetAndStart();
                    }
                }
            }
        } else if (e.getID() == MouseEvent.MOUSE_PRESSED) {
            final int row = this.rowAtPoint(e.getPoint());
            if (row < 0) {
                this.clearSelection();
            } else if (e.getClickCount() == 1 && e.getButton() == MouseEvent.BUTTON1) {
                final E obj = this.getModel().getObjectbyRow(row);
                boolean ret = false;
                if (obj != null) {
                    final ExtColumn<E> col = this.getExtColumnAtPoint(e.getPoint());
                    if (col != null) {
                        ret = col.onMousePressed(e, obj);
                    }
                    if (ret == false) {
                        ret = this.onMousePressed(e, obj);
                    }
                    if (ret == false) {
                        final int[] slRows = this.getSelectedRows();
                        if (slRows.length == 1 && row == slRows[0]) {
                            // rename
                            if (obj != null && col != null) {
                                this.clickDelayerRunable = new Runnable() {
                                    @Override
                                    public void run() {
                                        new EDTRunner() {
                                            @Override
                                            protected void runInEDT() {
                                                final Point mousePosition = ToolTipController.getMouseLocation();
                                                if (mousePosition != null) {
                                                    SwingUtilities.convertPointFromScreen(mousePosition, ExtTable.this);
                                                }
                                                if (mousePosition != null) {
                                                    final int rowNow = ExtTable.this.rowAtPoint(mousePosition);
                                                    if (rowNow != row) {
                                                        return;
                                                    }
                                                    final ExtColumn<E> colNow = ExtTable.this.getExtColumnAtPoint(mousePosition);
                                                    if (col != colNow) {
                                                        return;
                                                    } else if (getModel().getObjectbyRow(rowNow) != obj) {
                                                        return;
                                                    }
                                                }
                                                if (!ExtTable.this.getSelectionModel().isSelectedIndex(row)) {
                                                    return;
                                                }
                                                if (!col.onRenameClick(e, obj)) {
                                                    ExtTable.this.onRenameClick(e, obj);
                                                }
                                                // we have to dispatch this event. the
                                                // new
                                                // editor will
                                                // get this event as first mousevent.
                                                // textfields
                                                // for
                                                // example will get focused
                                                ExtTable.this.setDispatchComponent(e);
                                                final CellEditor ce = ExtTable.this.getCellEditor();
                                                if (ce != null) {
                                                    ce.shouldSelectCell(e);
                                                }
                                            }
                                        };
                                    }
                                };
                            }
                        }
                    }
                    /*
                     * We do not return in case of ret==true.. this wouldcreate several issues with drag/drop as well as the selection
                     * model.
                     */
                }
            }
        }
        /*
         * moved this at the end of this function so it does not pre-interfere with customized click handling (eg changing selection)
         */
        super.processMouseEvent(e);
    }

    protected boolean isContextMenuTrigger(final MouseEvent e) {
        return CrossSystem.isContextMenuTrigger(e);
    }

    /**
     *
     * onSingleClick is called with a bit delay - we have to wait if it will become a double click. Moreover, the click event is on mouse
     * release. onMousePressed however is called directly in press, and may get used to achieve a more direct responsive experience
     *
     * @param e
     * @param obj
     * @return
     */
    public boolean onMousePressed(MouseEvent e, E obj) {
        return false;
    }

    /**
     * we listen on dropLocation-changes to maintain correct rowHighlighters!
     */
    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        if ("dropLocation".equals(evt.getPropertyName())) {
            final JTable.DropLocation oldLoc = (JTable.DropLocation) evt.getOldValue();
            final JTable.DropLocation newLoc = (JTable.DropLocation) evt.getNewValue();
            if (!this.dropLocationSame(oldLoc, newLoc)) {
                /* dropLocation changed, so lets refresh rowHighlighters */
                /* refresh highlighters during dnd */
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        ExtTable.this.repaint();
                    }
                });
            }
        }
    }

    protected void reconfigureColumnButton() {
        final Container c1 = this.getParent();
        if (c1 instanceof JViewport) {
            final Container c2 = c1.getParent();
            if (c2 instanceof JScrollPane) {
                final JScrollPane scrollPane = (JScrollPane) c2;
                final JViewport viewport = scrollPane.getViewport();
                if (viewport == null || viewport.getView() != this) {
                    return;
                }
                if (this.isColumnButtonVisible()) {
                    this.verticalScrollPolicy = scrollPane.getVerticalScrollBarPolicy();
                    scrollPane.setCorner(ScrollPaneConstants.UPPER_TRAILING_CORNER, this.getColumnButton());
                    scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
                } else {
                    if (this.verticalScrollPolicy != 0) {
                        /* http://java.net/jira/browse/SWINGX-155 */
                        scrollPane.setVerticalScrollBarPolicy(this.verticalScrollPolicy);
                    }
                    try {
                        scrollPane.setCorner(ScrollPaneConstants.UPPER_TRAILING_CORNER, null);
                    } catch (final Throwable nothing) {
                    }
                }
            }
        }
    }

    /**
     * Removes a rowhilighter
     *
     * @param highlighter
     */
    public void removeRowHighlighter(final ExtOverlayRowHighlighter highlighter) {
        this.rowHighlighters.remove(highlighter);
    }

    /**
     * Resets the columnwidth to their default value. If their is empty space afterwards, the table will distribute the DELTA to all columns
     * PLease make sure to call {@link #updateColumns()} afterwards
     */
    public void resetColumnDimensions() {
        for (final ExtColumn<E> col : this.getModel().getColumns()) {
            // col.getTableColumn().setPreferredWidth(col.getDefaultWidth());
            try {
                this.getStorage().put(this.getColumnStoreKey("WIDTH_COL_", col.getID()), col.getDefaultWidth());
            } catch (final Exception e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
        }
    }

    /**
     * Resets the column locks to their default value. If their is empty space afterwards, the table will distribute the DELTA to all
     * columns PLease make sure to call {@link #updateColumns()} afterwards
     */
    public void resetColumnLocks() {
        for (final ExtColumn<E> col : this.getModel().getColumns()) {
            // col.getTableColumn().setPreferredWidth(col.getDefaultWidth());
            this.getStorage().put("ColumnWidthLocked_" + this.getColumnSaveID() + col.getID(), !col.isDefaultResizable());
        }
    }

    /**
     * Resets the Order of the columns to their default PLease make sure to call {@link #updateColumns()} afterwards
     */
    public void resetColumnOrder() {
        for (int i = 0; i < this.getModel().getColumns().size(); i++) {
            // col.getTableColumn().setPreferredWidth(col.getDefaultWidth());
            final ExtColumn<E> col = this.getModel().getColumns().get(i);
            try {
                this.getStorage().put(this.getColumnStoreKey("POS_COL_", i), col.getID());
            } catch (final Exception e1) {
                org.appwork.loggingv3.LogV3.log(e1);
            }
        }
    }

    /**
     * Resets the Visibility of all columns to their Default value. PLease make sure to call {@link #updateColumns()} afterwards
     */
    public void resetColumnVisibility() {
        for (final ExtColumn<E> col : this.getModel().getColumns()) {
            // col.getTableColumn().setPreferredWidth(col.getDefaultWidth());
            try {
                this.getStorage().put(this.getColumnStoreKey("VISABLE_COL_", col.getID()), col.isDefaultVisible());
            } catch (final Exception e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
        }
    }

    //
    // private int viewIndexForColumn(final TableColumn aColumn) {
    // final TableColumnModel cm = this.getColumnModel();
    // for (int column = 0; column < cm.getColumnCount(); column++) {
    // if (cm.getColumn(column) == aColumn) { return column; }
    // }
    // return -1;
    // }
    public void saveWidthsRatio() {
        for (int i = 0; i < this.getColumnCount(); i++) {
            final ExtColumn<E> col = this.getModel().getExtColumnByModelIndex(this.convertColumnIndexToModel(i));
            try {
                col.getTableColumn().setPreferredWidth(col.getTableColumn().getWidth());
                ExtTable.this.getStorage().put(this.getColumnStoreKey("WIDTH_COL_", col.getID()), col.getTableColumn().getWidth());
            } catch (final Exception e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
        }
    }

    public void scrollToRow(final int row, final int preferedXPosition) {
        if (row < 0) {
            return;
        }
        new EDTHelper<Object>() {
            @Override
            public Object edtRun() {
                final JViewport viewport = (JViewport) ExtTable.this.getParent();
                if (viewport == null) {
                    return null;
                }
                final Rectangle rect = ExtTable.this.getCellRect(row, 0, true);
                final Rectangle viewRect = viewport.getViewRect();
                rect.width = viewRect.width;
                rect.height = viewRect.height;
                if (preferedXPosition < 0) {
                    rect.x = viewRect.x;
                } else {
                    rect.x = preferedXPosition;
                }
                ExtTable.this.scrollRectToVisible(rect);
                return null;
            }
        }.start();
    }

    public void scrollToSelection(final int preferedXPosition) {
        new EDTHelper<Object>() {
            @Override
            public Object edtRun() {
                final JViewport viewport = (JViewport) ExtTable.this.getParent();
                if (viewport == null) {
                    return null;
                }
                final int[] sel = ExtTable.this.getSelectedRows();
                if (sel == null || sel.length == 0) {
                    return null;
                }
                final Rectangle rect = ExtTable.this.getCellRect(sel[0], 0, true);
                final Rectangle rect2 = ExtTable.this.getCellRect(sel[sel.length - 1], 0, true);
                rect.height += rect2.y - rect.y;
                final Rectangle viewRect = viewport.getViewRect();
                rect.width = viewRect.width;
                if (preferedXPosition < 0) {
                    rect.x = viewRect.x;
                } else {
                    rect.x = preferedXPosition;
                }
                ExtTable.this.scrollRectToVisible(rect);
                return null;
            }
        }.start();
    }

    public void setColumnBottonVisibility(final boolean visible) {
        this.columnButtonVisible = visible;
        this.reconfigureColumnButton();
    }

    public void setColumnButton(final JComponent c) {
        this.columnButton = c;
        this.reconfigureColumnButton();
    }

    public void setColumnSaveID(String columnSaveID) {
        if (columnSaveID == null) {
            columnSaveID = ExtTable.DEFAULT_COLUMN_STORE;
        }
        this.columnSaveID = columnSaveID;
        this.updateColumns();
    }

    private void setDispatchComponent(final MouseEvent e) {
        final Component editorComponent = this.getEditorComponent();
        if (editorComponent != null) {
            final Point p = e.getPoint();
            final Point p2 = SwingUtilities.convertPoint(this, p, editorComponent);
            final Component dispatchComponent = SwingUtilities.getDeepestComponentAt(editorComponent, p2.x, p2.y);
            if (dispatchComponent != null) {
                org.appwork.sunwrapper.sun.swing.SwingUtilities2Wrapper.setSkipClickCount(dispatchComponent, e.getClickCount() - 1);
                final MouseEvent e2 = SwingUtilities.convertMouseEvent(this, e, dispatchComponent);
                dispatchComponent.dispatchEvent(e2);
            }
        }
    }

    /**
     * @param searchEnabled
     *            the searchEnabled to set
     */
    public void setSearchEnabled(final boolean searchEnabled) {
        this.searchEnabled = searchEnabled;
    }

    public void setTransferHandler(final ExtTransferHandler<E> newHandler) {
        newHandler.setTable(this);
        super.setTransferHandler(newHandler);
    }

    /**
     * @return
     */
    protected long setupRenameClickInterval() {
        return 500;
    }

    /**
     * @param text
     * @param b
     * @param regex
     */
    public void runSearch(final String text, final boolean caseSensitive, final boolean regex) {
        final int[] sel = this.getSelectedRows();
        int startRow = -1;
        if (sel != null && sel.length > 0) {
            startRow = sel[sel.length - 1];
        }
        final int startAt = startRow;
        new Thread("Run Table Search") {
            {
                setDaemon(true);
            }

            @Override
            public void run() {
                final E found = getModel().searchNextObject(startAt + 1, text, caseSensitive, regex);
                if (found != null) {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            getModel().setSelectedObject(found);
                            final int row = getSelectedRow();
                            final Rectangle rowRect = getCellRect(row, 0, true);
                            if (!getVisibleRect().intersects(rowRect)) {
                                // only scrill if the row is not visible
                                scrollToRow(row, -1);
                                // scroll parent visible vertical bars and make sure rowRect is complete visible, so scroll height*2
                                rowRect.height = rowRect.height * 2;
                                Component parent = ExtTable.this.getParent();
                                while (parent != null) {
                                    if (parent instanceof JScrollPane) {
                                        final JScrollPane scroll = (JScrollPane) parent;
                                        final JScrollBar verticalScrollBar;
                                        if ((verticalScrollBar = scroll.getVerticalScrollBar()) != null && verticalScrollBar.isVisible()) {
                                            scroll.scrollRectToVisible(rowRect);
                                        }
                                    }
                                    parent = parent.getParent();
                                }
                            }
                        }
                    };
                } else {
                    CrossSystem.playErrorSound();
                }
            };;
        }.start();
    }

    /**
     * Starts a Search Prozess. Usualy opens a Search Dialog
     */
    public synchronized void startSearch() {
        if (this.searchDialog != null && this.searchDialog.isShowing()) {
            this.searchDialog.requestFocus();
        } else {
            this.searchDialog = new SearchDialog(SearchDialog.NO_REGEX_FLAG, this) {
                private static final long serialVersionUID = 2652101312418765845L;

                @Override
                public void actionPerformed(final ActionEvent e) {
                    final String ret = ExtTable.this.searchDialog.getReturnID();
                    if (ret != null) {
                        runSearch(ret, ExtTable.this.searchDialog.isCaseSensitive(), ExtTable.this.searchDialog.isRegex());
                    }
                }
            };
        }
    }

    public void updateColumns() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                ExtTable.this.createColumns();
                ExtTable.this.revalidate();
                ExtTable.this.repaint();
                if (fireTableStructureChangedOnUpdateColumns()) {
                    getModel().fireTableStructureChanged();
                }
            }
        };
    }

    protected boolean fireTableStructureChangedOnUpdateColumns() {
        return false;
    }

    @Override
    public boolean updateTooltip(final ExtTooltip activeToolTip, final MouseEvent e) {
        final int row = this.getRowIndexByPoint(e.getPoint());
        final ExtColumn<E> col = this.getExtColumnAtPoint(e.getPoint());
        return this.lastTooltipCol != col || this.lastTooltipRow != row;
    }

    /**
     * @return
     */
    public boolean isColumnLockingFeatureEnabled() {
        return true;
    }
}
