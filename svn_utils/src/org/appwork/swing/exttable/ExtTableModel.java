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
package org.appwork.swing.exttable;

import java.awt.Dimension;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Pattern;

import javax.swing.Icon;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.Storage;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.swing.EDTHelper;
import org.appwork.utils.swing.EDTRunner;

/**
 * NOTE about Selection issues: in case you use fireTableXYEvents, it will clear the SelectionModel and you will loose your current ongoing
 * Selection! you have to save anchor/lead of selectionmodel, then fire event, restore saved selections and then set anchor/lead again
 *
 * @author daniel
 *
 * @param <E>
 */
public abstract class ExtTableModel<E> extends AbstractTableModel {
    /**
     *
     */
    public static final String                                     SORT_ORDER_ID_KEY      = "SORT_ORDER_ID";
    /**
     *
     */
    public static final String                                     SORTCOLUMN_KEY         = "SORTCOLUMN";
    /**
     *
     */
    private static final long                                      serialVersionUID       = 939549808899567618L;
    /**
     * complete table structure has changed
     */
    protected static final int                                     UPDATE_STRUCTURE       = 1;
    /**
     * Column instances
     */
    protected java.util.List<ExtColumn<E>>                         columns                = new ArrayList<ExtColumn<E>>();
    /**
     * Modelid to have an seperate key for database savong
     */
    private String                                                 modelID;
    /**
     * the table that uses this model
     */
    private ExtTable<E>                                            table                  = null;
    /**
     * a list of objects. Each object represents one table row
     */
    protected List<E>                                              tableData              = new ArrayList<E>();
    protected volatile ExtColumn<E>                                sortColumn;
    private volatile java.util.List<ExtComponentRowHighlighter<E>> extComponentRowHighlighters;
    private Object                                                 highlighterLock        = new Object();
    private Icon                                                   iconAsc;
    private Icon                                                   iconDesc;
    private PropertyChangeListener                                 replaceDelayer;
    private volatile boolean                                       replaceDelayerSet      = false;
    private volatile List<E>                                       delayedNewTableData    = null;
    private boolean                                                debugTableModel        = false;
    private AtomicBoolean                                          tableStructureChanging = new AtomicBoolean(false);
    private AtomicBoolean                                          tableSelectionClearing = new AtomicBoolean(false);
    private ExtTableModelEventSender                               eventSender;
    private boolean                                                autoWidthEnabled       = false;
    private int                                                    columnModifications    = 0;

    public int getColumnModifications() {
        return columnModifications;
    }

    /**
     * Create a new ExtTableModel.
     *
     * @param database
     *            databaseinterface instance to store internal tabledata across sessions
     * @param id
     *            storageID.
     */
    public ExtTableModel(final String id) {
        super();
        init(id);
    }

    /**
     * @param id
     */
    protected void init(final String id) {
        this.extComponentRowHighlighters = new ArrayList<ExtComponentRowHighlighter<E>>();
        this.modelID = id;
        this.iconAsc = createSortASCIcon();
        this.iconDesc = createSortDESCIcon();
        this.initModel();
        /**
         * we use this PropertyChangeListener to avoid tableRefresh while the table is in editing mode
         **/
        this.replaceDelayer = new PropertyChangeListener() {
            @Override
            public void propertyChange(final PropertyChangeEvent evt) {
                if ("tableCellEditor".equalsIgnoreCase(evt.getPropertyName()) && evt.getNewValue() == null) {
                    /*
                     * tableCellEditor is null again, now we can refresh the TableData and Selection
                     */
                    final ExtTable<E> ltable = ExtTableModel.this.getTable();
                    if (ltable != null) {
                        ltable.removePropertyChangeListener(this);
                    }
                    ExtTableModel.this._replaceTableData(ExtTableModel.this.delayedNewTableData, false);
                }
            }
        };
    }

    protected Icon createSortDESCIcon() {
        return ExtTableIcon.TABLE_SORT_DESC.get(6);
    }

    protected Icon createSortASCIcon() {
        return ExtTableIcon.TABLE_SORT_ASC.get(6);
    }

    @Override
    public void fireTableDataChanged() {
        super.fireTableDataChanged();
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.table.AbstractTableModel#fireTableChanged(javax.swing.event.TableModelEvent)
     */
    @Override
    public void fireTableChanged(TableModelEvent e) {
        autoColumnWidth();
        super.fireTableChanged(e);
    }

    /**
     * @param column
     */
    protected void autoColumnWidth() {
        try {
            if (getTable() == null) {
                return;
            }
            if (!isAutoWidthEnabled()) {
                return;
            }
            HashMap<ExtColumn, Integer> map = new HashMap<ExtColumn, Integer>();
            List<E> elements = getElements();
            Integer width;
            for (int i = 0; i < elements.size(); i++) {
                E e = elements.get(i);
                for (ExtColumn<E> column : getColumns()) {
                    // System.out.println("Auto " + column.getName() + " - " + column.isAutoWidthEnabled() + " - " + column.isResizable());
                    if (column.isAutoWidthEnabled() && column.isResizable()) {
                        width = map.get(column);
                        if (width == null) {
                            width = column.getMinWidth();
                            map.put(column, width);
                        }
                        Dimension c = column.getCellSizeEstimation(e, i);
                        if (c != null) {
                            int wi = c.width;
                            if (wi > width) {
                                map.put(column, wi);
                            }
                        }
                    }
                }
            }
            for (Entry<ExtColumn, Integer> es : map.entrySet()) {
                int adjusted = es.getKey().adjustWidth(es.getValue());
                if (adjusted <= 0) {
                    // No forced with
                    es.getKey().setForcedWidth(adjusted);
                } else {
                    int max = es.getKey().getMaxWidth();
                    int min = es.getKey().getMinWidth();
                    if (max > 0) {
                        adjusted = Math.min(adjusted, es.getKey().getMaxWidth());
                    }
                    if (min > 0) {
                        adjusted = Math.max(adjusted, es.getKey().getMinWidth());
                    }
                    es.getKey().setForcedWidth(adjusted);
                }
            }
            // borders
            // width += 0;
            // width = Math.max(column.getDefaultWidth(), width);
        } catch (Throwable e) {
            // just to be sure. new feature...a try catch is better than a unexpected exception...
            e.printStackTrace();
        }
    }

    public void _fireTableStructureChanged(final List<E> newtableData, final boolean refreshSort) {
        if (this.isDebugTableModel() && SwingUtilities.isEventDispatchThread()) {
            org.appwork.loggingv3.LogV3.log(new WTFException("_fireTableStructureChanged inside EDT! "));
        }
        final List<E> tableData;
        if (refreshSort) {
            tableData = this.refreshSort(newtableData);
        } else {
            tableData = this.refreshUnSort(newtableData);
        }
        this._replaceTableData(tableData, true);
    }

    /**
     * this replaces the tables Data and Selection
     *
     * checkEditing tells if we should check for table Editing mode
     *
     * if it is false, we will replace the data for sure
     *
     * if it is true, we check whether the table is in editing mode. if so we add a propertychange listener and temporarily save the new
     * data and selection
     *
     * @param newTableData
     * @param selection
     * @param checkEditing
     */
    protected void _replaceTableData(final List<E> newTableData, final boolean checkEditing) {
        if (newTableData == null) {
            return;
        }
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                if (ExtTableModel.this.tableStructureChanging.getAndSet(false)) {
                    throw new IllegalStateException("_replaceTableData within _replaceTableData is forbidden!");
                }
                final ExtTable<E> ltable = ExtTableModel.this.getTable();
                final boolean replaceNow = !checkEditing || ltable == null || !ltable.isEditing();
                if (replaceNow) {
                    try {
                        if (ltable != null) {
                            /* replace now */
                            /* clear delayed TableData and Selection */
                            if (ExtTableModel.this.replaceDelayerSet) {
                                ExtTableModel.this.replaceDelayerSet = false;
                                ExtTableModel.this.delayedNewTableData = null;
                                ltable.removePropertyChangeListener(ExtTableModel.this.replaceDelayer);
                            }
                            /* replace TableData and set Selection */
                            final ListSelectionModel s = ltable.getSelectionModel();
                            final boolean adjusting = s.getValueIsAdjusting();
                            int leadIndex = -1;
                            int anchorIndex = -1;
                            E leadObject = null;
                            E anchorObject = null;
                            final boolean hadSelectedObjects = ExtTableModel.this.hasSelectedObjects();
                            final List<E> selectedObjects;
                            final BitSet selectedRowsBitSet = new BitSet();
                            try {
                                if (newTableData.size() > 0) {
                                    selectedObjects = ExtTableModel.this.getSelectedObjects(-1, selectedRowsBitSet);
                                    // we only need this variable to restore
                                    // selection after removing elements.
                                    // min = newtableData.size() < getTableData().size() ? findFirstSelectedRow(s) : -1;
                                    leadIndex = s.getLeadSelectionIndex();
                                    anchorIndex = s.getAnchorSelectionIndex();
                                    leadObject = adjusting && leadIndex >= 0 ? ExtTableModel.this.getObjectbyRow(leadIndex) : null;
                                    anchorObject = adjusting && anchorIndex >= 0 ? ExtTableModel.this.getObjectbyRow(anchorIndex) : null;
                                    if (adjusting && ExtTableModel.this.isDebugTableModel()) {
                                        System.out.println("before:leadIndex=" + leadIndex + "->" + leadObject + "|anchorIndex=" + anchorIndex + "->" + anchorObject);
                                    }
                                } else {
                                    selectedObjects = null;
                                }
                                final List<E> oldTableData = getTableData();
                                ExtTableModel.this.tableStructureChanging.set(true);
                                ExtTableModel.this.setTableData(newTableData);
                                try {
                                    ExtTableModel.this.tableSelectionClearing.set(true);
                                    ExtTableModel.this.fireTableStructureChanged();
                                } finally {
                                    ExtTableModel.this.tableSelectionClearing.set(false);
                                }
                                if (ExtTableModel.this.postSetTableData(newTableData) && ExtTableModel.this.getRowCount() > 0) {
                                    if (selectedObjects != null && selectedObjects.size() > 0) {
                                        int[] ret = ExtTableModel.this.setSelectedObjects(selectedObjects);
                                        if (ret[0] == -1) {
                                            final int[] rows = guessSelectedRows(oldTableData, leadIndex, anchorIndex, selectedRowsBitSet);
                                            if (rows != null && rows.length > 0) {
                                                ret = ExtTableModel.this.setSelectedRows(rows);
                                            }
                                        }
                                        if (adjusting) {
                                            if (ret[0] != -1 || (ret[0] != ret[1])) {
                                                if (leadObject != null) {
                                                    /*
                                                     * check if our leadObject does still exist
                                                     */
                                                    leadIndex = ExtTableModel.this.getRowforObject(leadObject);
                                                } else {
                                                    leadIndex = -1;
                                                }
                                                if (anchorObject != null) {
                                                    /*
                                                     * check if our anchorObject does still exist
                                                     */
                                                    anchorIndex = ExtTableModel.this.getRowforObject(anchorObject);
                                                } else {
                                                    anchorIndex = -1;
                                                }
                                                if (ExtTableModel.this.isDebugTableModel()) {
                                                    System.out.println("after:leadIndex=" + leadIndex + "->" + leadObject + "|anchorIndex=" + anchorIndex + "->" + anchorObject);
                                                }
                                                if (leadIndex >= 0 && anchorIndex >= 0) {
                                                    s.setValueIsAdjusting(true);
                                                    /*
                                                     * sort begin/end so we can loop through the items
                                                     */
                                                    int begin = leadIndex;
                                                    int end = anchorIndex;
                                                    if (end < begin) {
                                                        begin = anchorIndex;
                                                        end = leadIndex;
                                                    }
                                                    /*
                                                     * check if we have holes in our Selection
                                                     */
                                                    boolean selectionHole = false;
                                                    for (int index = begin; index <= end; index++) {
                                                        if (s.isSelectedIndex(index) == false) {
                                                            selectionHole = true;
                                                            break;
                                                        }
                                                    }
                                                    /*
                                                     * only set adjusting if we have a lead/anchor
                                                     */
                                                    if (ExtTableModel.this.isDebugTableModel()) {
                                                        if (selectionHole == false) {
                                                            System.out.println("No holes in selection: from " + begin + " to " + end);
                                                        } else {
                                                            System.out.println("Holes in selection: from " + begin + " to " + end);
                                                        }
                                                    }
                                                    s.setAnchorSelectionIndex(anchorIndex);
                                                    s.setLeadSelectionIndex(leadIndex);
                                                }
                                            }
                                        }
                                    }
                                }
                            } finally {
                                if (hadSelectedObjects && ExtTableModel.this.hasSelectedObjects() == false) {
                                    /*
                                     * we create empty selection event(toggle of valueadjusting) because tableSelectionClearing was used
                                     * during fireTableStructureChanged to avoid onSelectionChanged
                                     */
                                    s.setValueIsAdjusting(true);
                                    s.setAnchorSelectionIndex(1);
                                    s.setValueIsAdjusting(false);
                                }
                            }
                        } else {
                            ExtTableModel.this.tableStructureChanging.set(true);
                            ExtTableModel.this.setTableData(newTableData);
                            try {
                                ExtTableModel.this.tableSelectionClearing.set(true);
                                ExtTableModel.this.fireTableStructureChanged();
                            } finally {
                                ExtTableModel.this.tableSelectionClearing.set(false);
                            }
                            ExtTableModel.this.postSetTableData(newTableData);
                        }
                    } finally {
                        ExtTableModel.this.tableStructureChanging.set(false);
                    }
                } else {
                    /* replace later because table is in editing mode */
                    /* set delayed TableData and Selection */
                    if (ltable != null) {
                        ExtTableModel.this.delayedNewTableData = newTableData;
                        if (ExtTableModel.this.replaceDelayerSet == false) {
                            ExtTableModel.this.replaceDelayerSet = true;
                            ltable.addPropertyChangeListener(ExtTableModel.this.replaceDelayer);
                        }
                    }
                }
            }
        };
    }

    protected int[] guessSelectedRows(List<E> oldTableData, int leadIndex, int anchorIndex, BitSet selectedRowsBitSet) {
        return null;
    }

    public void addAllElements(final Collection<E> entries) {
        final java.util.List<E> newdata = new ArrayList<E>(this.getTableData());
        for (final E n : entries) {
            newdata.add(n);
        }
        this._fireTableStructureChanged(newdata, true);
    }

    /**
     * @param files
     */
    public void addAllElements(final E... files) {
        if (files == null || files.length == 0) {
            return;
        }
        final java.util.List<E> newdata = new ArrayList<E>(this.getTableData());
        for (final E n : files) {
            newdata.add(n);
        }
        this._fireTableStructureChanged(newdata, true);
    }

    /**
     * Add a new Column to the model
     *
     * @param e
     * @see #columns
     */
    public void addColumn(final ExtColumn<E> e) {
        e.setModel(this);
        columnModifications++;
        this.columns.add(e);
    }

    /**
     * Adds a new column at the given index
     *
     * @param e
     * @param index
     * @see #addColumn(ExtColumn)
     */
    public void addColumn(final ExtColumn<E> e, final int index) {
        e.setModel(this);
        columnModifications++;
        this.columns.add(index, e);
    }

    /**
     * @param at
     */
    public void addElement(final E at) {
        final java.util.List<E> newdata = new ArrayList<E>(this.getTableData());
        newdata.add(at);
        this._fireTableStructureChanged(newdata, true);
    }

    public void addExtComponentRowHighlighter(final ExtComponentRowHighlighter<E> h) {
        synchronized (this.highlighterLock) {
            final ArrayList<ExtComponentRowHighlighter<E>> newextComponentRowHighlighters = new ArrayList<ExtComponentRowHighlighter<E>>(this.extComponentRowHighlighters);
            newextComponentRowHighlighters.add(h);
            Collections.sort(newextComponentRowHighlighters, new Comparator<ExtComponentRowHighlighter<E>>() {
                @Override
                public int compare(final ExtComponentRowHighlighter<E> o1, final ExtComponentRowHighlighter<E> o2) {
                    return CompareUtils.compareInt(o1.getPriority(), o2.getPriority());
                }
            });
            this.extComponentRowHighlighters = newextComponentRowHighlighters;
        }
    }

    /**
     *
     */
    public void clear() {
        ExtTableModel.this._replaceTableData(new ArrayList<E>(), true);
    }

    /**
     * clears all selection models
     */
    public void clearSelection() {
        final ExtTable<E> ltable = this.getTable();
        if (ltable != null) {
            final ListSelectionModel selectionModel = ltable.getSelectionModel();
            if (selectionModel != null) {
                selectionModel.clearSelection();
            }
        }
    }

    /**
     * @param at
     * @return
     */
    public boolean contains(final E at) {
        return this.getTableData().contains(at);
    }

    public int countSelectedObjects() {
        final ExtTable<E> ltable = this.getTable();
        if (ltable == null) {
            return 0;
        }
        return ltable.getSelectedRowCount();
    }

    /**
     * Returns the Celleditor for the given column
     *
     * @param modelColumnIndex
     * @return
     */
    public TableCellEditor getCelleditorByColumn(final int modelColumnIndex) {
        /*
         * Math.max(0, columnIndex)
         *
         * WORKAROUND for -1 column access,Index out of Bound,Unknown why it happens but this workaround seems to do its job
         */
        return this.getExtColumnByModelIndex(modelColumnIndex);
    }

    /**
     * Returns the Cellrenderer for this column
     *
     * @param columnIndex
     * @return
     */
    public ExtColumn<E> getCellrendererByColumn(final int columnIndex) {
        /*
         * Math.max(0, columnIndex)
         *
         * WORKAROUND for -1 column access,Index out of Bound,Unknown why it happens but this workaround seems to do its job
         */
        return this.columns.get(Math.max(0, columnIndex));
    }

    @SuppressWarnings("unchecked")
    public <T extends ExtColumn<E>> T getColumnByClass(final Class<T> clazz) {
        try {
            for (final ExtColumn<?> column : this.columns) {
                if (column.getClass().equals(clazz)) {
                    return (T) column;
                }
            }
        } catch (final Exception e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
        return null;
    }

    @Override
    public Class<?> getColumnClass(final int columnIndex) {
        return Object.class;
    }

    /**
     * @return Returns the number of columns defined (incl. invisible columns)
     */
    public int getColumnCount() {
        return this.columns.size();
    }

    /**
     * @return the columns headername
     * @see ExtColumn#getName()
     */
    @Override
    public String getColumnName(final int column) {
        /*
         * Math.max(0, columnIndex)
         *
         * WORKAROUND for -1 column access,Index out of Bound,Unknown why it happens but this workaround seems to do its job
         */
        return this.columns.get(Math.max(0, column)).getName();
    }

    public java.util.List<ExtColumn<E>> getColumns() {
        return Collections.unmodifiableList(this.columns);
    }

    /**
     * @return the default sort column. override {@link #isSortStateSaverEnabled()} to return to default sort on each session
     */
    protected ExtColumn<E> getDefaultSortColumn() {
        for (final ExtColumn<E> c : this.columns) {
            if (c.isSortable(null)) {
                return c;
            }
        }
        return null;
    }

    public NumberFormat getNumberFormat(ExtColumn<E> column) {
        return column.getDefaultNumberFormat();
    }

    /**
     * @param i
     * @return
     */
    public E getElementAt(final int i) {
        final List<E> ltableData = this.getTableData();
        if (i >= 0 && i < ltableData.size()) {
            return ltableData.get(i);
        }
        return null;
    }

    /**
     * @return
     */
    public java.util.List<E> getElements() {
        return new ArrayList<E>(this.getTableData());
    }

    public ExtTableModelEventSender getEventSender() {
        if (this.eventSender == null) {
            this.eventSender = new ExtTableModelEventSender();
            this.addTableModelListener(new TableModelListener() {
                @Override
                public void tableChanged(final TableModelEvent e) {
                    ExtTableModel.this.eventSender.fireEvent(new ExtTableModelEventWrapper(ExtTableModel.this, e));
                }
            });
        }
        return this.eventSender;
    }

    /**
     * Returns the Columninstance
     *
     * @param columnIndex
     *            Model Index. NOT Real index
     * @return
     */
    public ExtColumn<E> getExtColumnByModelIndex(final int modelColumnIndex) {
        return this.columns.get(Math.max(0, modelColumnIndex));
    }

    /**
     * @param i
     * @return
     */
    public ExtColumn<E> getExtColumnByViewIndex(final int viewColumnIndex) {
        return this.getExtColumnByModelIndex(this.getTable().convertColumnIndexToModel(viewColumnIndex));
    }

    /**
     * @return a list of all ExtComponentRowHighlighters
     */
    public java.util.List<ExtComponentRowHighlighter<E>> getExtComponentRowHighlighters() {
        return this.extComponentRowHighlighters;
    }

    /**
     * @return
     */
    public int getExtViewColumnCount() {
        return this.getTable().getColumnCount();
    }

    /**
     * @return the modelID
     */
    public String getModelID() {
        return this.modelID;
    }

    /**
     * @param sortOrderIdentifier
     */
    /**
     * TODO: review sort identifier stuff and rewrite to use ENUMs
     *
     * @param sortOrderIdentifier
     * @return
     */
    public String getNextSortIdentifier(final String sortOrderIdentifier) {
        if (sortOrderIdentifier == null || sortOrderIdentifier.equals(ExtColumn.SORT_DESC)) {
            return ExtColumn.SORT_ASC;
        } else {
            return ExtColumn.SORT_DESC;
        }
    }

    /**
     * Returns the object that represents the row
     *
     * @param index
     * @return
     */
    public E getObjectbyRow(final int index) {
        return this.getElementAt(index);
    }

    /**
     * Returns how many rows the model contains
     *
     * @see #tableData
     */
    public int getRowCount() {
        return this.getTableData().size();
    }

    /**
     * Returns the row index for a given Object
     *
     * @param o
     * @return
     */
    public int getRowforObject(final E o) {
        return this.getTableData().indexOf(o);
    }

    /**
     * Returns all selected Objects
     *
     * @return
     */
    public List<E> getSelectedObjects() {
        return this.getSelectedObjects(-1);
    }

    public List<E> getSelectedObjects(final int maxItems) {
        return getSelectedObjects(maxItems, null);
    }

    public List<E> getSelectedObjects(final int maxItems, final BitSet selectedRows) {
        final ExtTable<E> ltable = this.getTable();
        final java.util.List<E> ret = new ArrayList<E>();
        if (ltable == null || this.tableSelectionClearing.get()) {
            return ret;
        }
        final ListSelectionModel selectionModel = ltable.getSelectionModel();
        if (selectionModel == null || selectionModel.isSelectionEmpty()) {
            return ret;
        }
        final List<E> ltableData = this.getTableData();
        final int iMin = selectionModel.getMinSelectionIndex();
        final int iMax = selectionModel.getMaxSelectionIndex();
        if (iMin == -1 || iMax == -1) {
            return ret;
        }
        if (iMin >= ltableData.size() || iMax >= ltableData.size()) {
            throw new IllegalStateException("SelectionModel and TableData missmatch! IMin:" + iMin + "|IMax:" + iMax + "|TableSize:" + ltableData.size());
        }
        if (maxItems < 0) {
            for (int i = iMin; i <= iMax; i++) {
                if (selectionModel.isSelectedIndex(i)) {
                    final E elem = ltableData.get(i);
                    if (elem != null) {
                        if (selectedRows != null) {
                            selectedRows.set(i);
                        }
                        ret.add(elem);
                    }
                }
            }
        } else {
            for (int i = iMin; i <= iMax; i++) {
                if (selectionModel.isSelectedIndex(i)) {
                    final E elem = ltableData.get(i);
                    if (elem != null) {
                        if (selectedRows != null) {
                            selectedRows.set(i);
                        }
                        ret.add(elem);
                        if (ret.size() > maxItems) {
                            break;
                        }
                    }
                }
            }
        }
        return ret;
    }

    /**
     * Returns the currently row sort column
     *
     * @return the {@link ExtTableModel#sortColumn}
     * @see ExtTableModel#sortColumn
     */
    public ExtColumn<E> getSortColumn() {
        return this.sortColumn;
    }

    /**
     * @param sortOrderIdentifier
     * @return
     */
    public Icon getSortIcon(final String sortOrderIdentifier) {
        if (sortOrderIdentifier == null || sortOrderIdentifier == ExtColumn.SORT_ASC) {
            return this.iconAsc;
        } else {
            return this.iconDesc;
        }
    }

    /**
     * @return
     */
    protected Storage getStorage() {
        return JSonStorage.getPlainStorage("ExtTable_" + this.getModelID());
    }

    /**
     * @return the {@link ExtTableModel#table}
     * @see ExtTableModel#table
     */
    public ExtTable<E> getTable() {
        return this.table;
    }

    /**
     * @return the {@link ExtTableModel#tableData}
     * @see ExtTableModel#tableData
     */
    public List<E> getTableData() {
        return this.tableData;
    }

    /**
     * returns a copy of current objects in tablemodel
     *
     * @return
     */
    public List<E> getTableObjects() {
        return new ArrayList<E>(this.getTableData());
    }

    /**
     * Returns the object for row 'rowIndex'. IN ExtTableModel, each row is represented By one single object. the ExtColums renderer just
     * renders each object in its way
     */
    public E getValueAt(final int rowIndex, final int columnIndex) {
        return this.getElementAt(rowIndex);
    }

    public boolean hasSelectedObjects() {
        return hasMinimumSelectedObjects(1);
    }

    public boolean hasMinimumSelectedObjects(final int num) {
        final ExtTable<E> ltable = this.getTable();
        if (ltable == null || this.isTableSelectionClearing()) {
            return false;
        } else {
            final ListSelectionModel selectionModel = ltable.getSelectionModel();
            final int iMin = selectionModel.getMinSelectionIndex();
            final int iMax = selectionModel.getMaxSelectionIndex();
            if (iMin == -1 || iMax == -1 || ((iMax - iMin) + 1 < num)) {
                return false;
            } else {
                int count = 0;
                for (int i = iMin; i <= iMax; i++) {
                    if (selectionModel.isSelectedIndex(i)) {
                        if (++count == num) {
                            return true;
                        }
                    }
                }
                return false;
            }
        }
    }

    /**
     * Initialize the colums.
     *
     * e.g.: this.addColumn(new NameColumn("Name", this));<br>
     * this.addColumn(new UploadDateColumn("Upload date", this));<br>
     * this.addColumn(new FilesizeColumn("Size", this));<br>
     *
     */
    protected abstract void initColumns();

    protected void initModel() {
        this.initColumns();
        for (ExtColumn<E> c : getColumns()) {
            if (c.isAutoWidthEnabled()) {
                autoWidthEnabled = true;
                break;
            }
        }
        final ExtColumn<E> defSortColumn = this.getDefaultSortColumn();
        String columnId = defSortColumn == null ? null : defSortColumn.getID();
        String columnSortMode = null;
        if (this.isSortStateSaverEnabled()) {
            columnId = this.getStorage().get(ExtTableModel.SORTCOLUMN_KEY, columnId);
            columnSortMode = this.getStorage().get(ExtTableModel.SORT_ORDER_ID_KEY, null);
            /*
             * restore sortMode by using shared StringObject so that String == String works
             */
            if (columnSortMode != null) {
                if (columnSortMode.equals(ExtColumn.SORT_ASC)) {
                    columnSortMode = ExtColumn.SORT_ASC;
                } else if (columnSortMode.equals(ExtColumn.SORT_DESC)) {
                    columnSortMode = ExtColumn.SORT_DESC;
                }
            }
        }
        if (columnId != null) {
            for (final ExtColumn<E> col : this.columns) {
                if (col.getID().equals(columnId)) {
                    col.setSortOrderIdentifier(columnSortMode);
                    this.sortColumn = col;
                    break;
                }
            }
        }
    }

    public boolean isAutoWidthEnabled() {
        return autoWidthEnabled;
    }

    public void setAutoWidthEnabled(boolean autoWidthEnabled) {
        this.autoWidthEnabled = autoWidthEnabled;
    }

    /**
     * @return if the cell is editable. This information is stored in the ExtColumn instance.
     * @see ExtColumn#isCellEditable(int, int)
     */
    @Override
    public boolean isCellEditable(final int rowIndex, final int columnIndex) {
        return this.columns.get(columnIndex).isCellEditable(rowIndex, columnIndex);
    }

    /**
     * @return the debugTableModel
     */
    public boolean isDebugTableModel() {
        return this.debugTableModel;
    }

    /**
     * checks if this column is allowed to be hidden
     *
     * @param column
     * @return
     */
    public boolean isHidable(final int column) {
        final ExtColumn<E> col = this.getExtColumnByModelIndex(column);
        try {
            return col.isHidable();
        } catch (final Exception e) {
            org.appwork.loggingv3.LogV3.log(e);
            return true;
        }
    }

    protected boolean isSortStateSaverEnabled() {
        return true;
    }

    public boolean isTableSelectionClearing() {
        return this.tableSelectionClearing.get();
    }

    public boolean isTableStructureChanging() {
        return this.tableStructureChanging.get();
    }

    /**
     * Retrieves visible information form database interface to determine if the column is visible or not
     *
     * @param column
     * @return
     */
    public boolean isColumnVisible(final int column) {
        final ExtColumn<E> col = this.getExtColumnByModelIndex(column);
        return isColumnVisible(col);
    }

    /**
     * @param col
     * @return
     */
    public boolean isColumnVisible(final ExtColumn<E> col) {
        try {
            return col.isVisible(this.getTable().getColumnStore("VISABLE_COL_", col.getID(), col.isDefaultVisible()));
        } catch (final Exception e) {
            org.appwork.loggingv3.LogV3.log(e);
            return true;
        }
    }

    /**
     * call this for drag&drop or cut&paste
     *
     * @param transferData
     * @param dropRow
     * @return
     */
    public boolean move(final java.util.List<E> transferData, final int dropRow) {
        try {
            final java.util.List<E> newdata = new ArrayList<E>(this.getTableData().size());
            final List<E> before = new ArrayList<E>(this.getTableData().subList(0, dropRow));
            final List<E> after = new ArrayList<E>(this.getTableData().subList(dropRow, this.getTableData().size()));
            before.removeAll(transferData);
            after.removeAll(transferData);
            newdata.addAll(before);
            newdata.addAll(transferData);
            newdata.addAll(after);
            this._fireTableStructureChanged(newdata, true);
            return true;
        } catch (final Throwable t) {
            t.printStackTrace();
        }
        return false;
    }

    /*
     * this will be called after fireTableStructureChanged. you can customize everything after this
     *
     * true = restore selection
     *
     * false = do not restore selection
     */
    protected boolean postSetTableData(final List<E> newtableData) {
        return true;
    }

    /**
     * Restores the sort order according to {@link #getSortColumn()} and {@link #getSortOrderIdentifier() == ExtColumn.SORT_ASC}
     *
     *
     */
    public void refreshSort() {
        this._fireTableStructureChanged(this.getTableObjects(), true);
    }

    public List<E> refreshSort(final List<E> data) {
        if (this.isDebugTableModel() && SwingUtilities.isEventDispatchThread()) {
            org.appwork.loggingv3.LogV3.log(new WTFException("refreshSort inside EDT! "));
        }
        boolean sameTable = false;
        if (data == this.getTableData()) {
            sameTable = true;
        }
        final List<E> ret = this.sort(data, this.sortColumn);
        if (this.isDebugTableModel() && this.getTableData() == ret && sameTable) {
            org.appwork.loggingv3.LogV3.log(new WTFException("WARNING: sorting on live backend!"));
        }
        return ret;
    }

    public List<E> refreshUnSort(final List<E> data) {
        return data;
    }

    /**
     * @param selectedObjects
     */
    public void removeAll(final java.util.List<E> selectedObjects) {
        final java.util.List<E> tmp = new ArrayList<E>(this.getTableData());
        tmp.removeAll(selectedObjects);
        this._fireTableStructureChanged(tmp, true);
    }

    /**
     * @param startRow
     * @param ret
     * @param caseSensitive
     * @param regex
     * @return
     */
    public E searchNextObject(final int startRow, final String ret, final boolean caseSensitive, final boolean regex) {
        final Pattern p;
        if (!regex) {
            final String[] pats = ret.split("\\*");
            final StringBuilder pattern = new StringBuilder();
            for (final String pp : pats) {
                if (pattern.length() > 0) {
                    pattern.append(".*?");
                }
                pattern.append(Pattern.quote(pp));
            }
            p = Pattern.compile(".*?" + pattern.toString() + ".*?", caseSensitive == false ? Pattern.CASE_INSENSITIVE : 0 | Pattern.DOTALL);
        } else {
            p = Pattern.compile(".*?" + ret + ".*?", caseSensitive == false ? Pattern.CASE_INSENSITIVE : 0 | Pattern.DOTALL);
        }
        final List<E> ltableData = this.getTableData();
        for (int i = startRow; i < ltableData.size(); i++) {
            for (int c = 0; c < this.columns.size(); c++) {
                if (this.columns.get(c).matchSearch(ltableData.get(i), p)) {
                    return ltableData.get(i);
                }
            }
        }
        for (int i = 0; i < startRow; i++) {
            for (int c = 0; c < this.columns.size(); c++) {
                if (this.columns.get(c).matchSearch(ltableData.get(i), p)) {
                    return ltableData.get(i);
                }
            }
        }
        return null;
    }

    /**
     * @param column
     * @param b
     */
    public void setColumnVisible(final ExtColumn<E> column, final boolean visible) {
        if (isColumnVisible(column) != visible) {
            try {
                this.getTable().getStorage().put(this.getTable().getColumnStoreKey("VISABLE_COL_", column.getID()), visible);
            } catch (final Exception e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
            this.getTable().updateColumns();
        }
    }

    /**
     * Sets the column visible or invisible. This information is stored in database interface for cross session use.
     *
     *
     * @param column
     * @param visible
     */
    public void setColumnVisible(final int modelColumnIndex, final boolean visible) {
        this.setColumnVisible(this.getExtColumnByModelIndex(modelColumnIndex), visible);
    }

    /**
     * @param debugTableModel
     *            the debugTableModel to set
     */
    public void setDebugTableModel(final boolean debugTableModel) {
        this.debugTableModel = debugTableModel;
    }

    /**
     * @param latest
     */
    public void setSelectedObject(final E latest) {
        if (latest == null) {
            setSelectedObjects(null);
        } else {
            final List<E> objects = new ArrayList<E>();
            objects.add(latest);
            setSelectedObjects(objects);
        }
    }

    /**
     * Sets the current selection to the given objects
     *
     * @param selections
     */
    public int[] setSelectedObjects(final Collection<E> selections) {
        return new EDTHelper<int[]>() {
            @Override
            public int[] edtRun() {
                final ExtTable<E> ltable = ExtTableModel.this.getTable();
                if (ltable == null) {
                    return new int[] { -1, -1 };
                }
                final ListSelectionModel s = ltable.getSelectionModel();
                final boolean isValueAdjusting = s.getValueIsAdjusting();
                s.setValueIsAdjusting(true);
                try {
                    if (ExtTableModel.this.hasSelectedObjects()) {
                        ExtTableModel.this.clearSelection();
                    }
                    final List<E> ltableData = getTableData();
                    if (selections == null || selections.size() == 0 || ltableData.size() == 0) {
                        return new int[] { -1, -1 };
                    }
                    // Transform to rowindex list
                    final int tableDataSize = ltableData.size();
                    final int selectionSize = selections.size();
                    int[] selectedRows = new int[selectionSize];
                    int selectedRowsCounter = 0;
                    int lastOptimizedIndex = 0;
                    final ArrayList<E> unoptimizedSelection = new ArrayList<E>();
                    final int[] maxUnoptimizedIndices = new int[selectionSize];
                    int unoptimizedIndex = 0;
                    optmizedLoop: for (E obj : selections) {
                        for (int tableIndex = lastOptimizedIndex; tableIndex < tableDataSize; tableIndex++) {
                            if (obj == ltableData.get(tableIndex)) {
                                lastOptimizedIndex = tableIndex;
                                selectedRows[selectedRowsCounter++] = tableIndex;
                                continue optmizedLoop;
                            }
                        }
                        unoptimizedSelection.add(obj);
                        maxUnoptimizedIndices[unoptimizedIndex++] = lastOptimizedIndex;
                    }
                    final int unoptimizedSize = unoptimizedSelection.size();
                    findLoop: for (int findIndex = 0; findIndex < unoptimizedSize; findIndex++) {
                        final E obj = unoptimizedSelection.get(findIndex);
                        final int max = maxUnoptimizedIndices[findIndex];
                        for (int tableIndex = 0; tableIndex < max; tableIndex++) {
                            if (obj == ltableData.get(tableIndex)) {
                                selectedRows[selectedRowsCounter++] = tableIndex;
                                continue findLoop;
                            }
                        }
                    }
                    if (selectedRowsCounter == 0) {
                        return new int[] { -1, -1 };
                    }
                    selectedRows = Arrays.copyOf(selectedRows, selectedRowsCounter);
                    Arrays.sort(selectedRows);
                    int index0 = -1;
                    int index1 = -1;
                    for (int selectedRowIndex = 0; selectedRowIndex < selectedRowsCounter; selectedRowIndex++) {
                        final int row = selectedRows[selectedRowIndex];
                        if (index0 < 0) {
                            index0 = row;
                        } else {
                            if (index1 < 0) {
                                if (row == index0 + 1) {
                                    index1 = row;
                                } else {
                                    ltable.addRowSelectionInterval(index0, index0);
                                    index0 = row;
                                }
                            } else {
                                if (row == index1 + 1) {
                                    index1 = row;
                                } else {
                                    ltable.addRowSelectionInterval(index0, index1);
                                    index0 = row;
                                    index1 = -1;
                                }
                            }
                        }
                    }
                    if (index0 >= 0) {
                        if (index1 < 0) {
                            ltable.addRowSelectionInterval(index0, index0);
                        } else {
                            ltable.addRowSelectionInterval(index0, index1);
                        }
                    }
                    return new int[] { selectedRows[0], selectedRows[selectedRowsCounter - 1] };
                } finally {
                    s.setValueIsAdjusting(isValueAdjusting);
                }
            }
        }.getReturnValue();
    }

    public int[] setSelectedRows(final int[] rows) {
        return new EDTHelper<int[]>() {
            @Override
            public int[] edtRun() {
                final ExtTable<E> ltable = ExtTableModel.this.getTable();
                final ListSelectionModel s = ltable.getSelectionModel();
                final boolean isValueAdjusting = s.getValueIsAdjusting();
                s.setValueIsAdjusting(true);
                try {
                    if (ExtTableModel.this.hasSelectedObjects()) {
                        ExtTableModel.this.clearSelection();
                    }
                    if (rows == null || rows.length == 0 || ExtTableModel.this.getTableData().size() == 0) {
                        return new int[] { -1, -1 };
                    }
                    final int[] selectedRows = rows.clone();
                    Arrays.sort(selectedRows);
                    int index0 = -1;
                    int index1 = -1;
                    int rowIndex = 0;
                    for (rowIndex = 0; rowIndex < selectedRows.length; rowIndex++) {
                        final int row = selectedRows[rowIndex];
                        if (index0 < 0) {
                            index0 = row;
                        } else {
                            if (index1 < 0) {
                                if (row == index0 + 1) {
                                    index1 = row;
                                } else {
                                    ltable.addRowSelectionInterval(index0, index0);
                                    index0 = row;
                                }
                            } else {
                                if (row == index1 + 1) {
                                    index1 = row;
                                } else {
                                    ltable.addRowSelectionInterval(index0, index1);
                                    index0 = row;
                                    index1 = -1;
                                }
                            }
                        }
                    }
                    if (index0 >= 0) {
                        if (index1 < 0) {
                            ltable.addRowSelectionInterval(index0, index0);
                        } else {
                            ltable.addRowSelectionInterval(index0, index1);
                        }
                    }
                    return new int[] { selectedRows[0], selectedRows[selectedRows.length - 1] };
                } finally {
                    s.setValueIsAdjusting(isValueAdjusting);
                }
            }
        }.getReturnValue();
    }

    public void setSortColumn(final ExtColumn<E> e) {
        this.sortColumn = e;
    }

    /**
     * Sets the table in which the model is used. This method should only be used internally.
     *
     * @param table
     */
    protected void setTable(final ExtTable<E> table) {
        this.table = table;
        if (isUpdateTableColumnsOnTableResize()) {
            table.addComponentListener(new ComponentAdapter() {
                @Override
                public void componentResized(final ComponentEvent e) {
                    for (final ExtColumn<E> c : getColumns()) {
                        c.onTableResized();
                    }
                    autoColumnWidth();
                }
            });
        }
    }

    /**
     * override if you want getMinWidth and getMaxWidth in the columns to get re-evaluated if the table size changes
     *
     * @return
     */
    protected boolean isUpdateTableColumnsOnTableResize() {
        return false;
    }

    protected void setTableData(final List<E> data) {
        this.tableData = data;
    }

    // TODO docu
    @Override
    public void setValueAt(final Object value, final int rowIndex, final int columnIndex) {
        this.columns.get(columnIndex).setValueAt(value, rowIndex, columnIndex);
    }

    /**
     * Sorts given modeldata with the column's rowsorter
     *
     **/
    public List<E> sort(final List<E> data, final ExtColumn<E> column) {
        this.sortColumn = column;
        if (column != null) {
            final String id = column.getSortOrderIdentifier();
            try {
                this.getStorage().put(ExtTableModel.SORT_ORDER_ID_KEY, id);
                this.getStorage().put(ExtTableModel.SORTCOLUMN_KEY, column.getID());
            } catch (final Exception e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
            try {
                Collections.sort(data, column.getRowSorter());
            } catch (final Throwable e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
        } else {
            try {
                this.getStorage().put(ExtTableModel.SORT_ORDER_ID_KEY, (String) null);
                this.getStorage().put(ExtTableModel.SORTCOLUMN_KEY, (String) null);
            } catch (final Exception e) {
                org.appwork.loggingv3.LogV3.log(e);
            }
        }
        return data;
    }

    protected int findFirstSelectedRow(final ListSelectionModel s) {
        int min;
        min = s.getMinSelectionIndex();
        return min;
    }
}
