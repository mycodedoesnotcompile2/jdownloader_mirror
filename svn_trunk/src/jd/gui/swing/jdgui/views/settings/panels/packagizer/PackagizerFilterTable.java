package jd.gui.swing.jdgui.views.settings.panels.packagizer;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.swing.DropMode;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.ListSelectionModel;

import jd.gui.swing.jdgui.BasicJDTable;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtTransferHandler;
import org.appwork.utils.Application;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.controlling.packagizer.PackagizerController;
import org.jdownloader.controlling.packagizer.PackagizerRule;
import org.jdownloader.logging.LogController;

public class PackagizerFilterTable extends BasicJDTable<PackagizerRule> {
    private static final long      serialVersionUID = 4698030718806607175L;
    private final PackagizerFilter packagizer;

    public PackagizerFilterTable(PackagizerFilter packagizer) {
        super(new FilterTableModel("PackagizerFilterTable"));
        this.setSearchEnabled(true);
        getTableHeader().setReorderingAllowed(false);
        this.setDragEnabled(true);
        setTransferHandler(new PackagizerRuleTableTransferHandler());
        if (Application.getJavaVersion() >= Application.JAVA16) {
            setDropMode(DropMode.INSERT_ROWS);
        }
        setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        this.packagizer = packagizer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.swing.exttable.ExtTable#onContextMenu(javax.swing.JPopupMenu , java.lang.Object, java.util.ArrayList,
     * org.appwork.swing.exttable.ExtColumn)
     */
    @Override
    protected JPopupMenu onContextMenu(JPopupMenu popup, PackagizerRule contextObject, java.util.List<PackagizerRule> selection, ExtColumn<PackagizerRule> column, MouseEvent ev) {
        popup.add(new JMenuItem(new NewAction(this)));
        popup.add(new JMenuItem(new RemoveAction(this, selection, false)));
        popup.add(new JMenuItem(new DuplicateAction(contextObject, this)));
        popup.addSeparator();
        popup.add(new ExportAction(packagizer, selection));
        return popup;
    }

    @Override
    protected boolean onDoubleClick(MouseEvent e, PackagizerRule obj) {
        PackagizerFilterRuleDialog.showDialog(obj, new Runnable() {
            @Override
            public void run() {
                PackagizerController.getInstance().update();
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        getModel().fireTableDataChanged();
                    }
                };
            }
        });
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.swing.exttable.ExtTable#onShortcutDelete(java.util.ArrayList , java.awt.event.KeyEvent, boolean)
     */
    @Override
    protected boolean onShortcutDelete(java.util.List<PackagizerRule> selectedObjects, KeyEvent evt, boolean direct) {
        new RemoveAction(this, selectedObjects, direct).actionPerformed(null);
        return true;
    }

    /**
     * {@link ExtTransferHandler} for the packagizer rule table. In addition to the internal row reordering handled by the super
     * class, it accepts drops of *{@link ImportAction#EXT} files from the OS file manager, importing the contained rules just like
     * the import button does.
     */
    private static class PackagizerRuleTableTransferHandler extends ExtTransferHandler<PackagizerRule> {
        private static final long serialVersionUID = 1L;

        @Override
        public boolean canImport(final TransferSupport support) {
            if (support.isDrop() && support.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
                return true;
            }
            return super.canImport(support);
        }

        @Override
        public boolean importData(final TransferSupport support) {
            if (support.isDrop() && support.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
                return importFiles(support);
            }
            return super.importData(support);
        }

        @SuppressWarnings("unchecked")
        private boolean importFiles(final TransferSupport support) {
            final List<File> files;
            try {
                files = (List<File>) support.getTransferable().getTransferData(DataFlavor.javaFileListFlavor);
            } catch (final UnsupportedFlavorException e) {
                LogController.CL().log(e);
                return false;
            } catch (final IOException e) {
                LogController.CL().log(e);
                return false;
            }
            if (files == null || files.size() == 0) {
                return false;
            }
            // Read/parse/import off the EDT: importList does file IO and may show a modal dialog, which would block the GUI.
            final Thread thread = new Thread("Import packagizer rules") {
                {
                    setDaemon(true);
                }

                @Override
                public void run() {
                    for (final File file : files) {
                        if (file == null || !file.isFile() || !StringUtils.endsWithCaseInsensitive(file.getName(), ImportAction.EXT)) {
                            continue;
                        }
                        PackagizerController.getInstance().importList(file, true);
                    }
                }
            };
            thread.start();
            return true;
        }
    }
}
