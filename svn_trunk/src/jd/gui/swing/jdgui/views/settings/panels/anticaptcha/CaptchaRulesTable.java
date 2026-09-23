package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

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

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtTransferHandler;
import org.appwork.utils.Application;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.captcha.v2.CaptchaChallengeFilter;
import org.jdownloader.captcha.v2.CaptchaChallengeFilterController;
import org.jdownloader.logging.LogController;

import jd.gui.swing.jdgui.BasicJDTable;

/**
 * Table for {@link CaptchaChallengeFilter} rules. Behaves like
 * {@link jd.gui.swing.jdgui.views.settings.panels.packagizer.PackagizerFilterTable}: rows can be dragged to reorder them (handled by
 * {@link CaptchaRulesTableModel#move(List, int)}), files with the {@link CaptchaRulesExportAction#EXT} extension can be dropped from the
 * OS file manager to import them (like the Import button), and the right-click menu offers Add/Remove/Duplicate/Export.
 */
public class CaptchaRulesTable extends BasicJDTable<CaptchaChallengeFilter> {
    private static final long serialVersionUID = 1L;

    public CaptchaRulesTable(final CaptchaRulesTableModel model) {
        super(model);
        setSearchEnabled(true);
        getTableHeader().setReorderingAllowed(false);
        setDragEnabled(true);
        setTransferHandler(new CaptchaRulesTableTransferHandler());
        if (Application.getJavaVersion() >= Application.JAVA16) {
            setDropMode(DropMode.INSERT_ROWS);
        }
        setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    }

    @Override
    public CaptchaRulesTableModel getModel() {
        return (CaptchaRulesTableModel) super.getModel();
    }

    @Override
    protected JPopupMenu onContextMenu(final JPopupMenu popup, final CaptchaChallengeFilter contextObject, final List<CaptchaChallengeFilter> selection, final ExtColumn<CaptchaChallengeFilter> column, final MouseEvent ev) {
        popup.add(new JMenuItem(new CaptchaRulesAddAction(this)));
        popup.add(new JMenuItem(new CaptchaRulesRemoveAction(this, selection, false)));
        popup.add(new JMenuItem(new CaptchaRulesDuplicateAction(contextObject, this)));
        popup.addSeparator();
        popup.add(new JMenuItem(new CaptchaRulesExportAction(this, selection)));
        return popup;
    }

    @Override
    protected boolean onShortcutDelete(final List<CaptchaChallengeFilter> selectedObjects, final KeyEvent evt, final boolean direct) {
        new CaptchaRulesRemoveAction(this, selectedObjects, direct).actionPerformed(null);
        return true;
    }

    /**
     * {@link ExtTransferHandler} for the captcha rules table. In addition to the internal row reordering handled by the super class, it
     * accepts drops of *{@link CaptchaRulesExportAction#EXT} files from the OS file manager, importing the contained rules just like the
     * import button does.
     */
    private class CaptchaRulesTableTransferHandler extends ExtTransferHandler<CaptchaChallengeFilter> {
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
            if (files == null || files.isEmpty()) {
                return false;
            }
            /* Read/parse/import off the EDT: importList does file IO and may show a modal dialog, which would block the GUI. */
            final Thread thread = new Thread("Import captcha rules") {
                {
                    setDaemon(true);
                }

                @Override
                public void run() {
                    for (final File file : files) {
                        if (file != null && file.isFile() && StringUtils.endsWithCaseInsensitive(file.getName(), CaptchaRulesExportAction.EXT)) {
                            CaptchaChallengeFilterController.getInstance().importList(file);
                        }
                    }
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            getModel().refresh();
                        }
                    };
                }
            };
            thread.start();
            return true;
        }
    }
}
