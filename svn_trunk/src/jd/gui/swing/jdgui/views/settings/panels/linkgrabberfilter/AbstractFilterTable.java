package jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.File;
import java.io.IOException;
import java.util.List;

import jd.gui.swing.jdgui.BasicJDTable;

import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.ExtTransferHandler;
import org.appwork.utils.StringUtils;
import org.jdownloader.controlling.filter.LinkFilterController;
import org.jdownloader.controlling.filter.LinkgrabberFilterRule;
import org.jdownloader.logging.LogController;

public class AbstractFilterTable extends BasicJDTable<LinkgrabberFilterRule> {

    public AbstractFilterTable(ExtTableModel<LinkgrabberFilterRule> tableModel, String importExtension) {
        super(tableModel);
        setTransferHandler(new LinkgrabberFilterRuleTableTransferHandler(importExtension));
    }

    /**
     * {@link ExtTransferHandler} that accepts drops of *{@code importExtension} files from the OS file manager, importing the
     * contained rules just like the import button does.
     */
    private static class LinkgrabberFilterRuleTableTransferHandler extends ExtTransferHandler<LinkgrabberFilterRule> {
        private static final long  serialVersionUID = 1L;
        private final String       ext;

        public LinkgrabberFilterRuleTableTransferHandler(final String ext) {
            this.ext = ext;
        }

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
            final String ext = this.ext;
            final Thread thread = new Thread("Import filter rules") {
                {
                    setDaemon(true);
                }

                @Override
                public void run() {
                    for (final File file : files) {
                        if (file == null || !file.isFile() || !StringUtils.endsWithCaseInsensitive(file.getName(), ext)) {
                            continue;
                        }
                        LinkFilterController.getInstance().importList(file, true);
                    }
                }
            };
            thread.start();
            return true;
        }
    }
}
