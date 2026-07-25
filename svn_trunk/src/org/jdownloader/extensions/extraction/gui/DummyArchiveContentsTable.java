package org.jdownloader.extensions.extraction.gui;

import java.awt.Color;
import java.awt.event.KeyEvent;
import java.util.Collections;
import java.util.List;

import jd.gui.swing.jdgui.BasicJDTable;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtOverlayRowHighlighter;
import org.appwork.swing.exttable.ExtTable;
import org.appwork.utils.ColorUtils;
import org.jdownloader.extensions.extraction.DummyArchive;
import org.jdownloader.extensions.extraction.DummyArchiveFile;
import org.jdownloader.extensions.extraction.bindings.crawledlink.CrawledLinkArchiveFile;

public class DummyArchiveContentsTable extends BasicJDTable<DummyArchiveFile> {

    public DummyArchiveContentsTable(DummyArchive da) {
        this(Collections.singletonList(da));
    }

    public DummyArchiveContentsTable(List<DummyArchive> archives) {
        super(new DummyArchiveContentsTableModel(archives));
        boolean linkgrabber = false;
        outer: for (DummyArchive da : archives) {
            for (DummyArchiveFile daf : da.getList()) {
                if (daf.getArchiveFile() != null && daf.getArchiveFile() instanceof CrawledLinkArchiveFile) {
                    linkgrabber = true;
                    break outer;
                }
            }
        }

        getModel().setColumnVisible(((DummyArchiveContentsTableModel) getModel()).getPackageName(), linkgrabber);
        getModel().setColumnVisible(((DummyArchiveContentsTableModel) getModel()).getLinkStatus(), linkgrabber);
        addRowHighlighter(new ExtOverlayRowHighlighter(null, ColorUtils.getAlphaInstance(Color.RED, 20)) {

            @Override
            public boolean doHighlight(ExtTable<?> extTable, int row) {
                final DummyArchiveFile e = getModel().getObjectbyRow(row);
                return e.isMissing();
            }
        });
        addRowHighlighter(new ExtOverlayRowHighlighter(null, ColorUtils.getAlphaInstance(Color.ORANGE, 20)) {

            @Override
            public boolean doHighlight(ExtTable<?> extTable, int row) {
                final DummyArchiveFile e = getModel().getObjectbyRow(row);
                return Boolean.TRUE.equals(e.isIncomplete()) && !e.isLocalFileAvailable();
            }
        });

        addRowHighlighter(new ExtOverlayRowHighlighter(null, ColorUtils.getAlphaInstance(Color.GREEN, 20)) {

            @Override
            public boolean doHighlight(ExtTable<?> extTable, int row) {
                final DummyArchiveFile e = getModel().getObjectbyRow(row);
                return e.isLocalFileAvailable();
            }
        });
    }

    /**
     * CTRL+C handler (ExtTable hook): if the focused cell (last clicked, not the whole row selection) is in the package name column, copies
     * the package names of all selected rows to the clipboard - deduplicated, one per line - and consumes the event. For any other column
     * returns false so the default shortcut handling stays untouched.
     */
    @Override
    protected boolean onShortcutCopy(List<DummyArchiveFile> selectedObjects, KeyEvent evt) {
        if (selectedObjects == null || selectedObjects.size() == 0) {
            return false;
        }
        final int viewColumn = getSelectedColumn();
        if (viewColumn == -1) {
            return false;
        }
        final DummyArchiveContentsTableModel model = (DummyArchiveContentsTableModel) getModel();
        final ExtColumn<DummyArchiveFile> column = model.getExtColumnByModelIndex(convertColumnIndexToModel(viewColumn));
        if (column != model.getPackageName()) {
            return false;
        }
        return model.copyPackageNamesToClipboard(selectedObjects);
    }
}
