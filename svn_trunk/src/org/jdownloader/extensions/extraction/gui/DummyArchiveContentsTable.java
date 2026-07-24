package org.jdownloader.extensions.extraction.gui;

import java.awt.Color;
import java.util.Collections;
import java.util.List;

import jd.gui.swing.jdgui.BasicJDTable;

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
}
