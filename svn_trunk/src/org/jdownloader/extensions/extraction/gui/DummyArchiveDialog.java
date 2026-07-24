package org.jdownloader.extensions.extraction.gui;

import java.awt.Dimension;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JScrollPane;

import org.appwork.swing.MigPanel;
import org.appwork.uio.UIOManager;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.extensions.extraction.DummyArchive;
import org.jdownloader.extensions.extraction.translate.T;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.AbstractIcon;

public class DummyArchiveDialog extends AbstractDialog<Object> {
    private final List<DummyArchive> archives;

    public DummyArchiveDialog(DummyArchive da) {
        this(Collections.singletonList(da));
    }

    public DummyArchiveDialog(List<DummyArchive> archives) {
        super(Dialog.STYLE_HIDE_ICON | UIOManager.BUTTONS_HIDE_CANCEL, buildTitle(archives), null, T.T.close(), null);
        this.archives = archives;
    }

    private static String buildTitle(List<DummyArchive> archives) {
        if (archives.size() == 1) {
            return T.T.dummyarchivedialog_title(archives.get(0).getName());
        } else {
            return T.T.dummyarchivedialog_title_multi(archives.size());
        }
    }

    @Override
    protected Object createReturnValue() {
        return null;
    }

    @Override
    protected boolean isResizable() {
        return true;
    }

    @Override
    public JComponent layoutDialogContent() {
        MigPanel d = new MigPanel("ins 5,wrap 1", "[]", "[grow,fill]");
        boolean allComplete = true;
        int totalSize = 0;
        final LinkedHashSet<String> types = new LinkedHashSet<String>();
        for (final DummyArchive archive : archives) {
            if (!archive.isComplete()) {
                allComplete = false;
            }
            totalSize += archive.getSize();
            if (archive.getType() != null) {
                types.add(archive.getType());
            }
        }
        final String typeInfo = types.toString().replaceAll("^\\[|\\]$", "");
        if (allComplete) {
            JLabel lbl = new JLabel();
            d.add(lbl, "pushx,growx");
            lbl.setIcon(new AbstractIcon(IconKey.ICON_OK, 32));
            lbl.setText(T.T.ValidateArchiveAction_actionPerformed_(totalSize));
            lbl = new JLabel();
            d.add(lbl, "pushx,growx");
            lbl.setIcon(new AbstractIcon(IconKey.ICON_INFO, 32));
            lbl.setText(T.T.ValidateArchiveAction_actionPerformed_information(typeInfo));
        } else {
            JLabel lbl = new JLabel();
            d.add(lbl, "pushx,growx");
            lbl.setIcon(new AbstractIcon(IconKey.ICON_STOP, 32));
            lbl.setText(T.T.ValidateArchiveAction_actionPerformed_bad(totalSize));
            lbl = new JLabel();
            d.add(lbl, "pushx,growx");
            lbl.setIcon(new AbstractIcon(IconKey.ICON_INFO, 32));
            lbl.setText(T.T.ValidateArchiveAction_actionPerformed_information(typeInfo));
        }
        DummyArchiveContentsTable table = new DummyArchiveContentsTable(archives);
        d.add(new JScrollPane(table), "spanx,pushx,growx");
        setPreferredSize(new Dimension(500, 500));
        return d;
    }
}
