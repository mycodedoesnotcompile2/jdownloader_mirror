package org.jdownloader.extensions.extraction.contextmenu.downloadlist.action;

import java.awt.event.ActionEvent;
import java.util.List;

import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.WarnLevel;

import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.BooleanStatus;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.AbstractExtractionContextAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.SelectionInfo;

public class AutoExtractEnabledToggleAction extends AbstractExtractionContextAction {

    public AutoExtractEnabledToggleAction() {
        super();
        setName(org.jdownloader.extensions.extraction.translate.T.T.contextmenu_autoextract());
        setIconKey(IconKey.ICON_REFRESH);
        setSelected(false);
    }

    public void setSelected(final boolean selected) {
        super.setSelected(selected);
        if (isSelected()) {
            setName(org.jdownloader.extensions.extraction.translate.T.T.contextmenu_disable_auto_extract2());
        } else {
            setName(org.jdownloader.extensions.extraction.translate.T.T.contextmenu_disable_auto_extract2());
        }
    }

    @Override
    protected void onAsyncInitDone(List<Archive> archives, SelectionInfo<?, ?> selectionInfo) {
        super.onAsyncInitDone(archives, selectionInfo);
        if (archives == null || archives.size() == 0) {
            return;
        }
        setSelected(_getExtension().isAutoExtractEnabled(archives.get(0)));
    }

    @Override
    protected void onActionPerformed(ActionEvent e, List<Archive> archives, SelectionInfo<?, ?> selectionInfo) {
        for (Archive archive : archives) {
            archive.setAutoExtract(isSelected() ? BooleanStatus.TRUE : BooleanStatus.FALSE);
        }
        if (JDGui.bugme(WarnLevel.NORMAL)) {
            Dialog.getInstance().showMessageDialog(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, isSelected() ? org.jdownloader.extensions.extraction.translate.T.T.set_autoextract_true() : org.jdownloader.extensions.extraction.translate.T.T.set_autoextract_false());
        }
    }

}
