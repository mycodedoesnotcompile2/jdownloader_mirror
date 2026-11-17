package org.jdownloader.gui.views.downloads.columns;

import java.awt.event.FocusEvent;
import java.awt.event.MouseEvent;
import java.io.File;

import javax.swing.JPopupMenu;

import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.LinkTreeUtils;
import org.jdownloader.gui.views.downloads.action.SetDownloadFolderInDownloadTableAction;

public class DownloadFolderColumn extends ExtTextColumn<AbstractNode> {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    public JPopupMenu createHeaderPopup() {

        return FileColumn.createColumnPopup(this, getMinWidth() == getMaxWidth() && getMaxWidth() > 0);

    }

    public DownloadFolderColumn() {
        super(_GUI.T.LinkGrabberTableModel_initColumns_folder());
        setClickcount(0);
    }

    @Override
    protected String getTooltipText(final AbstractNode obj) {
        return LinkTreeUtils.createDownloadDirectoryTooltip(obj, super.getTooltipText(obj));
    }

    @Override
    public void focusGained(final FocusEvent e) {
    }

    @Override
    public boolean isEditable(AbstractNode obj) {
        return false;
    }

    @Override
    public boolean onRenameClick(MouseEvent e, AbstractNode clicked) {
        // JDGui.help(_GUI.T.literall_usage_tipp(), _GUI.T.DownloadFolderColumn_onSingleClick_object_(), new AbstractIcon(IconKey.ICON_smart", 48));

        new SetDownloadFolderInDownloadTableAction(new SelectionInfo<FilePackage, DownloadLink>(clicked)).actionPerformed(null);
        return true;
    }

    @Override
    public boolean onDoubleClick(MouseEvent e, AbstractNode value) {
        /* The file system access happens in a background thread, not in the EDT */
        return LinkTreeUtils.openDownloadDirectoryAsync(value);
    }

    @Override
    public boolean onSingleClick(MouseEvent e, AbstractNode obj) {

        return false;
    }

    @Override
    public boolean isSortable(AbstractNode obj) {
        return true;
    }

    @Override
    public boolean isEnabled(final AbstractNode obj) {
        if (obj instanceof AbstractPackageNode) {
            return ((AbstractPackageNode) obj).getView().isEnabled();
        }
        return obj.isEnabled();
    }

    @Override
    public String getStringValue(AbstractNode value) {
        File ret = LinkTreeUtils.getDownloadDirectory(value);
        if (ret != null) {
            return ret.toString();
        }
        return null;
    }

}
