package org.jdownloader.gui.views.components.packagetable.actions;

import java.awt.event.ActionEvent;

import org.appwork.swing.exttable.SearchDialog;
import org.appwork.utils.locale._AWU;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.event.GUIEventSender;
import org.jdownloader.gui.toolbar.action.AbstractToolBarAction;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable;
import org.jdownloader.gui.views.downloads.DownloadsView;
import org.jdownloader.gui.views.downloads.table.DownloadsTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberView;

import jd.controlling.packagecontroller.AbstractNode;
import jd.gui.swing.jdgui.MainTabbedPane;
import jd.gui.swing.jdgui.interfaces.View;

public class SearchToolbarAction extends AbstractToolBarAction {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    // TODO: currently search only processes visible entries, but filtered/collapsed packages are not searched
    public SearchToolbarAction() {
        setIconKey(IconKey.ICON_SEARCH);
        setName(_AWU.T.EXTTABLE_SEARCH_DIALOG_TITLE());
        GUIEventSender.getInstance().addListener(this, true);
        onGuiMainTabSwitch(null, MainTabbedPane.getInstance().getSelectedView());
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (!isEnabled()) {
            return;
        }
        final View view = MainTabbedPane.getInstance().getSelectedView();
        final PackageControllerTable<?, ?> table;
        if (view instanceof DownloadsView) {
            table = DownloadsTable.getInstance();
        } else if (view instanceof LinkGrabberView) {
            table = LinkGrabberTable.getInstance();
        } else {
            return;
        }
        new SearchDialog(0, table) {
            private static final long serialVersionUID = 2652101312418765845L;

            @Override
            public void actionPerformed(final ActionEvent e) {
                final String ret = getReturnID();
                if (ret == null) {
                    return;
                }
                final int[] sel = table.getSelectedRows();
                int startRow = -1;
                if (sel != null && sel.length > 0) {
                    startRow = sel[sel.length - 1];
                }
                final AbstractNode found = table.getModel().searchNextObject(startRow + 1, ret, isCaseSensitive(), isRegex());
                if (found == null) {
                    /* No match: keep the current selection instead of clearing it via setSelectedObject(null) */
                    return;
                }
                table.getModel().setSelectedObject(found);
                table.scrollToSelection(-1);
            }
        };
    }

    @Override
    public void onGuiMainTabSwitch(View oldView, final View newView) {
        setEnabled(newView instanceof DownloadsView || newView instanceof LinkGrabberView);
    }

    @Override
    protected String createTooltip() {
        return _AWU.T.EXTTABLE_SEARCH_DIALOG_TITLE();
    }
}