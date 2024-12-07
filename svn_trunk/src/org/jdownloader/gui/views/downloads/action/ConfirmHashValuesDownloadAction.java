package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import jd.controlling.TaskQueue;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.download.HashInfo;

import org.appwork.swing.exttable.ExtTableEvent;
import org.appwork.swing.exttable.ExtTableListener;
import org.appwork.swing.exttable.ExtTableModelEventWrapper;
import org.appwork.swing.exttable.ExtTableModelListener;
import org.appwork.utils.event.queue.QueueAction;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.downloads.table.DownloadsTable;
import org.jdownloader.gui.views.linkgrabber.bottombar.IncludedSelectionSetup;

public class ConfirmHashValuesDownloadAction extends CustomizableTableContextAppAction<FilePackage, DownloadLink> implements ActionContext, ExtTableListener, ExtTableModelListener {
    private IncludedSelectionSetup includedSelection;

    public ConfirmHashValuesDownloadAction() {
        super(true, true);
        setIconKey(IconKey.ICON_HASHSUM);
        setName(_GUI.T.ConfirmHashValuesAction());
        addContextSetup(includedSelection = new IncludedSelectionSetup(DownloadsTable.getInstance(), this, this));
    }

    @Override
    protected void getSelection(final SelectionInfoCallback<FilePackage, DownloadLink> callback, final SelectionType selectionType) {
        DownloadsTable.getInstance().getSelectionInfo(callback, selectionType);
    }

    @Override
    protected SelectionType getSelectionType() {
        return includedSelection.getSelectionType();
    }

    @Override
    protected void onActionPerformed(ActionEvent e, SelectionType selectionType, SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        final List<DownloadLink> links;
        switch (selectionType) {
        case NONE:
            return;
        case SELECTED:
            links = selectionInfo.getChildren();
            break;
        case UNSELECTED:
            if (selectionInfo.getUnselectedChildren() != null) {
                links = selectionInfo.getUnselectedChildren();
            } else {
                links = null;
            }
            break;
        default:
            links = selectionInfo.getChildren();
            break;
        }
        if (links == null || links.size() == 0) {
            return;
        }
        TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                final HashMap<String, List<DownloadLink>> map = new HashMap<String, List<DownloadLink>>();
                for (DownloadLink cl : links) {
                    List<DownloadLink> list = map.get(cl.getName());
                    if (list == null) {
                        list = new ArrayList<DownloadLink>();
                        map.put(cl.getName(), list);
                    }
                    list.add(cl);
                }
                main: for (final Entry<String, List<DownloadLink>> se : map.entrySet()) {
                    final List<DownloadLink> list = se.getValue();
                    final HashMap<HashInfo.TYPE, HashInfo> knownHashInfos = new HashMap<HashInfo.TYPE, HashInfo>();
                    for (final DownloadLink cl : list) {
                        final HashInfo hashInfo = cl.getDownloadLink().getHashInfo();
                        if (hashInfo != null) {
                            final HashInfo existing = knownHashInfos.get(hashInfo.getType());
                            if (existing == null) {
                                knownHashInfos.put(hashInfo.getType(), hashInfo);
                            } else if (!existing.equals(hashInfo)) {
                                continue main;
                            }
                        }
                    }
                    if (knownHashInfos.size() > 0) {
                        for (HashInfo.TYPE type : HashInfo.TYPE.values()) {
                            final HashInfo existing = knownHashInfos.get(type);
                            if (existing != null) {
                                final HashInfo newHashInfo = HashInfo.newInstanceSafe(existing.getHash(), existing.getType(), existing.isTrustworthy(), true);
                                for (final DownloadLink cl : list) {
                                    cl.getDownloadLink().setHashInfo(newHashInfo);
                                }
                                break;
                            }
                        }
                    }
                }
                return null;
            }
        });
    }

    @Override
    public void onExtTableEvent(ExtTableEvent<?> event) {
    }

    @Override
    public void onExtTableModelEvent(ExtTableModelEventWrapper event) {
    }
}
