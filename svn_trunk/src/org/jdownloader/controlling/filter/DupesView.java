package org.jdownloader.controlling.filter;

import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog.BooleanStatusFilter.Matchtype;
import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog.DownloadListDupeFilter;

import org.jdownloader.gui.IconKey;
import org.jdownloader.translate._JDT;

public class DupesView extends LinkgrabberFilterRule {
    public static final String ID = "DupesView";

    public DupesView() {

    }

    public LinkgrabberFilterRule init() {
        setDownloadListDupeFilter(new DownloadListDupeFilter(Matchtype.IS_TRUE, true));
        setName(_JDT.T.LinkFilterSettings_DefaultFilterList_dupes());
        setIconKey(IconKey.ICON_COPY);
        setEnabled(true);
        setAccept(true);
        setId(ID);
        setStaticRule(true);
        return this;
    }
}
