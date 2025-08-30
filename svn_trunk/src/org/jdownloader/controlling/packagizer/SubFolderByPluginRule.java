package org.jdownloader.controlling.packagizer;

import jd.plugins.DownloadLink;

import org.jdownloader.controlling.filter.BooleanFilter;
import org.jdownloader.gui.IconKey;
import org.jdownloader.translate._JDT;

public class SubFolderByPluginRule extends PackagizerRule {
    public static final String ID = "SubFolderByPluginRule";

    public SubFolderByPluginRule() {
        super();
    }

    @Override
    public SubFolderByPluginRule init() {
        setMatchAlwaysFilter(new BooleanFilter(true));
        setDownloadDestination("<jd:" + DownloadLink.RELATIVE_DOWNLOAD_FOLDER_PATH + ">");
        setIconKey(IconKey.ICON_FOLDER);
        setName(_JDT.T.PackagizerSettings_folderbyplugin_rule_name2());
        setEnabled(true);
        setId(ID);
        setStaticRule(true);
        return this;
    }
}
