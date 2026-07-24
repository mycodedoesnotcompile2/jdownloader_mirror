package org.jdownloader.extensions.extraction.gui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;

import javax.swing.Icon;

import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.jdownloader.extensions.extraction.ArchiveFile;
import org.jdownloader.extensions.extraction.DummyArchive;
import org.jdownloader.extensions.extraction.DummyArchiveFile;
import org.jdownloader.extensions.extraction.bindings.crawledlink.CrawledLinkArchiveFile;
import org.jdownloader.extensions.extraction.bindings.downloadlink.DownloadLinkArchiveFile;
import org.jdownloader.extensions.extraction.bindings.file.FileArchiveFile;
import org.jdownloader.extensions.extraction.translate.T;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.AbstractIcon;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.FilePackage;

public class DummyArchiveContentsTableModel extends ExtTableModel<DummyArchiveFile> {
    private ExtTextColumn<DummyArchiveFile>                 packageName;
    private ExtTextColumn<DummyArchiveFile>                 local;
    private ExtTextColumn<DummyArchiveFile>                 linkStatus;
    private ExtTextColumn<DummyArchiveFile>                 name;
    /**
     * All parts of a single DummyArchive belong to the same linkgrabber package. We resolve the package name once per archive from the
     * first CrawledLinkArchiveFile/DownloadLinkArchiveFile found in that archive's list, so that missing/placeholder entries (which carry
     * no link) can still display the correct name. Since this model can show the combined contents of several incomplete archives (e.g.
     * several packages confirmed at once), we keep the resolved name per DummyArchiveFile rather than a single model-wide name.
     */
    private final IdentityHashMap<DummyArchiveFile, String> packageNameByFile;

    public ExtTextColumn<DummyArchiveFile> getLocal() {
        return local;
    }

    public DummyArchiveContentsTableModel(DummyArchive da) {
        this(Collections.singletonList(da));
    }

    public DummyArchiveContentsTableModel(List<DummyArchive> archives) {
        super("DummyArchiveContentsTableModel");
        this.packageNameByFile = new IdentityHashMap<DummyArchiveFile, String>();
        final ArrayList<DummyArchiveFile> combined = new ArrayList<DummyArchiveFile>();
        for (final DummyArchive da : archives) {
            String pkgName = "";
            for (final DummyArchiveFile daf : da.getList()) {
                final ArchiveFile archiveFile = daf.getArchiveFile();
                if (archiveFile instanceof CrawledLinkArchiveFile) {
                    final CrawledLink link = ((CrawledLinkArchiveFile) archiveFile).getLinks().get(0);
                    final CrawledPackage pkg = link.getParentNode();
                    if (pkg != null) {
                        pkgName = pkg.getName();
                        break;
                    }
                } else if (archiveFile instanceof DownloadLinkArchiveFile) {
                    final DownloadLink link = ((DownloadLinkArchiveFile) archiveFile).getDownloadLinks().get(0);
                    final FilePackage pkg = link.getParentNode();
                    if (pkg != null) {
                        pkgName = pkg.getName();
                        break;
                    }
                }
            }
            for (final DummyArchiveFile daf : da.getList()) {
                packageNameByFile.put(daf, pkgName);
            }
            combined.addAll(da.getList());
        }
        _fireTableStructureChanged(combined, true);
    }

    @Override
    protected void initColumns() {
        addColumn(packageName = new ExtTextColumn<DummyArchiveFile>(T.T.packagename()) {
            // TODO: Maybe implement double-click = highlight package in linkgrabber and/or copy package name
            @Override
            public String getStringValue(DummyArchiveFile value) {
                return packageNameByFile.get(value);
            }
        });
        addColumn(name = new ExtTextColumn<DummyArchiveFile>(T.T.filename()) {
            @Override
            public String getStringValue(DummyArchiveFile value) {
                return value.getName();
            }

            @Override
            protected String getTooltipText(DummyArchiveFile value) {
                if (value.getArchiveFile() instanceof FileArchiveFile) {
                    if (((FileArchiveFile) value.getArchiveFile()).getFile().exists()) {
                        return T.T.file_exists();
                    } else {
                        return T.T.file_exists_not();
                    }
                } else {
                    if (value.getArchiveFile() == null) {
                        if (value.isMissing() || Boolean.TRUE.equals(value.isIncomplete())) {
                            return T.T.file_exists_not();
                        }
                        return T.T.unknown_tt();
                    } else {
                        if (value.isMissing() || Boolean.TRUE.equals(value.isIncomplete())) {
                            return T.T.offline_tt();
                        }
                        if (value.getOnlineStatus() == AvailableStatus.TRUE) {
                            return T.T.online_tt();
                        }
                        return T.T.unknown_tt();
                    }
                }
            }
        });
        addColumn(linkStatus = new ExtTextColumn<DummyArchiveFile>(T.T.exists()) {
            private Icon unknown;
            private Icon online;
            private Icon offline;
            {
                unknown = new AbstractIcon(IconKey.ICON_HELP, 16);
                online = new AbstractIcon(IconKey.ICON_TRUE, 16);
                offline = new AbstractIcon(IconKey.ICON_ERROR, 16);
            }

            @Override
            protected Icon getIcon(DummyArchiveFile value) {
                if (value.getOnlineStatus() == AvailableStatus.TRUE) {
                    return online;
                }
                if (value.isMissing() || Boolean.TRUE.equals(value.isIncomplete())) {
                    return offline;
                }
                return unknown;
            }

            @Override
            public String getStringValue(DummyArchiveFile value) {
                if (value.getOnlineStatus() == AvailableStatus.TRUE) {
                    return T.T.online();
                }
                if (value.isMissing() || Boolean.TRUE.equals(value.isIncomplete())) {
                    return T.T.offline();
                }
                return T.T.unknown();
            }

            @Override
            protected String getTooltipText(DummyArchiveFile value) {
                if (value.getOnlineStatus() == AvailableStatus.TRUE) {
                    return T.T.online_tt();
                }
                if (value.isMissing() || Boolean.TRUE.equals(value.isIncomplete())) {
                    return T.T.offline_tt();
                }
                return T.T.unknown_tt();
            }
        });
        addColumn(local = new ExtTextColumn<DummyArchiveFile>(T.T.local()) {
            @Override
            protected Icon getIcon(DummyArchiveFile value) {
                if (value.isLocalFileAvailable()) {
                    return new AbstractIcon(IconKey.ICON_TRUE, 16);
                }
                return new AbstractIcon(IconKey.ICON_FALSE, 16);
            }

            @Override
            public String getStringValue(DummyArchiveFile value) {
                if (value.isLocalFileAvailable()) { //
                    return T.T.downloadedok();
                }
                return T.T.downloadedbad();
            }

            @Override
            protected String getTooltipText(DummyArchiveFile value) {
                return getStringValue(value);
            }
        });
    }

    public ExtTextColumn<DummyArchiveFile> getPackageName() {
        return packageName;
    }

    public ExtTextColumn<DummyArchiveFile> getLinkStatus() {
        return linkStatus;
    }

    public ExtTextColumn<DummyArchiveFile> getName() {
        return name;
    }
}
