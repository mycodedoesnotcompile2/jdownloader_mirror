package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.regex.Pattern;

import javax.swing.TransferHandler;

import jd.controlling.ClipboardMonitoring;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.linkcrawler.CrawledPackageView;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.gui.swing.jdgui.MainTabbedPane;
import jd.parser.Regex;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.FilePackageView;
import jd.plugins.download.HashInfo;

import org.appwork.utils.Files;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.ArchiveValidator;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.SelectionInfo.PackageView;
import org.jdownloader.gui.views.components.packagetable.LinkTreeUtils;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.components.packagetable.dragdrop.PackageControllerTableTransferHandler;
import org.jdownloader.gui.views.downloads.table.DownloadsTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTable;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.UrlDisplayType;
import org.jdownloader.translate._JDT;

public class CopyGenericContextAction extends CustomizableTableContextAppAction implements ActionContext {
    private static final String PATTERN_NAME                  = "{name}";                      // depends on type
    private static final String PATTERN_PACKAGE_NAME          = "{packagename}";               // always package name
    private static final String PATTERN_NAME_NOEXT            = "{name_noext}";
    private static final String PATTERN_NEWLINE               = "{newline}";
    private static final String PATTERN_TAB                   = "{tab}";
    private static final String PATTERN_DOWNLOADLINK_PROPERTY = "{jd:prop:yourWishedProperty}";
    private static final String PATTERN_COMMENT               = "{comment}";
    private static final String PATTERN_HASH                  = "{hash}";
    private static final String PATTERN_FILESIZE_RAW          = "{filesize_raw}";
    private static final String PATTERN_FILESIZE_B            = "{filesize}";
    private static final String PATTERN_FILESIZE_KIB          = "{filesize_kib}";
    private static final String PATTERN_FILESIZE_MIB          = "{filesize_mib}";
    private static final String PATTERN_FILESIZE_GIB          = "{filesize_gib}";
    private static final String PATTERN_URL                   = "{url}";
    private static final String PATTERN_HOST                  = "{host}";
    private static final String PATTERN_URL_CONTAINER         = "{url.container}";
    private static final String PATTERN_URL_ORIGIN            = "{url.origin}";
    private static final String PATTERN_URL_CONTENT           = "{url.content}";
    private static final String PATTERN_URL_REFERRER          = "{url.referrer}";
    private static final String PATTERN_ARCHIVE_PASSWORD      = "{archive.password}";
    private static final String PATTERN_TYPE                  = "{type}";
    private static final String PATTERN_EXTENSION             = "{ext}";
    private static final String PATTERN_PATH                  = "{path}";

    public CopyGenericContextAction() {
        super(true, true);
        setIconKey(IconKey.ICON_COPY);
        setName(_GUI.T.CopyGenericContextAction());
        setAccelerator(KeyEvent.VK_C);
    }

    public static String getTranslationForPatternPackages() {
        final StringBuilder sb = new StringBuilder();
        sb.append("<html>");
        sb.append(_JDT.T.CopyGenericContextAction_getTranslationForPatternPackages_v3());
        sb.append("<br><ul>");
        for (final String pattern : new String[] { PATTERN_TYPE, PATTERN_PATH, PATTERN_COMMENT, PATTERN_FILESIZE_RAW, PATTERN_FILESIZE_B, PATTERN_FILESIZE_KIB, PATTERN_FILESIZE_MIB, PATTERN_FILESIZE_GIB, PATTERN_NEWLINE, PATTERN_TAB, PATTERN_NAME, PATTERN_PACKAGE_NAME }) {
            sb.append("<li>").append(pattern).append("</li>");
        }
        sb.append("</ul></html>");
        return sb.toString();
    }

    public static String getTranslationForPatternLinks() {
        final StringBuilder sb = new StringBuilder();
        sb.append("<html>");
        sb.append(_JDT.T.CopyGenericContextAction_getTranslationForPatternLinks_v3());
        sb.append("<br><ul>");
        for (final String pattern : new String[] { PATTERN_TYPE, PATTERN_PATH, PATTERN_COMMENT, PATTERN_FILESIZE_RAW, PATTERN_FILESIZE_B, PATTERN_FILESIZE_KIB, PATTERN_FILESIZE_MIB, PATTERN_FILESIZE_GIB, PATTERN_NEWLINE, PATTERN_TAB, PATTERN_NAME, PATTERN_PACKAGE_NAME, PATTERN_HOST, PATTERN_NAME_NOEXT, PATTERN_EXTENSION, PATTERN_HASH, PATTERN_URL, PATTERN_URL_CONTAINER, PATTERN_URL_CONTENT, PATTERN_URL_ORIGIN, PATTERN_URL_REFERRER, PATTERN_ARCHIVE_PASSWORD, PATTERN_DOWNLOADLINK_PROPERTY }) {
            sb.append("<li>").append(pattern).append("</li>");
        }
        sb.append("</ul></html>");
        return sb.toString();
    }

    public static String getTranslationForSmartSelection() {
        return _JDT.T.CopyGenericContextAction_getTranslationForSmartSelection();
    }

    private String patternPackages;

    @Customizer(link = "#getTranslationForPatternPackages")
    public String getPatternPackages() {
        return patternPackages;
    }

    public void setPatternPackages(String copyPattern) {
        this.patternPackages = copyPattern;
        setTooltipText(_GUI.T.CopyGenericContextAction_tt(getPatternPackages() + " - " + getPatternLinks()));
    }

    private String patternLinks;

    @Customizer(link = "#getTranslationForPatternLinks")
    public String getPatternLinks() {
        return patternLinks;
    }

    public void setPatternLinks(String patternLinks) {
        this.patternLinks = patternLinks;
        setTooltipText(_GUI.T.CopyGenericContextAction_tt(getPatternPackages() + " - " + getPatternLinks()));
    }

    private boolean smartSelection;

    @Customizer(link = "#getTranslationForSmartSelection")
    public boolean isSmartSelection() {
        return smartSelection;
    }

    public void setSmartSelection(boolean smartSelection) {
        this.smartSelection = smartSelection;
    }

    @Override
    protected void initTableContext(boolean empty, boolean selection) {
    }

    @Override
    public void initContextDefaults() {
        super.initContextDefaults();
        patternPackages = "";
        patternLinks = PATTERN_TYPE + ";" + PATTERN_NAME + ";" + PATTERN_URL;
        smartSelection = true;
    }

    @Override
    public void requestUpdate(Object requestor) {
        super.requestUpdate(requestor);
    }

    public <ParentType extends AbstractPackageNode<ChildrenType, ParentType>, ChildrenType extends AbstractPackageChildrenNode<ParentType>> SelectionInfoCallback<ParentType, ChildrenType> getCallback(final PackageControllerTable<ParentType, ChildrenType> table) {
        return new SelectionInfoCallback<ParentType, ChildrenType>() {

            @Override
            public void onSelectionInfo(SelectionInfo<ParentType, ChildrenType> selectionInfo) {
                final StringBuilder sb = new StringBuilder();
                if (isSmartSelection()) {
                    int children = 0;
                    for (final PackageView<ParentType, ChildrenType> pv : selectionInfo.getPackageViews()) {
                        final List<ChildrenType> childs = pv.getChildren();
                        children += childs.size();
                        if (children > 1) {
                            break;
                        }
                    }
                    final boolean contentPermission = children == 1;
                    for (final PackageView<ParentType, ChildrenType> pv : selectionInfo.getPackageViews()) {
                        final ParentType pkg = pv.getPackage();
                        add(sb, pkg, false);
                        final List<ChildrenType> childs = pv.getChildren();
                        for (final ChildrenType c : childs) {
                            add(sb, c, contentPermission);
                        }
                    }
                } else {
                    final List<AbstractNode> selection = selectionInfo.getRawSelection();
                    final boolean contentPermission = selection.size() == 1 && selection.get(0) instanceof AbstractPackageChildrenNode;
                    for (final AbstractNode pv : selection) {
                        add(sb, pv, contentPermission);
                    }
                }
                final TransferHandler transferHandler = table.getTransferHandler();
                if (transferHandler instanceof PackageControllerTableTransferHandler) {
                    ((PackageControllerTableTransferHandler) transferHandler).setTransferableStringContent(sb.toString());
                    transferHandler.getCopyAction().actionPerformed(new ActionEvent(table, ActionEvent.ACTION_FIRST, "copy"));
                } else {
                    ClipboardMonitoring.getINSTANCE().setCurrentContent(sb.toString());
                }
            }

            @Override
            public boolean isCancelled() {
                return false;
            }
        };
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (MainTabbedPane.getInstance().isDownloadView()) {
            DownloadsTable.getInstance().getSelectionInfo(getCallback(DownloadsTable.getInstance()), SelectionType.SELECTED);
        } else if (MainTabbedPane.getInstance().isLinkgrabberView()) {
            LinkGrabberTable.getInstance().getSelectionInfo(getCallback(LinkGrabberTable.getInstance()), SelectionType.SELECTED);
        }
    }

    private final String getUrlByType(final UrlDisplayType dt, final AbstractNode node) {
        final DownloadLink link;
        if (node instanceof DownloadLink) {
            link = (DownloadLink) node;
        } else if (node instanceof CrawledLink) {
            link = ((CrawledLink) node).getDownloadLink();
        } else {
            return null;
        }
        switch (dt) {
        case CUSTOM:
            return link.getCustomUrl();
        case REFERRER:
            return link.getReferrerUrl();
        case CONTAINER:
            return link.getContainerUrl();
        case ORIGIN:
            return link.getOriginUrl();
        case CONTENT:
            final String contentURL = link.getContentUrl();
            if (contentURL != null) {
                return contentURL;
            } else {
                return link.getPluginPatternMatcher();
            }
        default:
            return null;
        }
    }

    private final String formatFileSize(final long fileSize, SIZEUNIT sizeUnit) {
        return SIZEUNIT.formatValue(sizeUnit, fileSize);
    }

    private final String toUpperCase(final String input) {
        if (input != null) {
            return input.toUpperCase(Locale.ENGLISH);
        } else {
            return null;
        }
    }

    private final String replaceDate(String line) {
        while (true) {
            final String timeFormat[] = new Regex(line, "(\\{date_(.*?)\\})").getRow(0);
            if (timeFormat != null) {
                try {
                    final SimpleDateFormat dateFormat = new SimpleDateFormat(timeFormat[1], Locale.ENGLISH);
                    line = line.replace(timeFormat[0], dateFormat.format(new Date(System.currentTimeMillis())));
                } catch (final Throwable e) {
                    line = line.replace(timeFormat[0], "");
                }
            } else {
                return line;
            }
        }
    }

    private final String replaceArchiveInfos(AbstractNode pv, String line) {
        if (StringUtils.contains(line, PATTERN_ARCHIVE_PASSWORD)) {
            final List<Archive> archives = ArchiveValidator.getArchivesFromPackageChildren(Arrays.asList(new AbstractNode[] { pv }), 1);
            if (archives != null && archives.size() == 1) {
                line = line.replace(PATTERN_ARCHIVE_PASSWORD, nulltoString(archives.get(0).getFinalPassword()));
            } else {
                line = line.replace(PATTERN_ARCHIVE_PASSWORD, nulltoString(null));
            }
        }
        return line;
    }

    public void add(StringBuilder sb, AbstractNode pv, final boolean contentPermission) {
        String line = null;
        if (pv instanceof FilePackage) {
            line = getPatternPackages();
            line = replaceDate(line);
            final FilePackage pkg = (FilePackage) pv;
            final FilePackageView fpv = new FilePackageView(pkg);
            fpv.aggregate();
            line = line.replace(PATTERN_TYPE, "Package");
            line = line.replace(PATTERN_PATH, nulltoString(LinkTreeUtils.getDownloadDirectory(pkg)));
            line = line.replace(PATTERN_COMMENT, nulltoString(pkg.getComment()));
            final long fileSize = fpv.getSize();
            line = line.replace(PATTERN_FILESIZE_RAW, nulltoString(Long.toString(fileSize)));
            line = line.replace(PATTERN_FILESIZE_B, nulltoString(formatFileSize(fileSize, SIZEUNIT.B)));
            line = line.replace(PATTERN_FILESIZE_KIB, nulltoString(formatFileSize(fileSize, SIZEUNIT.KiB)));
            line = line.replace(PATTERN_FILESIZE_MIB, nulltoString(formatFileSize(fileSize, SIZEUNIT.MiB)));
            line = line.replace(PATTERN_FILESIZE_GIB, nulltoString(formatFileSize(fileSize, SIZEUNIT.GiB)));
            line = line.replace(PATTERN_NEWLINE, CrossSystem.getNewLine());
            line = line.replace(PATTERN_TAB, "\t");
            final String name = pkg.getName();
            line = line.replace(PATTERN_NAME, nulltoString(name));
            line = line.replace(PATTERN_PACKAGE_NAME, nulltoString(name));
        } else if (pv instanceof DownloadLink) {
            line = getPatternLinks();
            line = replaceDate(line);
            line = replaceArchiveInfos(pv, line);
            final DownloadLink link = (DownloadLink) pv;
            final FilePackage fp = link.getFilePackage();
            line = line.replace(PATTERN_TYPE, "Link");
            line = line.replace(PATTERN_HOST, nulltoString(link.getHost()));
            line = line.replace(PATTERN_PATH, nulltoString(LinkTreeUtils.getDownloadDirectory(link)));
            line = line.replace(PATTERN_COMMENT, nulltoString(link.getComment()));
            final long fileSize = link.getView().getBytesTotalEstimated();
            line = line.replace(PATTERN_FILESIZE_RAW, nulltoString(Long.toString(fileSize)));
            line = line.replace(PATTERN_FILESIZE_B, nulltoString(formatFileSize(fileSize, SIZEUNIT.B)));
            line = line.replace(PATTERN_FILESIZE_KIB, nulltoString(formatFileSize(fileSize, SIZEUNIT.KiB)));
            line = line.replace(PATTERN_FILESIZE_MIB, nulltoString(formatFileSize(fileSize, SIZEUNIT.MiB)));
            line = line.replace(PATTERN_FILESIZE_GIB, nulltoString(formatFileSize(fileSize, SIZEUNIT.GiB)));
            line = line.replace(PATTERN_NEWLINE, CrossSystem.getNewLine());
            line = line.replace(PATTERN_TAB, "\t");
            line = replaceDownloadLinkProperties(link, line);
            final String name = link.getView().getDisplayName();
            line = line.replace(PATTERN_NAME, nulltoString(name));
            line = line.replace(PATTERN_PACKAGE_NAME, nulltoString(fp.getName()));
            line = line.replace(PATTERN_NAME_NOEXT, nulltoString(Files.getFileNameWithoutExtension(name)));
            line = line.replace(PATTERN_EXTENSION, nulltoString(toUpperCase(Files.getExtension(name))));
            final HashInfo hashInfo = link.getHashInfo();
            for (final HashInfo.TYPE hashType : HashInfo.TYPE.values()) {
                final String hashString;
                if (hashInfo != null && hashInfo.getType() == hashType) {
                    hashString = hashInfo.getHash();
                } else {
                    hashString = null;
                }
                line = line.replace("{" + hashType.name().replace("-", "").toLowerCase(Locale.ENGLISH) + "}", nulltoString(hashString));
            }
            line = line.replace(PATTERN_HASH, nulltoString(hashInfo != null ? hashInfo.getHash() : null));
            line = line.replace(PATTERN_URL, nulltoString(link.getView().getDisplayUrl()));
            line = line.replace(PATTERN_URL_CONTAINER, nulltoString(LinkTreeUtils.getUrlByType(UrlDisplayType.CONTAINER, link)));
            if (contentPermission) {
                line = line.replace(PATTERN_URL_CONTENT, nulltoString(getUrlByType(UrlDisplayType.CONTENT, link)));
            } else {
                line = line.replace(PATTERN_URL_CONTENT, nulltoString(LinkTreeUtils.getUrlByType(UrlDisplayType.CONTENT, link)));
            }
            line = line.replace(PATTERN_URL_ORIGIN, nulltoString(LinkTreeUtils.getUrlByType(UrlDisplayType.ORIGIN, link)));
            line = line.replace(PATTERN_URL_REFERRER, nulltoString(LinkTreeUtils.getUrlByType(UrlDisplayType.REFERRER, link)));
        } else if (pv instanceof CrawledLink) {
            line = getPatternLinks();
            line = replaceDate(line);
            line = replaceArchiveInfos(pv, line);
            final CrawledLink link = (CrawledLink) pv;
            final CrawledPackage cp = link.getParentNode();
            line = line.replace(PATTERN_TYPE, "Link");
            line = line.replace(PATTERN_HOST, nulltoString(link.getHost()));
            line = line.replace(PATTERN_COMMENT, nulltoString(link.getComment()));
            line = line.replace(PATTERN_PATH, nulltoString(LinkTreeUtils.getDownloadDirectory(link)));
            final long fileSize = link.getSize();
            line = line.replace(PATTERN_FILESIZE_RAW, nulltoString(Long.toString(fileSize)));
            line = line.replace(PATTERN_FILESIZE_B, nulltoString(formatFileSize(fileSize, SIZEUNIT.B)));
            line = line.replace(PATTERN_FILESIZE_KIB, nulltoString(formatFileSize(fileSize, SIZEUNIT.KiB)));
            line = line.replace(PATTERN_FILESIZE_MIB, nulltoString(formatFileSize(fileSize, SIZEUNIT.MiB)));
            line = line.replace(PATTERN_FILESIZE_GIB, nulltoString(formatFileSize(fileSize, SIZEUNIT.GiB)));
            line = line.replace(PATTERN_NEWLINE, CrossSystem.getNewLine());
            line = line.replace(PATTERN_TAB, "\t");
            line = replaceDownloadLinkProperties(link.getDownloadLink(), line);
            final String name = link.getName();
            line = line.replace(PATTERN_NAME, nulltoString(name));
            line = line.replace(PATTERN_PACKAGE_NAME, nulltoString(cp.getName()));
            line = line.replace(PATTERN_NAME_NOEXT, nulltoString(Files.getFileNameWithoutExtension(name)));
            line = line.replace(PATTERN_EXTENSION, nulltoString(toUpperCase(Files.getExtension(name))));
            final HashInfo hashInfo = link.getDownloadLink().getHashInfo();
            for (final HashInfo.TYPE hashType : HashInfo.TYPE.values()) {
                final String hashString;
                if (hashInfo != null && hashInfo.getType() == hashType) {
                    hashString = hashInfo.getHash();
                } else {
                    hashString = null;
                }
                line = line.replace("{" + hashType.name().replace("-", "").toLowerCase(Locale.ENGLISH) + "}", nulltoString(hashString));
            }
            line = line.replace(PATTERN_HASH, nulltoString(hashInfo != null ? hashInfo.getHash() : null));
            line = line.replace(PATTERN_URL, nulltoString(link.getDownloadLink().getView().getDisplayUrl()));
            line = line.replace(PATTERN_URL_CONTAINER, nulltoString(LinkTreeUtils.getUrlByType(UrlDisplayType.CONTAINER, link)));
            if (contentPermission) {
                line = line.replace(PATTERN_URL_CONTENT, nulltoString(getUrlByType(UrlDisplayType.CONTENT, link)));
            } else {
                line = line.replace(PATTERN_URL_CONTENT, nulltoString(LinkTreeUtils.getUrlByType(UrlDisplayType.CONTENT, link)));
            }
            line = line.replace(PATTERN_URL_ORIGIN, nulltoString(LinkTreeUtils.getUrlByType(UrlDisplayType.ORIGIN, link)));
            line = line.replace(PATTERN_URL_REFERRER, nulltoString(LinkTreeUtils.getUrlByType(UrlDisplayType.REFERRER, link)));
        } else if (pv instanceof CrawledPackage) {
            line = getPatternPackages();
            line = replaceDate(line);
            final CrawledPackage pkg = (CrawledPackage) pv;
            final CrawledPackageView fpv = new CrawledPackageView(pkg).aggregate();
            line = line.replace(PATTERN_TYPE, "Package");
            line = line.replace(PATTERN_COMMENT, nulltoString(pkg.getComment()));
            line = line.replace(PATTERN_PATH, nulltoString(LinkTreeUtils.getDownloadDirectory(pkg)));
            final long fileSize = fpv.getFileSize();
            line = line.replace(PATTERN_FILESIZE_RAW, nulltoString(Long.toString(fileSize)));
            line = line.replace(PATTERN_FILESIZE_B, nulltoString(formatFileSize(fileSize, SIZEUNIT.B)));
            line = line.replace(PATTERN_FILESIZE_KIB, nulltoString(formatFileSize(fileSize, SIZEUNIT.KiB)));
            line = line.replace(PATTERN_FILESIZE_MIB, nulltoString(formatFileSize(fileSize, SIZEUNIT.MiB)));
            line = line.replace(PATTERN_FILESIZE_GIB, nulltoString(formatFileSize(fileSize, SIZEUNIT.GiB)));
            line = line.replace(PATTERN_NEWLINE, CrossSystem.getNewLine());
            line = line.replace(PATTERN_TAB, "\t");
            final String name = pkg.getName();
            line = line.replace(PATTERN_NAME, nulltoString(name));
            line = line.replace(PATTERN_PACKAGE_NAME, nulltoString(name));
        }
        if (StringUtils.isNotEmpty(line)) {
            if (sb.length() > 0) {
                sb.append(CrossSystem.getNewLine());
            }
            sb.append(line);
        }
    }

    private final static Pattern PROPERTIES_TAG_PATTERN = Pattern.compile("(?i)(\\{jd:prop:([^\\}]+)\\})");

    private final String replaceDownloadLinkProperties(final DownloadLink link, final String input) {
        String line = input;
        final String[][] propertiesToReplace = new Regex(input, PROPERTIES_TAG_PATTERN).getMatches();
        for (final String[] propertyInfo : propertiesToReplace) {
            final String completeStringToReplace = propertyInfo[0];
            final String property = propertyInfo[1];
            final String content = nulltoString(link.getProperty(property));
            line = line.replace(completeStringToReplace, content);
        }
        return line;
    }

    private final String nulltoString(final Object comment) {
        return StringUtils.valueOrEmpty(StringUtils.valueOfOrNull(comment));
    }

}
