package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.swing.TransferHandler;

import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.controlling.contextmenu.DelegatingReplacerTag;
import org.jdownloader.controlling.contextmenu.ReplacerTag;
import org.jdownloader.controlling.contextmenu.ReplacerTagDefinition;
import org.jdownloader.controlling.contextmenu.ReplacerTagProvider;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.SelectionInfo.PackageView;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.components.packagetable.dragdrop.PackageControllerTableTransferHandler;
import org.jdownloader.gui.views.downloads.action.replacer.DownloadReplacerTag;
import org.jdownloader.gui.views.downloads.table.DownloadsTable;
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTable;
import org.jdownloader.translate._JDT;

import jd.controlling.ClipboardMonitoring;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.gui.swing.jdgui.MainTabbedPane;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

public class CopyGenericContextAction extends CustomizableTableContextAppAction implements ActionContext, ReplacerTagProvider {
    /*
     * The tags that are offered in the menu editor for the pattern of packages and for the pattern of links. The order is the order in the
     * menu editor.
     */
    private static final List<ReplacerTag> PACKAGE_TAGS = new ArrayList<ReplacerTag>(Arrays.asList(DownloadReplacerTag.TYPE, DownloadReplacerTag.PACKAGE_NAME, DownloadReplacerTag.PATH, DownloadReplacerTag.COMMENT, DownloadReplacerTag.PACKAGE_SIZE, DownloadReplacerTag.PACKAGE_STATUS, DownloadReplacerTag.PACKAGE_ENABLED, DownloadReplacerTag.PACKAGE_PRIORITY, DownloadReplacerTag.DATE, DownloadReplacerTag.PACKAGE_DATE, DownloadReplacerTag.NEWLINE, DownloadReplacerTag.TAB));
    private static final List<ReplacerTag> LINK_TAGS    = new ArrayList<ReplacerTag>(Arrays.asList(DownloadReplacerTag.TYPE, DownloadReplacerTag.FILE_NAME, DownloadReplacerTag.NAME_WITHOUT_EXTENSION, DownloadReplacerTag.EXTENSION, DownloadReplacerTag.PACKAGE_NAME, DownloadReplacerTag.PATH, DownloadReplacerTag.HOST, DownloadReplacerTag.COMMENT, DownloadReplacerTag.AVAILABILITY, DownloadReplacerTag.STATUS, DownloadReplacerTag.ENABLED, DownloadReplacerTag.PRIORITY, DownloadReplacerTag.HASH, DownloadReplacerTag.FILESIZE, DownloadReplacerTag.URL, DownloadReplacerTag.ARCHIVE_PASSWORD, DownloadReplacerTag.PROPERTY, DownloadReplacerTag.DATE, DownloadReplacerTag.MODIFIED_DATE, DownloadReplacerTag.PACKAGE_COMMENT, DownloadReplacerTag.PACKAGE_SIZE, DownloadReplacerTag.PACKAGE_STATUS, DownloadReplacerTag.PACKAGE_ENABLED, DownloadReplacerTag.PACKAGE_PRIORITY, DownloadReplacerTag.PACKAGE_DATE, DownloadReplacerTag.NEWLINE, DownloadReplacerTag.TAB));
    /*
     * The tags that are applied. In addition to the offered ones the legacy tags {name} and {filesize} (and {packagecomment}) work in the
     * pattern of packages, {name} in the pattern of links. Tags that did not exist before (status, enabled, priority, modifiedDate) have no
     * legacy variant for packages, packages use the tags of the package.
     */
    private static final List<ReplacerTag> PACKAGE_PATTERN_TAGS = new ArrayList<ReplacerTag>(PACKAGE_TAGS);
    private static final List<ReplacerTag> LINK_PATTERN_TAGS    = new ArrayList<ReplacerTag>(LINK_TAGS);
    static {
        PACKAGE_PATTERN_TAGS.add(DownloadReplacerTag.NAME);
        PACKAGE_PATTERN_TAGS.add(DownloadReplacerTag.FILESIZE);
        PACKAGE_PATTERN_TAGS.add(DownloadReplacerTag.PACKAGE_COMMENT);
        LINK_PATTERN_TAGS.add(DownloadReplacerTag.NAME);
    }

    public CopyGenericContextAction() {
        super(true, true);
        setIconKey(IconKey.ICON_COPY);
        setName(_GUI.T.CopyGenericContextAction());
        setAccelerator(KeyEvent.VK_C);
    }

    public static String getTranslationForPatternPackages() {
        return _JDT.T.CopyGenericContextAction_getTranslationForPatternPackages_v3();
    }

    public static String getTranslationForPatternLinks() {
        return _JDT.T.CopyGenericContextAction_getTranslationForPatternLinks_v3();
    }

    public static String getTranslationForSmartSelection() {
        return _JDT.T.CopyGenericContextAction_getTranslationForSmartSelection();
    }

    @Override
    public List<ReplacerTag> getReplacerTags(final String customizerKey) {
        if ("patternPackages".equalsIgnoreCase(customizerKey)) {
            return PACKAGE_TAGS;
        } else if ("patternLinks".equalsIgnoreCase(customizerKey)) {
            return LINK_TAGS;
        } else {
            return null;
        }
    }

    private String patternPackages;

    @Customizer(link = "#getTranslationForPatternPackages")
    public String getPatternPackages() {
        return patternPackages;
    }

    public void setPatternPackages(String copyPattern) {
        this.patternPackages = copyPattern;
        updateTooltip();
    }

    /**
     * Builds the tooltip from the patterns that are actually set. Empty or missing patterns are left out.
     */
    private void updateTooltip() {
        final StringBuilder sb = new StringBuilder();
        for (final String pattern : new String[] { patternPackages, patternLinks }) {
            if (StringUtils.isNotEmpty(pattern)) {
                if (sb.length() > 0) {
                    sb.append(" - ");
                }
                sb.append(pattern);
            }
        }
        setTooltipText(_GUI.T.CopyGenericContextAction_tt(sb.toString()));
    }

    private String patternLinks;

    @Customizer(link = "#getTranslationForPatternLinks")
    public String getPatternLinks() {
        return patternLinks;
    }

    public void setPatternLinks(String patternLinks) {
        this.patternLinks = patternLinks;
        updateTooltip();
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
        patternLinks = "{type};{name};{url}";
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
                final String string = fromSelectionInfo(selectionInfo);
                final TransferHandler transferHandler = table.getTransferHandler();
                if (transferHandler instanceof PackageControllerTableTransferHandler) {
                    ((PackageControllerTableTransferHandler) transferHandler).setTransferableStringContent(string);
                    transferHandler.getCopyAction().actionPerformed(new ActionEvent(table, ActionEvent.ACTION_FIRST, "copy"));
                } else {
                    ClipboardMonitoring.getINSTANCE().setCurrentContent(string);
                }
            }

            @Override
            public boolean isCancelled() {
                return false;
            }
        };
    }

    public <ParentType extends AbstractPackageNode<ChildrenType, ParentType>, ChildrenType extends AbstractPackageChildrenNode<ParentType>> String fromSelectionInfo(SelectionInfo<ParentType, ChildrenType> selectionInfo) {
        final StringBuilder sb = new StringBuilder();
        /* Only needed if the link pattern contains the tag. Resolved once for the whole selection because it is expensive (file IO). */
        final boolean archivePasswordNeeded = StringUtils.contains(getPatternLinks(), "{archive.password");
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
            Map<AbstractNode, String> archivePasswords = null;
            if (archivePasswordNeeded) {
                final List<AbstractNode> links = new ArrayList<AbstractNode>();
                for (final PackageView<ParentType, ChildrenType> pv : selectionInfo.getPackageViews()) {
                    links.addAll(pv.getChildren());
                }
                archivePasswords = DownloadReplacerTag.resolveArchivePasswords(links);
            }
            final List<ReplacerTag> linkTags = createLinkTags(contentPermission, archivePasswords);
            for (final PackageView<ParentType, ChildrenType> pv : selectionInfo.getPackageViews()) {
                final ParentType pkg = pv.getPackage();
                add(sb, pkg, linkTags);
                final List<ChildrenType> childs = pv.getChildren();
                for (final ChildrenType c : childs) {
                    add(sb, c, linkTags);
                }
            }
        } else {
            final List<AbstractNode> selection = selectionInfo.getRawSelection();
            final boolean contentPermission = selection.size() == 1 && selection.get(0) instanceof AbstractPackageChildrenNode;
            final Map<AbstractNode, String> archivePasswords = archivePasswordNeeded ? DownloadReplacerTag.resolveArchivePasswords(selection) : null;
            final List<ReplacerTag> linkTags = createLinkTags(contentPermission, archivePasswords);
            for (final AbstractNode pv : selection) {
                add(sb, pv, linkTags);
            }
        }
        return sb.toString();
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (MainTabbedPane.getInstance().isDownloadView()) {
            DownloadsTable.getInstance().getSelectionInfo(getCallback(DownloadsTable.getInstance()), SelectionType.SELECTED);
        } else if (MainTabbedPane.getInstance().isLinkgrabberView()) {
            LinkGrabberTable.getInstance().getSelectionInfo(getCallback(LinkGrabberTable.getInstance()), SelectionType.SELECTED);
        }
    }

    public void add(StringBuilder sb, AbstractNode pv, final boolean contentPermission) {
        add(sb, pv, createLinkTags(contentPermission, null));
    }

    /**
     * The tags for the links of one copy action. Infos about the selection are not part of the links, so the tags that need them are
     * replaced by tags that know them. Everything else is passed on to the tags of the enum.
     *
     * @param singleLink
     *            true if the selection consists of exactly one link
     * @param archivePasswords
     *            The archive passwords of the selection, resolved up front, or null
     */
    private List<ReplacerTag> createLinkTags(final boolean singleLink, final Map<AbstractNode, String> archivePasswords) {
        final List<ReplacerTag> ret = new ArrayList<ReplacerTag>(LINK_PATTERN_TAGS);
        for (int i = 0; i < ret.size(); i++) {
            if (ret.get(i) == DownloadReplacerTag.URL && singleLink) {
                /* The content URL of a single link is not filtered */
                ret.set(i, new DelegatingReplacerTag(DownloadReplacerTag.URL) {
                    @Override
                    public String apply(final String replace, final Object object) {
                        if ("url.content".equals(replace) && object instanceof AbstractNode && (object instanceof DownloadLink || object instanceof CrawledLink)) {
                            return DownloadReplacerTag.getContentUrlOfSingleLink((AbstractNode) object);
                        }
                        return super.apply(replace, object);
                    }
                });
            } else if (ret.get(i) == DownloadReplacerTag.ARCHIVE_PASSWORD && archivePasswords != null) {
                /* Passwords of all links have been resolved at once */
                ret.set(i, new DelegatingReplacerTag(DownloadReplacerTag.ARCHIVE_PASSWORD) {
                    @Override
                    public String apply(final String replace, final Object object) {
                        if (!"archive.password".equals(replace) || !(object instanceof DownloadLink || object instanceof CrawledLink)) {
                            return super.apply(replace, object);
                        }
                        final String password = archivePasswords.get(object);
                        return password != null ? password : "";
                    }
                });
            }
        }
        return ret;
    }

    private void add(StringBuilder sb, AbstractNode pv, final List<ReplacerTag> linkTags) {
        final List<ReplacerTag> tags;
        final String pattern;
        if (pv instanceof FilePackage || pv instanceof CrawledPackage) {
            tags = PACKAGE_PATTERN_TAGS;
            pattern = getPatternPackages();
        } else if (pv instanceof DownloadLink || pv instanceof CrawledLink) {
            tags = linkTags;
            pattern = getPatternLinks();
        } else {
            return;
        }
        final String line = ReplacerTagDefinition.replace(tags, pattern, pv);
        if (StringUtils.isNotEmpty(line)) {
            if (sb.length() > 0) {
                sb.append(CrossSystem.getNewLine());
            }
            sb.append(line);
        }
    }
}
