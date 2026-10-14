package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.swing.TransferHandler;

import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.controlling.contextmenu.Replacer;
import org.jdownloader.controlling.contextmenu.ReplacerProvider;
import org.jdownloader.controlling.contextmenu.Replacers;
import org.jdownloader.controlling.contextmenu.Tag;
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

public class CopyGenericContextAction extends CustomizableTableContextAppAction implements ActionContext, ReplacerProvider {
    /*
     * The tags that are offered in the menu editor for the pattern of packages and for the pattern of links. The order is the order in the
     * menu editor.
     */
    private static final List<Replacer> PACKAGE_TAGS = new ArrayList<Replacer>(Arrays.asList(DownloadReplacerTag.TYPE, DownloadReplacerTag.PACKAGE_NAME, DownloadReplacerTag.PATH, DownloadReplacerTag.COMMENT, DownloadReplacerTag.PACKAGE_SIZE, DownloadReplacerTag.PACKAGE_STATUS, DownloadReplacerTag.PACKAGE_ENABLED, DownloadReplacerTag.PACKAGE_PRIORITY, DownloadReplacerTag.DATE, DownloadReplacerTag.PACKAGE_DATE, DownloadReplacerTag.NEWLINE, DownloadReplacerTag.TAB));
    private static final List<Replacer> LINK_TAGS    = new ArrayList<Replacer>(Arrays.asList(DownloadReplacerTag.TYPE, DownloadReplacerTag.FILE_NAME, DownloadReplacerTag.NAME_WITHOUT_EXTENSION, DownloadReplacerTag.EXTENSION, DownloadReplacerTag.PACKAGE_NAME, DownloadReplacerTag.PATH, DownloadReplacerTag.HOST, DownloadReplacerTag.COMMENT, DownloadReplacerTag.AVAILABILITY, DownloadReplacerTag.STATUS, DownloadReplacerTag.ENABLED, DownloadReplacerTag.PRIORITY, DownloadReplacerTag.HASH, DownloadReplacerTag.FILESIZE, DownloadReplacerTag.URL, DownloadReplacerTag.ARCHIVE_PASSWORD, DownloadReplacerTag.PROPERTY, DownloadReplacerTag.DATE, DownloadReplacerTag.MODIFIED_DATE, DownloadReplacerTag.PACKAGE_COMMENT, DownloadReplacerTag.PACKAGE_SIZE, DownloadReplacerTag.PACKAGE_STATUS, DownloadReplacerTag.PACKAGE_ENABLED, DownloadReplacerTag.PACKAGE_PRIORITY, DownloadReplacerTag.PACKAGE_DATE, DownloadReplacerTag.NEWLINE, DownloadReplacerTag.TAB));
    /*
     * The tags that are applied. In addition to the offered ones the legacy tags {name} and {filesize} (and {packagecomment}) work in the
     * pattern of packages, {name} in the pattern of links. Tags that did not exist before (status, enabled, priority, modifiedDate) have no
     * legacy variant for packages, packages use the tags of the package.
     */
    private static final List<Replacer> PACKAGE_PATTERN_TAGS = new ArrayList<Replacer>(PACKAGE_TAGS);
    private static final List<Replacer> LINK_PATTERN_TAGS    = new ArrayList<Replacer>(LINK_TAGS);
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
    public List<Replacer> getReplacers(final String customizerKey) {
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
            final List<Replacer> linkTags = createLinkReplacers(contentPermission, archivePasswords);
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
            final List<Replacer> linkTags = createLinkReplacers(contentPermission, archivePasswords);
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
        add(sb, pv, createLinkReplacers(contentPermission, null));
    }

    /**
     * Replaces a single tag by a value that depends on the selection. Infos about the selection are not part of the links, so the
     * replacers of the enum cannot know them. It is applied before the replacers of the enum, which then do not see the tag anymore.
     * Only links are handled, other objects and other tags stay unchanged.
     */
    private abstract static class SelectionReplacer implements Replacer {
        private final String key;

        private SelectionReplacer(final String key) {
            this.key = key;
        }

        @Override
        public List<Tag> getTags() {
            /* The tag is offered by the replacer of the enum */
            return Collections.emptyList();
        }

        @Override
        public String replace(final String input, final Object object) {
            if (input == null || !(object instanceof DownloadLink || object instanceof CrawledLink)) {
                return input;
            }
            return Replacers.replaceTags(input, new Replacers.TagResolver() {
                @Override
                public String resolve(final String tag) {
                    return key.equals(tag) ? getValue((AbstractNode) object) : null;
                }
            });
        }

        /**
         * @return The value of the tag for this link. An empty string if there is no data.
         */
        protected abstract String getValue(AbstractNode link);
    }

    /**
     * The replacers for the links of one copy action: The replacers for the infos about the selection, followed by the replacers of the
     * enum.
     *
     * @param singleLink
     *            true if the selection consists of exactly one link
     * @param archivePasswords
     *            The archive passwords of the selection, resolved up front, or null
     */
    private List<Replacer> createLinkReplacers(final boolean singleLink, final Map<AbstractNode, String> archivePasswords) {
        final List<Replacer> ret = new ArrayList<Replacer>();
        if (singleLink) {
            /* The content URL of a single link is not filtered */
            ret.add(new SelectionReplacer("url.content") {
                @Override
                protected String getValue(final AbstractNode link) {
                    return DownloadReplacerTag.getContentUrlOfSingleLink(link);
                }
            });
        }
        if (archivePasswords != null) {
            /*
             * Look up the password in the map that has been created once for the whole selection. An empty string means that there is no
             * data (link is in no archive or in several).
             */
            ret.add(new SelectionReplacer("archive.password") {
                @Override
                protected String getValue(final AbstractNode link) {
                    final String password = archivePasswords.get(link);
                    return password != null ? password : "";
                }
            });
        }
        ret.addAll(LINK_PATTERN_TAGS);
        return ret;
    }

    private void add(StringBuilder sb, AbstractNode pv, final List<Replacer> linkTags) {
        final List<Replacer> tags;
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
        final String line = Replacers.replace(tags, pattern, pv);
        if (StringUtils.isNotEmpty(line)) {
            if (sb.length() > 0) {
                sb.append(CrossSystem.getNewLine());
            }
            sb.append(line);
        }
    }
}
