package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;
import java.util.List;
import java.util.Set;

import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.appwork.utils.swing.dialog.ProgressDialog;
import org.appwork.utils.swing.dialog.ProgressDialog.ProgressGetter;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.LinkTreeUtils;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.settings.UrlDisplayType;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.plugins.DownloadLink;

/**
 * "Open in Browser" context menu action.
 *
 * This class holds the complete functionality and is used directly by the DownloadTable. The LinkGrabber variant
 * ({@link org.jdownloader.gui.views.linkgrabber.contextmenu.OpenInBrowserAction}) simply derives from it with the CrawledPackage /
 * CrawledLink types, so the logic only exists once.
 */
public class OpenInBrowserAction<PackageType extends AbstractPackageNode<ChildrenType, PackageType>, ChildrenType extends AbstractPackageChildrenNode<PackageType>> extends CustomizableTableContextAppAction<PackageType, ChildrenType> implements ActionContext {
    private static final long   serialVersionUID = 7911375550836173693L;
    private final static String NAME             = _GUI.T.gui_table_contextmenu_browselink();

    /**
     * Selectable link type for the "Open in Browser" action.
     *
     * DEFAULT keeps the classic behaviour (open the default display URL, see {@link LinkTreeUtils#getURLs}). All other values map to a
     * {@link UrlDisplayType} and open exactly that URL type (see {@link LinkTreeUtils#getUrlByType}).
     */
    public static enum LinkType implements LabelInterface {
        DEFAULT(null),
        CUSTOM(UrlDisplayType.CUSTOM),
        REFERRER(UrlDisplayType.REFERRER),
        ORIGIN(UrlDisplayType.ORIGIN),
        CONTAINER(UrlDisplayType.CONTAINER),
        CONTENT(UrlDisplayType.CONTENT);

        private final UrlDisplayType urlDisplayType;

        private LinkType(final UrlDisplayType urlDisplayType) {
            this.urlDisplayType = urlDisplayType;
        }

        /**
         * @return the mapped {@link UrlDisplayType}, or null for {@link #DEFAULT}.
         */
        public UrlDisplayType getUrlDisplayType() {
            return urlDisplayType;
        }

        @Override
        public String getLabel() {
            if (urlDisplayType != null) {
                return urlDisplayType.getTranslatedName();
            }
            return _GUI.T.gui_table_contextmenu_browselink_urltype_default();
        }
    }

    public OpenInBrowserAction() {
        setIconKey(IconKey.ICON_BROWSE);
        setName(NAME);
    }

    private int      threshold         = 50;
    private int      delay             = 1000;
    private LinkType linkType          = LinkType.DEFAULT;
    private boolean  fallbackToDefault = true;

    public static String getTranslationLinkType() {
        return _GUI.T.gui_table_contextmenu_browselink_urltype();
    }

    @Order(10)
    @Customizer(link = "#getTranslationLinkType")
    public LinkType getLinkType() {
        return linkType;
    }

    public void setLinkType(LinkType linkType) {
        if (linkType == null) {
            this.linkType = LinkType.DEFAULT;
        } else {
            this.linkType = linkType;
        }
    }

    public static String getTranslationFallbackToDefault() {
        return _GUI.T.gui_table_contextmenu_browselink_urltype_fallback();
    }

    @Order(20)
    @Customizer(link = "#getTranslationFallbackToDefault")
    public boolean isFallbackToDefault() {
        return fallbackToDefault;
    }

    public void setFallbackToDefault(boolean fallbackToDefault) {
        this.fallbackToDefault = fallbackToDefault;
    }

    public static String getTranslationOpenDelay() {
        return _GUI.T.gui_table_contextmenu_browselink_delay();
    }

    @Order(30)
    @Customizer(link = "#getTranslationOpenDelay")
    public int getOpenDelay() {
        return delay;
    }

    public void setOpenDelay(int delay) {
        this.delay = Math.max(100, delay);
    }

    public static String getTranslationMaxOpenThreshold() {
        return _GUI.T.gui_table_contextmenu_browselink_maxurls();
    }

    @Order(40)
    @Customizer(link = "#getTranslationMaxOpenThreshold")
    public int getMaxOpenThreshold() {
        return threshold;
    }

    public void setMaxOpenThreshold(int threshold) {
        this.threshold = Math.max(-1, threshold);
    }

    @Override
    protected void onRequestUpdateSelection(Object requestor, SelectionType selectionType, SelectionInfo<PackageType, ChildrenType> selectionInfo) {
        final int threshold = getMaxOpenThreshold();
        if (!CrossSystem.isOpenBrowserSupported() || threshold == 0) {
            setEnabled(false);
            return;
        }
        if (SelectionInfo.isEmpty(selectionInfo)) {
            setEnabled(false);
            return;
        }
        if (threshold < 0) {
            setEnabled(true);
            return;
        }
        final List<ChildrenType> links = selectionInfo.getChildren();
        if (links.size() > threshold) {
            setEnabled(false);
            return;
        }
        for (final ChildrenType child : links) {
            final DownloadLink link;
            if (child instanceof DownloadLink) {
                link = (DownloadLink) child;
            } else if (child instanceof CrawledLink) {
                link = ((CrawledLink) child).getDownloadLink();
            } else {
                link = null;
            }
            if (link != null && link.getView().getDisplayUrl() != null) {
                setEnabled(true);
                return;
            }
        }
        setEnabled(false);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        if (!isEnabled()) {
            return;
        }
        super.actionPerformed(e);
    }

    @Override
    protected void onActionPerformed(ActionEvent e, SelectionType selectionType, final SelectionInfo<PackageType, ChildrenType> selectionInfo) {
        if (SelectionInfo.isEmpty(selectionInfo)) {
            return;
        } else if (!isEnabled()) {
            return;
        }
        new Thread("OpenInBrowserAction") {
            public void run() {
                final int delay = getOpenDelay();
                final LinkType linkType = getLinkType();
                final Set<String> urls;
                if (linkType == null || linkType == LinkType.DEFAULT) {
                    urls = LinkTreeUtils.getURLs(selectionInfo, true);
                } else {
                    urls = LinkTreeUtils.getURLs(selectionInfo, linkType.getUrlDisplayType(), isFallbackToDefault());
                }
                if (urls == null || urls.isEmpty()) {
                    /**
                     * Do not open progress dialog on empty list. Can be empty if e.g. users' selected url-type is not available for any
                     * item of the selection. <br>
                     */
                    return;
                }
                final ProgressDialog pg = new ProgressDialog(new ProgressGetter() {
                    private int total = -1;
                    private int current;

                    @Override
                    public void run() throws Exception {
                        total = urls.size();
                        current = 0;
                        for (String url : urls) {
                            CrossSystem.openURL(url);
                            current++;
                            if (current >= total) {
                                break;
                            }
                            Thread.sleep(delay);
                        }
                    }

                    @Override
                    public String getString() {
                        return current + "/" + total;
                    }

                    @Override
                    public int getProgress() {
                        if (total == 0) {
                            return -1;
                        }
                        final int ret = (current * 100) / total;
                        return ret;
                    }

                    @Override
                    public String getLabelString() {
                        return null;
                    }
                }, 0, _GUI.T.OpenInBrowserAction_actionPerformed_open_in_browser__multi(), _GUI.T.OpenInBrowserAction_actionPerformed_open_in_browser__multi_msg(urls.size()), NewTheme.I().getIcon(IconKey.ICON_BROWSE, 32), null, null);
                try {
                    Dialog.getInstance().showDialog(pg);
                } catch (DialogNoAnswerException e) {
                    e.printStackTrace();
                }
            }
        }.start();
    }
}
