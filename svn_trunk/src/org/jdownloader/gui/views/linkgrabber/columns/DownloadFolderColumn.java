package org.jdownloader.gui.views.linkgrabber.columns;

import java.awt.event.FocusEvent;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.ArrayList;

import javax.swing.JPopupMenu;

import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcollector.LinknameCleaner;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.linkcrawler.CrawledPackage.TYPE;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageNode;

import org.appwork.swing.components.ExtButton;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.LinkTreeUtils;
import org.jdownloader.gui.views.downloads.columns.FileColumn;
import org.jdownloader.gui.views.linkgrabber.contextmenu.SetDownloadFolderInLinkgrabberAction;
import org.jdownloader.settings.staticreferences.CFG_LINKCOLLECTOR;
import org.jdownloader.translate._JDT;

public class DownloadFolderColumn extends ExtTextColumn<AbstractNode> {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private AbstractNode      editing;
    private ExtButton         open;

    public DownloadFolderColumn() {
        super(_GUI.T.LinkGrabberTableModel_initColumns_folder());
        setClickcount(0);
    }

    public JPopupMenu createHeaderPopup() {
        return FileColumn.createColumnPopup(this, getMinWidth() == getMaxWidth() && getMaxWidth() > 0);
    }

    @Override
    public void focusGained(final FocusEvent e) {
    }

    @Override
    protected String getTooltipText(AbstractNode obj) {
        final String ret = super.getTooltipText(obj);
        if (StringUtils.isNotEmpty(ret)) {
            final File raw = LinkTreeUtils.getRawDownloadDirectory(obj);
            String rawString = null;
            if (raw != null && (rawString = raw.getPath()).contains("<jd:")) {
                return "<html><pre>" + ret + "\r\n(" + rawString.replaceAll("<", "&lt;").replaceAll(">", "&gt;") + ")</html>";
            }
        }
        return ret;
    }

    @Override
    public boolean onRenameClick(MouseEvent e, AbstractNode obj) {
        SetDownloadFolderInLinkgrabberAction r = new SetDownloadFolderInLinkgrabberAction(new SelectionInfo<CrawledPackage, CrawledLink>(obj));
        r.requestUpdate(this);
        r.actionPerformed(null);
        return true;
    }

    @Override
    public boolean isEditable(AbstractNode obj) {
        return false;
    }

    @Override
    public boolean onDoubleClick(MouseEvent e, AbstractNode value) {
        if (CrossSystem.isOpenFileSupported() && value != null) {
            final File ret = LinkTreeUtils.getDownloadDirectory(value);
            if (ret != null && ret.exists() && ret.isDirectory()) {
                CrossSystem.openFile(ret);
            }
            return true;
        }
        return false;
    }

    @Override
    public boolean onSingleClick(MouseEvent e, AbstractNode obj) {
        // JDGui.help(_GUI.T.literall_usage_tipp(),
        // _GUI.T.DownloadFolderColumn_onSingleClick_object_(),
        // new AbstractIcon(IconKey.ICON_smart", 48));
        return false;
    }

    @Override
    public boolean isSortable(AbstractNode obj) {
        return true;
    }

    @Override
    protected void setStringValue(final String value, final AbstractNode object) {
        if (StringUtils.isEmpty(value) || object == null) {
            return;
        }
        final File oldPath = LinkTreeUtils.getDownloadDirectory(object);
        final File newPath = LinkTreeUtils.getDownloadDirectory(value, null, object);
        if (oldPath.equals(newPath)) {
            /* both paths are same, so nothing to do */
            return;
        }
        if (object instanceof CrawledPackage) {
            LinkCollector.getInstance().getQueue().add(new QueueAction<Object, RuntimeException>(org.appwork.utils.event.queue.Queue.QueuePriority.HIGH) {
                @Override
                protected Object run() {
                    ((CrawledPackage) object).setDownloadFolder(value);
                    return null;
                }
            });
            return;
        } else if (object instanceof CrawledLink) {
            final CrawledPackage p = ((CrawledLink) object).getParentNode();
            try {
                Dialog.getInstance().showConfirmDialog(Dialog.LOGIC_DONOTSHOW_BASED_ON_TITLE_ONLY | Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, _JDT.T.SetDownloadFolderAction_actionPerformed_(p.getName()), _JDT.T.SetDownloadFolderAction_msg(p.getName(), 1), null, _JDT.T.SetDownloadFolderAction_yes(), _JDT.T.SetDownloadFolderAction_no());
                LinkCollector.getInstance().getQueue().add(new QueueAction<Object, RuntimeException>(org.appwork.utils.event.queue.Queue.QueuePriority.HIGH) {
                    @Override
                    protected Object run() {
                        p.setDownloadFolder(value);
                        return null;
                    }
                });
                return;
            } catch (DialogClosedException e) {
                return;
            } catch (DialogCanceledException e) {
                /* user clicked no */
            }
            final CrawledPackage pkg = new CrawledPackage();
            pkg.setExpanded(CFG_LINKCOLLECTOR.CFG.isPackageAutoExpanded());
            if (TYPE.NORMAL != p.getType()) {
                final String packagename;
                if (object instanceof AbstractPackageNode) {
                    /* Package */
                    packagename = object.getName();
                } else {
                    /* File */
                    packagename = LinknameCleaner.derivePackagenameFromFilename(object.getName());
                }
                pkg.setName(packagename);
            } else {
                pkg.setName(p.getName());
            }
            pkg.setComment(p.getComment());
            pkg.setDownloadFolder(value);
            final java.util.List<CrawledLink> links = new ArrayList<CrawledLink>();
            links.add((CrawledLink) object);
            LinkCollector.getInstance().getQueue().add(new QueueAction<Object, RuntimeException>(org.appwork.utils.event.queue.Queue.QueuePriority.HIGH) {
                @Override
                protected Object run() {
                    LinkCollector.getInstance().moveOrAddAt(pkg, links, -1);
                    return null;
                }
            });
        }
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
