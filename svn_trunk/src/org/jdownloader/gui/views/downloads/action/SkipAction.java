package org.jdownloader.gui.views.downloads.action;

import java.awt.event.ActionEvent;
import java.util.List;

import javax.swing.Icon;

import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.controlling.contextmenu.ActionContext;
import org.jdownloader.controlling.contextmenu.CustomizableTableContextAppAction;
import org.jdownloader.controlling.contextmenu.Customizer;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.KeyObserver;
import org.jdownloader.gui.event.GUIEventSender;
import org.jdownloader.gui.event.GUIListener;
import org.jdownloader.gui.toolbar.UnskipLinksAction;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.SkipReason;

import jd.controlling.TaskQueue;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.controlling.packagecontroller.AbstractNode;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.WarnLevel;
import jd.gui.swing.jdgui.interfaces.View;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.download.DownloadInterface;

/**
 * Skips and unskips the selected links, like {@link org.jdownloader.gui.views.components.packagetable.context.EnabledAction} enables and
 * disables them: A checkbox shows whether the selection is skipped, a click toggles it. With a mixed selection the click makes all links
 * like the link that was clicked (Ctrl/Meta: the other way round). Skipping is only possible while the downloads are running (and not
 * paused), unskipping is possible in every state.
 */
public class SkipAction extends CustomizableTableContextAppAction<FilePackage, DownloadLink> implements GUIListener, ActionContext {
    private static final long serialVersionUID = 7107840091963427544L;

    /** The mode of the action: Always skip, always unskip, or decide by the selection (AUTO). Ctrl/Meta reverses SKIP and UNSKIP. */
    public static enum SkipActionMode implements LabelInterface {
        SKIP() {
            @Override
            public String getLabel() {
                return _GUI.T.ForceDownloadAction_SkipDownloadAction();
            }
        },
        UNSKIP() {
            @Override
            public String getLabel() {
                return _GUI.T.ForceDownloadAction_UnskipDownloadAction();
            }
        },
        AUTO();
        @Override
        public String getLabel() {
            return this.name();
        }
    }

    /** What a click does (skip) and how the action looks. */
    private static enum State {
        ALL_SKIPPED(false, getCheckBoxedIcon("skipped", true, true)),
        ALL_UNSKIPPED(true, getCheckBoxedIcon("skipped", false, true)),
        MIXED_SKIP(true, getCheckBoxedIcon("skipped", true, false)),
        MIXED_UNSKIP(false, getCheckBoxedIcon("skipped", false, false));
        private final Icon    icon;
        private final boolean skip;

        private State(final boolean skip, final Icon icon) {
            this.icon = icon;
            this.skip = skip;
        }
    }

    private final static String                               NAME_UNSKIP = _GUI.T.ForceDownloadAction_UnskipDownloadAction();
    private final static String                               NAME_SKIP   = _GUI.T.ForceDownloadAction_SkipDownloadAction();
    private volatile State                                    state       = State.MIXED_SKIP;
    /* The selection of the last update, to show the right state when the Ctrl/Meta key is pressed or released. */
    private volatile SelectionInfo<FilePackage, DownloadLink> selection   = null;
    private volatile boolean                                  metaCtrl;
    private SkipActionMode                                    mode        = SkipActionMode.AUTO;

    public SkipAction() {
        super();
        setSmallIcon(State.ALL_UNSKIPPED.icon);
        setName(NAME_SKIP);
        GUIEventSender.getInstance().addListener(this, true);
        metaCtrl = KeyObserver.getInstance().isMetaDown(true) || KeyObserver.getInstance().isControlDown(true);
    }

    public static String getTranslationForMode() {
        return _GUI.T.EnabledAction_EnabledAction_mode();
    }

    @Customizer(link = "#getTranslationForMode")
    public SkipActionMode getMode() {
        return mode;
    }

    public void setMode(final SkipActionMode mode) {
        if (mode == null) {
            this.mode = SkipActionMode.AUTO;
        } else {
            this.mode = mode;
        }
    }

    @Override
    public void onKeyModifier(final int parameter) {
        final boolean before = metaCtrl;
        metaCtrl = KeyObserver.getInstance().isControlDown(false) || KeyObserver.getInstance().isMetaDown(false);
        if (before != metaCtrl) {
            updateStateAndLabelAndIcon(selection);
        }
    }

    @Override
    public void onGuiMainTabSwitch(final View oldView, final View newView) {
    }

    @Override
    protected void onRequestUpdateSelection(final Object requestor, final SelectionType selectionType, final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        super.onRequestUpdateSelection(requestor, selectionType, selectionInfo);
        selection = selectionInfo;
        updateStateAndLabelAndIcon(selectionInfo);
    }

    /**
     * Sets the state, the name, the icon and whether the action is enabled: Unskipping is always possible, skipping only while the
     * downloads are running and not paused. The state can change with the Ctrl/Meta key, so this has to be done again then.
     */
    private void updateStateAndLabelAndIcon(final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        if (SelectionInfo.isEmpty(selectionInfo)) {
            setSmallIcon(State.ALL_UNSKIPPED.icon);
            setName(NAME_SKIP);
            return;
        }
        final State newState = getState(selectionInfo);
        state = newState;
        setSmallIcon(newState.icon);
        setName(newState.skip ? NAME_SKIP : NAME_UNSKIP);
        setEnabled(!newState.skip || DownloadWatchDog.getInstance().isRunningAndNotPaused());
    }

    private State getState(final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        switch (getMode()) {
        case SKIP:
            return metaCtrl ? State.ALL_SKIPPED : State.ALL_UNSKIPPED;
        case UNSKIP:
            return metaCtrl ? State.ALL_UNSKIPPED : State.ALL_SKIPPED;
        case AUTO:
        default:
            Boolean first = null;
            for (final DownloadLink link : selectionInfo.getChildren()) {
                if (first == null) {
                    first = link.isSkipped();
                } else if (link.isSkipped() != first) {
                    /* Mixed: The click makes all links like the link that was clicked (a package counts as not skipped). */
                    final AbstractNode context = selectionInfo.getRawContext();
                    final boolean contextSkipped = context instanceof DownloadLink && ((DownloadLink) context).isSkipped();
                    if (metaCtrl) {
                        return contextSkipped ? State.MIXED_UNSKIP : State.MIXED_SKIP;
                    } else {
                        return contextSkipped ? State.MIXED_SKIP : State.MIXED_UNSKIP;
                    }
                }
            }
            return first ? State.ALL_SKIPPED : State.ALL_UNSKIPPED;
        }
    }

    @Override
    protected void onActionPerformed(final ActionEvent e, final SelectionType selectionType, final SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
        if (!isEnabled()) {
            return;
        }
        final List<DownloadLink> children = selectionInfo.getChildren();
        if (!state.skip) {
            /* Like the action Unskip selected: Also removes the captcha blocks that apply to the links. */
            UnskipLinksAction.unSkipLinks(children);
            return;
        }
        TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                if (!DownloadWatchDog.getInstance().isRunningAndNotPaused()) {
                    return null;
                }
                int nonResumableDownloads = 0;
                long estimatedLostDownloadedBytes = 0;
                for (final DownloadLink downloadLink : children) {
                    final SingleDownloadController con = downloadLink.getDownloadLinkController();
                    if (con != null) {
                        final DownloadInterface dl = con.getDownloadInstance();
                        if (dl != null && !con.getDownloadLink().isResumeable()) {
                            nonResumableDownloads++;
                            estimatedLostDownloadedBytes += Math.max(0, con.getDownloadLink().getView().getBytesLoaded());
                        }
                    }
                }
                if (nonResumableDownloads > 0) {
                    final WarnLevel level;
                    if (estimatedLostDownloadedBytes > 0) {
                        level = WarnLevel.SEVERE;
                    } else {
                        level = WarnLevel.LOW;
                    }
                    if (JDGui.bugme(level)) {
                        if (!UIOManager.I().showConfirmDialog(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, _GUI.T.lit_are_you_sure(), _GUI.T.SkipAction_run_msg_(SizeFormatter.formatBytes(estimatedLostDownloadedBytes), nonResumableDownloads), new AbstractIcon(IconKey.ICON_SKIPPED, 32), _GUI.T.lit_yes(), _GUI.T.lit_no(), "org.jdownloader.gui.views.downloads.action.SkipAction")) {
                            return null;
                        }
                    }
                }
                for (final DownloadLink link : children) {
                    // keep existing skip reason if a reason is already set
                    if (!link.isSkipped()) {
                        link.setSkipReason(SkipReason.MANUAL);
                    }
                }
                return null;
            }
        });
    }
}
