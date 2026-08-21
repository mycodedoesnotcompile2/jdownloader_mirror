package jd.gui.swing.jdgui.components.toolbar.actions;

import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.toolbar.action.AbstractToolbarToggleAction;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.settings.staticreferences.CFG_LINKGRABBER;

/**
 * Toggles the global "merge moved packages into existing same named packages in the downloadlist" setting. Behaves like
 * {@link ClipBoardToggleAction} but without the flashing behavior.
 */
public class MergeSameNamedPackagesOnConfirmToggleAction extends AbstractToolbarToggleAction {
    public MergeSameNamedPackagesOnConfirmToggleAction() {
        super(CFG_LINKGRABBER.MERGE_SAME_NAMED_PACKAGES_IN_DOWNLOADLIST_IN_EXISTING_PACKAGES_ON_CONFIRM_DEFAULT_ENABLED);
        setIconKey(IconKey.ICON_REMOVE_DUPES);
    }

    @Override
    protected String createTooltip() {
        return _GUI.T.MergeSameNamedPackagesOnConfirmToggleAction_tooltip();
    }

    @Override
    protected String getNameWhenDisabled() {
        return _GUI.T.MergeSameNamedPackagesOnConfirmToggleAction_getNameWhenDisabled_();
    }

    @Override
    protected String getNameWhenEnabled() {
        return _GUI.T.MergeSameNamedPackagesOnConfirmToggleAction_getNameWhenEnabled_();
    }
}
