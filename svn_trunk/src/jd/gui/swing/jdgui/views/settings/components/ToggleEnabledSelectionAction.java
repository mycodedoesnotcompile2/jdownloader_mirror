package jd.gui.swing.jdgui.views.settings.components;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Icon;

import org.appwork.swing.components.CheckBoxIcon;
import org.appwork.swing.components.ExtMergedIcon;
import org.appwork.utils.images.TransparentIcon;
import org.jdownloader.actions.AppAction;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;

/**
 * Context menu action which enables/disables all selected table entries at once. Behaves like the enable/disable action of the download
 * list: label and checkbox icon depend on the state of the selection (all enabled / all disabled / mixed). With a mixed selection, the
 * entry the context menu was opened on decides: if it is enabled, the whole selection gets enabled, otherwise disabled.
 */
public abstract class ToggleEnabledSelectionAction<T> extends AppAction {
    private static final long serialVersionUID = 1L;

    private static enum State {
        ALL_ENABLED(false, true, true),
        ALL_DISABLED(true, false, true),
        MIXED_ENABLE(true, true, false),
        MIXED_DISABLE(false, false, false);
        private final boolean enable;
        private final boolean iconSelected;
        private final boolean iconEnabled;

        private State(final boolean enable, final boolean iconSelected, final boolean iconEnabled) {
            this.enable = enable;
            this.iconSelected = iconSelected;
            this.iconEnabled = iconEnabled;
        }

        private Icon createIcon() {
            return new ExtMergedIcon(NewTheme.I().getIcon("select", 18), -2, -2).add(new TransparentIcon(new CheckBoxIcon(14, iconSelected, iconEnabled), 0.75f), 6, 6);
        }
    }

    private final List<T> selection;
    private final State   state;

    public ToggleEnabledSelectionAction(final T contextObject, final List<T> selection) {
        this.selection = new ArrayList<T>(selection);
        this.state = getState(contextObject);
        setSmallIcon(state.createIcon());
        setName(state.enable ? _GUI.T.EnabledAction_EnabledAction_enable() : _GUI.T.EnabledAction_EnabledAction_disable());
    }

    private State getState(final T contextObject) {
        Boolean first = null;
        for (final T entry : selection) {
            final boolean enabled = isEntryEnabled(entry);
            if (first == null) {
                first = enabled;
            } else if (first.booleanValue() != enabled) {
                if (contextObject != null && isEntryEnabled(contextObject)) {
                    return State.MIXED_ENABLE;
                } else {
                    return State.MIXED_DISABLE;
                }
            }
        }
        return first != null && first.booleanValue() ? State.ALL_ENABLED : State.ALL_DISABLED;
    }

    protected abstract boolean isEntryEnabled(T entry);

    protected abstract void setEntryEnabled(T entry, boolean enabled);

    /**
     * Called after all entries have been changed, e.g. to repaint the table.
     */
    protected void onFinished() {
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        for (final T entry : selection) {
            setEntryEnabled(entry, state.enable);
        }
        onFinished();
    }
}
