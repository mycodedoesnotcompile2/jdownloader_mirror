package jd.gui.swing.jdgui.views.settings.panels.accountmanager;

import java.awt.event.ActionEvent;
import java.util.List;

import javax.swing.AbstractAction;

import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;

/**
 * Context menu counterpart of the button in the "Settings" column: opens the plugin settings of the selected account (see
 * {@link AccountEntry#showConfiguration()}).
 */
public class SettingsAction extends AbstractAction {
    private static final long        serialVersionUID = 1L;
    private final List<AccountEntry> selection;

    public SettingsAction(final List<AccountEntry> selectedObjects) {
        selection = selectedObjects;
        this.putValue(NAME, _GUI.T.lit_settings());
        this.putValue(AbstractAction.SMALL_ICON, new AbstractIcon(IconKey.ICON_SETTINGS, 16));
    }

    public void actionPerformed(final ActionEvent e) {
        if (!isEnabled()) {
            return;
        }
        selection.get(0).showConfiguration();
    }

    @Override
    public boolean isEnabled() {
        return selection != null && selection.size() == 1 && selection.get(0).hasConfiguration();
    }
}
