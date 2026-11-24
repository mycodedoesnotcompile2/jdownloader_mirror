package org.jdownloader.gui.dialog;

import java.awt.Dialog.ModalityType;
import java.util.concurrent.TimeUnit;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;

import org.appwork.uio.UIOManager;
import org.appwork.utils.swing.dialog.ConfirmDialog;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.sponsor.AdDisclosureHelper;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;

import jd.controlling.AccountController;
import jd.plugins.PluginForHost;

public class AskToUsePremiumDialog extends ConfirmDialog implements AskToUsePremiumDialogInterface {
    protected final PluginForHost plugin;

    public AskToUsePremiumDialog(PluginForHost plugin) {
        super(UIOManager.LOGIC_COUNTDOWN, _GUI.T.PluginForHost_showFreeDialog_title(plugin.getHost()), _GUI.T.PluginForHost_showFreeDialog_message(plugin.getHost()), new AbstractIcon(IconKey.ICON_PREMIUM, 32), _GUI.T.lit_yes(), _GUI.T.lit_no());
        setTimeout((int) TimeUnit.MINUTES.toMillis(5));
        this.plugin = plugin;
    }

    @Override
    protected void addButtons(JPanel buttonBar) {
        /* The ad disclosure is intentionally always shown here. */
        final JLabel adDisclosureText = AdDisclosureHelper.createAdDisclosureLabel(null, SwingConstants.BOTTOM);
        buttonBar.add(adDisclosureText, "aligny bottom, gapbottom 2", 0);
    };

    @Override
    public ModalityType getModalityType() {
        return ModalityType.MODELESS;
    }

    @Override
    public String getPremiumUrl() {
        // marker: see adDisclosure label
        return AccountController.buildAfflink(plugin.getLazyP(), plugin, "freedialog");
    }

    @Override
    public String getDomain() {
        return plugin.getHost();
    }
}