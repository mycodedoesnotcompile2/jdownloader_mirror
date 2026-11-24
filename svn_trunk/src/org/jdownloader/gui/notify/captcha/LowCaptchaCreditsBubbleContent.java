package org.jdownloader.gui.notify.captcha;

import java.awt.event.ActionEvent;

import org.appwork.swing.components.ExtButton;
import org.appwork.swing.components.ExtTextArea;
import org.appwork.utils.swing.SwingUtils;
import org.jdownloader.actions.AppAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.notify.AbstractBubbleContentPanel;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.captchasolver.PluginForCaptchaSolverSolverService;

import jd.controlling.AccountController;
import jd.gui.swing.jdgui.views.settings.panels.accountmanager.BuyAction;
import jd.plugins.Account;
import jd.plugins.PluginForHost;
import net.miginfocom.swing.MigLayout;

/**
 * Content of the low captcha solver credits bubble: the warning text plus buttons (open account manager, buy more credits, hide this
 * session). There is no icon next to the text: the favicon of the solver is shown in the title of the bubble (see
 * {@link LowCaptchaCreditsBubble}), where its small size does not waste space. The window itself does not react to clicks, only the buttons
 * do.
 */
public class LowCaptchaCreditsBubbleContent extends AbstractBubbleContentPanel {
    /**
     * @param plugin
     *            The captcha solver plugin. The buy button (opens the buy dialog with this solver preselected) is only shown if the plugin
     *            has premium enabled.
     */
    public LowCaptchaCreditsBubbleContent(final Account account, final PluginForHost plugin, final String text) {
        super();
        setLayout(new MigLayout("ins 0,wrap 1", "[grow,fill]", "[]"));
        final ExtTextArea textArea = new ExtTextArea();
        textArea.setLabelMode(true);
        textArea.setLineWrap(true);
        textArea.setWrapStyleWord(true);
        SwingUtils.setOpaque(textArea, false);
        textArea.setText(text);
        add(textArea, "pushx,growx");
        add(new ExtButton(new AppAction() {
            {
                setName(_GUI.T.LowCaptchaCreditsBubble_button_accountManager());
                setIconKey(IconKey.ICON_PREMIUM);
            }

            @Override
            public void actionPerformed(final ActionEvent e) {
                getWindow().hideBubble(0);
                PluginForCaptchaSolverSolverService.openAccountManager(account);
            }
        }), "pushx,growx");
        if (plugin.isPremiumEnabled()) {
            add(new ExtButton(new AppAction() {
                {
                    setName(_GUI.T.LowCaptchaCreditsBubble_button_buyCredits());
                    setIconKey(IconKey.ICON_MONEY);
                }

                @Override
                public void actionPerformed(final ActionEvent e) {
                    getWindow().hideBubble(0);
                    new BuyAction(plugin.getLazyP(), "captchasolver/lowcreditsbubble").actionPerformed(e);
                }
            }), "pushx,growx");
        }
        add(new ExtButton(new AppAction() {
            {
                setName(_GUI.T.LowCaptchaCreditsBubble_button_doNotShowAgain());
                setIconKey(IconKey.ICON_FALSE);
            }

            @Override
            public void actionPerformed(final ActionEvent e) {
                getWindow().hideBubble(0);
                AccountController.getInstance().suppressLowCreditsBubble(account);
            }
        }), "pushx,growx");
    }

    @Override
    public void updateLayout() {
    }
}
