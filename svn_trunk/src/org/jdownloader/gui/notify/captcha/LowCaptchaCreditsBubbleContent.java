package org.jdownloader.gui.notify.captcha;

import java.awt.event.ActionEvent;

import net.miginfocom.swing.MigLayout;

import org.appwork.swing.components.ExtButton;
import org.appwork.swing.components.ExtTextArea;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.SwingUtils;
import org.jdownloader.actions.AppAction;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.notify.AbstractBubbleContentPanel;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.captchasolver.PluginForCaptchaSolverSolverService;

import jd.controlling.AccountController;
import jd.plugins.Account;

/**
 * Content of the low captcha solver credits bubble: the warning text plus buttons (open account manager, buy more credits, hide this
 * session). There is no icon next to the text: the favicon of the solver is shown in the title of the bubble (see
 * {@link LowCaptchaCreditsBubble}), where its small size does not waste space. The window itself does not react to clicks, only the
 * buttons do.
 */
public class LowCaptchaCreditsBubbleContent extends AbstractBubbleContentPanel {
    /**
     * @param buyCreditsUrl
     *            The solver's "buy premium" URL. If null or empty, the buy button is not shown.
     */
    public LowCaptchaCreditsBubbleContent(final Account account, final String text, final String buyCreditsUrl) {
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
                PluginForCaptchaSolverSolverService.openAccountManager(account);
                getWindow().hideBubble(0);
            }
        }), "pushx,growx");
        if (StringUtils.isNotEmpty(buyCreditsUrl)) {
            add(new ExtButton(new AppAction() {
                {
                    setName(_GUI.T.LowCaptchaCreditsBubble_button_buyCredits());
                    setIconKey(IconKey.ICON_MONEY);
                }

                @Override
                public void actionPerformed(final ActionEvent e) {
                    CrossSystem.openURL(buyCreditsUrl);
                    getWindow().hideBubble(0);
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
                AccountController.getInstance().suppressLowCreditsBubble(account);
                getWindow().hideBubble(0);
            }
        }), "pushx,growx");
    }

    @Override
    public void updateLayout() {
    }
}
