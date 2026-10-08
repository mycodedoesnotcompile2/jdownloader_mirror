package org.jdownloader.gui.notify.captcha;

import java.util.List;

import org.jdownloader.DomainInfo;
import org.jdownloader.gui.notify.AbstractBubbleSupport;
import org.jdownloader.gui.notify.BubbleNotify.AbstractNotifyWindowFactory;
import org.jdownloader.gui.notify.Element;
import org.jdownloader.gui.notify.gui.AbstractNotifyWindow;
import org.jdownloader.gui.notify.gui.CFG_BUBBLE;
import org.jdownloader.gui.translate._GUI;

import jd.plugins.Account;

/**
 * Bubble which warns the user that the credits of a captcha solver account are running low. It offers two buttons: open the account
 * manager with the affected account selected, and open the solver's "buy premium" page in the browser.
 */
public class LowCaptchaCreditsBubbleSupport extends AbstractBubbleSupport {
    private static final LowCaptchaCreditsBubbleSupport INSTANCE = new LowCaptchaCreditsBubbleSupport();

    public static LowCaptchaCreditsBubbleSupport getInstance() {
        return INSTANCE;
    }

    private LowCaptchaCreditsBubbleSupport() {
        super(_GUI.T.LowCaptchaCreditsBubbleSupport_label(), CFG_BUBBLE.BUBBLE_NOTIFY_ON_LOW_CAPTCHA_CREDITS_ENABLED);
    }

    @Override
    public List<Element> getElements() {
        return null;
    }

    /**
     * @param balance
     *            Already formatted remaining credits
     * @param threshold
     *            Already formatted warning threshold
     * @param buyCreditsUrl
     *            The solver's full "buy premium" link (built like the account manager's one, see AccountController#buildAfflink), may be null (then the buy button is not shown)
     */
    public void show(final Account account, final String balance, final String threshold, final String buyCreditsUrl) {
        show(new AbstractNotifyWindowFactory() {
            @Override
            public AbstractNotifyWindow<?> buildAbstractNotifyWindow() {
                /* The domain and its favicon (small, 16px) are part of the title only: a bigger favicon would be lowres and waste space. */
                final DomainInfo domainInfo = account.getDomainInfo();
                return new LowCaptchaCreditsBubble(LowCaptchaCreditsBubbleSupport.this, _GUI.T.LowCaptchaCreditsBubble_caption(domainInfo.getTld()), domainInfo.getIcon(16), account, _GUI.T.LowCaptchaCreditsBubble_text(balance, threshold), buyCreditsUrl);
            }
        });
    }
}
