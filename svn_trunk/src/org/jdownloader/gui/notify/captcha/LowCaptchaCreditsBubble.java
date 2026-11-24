package org.jdownloader.gui.notify.captcha;

import javax.swing.Icon;

import org.jdownloader.gui.notify.gui.AbstractNotifyWindow;

import jd.plugins.Account;
import jd.plugins.PluginForHost;

/**
 * Bubble window for {@link LowCaptchaCreditsBubbleSupport}. Unlike {@link org.jdownloader.gui.notify.BasicNotify} a click on the bubble
 * itself does nothing: the actions are only available via the buttons of its content.
 */
public class LowCaptchaCreditsBubble extends AbstractNotifyWindow<LowCaptchaCreditsBubbleContent> {
    /**
     * @param headerIcon
     *            Small icon shown in front of the title (the favicon of the solver)
     */
    public LowCaptchaCreditsBubble(final LowCaptchaCreditsBubbleSupport support, final String caption, final Icon headerIcon, final Account account, final PluginForHost plugin, final String text) {
        super(support, caption, new LowCaptchaCreditsBubbleContent(account, plugin, text));
        setHeaderIcon(headerIcon);
    }
}
