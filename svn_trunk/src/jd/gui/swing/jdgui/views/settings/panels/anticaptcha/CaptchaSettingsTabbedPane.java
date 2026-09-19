package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import javax.swing.JTabbedPane;

import jd.gui.swing.jdgui.views.settings.components.SettingsComponent;

/**
 * Simple {@link JTabbedPane} that can be added to an {@link org.jdownloader.gui.settings.AbstractConfigPanel} as a growing settings
 * component. Used to host the captcha solver overview and the captcha rules as tabs.
 */
public class CaptchaSettingsTabbedPane extends JTabbedPane implements SettingsComponent {
    private static final long serialVersionUID = 1L;

    @Override
    public String getConstraints() {
        return "height 60:n:n,pushy,growy";
    }

    @Override
    public boolean isMultiline() {
        return true;
    }
}
