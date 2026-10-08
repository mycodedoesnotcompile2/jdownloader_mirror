package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.Rectangle;

import javax.swing.Scrollable;

import org.appwork.swing.MigPanel;

/**
 * A panel which always has the width of the scroll pane viewport it is shown in (it never gets wider because of its content). Without
 * this, a view of a scroll pane gets at least its preferred width, and the preferred width of e.g. long, not yet wrapped description
 * labels would push other components (like input fields) out of the visible area, since the scroll panes in the captcha settings never
 * show a horizontal scrollbar. Same approach as the scroll wrapper of the plugin settings panel.
 */
public class WidthTrackingPanel extends MigPanel implements Scrollable {
    private static final long serialVersionUID = 1L;

    public WidthTrackingPanel(final String layoutConstraints, final String colConstraints, final String rowConstraints) {
        super(layoutConstraints, colConstraints, rowConstraints);
        setOpaque(false);
    }

    @Override
    public Dimension getPreferredScrollableViewportSize() {
        return getPreferredSize();
    }

    @Override
    public int getScrollableBlockIncrement(final Rectangle visibleRect, final int orientation, final int direction) {
        return Math.max(visibleRect.height * 9 / 10, 1);
    }

    @Override
    public int getScrollableUnitIncrement(final Rectangle visibleRect, final int orientation, final int direction) {
        return Math.max(visibleRect.height / 10, 1);
    }

    @Override
    public boolean getScrollableTracksViewportWidth() {
        final Container parent = getParent();
        /* Not tracking would only be needed if the viewport is narrower than the absolute minimum width of the content. */
        return parent == null || parent.getWidth() >= getMinimumSize().width;
    }

    @Override
    public boolean getScrollableTracksViewportHeight() {
        return false;
    }
}
