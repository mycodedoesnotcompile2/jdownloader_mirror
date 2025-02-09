package jd.gui.swing.laf;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.net.URL;

import javax.swing.Icon;
import javax.swing.JComponent;

import org.appwork.resources.DefaultIconFactory;
import org.appwork.utils.DebugMode;
import org.appwork.utils.images.MultiResIconImpl;

public class FlatLafIconFactory extends DefaultIconFactory {
    public static class InterpolatingIcon implements Icon {
        private final Icon delegate;

        public InterpolatingIcon(final Icon urlToIcon) {
            DebugMode.breakIf(urlToIcon == null);
            this.delegate = urlToIcon;
        }

        @Override
        public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
            if (this.delegate instanceof MultiResIconImpl) {
                this.delegate.paintIcon(c, g, x, y);
                return;
            }
            final AffineTransform currentTransform = ((Graphics2D) g).getTransform();
            final double scaleX = currentTransform.getScaleX();
            final double scaleY = currentTransform.getScaleY();
            if (scaleX == 1.0 && scaleY == 1.0) {
                // no scaling;
                this.delegate.paintIcon(c, g, x, y);
                return;
            }
            Object restore = ((Graphics2D) g).getRenderingHint(RenderingHints.KEY_INTERPOLATION);
            try {
                ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
                this.delegate.paintIcon(c, g, x, y);
            } finally {
                if (restore == null) {
                    restore = RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR;
                }
                ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_INTERPOLATION, restore);
            }
        }

        @Override
        public int getIconWidth() {
            return this.delegate.getIconWidth();
        }

        @Override
        public int getIconHeight() {
            return this.delegate.getIconHeight();
        }
    }

    public FlatLafIconFactory() {
    }

    @Override
    public Icon urlToNonImageIcon(final URL url, final int w, final int h) {
        Icon mayBeNull = super.urlToNonImageIcon(url, w, h);
        if (mayBeNull == null) {
            return null;
        }
        return new InterpolatingIcon(mayBeNull);
    }

    @Override
    public Icon scale(final Icon ret, final int w, final int h) {
        return new InterpolatingIcon(super.scale(ret, w, h));
    }

    @Override
    public Icon getDisabled(final JComponent component, final Icon icon) {
        return new InterpolatingIcon(super.getDisabled(component, icon));
    }

    @Override
    public Icon imageToIcon(Image image, int w, int h) {
        return new InterpolatingIcon(super.imageToIcon(image, w, h));
    }
}
