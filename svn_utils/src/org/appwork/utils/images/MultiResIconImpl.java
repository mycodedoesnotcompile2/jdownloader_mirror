/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.images;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.appwork.utils.CompareUtils;

/**
 * An icon that has several images in different resolution. the icon tries to paint the image as good as possible
 *
 * @author thomas
 * @date 22.11.2023
 *
 */
public class MultiResIconImpl implements MultiResIcon {
    private CopyOnWriteArrayList<BufferedImage> images;

    public List<BufferedImage> getImages() {
        return Collections.unmodifiableList(images);
    }

    private int           iconWidth;
    private int           iconHeight;
    private BufferedImage largest;

    public BufferedImage getLargest() {
        return largest;
    }

    public int getIconWidth() {
        return iconWidth;
    }

    public void setIconWidth(int iconWidth) {
        this.iconWidth = iconWidth;
    }

    public int getIconHeight() {
        return iconHeight;
    }

    public void setIconHeight(int iconHeight) {
        this.iconHeight = iconHeight;
    }

    /**
     * @param icons
     * @param iconSize
     * @param iconSize2
     */
    public MultiResIconImpl(List<BufferedImage> images, int iconWidth, int iconHeight) {
        ArrayList<BufferedImage> tmp = new ArrayList<BufferedImage>(images);
        Collections.sort(tmp, new Comparator<BufferedImage>() {
            @Override
            public int compare(BufferedImage o1, BufferedImage o2) {
                return CompareUtils.compareInt(o1.getWidth() * o1.getHeight(), o2.getWidth() * o2.getHeight());
            }
        });
        this.images = new CopyOnWriteArrayList<BufferedImage>(tmp);
        this.largest = this.images.get(images.size() - 1);
        if (iconWidth <= 0) {
            iconWidth = largest.getWidth();
        }
        if (iconHeight <= 0) {
            iconHeight = largest.getHeight();
        }
        this.iconWidth = iconWidth;
        this.iconHeight = iconHeight;
    }

    /**
     * @see javax.swing.Icon#paintIcon(java.awt.Component, java.awt.Graphics, int, int)
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        paintIcon(c, g, x, y, getIconWidth(), getIconHeight());
    }

    /**
     * @see org.appwork.utils.images.MultiResIcon#paintIcon(java.awt.Component, java.awt.Graphics, int, int, int, int)
     */
    @Override
    public void paintIcon(Component c, Graphics g, int x, int y, int width, int height) {
        AffineTransform currentTransform = ((Graphics2D) g).getTransform();
        double scaleX = currentTransform.getScaleX();
        double scaleY = currentTransform.getScaleY();
        Graphics2D g2d = ((Graphics2D) g);
        BufferedImage base = largest;
        int index = 0;
        if (width <= 0) {
            width = largest.getWidth();
        }
        if (height <= 0) {
            height = largest.getHeight();
        }
        if (width > 0 && height > 0) {
            int scaledWidth = (int) (width * scaleX);
            int scaledHeight = (int) (height * scaleY);
            for (BufferedImage image : images) {
                base = image;
                if (image.getWidth(null) >= scaledWidth && image.getHeight(null) >= scaledHeight) {
                    break;
                }
                index++;
            }
            if (base.getWidth(null) != scaledWidth || base.getHeight(null) != scaledHeight) {
                // cache
                base = IconIO.getScaledInstance(base, scaledWidth, scaledHeight, Interpolation.BICUBIC, true);
                images.add(index, base);
            }
            if (scaleX != 1.0 || scaleY != 1.0) {
                // Inverse the scaling
                AffineTransform inverseScale = new AffineTransform();
                inverseScale.scale(1.0 / scaleX, 1.0 / scaleY);
                g2d.translate(x, y);
                g2d.transform(inverseScale); // Apply the inverse scaling
                // Draw a larger image to compensate for the removed scaling
                g2d.drawImage(base, 0, 0, scaledWidth, scaledHeight, c);
                // Restore the original transform to avoid affecting further drawing
                g2d.setTransform(currentTransform);
                return;
            } else {
                g.drawImage(base, x, y, c);
            }
        }
    }

    /**
     * @return the image that represesents exactly the icons size or null
     */
    public Image getImage() {
        for (BufferedImage image : images) {
            if (image.getWidth(null) == getIconWidth() && image.getHeight(null) == getIconHeight()) {
                return image;
            }
        }
        return null;
    }
}
