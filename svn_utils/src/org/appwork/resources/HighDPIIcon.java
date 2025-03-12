/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.resources;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.geom.AffineTransform;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import org.appwork.utils.images.AbstractIconPipe;
import org.appwork.utils.images.AlignHorizontal;
import org.appwork.utils.images.AlignVertical;
import org.appwork.utils.images.CroppedIcon;
import org.appwork.utils.images.ModificationType;

/**
 * @author thomas
 * @date Feb 8, 2025
 *
 */
public class HighDPIIcon extends AbstractIconPipe {
    public HighDPIIcon(Icon icon) {
        super(icon);
    }

    private static final Set<ModificationType> MODIFICATIONS = Collections.unmodifiableSet(new HashSet<ModificationType>(Arrays.asList(ModificationType.NONE)));

    /**
     * @see org.appwork.utils.images.IconPipe#getModifications()
     */
    @Override
    public Set<ModificationType> getModifications() {
        return MODIFICATIONS;
    }

    /**
     * @param image
     */
    public HighDPIIcon(Image image) {
        this(new ImageIcon(image));
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y, List<Icon> parents) {
        if (!(delegate instanceof ImageIcon)) {
            paintDelegate(c, g, x, y, parents);
            return;
        }
        Image image = ((ImageIcon) delegate).getImage();
        Graphics2D g2 = (Graphics2D) g;
        AffineTransform transform = g2.getTransform();
        double scaleX = transform.getScaleX();
        double scaleY = transform.getScaleY();
        if (scaleX != 1d || scaleY != 1d) {
            if (isHDPIFixRequired(scaleX, scaleY, transform.getTranslateX(), transform.getTranslateY())) {
                double scaledW = getIconWidth() * scaleX;
                double scaledH = getIconHeight() * scaleY;
                Image best = MultiResolutionImageHelper.isInstanceOf(image) ? MultiResolutionImageHelper.getResolutionVariant(image, scaledW, scaledH) : image;
                boolean perfectFit = best.getWidth(null) == scaledW && best.getHeight(null) == scaledH;
                AffineTransform currentTransform = g2.getTransform();
                // correct x/y position and avoid that hava tries to draw between physical pixals.
                double fixtX = currentTransform.getTranslateX() % 1d;
                double fixtY = currentTransform.getTranslateY() % 1d;
                g2.translate(-fixtX, -fixtY);
                if (perfectFit) {
                    // System.out.println("perfeftFit HighDPI");
                    g2.scale(1 / scaleX, 1 / scaleY);
                    g2.drawImage(best, (int) Math.round(x * scaleX), (int) Math.round(y * scaleY), c);
                    g2.translate(fixtX, fixtY);
                    g2.scale(scaleX, scaleY);
                } else if (Math.abs(scaledW - best.getWidth(null)) > 1 || Math.abs(scaledH - best.getHeight(null)) > 1) {
                    // we have no highRes image. draw normal
                    // System.out.println("bypass HighDPI");
                    paintDelegate(c, g, x, y, parents);
                    g2.translate(fixtX, fixtY);
                    return;
                } else {
                    // System.out.println("crop HighDPI");
                    g2.scale(1 / scaleX, 1 / scaleY);
                    new CroppedIcon(new ImageIcon(best), (int) Math.round(scaledW), (int) Math.round(scaledH), AlignHorizontal.CENTER, AlignVertical.CENTER).paintIcon(c, g2, (int) (x * scaleX), (int) (y * scaleY));
                    g2.translate(fixtX, fixtY);
                    g2.scale(scaleX, scaleY);
                }
                return;
            }
        }
        paintDelegate(c, g, x, y, parents);
    }

    /**
     * @param scaleX
     * @param scaleY
     * @return
     */
    /**
     * @param scaleY
     * @param scaleX
     * @param translateY
     * @param translateX
     * @return
     */
    public boolean isHDPIFixRequired(double scaleX, double scaleY, double translateX, double translateY) {
        // Check if the current offset is not on whole physical pixels, but somewhere in between. This would result in intermediate pixel
        // calculation and ugly display
        // translateX *= scaleX;
        if (translateX % 1d != 0) {
            return true;
        }
        // translateY *= scaleY;
        if (translateY % 1d != 0) {
            return true;
        }
        boolean scaleIsDividableBy50 = (scaleX % 0.50d == 0) && (scaleY % 0.50d == 0);
        boolean iconIsDividableBy2 = getIconHeight() % 2 == 0 && getIconHeight() % 2 == 0;
        if (scaleIsDividableBy50 && iconIsDividableBy2) {
            // Example: the scale is 150%, the icon is 13x13 px. Java expects a highDPI Image of 13*150%=19,5px. We cannot provide
            // 19,5Pixel, so java will try to scale it and try to paint the result on a half physical pixel -> The result cannot be good
            // ergo: For a Monitor scale of 150%, all images should be dividable by 2
            return false;
        }
        boolean scaleIsDividableBy25 = (scaleX % 0.25d == 0) && (scaleY % 0.25d == 0);
        boolean iconIsDividableBy4 = getIconHeight() % 4 == 0 && getIconHeight() % 4 == 0;
        if (scaleIsDividableBy25 && iconIsDividableBy4) {
            // example: same as above. but with 1/4 and 4
            return false;
        }
        return true;
    }
}