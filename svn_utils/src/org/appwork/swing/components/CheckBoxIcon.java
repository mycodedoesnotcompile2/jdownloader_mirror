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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.swing.components;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashSet;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;

import org.appwork.resources.MultiResolutionImageHelper;
import org.appwork.utils.Application;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.images.ImageCropper;
import org.appwork.utils.images.Interpolation;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;

public final class CheckBoxIcon extends ImageIcon implements Icon {
    public static final CheckBoxIcon FALSE     = new CheckBoxIcon(false);
    public static final CheckBoxIcon TRUE      = new CheckBoxIcon(true);
    public static final CheckBoxIcon UNDEFINED = new CheckBoxIcon(true, false);

    public CheckBoxIcon(final boolean selected, final boolean enabled) {
        // we need this workaround.
        // if we would use cb.paint(g); for every paintIcon call, this might
        // habe sideeffects on the LAF painter.
        Image image = null;
        if (Application.isHeadless()) {
            final Icon icon;
            if (selected) {
                image = HeadlessCheckboxIconRef.HEADLESS_checkbox_true.image(14);
            } else {
                image = HeadlessCheckboxIconRef.HEADLESS_checkbox_false.image(14);
            }
            if (!enabled) {
                image = IconIO.toGrayScale(image);
            }
        } else {
            ArrayList<Image> images = new ArrayList<Image>();
            HashSet<String> dupe = new HashSet<String>();
            dupe.add("1.0:1.0");
            Image base;
            Image img = getImage(1, 1, enabled, selected);
            // +2 to hava slightly border buffer - this avoids cliping borders during scaling
            images.add(base = IconIO.centerImage(img, img.getWidth(null) + 2, img.getHeight(null) + 2, null));
            if (MultiResolutionImageHelper.isSupported()) {
                for (GraphicsDevice sd : GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices()) {
                    AffineTransform tx = sd.getDefaultConfiguration().getDefaultTransform();
                    if (dupe.add(tx.getScaleX() + ":" + tx.getScaleY())) {
                        img = getImage(tx.getScaleX(), tx.getScaleY(), enabled, selected);
                        // use center to ensure proper scaling factors. due to the cropper in getImage, the upscaled versions might be
                        // actually slightly bigger or smaller than desired
                        img = IconIO.centerImage(img, IconIO.clipScale(base.getWidth(null), tx.getScaleX()), IconIO.clipScale(base.getHeight(null), tx.getScaleY()), null);
                        images.add(img);
                    }
                }
                image = MultiResolutionImageHelper.create(base, images);
            } else {
                // since we paint our own image below, we could implement an own MultiResLogic here - or Use MultiResIconImpl.
                image = base;
            }
        }
        setImage(image);
    }

    /**
     * @see javax.swing.ImageIcon#paintIcon(java.awt.Component, java.awt.Graphics, int, int)
     */
    @Override
    public synchronized void paintIcon(Component c, Graphics g, int x, int y) {
        if (true) {
            //
            super.paintIcon(c, g, x, y);
            return;
        } else {
            // might become not sharp
            Object restore = ((Graphics2D) g).getRenderingHint(RenderingHints.KEY_INTERPOLATION);
            try {
                ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_INTERPOLATION, Interpolation.BICUBIC.getHint());
                super.paintIcon(c, g, x, y);
            } finally {
                if (restore == null) {
                    restore = RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR;
                }
                ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_INTERPOLATION, restore);
            }
        }
    }

    public CheckBoxIcon(boolean selected) {
        this(selected, true);
    }

    public static void main(String[] args) {
        Image image = getImage(1.25d, 1.25d, true, true);
        Image small = getImage(1, 1, false, false);
        ;
        try {
            Dialog.getInstance().showConfirmDialog(0, "b", "", TRUE, null, null);
        } catch (DialogClosedException e) {
            // LogV3.log(e);
        } catch (DialogCanceledException e) {
            // LogV3.log(e);
        }
    }

    /**
     * @param scaleX
     * @param scaleY
     * @param selected
     * @param enabled
     * @return
     */
    private static Image getImage(double scaleX, double scaleY, boolean enabled, boolean selected) {
        JCheckBox checkBox = new JCheckBox();
        checkBox.setEnabled(enabled);
        checkBox.setSelected(selected);
        checkBox.setSize(checkBox.getPreferredSize());
        checkBox.setOpaque(false);
        checkBox.setBackground(new Color(1, 0, 0, 0f));
        // +10 due to unknown insets for shadows etc
        BufferedImage image = new BufferedImage((int) ((checkBox.getWidth() + 10) * scaleX), (int) ((checkBox.getHeight() + 10) * scaleY), BufferedImage.TYPE_INT_ARGB);
        Graphics2D g2d = image.createGraphics();
        g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        g2d.scale(scaleX, scaleY);
        checkBox.paint(g2d);
        g2d.dispose();
        // crop - each laf has different insets. this cropping automates the process somehow.
        Image croppedImage = new ImageCropper().crop(image);
        return croppedImage;
    }
}