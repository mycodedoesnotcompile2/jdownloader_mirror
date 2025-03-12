/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
import java.awt.Image;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.Icon;

import org.appwork.resources.HighDPIIcon;
import org.appwork.resources.MultiResolutionImageHelper;

/**
 * @author thomas
 * @date 13.12.2023
 *
 */
public class DisabledIcon extends AbstractIconPipe {
    private HighDPIIcon grayedIcon;
    private final int   width;
    private final int   height;

    public DisabledIcon(Icon icon) {
        super(icon);
        width = icon.getIconWidth();
        height = icon.getIconHeight();
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y, List<Icon> parents) {
        if (grayedIcon == null) {
            synchronized (this) {
                if (grayedIcon == null) {
                    // System.out.println("To GrayScale DisabledIcon: " + IconIO.getPrimaryIdentifier(delegate));
                    Image image = IconIO.toImage(delegate);
                    if (MultiResolutionImageHelper.isSupported() && !MultiResolutionImageHelper.isInstanceOf(image)) {
                        // if delegate is e.g. a extmergeIcon, we cannot get an multiRes image from it.
                        // this workaround. assumes, that there might be some highdpi icons inside it, and create a bigger version of it
                        double scaling = MultiResolutionImageHelper.getHighestMonitorScaling();
                        if (scaling != 1.0) {
                            Image imagebig = IconIO.toImage(IconIO.getScaledInstance(delegate, (int) Math.round(getIconWidth() * scaling), (int) Math.round(getIconHeight() * scaling)));
                            image = MultiResolutionImageHelper.create(image, Arrays.asList(image, imagebig));
                        }
                    }
                    Image grayImage = IconIO.toGrayScale(image);
                    grayedIcon = new HighDPIIcon(grayImage);
                    delegate = null;
                }
            }
        }
        if (parents == null) {
            parents = new ArrayList<Icon>();
        }
        parents.add(this);
        grayedIcon.paintIcon(c, g, x, y, parents);
    }

    /**
     * @see org.appwork.utils.images.AbstractIconPipe#getIconHeight()
     */
    /**
     * @see org.appwork.utils.images.AbstractIconPipe#getIconWidth()
     */
    @Override
    public int getIconWidth() {
        return width;
    }

    /**
     * @see org.appwork.utils.images.AbstractIconPipe#getIconHeight()
     */
    @Override
    public int getIconHeight() {
        return height;
    }

    private static final Set<ModificationType> MODIFICATIONS = Collections.unmodifiableSet(new HashSet<ModificationType>(Arrays.asList(ModificationType.COLOR)));

    /**
     * @see org.appwork.utils.images.IconPipe#getModifications()
     */
    @Override
    public Set<ModificationType> getModifications() {
        return MODIFICATIONS;
    }
}
