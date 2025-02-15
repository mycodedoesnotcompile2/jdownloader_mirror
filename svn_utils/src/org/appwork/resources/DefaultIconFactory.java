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
package org.appwork.resources;

import java.awt.Image;
import java.net.URL;

import javax.swing.Icon;
import javax.swing.JComponent;

import org.appwork.utils.ImageProvider.ImageProvider;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.images.Interpolation;

/**
 * @author thomas
 * @date 23.10.2023
 *
 */
public class DefaultIconFactory implements IconFactory {
    /**
     * @see org.appwork.resources.IconFactory#urlToVectorIcon(java.net.URL, int, int)
     */
    @Override
    public Icon urlToVectorIcon(URL url, int w, int h) {
        return IconIO.loadVectorIcon(url, w, h);
    }

    /**
     * @see org.appwork.resources.IconFactory#scale(javax.swing.Icon, int, int)
     */
    @Override
    public Icon scale(Icon ret, int w, int h) {
        return IconIO.getScaledInstance(ret, w, h, Interpolation.BICUBIC);
    }

    /**
     * @see org.appwork.resources.IconFactory#getDisabled(javax.swing.JComponent, javax.swing.Icon)
     */
    @Override
    public Icon getDisabled(JComponent component, Icon icon) {
        // (DebugMode.debugger();
        return ImageProvider.getDisabledIcon(component, icon);
    }

    /**
     * @see org.appwork.resources.IconFactory#urlToImage(java.net.URL, int, int)
     */
    @Override
    public Image urlToImage(URL url) {
        return IconIO.loadImage(url);
    }

    /**
     * @see org.appwork.resources.IconFactory#imageToIcon(java.awt.Image, int, int)
     */
    @Override
    public Icon imageToIcon(Image image, int w, int h) {
        return new HighDPIIcon(image);
    }
}
