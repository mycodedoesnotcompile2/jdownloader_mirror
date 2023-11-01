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
package org.appwork.utils.swing.dimensor;

import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Window;

import org.appwork.utils.StringUtils;

/**
 * @author Thomas
 *
 */
public abstract class AbstractDimensor implements DimensorInterface {
    protected String id;

    /**
     * @param id
     */
    public AbstractDimensor(String id) {
        this.id = id;
    }

    /**
     * @param dimension
     * @param dialog
     * @return
     */
    protected Dimension validate(final Dimension dimension, final Window window) {
        if (dimension != null && window != null) {
            final Dimension minimum = window.getMinimumSize();
            if (minimum != null) {
                if (dimension.height <= minimum.height && dimension.width <= minimum.width) {
                    return null;
                } else {
                    final Dimension ret = new Dimension(Math.max(dimension.width, minimum.width), Math.max(dimension.height, minimum.height));
                    return ret;
                }
            }
        }
        return dimension;
    }

    /**
     * @param frame
     * @return
     */
    protected String getStorageID(Window window) {
        return getClass().getName().replaceAll("(^.*\\.|[\\d\\$]+$)", "") + "-" + getID(window);
    }

    protected String getID(final Window window) {
        if (id == null) {
            String title = "";
            if (window instanceof Dialog) {
                title = ((Dialog) window).getTitle();
            }
            if (StringUtils.isEmpty(title) && window instanceof Frame) {
                title = ((Frame) window).getTitle();
            }
            id = window.getClass().getName().replaceAll("[\\d\\$]+$", "") + "-" + title;
        }
        return id;
    }

}
