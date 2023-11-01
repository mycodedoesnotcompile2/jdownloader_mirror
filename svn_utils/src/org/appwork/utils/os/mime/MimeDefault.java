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
package org.appwork.utils.os.mime;

import java.io.IOException;
import java.util.HashMap;

import javax.swing.Icon;

import org.appwork.resources.AWIcon;
import org.appwork.resources.AWUTheme;

public class MimeDefault implements Mime {
    /**
     * Cache for the MIME descriptions
     */
    private final static HashMap<String, String> DESCRIPTION_CACHE = new HashMap<String, String>();

    /**
     * Returns a icon from the cache.
     *
     * @param iconKey
     * @return
     */
    protected Icon getCacheIcon(final String iconKey) {
        return AWUTheme.I().getCached(iconKey);
    }

    public Icon getFileIcon(final String extension, final int width, final int height) throws IOException {
        return AWIcon.MIME_fileIcon.get(width);
    }

    /**
     * Builds the icon key.
     *
     * @param extension
     * @param width
     * @param height
     * @return
     */
    protected String getIconKey(final String extension, final int width, final int height) {
        final StringBuilder sb = new StringBuilder();
        sb.append(extension);
        sb.append("_");
        sb.append(width);
        sb.append("x");
        sb.append(height);
        return sb.toString();
    }

    public String getMimeDescription(final String mimetype) {
        return mimetype;
    }

    /**
     * Returns the Mime dexcription from the cache
     *
     * @param mimetype
     * @return
     */
    protected String getMimeDescriptionCache(final String mimetype) {
        synchronized (DESCRIPTION_CACHE) {
            return MimeDefault.DESCRIPTION_CACHE.get(mimetype);
        }
    }

    /**
     * Saves a icon in the cache.
     *
     * @param iconKey
     * @param icon
     */
    protected void saveIconCache(final String iconKey, final Icon icon) {
        AWUTheme.I().cache(icon, iconKey);
    }

    /**
     * Saves a mime description in the cache
     *
     * @param mimetype
     * @param description
     */
    protected void saveMimeDescriptionCache(final String mimetype, final String description) {
        synchronized (DESCRIPTION_CACHE) {
            MimeDefault.DESCRIPTION_CACHE.put(mimetype, description);
        }
    }
}