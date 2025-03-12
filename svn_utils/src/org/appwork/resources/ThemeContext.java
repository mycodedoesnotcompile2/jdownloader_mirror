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

import java.net.URL;
import java.util.HashMap;

/**
 * @author thomas
 * @date Feb 12, 2025
 *
 */
public class ThemeContext {
    public static enum Target {
        IMAGE,
        ICON
    }

    private boolean              usecache                      = true;
    private boolean              createMultiResolutionImages   = true;
    private boolean              doNotUpscaleButThrowException = false;
    private HashMap<String, URL> urls                          = new HashMap<String, URL>();
    private Target               target                        = null;
    private boolean              debugDrawEnabled              = false;

    /**
     * @return the debugDrawEnabled
     */
    public boolean isDebugDrawEnabled() {
        return debugDrawEnabled;
    }

    /**
     * @param debugDrawEnabled
     *            the debugDrawEnabled to set
     * @return
     */
    public ThemeContext setDebugDrawEnabled(boolean debugDrawEnabled) {
        this.debugDrawEnabled = debugDrawEnabled;
        return this;
    }

    public ThemeContext withDebugDrawEnabled() {
        return setDebugDrawEnabled(true);
    }

    public boolean isUsecache() {
        return usecache;
    }

    public ThemeContext setUsecache(boolean usecache) {
        this.usecache = usecache;
        return this;
    }

    public ThemeContext withoutCache() {
        return setUsecache(false);
    }

    public boolean isCreateMultiResolutionImages() {
        return createMultiResolutionImages;
    }

    public ThemeContext setCreateMultiResolutionImages(boolean createMultiResolutionImages) {
        this.createMultiResolutionImages = createMultiResolutionImages;
        return this;
    }

    public ThemeContext withoutMultiResolutionImages() {
        return setCreateMultiResolutionImages(false);
    }

    public boolean isDoNotUpscaleButThrowException() {
        return doNotUpscaleButThrowException;
    }

    public ThemeContext setDoNotUpscaleButThrowException(boolean doNotUpscaleButThrowException) {
        this.doNotUpscaleButThrowException = doNotUpscaleButThrowException;
        return this;
    }

    public ThemeContext withExceptionOnUpscale() {
        return setDoNotUpscaleButThrowException(true);
    }

    /**
     * @return
     */
    public String toKeyID() {
        StringBuilder key = null;
        if (isCreateMultiResolutionImages()) {
            key = key == null ? new StringBuilder() : key;
            key.append(".MULTIRES");
        }
        if (isDoNotUpscaleButThrowException()) {
            key = key == null ? new StringBuilder() : key;
            key.append(".NO_UPSCALE");
        }
        if (isDebugDrawEnabled()) {
            key = key == null ? new StringBuilder() : key;
            key.append(".DEBUG");
        }
        return key == null ? "" : key.toString();
    }

    /**
     * @param url
     */
    public ThemeContext putURL(String relPath, URL url) {
        this.urls.put(relPath, url);
        return this;
    }

    public URL getURL(String relativePath) {
        return urls.get(relativePath);
    }

    /**
     * @param image
     */
    protected void ensureTarget(Target newTarget) {
        if (target == null) {
            target = newTarget;
        }
    }

    /**
     * @return the target
     */
    protected Target getTarget() {
        return target;
    }
}
