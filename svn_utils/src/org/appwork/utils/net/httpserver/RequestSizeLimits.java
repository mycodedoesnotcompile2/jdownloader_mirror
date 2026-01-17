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
package org.appwork.utils.net.httpserver;

/**
 * Configuration class for request size limits.
 *
 * @author AppWork
 */
public class RequestSizeLimits {

    /**
     * Maximum allowed size for request headers including request line, headers, and GET parameters (in bytes). Set to -1 to disable this
     * limit.
     */
    private final int  maxHeaderSize;

    /**
     * Maximum allowed size for POST body (in bytes). Set to -1 to disable this limit.
     */
    private final long maxPostBodySize;

    /**
     * Maximum allowed size for processed POST data (after decompression, decryption, etc.) in bytes. Set to -1 to disable this limit.
     */
    private final long maxPostProcessedSize;

    /**
     * Creates a new RequestSizeLimits configuration.
     *
     * @param maxHeaderSize
     *            Maximum size for headers (request line + headers + GET parameters) in bytes. Use -1 to disable.
     * @param maxPostBodySize
     *            Maximum size for POST body in bytes. Use -1 to disable.
     */
    public RequestSizeLimits(final int maxHeaderSize, final long maxPostBodySize) {
        this.maxHeaderSize = maxHeaderSize;
        this.maxPostBodySize = maxPostBodySize;
        this.maxPostProcessedSize = -1; // Disabled by default
    }

    /**
     * Creates a new RequestSizeLimits configuration.
     *
     * @param maxHeaderSize
     *            Maximum size for headers (request line + headers + GET parameters) in bytes. Use -1 to disable.
     * @param maxPostBodySize
     *            Maximum size for POST body in bytes. Use -1 to disable.
     * @param maxPostProcessedSize
     *            Maximum size for processed POST data (after decompression, decryption, etc.) in bytes. Use -1 to disable.
     */
    public RequestSizeLimits(final int maxHeaderSize, final long maxPostBodySize, final long maxPostProcessedSize) {
        this.maxHeaderSize = maxHeaderSize;
        this.maxPostBodySize = maxPostBodySize;
        this.maxPostProcessedSize = maxPostProcessedSize;
    }

    /**
     * Returns the maximum allowed header size in bytes.
     *
     * @return Maximum header size in bytes, or -1 if disabled
     */
    public int getMaxHeaderSize() {
        return maxHeaderSize;
    }

    /**
     * Returns the maximum allowed POST body size in bytes.
     *
     * @return Maximum POST body size in bytes, or -1 if disabled
     */
    public long getMaxPostBodySize() {
        return maxPostBodySize;
    }

    /**
     * Checks if header size limit is enabled.
     *
     * @return true if header size limit is enabled
     */
    public boolean isHeaderSizeLimitEnabled() {
        return maxHeaderSize > 0;
    }

    /**
     * Checks if POST body size limit is enabled.
     *
     * @return true if POST body size limit is enabled
     */
    public boolean isPostBodySizeLimitEnabled() {
        return maxPostBodySize > 0;
    }

    /**
     * Returns the maximum allowed processed POST data size in bytes.
     *
     * @return Maximum processed POST data size in bytes, or -1 if disabled
     */
    public long getMaxPostProcessedSize() {
        return maxPostProcessedSize;
    }

    /**
     * Checks if processed POST data size limit is enabled.
     *
     * @return true if processed POST data size limit is enabled
     */
    public boolean isPostProcessedSizeLimitEnabled() {
        return maxPostProcessedSize > 0;
    }

    /**
     * Returns a string representation of this request size limits configuration for debugging purposes.
     * 
     * <p>
     * The format shows all configured limits in a readable format:
     * </p>
     * <pre>
     * RequestSizeLimits[maxHeaderSize=8192, maxPostBodySize=1048576, maxPostProcessedSize=-1]
     * </pre>
     * 
     * @return A string representation of this request size limits configuration
     */
    @Override
    public String toString() {
        return "RequestSizeLimits[maxHeaderSize=" + this.maxHeaderSize + 
               ", maxPostBodySize=" + this.maxPostBodySize + 
               ", maxPostProcessedSize=" + this.maxPostProcessedSize + "]";
    }
}
