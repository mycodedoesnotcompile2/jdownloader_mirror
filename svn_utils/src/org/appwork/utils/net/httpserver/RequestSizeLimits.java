/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
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
     * Maximum allowed size for request headers including request line, headers, and GET parameters. Set to -1 to disable this limit.
     */
    private final long maxHeaderSize;

    /**
     * Maximum allowed size for POST body. Set to -1 to disable this limit.
     */
    private final long maxPostBodySize;

    /**
     * Maximum allowed size for processed POST data (after decompression, decryption, etc.). Set to -1 to disable this limit.
     */
    private final long maxPostProcessedSize;

    /**
     * Maximum bytes to drain from input stream on limit exceeded to prevent TCP RST. Set to -1 to disable draining.
     */
    private final long maxDrainInputStreamBytes;

    /**
     * Timeout for draining input stream in milliseconds. Draining stops when timeout is reached or maxDrainInputStreamBytes is reached. Set to -1 to
     * disable timeout (draining only limited by maxDrainInputStreamBytes).
     */
    private final long drainInputStreamTimeout;

    /** Default maximum header size: 16 KB */
    public static final long DEFAULT_MAX_HEADER_SIZE               = 16 * 1024L;
    /** Default maximum POST body size: 2 MB */
    public static final long DEFAULT_MAX_POST_BODY_SIZE            = 2 * 1024 * 1024L;
    /** Default maximum processed POST data size: 10 MB (after decompression, decryption, etc.) */
    public static final long DEFAULT_MAX_POST_PROCESSED_SIZE       = 10 * 1024 * 1024L;
    /** Default maximum bytes to drain from input stream: 10 MB */
    public static final long DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES  = 10 * 1024 * 1024L;
    /** Default timeout for draining input stream: 5 seconds (5000 ms) */
    public static final long DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT    = 5000L;

    /**
     * Creates a new RequestSizeLimits configuration.
     *
     * @param maxHeaderSize
     *            Maximum size for headers (request line + headers + GET parameters) in bytes. Use -1 to disable.
     * @param maxPostBodySize
     *            Maximum size for POST body in bytes. Use -1 to disable.
     */
    public RequestSizeLimits(final long maxHeaderSize, final long maxPostBodySize) {
        this(maxHeaderSize, maxPostBodySize, DEFAULT_MAX_POST_PROCESSED_SIZE, DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES, DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT);
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
    public RequestSizeLimits(final long maxHeaderSize, final long maxPostBodySize, final long maxPostProcessedSize) {
        this(maxHeaderSize, maxPostBodySize, maxPostProcessedSize, DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES, DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT);
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
     * @param maxDrainInputStreamBytes
     *            Maximum bytes to drain from input stream on limit exceeded to prevent TCP RST. Use -1 to disable.
     */
    public RequestSizeLimits(final long maxHeaderSize, final long maxPostBodySize, final long maxPostProcessedSize, final long maxDrainInputStreamBytes) {
        this(maxHeaderSize, maxPostBodySize, maxPostProcessedSize, maxDrainInputStreamBytes, DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT);
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
     * @param maxDrainInputStreamBytes
     *            Maximum bytes to drain from input stream on limit exceeded to prevent TCP RST. Use -1 to disable.
     * @param drainInputStreamTimeout
     *            Timeout for draining in milliseconds. Draining stops when this timeout is reached. Use -1 to disable timeout.
     */
    public RequestSizeLimits(final long maxHeaderSize, final long maxPostBodySize, final long maxPostProcessedSize, final long maxDrainInputStreamBytes, final long drainInputStreamTimeout) {
        this.maxHeaderSize = maxHeaderSize;
        this.maxPostBodySize = maxPostBodySize;
        this.maxPostProcessedSize = maxPostProcessedSize;
        this.maxDrainInputStreamBytes = maxDrainInputStreamBytes;
        this.drainInputStreamTimeout = drainInputStreamTimeout;
    }

    /**
     * Creates a new RequestSizeLimits configuration with default values.
     */
    public RequestSizeLimits() {
        this(DEFAULT_MAX_HEADER_SIZE, DEFAULT_MAX_POST_BODY_SIZE, DEFAULT_MAX_POST_PROCESSED_SIZE, DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES, DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT);
    }

    /**
     * Backwards-compatible constructor for int/long values. Use -1 or 0 to disable limits.
     *
     * @param maxHeaderSize
     *            Maximum size for headers in bytes. Use -1 or 0 to disable.
     * @param maxPostBodySize
     *            Maximum size for POST body in bytes. Use -1 or 0 to disable.
     * @param maxPostProcessedSize
     *            Maximum size for processed POST data in bytes. Use -1 or 0 to disable.
     */
    public RequestSizeLimits(final int maxHeaderSize, final long maxPostBodySize, final long maxPostProcessedSize) {
        this(maxHeaderSize > 0 ? (long) maxHeaderSize : -1, maxPostBodySize > 0 ? maxPostBodySize : -1, maxPostProcessedSize > 0 ? maxPostProcessedSize : -1, DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES, DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT);
    }

    /**
     * Backwards-compatible constructor for int/long values. Use -1 or 0 to disable limits.
     *
     * @param maxHeaderSize
     *            Maximum size for headers in bytes. Use -1 or 0 to disable.
     * @param maxPostBodySize
     *            Maximum size for POST body in bytes. Use -1 or 0 to disable.
     */
    public RequestSizeLimits(final int maxHeaderSize, final long maxPostBodySize) {
        this(maxHeaderSize > 0 ? (long) maxHeaderSize : -1, maxPostBodySize > 0 ? maxPostBodySize : -1, DEFAULT_MAX_POST_PROCESSED_SIZE, DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES, DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT);
    }

    /**
     * Returns the maximum allowed header size in bytes.
     *
     * @return Maximum header size in bytes, or -1 if disabled
     */
    public long getMaxHeaderSize() {
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
     * @return true if header size limit is enabled (> 0)
     */
    public boolean isHeaderSizeLimitEnabled() {
        return maxHeaderSize > 0;
    }

    /**
     * Checks if POST body size limit is enabled.
     *
     * @return true if POST body size limit is enabled (> 0)
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
     * @return true if processed POST data size limit is enabled (> 0)
     */
    public boolean isPostProcessedSizeLimitEnabled() {
        return maxPostProcessedSize > 0;
    }

    /**
     * Returns the maximum bytes to drain from input stream on limit exceeded.
     *
     * @return Maximum bytes to drain, or -1 if draining is disabled
     */
    public long getMaxDrainInputStreamBytes() {
        return maxDrainInputStreamBytes;
    }

    /**
     * Returns the timeout for draining input stream in milliseconds.
     *
     * @return Timeout for draining in milliseconds, or -1 if no timeout is configured
     */
    public long getDrainInputStreamTimeout() {
        return drainInputStreamTimeout;
    }

    /**
     * Checks if input stream draining is enabled. Draining is enabled if timeout is not -1 OR maxDrainInputStreamBytes is > 0.
     *
     * @return true if draining is enabled
     */
    public boolean isDrainInputStreamEnabled() {
        return drainInputStreamTimeout != -1 || maxDrainInputStreamBytes > 0;
    }

    /**
     * Returns a string representation of this request size limits configuration for debugging purposes.
     *
     * <p>
     * The format shows all configured limits in a readable format:
     * </p>
     *
     * <pre>
     * RequestSizeLimits[maxHeaderSize=8192, maxPostBodySize=1048576, maxPostProcessedSize=-1, maxDrainInputStreamBytes=10485760, drainInputStreamTimeout=5000ms]
     * </pre>
     *
     * @return A string representation of this request size limits configuration
     */
    @Override
    public String toString() {
        return "RequestSizeLimits[maxHeaderSize=" + this.maxHeaderSize + ", maxPostBodySize=" + this.maxPostBodySize + ", maxPostProcessedSize=" + this.maxPostProcessedSize + ", maxDrainInputStreamBytes=" + this.maxDrainInputStreamBytes + ", drainInputStreamTimeout=" + this.drainInputStreamTimeout + "ms]";
    }
}
