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

import org.appwork.utils.ReadableBytes;
import org.appwork.utils.duration.TimeSpan;

/**
 * Configuration class for request size limits.
 *
 * @author AppWork
 */
public class RequestSizeLimits {

    /**
     * Maximum allowed size for request headers including request line, headers, and GET parameters. Set to null to disable this limit.
     */
    private final ReadableBytes maxHeaderSize;

    /**
     * Maximum allowed size for POST body. Set to null to disable this limit.
     */
    private final ReadableBytes maxPostBodySize;

    /**
     * Maximum allowed size for processed POST data (after decompression, decryption, etc.). Set to null to disable this limit.
     */
    private final ReadableBytes maxPostProcessedSize;

    /**
     * Maximum bytes to drain from input stream on limit exceeded to prevent TCP RST. Set to null to disable draining.
     */
    private final ReadableBytes maxDrainInputStreamBytes;

    /**
     * Timeout for draining input stream. Draining stops when timeout is reached or maxDrainInputStreamBytes is reached. Set to null to
     * disable timeout (draining only limited by maxDrainInputStreamBytes).
     */
    private final TimeSpan      drainInputStreamTimeout;

    /** Default maximum header size: 16 KB */
    public static final ReadableBytes DEFAULT_MAX_HEADER_SIZE               = ReadableBytes.fromBytes(16 * 1024);
    /** Default maximum POST body size: 2 MB */
    public static final ReadableBytes DEFAULT_MAX_POST_BODY_SIZE            = ReadableBytes.fromBytes(2 * 1024 * 1024);
    /** Default maximum processed POST data size: 10 MB (after decompression, decryption, etc.) */
    public static final ReadableBytes DEFAULT_MAX_POST_PROCESSED_SIZE       = ReadableBytes.fromBytes(10 * 1024 * 1024);
    /** Default maximum bytes to drain from input stream: 10 MB */
    public static final ReadableBytes DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES  = ReadableBytes.fromBytes(10 * 1024 * 1024);
    /** Default timeout for draining input stream: 5 seconds */
    public static final TimeSpan      DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT    = TimeSpan.fromMillis(5000);

    /**
     * Creates a new RequestSizeLimits configuration.
     *
     * @param maxHeaderSize
     *            Maximum size for headers (request line + headers + GET parameters). Use null to disable.
     * @param maxPostBodySize
     *            Maximum size for POST body. Use null to disable.
     */
    public RequestSizeLimits(final ReadableBytes maxHeaderSize, final ReadableBytes maxPostBodySize) {
        this(maxHeaderSize, maxPostBodySize, DEFAULT_MAX_POST_PROCESSED_SIZE, DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES, DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT);
    }

    /**
     * Creates a new RequestSizeLimits configuration.
     *
     * @param maxHeaderSize
     *            Maximum size for headers (request line + headers + GET parameters). Use null to disable.
     * @param maxPostBodySize
     *            Maximum size for POST body. Use null to disable.
     * @param maxPostProcessedSize
     *            Maximum size for processed POST data (after decompression, decryption, etc.). Use null to disable.
     */
    public RequestSizeLimits(final ReadableBytes maxHeaderSize, final ReadableBytes maxPostBodySize, final ReadableBytes maxPostProcessedSize) {
        this(maxHeaderSize, maxPostBodySize, maxPostProcessedSize, DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES, DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT);
    }

    /**
     * Creates a new RequestSizeLimits configuration.
     *
     * @param maxHeaderSize
     *            Maximum size for headers (request line + headers + GET parameters). Use null to disable.
     * @param maxPostBodySize
     *            Maximum size for POST body. Use null to disable.
     * @param maxPostProcessedSize
     *            Maximum size for processed POST data (after decompression, decryption, etc.). Use null to disable.
     * @param maxDrainInputStreamBytes
     *            Maximum bytes to drain from input stream on limit exceeded to prevent TCP RST. Use null to disable.
     */
    public RequestSizeLimits(final ReadableBytes maxHeaderSize, final ReadableBytes maxPostBodySize, final ReadableBytes maxPostProcessedSize, final ReadableBytes maxDrainInputStreamBytes) {
        this(maxHeaderSize, maxPostBodySize, maxPostProcessedSize, maxDrainInputStreamBytes, DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT);
    }

    /**
     * Creates a new RequestSizeLimits configuration.
     *
     * @param maxHeaderSize
     *            Maximum size for headers (request line + headers + GET parameters). Use null to disable.
     * @param maxPostBodySize
     *            Maximum size for POST body. Use null to disable.
     * @param maxPostProcessedSize
     *            Maximum size for processed POST data (after decompression, decryption, etc.). Use null to disable.
     * @param maxDrainInputStreamBytes
     *            Maximum bytes to drain from input stream on limit exceeded to prevent TCP RST. Use null to disable.
     * @param drainInputStreamTimeout
     *            Timeout for draining. Draining stops when this timeout is reached. Use null to disable timeout.
     */
    public RequestSizeLimits(final ReadableBytes maxHeaderSize, final ReadableBytes maxPostBodySize, final ReadableBytes maxPostProcessedSize, final ReadableBytes maxDrainInputStreamBytes, final TimeSpan drainInputStreamTimeout) {
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
     * Backwards-compatible constructor for int/long values. Converts primitive values to ReadableBytes. Use -1 or 0 to disable limits.
     *
     * @param maxHeaderSize
     *            Maximum size for headers in bytes. Use -1 or 0 to disable.
     * @param maxPostBodySize
     *            Maximum size for POST body in bytes. Use -1 or 0 to disable.
     * @param maxPostProcessedSize
     *            Maximum size for processed POST data in bytes. Use -1 or 0 to disable.
     */
    public RequestSizeLimits(final int maxHeaderSize, final long maxPostBodySize, final long maxPostProcessedSize) {
        this(maxHeaderSize > 0 ? ReadableBytes.fromBytes(maxHeaderSize) : null, maxPostBodySize > 0 ? ReadableBytes.fromBytes(maxPostBodySize) : null, maxPostProcessedSize > 0 ? ReadableBytes.fromBytes(maxPostProcessedSize) : null, DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES, DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT);
    }

    /**
     * Backwards-compatible constructor for int/long values. Converts primitive values to ReadableBytes. Use -1 or 0 to disable limits.
     *
     * @param maxHeaderSize
     *            Maximum size for headers in bytes. Use -1 or 0 to disable.
     * @param maxPostBodySize
     *            Maximum size for POST body in bytes. Use -1 or 0 to disable.
     */
    public RequestSizeLimits(final int maxHeaderSize, final long maxPostBodySize) {
        this(maxHeaderSize > 0 ? ReadableBytes.fromBytes(maxHeaderSize) : null, maxPostBodySize > 0 ? ReadableBytes.fromBytes(maxPostBodySize) : null, DEFAULT_MAX_POST_PROCESSED_SIZE, DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES, DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT);
    }

    /**
     * Returns the maximum allowed header size in bytes.
     *
     * @return Maximum header size in bytes, or 0 if disabled (null is treated as 0)
     */
    public long getMaxHeaderSize() {
        return maxHeaderSize != null ? maxHeaderSize.getBytes() : 0;
    }

    /**
     * Returns the maximum allowed POST body size in bytes.
     *
     * @return Maximum POST body size in bytes, or 0 if disabled (null is treated as 0)
     */
    public long getMaxPostBodySize() {
        return maxPostBodySize != null ? maxPostBodySize.getBytes() : 0;
    }

    /**
     * Checks if header size limit is enabled.
     *
     * @return true if header size limit is enabled (not null and > 0)
     */
    public boolean isHeaderSizeLimitEnabled() {
        return maxHeaderSize != null && maxHeaderSize.getBytes() > 0;
    }

    /**
     * Checks if POST body size limit is enabled.
     *
     * @return true if POST body size limit is enabled (not null and > 0)
     */
    public boolean isPostBodySizeLimitEnabled() {
        return maxPostBodySize != null && maxPostBodySize.getBytes() > 0;
    }

    /**
     * Returns the maximum allowed processed POST data size in bytes.
     *
     * @return Maximum processed POST data size in bytes, or 0 if disabled (null is treated as 0)
     */
    public long getMaxPostProcessedSize() {
        return maxPostProcessedSize != null ? maxPostProcessedSize.getBytes() : 0;
    }

    /**
     * Checks if processed POST data size limit is enabled.
     *
     * @return true if processed POST data size limit is enabled (not null and > 0)
     */
    public boolean isPostProcessedSizeLimitEnabled() {
        return maxPostProcessedSize != null && maxPostProcessedSize.getBytes() > 0;
    }

    /**
     * Returns the maximum bytes to drain from input stream on limit exceeded.
     *
     * @return Maximum bytes to drain, or 0 if draining is disabled (null is treated as 0)
     */
    public long getMaxDrainInputStreamBytes() {
        return maxDrainInputStreamBytes != null ? maxDrainInputStreamBytes.getBytes() : 0;
    }

    /**
     * Returns the timeout for draining input stream.
     *
     * @return Timeout for draining, or null if no timeout is configured
     */
    public TimeSpan getDrainInputStreamTimeout() {
        return drainInputStreamTimeout;
    }

    /**
     * Checks if input stream draining is enabled. Draining is enabled if timeout is not null OR maxDrainInputStreamBytes is not null and
     * > 0.
     *
     * @return true if draining is enabled
     */
    public boolean isDrainInputStreamEnabled() {
        return drainInputStreamTimeout != null || (maxDrainInputStreamBytes != null && maxDrainInputStreamBytes.getBytes() > 0);
    }

    /**
     * Returns a string representation of this request size limits configuration for debugging purposes.
     *
     * <p>
     * The format shows all configured limits in a readable format:
     * </p>
     *
     * <pre>
     * RequestSizeLimits[maxHeaderSize=8192, maxPostBodySize=1048576, maxPostProcessedSize=-1, maxDrainInputStreamBytes=10485760, drainInputStreamTimeout=5s]
     * </pre>
     *
     * @return A string representation of this request size limits configuration
     */
    @Override
    public String toString() {
        return "RequestSizeLimits[maxHeaderSize=" + this.maxHeaderSize + ", maxPostBodySize=" + this.maxPostBodySize + ", maxPostProcessedSize=" + this.maxPostProcessedSize + ", maxDrainInputStreamBytes=" + this.maxDrainInputStreamBytes + ", drainInputStreamTimeout=" + this.drainInputStreamTimeout + "]";
    }
}
