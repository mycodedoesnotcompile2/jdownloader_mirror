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
 * Configuration class for request size limits used by HTTP server implementations.
 * 
 * <p>
 * This class provides configurable limits for various aspects of HTTP request processing:
 * </p>
 * <ul>
 * <li><b>Header size:</b> Maximum size for request headers including request line, headers, and GET parameters</li>
 * <li><b>POST body size:</b> Maximum size for raw POST body data</li>
 * <li><b>Processed POST size:</b> Maximum size for processed POST data after decompression, decryption, etc.</li>
 * <li><b>Input stream draining:</b> Configuration for draining excess data from input stream when limits are exceeded to prevent TCP RST</li>
 * </ul>
 * 
 * <p>
 * <b>Default Values:</b>
 * </p>
 * <ul>
 * <li>Maximum header size: <b>4 KB</b> ({@value #DEFAULT_MAX_HEADER_SIZE} bytes)</li>
 * <li>Maximum POST body size: <b>1 MB</b> ({@value #DEFAULT_MAX_POST_BODY_SIZE} bytes)</li>
 * <li>Maximum processed POST size: <b>2 MB</b> ({@value #DEFAULT_MAX_POST_PROCESSED_SIZE} bytes)</li>
 * <li>Maximum drain bytes: <b>10 MB</b> ({@value #DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES} bytes)</li>
 * <li>Drain timeout: <b>5 seconds</b> ({@value #DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT} ms)</li>
 * </ul>
 * 
 * <p>
 * All limits can be disabled by setting them to -1. When a limit is disabled, no size checking is performed for that aspect.
 * </p>
 * 
 * <p>
 * Use the default constructor {@link #RequestSizeLimits()} to create an instance with default values, or use one of the
 * parameterized constructors to customize specific limits.
 * </p>
 *
 * @author AppWork
 */
public class RequestSizeLimits {
    /**
     * Maximum allowed size for request headers including request line, headers, and GET parameters. Set to -1 to disable this limit.
     */
    private long maxHeaderSize;

    /**
     * @param maxHeaderSize
     *            the maxHeaderSize to set
     */
    public void setMaxHeaderSize(long maxHeaderSize) {
        this.maxHeaderSize = maxHeaderSize;
    }

    /**
     * @param maxPostBodySize
     *            the maxPostBodySize to set
     */
    public void setMaxPostBodySize(long maxPostBodySize) {
        this.maxPostBodySize = maxPostBodySize;
    }

    /**
     * @param maxPostProcessedSize
     *            the maxPostProcessedSize to set
     */
    public void setMaxPostProcessedSize(long maxPostProcessedSize) {
        this.maxPostProcessedSize = maxPostProcessedSize;
    }

    /**
     * @param maxDrainInputStreamBytes
     *            the maxDrainInputStreamBytes to set
     */
    public void setMaxDrainInputStreamBytes(long maxDrainInputStreamBytes) {
        this.maxDrainInputStreamBytes = maxDrainInputStreamBytes;
    }

    /**
     * @param drainInputStreamTimeout
     *            the drainInputStreamTimeout to set
     */
    public void setDrainInputStreamTimeout(long drainInputStreamTimeout) {
        this.drainInputStreamTimeout = drainInputStreamTimeout;
    }

    /**
     * Maximum allowed size for POST body. Set to -1 to disable this limit.
     */
    private long             maxPostBodySize;
    /**
     * Maximum allowed size for processed POST data (after decompression, decryption, etc.). Set to -1 to disable this limit.
     */
    private long             maxPostProcessedSize;
    /**
     * Maximum bytes to drain from input stream on limit exceeded to prevent TCP RST. Set to -1 to disable draining.
     */
    private long             maxDrainInputStreamBytes;
    /**
     * Timeout for draining input stream in milliseconds. Draining stops when timeout is reached or maxDrainInputStreamBytes is reached. Set
     * to -1 to disable timeout (draining only limited by maxDrainInputStreamBytes).
     */
    private long             drainInputStreamTimeout;
    /** 
     * Default maximum header size: 4 KB (4096 bytes).
     * Includes request line, headers, and GET parameters.
     */
    public static final long DEFAULT_MAX_HEADER_SIZE              = 4 * 1024L;
    
    /** 
     * Default maximum POST body size: 1 MB (1048576 bytes).
     * This limit applies to the raw POST body data before any processing.
     */
    public static final long DEFAULT_MAX_POST_BODY_SIZE           = 1 * 1024 * 1024L;
    
    /** 
     * Default maximum processed POST data size: 2 MB (2097152 bytes).
     * This limit applies after decompression, decryption, or other processing operations.
     * Defaults to 2x the POST body size limit.
     */
    public static final long DEFAULT_MAX_POST_PROCESSED_SIZE      = DEFAULT_MAX_POST_BODY_SIZE * 2;
    
    /** 
     * Default maximum bytes to drain from input stream: 10 MB (10485760 bytes).
     * When a size limit is exceeded, this many bytes will be drained from the input stream
     * to prevent TCP RST errors. Set to -1 to disable draining.
     */
    public static final long DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES = DEFAULT_MAX_POST_BODY_SIZE * 10;
    
    /** 
     * Default timeout for draining input stream: 5 seconds (5000 ms).
     * Draining stops when this timeout is reached or when maxDrainInputStreamBytes is reached,
     * whichever comes first. Set to -1 to disable timeout (draining only limited by maxDrainInputStreamBytes).
     */
    public static final long DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT   = 5000L;

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
     * 
     * <p>
     * Default values are:
     * </p>
     * <ul>
     * <li>Maximum header size: {@value #DEFAULT_MAX_HEADER_SIZE} bytes (4 KB)</li>
     * <li>Maximum POST body size: {@value #DEFAULT_MAX_POST_BODY_SIZE} bytes (1 MB)</li>
     * <li>Maximum processed POST size: {@value #DEFAULT_MAX_POST_PROCESSED_SIZE} bytes (2 MB)</li>
     * <li>Maximum drain bytes: {@value #DEFAULT_MAX_DRAIN_INPUT_STREAM_BYTES} bytes (10 MB)</li>
     * <li>Drain timeout: {@value #DEFAULT_DRAIN_INPUT_STREAM_TIMEOUT} ms (5 seconds)</li>
     * </ul>
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
