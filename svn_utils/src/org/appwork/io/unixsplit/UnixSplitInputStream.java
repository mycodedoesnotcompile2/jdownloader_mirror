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
package org.appwork.io.unixsplit;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

/**
 * @author daniel
 * @date Jul 15, 2021
 *
 */
public class UnixSplitInputStream extends InputStream {

    private static final String SUFFIX_PATTERN = "^(.+)\\.([a-z]{2,})$";

    protected final File        baseFile;
    protected final int         suffixLength;

    protected int               segmentIndex   = 0;
    protected InputStream       segment        = null;
    protected boolean           eof            = false;

    public UnixSplitInputStream(final File file) throws IOException {
        final String fileName = file.getName();
        if (fileName.matches(SUFFIX_PATTERN)) {
            final String suffix = fileName.replaceFirst(SUFFIX_PATTERN, "$2");
            segmentIndex = parseSegment(suffix);
            suffixLength = suffix.length();
        } else {
            throw new IOException("could not auto detect suffix:" + fileName);
        }
        final String filePath = file.getAbsolutePath();
        baseFile = new File(filePath.substring(0, filePath.length() - suffixLength));
        openNextSegment();
    }

    public UnixSplitInputStream(final File baseFile, final int segmentIndex, final int suffixLength) throws IOException {
        this.baseFile = baseFile;
        this.suffixLength = suffixLength;
        this.segmentIndex = segmentIndex;
        openNextSegment();
    }

    @Override
    public int available() throws IOException {
        if (eof) {
            return 0;
        } else {
            return segment.available();
        }
    }

    @Override
    public int read() throws IOException {
        while (true) {
            if (eof) {
                return -1;
            } else {
                final int ret = segment.read();
                if (ret == -1) {
                    if (!openNextSegment()) {
                        return -1;
                    } else {
                        continue;
                    }
                } else {
                    return ret;
                }
            }
        }
    }

    protected boolean openNextSegment() throws IOException {
        if (!eof) {
            if (segment != null) {
                segment.close();
            }
            final String suffix;
            try {
                suffix = buildSegment(segmentIndex, suffixLength);
            } catch (IllegalArgumentException e) {
                eof = true;
                if (throwNextSegmentException(e, segmentIndex)) {
                    throw e;
                } else {
                    return false;
                }
            }
            try {
                segment = openInputStream(new File(baseFile + suffix));
                segmentIndex++;
                return true;
            } catch (FileNotFoundException e) {
                eof = true;
                if (throwNextSegmentException(e, segmentIndex)) {
                    throw e;
                } else {
                    return false;
                }
            }
        } else {
            return false;
        }
    }

    protected boolean throwNextSegmentException(Exception e, int segmentIndex) {
        return segment == null;
    }

    /**
     *
     * @param file
     * @return
     * @throws IOException
     * @throws FileNotFoundException
     *             to signal end of unix split archive (no next segment available)
     */
    protected InputStream openInputStream(final File file) throws IOException {
        return new FileInputStream(file);
    }

    @Override
    public void close() throws IOException {
        try {
            if (segment != null) {
                segment.close();
            }
        } finally {
            eof = true;
            segment = null;
        }
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException {
        while (true) {
            if (eof) {
                return -1;
            } else {
                final int ret = segment.read(b, off, len);
                if (ret == -1) {
                    if (!openNextSegment()) {
                        return -1;
                    } else {
                        continue;
                    }
                } else {
                    return ret;
                }
            }
        }
    }

    public static int parseSegment(final String segment) {
        final int length = segment.length();
        int ret = 0;
        for (int i = 0; i < length; i++) {
            final int x = segment.charAt(i) - 'a';
            ret += x * Math.pow(26, length - 1 - i);
        }
        return ret;
    }

    public static String buildSegment(int value, final int suffixLength) throws IllegalArgumentException {
        final int max = (int) Math.pow(26, suffixLength);
        if (value > max) {
            throw new IllegalArgumentException("value:" + value + " is too large for suffixLength:" + suffixLength + "(max:" + max + ")");
        } else {
            final char[] ret = new char[suffixLength];
            for (int index = 0; index < suffixLength; index++) {
                final int pos = (int) Math.pow(26, suffixLength - 1 - index);
                final int remaining = value / pos;
                value = value % pos;
                ret[index] = (char) ('a' + remaining);
            }
            return new String(ret);
        }
    }

}
