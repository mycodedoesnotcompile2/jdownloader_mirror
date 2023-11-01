package org.appwork.utils;

public class BinaryUtils {
    /**
     * Merges all arrays into a single one
     *
     * @param prefix
     * @param ret
     * @return
     */
    public static byte[] mergeArrays(final byte[]... parts) {
        int index = 0;
        for (final byte[] b : parts) {
            index += b.length;
        }
        final byte[] ret = new byte[index];
        index = 0;
        for (final byte[] part : parts) {
            System.arraycopy(part, 0, ret, index, part.length);
            index += part.length;
        }
        return ret;
    }

    /**
     *
     * @throws IndexOutOfBoundsException
     * @return true if a and b match for the first <length> bytes starting at <offsetA/offsetB>
     */
    public static boolean matches(final byte[] a, final byte[] b, final int offsetA, final int offsetB, final int length) {
        for (int i = offsetA; i < offsetA + length; i++) {
            if (a[i] != b[i - offsetA + offsetB]) {
                return false;
            }
        }
        return true;
    }
}
