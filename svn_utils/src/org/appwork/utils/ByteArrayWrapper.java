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
package org.appwork.utils;

import java.nio.charset.Charset;

/**
 * @author daniel
 *
 */
public final class ByteArrayWrapper {
    private final byte[] byteArray;
    private final int    offset;
    private final int    length;
    private final int    hashCode;

    public ByteArrayWrapper(final byte[] byteArray, final boolean clone) {
        this(byteArray, 0, byteArray.length, clone);
    }

    public ByteArrayWrapper(final byte[] byteArray, final int offset, final int length, final boolean clone) {
        this(byteArray, offset, length, clone, null);
    }

    public ByteArrayWrapper(final byte[] byteArray, final int offset, final int length, final boolean clone, final Integer hashCode) {
        if (clone) {
            this.byteArray = new byte[length];
            System.arraycopy(byteArray, offset, this.byteArray, 0, length);
            this.offset = 0;
            this.length = byteArray.length;
        } else {
            this.byteArray = byteArray;
            this.offset = offset;
            this.length = length;
        }
        if (hashCode != null) {
            this.hashCode = hashCode.intValue();
        } else {
            this.hashCode = this.calcHashCode(this.byteArray, this.offset, this.length);
        }
    }

    private final int calcHashCode(final byte[] byteArray, final int offset, final int length) {
        int result = 1;
        for (int index = offset; index < length; index++) {
            result = 31 * result + byteArray[index];
        }
        return result;
    }

    @Override
    public final boolean equals(final Object obj) {
        if (obj == null || !(obj instanceof ByteArrayWrapper) || this.hashCode() != obj.hashCode()) {
            return false;
        } else if (obj == this) {
            return true;
        } else {
            final ByteArrayWrapper other = (ByteArrayWrapper) obj;
            if (other.getLength() != this.getLength()) {
                return false;
            } else {
                int index2 = other.getOffset();
                for (int index = this.getOffset(); index < this.getLength(); index++) {
                    if (this.getByteArray()[index] != other.getByteArray()[index2]) {
                        return false;
                    }
                    index2++;
                }
                return true;
            }
        }
    }

    public final byte[] getByteArray() {
        return this.byteArray;
    }

    public final int getLength() {
        return this.length;
    }

    public final int getOffset() {
        return this.offset;
    }

    @Override
    public final int hashCode() {
        return this.hashCode;
    }

    @Override
    public String toString() {
        return toString(null);
    }

    public String toString(final Charset charset) {
        if (charset == null) {
            return new String(this.byteArray, this.offset, this.length);
        } else {
            return new String(this.byteArray, this.offset, this.length, charset);
        }
    }
}
