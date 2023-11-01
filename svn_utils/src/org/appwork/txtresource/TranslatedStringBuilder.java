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
package org.appwork.txtresource;

import java.util.Arrays;

/**
 * @author daniel
 * @date May 19, 2020
 *
 */
public class TranslatedStringBuilder {

    protected char[] value = null;
    protected int    count = 0;

    protected TranslatedStringBuilder(int capacity) {
        value = new char[capacity];
    }

    private TranslatedStringBuilder appendNull() {
        int c = count;
        ensureCapacityInternal(c + 4);
        final char[] value = this.value;
        value[c++] = 'n';
        value[c++] = 'u';
        value[c++] = 'l';
        value[c++] = 'l';
        count = c;
        return this;
    }

    public TranslatedStringBuilder append(Object obj) {
        if (obj == null) {
            return appendNull();
        } else {
            return append(obj.toString());
        }
    }

    public TranslatedStringBuilder append(String str) {
        if (str == null) {
            return appendNull();
        } else {
            return append(str, 0, str.length());
        }
    }

    public int length() {
        return count;
    }

    public int capacity() {
        return value.length;
    }

    public void clear() {
        count = 0;
    }

    public TranslatedStringBuilder append(char[] str) {
        if (str == null) {
            return appendNull();
        } else {
            int len = str.length;
            ensureCapacityInternal(count + len);
            System.arraycopy(str, 0, value, count, len);
            count += len;
            return this;
        }
    }

    public String toString() {
        return new String(value, 0, count);
    }

    public TranslatedStringBuilder append(String str, int from, int to) {
        if (str == null) {
            return appendNull();
        } else {
            int len = to - from;
            ensureCapacityInternal(count + len);
            str.getChars(from, to, value, count);
            count += len;
            return this;
        }
    }

    private static final int MAX_ARRAY_SIZE = Integer.MAX_VALUE - 8;

    private void ensureCapacityInternal(int minimumCapacity) {
        // overflow-conscious code
        if (minimumCapacity - value.length > 0) {
            value = Arrays.copyOf(value, newCapacity(minimumCapacity));
        }
    }

    private int newCapacity(int minCapacity) {
        // overflow-conscious code
        int newCapacity = (value.length << 1) + 2;
        if (newCapacity - minCapacity < 0) {
            newCapacity = minCapacity;
        }
        return (newCapacity <= 0 || MAX_ARRAY_SIZE - newCapacity < 0) ? hugeCapacity(minCapacity) : newCapacity;
    }

    private int hugeCapacity(int minCapacity) {
        if (Integer.MAX_VALUE - minCapacity < 0) { // overflow
            throw new OutOfMemoryError();
        }
        return (minCapacity > MAX_ARRAY_SIZE) ? minCapacity : MAX_ARRAY_SIZE;
    }
}
