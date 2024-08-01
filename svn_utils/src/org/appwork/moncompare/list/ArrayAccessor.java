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
package org.appwork.moncompare.list;

import java.lang.reflect.Array;
import java.util.Iterator;

import org.appwork.exceptions.NotSupportedException;

/**
 *
 *
 * @author thomas
 * @date 14.10.2023
 *
 */
public class ArrayAccessor<MatcherType> extends ListAccessorInterface {
    final private Object array;
    final private int    length;

    /**
     * @param expression
     */
    public ArrayAccessor(Object expression) {
        array = expression;
        length = Array.getLength(array);
    }

    @Override
    public Iterator<Object> iterator() {
        return new Iterator<Object>() {
            private int index;
            {
                index = 0;
            }

            @Override
            public boolean hasNext() {
                return length > index;
            }

            @Override
            public Object next() {
                return get(index++);
            }
        };
    }

    /**
     * @see org.appwork.moncompare.list.ListAccessorInterface#size()
     */
    @Override
    public int size() {
        return length;
    }

    /**
     * @see org.appwork.moncompare.list.ListAccessorInterface#get(int)
     */
    @Override
    public Object get(int arrayIndex) {
        return Array.get(array, arrayIndex);
    }

    /**
     * @see org.appwork.moncompare.list.ListAccessorInterface#set(int, java.lang.Object)
     */
    @Override
    public void set(int arrayIndex, Object value) {
        if (arrayIndex >= size()) {
            throw new IndexOutOfBoundsException("size:" + size() + "|index:" + arrayIndex);
        } else {
            Array.set(array, arrayIndex, value);
        }
    }

    /**
     * @see org.appwork.moncompare.list.ListAccessorInterface#remove(int)
     */
    @Override
    public Object remove(int index) {
        throw new NotSupportedException("Cannot remove an entry from an array");
    }

    /**
     * @see org.appwork.moncompare.list.ListAccessorInterface#add(java.lang.Object)
     */
    @Override
    public void add(Object value) {
        throw new NotSupportedException("Cannot add an entry to an array");
    }
}