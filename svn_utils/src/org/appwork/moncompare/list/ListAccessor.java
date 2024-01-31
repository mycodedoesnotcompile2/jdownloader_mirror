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

import java.util.Iterator;
import java.util.List;

import org.appwork.moncompare.Condition;

/**
 * @author thomas
 * @date 14.10.2023
 *
 */
public class ListAccessor extends ListAccessorInterface {
    private final List<Object> list;

    /**
     * @param expression
     */
    public ListAccessor(List<Object> expression) {
        this.list = expression;
    }

    /**
     * @see java.lang.Iterable#iterator()
     */
    @Override
    public Iterator<Object> iterator() {
        return list.iterator();
    }

    /**
     * @see org.appwork.moncompare.list.ListAccessorInterface#size()
     */
    @Override
    public int size() {
        return list.size();
    }

    /**
     * @see org.appwork.moncompare.list.ListAccessorInterface#get(int)
     */
    @Override
    public Object get(int i) {
        return list.get(i);
    }

    /**
     * @see org.appwork.moncompare.list.ListAccessorInterface#set(int, java.lang.Object)
     */
    @Override
    public void set(int index, Object value) {
        if (index == size()) {
            list.add(value);
        } else if (index > size()) {
            throw new IndexOutOfBoundsException();
        } else {
            list.set(index, value);
        }
    }

    /**
     * @see org.appwork.moncompare.list.ListAccessorInterface#remove(int)
     */
    @Override
    public Object remove(int index) {
        try {
            return list.remove(index);
        } catch (IndexOutOfBoundsException e) {
            return Condition.KEY_DOES_NOT_EXIST;
        }
    }

    /**
     * @see org.appwork.moncompare.list.ListAccessorInterface#add(java.lang.Object)
     */
    @Override
    public void add(Object value) {
        list.add(value);
    }
}
