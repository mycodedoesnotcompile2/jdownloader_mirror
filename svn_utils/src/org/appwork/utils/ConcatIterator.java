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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

/**
 * @author daniel
 *
 */
public class ConcatIterator<E> implements Iterator<E>, Iterable<E> {
    private final List<Iterator<? extends E>> iterators;
    private int                               iteratorIndex = 0;
    private E                                 next          = null;
    private boolean                           nextSet       = false;

    public ConcatIterator() {
        this.iterators = new ArrayList<Iterator<? extends E>>();
    }

    public ConcatIterator(final Iterator<? extends E>... iterators) {
        this.iterators = new ArrayList<Iterator<? extends E>>(Arrays.asList(iterators));
    }

    public ConcatIterator(final Iterable<? extends E>... iterables) {
        final List<Iterator<? extends E>> ret = new ArrayList<Iterator<? extends E>>();
        for (final Iterable<? extends E> it : iterables) {
            if (it == null) {
                continue;
            }
            ret.add(it.iterator());
        }
        this.iterators = ret;
    }

    @Override
    public boolean hasNext() {
        if (this.nextSet) {
            return true;
        }
        while (this.iteratorIndex < this.iterators.size()) {
            final Iterator<? extends E> nextIt = this.iterators.get(this.iteratorIndex);
            if (nextIt == null && this.isIgnoreNull()) {
                this.iteratorIndex++;
                continue;
            }
            if (nextIt == null) {
                throw new NullPointerException("set #IgnoreNull to ignore null entries");
            }
            if (nextIt.hasNext()) {
                this.next = this.iterators.get(this.iteratorIndex).next();
                this.nextSet = true;
                return true;
            }
            this.iteratorIndex++;
        }
        return false;
    }

    private boolean ignoreNull = false;

    public boolean isIgnoreNull() {
        return this.ignoreNull;
    }

    public ConcatIterator<E> ignoreNull(final boolean ignoreNull) {
        this.setIgnoreNull(ignoreNull);
        return this;
    }

    public void setIgnoreNull(final boolean ignoreNull) {
        this.ignoreNull = ignoreNull;
    }

    @Override
    public Iterator<E> iterator() {
        return this;
    }

    @Override
    public E next() {
        if (this.nextSet) {
            final E ret = this.next;
            this.next = null;
            this.nextSet = false;
            return ret;
        }
        if (this.hasNext()) {
            final E ret = this.next;
            this.next = null;
            this.nextSet = false;
            return ret;
        } else {
            return null;
        }
    }

    @Override
    public void remove() {
    }

    public void add(final Iterator<E> iterator) {
        this.iterators.add(iterator);
    }
}
