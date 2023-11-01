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
package org.appwork.storage.config;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

/**
 * @author Daniel Wilhelm
 * 
 */
public class WeakHashSet<E> extends AbstractSet<E> implements Set<E> {

    protected final ReferenceQueue<Object>                          queue = new ReferenceQueue<Object>();
    protected final HashMap<WeakHashSetElement, WeakHashSetElement> map   = new HashMap<WeakHashSetElement, WeakHashSetElement>();

    @Override
    public boolean contains(Object o) {
        return this.map.containsKey(WeakHashSetElement.create(o));
    }

    @Override
    public boolean add(E e) {
        this.cleanUp();
        WeakHashSetElement item = WeakHashSetElement.create(e, this.queue);
        return this.map.put(item, item) == null;
    }

    @SuppressWarnings("unchecked")
    public E getDuplicateOrAdd(E e) {
        WeakHashSetElement item = WeakHashSetElement.create(e);
        WeakHashSetElement exists = this.map.get(item);
        Object ret = null;
        if (exists != null && (ret = exists.get()) != null) { return (E) ret; }
        this.cleanUp();
        this.map.put(item, item);
        return e;
    }

    @Override
    public boolean remove(Object o) {
        WeakHashSetElement removeItem = WeakHashSetElement.create(o);
        WeakHashSetElement removedItem = this.map.remove(removeItem);
        this.cleanUp();
        if (removedItem == null) { return false; }
        return removedItem.equals(removeItem);
    }

    private void cleanUp() {
        Object item = null;
        while ((item = this.queue.poll()) != null) {
            this.map.remove(item);
        }
    }

    static private class WeakHashSetElement extends WeakReference<Object> {

        private int weakHashSetElementHash = -1;

        private WeakHashSetElement(Object o) {
            super(o);
            this.weakHashSetElementHash = o.hashCode();
        }

        private WeakHashSetElement(Object o, ReferenceQueue<Object> q) {
            super(o, q);
            this.weakHashSetElementHash = o.hashCode();
        }

        private static WeakHashSetElement create(Object o) {
            return o == null ? null : new WeakHashSetElement(o);
        }

        private static WeakHashSetElement create(Object o, ReferenceQueue<Object> q) {
            return o == null ? null : new WeakHashSetElement(o, q);
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) { return true; }
            if (!(o instanceof WeakHashSetElement)) { return false; }
            Object t = this.get();
            Object u = ((WeakHashSetElement) o).get();
            if (t == u) { return true; }
            if (t == null || u == null) { return false; }
            return t.equals(u);
        }

        @Override
        public int hashCode() {
            return this.weakHashSetElementHash;
        }

    }

    @Override
    public Iterator<E> iterator() {
        this.cleanUp();
        final Iterator<WeakHashSetElement> i = this.map.keySet().iterator();

        return new Iterator<E>() {
            public boolean hasNext() {
                return i.hasNext();
            }

            public E next() {
                return (E) i.next().get();
            }

            public void remove() {
                i.remove();
            }
        };
    }

    @Override
    public int size() {
        return this.map.size();
    }

    public static void main(String[] args) {
        java.util.List<String> strong = new ArrayList<String>();
        WeakHashSet<String> test = new WeakHashSet<String>();
        {
            for (int i = 1; i < 1000000; i++) {
                String strongItem = "" + i;
                strong.add(strongItem);
                test.add(strongItem);
                System.out.println(test.size());
            }
        }

    }
}
