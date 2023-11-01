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
package org.appwork.utils.net.httpconnection;

import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * @author daniel
 *
 */
public class HTTPHeaderMap<V> implements Map<String, V> {
    private final class HTTPHeaderKeySet extends AbstractSet<String> {
        @Override
        public void clear() {
            HTTPHeaderMap.this.clear();
        }

        @Override
        public boolean contains(final Object o) {
            return HTTPHeaderMap.this.containsKey(o);
        }

        @Override
        public Iterator<String> iterator() {
            final Iterator<HTTPHeaderMapEntry> it = HTTPHeaderMap.this.internalMap.keySet().iterator();
            return new Iterator<String>() {
                @Override
                public boolean hasNext() {
                    return it.hasNext();
                }

                @Override
                public String next() {
                    final HTTPHeaderMapEntry next = it.next();
                    if (next != null) {
                        return next.getHttpHeaderFieldName();
                    }
                    return null;
                }

                @Override
                public void remove() {
                    it.remove();
                }
            };
        }

        @Override
        public boolean remove(final Object o) {
            return HTTPHeaderMap.this.containsKey(o);
        }

        @Override
        public int size() {
            return HTTPHeaderMap.this.size();
        }
    }

    protected static class HTTPHeaderMapEntry {
        private final String httpHeaderFieldName;
        private final int    lowerCaseHash;

        protected HTTPHeaderMapEntry(final String httpHeaderFieldName) {
            this.httpHeaderFieldName = httpHeaderFieldName;
            this.lowerCaseHash = httpHeaderFieldName.toLowerCase(Locale.ENGLISH).hashCode();
        }

        @Override
        public boolean equals(final Object obj) {
            if (obj == null) {
                return false;
            }
            if (obj == this) {
                return true;
            }
            if (!(obj instanceof HTTPHeaderMapEntry)) {
                return false;
            }
            final HTTPHeaderMapEntry other = (HTTPHeaderMapEntry) obj;
            return other.getHttpHeaderFieldName().equalsIgnoreCase(this.getHttpHeaderFieldName());
        }

        public String getHttpHeaderFieldName() {
            return this.httpHeaderFieldName;
        }

        @Override
        public int hashCode() {
            return this.lowerCaseHash;
        }

        @Override
        public String toString() {
            return this.httpHeaderFieldName;
        }
    }

    public static void main(final String[] args) {
        final Map<String, String> map = new HTTPHeaderMap<String>();
        map.put("Abc", "abc");
        map.put("xyz", "xyz");
        map.put("xyz1", "xyz");
        map.put("ABC", "ABC");
        map.put(null, "BC");
        System.out.println(map);
        map.remove("AbC");
        System.out.println(map);
        map.put("ABC", "ABC");
        map.put(null, "Bs");
        System.out.println(map);
        System.out.println(map.get("aBc"));
        System.out.println(map.get("xyZ"));
        System.out.println(map.get(""));
        System.out.println(map.get(null));
        System.out.println(map.values());
        System.out.println(map.keySet());
        final Iterator<Entry<String, String>> it = map.entrySet().iterator();
        Entry<String, String> next = null;
        final ArrayList<Entry<String, String>> retain = new ArrayList<Map.Entry<String, String>>();
        while (it.hasNext()) {
            next = it.next();
            System.out.println(next.getKey() + " = " + next.getValue());
        }
        System.out.println("next round");
        System.out.println(map);
        retain.add(next);
        System.out.println(new ArrayList<String>(map.values()));
        System.out.println(new ArrayList<String>(map.keySet()));
        System.out.println(new ArrayList<Entry<String, String>>(map.entrySet()));
        System.out.println("entry " + map.entrySet().retainAll(retain));
        System.out.println(map);
    }

    protected final LinkedHashMap<HTTPHeaderMapEntry, V> internalMap;

    public HTTPHeaderMap() {
        this.internalMap = new LinkedHashMap<HTTPHeaderMapEntry, V>();
    }

    public HTTPHeaderMap(final int initialCapacity) {
        this.internalMap = new LinkedHashMap<HTTPHeaderMapEntry, V>(initialCapacity);
    }

    public HTTPHeaderMap(final int initialCapacity, final float loadFactor) {
        this.internalMap = new LinkedHashMap<HTTPHeaderMapEntry, V>(initialCapacity, loadFactor);
    }

    @Override
    public void clear() {
        this.internalMap.clear();
    }

    @Override
    public boolean containsKey(final Object key) {
        if (key != null && key instanceof String) {
            return this.internalMap.containsKey(new HTTPHeaderMapEntry((String) key));
        }
        return false;
    }

    @Override
    public boolean containsValue(final Object value) {
        V casted = (V) value;
        return this.internalMap.containsValue(casted);
    }

    private Entry<String, V> convertFromEntry(final Map.Entry<HTTPHeaderMapEntry, V> e) {
        final String key;
        if (e.getKey() == null) {
            key = null;
        } else {
            key = e.getKey().getHttpHeaderFieldName();
        }
        final Entry<String, V> entry = new Map.Entry<String, V>() {
            V value = e.getValue();

            @Override
            public final boolean equals(final Object o) {
                if (!(o instanceof Map.Entry)) {
                    return false;
                }
                final Map.Entry e = (Map.Entry) o;
                final Object k1 = this.getKey();
                final Object k2 = e.getKey();
                if (k1 == k2 || k1 != null && k1.equals(k2)) {
                    final Object v1 = this.getValue();
                    final Object v2 = e.getValue();
                    if (v1 == v2 || v1 != null && v1.equals(v2)) {
                        return true;
                    }
                }
                return false;
            }

            @Override
            public String getKey() {
                return key;
            }

            @Override
            public V getValue() {
                return this.value;
            }

            @Override
            public final int hashCode() {
                return hashCode(this.getKey()) ^ hashCode(this.getValue());
            }

            private int hashCode(Object o) {
                return o != null ? o.hashCode() : 0;
            }

            @Override
            public V setValue(final V value) {
                final V old = value;
                this.value = value;
                return old;
            }

            @Override
            public final String toString() {
                return this.getKey() + "=" + this.getValue();
            }
        };
        return entry;
    }

    private Entry<HTTPHeaderMapEntry, V> convertToEntry(final Map.Entry<String, V> e) {
        final HTTPHeaderMapEntry key;
        if (e.getKey() == null) {
            key = null;
        } else {
            key = new HTTPHeaderMapEntry(e.getKey());
        }
        final Entry<HTTPHeaderMapEntry, V> entry = new Map.Entry<HTTPHeaderMapEntry, V>() {
            V value = e.getValue();

            @Override
            public final boolean equals(final Object o) {
                if (!(o instanceof Map.Entry)) {
                    return false;
                }
                final Map.Entry e = (Map.Entry) o;
                final Object k1 = this.getKey();
                final Object k2 = e.getKey();
                if (k1 == k2 || k1 != null && k1.equals(k2)) {
                    final Object v1 = this.getValue();
                    final Object v2 = e.getValue();
                    if (v1 == v2 || v1 != null && v1.equals(v2)) {
                        return true;
                    }
                }
                return false;
            }

            @Override
            public HTTPHeaderMapEntry getKey() {
                return key;
            }

            @Override
            public V getValue() {
                return this.value;
            }

            @Override
            public final int hashCode() {
                return hashCode(this.getKey()) ^ hashCode(this.getValue());
            }

            private int hashCode(Object o) {
                return o != null ? o.hashCode() : 0;
            }

            @Override
            public V setValue(final V value) {
                final V old = value;
                this.value = value;
                return old;
            }

            @Override
            public final String toString() {
                return this.getKey() + "=" + this.getValue();
            }
        };
        return entry;
    }

    @Override
    public Set<java.util.Map.Entry<String, V>> entrySet() {
        final Set<Entry<HTTPHeaderMapEntry, V>> set = this.internalMap.entrySet();
        return new Set<Map.Entry<String, V>>() {
            @Override
            public boolean add(final java.util.Map.Entry<String, V> e) {
                return set.add(HTTPHeaderMap.this.convertToEntry(e));
            }

            @Override
            public boolean addAll(final Collection<? extends java.util.Map.Entry<String, V>> c) {
                boolean changed = false;
                for (final Map.Entry<String, V> entry : c) {
                    if (this.add(entry)) {
                        changed = true;
                    }
                }
                return changed;
            }

            @Override
            public void clear() {
                HTTPHeaderMap.this.internalMap.clear();
            }

            @Override
            public boolean contains(final Object o) {
                return this.get(o) != null;
            }

            @Override
            public boolean containsAll(final Collection<?> c) {
                for (final Object o : c) {
                    if (!this.contains(o)) {
                        return false;
                    }
                }
                return true;
            }

            @Override
            public boolean equals(final Object o) {
                if (o == this) {
                    return true;
                }
                if (!(o instanceof Set)) {
                    return false;
                }
                final Collection c = (Collection) o;
                if (c.size() != this.size()) {
                    return false;
                }
                try {
                    return this.containsAll(c);
                } catch (final ClassCastException unused) {
                    return false;
                } catch (final NullPointerException unused) {
                    return false;
                }
            }

            private Entry<HTTPHeaderMapEntry, V> get(final Object o) {
                if (!(o instanceof Map.Entry)) {
                    return null;
                }
                final Map.Entry<?, V> e = (Map.Entry<?, V>) o;
                if (e.getKey() != null && !(e.getKey() instanceof String)) {
                    return null;
                }
                final Iterator<Entry<HTTPHeaderMapEntry, V>> it = HTTPHeaderMap.this.internalMap.entrySet().iterator();
                while (it.hasNext()) {
                    final Entry<HTTPHeaderMapEntry, V> next = it.next();
                    final HTTPHeaderMapEntry key = next.getKey();
                    if (key == null && e.getKey() == null || key.getHttpHeaderFieldName().equalsIgnoreCase((String) e.getKey())) {
                        if (next.getValue() == e.getKey() || next.getValue() != null && next.getValue().equals(e.getValue())) {
                            return next;
                        }
                    }
                }
                return null;
            }

            @Override
            public int hashCode() {
                int h = 0;
                final Iterator<?> i = this.iterator();
                while (i.hasNext()) {
                    final Object obj = i.next();
                    if (obj != null) {
                        h += obj.hashCode();
                    }
                }
                return h;
            }

            @Override
            public boolean isEmpty() {
                return set.isEmpty();
            }

            @Override
            public Iterator<java.util.Map.Entry<String, V>> iterator() {
                final Iterator<Entry<HTTPHeaderMapEntry, V>> it = HTTPHeaderMap.this.internalMap.entrySet().iterator();
                return new Iterator<Map.Entry<String, V>>() {
                    @Override
                    public boolean hasNext() {
                        return it.hasNext();
                    }

                    @Override
                    public Map.Entry<String, V> next() {
                        final Entry<HTTPHeaderMapEntry, V> next = it.next();
                        if (next != null) {
                            return HTTPHeaderMap.this.convertFromEntry(next);
                        }
                        return null;
                    }

                    @Override
                    public void remove() {
                        it.remove();
                    }
                };
            }

            @Override
            public boolean remove(final Object o) {
                final Entry<HTTPHeaderMapEntry, V> entry = this.get(o);
                if (entry != null) {
                    return HTTPHeaderMap.this.internalMap.entrySet().remove(entry);
                }
                return false;
            }

            @Override
            public boolean removeAll(final Collection<?> c) {
                boolean changed = false;
                for (final Object o : c) {
                    if (this.remove(o)) {
                        changed = true;
                    }
                }
                return changed;
            }

            @Override
            public boolean retainAll(final Collection<?> c) {
                boolean modified = false;
                final Iterator<?> it = this.iterator();
                while (it.hasNext()) {
                    if (!c.contains(it.next())) {
                        it.remove();
                        modified = true;
                    }
                }
                return modified;
            }

            @Override
            public int size() {
                return set.size();
            }

            @Override
            public Object[] toArray() {
                final ArrayList<Object> ret = new ArrayList<Object>();
                final Iterator<?> it = this.iterator();
                while (it.hasNext()) {
                    ret.add(it.next());
                }
                return ret.toArray();
            }

            @Override
            public <T> T[] toArray(final T[] a) {
                final ArrayList<Object> ret = new ArrayList<Object>();
                final Iterator<?> it = this.iterator();
                while (it.hasNext()) {
                    ret.add(it.next());
                }
                return ret.toArray(a);
            }

            @Override
            public String toString() {
                return HTTPHeaderMap.this.internalMap.entrySet().toString();
            }
        };
    }

    @Override
    public V get(final Object key) {
        if (key == null) {
            return this.internalMap.get(null);
        }
        if (key instanceof String) {
            return this.internalMap.get(new HTTPHeaderMapEntry((String) key));
        }
        return null;
    }

    @Override
    public boolean isEmpty() {
        return this.internalMap.isEmpty();
    }

    @Override
    public Set<String> keySet() {
        return new HTTPHeaderKeySet();
    }

    @Override
    public V put(final String key, final V value) {
        if (key == null) {
            return this.internalMap.put(null, value);
        }
        return this.internalMap.put(new HTTPHeaderMapEntry(key), value);
    }

    @Override
    public void putAll(final Map<? extends String, ? extends V> m) {
        if (m.size() == 0) {
            return;
        }
        for (final Map.Entry<? extends String, ? extends V> e : m.entrySet()) {
            this.put(e.getKey(), e.getValue());
        }
    }

    @Override
    public V remove(final Object key) {
        if (key == null) {
            return this.internalMap.remove(null);
        }
        if (key instanceof String) {
            return this.internalMap.remove(new HTTPHeaderMapEntry((String) key));
        }
        return null;
    }

    @Override
    public int size() {
        return this.internalMap.size();
    }

    @Override
    public String toString() {
        return this.internalMap.toString();
    }

    @Override
    public Collection<V> values() {
        return this.internalMap.values();
    }
}
