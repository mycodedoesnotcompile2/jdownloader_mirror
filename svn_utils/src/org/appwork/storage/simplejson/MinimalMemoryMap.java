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
package org.appwork.storage.simplejson;

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * @author Daniel Wilhelm
 * @date 26.06.2022
 *
 */
public class MinimalMemoryMap<K, V> extends AbstractMap<K, V> implements Map<K, V> {
    /**
     * IDEAS:
     *
     * - binary sort the entries in table, use binary search for fast access to the table
     *
     * -after remove of entry move remaining entries in table to the left, store size(count) so we can fast add new entries at the end of
     * the table (increases size of object)
     */
    private static final Object NULLKEY = new Object();

    public MinimalMemoryMap() {
    }

    public MinimalMemoryMap(int initialCapacity) {
        if (initialCapacity > 0) {
            ensureCapacity(initialCapacity);
        }
    }

    public MinimalMemoryMap(Map<K, V> map) {
        if (map != null) {
            ensureCapacity(map.size());
            for (Entry<K, V> entry : map.entrySet()) {
                putObject(entry.getKey(), entry.getValue());
            }
        }
    }

    private Object[] table = null;

    public static void main(String[] args) {
        MinimalMemoryMap<String, Object> test = new MinimalMemoryMap<String, Object>();
        System.out.println(test.put("test", Boolean.TRUE) + " " + test.size() + "\t" + test);
        System.out.println(test.put("test", Boolean.FALSE) + " " + test.size() + "\t" + test);
        System.out.println(test.put("test2", Boolean.FALSE) + " " + test.size() + "\t" + test);
        System.out.println(test.put(null, Boolean.FALSE) + " " + test.size() + "\t" + test);
        System.out.println(test.put(null, Boolean.TRUE) + " " + test.size() + "\t" + test);
        System.out.println(test.remove(null) + " " + test.size() + "\t" + test);
        System.out.println(test.remove(null) + " " + test.size() + "\t" + test);
        Iterator<Entry<String, Object>> it = test.entrySet().iterator();
        System.out.println(test.remove("test") + " " + test.size() + "\t" + test);
        System.out.println(test.remove("test") + " " + test.size() + "\t" + test);
    }

    protected Object[] getTable() {
        return table;
    }

    protected Object[] ensureCapacity(final int capacity) {
        if (table == null && capacity == 0) {
            return null;
        } else if (table == null) {
            table = new Object[capacity * 2];
        } else {
            final int length = table.length;
            final int grow = (capacity * 2) - length;
            if (grow > 0) {
                final Object[] tmpTable = new Object[length + grow];
                System.arraycopy(table, 0, tmpTable, 0, length);
                this.table = tmpTable;
            }
        }
        return table;
    }

    @Override
    public boolean containsKey(Object key) {
        return getKeyIndex(key) >= 0;
    }

    @Override
    public V remove(Object key) {
        final Object[] table = getTable();
        final int index = getKeyIndex(key);
        if (index >= 0 && table != null) {
            final V value = (V) table[index + 1];
            table[index] = null;
            table[index + 1] = null;
            return value;
        }
        return null;
    }

    @Override
    public V put(K key, V value) {
        return putObject(key, value);
    }

    @Override
    public V get(Object key) {
        return getValue(key);
    }

    protected V putObject(Object key, V value) {
        if (key == null) {
            key = NULLKEY;
        }
        final Object[] table = ensureCapacity(1);
        final int length = table.length;
        int index = getKeyIndex(key);
        if (index != -1) {
            final V old = (V) table[index + 1];
            table[index + 1] = value;
            return old;
        } else {
            for (index = 0; index < length; index += 2) {
                if (table[index] == null) {
                    table[index] = key;
                    table[index + 1] = value;
                    return null;
                }
            }
            final Object[] newTable = ensureCapacity((length / 2) + 1);
            newTable[length] = key;
            newTable[length + 1] = value;
            return null;
        }
    }

    protected V getValue(Object key) {
        final Object[] table = getTable();
        if (table != null) {
            if (key == null) {
                key = NULLKEY;
            }
            final int length = table.length;
            for (int index = 0; index < length; index += 2) {
                if (key.equals(table[index])) {
                    return (V) table[index + 1];
                }
            }
        }
        return null;
    }

    protected int getKeyIndex(Object key) {
        final Object[] table = getTable();
        if (table != null) {
            if (key == null) {
                key = NULLKEY;
            }
            final int length = table.length;
            for (int index = 0; index < length; index += 2) {
                if (key.equals(table[index])) {
                    return index;
                }
            }
        }
        return -1;
    }

    @Override
    public boolean containsValue(Object value) {
        final Object[] table = getTable();
        if (table != null) {
            final int length = table.length;
            for (int index = 0; index < length; index += 2) {
                final Object v = table[index + 1];
                if ((value != null && value.equals(v)) || (value == null && v == null)) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public void clear() {
        table = null;
    }

    @Override
    public int size() {
        final Object[] table = getTable();
        int ret = 0;
        if (table != null) {
            final int length = table.length;
            for (int index = 0; index < length - 1; index = index + 2) {
                if (table[index] != null) {
                    ret++;
                }
            }
        }
        return ret;
    }

    @Override
    public boolean isEmpty() {
        final Object[] table = getTable();
        if (table != null) {
            final int length = table.length;
            for (int index = 0; index < length - 1; index = index + 2) {
                if (table[index] != null) {
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    public Set<Entry<K, V>> entrySet() {
        return new AbstractSet<Map.Entry<K, V>>() {
            @Override
            public Iterator<Entry<K, V>> iterator() {
                return new Iterator<Map.Entry<K, V>>() {
                    int         index = 0;
                    Entry<K, V> last  = null;

                    @Override
                    public boolean hasNext() {
                        final Object[] table = getTable();
                        int pos = index;
                        while (table != null && pos < table.length - 1) {
                            if (table[pos] != null) {
                                return true;
                            } else {
                                pos = pos + 2;
                            }
                        }
                        return false;
                    }

                    @Override
                    public void remove() {
                        if (last != null) {
                            MinimalMemoryMap.this.remove(last.getKey());
                            last = null;
                        }
                    }

                    @Override
                    public Entry<K, V> next() {
                        final Object[] table = getTable();
                        int pos = index;
                        while (table != null && pos < table.length - 1) {
                            final Object key = table[pos];
                            final V value = (V) table[pos + 1];
                            pos = pos + 2;
                            index = pos;
                            if (key != null) {
                                last = new SimpleEntry<K, V>(key == NULLKEY ? null : (K) key, value) {
                                    @Override
                                    public V setValue(V value) {
                                        super.setValue(value);
                                        return MinimalMemoryMap.this.put(getKey(), value);
                                    }
                                };
                                return last;
                            }
                        }
                        return null;
                    }
                };
            }

            @Override
            public boolean isEmpty() {
                return MinimalMemoryMap.this.isEmpty();
            }

            @Override
            public int size() {
                return MinimalMemoryMap.this.size();
            }
        };
    }
}
