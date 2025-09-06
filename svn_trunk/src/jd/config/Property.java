//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.config;

import java.lang.ref.WeakReference;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.WeakHashMap;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.storage.simplejson.MinimalMemoryMap;
import org.appwork.utils.ReflectionUtils;

/**
 * Von dieser Klasse kann abgeleitet werden wenn die Neue Klasse Properties unterstützen soll. Die SimpleGUI elemente nutzen das um einfache
 * Dialogelemente zu erstellen. Ein Automatisiertes speichern/laden wird dadurch möglich
 *
 * @author JD-Team
 *
 */
public class Property {
    private final static WeakHashMap<String, WeakReference<String>> DEDUPEMAP = new WeakHashMap<String, WeakReference<String>>();

    // private final static WeakStringCache DEDUPECACHE = new WeakStringCache();

    public static String dedupeString(String string) {
        if (string == null) {
            return null;
        }
        synchronized (DEDUPEMAP) {
            String ret = null;
            WeakReference<String> ref = DEDUPEMAP.get(string);
            if (ref != null && (ret = ref.get()) != null) {
                return ret;
            }
            ref = new WeakReference<String>(string);
            DEDUPEMAP.put(string, ref);
            return string;
        }
    }

    public static String returnDedupeStringInstance(String string) {
        if (string == null) {
            return null;
        }
        synchronized (DEDUPEMAP) {
            final WeakReference<String> ref = DEDUPEMAP.get(string);
            final String ret;
            if (ref != null && (ret = ref.get()) != null) {
                return ret;
            }
            return null;
        }
    }

    private static final long  serialVersionUID  = -6093927038856757256L;
    /**
     * Null value used to remove a key completly.
     */
    public static final Object NULL              = new Object();
    private final Object       NEWIMPLEMENTATION = new Object();
    private Object[]           propertiesList    = null;

    private void ensureCapacity(final int capacity) {
        synchronized (NEWIMPLEMENTATION) {
            if (propertiesList == null) {
                propertiesList = new Object[capacity * 2];
            } else {
                final int length = propertiesList.length;
                final int grow = (capacity * 2) - length;
                if (grow > 0) {
                    final Object[] tmpPropertiesList = new Object[length + grow];
                    System.arraycopy(propertiesList, 0, tmpPropertiesList, 0, length);
                }
            }
        }
    }

    private boolean putObject(String key, Object value) {
        synchronized (NEWIMPLEMENTATION) {
            if (propertiesList == null && (value == null || value == NULL)) {
                return false;
            }
            ensureCapacity(1);
            final Object[] propertiesList = this.propertiesList;
            if (key != null) {
                final int length = propertiesList.length;
                int index = getObjectIndex(key);
                if (index != -1) {
                    if (value == null || value == NULL) {
                        propertiesList[index] = null;
                        propertiesList[index + 1] = null;
                        return true;
                    } else {
                        final Object old = propertiesList[index + 1];
                        if (value instanceof String) {
                            propertiesList[index + 1] = dedupeValueString(key, (String) value);
                        } else {
                            propertiesList[index + 1] = value;
                        }
                        if (old == null && value != null) {
                            return true;
                        } else {
                            return !old.equals(value);
                        }
                    }
                } else if (value == null || value == NULL) {
                    return false;
                }
                for (index = 0; index < length; index += 2) {
                    if (propertiesList[index] == null) {
                        propertiesList[index] = dedupeKeyString(key);
                        if (value instanceof String) {
                            propertiesList[index + 1] = dedupeValueString(key, (String) value);
                        } else {
                            propertiesList[index + 1] = value;
                        }
                        return true;
                    }
                }
                final Object[] tmpPropertiesList = new Object[length + 2];
                System.arraycopy(propertiesList, 0, tmpPropertiesList, 0, length);
                tmpPropertiesList[length] = dedupeKeyString(key);
                if (value instanceof String) {
                    tmpPropertiesList[length + 1] = dedupeValueString(key, (String) value);
                } else {
                    tmpPropertiesList[length + 1] = value;
                }
                this.propertiesList = tmpPropertiesList;
                return true;
            }
            return false;
        }
    }

    private Object getObject(String key) {
        synchronized (NEWIMPLEMENTATION) {
            final Object[] propertiesList = this.propertiesList;
            if (propertiesList != null && key != null) {
                final int length = propertiesList.length;
                for (int index = 0; index < length; index += 2) {
                    if (key.equals(propertiesList[index])) {
                        return propertiesList[index + 1];
                    }
                }
            }
        }
        return null;
    }

    private int getObjectIndex(String key) {
        synchronized (NEWIMPLEMENTATION) {
            final Object[] propertiesList = this.propertiesList;
            if (propertiesList != null && key != null) {
                final int length = propertiesList.length;
                for (int index = 0; index < length; index += 2) {
                    if (key.equals(propertiesList[index])) {
                        return index;
                    }
                }
            }
        }
        return -1;
    }

    public Property() {
    }

    public boolean removeProperty(String key) {
        return setProperty(key, Property.NULL);
    }

    /**
     * Gibt einen Boolean zu key zurück. Es wird versuchtden Wert zu einem passendem Wert umzuformen
     *
     * @param key
     * @return
     */
    public Boolean getBooleanProperty(final String key) {
        return getBooleanProperty(key, false);
    }

    public <T> T getObjectProperty(String key, TypeRef<T> typeRef) {
        final Object raw = getProperty(key);
        if (raw == null) {
            return null;
        } else if (typeRef.getType().equals(raw.getClass())) {
            return (T) raw;
        } else {
            final T ret = JSonStorage.convert(raw, typeRef);
            setProperty(key, ret);
            return ret;
        }
    }

    public Boolean getBooleanProperty(final String key, final boolean def) {
        return getBooleanProperty(key, (Boolean) def);
    }

    public Boolean getBooleanProperty(final String key, final Boolean def) {
        try {
            final Object value = getProperty(key, def);
            if (value instanceof Boolean) {
                return (Boolean) value;
            } else if (value == null) {
                return def;
            } else {
                final String stringValue = String.valueOf(value);
                if (stringValue != null) {
                    if ("true".equalsIgnoreCase(stringValue) || "1".equals(stringValue)) {
                        return true;
                    } else if ("false".equalsIgnoreCase(stringValue) || "0".equals(stringValue)) {
                        return false;
                    } else {
                        return stringValue.length() > 0;
                    }
                }
            }
        } catch (final Exception e) {
        }
        return def;
    }

    /**
     * Gibt einen Integerwert zu key zurück. Es wird versucht, den Wert zu einem passendem Integer umzuformen
     *
     * @param key
     *            Schlüssel des Wertes
     * @return Der Wert
     */
    public int getIntegerProperty(final String key) {
        return getIntegerProperty(key, -1);
    }

    public int getIntegerProperty(final String key, final int def) {
        try {
            final Object value = getProperty(key, def);
            if (value instanceof String) {
                return Integer.parseInt((String) value);
            } else if (value instanceof Number) {
                return ((Number) value).intValue();
            }
        } catch (final Exception e) {
        }
        return def;
    }

    /**
     * @since JD2
     */
    public long getLongProperty(final String key, final long def) {
        try {
            final Object value = getProperty(key, def);
            if (value instanceof String) {
                return Long.parseLong((String) value);
            } else if (value instanceof Number) {
                return ((Number) value).longValue();
            }
        } catch (final Exception e) {
        }
        return def;
    }

    public int getPropertiesSize() {
        synchronized (NEWIMPLEMENTATION) {
            final Object[] propertiesList = this.propertiesList;
            int size = 0;
            if (propertiesList != null) {
                final int length = propertiesList.length;
                for (int index = 0; index < length; index += 2) {
                    if (propertiesList[index] != null) {
                        size++;
                    }
                }
            }
            return size;
        }
    }

    /**
     * returns a copy of the internal Map
     *
     * @return
     */
    public Map<String, Object> getProperties() {
        synchronized (NEWIMPLEMENTATION) {
            final Object[] propertiesList = this.propertiesList;
            final int size = getPropertiesSize();
            final Map<String, Object> ret = newMapInstance(size);
            if (propertiesList != null && size > 0) {
                final int length = propertiesList.length;
                for (int index = 0; index < length; index += 2) {
                    if (propertiesList[index] != null) {
                        ret.put((String) propertiesList[index], propertiesList[index + 1]);
                    }
                }
            }
            return ret;
        }
    }

    public static Map<String, Object> newMapInstance(int size) {
        return new MinimalMemoryMap<String, Object>(size);
    }

    /**
     * Returns the value for key
     *
     * @param key
     * @return Value for key
     */
    public Object getProperty(final String key) {
        if (key == null) {
            throw new WTFException("key ==null is forbidden!");
        } else {
            return getObject(key);
        }
    }

    /**
     * Returns the value for key, and if none is set def
     *
     * @param key
     * @param def
     * @return value
     */
    public Object getProperty(final String key, final Object def) {
        Object ret = getProperty(key);
        if (def instanceof Number && ret instanceof Number) {
            if (def.getClass().equals(ret.getClass())) {
                return ret;
            } else {
                ret = ReflectionUtils.cast(ret, def.getClass());
            }
        }
        if (ret == null) {
            return def;
        } else {
            return ret;
        }
    }

    /**
     * Gibt einen String zu key zurück. Es wird versuchtden Wert zu einem passendem Wert umzuformen
     *
     * @param key
     * @return
     */
    public String getStringProperty(final String key) {
        return getStringProperty(key, null);
    }

    public String getStringProperty(final String key, final String def) {
        try {
            final Object r = getProperty(key, def);
            final String ret = (r == null) ? null : r.toString();
            return ret;
        } catch (final Exception e) {
            return def;
        }
    }

    public boolean hasProperty(final String key) {
        if (key == null) {
            throw new WTFException("key ==null is forbidden!");
        } else {
            return getObjectIndex(key) != -1;
        }
    }

    protected Map<String, Object> optimizeMapInstance(final Map<String, Object> map) {
        if (map != null && map.size() > 0) {
            final Map<String, Object> ret = newMapInstance(0);
            final Iterator<Entry<String, Object>> it = map.entrySet().iterator();
            while (it.hasNext()) {
                final Entry<String, Object> next = it.next();
                String key = next.getKey();
                Object value = next.getValue();
                if (key == null || value == null) {
                    continue;
                } else {
                    key = dedupeKeyString(key);
                    if (value instanceof Map) {
                        value = optimizeMapInstance((Map<String, Object>) value);
                        if (value != null) {
                            ret.put(key, value);
                        }
                    } else if (value instanceof String) {
                        ret.put(key, dedupeValueString(key, (String) value));
                    } else {
                        ret.put(key, value);
                    }
                }
            }
            if (ret.size() > 0) {
                return ret;
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    protected String dedupeValueString(String key, String value) {
        return dedupeString(value);
    }

    protected String dedupeKeyString(String key) {
        return dedupeString(key);
    }

    public void setProperties(final Map<String, Object> properties) {
        final Map<String, Object> newProperties = optimizeMapInstance(properties);
        if (newProperties != null && newProperties.size() > 0) {
            synchronized (NEWIMPLEMENTATION) {
                propertiesList = null;
                ensureCapacity(newProperties.size());
                for (final Entry<String, Object> entry : newProperties.entrySet()) {
                    putObject(entry.getKey(), entry.getValue());
                }
            }
        } else {
            synchronized (NEWIMPLEMENTATION) {
                propertiesList = null;
            }
        }
    }

    /**
     * Stores a value. Warning: DO not store other stuff than primitives/lists/maps!!
     *
     * @param key
     * @param value
     */
    public boolean setProperty(String key, Object value) {
        if (key == null) {
            throw new WTFException("key ==null is forbidden!");
        }
        return putObject(key, value);
    }

    /**
     * GIbt die Proprties als String zurück
     *
     * @return PropertyString
     */
    @Override
    public String toString() {
        final Map<String, Object> lInternal = getProperties();
        if (lInternal == null || lInternal.size() == 0) {
            return "Property: empty";
        } else {
            synchronized (lInternal) {
                return "Property: " + lInternal;
            }
        }
    }
}