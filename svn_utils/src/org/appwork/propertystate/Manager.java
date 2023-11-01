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
package org.appwork.propertystate;

import java.lang.ref.WeakReference;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.reflection.CompiledType;

public class Manager {
    /**
     *
     */
    static final String                              INDEX_PROPERTY = "_";
    private static final Map<String, Object>         EMPTY_MAP      = Collections.unmodifiableMap(new HashMap<String, Object>());
    private LinkedList<WeakReference<PropertyState>> states         = new LinkedList<WeakReference<PropertyState>>();
    private EmptyStateImpl                           empty;
    private Class<?>                                 propertyAccessClassContainer;

    public Manager(Class<?> propertyAccessClassContainer) {
        empty = new EmptyStateImpl(this);
        this.propertyAccessClassContainer = propertyAccessClassContainer;
        states.add(new WeakReference<PropertyState>(empty));
    }

    public PropertyState deriveNewInstance(PropertyState currentState, String key, Object arg) {
        if (currentState == null || currentState == getEmptyState()) {
            return new SinglePropertyStateImpl(this, key, arg);
        } else {
            return new PropertyStateImpl(this, currentState, key, arg);
        }
    }

    /**
     * @param class1
     * @param map
     * @return
     * @return
     */
    protected Map<String, Object> fixImportMap(Class<?> class1, Map<String, Object> map) {
        FlexiJSonMapper mapper = null;
        HashMap<String, Object> ret = null;
        for (Field f : class1.getDeclaredFields()) {
            if (PropertyAccess.class.isAssignableFrom(f.getType()) && Modifier.isStatic(f.getModifiers())) {
                try {
                    f.setAccessible(true);
                    PropertyAccess<?, ?> access = (PropertyAccess<?, ?>) f.get(null);
                    if (!map.containsKey(access.getKey())) {
                        continue;
                    }
                    Object value = map.get(access.getKey());
                    if (value == null) {
                        Object newValue = ReflectionUtils.cast(value, access.getRawClass());
                        if (newValue != null && newValue != value) {
                            if (ret == null) {
                                ret = new HashMap<String, Object>(map);
                            }
                            ret.put(access.getKey(), newValue);
                        }
                        // TODO: Check/Write Test for anonym enums
                    } else if (access.getRawClass().isEnum()) {
                        if (value instanceof String) {
                            @SuppressWarnings({ "unchecked", "rawtypes" })
                            Enum enumValue = Enum.valueOf((Class<Enum>) access.getRawClass(), (String) value);
                            if (ret == null) {
                                ret = new HashMap<String, Object>(map);
                            }
                            ret.put(access.getKey(), enumValue);
                        }
                    } else if (!Clazz.isPrimitive(access.getRawClass()) && access.getRawClass() != String.class) {
                        if (mapper == null) {
                            mapper = new FlexiJSonMapper();
                        }
                        if (value != null) {
                            if (!access.getRawClass().isAssignableFrom(value.getClass())) {
                                FlexiJSonNode node = mapper.objectToJsonNode(value);
                                Object fixedValue = mapper.jsonToObject(node, CompiledType.create(access.getType()));
                                if (ret == null) {
                                    ret = new HashMap<String, Object>(map);
                                }
                                ret.put(access.getKey(), fixedValue);
                            }
                        }
                    }
                } catch (IllegalArgumentException e) {
                    LogV3.log(e);
                } catch (IllegalAccessException e) {
                    LogV3.log(e);
                } catch (FlexiMapperException e) {
                    LogV3.log(e);
                }
            }
        }
        return ret == null ? map : ret;
    }

    /**
     * @param map
     * @return
     */
    public PropertyState createState(Map<String, Object> input) {
        if (input == null) {
            return empty;
        }
        if (input.size() == 0) {
            return empty;
        }
        Map<String, Object> map = input;
        synchronized (this) {
            final Number index = (Number) map.get(INDEX_PROPERTY);
            PropertyState found = getCached(map, index);
            if (found != null) {
                return found;
            }
            if (propertyAccessClassContainer != null) {
                map = fixImportMap(propertyAccessClassContainer, map);
            }
            for (Iterator<WeakReference<PropertyState>> it = states.iterator(); it.hasNext();) {
                WeakReference<PropertyState> ref = it.next();
                PropertyState existing = ref.get();
                if (existing == null) {
                    System.out.println("Cleanup unused state");
                    it.remove();
                    continue;
                }
                if (existing.equalsMap(map)) {
                    found = existing;
                    break;
                }
            }
            if (found == null) {
                found = deriveNewInstance(map);
                states.add(new WeakReference<PropertyState>(found));
            }
            putCached(index, found);
            return found;
        }
    }

    protected void putCached(final Number index, PropertyState found) {
        if (index == null) {
            return;
        }
        PropertyStateIndexCache cache = threadIndexCache.get();
        if (cache != null) {
            cache.put(index.intValue(), found);
        }
    }

    protected PropertyState getCached(Map<String, Object> map, Number index) {
        if (index != null) {
            PropertyStateIndexCache cache = threadIndexCache.get();
            if (map.size() == 1) {
                // reference
                if (cache == null) {
                    throw new IllegalStateException("Cannot resolve index reference - no Cache Controller is set for this thread/manager");
                } else {
                    PropertyState hit = cache.getState(index.intValue());
                    if (hit != null) {
                        return hit;
                    } else {
                        throw new IllegalStateException("Cannot find index in Cache Controller");
                    }
                }
            }
        }
        return null;
    }

    public PropertyState deriveNewInstance(Map<String, Object> map) {
        if (map == null) {
            return empty;
        }
        if (map.size() == 0) {
            return empty;
        }
        boolean hasIndex = map.containsKey(INDEX_PROPERTY);
        PropertyState currentState = null;
        if (map.size() == 1 || (map.size() == 2 && hasIndex)) {
            for (Entry<String, Object> e : map.entrySet()) {
                if (hasIndex && INDEX_PROPERTY.equals(e.getKey())) {
                    continue;
                }
                currentState = new SinglePropertyStateImpl(this, e.getKey(), e.getValue());
            }
        } else {
            currentState = new PropertyStateImpl(this, map);
        }
        return currentState;
    }

    public PropertyState deriveNewInstanceWithoutKey(PropertyState currentState, String removeKey) {
        if (currentState == null || currentState == getEmptyState()) {
            return empty;
        } else if (currentState.size() == 1 && currentState.containsKey(removeKey)) {
            return empty;
        } else if (currentState.size() == 2 && currentState.containsKey(removeKey)) {
            for (String key : currentState.keySet()) {
                if (key.equals(removeKey)) {
                    continue;
                }
                return new SinglePropertyStateImpl(this, key, currentState.get(key));
            }
            throw new IllegalStateException();
        } else {
            return new PropertyStateImpl(this, currentState, removeKey);
        }
    }

    /**
     * @param old
     * @param key
     * @return
     */
    public PropertyState remove(PropertyState currentState, String key) {
        synchronized (this) {
            for (Iterator<WeakReference<PropertyState>> it = states.iterator(); it.hasNext();) {
                PropertyState existing = it.next().get();
                if (existing == null) {
                    it.remove();
                    continue;
                }
                if (existing.wouldEqualAfterRemoval(currentState == null ? empty : currentState, key)) {
                    return existing;
                }
            }
            currentState = deriveNewInstanceWithoutKey(currentState, key);
            states.add(new WeakReference<PropertyState>(currentState));
            return currentState;
        }
    }

    public PropertyState put(PropertyState currentState, String key, Object arg) {
        synchronized (this) {
            for (Iterator<WeakReference<PropertyState>> it = states.iterator(); it.hasNext();) {
                PropertyState existing = it.next().get();
                if (existing == null) {
                    it.remove();
                    continue;
                }
                if (existing.wouldEqualAfterModification(currentState == null ? empty : currentState, key, arg)) {
                    return existing;
                } else {
                    // System.out.println("No Match " + existing);
                }
            }
            currentState = deriveNewInstance(currentState, key, arg);
            System.out.println("Create new State (" + (states.size() + 1) + "): " + currentState);
            states.add(new WeakReference<PropertyState>(currentState));
            return currentState;
        }
    }

    public PropertyState getEmptyState() {
        return empty;
    }

    /**
     * @return
     */
    public List<PropertyState> getStates() {
        synchronized (this) {
            ArrayList<PropertyState> ret = new ArrayList<PropertyState>();
            for (Iterator<WeakReference<PropertyState>> it = states.iterator(); it.hasNext();) {
                PropertyState existing = it.next().get();
                if (existing == null) {
                    it.remove();
                    continue;
                }
                ret.add(existing);
            }
            return ret;
        }
    }

    /**
     * @param state
     * @return
     */
    public Map<String, Object> toIndexMap(PropertyState state) {
        if (state.size() == 0) {
            return EMPTY_MAP;
        }
        PropertyStateIndexCache cache = threadIndexCache.get();
        if (cache == null) {
            throw new IllegalStateException("No Cache Controller is set for this thread/Manager");
        }
        return cache.toMap(state);
    }

    protected final ThreadLocal<PropertyStateIndexCache> threadIndexCache = new ThreadLocal<PropertyStateIndexCache>();

    public void setThreadIndexCache(PropertyStateIndexCache cache) {
        threadIndexCache.set(cache);
    }
}