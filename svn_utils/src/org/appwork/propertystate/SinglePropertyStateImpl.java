package org.appwork.propertystate;

import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.utils.CompareUtils;
import org.appwork.utils.ReflectionUtils;

public class SinglePropertyStateImpl implements PropertyState {
    private final String  key;
    private final Object  value;
    private final Manager manager;

    public Object getValue() {
        return value;
    }

    public SinglePropertyStateImpl(Manager manager, String key, Object value) {
        this.manager = manager;
        this.key = key;
        this.value = value;
    }

    public String getKey() {
        return key;
    }

    @Override
    public Object get(String key) {
        if (key.equals(this.key)) {
            return value;
        } else {
            return null;
        }
    }

    @Override
    public boolean wouldEqualAfterModification(PropertyState currentState, String name, Object arg) {
        return PropertyCompareHelper.wouldEqualAfterModification(this, currentState, name, arg);

    }

    @Override
    public int size() {
        return 1;
    }

    @Override
    public boolean containsKey(Object name) {
        return key.equals(name);
    }

    @Override
    public String toString() {
        return "SingleState\r\n" + key + "=" + value;
    }

    @Override
    public Map<String, Object> toMap() {
        HashMap<String, Object> ret = new HashMap<String, Object>();
        ret.put(key, value);
        return ret;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.propertystate.PropertyState#equalsMap(java.util.Map)
     */
    @Override
    public boolean equalsMap(Map<String, Object> map) {

        if (map.containsKey(Manager.INDEX_PROPERTY)) {
            if (size() != 2) {
                return false;
            }
        } else {
            if (map.size() != 1) {
                return false;
            }
        }

        for (Entry<String, Object> e : map.entrySet()) {
            if (Manager.INDEX_PROPERTY.equals(e.getKey())) {
                continue;
            }
            return key.equals(e.getKey()) && CompareUtils.equalsDeep(e.getValue(), value);
        }
        return false;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T get(String key, Type target, T defaultValue) {
        if (!containsKey(key)) {
            return defaultValue;
        }
        Object ret = get(key);
        return (T) ReflectionUtils.cast(ret, target);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.propertystate.PropertyState#wouldEqualAfterRemoval(org.appwork.propertystate.PropertyState, java.lang.String)
     */
    @Override
    public boolean wouldEqualAfterRemoval(PropertyState currentState, String name) {
        return PropertyCompareHelper.wouldEqualAfterRemoval(this, currentState, name);

    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.propertystate.PropertyState#keySet()
     */
    @Override
    public Collection<String> keySet() {
        return Arrays.asList(key);
    }

    public Manager getManager() {
        return manager;
    }
}
