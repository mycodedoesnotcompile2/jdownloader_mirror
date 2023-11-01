package org.appwork.propertystate;

import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.reflection.Clazz;

public class PropertyStateImpl extends HashMap<String, Object> implements PropertyState {
    private final Manager manager;

    public PropertyStateImpl(Manager manager) {
        this.manager = manager;
    }

    /**
     * @param map
     */
    public PropertyStateImpl(Manager manager, Map<String, Object> map) {
        super(map);
        super.remove(Manager.INDEX_PROPERTY);
        this.manager = manager;

    }

    public PropertyStateImpl(Manager manager, PropertyState origin, String key, Object value) {
        this(manager);
        if (origin != null) {
            if (origin instanceof SinglePropertyStateImpl) {
                super.put(((SinglePropertyStateImpl) origin).getKey(), ((SinglePropertyStateImpl) origin).getValue());
            } else {
                super.putAll(origin.toMap());
            }
        }
        super.put(key, value);
    }

    public PropertyStateImpl(Manager manager, PropertyState origin, String removeKey) {
        this(manager);
        if (origin != null) {
            if (origin instanceof SinglePropertyStateImpl) {
                super.put(((SinglePropertyStateImpl) origin).getKey(), ((SinglePropertyStateImpl) origin).getValue());
            } else {
                super.putAll(origin.toMap());
            }
        }
        super.remove(removeKey);
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.propertystate.PropertyState#equalsMap(java.util.Map)
     */
    @Override
    public boolean equalsMap(Map<String, Object> map) {

        if (map.containsKey(Manager.INDEX_PROPERTY)) {
            if (size() != map.size() - 1) {
                return false;
            }
        } else {
            if (size() != map.size()) {
                return false;
            }
        }

        for (Entry<?, ?> es : entrySet()) {

            Object other = map.get(es.getKey());
            if (other == null && !map.containsKey(es.getKey())) {
                return false;
            }
            if (!CompareUtils.equalsDeep(es.getValue(), other)) {
                return false;
            }
        }
        return true;

    }

    @Override
    public Object get(String key) {
        return super.get(key);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.propertystate.PropertyState#get(java.lang.String, org.appwork.utils.logging2.sendlogs.T)
     */
    @SuppressWarnings("unchecked")
    @Override
    public <T> T get(String key, Type target, T defaultValue) {
        if (!containsKey(key)) {
            return defaultValue;
        }
        Object ret = get(key);
        if (ret == null && Clazz.isPrimitive(target)) {
            LogV3.log(new Exception("Class Mismatch. Expected: " + target + " in State: " + null + ". return default instead"));
            return defaultValue;
        }
        ret = ReflectionUtils.cast(ret, target);
        if (ret != null) {
            Class<?> raw = ReflectionUtils.getRaw(target);

            if (!Clazz.equalsIgnorePrimitive(ret.getClass(), target) && !raw.isAssignableFrom(ret.getClass())) {
                LogV3.log(new Exception("Class Mismatch. Expected: " + target + " in State: " + ret.getClass() + " " + ret + ". return default instead"));
                return defaultValue;
            }
        }
        return (T) ret;
    }

    public Manager getManager() {
        return manager;
    }

    @Override
    public Object put(String key, Object value) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void putAll(Map<? extends String, ? extends Object> m) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Object remove(Object key) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Map<String, Object> toMap() {
        return this;
    }

    @Override
    public String toString() {
        return "State\r\n" + super.toString();
    }

    @Override
    public boolean wouldEqualAfterModification(PropertyState oldState, String name, Object arg) {
        return PropertyCompareHelper.wouldEqualAfterModification(this, oldState, name, arg);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.propertystate.PropertyState#wouldEqualAfterRemoval(org.appwork.propertystate.PropertyState, java.lang.String)
     */
    @Override
    public boolean wouldEqualAfterRemoval(PropertyState oldState, String name) {
        return PropertyCompareHelper.wouldEqualAfterRemoval(this, oldState, name);

    }
}
