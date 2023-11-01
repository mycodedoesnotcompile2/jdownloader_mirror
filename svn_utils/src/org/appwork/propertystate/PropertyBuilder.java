package org.appwork.propertystate;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Map;
import java.util.WeakHashMap;

import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.ReflectionUtils;

public class PropertyBuilder<T> {
    public static class Handler implements InvocationHandler {
        private Manager manager;

        public Manager getManager() {
            return manager;
        }

        private PropertyState state;

        public Handler(Manager manager) {
            this.manager = manager;
            state = manager.getEmptyState();
        }

        @Override
        public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
            if (method.getName().equals("toString")) {
                return "MemProp " + state.toString();
            }
            String key = ClassCache.createKey(method);
            if (key == null) {
                throw new IllegalArgumentException("No Setter or Getter " + method);
            }
            synchronized (this) {
                if (args != null && args.length == 1) {
                    Object old = state.get(key);
                    state = manager.put(state, key, args[0]);
                    if (method.getReturnType() != void.class) {
                        // return true if the value was changed
                        return !CompareUtils.equalsDeep(old, state.get(key));
                    }
                    return null;
                } else if (args == null || args.length == 0) {
                    if (method.getReturnType() != void.class) {
                        Object ret = state.get(key);
                        return ReflectionUtils.cast(ret, method.getReturnType());
                    } else {
                        throw new IllegalArgumentException("Invalid method " + method);
                    }
                } else {
                    throw new IllegalArgumentException("Invalid method " + method);
                }
            }
        }

        /**
         * @param propertyState
         * @param map
         */
        public void setState(Map<String, Object> map) {
            state = manager.createState(map);

        }

        /**
         * @return
         */
        public PropertyState getState() {
            return state;
        }

    }

    private Class<?> targetInterface;

    public PropertyBuilder(Class<T> interf) {
        this.targetInterface = interf;
    }

    private static final WeakHashMap<Class<?>, Manager> CONTROLLER = new WeakHashMap<Class<?>, Manager>();

    public T build() {
        Manager manager = createManager(targetInterface);
        return build(manager);
    }

    public T build(Manager manager) {
        return (T) Proxy.newProxyInstance(targetInterface.getClassLoader(), new Class[] { targetInterface }, new Handler(manager));
    }

    public Manager createManager(Class<?> targetInterface2) {
        Manager manager;
        synchronized (CONTROLLER) {
            manager = CONTROLLER.get(targetInterface);
            if (manager == null) {
                manager = createManager();
            }
            CONTROLLER.put(targetInterface, manager);
        }
        return manager;
    }

    private Manager createManager() {
        return new Manager(null);
    }

    /**
     * @param propertyState
     * @return
     */
    public static <T> Handler getHandler(T propertyState) {
        return (Handler) Proxy.getInvocationHandler(propertyState);

    }
}
