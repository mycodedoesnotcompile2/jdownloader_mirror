package org.jdownloader.extensions.eventscripter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import net.sourceforge.htmlunit.corejs.javascript.Context;
import net.sourceforge.htmlunit.corejs.javascript.NativeJavaObject;
import net.sourceforge.htmlunit.corejs.javascript.ScriptRuntime;
import net.sourceforge.htmlunit.corejs.javascript.Scriptable;

public class NativeJavaMapWrapper extends NativeJavaObject {

    private static final long         serialVersionUID = -3786257752907047381L;

    private final Map<Object, Object> map;

    @SuppressWarnings("unchecked")
    public NativeJavaMapWrapper(Scriptable scope, Object map) {
        super(scope, map, map.getClass());
        assert map instanceof Map;
        this.map = (Map<Object, Object>) map;
    }

    @Override
    public String getClassName() {
        return "JavaMap";
    }

    @Override
    public boolean has(String name, Scriptable start) {
        final Context cx = Context.getCurrentContext();
        if (cx != null) {
            if (map.containsKey(name)) {
                return true;
            }
        }
        return super.has(name, start);
    }

    @Override
    public boolean has(int index, Scriptable start) {
        final Context cx = Context.getCurrentContext();
        if (cx != null) {
            if (map.containsKey(Integer.valueOf(index))) {
                return true;
            }
        }
        return super.has(index, start);
    }

    @Override
    public Object get(String name, Scriptable start) {
        final Context cx = Context.getCurrentContext();
        if (cx != null) {
            if (map.containsKey(name)) {
                final Object obj = map.get(name);
                return cx.getWrapFactory().wrap(cx, this, obj, obj == null ? null : obj.getClass());
            }
        }
        return super.get(name, start);
    }

    @Override
    public Object get(int index, Scriptable start) {
        final Context cx = Context.getCurrentContext();
        if (cx != null) {
            if (map.containsKey(Integer.valueOf(index))) {
                final Object obj = map.get(Integer.valueOf(index));
                return cx.getWrapFactory().wrap(cx, this, obj, obj == null ? null : obj.getClass());
            }
        }
        return super.get(index, start);
    }

    @Override
    public void put(String name, Scriptable start, Object value) {
        final Context cx = Context.getCurrentContext();
        if (cx != null) {
            map.put(name, Context.jsToJava(value, Object.class));
        } else {
            super.put(name, start, value);
        }
    }

    @Override
    public void put(int index, Scriptable start, Object value) {
        final Context cx = Context.getCurrentContext();
        if (cx != null) {
            map.put(Integer.valueOf(index), Context.jsToJava(value, Object.class));
        } else {
            super.put(index, start, value);
        }
    }

    @Override
    public Object[] getIds() {
        final Context cx = Context.getCurrentContext();
        if (cx != null) {
            final List<Object> ids = new ArrayList<Object>(map.size());
            for (Object key : map.keySet()) {
                if (key instanceof Integer) {
                    ids.add(key);
                } else {
                    ids.add(ScriptRuntime.toString(key));
                }
            }
            return ids.toArray();
        }
        return super.getIds();
    }

    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

}