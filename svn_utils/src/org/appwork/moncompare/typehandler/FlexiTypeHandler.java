package org.appwork.moncompare.typehandler;

import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import org.appwork.exceptions.WTFException;
import org.appwork.moncompare.Condition;
import org.appwork.moncompare.TypeHandler;
import org.appwork.moncompare.list.ListAccessor;
import org.appwork.moncompare.list.ListAccessorInterface;
import org.appwork.moncompare.object.MapAccessorInterface;
import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.KeyValueElement;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.utils.CompareUtils;

public class FlexiTypeHandler implements TypeHandler {
    @Override
    public Object unwrapType(final Object value) {
        if (value instanceof FlexiJSonValue) {
            return ((FlexiJSonValue) value).getValue();
        }
        return value;
    }

    /**
     * @see org.appwork.moncompare.TypeHandler#getListAccessor(java.lang.Object)
     */
    @Override
    public ListAccessorInterface getListAccessor(Object expressions) {
        // FlexiJsonArray is a List - no custom handling required
        if (expressions instanceof FlexiJSonArray) {
            return new FlexiArrayWrapper((List<Object>) expressions);
        }
        return null;
    }

    @Override
    public Boolean equals(final Object a, final Object b) {
        return CompareUtils.equals(this.unwrapType(a), this.unwrapType(b));
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.moncompare.TypeHandler#compare(java.lang.Object, java.lang.Object)
     */
    @Override
    public Integer compare(Object a, Object b) {
        if (a instanceof FlexiJSonNode || b instanceof FlexiJSonNode) {
            return CompareUtils.tryToCompare(unwrapType(a), unwrapType(b));
        }
        return null;
    }

    /**
     * @author thomas
     * @date 16.10.2023
     *
     */
    public static class FlexiArrayWrapper extends ListAccessor {
        /**
         * @param expression
         */
        public FlexiArrayWrapper(List<Object> expression) {
            super(expression);
        }

        /**
         * @see org.appwork.moncompare.list.ListAccessor#set(int, java.lang.Object)
         */
        @Override
        public void set(int index, Object value) {
            if (!(value instanceof FlexiJSonNode)) {
                try {
                    value = new FlexiJSonMapper().objectToJsonNode(value);
                } catch (FlexiMapperException e) {
                    throw new IllegalArgumentException(e);
                }
            }
            super.set(index, value);
        }

        /**
         * @see org.appwork.moncompare.list.ListAccessor#add(java.lang.Object)
         */
        @Override
        public void add(Object value) {
            if (!(value instanceof FlexiJSonNode)) {
                try {
                    value = new FlexiJSonMapper().objectToJsonNode(value);
                } catch (FlexiMapperException e) {
                    throw new IllegalArgumentException(e);
                }
            }
            super.add(value);
        }
    }

    public class FlexiMapAccessor implements MapAccessorInterface {
        private FlexiJSonObject obj;

        /**
         * @param expression
         */
        public FlexiMapAccessor(FlexiJSonObject expression) {
            this.obj = expression;
        }

        /**
         * @see java.lang.Iterable#iterator()
         */
        @Override
        public Iterator<Entry<String, Object>> iterator() {
            return new Iterator<Entry<String, Object>>() {
                private int index;
                {
                    index = 0;
                }

                @Override
                public boolean hasNext() {
                    return obj.size() > index;
                }

                /**
                 * @see java.util.Iterator#next()
                 */
                @Override
                public Entry<String, Object> next() {
                    final KeyValueElement element = obj.getElements().get(index++);
                    return new Entry<String, Object>() {
                        @Override
                        public String getKey() {
                            return element.getKey();
                        }

                        @Override
                        public Object getValue() {
                            return element.getValue();
                        }

                        @Override
                        public Object setValue(Object value) {
                            throw new WTFException("Not Implemented");
                        }
                    };
                }
            };
        }

        /**
         * @see org.appwork.moncompare.object.MapAccessorInterface#size()
         */
        @Override
        public int size() {
            return obj.size();
        }

        /**
         * @see org.appwork.moncompare.object.MapAccessorInterface#keySet()
         */
        @Override
        public Set<String> keySet() {
            return obj.getKeys();
        }

        /**
         * @see org.appwork.moncompare.object.MapAccessorInterface#get(java.lang.Object)
         */
        @Override
        public Object get(String key) {
            KeyValueElement ret = obj.getElement(key);
            if (ret == null) {
                return Condition.KEY_DOES_NOT_EXIST;
            }
            return ret.getValue();
        }

        /**
         * @see org.appwork.moncompare.object.MapAccessorInterface#put(java.lang.String, java.lang.Object)
         */
        @Override
        public Object put(String key, Object value) {
            Object old = get(key);
            if (!(value instanceof FlexiJSonNode)) {
                try {
                    value = new FlexiJSonMapper().objectToJsonNode(value);
                } catch (FlexiMapperException e) {
                    throw new IllegalArgumentException(e);
                }
            }
            obj.add(new KeyValueElement(obj, key, (FlexiJSonNode) value));
            return old;
        }

        /**
         * @see org.appwork.moncompare.object.MapAccessorInterface#remove(java.lang.String)
         */
        @Override
        public Object remove(String key) {
            KeyValueElement ret = obj.remove(key);
            return ret == null ? Condition.KEY_DOES_NOT_EXIST : ret.getValue();
        }
    }

    /**
     * @see org.appwork.moncompare.TypeHandler#getMapAccessor(java.lang.Object)
     */
    @Override
    public MapAccessorInterface getMapAccessor(Object expression) {
        if (expression instanceof FlexiJSonObject) {
            return new FlexiMapAccessor((FlexiJSonObject) expression);
        }
        return null;
    }

    /**
     * @see org.appwork.moncompare.TypeHandler#newAutoCreateArray()
     */
    @Override
    public Object newAutoCreateArray() {
        return new FlexiJSonArray();
    }

    /**
     * @see org.appwork.moncompare.TypeHandler#newAutoCreateMap()
     */
    @Override
    public Object newAutoCreateMap() {
        return new FlexiJSonObject();
    }
}
