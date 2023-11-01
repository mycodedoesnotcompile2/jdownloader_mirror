package org.appwork.utils;

import java.util.HashMap;

import org.appwork.storage.config.annotations.IntegerInterface;
import org.appwork.utils.CounterMap.WrappedInteger;

public class CounterMap<KeyType> extends HashMap<KeyType, WrappedInteger> {
    public static class WrappedInteger implements IntegerInterface {
        private int i = 0;

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.storage.config.annotations.IntegerInterface#getInt()
         */
        @Override
        public int getInt() {
            return i;
        }

        /**
         * @return
         */
        public int increment(int add) {
            i += add;
            return i;
        }

        /*
         * (non-Javadoc)
         *
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return String.valueOf(i);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.HashMap#get(java.lang.Object)
     */
    public WrappedInteger getEnsureValue(KeyType key) {
        WrappedInteger ret = super.get(key);
        if (ret == null) {
            ret = new WrappedInteger();
            super.put(key, ret);
        }
        return ret;
    }

    /**
     * @param createUniqueID
     */
    public int increment(KeyType key) {
        return add(key, 1);
    }

    public int decrement(KeyType key) {
        return add(key, -1);
    }

    /**
     * @param createUniqueID
     */
    public int add(KeyType key, int add) {
        WrappedInteger wrapper = getEnsureValue(key);
        return wrapper.increment(add);
    }

    /**
     * @param createUniqueID
     * @return
     */
    public int getInt(KeyType key) {
        WrappedInteger ret = get(key);
        if (ret != null) {
            return ret.getInt();
        }
        return 0;
    }

    /**
     * @return
     */
    public HashMap<KeyType, Integer> toMap() {
        HashMap<KeyType, Integer> ret = new HashMap<KeyType, Integer>();
        for (java.util.Map.Entry<KeyType, WrappedInteger> es : entrySet()) {
            ret.put(es.getKey(), es.getValue().getInt());
        }
        return ret;
    }
}
