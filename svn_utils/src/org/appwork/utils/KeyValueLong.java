package org.appwork.utils;

import java.util.Map.Entry;

import org.appwork.storage.Storable;

public class KeyValueLong extends KeyValueEntry<Long, Long> implements Storable {
    /**
     *
     */
    public KeyValueLong(/* Storable */) {
        super();
    }

    public KeyValueLong(Long key, Long value) {
        super(key, value);
    }

    /**
     * @param entry
     */
    public KeyValueLong(Entry<Long, Long> entry) {
        super(entry);
    }
}
