package org.appwork.utils;

import java.util.Map.Entry;

import org.appwork.storage.Storable;

public class KeyValueStringEntry extends KeyValueEntry<String, String> implements Storable {
    /**
     *
     */
    public KeyValueStringEntry(/* Storable */) {
        super();
    }

    public KeyValueStringEntry(String key, String value) {
        super(key, value);
    }

    /**
     * @param entry
     */
    public KeyValueStringEntry(Entry<String, String> entry) {
        super(entry);
    }
}
