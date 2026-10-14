package org.jdownloader.controlling.contextmenu;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Example and help for a tag of a {@link Replacer}, e.g. for the menu editor. It has no influence on how the tag is replaced: The logic
 * is part of {@link Replacer#replace(String, Object)}.
 */
public final class Tag {
    private final List<String> keys;
    private final String       description;

    /**
     * @param keys
     *            Examples of the tag without braces, e.g. "hash_md5". The first one is the default. For tags with a variable part these
     *            are examples, e.g. "date_yyyy-MM-dd HH:mm:ss".
     */
    public Tag(final String[] keys, final String description) {
        final List<String> list = new ArrayList<String>();
        for (final String key : keys) {
            list.add(key);
        }
        this.keys = Collections.unmodifiableList(list);
        this.description = description;
    }

    public Tag(final String key, final String description) {
        this(new String[] { key }, description);
    }

    public List<String> getKeys() {
        return keys;
    }

    public String getDescription() {
        return description;
    }

    /**
     * @return The key as it is written into a pattern, e.g. "{hash_md5}"
     */
    public static String toPattern(final String key) {
        return "{" + key + "}";
    }
}
