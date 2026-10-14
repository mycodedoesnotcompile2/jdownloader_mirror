package org.jdownloader.controlling.contextmenu;

import java.util.List;

/**
 * A tag (or a group of related tags) that can be used in a pattern, e.g. <code>{hash_md5}</code>. An implementation contains everything
 * that belongs to the tag: the definition for the menu editor (possible replaces, description, text for missing data) and the logic that
 * creates the value. Use {@link ReplacerTagDefinition#replace(List, String, Object)} to apply a list of tags to a pattern.
 */
public interface ReplacerTag {
    /**
     * @return Possible replaces without braces, e.g. "hash_md5". The first one is the default. For tags with a variable part this are
     *         examples, e.g. "date_yyyy-MM-dd HH:mm:ss".
     */
    List<String> getReplaces();

    String getDescription();

    /**
     * @param replace
     *            One of the replaces or the tag as written in the pattern
     * @return The text that is used if there is no data for the replace
     */
    String getEmptyCharacter(String replace);

    /**
     * @return true if the replace always has a value, so that a text for missing data makes no sense
     */
    boolean isAlwaysAvailable(String replace);

    /**
     * Builds the tag as it is written into the pattern. E.g. "{hash_md5}".
     */
    String toTag(String replace, String emptyCharacterToUse);

    /**
     * @param replace
     *            The tag without braces and without the text for missing data, e.g. "hash_md5"
     * @param object
     *            The object the pattern is applied to, e.g. a link or a package. The replacer decides itself whether it can work with it.
     * @return The value, an empty string if there is no data, or null if this replacer does not handle the tag or the object. Tags that
     *         nobody handles stay unchanged in the pattern.
     */
    String apply(String replace, Object object);
}
