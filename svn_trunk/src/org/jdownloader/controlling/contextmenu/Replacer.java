package org.jdownloader.controlling.contextmenu;

import java.util.List;

/**
 * Replaces tags like <code>{hash_md5}</code> in a pattern. An implementation contains everything that belongs to its tags: the tags with
 * examples and help, and the logic that replaces them. Use {@link Replacers#replace(List, String, Object)} to apply a list of replacers.
 */
public interface Replacer {
    /**
     * @return The tags this replacer can replace. Only examples and help, e.g. for the menu editor. May be empty.
     */
    List<Tag> getTags();

    /**
     * Replaces all tags this replacer knows. Everything else, including tags that are unknown or that cannot be used with the given
     * object, has to stay unchanged.
     *
     * @param input
     *            The pattern, e.g. "{name};{hash_md5}"
     * @param object
     *            The object the pattern is applied to, e.g. a link or a package. The replacer decides itself whether it can work with it.
     * @return The pattern with the tags of this replacer replaced. Tags without data are replaced by an empty string.
     */
    String replace(String input, Object object);
}
