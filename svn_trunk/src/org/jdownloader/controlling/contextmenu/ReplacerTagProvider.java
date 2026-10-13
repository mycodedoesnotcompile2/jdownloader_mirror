package org.jdownloader.controlling.contextmenu;

import java.util.List;

/**
 * Can be implemented by an {@link ActionContext} that has customizable patterns. The menu editor then shows the available tags below the
 * matching text field.
 */
public interface ReplacerTagProvider {
    /**
     * @param customizerKey
     *            Key of the customizable setting (name of the getter without "get"), e.g. "patternLinks". Compare case insensitive.
     * @return The tags that can be used in this setting or null/empty if there are none
     */
    List<ReplacerTag> getReplacerTags(String customizerKey);
}
