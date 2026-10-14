package org.jdownloader.controlling.contextmenu;

import java.util.List;

/**
 * Can be implemented by an {@link ActionContext} that has customizable patterns. The menu editor then shows the available tags below the
 * matching text field.
 */
public interface ReplacerProvider {
    /**
     * @param customizerKey
     *            Key of the customizable setting (name of the getter without "get"), e.g. "patternLinks". Compare case insensitive.
     * @return The replacers whose tags can be used in this setting or null/empty if there are none
     */
    List<Replacer> getReplacers(String customizerKey);
}
