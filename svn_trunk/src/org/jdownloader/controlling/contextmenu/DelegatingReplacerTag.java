package org.jdownloader.controlling.contextmenu;

import java.util.List;

/**
 * Passes everything to another {@link ReplacerTag}. Extend it and override {@link #apply(String, Object)} to change the behavior of single
 * tags, e.g. for a single run with infos that are not part of the object the pattern is applied to.
 */
public class DelegatingReplacerTag implements ReplacerTag {
    private final ReplacerTag delegate;

    public DelegatingReplacerTag(final ReplacerTag delegate) {
        this.delegate = delegate;
    }

    @Override
    public List<String> getReplaces() {
        return delegate.getReplaces();
    }

    @Override
    public String getDescription() {
        return delegate.getDescription();
    }

    @Override
    public String getEmptyCharacter(final String replace) {
        return delegate.getEmptyCharacter(replace);
    }

    @Override
    public boolean isAlwaysAvailable(final String replace) {
        return delegate.isAlwaysAvailable(replace);
    }

    @Override
    public String toTag(final String replace, final String emptyCharacterToUse) {
        return delegate.toTag(replace, emptyCharacterToUse);
    }

    @Override
    public String apply(final String replace, final Object object) {
        return delegate.apply(replace, object);
    }
}
