package org.jdownloader.controlling.contextmenu;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Static helpers for {@link Replacer}s.
 */
public class Replacers {
    /* {tag} */
    private static final Pattern TAG_PATTERN = Pattern.compile("\\{([^{}]+)\\}");

    /**
     * Provides the value of a single tag. Used by {@link Replacers#replaceTags(String, TagResolver)}.
     */
    public interface TagResolver {
        /**
         * @param key
         *            The tag without braces, e.g. "hash_md5"
         * @return The value, an empty string if there is no data, or null if the tag is not handled. It then stays unchanged.
         */
        String resolve(String key);
    }

    /**
     * Applies the replacers to the pattern. The pattern is parsed once and every tag is given on its own to the replacers, in the order
     * of the list. The first replacer that changes it provides the value. A replaced value is never given to a replacer again, so tags
     * contained in a value (e.g. in a file name) are not replaced. Tags that no replacer handles stay unchanged.
     *
     * @return The pattern with all tags replaced or null if the pattern is null
     */
    public static String replace(final List<? extends Replacer> replacers, final String input, final Object object) {
        if (input == null) {
            return null;
        }
        final Matcher matcher = TAG_PATTERN.matcher(input);
        final StringBuffer sb = new StringBuffer();
        while (matcher.find()) {
            final String tag = matcher.group(0);
            String value = tag;
            for (final Replacer replacer : replacers) {
                final String replaced = replacer.replace(tag, object);
                if (replaced != null && !tag.equals(replaced)) {
                    value = replaced;
                    break;
                }
            }
            matcher.appendReplacement(sb, Matcher.quoteReplacement(value));
        }
        matcher.appendTail(sb);
        return sb.toString();
    }

    /**
     * Helper for the implementation of {@link Replacer#replace(String, Object)}: Finds all tags in the input and replaces them with the
     * value of the resolver. The input is parsed once, so tags contained in a replaced value are not replaced again.
     */
    public static String replaceTags(final String input, final TagResolver resolver) {
        final Matcher matcher = TAG_PATTERN.matcher(input);
        final StringBuffer sb = new StringBuffer();
        while (matcher.find()) {
            final String value = resolver.resolve(matcher.group(1));
            matcher.appendReplacement(sb, Matcher.quoteReplacement(value != null ? value : matcher.group(0)));
        }
        matcher.appendTail(sb);
        return sb.toString();
    }
}
