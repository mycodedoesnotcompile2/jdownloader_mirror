package org.jdownloader.controlling.contextmenu;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;

/**
 * The definition of a {@link ReplacerTag}: possible replaces, description, the text for missing data and which replaces always have a
 * value. An implementation of {@link ReplacerTag} can use it for the definition methods and only has to add the logic. It also contains
 * {@link #replace(List, String, Object)}, which applies a list of tags to a pattern.
 */
public class ReplacerTagDefinition {
    /* {tag} */
    private static final Pattern TAG_PATTERN = Pattern.compile("\\{([^{}]+)\\}");

    /**
     * Replaces all tags of the pattern in a single pass. The first replacer that handles a tag provides its value. If there is no data,
     * the default text of the tag is used. Tags that no replacer handles are left untouched. Because the
     * pattern is only parsed once, tags contained in replaced values (e.g. in a file name) are not replaced again.
     *
     * @param tags
     *            The replacers
     * @param pattern
     *            The pattern, e.g. "{name};{hash_md5}"
     * @param input
     *            The object the replacers get the data from
     * @return The pattern with all tags replaced or null if the pattern is null
     */
    public static String replace(final List<? extends ReplacerTag> tags, final String pattern, final Object input) {
        if (pattern == null) {
            return null;
        }
        final Matcher matcher = TAG_PATTERN.matcher(pattern);
        final StringBuffer sb = new StringBuffer();
        while (matcher.find()) {
            final String key = matcher.group(1);
            String replacement = matcher.group(0);
            for (final ReplacerTag tag : tags) {
                final String value = tag.apply(key, input);
                if (value == null) {
                    /* This replacer does not handle the tag or the input */
                    continue;
                }
                if (StringUtils.isNotEmpty(value)) {
                    replacement = value;
                } else {
                    replacement = tag.getEmptyCharacter(key);
                }
                break;
            }
            matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement));
        }
        matcher.appendTail(sb);
        return sb.toString();
    }

    private final List<String>        replaces;
    private final String              description;
    private final String              emptyCharacter;
    private final Map<String, String> emptyCharacterByReplace = new HashMap<String, String>();
    private boolean                   alwaysAvailable         = false;
    private final Set<String>         alwaysAvailableReplaces = new HashSet<String>();

    /**
     * Tag with a single replace.
     */
    public ReplacerTagDefinition(final String replace, final String description, final String emptyCharacter) {
        this(new String[] { replace }, description, emptyCharacter);
    }

    /**
     * @param replaces
     *            Possible replaces without braces, e.g. "hash_md5". The first one is the default.
     * @param description
     *            Description of the tag(s)
     * @param emptyCharacter
     *            Default text that is used if there is no data, e.g. "" (empty), "~", "0" or "-1"
     */
    public ReplacerTagDefinition(final String[] replaces, final String description, final String emptyCharacter) {
        this.description = description;
        this.emptyCharacter = emptyCharacter;
        final List<String> list = new ArrayList<String>();
        for (final String replace : replaces) {
            list.add(replace);
        }
        this.replaces = Collections.unmodifiableList(list);
    }

    public List<String> getReplaces() {
        return replaces;
    }

    public String getDescription() {
        return description;
    }

    /**
     * Sets a different default empty character for one of the replaces, e.g. "-1" for the raw file size within the file size tag.
     */
    public ReplacerTagDefinition setEmptyCharacter(final String replace, final String emptyCharacter) {
        emptyCharacterByReplace.put(replace, emptyCharacter);
        return this;
    }

    public String getEmptyCharacter(final String replace) {
        final String ret = emptyCharacterByReplace.get(replace);
        if (ret != null) {
            return ret;
        } else {
            return emptyCharacter;
        }
    }

    /**
     * Marks a tag that always has a value (there is no case of missing data), e.g. {tab} or {type}.
     */
    public ReplacerTagDefinition setAlwaysAvailable(final boolean alwaysAvailable) {
        this.alwaysAvailable = alwaysAvailable;
        return this;
    }

    /**
     * Marks only some of the replaces as always having a value, for tags that mix both kinds (e.g. the current date and the date added).
     */
    public ReplacerTagDefinition setAlwaysAvailable(final String... replaces) {
        for (final String replace : replaces) {
            alwaysAvailableReplaces.add(replace);
        }
        return this;
    }

    public boolean isAlwaysAvailable(final String replace) {
        return alwaysAvailable || alwaysAvailableReplaces.contains(replace);
    }

    /**
     * @return true if the given tag is one of the replaces. Tags with a variable part, e.g. dates or properties, need their own check.
     */
    public boolean isResponsibleFor(final String key) {
        return replaces.contains(key);
    }

    /**
     * Builds the tag as it is written into the pattern. A text for missing data cannot be set in the pattern, so it is ignored.
     */
    public String toTag(final String replace, final String emptyCharacterToUse) {
        return "{" + replace + "}";
    }
}
