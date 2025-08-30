package org.jdownloader.controlling.filter;

import java.util.regex.Pattern;

import org.appwork.storage.Storable;
import org.appwork.storage.StorableAllowPrivateAccessModifier;

public class CompiledRegexFilter extends RegexFilter implements Storable {
    @StorableAllowPrivateAccessModifier
    private CompiledRegexFilter() {
        pattern = null;
    }

    private final Pattern pattern;

    public CompiledRegexFilter(RegexFilter filter) {
        super(filter.enabled, filter.getMatchType(), filter.getRegex(), filter.isUseRegex());
        pattern = buildPattern();
    }

    public Pattern _getPattern() {
        return pattern;
    }

    private Boolean dynamicTags = null;

    protected Boolean getDynamicTags() {
        return dynamicTags;
    }

    protected void setDynamicTags(Boolean dynamicTags) {
        this.dynamicTags = dynamicTags;
    }

    public boolean matches(String string) {
        return matches(_getPattern(), string);
    }

    public boolean matches(Pattern pattern, String string) {
        final boolean ret;
        switch (getMatchType()) {
        case CONTAINS:
            ret = pattern.matcher(string).find();
            break;
        case EQUALS:
            ret = pattern.matcher(string).matches();
            break;
        case CONTAINS_NOT:
            ret = !pattern.matcher(string).find();
            break;
        case EQUALS_NOT:
            ret = !pattern.matcher(string).matches();
            break;
        default:
            ret = false;
            break;
        }
        return ret;
    }
}
