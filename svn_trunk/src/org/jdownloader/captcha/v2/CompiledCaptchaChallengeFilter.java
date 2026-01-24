package org.jdownloader.captcha.v2;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.Challenge.CaptchaRequestType;

import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.Plugin;

public class CompiledCaptchaChallengeFilter {
    private final CaptchaChallengeFilter filter;
    private final Pattern                domainPattern;

    public CompiledCaptchaChallengeFilter(final CaptchaChallengeFilter filter) {
        this.filter = filter;
        Pattern compiledPattern = null;
        if (filter.isRegex() && filter.getDomain() != null) {
            try {
                compiledPattern = Pattern.compile(filter.getDomain(), Pattern.CASE_INSENSITIVE);
            } catch (final Exception e) {
                /* Mark filter as broken if regex compilation fails */
                filter._setBroken(true);
                e.printStackTrace();
            }
        }
        this.domainPattern = compiledPattern;
    }

    public CaptchaChallengeFilter getFilter() {
        return filter;
    }

    public Pattern getDomainPattern() {
        return domainPattern;
    }

    /**
     * Returns true if filter is valid and can be used for matching.
     */
    public boolean isValid() {
        return filter._isValid();
    }

    /**
     * Checks if this filter matches the given challenge.
     *
     * @param c
     *            Challenge to check
     * @return true if filter matches, false otherwise
     */
    public boolean matches(final Challenge<?> c) {
        if (!isValid()) {
            return false;
        }
        /* Check domain match */
        if (!matchesDomain(c)) {
            return false;
        }
        /* Check captcha type match */
        if (!matchesCaptchaType(c)) {
            return false;
        }
        /* Check request type match */
        if (!matchesRequestType(c)) {
            return false;
        }
        /* All criteria match */
        return true;
    }

    /**
     * Checks if filter's domain pattern matches any of the challenge's domains.
     */
    public boolean matchesDomain(final Challenge<?> c) {
        final Set<String> hosts = getHostsForChallenge(c);
        for (final String host : hosts) {
            if (matchesSingleDomain(c, host)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Collects all possible host identifiers for the given challenge.
     */
    private Set<String> getHostsForChallenge(final Challenge<?> c) {
        final Set<String> hosts = new HashSet<String>();
        hosts.add(c.getHost());
        final Plugin plugin = c.getPlugin();
        if (plugin != null) {
            final String[] siteSupportedNames = plugin.siteSupportedNames();
            if (siteSupportedNames != null) {
                hosts.addAll(Arrays.asList(siteSupportedNames));
            }
        }
        return hosts;
    }

    /**
     * Checks if filter matches a single domain string.
     */
    private boolean matchesSingleDomain(final Challenge<?> c, final String host) {
        try {
            final String domainOrRegex = filter.getDomain();
            if (StringUtils.isEmpty(domainOrRegex)) {
                return false;
            }
            if (filter.isRegex()) {
                /* Regex matching */
                if (domainPattern == null) {
                    /* Pattern compilation failed */
                    return false;
                }
                if (domainPattern.matcher(host).matches()) {
                    return true;
                }
            } else {
                /* Plain domain matching (case-insensitive) */
                if (StringUtils.equalsIgnoreCase(domainOrRegex, host)) {
                    return true;
                }
            }
        } catch (final Throwable e) {
            if (c.getPlugin() != null) {
                c.getPlugin().getLogger().log(e);
            } else {
                e.printStackTrace();
            }
        }
        return false;
    }

    /**
     * Checks if filter's captcha type list matches the challenge's captcha type. Empty filter list means "match all types".
     */
    public boolean matchesCaptchaType(final Challenge<?> c) {
        final Set<CAPTCHA_TYPE> filterTypes = filter.getCaptchaTypes();
        if (filterTypes == null || filterTypes.isEmpty()) {
            /* Empty list means match all types */
            return true;
        }
        final CAPTCHA_TYPE captchaType = CAPTCHA_TYPE.getCaptchaTypeForChallenge(c);
        if (captchaType == null) {
            /* Challenge has no captcha type but filter requires specific types */
            return false;
        }
        return filterTypes.contains(captchaType);
    }

    /**
     * Checks if filter's request type list matches the challenge's request type. Empty filter list means "match all request types".
     */
    public boolean matchesRequestType(final Challenge<?> c) {
        final Set<CaptchaRequestType> filterRequestTypes = filter.getCaptchaRequestTypes();
        if (filterRequestTypes == null || filterRequestTypes.isEmpty()) {
            /* Empty list means match all request types */
            return true;
        }
        final CaptchaRequestType requestType = c.getCaptchaRequestType();
        if (requestType == null) {
            /* Challenge has no request type but filter requires specific types */
            return false;
        }
        return filterRequestTypes.contains(requestType);
    }

    /**
     * Returns the filter type (BLACKLIST or WHITELIST).
     */
    public CaptchaChallengeFilter.CaptchaFilterType getFilterType() {
        return filter.getFilterType();
    }

    @Override
    public String toString() {
        return "CompiledCaptchaChallengeFilter[" + filter.toString() + "]";
    }
}