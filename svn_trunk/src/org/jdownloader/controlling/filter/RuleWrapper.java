package org.jdownloader.controlling.filter;

import java.io.File;
import java.net.URL;
import java.util.regex.Pattern;

import org.appwork.utils.StringUtils;
import org.jdownloader.controlling.filter.RegexFilter.MatchType;
import org.jdownloader.myjdownloader.client.json.AvailableLinkState;

import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcollector.LinkOriginDetails;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.LinkCrawler;
import jd.plugins.DownloadLink;
import jd.plugins.LinkInfo;

public class RuleWrapper<T extends FilterRule> {
    private final CompiledRegexFilter        fileNameRule;
    private final CompiledPluginStatusFilter pluginStatusFilter;
    private final BooleanFilter              alwaysFilter;
    private final CompiledOriginFilter       originFilter;
    private final CompiledRegexFilter        packageNameRule;
    private final CompiledConditionFilter    conditionFilter;

    public CompiledPluginStatusFilter getPluginStatusFilter() {
        return pluginStatusFilter;
    }

    public RuleWrapper(T rule2) {
        this.rule = rule2;
        boolean requiresHoster = false;
        if (rule.getPluginStatusFilter().isEnabled()) {
            pluginStatusFilter = new CompiledPluginStatusFilter(rule.getPluginStatusFilter());
            requiresHoster = true;
        } else {
            pluginStatusFilter = null;
        }
        if (rule.getOnlineStatusFilter().isEnabled()) {
            onlineStatusFilter = new CompiledOnlineStatusFiler(rule.getOnlineStatusFilter());
        } else {
            onlineStatusFilter = null;
        }
        if (rule.getFilenameFilter().isEnabled()) {
            fileNameRule = new CompiledRegexFilter(rule.getFilenameFilter());
        } else {
            fileNameRule = null;
        }
        if (rule.getPackagenameFilter().isEnabled()) {
            packageNameRule = new CompiledRegexFilter(rule.getPackagenameFilter());
        } else {
            packageNameRule = null;
        }
        if (rule.getFilesizeFilter().isEnabled()) {
            filesizeRule = new CompiledFilesizeFilter(rule.getFilesizeFilter());
        } else {
            filesizeRule = null;
        }
        if (rule.getFiletypeFilter().isEnabled()) {
            filetypeFilter = new CompiledFiletypeFilter(rule.getFiletypeFilter());
        } else {
            filetypeFilter = null;
        }
        if (rule.getHosterURLFilter().isEnabled()) {
            hosterRule = new CompiledRegexFilter(rule.getHosterURLFilter());
            requiresHoster = true;
        } else {
            hosterRule = null;
        }
        if (rule.getSourceURLFilter().isEnabled()) {
            sourceRule = new CompiledRegexFilter(rule.getSourceURLFilter());
        } else {
            sourceRule = null;
        }
        if (rule.getOriginFilter().isEnabled()) {
            originFilter = new CompiledOriginFilter(rule.getOriginFilter());
        } else {
            originFilter = null;
        }
        if (rule.getConditionFilter().isEnabled()) {
            conditionFilter = new CompiledConditionFilter(rule.getConditionFilter());
        } else {
            conditionFilter = null;
        }
        if (rule.getMatchAlwaysFilter().isEnabled()) {
            alwaysFilter = rule.getMatchAlwaysFilter();
            // overwrites all others
            requiresHoster = false;
        } else {
            alwaysFilter = null;
        }
        this.requiresHoster = requiresHoster;
    }

    public CompiledConditionFilter getConditionFilter() {
        return conditionFilter;
    }

    public CompiledOriginFilter getOriginFilter() {
        return originFilter;
    }

    public BooleanFilter getAlwaysFilter() {
        return alwaysFilter;
    }

    public CompiledRegexFilter getFileNameRule() {
        return fileNameRule;
    }

    public CompiledRegexFilter getPackageNameRule() {
        return packageNameRule;
    }

    public boolean isRequiresHoster() {
        return requiresHoster;
    }

    public CompiledRegexFilter getHosterRule() {
        return hosterRule;
    }

    public CompiledRegexFilter getSourceRule() {
        return sourceRule;
    }

    public CompiledFilesizeFilter getFilesizeRule() {
        return filesizeRule;
    }

    public CompiledFiletypeFilter getFiletypeFilter() {
        return filetypeFilter;
    }

    private final boolean                   requiresHoster;
    private final CompiledRegexFilter       hosterRule;
    private final CompiledRegexFilter       sourceRule;
    private final CompiledFilesizeFilter    filesizeRule;
    private final CompiledFiletypeFilter    filetypeFilter;
    private final T                         rule;
    private final CompiledOnlineStatusFiler onlineStatusFilter;

    public T getRule() {
        return rule;
    }

    public CompiledOnlineStatusFiler getOnlineStatusFilter() {
        return onlineStatusFilter;
    }

    public static enum AUTO_PATTERN_MODE {
        FINDS,
        MATCHES,
        WILDCARD
    }

    public static Pattern createPattern(String regex, final boolean useRegex, final AUTO_PATTERN_MODE mode) {
        if (useRegex) {
            int flags = 0;
            if (regex != null && !regex.contains("(?i)") && !regex.contains("(?-i)")) {
                flags = flags | Pattern.CASE_INSENSITIVE;
            }
            if (regex != null && !regex.contains("(?s)") && !regex.contains("(?-s)")) {
                flags = flags | Pattern.DOTALL;
            }
            return Pattern.compile(regex, flags);
        } else {
            // https://en.wikipedia.org/wiki/Wildcard_character
            // *, it matches zero or more characters.
            // ?, it matches exactly one character.
            final String[] parts = regex.split("\\*+");
            final StringBuilder sb = new StringBuilder();
            boolean containsWildcard = false;
            boolean containsQuestionMark = false;
            if (regex.startsWith("*")) {
                containsWildcard = true;
                sb.append("(.*)");
            }
            int nonEmptyParts = 0;
            final StringBuilder partSb = new StringBuilder();
            for (int i = 0; i < parts.length; i++) {
                if (parts[i].length() != 0) {
                    if (nonEmptyParts > 0) {
                        containsWildcard = true;
                        sb.append("(.*?)");
                    }
                    final String part = parts[i];
                    for (int index = 0; index < part.length(); index++) {
                        final char c = part.charAt(index);
                        if (c == '?') {
                            if (partSb.length() > 0) {
                                sb.append(Pattern.quote(partSb.toString()));
                                partSb.setLength(0);
                            }
                            containsQuestionMark = true;
                            sb.append(".");
                        } else {
                            partSb.append(c);
                        }
                    }
                    if (partSb.length() > 0) {
                        sb.append(Pattern.quote(partSb.toString()));
                        partSb.setLength(0);
                    }
                    nonEmptyParts++;
                }
            }
            if (sb.length() == 0) {
                sb.append("(.*)");
            } else if (nonEmptyParts > 0) {
                if (regex.endsWith("?") && false) {
                    containsQuestionMark = true;
                    // this special handling is disabled, maybe add advanced setting for it
                    // ?, it matches exactly one character. If the question mark is placed at the end of the word, it will also match
                    // missing (zero) trailing characters.
                    if (sb.charAt(sb.length() - 1) == '.') {
                        sb.append("?");
                    } else {
                        sb.append(".");
                        sb.append("?");
                    }
                } else if (regex.endsWith("*")) {
                    containsWildcard = true;
                    sb.append("(.*)");
                }
                if (AUTO_PATTERN_MODE.MATCHES.equals(mode)) {
                    sb.insert(0, "^");
                    sb.append("$");
                } else if ((containsWildcard || containsQuestionMark) && AUTO_PATTERN_MODE.WILDCARD.equals(mode)) {
                    sb.insert(0, "^");
                    sb.append("$");
                }
            }
            return Pattern.compile(sb.toString(), Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
        }
    }

    public boolean checkFileType(final CrawledLink link) {
        final CompiledFiletypeFilter filetypeFilter = getFiletypeFilter();
        if (filetypeFilter != null) {
            final String url = link.getURL();
            final DownloadLink downloadLink = link.getDownloadLink();
            if (downloadLink != null) {
                final LinkInfo linkInfo = link.getLinkInfo();
                if (downloadLink.getFinalFileName() != null || downloadLink.getForcedFileName() != null) {
                    // filename available
                    return filetypeFilter.matches(linkInfo.getExtension().name(), linkInfo);
                } else if (link.getLinkState() == AvailableLinkState.ONLINE) {
                    // file is online
                    return filetypeFilter.matches(linkInfo.getExtension().name(), linkInfo);
                } else if (checkOnlineStatus(link)) {
                    // onlinestatus matches so we trust the available filename
                    return filetypeFilter.matches(linkInfo.getExtension().name(), linkInfo);
                } else {
                    return false;
                }
            } else if (StringUtils.startsWithCaseInsensitive(url, "file:")) {
                try {
                    final File file = new File(new URL(url).toURI());
                    final LinkInfo linkInfo = LinkInfo.getLinkInfo(file);
                    return filetypeFilter.matches(linkInfo.getExtension().name(), linkInfo);
                } catch (final Exception e) {
                    return false;
                }
            } else {
                return false;
            }
        }
        return true;
    }

    public boolean checkFileSize(final CrawledLink link) {
        final CompiledFilesizeFilter fileSizeRule = getFilesizeRule();
        if (fileSizeRule != null) {
            final String url = link.getURL();
            final DownloadLink downloadLink = link.getDownloadLink();
            if (downloadLink != null) {
                if (downloadLink.getVerifiedFileSize() >= 0) {
                    return fileSizeRule.matches(downloadLink.getVerifiedFileSize());
                } else if (link.getLinkState() == AvailableLinkState.ONLINE) {
                    return fileSizeRule.matches(link.getSize());
                } else if (checkOnlineStatus(link)) {
                    return fileSizeRule.matches(link.getSize());
                } else {
                    return false;
                }
            } else if (StringUtils.startsWithCaseInsensitive(url, "file:")) {
                try {
                    final File file = new File(new URL(url).toURI());
                    return fileSizeRule.matches(file.length());
                } catch (final Exception e) {
                    return false;
                }
            } else {
                return false;
            }
        } else {
            return true;
        }
    }

    public boolean checkPackageName(final CrawledLink link) {
        final CompiledRegexFilter packageNameRule = getPackageNameRule();
        if (packageNameRule != null) {
            String packagename = null;
            if (link != null) {
                if (link.getParentNode() != null) {
                    packagename = link.getParentNode().getName();
                }
                if (StringUtils.isEmpty(packagename) && link.getDesiredPackageInfo() != null) {
                    packagename = link.getDesiredPackageInfo().getName();
                }
            }
            if (StringUtils.isEmpty(packagename)) {
                return false;
            } else {
                return packageNameRule.matches(packagename);
            }
        } else {
            return true;
        }
    }

    public boolean checkFileName(final CrawledLink link) {
        final CompiledRegexFilter fileNameRule = getFileNameRule();
        if (fileNameRule != null) {
            final String url = link.getURL();
            final DownloadLink downloadLink = link.getDownloadLink();
            if (downloadLink != null) {
                if (downloadLink.getFinalFileName() != null || downloadLink.getForcedFileName() != null) {
                    // final or forced filename available
                    return fileNameRule.matches(link.getName());
                } else if (link.getLinkState() == AvailableLinkState.ONLINE) {
                    // file is online
                    return fileNameRule.matches(link.getName());
                } else if (checkOnlineStatus(link)) {
                    // onlinestatus matches so we trust the available filename
                    return fileNameRule.matches(link.getName());
                } else {
                    return false;
                }
            } else if (StringUtils.startsWithCaseInsensitive(url, "file:")) {
                try {
                    final File file = new File(new URL(url).toURI());
                    return fileNameRule.matches(file.getName());
                } catch (final Exception e) {
                    return false;
                }
            } else {
                return false;
            }
        } else {
            return true;
        }
    }

    public boolean checkHoster(final CrawledLink link) {
        final CompiledRegexFilter hosterRule = getHosterRule();
        if (hosterRule != null) {
            final DownloadLink dlLink = link.getDownloadLink();
            if (dlLink == null || link.gethPlugin() == null) {
                return false;
            } else {
                final String host = dlLink.getServiceHost(true);
                switch (hosterRule.getMatchType()) {
                case CONTAINS:
                case EQUALS:
                    return (host != null && hosterRule.matches(host)) || hosterRule.matches(dlLink.getContentUrlOrPatternMatcher());
                case CONTAINS_NOT:
                case EQUALS_NOT:
                    return (host == null || hosterRule.matches(host)) && hosterRule.matches(dlLink.getContentUrlOrPatternMatcher());
                default:
                    return false;
                }
            }
        } else {
            return true;
        }
    }

    public boolean checkSource(CrawledLink link) {
        final CompiledRegexFilter sourceRule = getSourceRule();
        if (sourceRule != null) {
            String[] sources = link.getSourceUrls();
            int i = 1;
            final String pattern = sourceRule._getPattern().pattern();
            final boolean indexed = pattern.matches("^\\-?\\d+\\\\\\. .+");
            final boolean inverted = indexed && pattern.startsWith("-");
            if (sources == null || sources.length == 0) {
                /* the first link never has sourceURLs */
                sources = new String[2];
                sources[0] = LinkCrawler.cleanURL(link.getURL());
                LinkCollectingJob job = link.getSourceJob();
                if (job != null) {
                    sources[1] = LinkCrawler.cleanURL(job.getCustomSourceUrl());
                }
            }
            final MatchType matchType = sourceRule.getMatchType();
            Boolean matches = null;
            for (int j = inverted ? 0 : sources.length - 1; (inverted ? (j < sources.length) : (j >= 0)); j = (inverted ? (j + 1) : (j - 1))) {
                final String url = sources[j];
                if (url != null) {
                    final String toMatch = indexed ? (inverted ? "-" : "") + (i++) + ". " + url : url;
                    if (indexed) {
                        switch (sourceRule.getMatchType()) {
                        case EQUALS:
                        case EQUALS_NOT:
                            if (sourceRule.matches(url)) {
                                return true;
                            }
                            break;
                        default:
                            // nothing
                            break;
                        }
                    } else {
                        final boolean match = sourceRule.matches(toMatch);
                        switch (matchType) {
                        case CONTAINS:
                            if (match) {
                                return true;
                            }
                            break;
                        case EQUALS:
                            return match;
                        case CONTAINS_NOT:
                        case EQUALS_NOT:
                            if (matches == null) {
                                matches = match;
                            } else {
                                matches = matches.booleanValue() && match;
                            }
                            break;
                        }
                    }
                }
            }
            if (matches != null) {
                return matches.booleanValue();
            } else {
                return false;
            }
        }
        return true;
    }

    public boolean checkOnlineStatus(final CrawledLink link) {
        final CompiledOnlineStatusFiler onlineStatusFilter = getOnlineStatusFilter();
        if (onlineStatusFilter != null) {
            final AvailableLinkState linkState = link.getLinkState();
            return onlineStatusFilter.matches(linkState);
        } else {
            return true;
        }
    }

    public boolean checkConditions(final CrawledLink link) {
        final CompiledConditionFilter conditionFiler = getConditionFilter();
        if (conditionFiler != null) {
            return conditionFiler.matches(link);
        } else {
            return true;
        }
    }

    public boolean checkOrigin(final CrawledLink link) {
        final CompiledOriginFilter originFiler = getOriginFilter();
        if (originFiler != null) {
            final LinkOriginDetails origin = link.getOrigin();
            if (origin == null) {
                return false;
            } else {
                return originFiler.matches(origin.getOrigin(), link);
            }
        } else {
            return true;
        }
    }

    public boolean checkPluginStatus(final CrawledLink link) {
        final CompiledPluginStatusFilter pluginStatusFilter = getPluginStatusFilter();
        if (pluginStatusFilter != null) {
            if (link.getDownloadLink() == null || link.gethPlugin() == null) {
                return false;
            } else {
                return pluginStatusFilter.matches(link);
            }
        } else {
            return true;
        }
    }

    public String getName() {
        return rule.getName();
    }

    public boolean isEnabled() {
        return rule.isEnabled();
    }

    public boolean isStopAfterThisRule() {
        // TODO: Add functionality
        return false;
    }
}
