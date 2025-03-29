package jd.controlling.linkcollector;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.annotations.AbstractValidator;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.storage.config.handler.ObjectKeyHandler;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter.ArchiveExtensions;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter.ExtensionsFilterInterface;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;

import jd.plugins.ParsedFilename;

public class LinknameCleaner {
    public static final Pattern   pat0     = Pattern.compile("(.*)(\\.|_|-)pa?r?t?\\.?[0-9]+.(rar|rev|exe)($|\\.html?)", Pattern.CASE_INSENSITIVE);
    public static final Pattern   pat1     = Pattern.compile("(.*)(\\.|_|-)part\\.?[0]*[1].(rar|rev|exe)($|\\.html?)", Pattern.CASE_INSENSITIVE);
    public static final Pattern   pat3     = Pattern.compile("(.*)\\.(?:rar|rev)($|\\.html?)", Pattern.CASE_INSENSITIVE);
    public static final Pattern   pat4     = Pattern.compile("(.*)\\.r\\d+($|\\.html?)", Pattern.CASE_INSENSITIVE);
    public static final Pattern   pat5     = Pattern.compile("(.*)(\\.|_|-)\\d+($|\\.html?)", Pattern.CASE_INSENSITIVE);
    public static final Pattern   par2     = Pattern.compile("(.*?)(\\.vol\\d+\\.par2$|\\.vol\\d+(?:\\+|-)\\d+\\.par2$|\\.par2$)", Pattern.CASE_INSENSITIVE);
    public static final Pattern   par      = Pattern.compile("(.*?)(\\.p\\d+$|\\.par$)", Pattern.CASE_INSENSITIVE);
    public static final Pattern[] rarPats  = new Pattern[] { pat0, pat1, pat3, pat4, pat5, par2, par };
    public static final Pattern   pat6     = Pattern.compile("(.*)\\.zip($|\\.html?)", Pattern.CASE_INSENSITIVE);
    public static final Pattern   pat7     = Pattern.compile("(.*)\\.z\\d+($|\\.html?)", Pattern.CASE_INSENSITIVE);
    public static final Pattern   pat8     = Pattern.compile("(?is).*\\.7z\\.[\\d]+($|\\.html?)", Pattern.CASE_INSENSITIVE);
    public static final Pattern   pat9     = Pattern.compile("(.*)\\.a.($|\\.html?)", Pattern.CASE_INSENSITIVE);
    public static final Pattern[] zipPats  = new Pattern[] { pat6, pat7, pat8, pat9 };
    public static final Pattern   pat10    = Pattern.compile("(.*)\\._((_[a-z]{1})|([a-z]{2}))(\\.|$)");
    public static Pattern         pat11    = null;
    public static Pattern[]       ffsjPats = null;
    static {
        try {
            /* this should be done on a better way with next major update */
            pat11 = Pattern.compile("(.+)(" + jd.plugins.hoster.DirectHTTP.ENDINGS + "$)", Pattern.CASE_INSENSITIVE);
            ffsjPats = new Pattern[] { pat10, pat11 };
        } catch (final Throwable e) {
            /* not loaded yet */
        }
    }
    public static final Pattern   pat13   = Pattern.compile("(part\\d+)", Pattern.CASE_INSENSITIVE);
    public static final Pattern   pat17   = Pattern.compile("(.+)\\.\\d+\\.xtm($|\\.html?)");
    public static final Pattern   pat18   = Pattern.compile("(.*)\\.isz($|\\.html?)", Pattern.CASE_INSENSITIVE);
    public static final Pattern   pat19   = Pattern.compile("(.*)\\.i\\d{2}$", Pattern.CASE_INSENSITIVE);
    public static final Pattern[] iszPats = new Pattern[] { pat18, pat19 };

    public static enum EXTENSION_SETTINGS {
        KEEP,
        REMOVE_KNOWN,
        REMOVE_ALL
    }

    private static volatile Map<Pattern, String> FILENAME_REPLACEMAP         = new HashMap<Pattern, String>();
    private static volatile Map<String, String>  FILENAME_REPLACEMAP_DEFAULT = new HashMap<String, String>();
    static {
        final ObjectKeyHandler replaceMapKeyHandler = CFG_GENERAL.FILENAME_CHARACTER_REGEX_REPLACEMAP;
        FILENAME_REPLACEMAP_DEFAULT = (Map<String, String>) replaceMapKeyHandler.getDefaultValue();
        replaceMapKeyHandler.getEventSender().addListener(new GenericConfigEventListener<Object>() {
            @Override
            public void onConfigValueModified(KeyHandler<Object> keyHandler, Object newValue) {
                FILENAME_REPLACEMAP = convertReplaceMap(FILENAME_REPLACEMAP_DEFAULT, (Map<String, String>) newValue);
            }

            @Override
            public void onConfigValidatorError(KeyHandler<Object> keyHandler, Object invalidValue, ValidationException validateException) {
            }
        });
        FILENAME_REPLACEMAP = convertReplaceMap(FILENAME_REPLACEMAP_DEFAULT, (Map<String, String>) replaceMapKeyHandler.getValue());
    }
    private static volatile Map<Pattern, String> FILENAME_TOO_LONG_REPLACEMAP         = new HashMap<Pattern, String>();
    private static volatile Map<String, String>  FILENAME_TOO_LONG_REPLACEMAP_DEFAULT = new HashMap<String, String>();
    static {
        final ObjectKeyHandler replaceMapKeyHandler = CFG_GENERAL.FILENAME_TOO_LONG_REGEX_REPLACEMAP;
        FILENAME_TOO_LONG_REPLACEMAP_DEFAULT = (Map<String, String>) replaceMapKeyHandler.getDefaultValue();
        replaceMapKeyHandler.getEventSender().addListener(new GenericConfigEventListener<Object>() {
            @Override
            public void onConfigValueModified(KeyHandler<Object> keyHandler, Object newValue) {
                FILENAME_TOO_LONG_REPLACEMAP = convertReplaceMap(FILENAME_TOO_LONG_REPLACEMAP_DEFAULT, (Map<String, String>) newValue);
            }

            @Override
            public void onConfigValidatorError(KeyHandler<Object> keyHandler, Object invalidValue, ValidationException validateException) {
            }
        });
        FILENAME_TOO_LONG_REPLACEMAP = convertReplaceMap(FILENAME_TOO_LONG_REPLACEMAP_DEFAULT, (Map<String, String>) replaceMapKeyHandler.getValue());
    }
    private static volatile Map<Pattern, String> PACKAGENAME_REPLACEMAP         = new HashMap<Pattern, String>();
    private static volatile Map<String, String>  PACKAGENAME_REPLACEMAP_DEFAULT = new HashMap<String, String>();
    static {
        final ObjectKeyHandler replaceMapKeyHandler = CFG_GENERAL.PACKAGE_NAME_CHARACTER_REGEX_REPLACEMAP;
        PACKAGENAME_REPLACEMAP_DEFAULT = (Map<String, String>) replaceMapKeyHandler.getDefaultValue();
        replaceMapKeyHandler.getEventSender().addListener(new GenericConfigEventListener<Object>() {
            @Override
            public void onConfigValueModified(KeyHandler<Object> keyHandler, Object newValue) {
                PACKAGENAME_REPLACEMAP = convertReplaceMap(PACKAGENAME_REPLACEMAP_DEFAULT, (Map<String, String>) newValue);
            }

            @Override
            public void onConfigValidatorError(KeyHandler<Object> keyHandler, Object invalidValue, ValidationException validateException) {
            }
        });
        PACKAGENAME_REPLACEMAP = convertReplaceMap(PACKAGENAME_REPLACEMAP_DEFAULT, (Map<String, String>) replaceMapKeyHandler.getValue());
    }

    public static class ReplaceMapValidator extends AbstractValidator<Map<String, String>> {
        @Override
        public void validate(KeyHandler<Map<String, String>> keyHandler, Map<String, String> value) throws ValidationException {
            try {
                for (final Entry<String, String> entry : value.entrySet()) {
                    if ((entry.getKey() != null && entry.getKey().length() != 0) && entry.getValue() != null) {
                        Pattern.compile(entry.getKey());
                        Matcher.quoteReplacement(entry.getValue());
                    }
                }
            } catch (Exception e) {
                throw new ValidationException(e);
            }
        }
    }

    /** Converts given <String, String> Map to <Pattern, String> Map. */
    private static Map<Pattern, String> convertReplaceMap(final Map<String, String> defaultReplaceMap, final Map<String, String> replaceMap) {
        final Map<Pattern, String> ret = new HashMap<Pattern, String>();
        final Set<String> keys = new HashSet<String>();// Pattern doesn't implement equals!
        if (replaceMap != null) {
            for (final Entry<String, String> entry : replaceMap.entrySet()) {
                if ((entry.getKey() != null && entry.getKey().length() != 0) && entry.getValue() != null) {
                    try {
                        ret.put(Pattern.compile(entry.getKey()), Matcher.quoteReplacement(entry.getValue()));
                        keys.add(entry.getKey());
                    } catch (final Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        }
        if (defaultReplaceMap != null) {
            for (final Entry<String, String> entry : defaultReplaceMap.entrySet()) {
                if ((entry.getKey() != null && entry.getKey().length() != 0) && entry.getValue() != null && !keys.contains(entry.getKey())) {
                    try {
                        ret.put(Pattern.compile(entry.getKey()), Matcher.quoteReplacement(entry.getValue()));
                    } catch (final Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        }
        if (ret.size() > 0) {
            return ret;
        } else {
            return null;
        }
    }

    private static String replaceCharactersByMap(final String input, final Map<Pattern, String> forbiddenCharacterRegexReplaceMap) {
        if (input == null) {
            return null;
        } else if (forbiddenCharacterRegexReplaceMap == null || forbiddenCharacterRegexReplaceMap.size() == 0) {
            return input;
        }
        String newstr = input;
        final Iterator<Entry<Pattern, String>> iterator = forbiddenCharacterRegexReplaceMap.entrySet().iterator();
        while (iterator.hasNext()) {
            final Entry<Pattern, String> entry = iterator.next();
            final Pattern pattern = entry.getKey();
            final String replacement = entry.getValue();
            if (replacement != null) {
                try {
                    newstr = pattern.matcher(newstr).replaceAll(replacement);
                } catch (final Exception e) {
                    e.printStackTrace();
                }
            }
        }
        /**
         * Users can put anything into that replace map. </br>
         * Try to avoid the results of adding something like ".+" resulting in empty filenames.
         */
        if (!StringUtils.isEmpty(newstr)) {
            return newstr;
        } else {
            /* Fallback */
            return input;
        }
    }

    public static String cleanFilename(final String filename) {
        String ret = replaceCharactersByMap(filename, FILENAME_REPLACEMAP);
        ret = CrossSystem.alleviatePathParts(ret, false);
        return ret;
    }

    /**
     * Shortens given filename to max length. </br>
     * Keeps file extension. </br>
     * Returns null if filename can't be shortened.
     */
    public static String shortenFilename(final ParsedFilename pfilename, final int maxLength) {
        if (pfilename == null) {
            throw new IllegalArgumentException();
        } else if (maxLength <= 0) {
            throw new IllegalArgumentException();
        } else if (pfilename.isMultipartArchive()) {
            /* Do not shorten names of multipart archive files. */
            return null;
        }
        String filenameWithoutExtension = pfilename.getFilenameWithoutExtensionAdvanced();
        final String ext = pfilename.getExtensionAdvanced();
        if (ext != null && ext.length() > maxLength) {
            /* Edge case */
            // logger.info("Cannot shorten filename because file extension wouldn't fit");
            return null;
        }
        // final int shortenToLength = Math.min(maxLength - ext.length(), pfilename.getOriginalfilename().length() - ext.length());
        final int maxLengthForFilenameWithoutExt;
        if (ext != null) {
            maxLengthForFilenameWithoutExt = maxLength - ext.length();
        } else {
            maxLengthForFilenameWithoutExt = maxLength;
        }
        filenameWithoutExtension = replaceCharactersByMap(filenameWithoutExtension, FILENAME_TOO_LONG_REPLACEMAP);
        /* Trim in case after cutting it, it randomly ends with a space. */
        filenameWithoutExtension = filenameWithoutExtension.trim();
        if (filenameWithoutExtension.length() > maxLengthForFilenameWithoutExt) {
            /* Filename is still too long -> Shorten it by cutting it from the end. */
            filenameWithoutExtension = filenameWithoutExtension.substring(0, maxLengthForFilenameWithoutExt);
            /* Trim in case after cutting it, it randomly ends with a space. */
            filenameWithoutExtension = filenameWithoutExtension.trim();
        }
        /* Re-Add file-extension if there is one. */
        if (ext != null) {
            filenameWithoutExtension += ext;
        }
        return filenameWithoutExtension;
    }

    public static String cleanPackagename(final String packageName, boolean allowCleanup) {
        String ret = replaceCharactersByMap(packageName, PACKAGENAME_REPLACEMAP);
        /* if enabled, replace dots and _ with spaces and do further clean ups */
        if (ret != null && allowCleanup && org.jdownloader.settings.staticreferences.CFG_GENERAL.CLEAN_UP_PACKAGENAMES.isEnabled()) {
            // TODO: maybe add own cleanup replace map that does this via pattern/replace or add those to package name replace map
            // Replace dots and underscores with spaces while avoiding double spaces
            // Handle special characters like dots and underscores while preventing double spaces
            StringBuilder sb = new StringBuilder();
            boolean lastWasSpace = false;
            for (int i = 0; i < ret.length(); i++) {
                char c = ret.charAt(i);
                if (c == '_' || c == '.') {
                    // Only add a space if the previous character wasn't a space
                    // and the next character isn't a space (to handle periods followed by spaces)
                    if (!lastWasSpace && (i + 1 >= ret.length() || ret.charAt(i + 1) != ' ')) {
                        sb.append(' ');
                        lastWasSpace = true;
                    }
                    // Skip the separator character
                } else if (c == ' ') {
                    // Only add spaces if the previous character wasn't a space
                    if (!lastWasSpace) {
                        sb.append(c);
                        lastWasSpace = true;
                    }
                } else {
                    // Regular character, add it
                    sb.append(c);
                    lastWasSpace = false;
                }
            }
            ret = sb.toString().trim();
        }
        ret = CrossSystem.alleviatePathParts(ret, false);
        return ret;
    }

    /**
     * Derives a package name from a filename by removing known archive extensions and other file extensions.
     *
     * @param filename
     *            The original filename to process
     * @return The derived package name
     */
    public static String derivePackagenameFromFilename(final String filename) {
        if (StringUtils.isEmpty(filename)) {
            return null;
        }
        String name = filename;
        boolean hasKnownExtension = false;
        // Step 1: Remove known archive file extensions
        ExtensionRemovalResult result = removeKnownArchiveExtensions(name);
        name = result.processedName;
        hasKnownExtension = result.extensionFound;
        // Step 2: Remove "part" patterns
        String tmpName = cutNameMatch(name, pat13);
        if (tmpName.length() > 3) {
            name = tmpName;
        }
        // Step 3: Remove other file extensions based on settings
        name = removeOtherFileExtensions(name, hasKnownExtension);
        // Step 4: Remove trailing separators (., -, _)
        name = removeTrailingSeparators(name);
        return name;
    }

    /**
     * Helper class to return multiple values from extension removal operations
     */
    private static class ExtensionRemovalResult {
        public final String  processedName;
        public final boolean extensionFound;

        public ExtensionRemovalResult(String processedName, boolean extensionFound) {
            this.processedName = processedName;
            this.extensionFound = extensionFound;
        }
    }

    /**
     * Removes known archive extensions from a filename
     *
     * @param name
     *            The filename to process
     * @return Result containing the processed name and whether an extension was found
     */
    private static ExtensionRemovalResult removeKnownArchiveExtensions(String name) {
        // Check RAR extensions
        ExtensionRemovalResult result = checkPatternList(name, rarPats);
        if (result.extensionFound) {
            return result;
        }
        // Check ZIP extensions
        result = checkPatternList(name, zipPats);
        if (result.extensionFound) {
            return result;
        }
        // Check ISZ extensions
        result = checkPatternList(name, iszPats);
        if (result.extensionFound) {
            return result;
        }
        // Check XtremSplit extension
        String processed = getNameMatch(name, pat17);
        if (!name.equals(processed)) {
            return new ExtensionRemovalResult(processed, true);
        }
        // Check FFSJ extensions
        if (ffsjPats != null) {
            for (Pattern pattern : ffsjPats) {
                processed = getNameMatch(name, pattern);
                if (!name.equalsIgnoreCase(processed)) {
                    return new ExtensionRemovalResult(processed, true);
                }
            }
        }
        return new ExtensionRemovalResult(name, false);
    }

    /**
     * Checks a filename against a list of patterns and returns the first match
     *
     * @param name
     *            The filename to check
     * @param patterns
     *            Array of patterns to check against
     * @return Result containing the processed name and whether a match was found
     */
    private static ExtensionRemovalResult checkPatternList(String name, Pattern[] patterns) {
        for (Pattern pattern : patterns) {
            String processed = getNameMatch(name, pattern);
            if (!name.equals(processed)) {
                return new ExtensionRemovalResult(processed, true);
            }
        }
        return new ExtensionRemovalResult(name, false);
    }

    /**
     * Removes other file extensions based on extension settings
     *
     * @param name
     *            The filename to process
     * @param hasKnownExtension
     *            Whether a known extension was already found and removed
     * @return The filename with extensions removed
     */
    private static String removeOtherFileExtensions(String name, boolean hasKnownExtension) {
        final EXTENSION_SETTINGS extensionSettings = EXTENSION_SETTINGS.REMOVE_ALL;
        if (EXTENSION_SETTINGS.REMOVE_ALL.equals(extensionSettings) || EXTENSION_SETTINGS.REMOVE_KNOWN.equals(extensionSettings)) {
            while (true) {
                final int lastPoint = name.lastIndexOf(".");
                if (lastPoint <= 0) {
                    break;
                }
                final String ext = name.substring(lastPoint + 1);
                final int extLength = ext.length();
                final ExtensionsFilterInterface knownExt = CompiledFiletypeFilter.getExtensionsFilterInterface(ext);
                // Case 1: Known extension (not archive number)
                if (knownExt != null && !ArchiveExtensions.NUM.equals(knownExt)) {
                    name = name.substring(0, lastPoint);
                    continue;
                }
                // Case 2: Short alphanumeric extension (only if REMOVE_ALL is set)
                if (extLength <= 4 && EXTENSION_SETTINGS.REMOVE_ALL.equals(extensionSettings) && ext.matches("^[0-9a-zA-z]+$")) {
                    if (!hasKnownExtension) {
                        // Only remove if we haven't found a known extension yet
                        name = name.substring(0, lastPoint);
                        continue;
                    } else {
                        // Otherwise stop processing (preserve remaining extension)
                        break;
                    }
                }
                // Case 3: Unknown/unhandled extension type
                break;
            }
        }
        return name;
    }

    /**
     * Removes trailing separator characters from a filename
     *
     * @param name
     *            The filename to process
     * @return The filename with trailing separators removed
     */
    private static String removeTrailingSeparators(String name) {
        int removeIndex = -1;
        for (int i = name.length() - 1; i >= 0; i--) {
            final char c = name.charAt(i);
            if (c == ',' || c == '_' || c == '-') {
                removeIndex = i;
            } else {
                break;
            }
        }
        if (removeIndex > 0) {
            name = name.substring(0, removeIndex);
        }
        return name;
    }

    private static String getNameMatch(final String name, final Pattern pattern) {
        String match = new Regex(name, pattern).getMatch(0);
        if (match != null) {
            return match;
        }
        return name;
    }

    private static String cutNameMatch(String name, Pattern pattern) {
        if (name == null) {
            return null;
        }
        String match = new Regex(name, pattern).getMatch(0);
        if (match != null) {
            int firstpos = name.indexOf(match);
            String tmp = name.substring(0, firstpos);
            int lastpos = name.indexOf(match) + match.length();
            if (!(lastpos > name.length())) {
                tmp = tmp + name.substring(lastpos);
            }
            name = tmp;
            /* remove seq. dots */
            name = name.replaceAll("\\.{2,}", ".");
            name = name.replaceAll("\\.{2,}", ".");
        }
        return name;
    }
}
