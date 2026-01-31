package org.jdownloader.captcha.v2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;

import jd.controlling.captcha.CaptchaSettings;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.StringUtils;

/**
 * Singleton Manager for managing CaptchaHistoryEntry objects.
 *
 * Features: - Persists up to 500 entries in the config - Automatically removes null and stale entries (older than 1 year) - Provides method
 * to query the last timestamp per CAPTCHA_TYPE - Thread-safe operations using CopyOnWriteArrayList
 */
public class CaptchaHistoryManager {
    private static final CaptchaHistoryManager              INSTANCE           = new CaptchaHistoryManager();
    private static final int                                MAX_ENTRIES        = 500;
    private static final long                               ONE_YEAR_IN_MILLIS = TimeUnit.DAYS.toMillis(365);
    protected final static CaptchaSettings                  CAPTCHA_SETTINGS   = JsonConfig.create(CaptchaSettings.class);
    private final CopyOnWriteArrayList<CaptchaHistoryEntry> entries;

    /**
     * Private constructor for singleton pattern.
     */
    private CaptchaHistoryManager() {
        CopyOnWriteArrayList<CaptchaHistoryEntry> entries = CAPTCHA_SETTINGS.getCaptchaHistoryEntries();
        if (entries == null) {
            entries = new CopyOnWriteArrayList<CaptchaHistoryEntry>();
            CAPTCHA_SETTINGS.setCaptchaHistoryEntries(entries);
        }
        this.entries = entries;
    }

    /**
     * Returns the only instance of CaptchaHistoryManager.
     *
     * @return CaptchaHistoryManager singleton instance
     */
    public static CaptchaHistoryManager getInstance() {
        return CaptchaHistoryManager.INSTANCE;
    }

    /**
     * Factory method to create a CaptchaHistoryEntry from a Challenge object. Automatically extracts host, CAPTCHA type and login status
     * from the challenge.
     *
     * @param challenge
     *            The Challenge object
     * @return A new CaptchaHistoryEntry with all relevant information
     * @throws IllegalArgumentException
     *             if challenge is null
     */
    public CaptchaHistoryEntry createCaptchaHistoryEntry(Challenge<?> challenge) {
        if (challenge == null) {
            throw new IllegalArgumentException("challenge cannot be null");
        }
        final String host = challenge.getHost();
        final CAPTCHA_TYPE captchaType = CAPTCHA_TYPE.getCaptchaTypeForChallenge(challenge);
        final boolean isLoginCaptcha = challenge.isAccountLogin();
        return new CaptchaHistoryEntry(host, captchaType, isLoginCaptcha);
    }

    /**
     * Adds a CaptchaHistoryEntry to the history. Persists the entry in the config and automatically performs cleanup.
     *
     * @param entry
     *            The CaptchaHistoryEntry to add
     * @throws IllegalArgumentException
     *             if entry is null
     */
    public void addEntry(CaptchaHistoryEntry entry) {
        if (entry == null) {
            throw new IllegalArgumentException("entry cannot be null");
        }
        entries.add(entry);
        cleanupEntries();
    }

    /**
     * Adds a CaptchaHistoryEntry to the history from a Challenge object. Automatically creates the entry from the challenge and persists
     * it. Extracts host, CAPTCHA type and login status directly from the challenge.
     *
     * @param challenge
     *            The Challenge object to create an entry from
     * @throws IllegalArgumentException
     *             if challenge is null
     */
    public void addEntry(Challenge<?> challenge) {
        if (challenge == null) {
            throw new IllegalArgumentException("challenge cannot be null");
        }
        final CaptchaHistoryEntry entry = new CaptchaHistoryEntry(challenge);
        addEntry(entry);
    }

    /**
     * Returns all entries for a specific CAPTCHA_TYPE.
     *
     * @param type
     *            The CAPTCHA_TYPE to filter
     * @return A list of all entries for this type
     * @throws IllegalArgumentException
     *             if type is null
     */
    public List<CaptchaHistoryEntry> getEntriesByCaptchaType(CAPTCHA_TYPE type) {
        if (type == null) {
            throw new IllegalArgumentException("type cannot be null");
        }
        final CaptchaHistoryEntryFilter filter = new CaptchaHistoryEntryFilter().withCaptchaType(type);
        return getFilteredEntries(filter);
    }

    /**
     * Returns the last (newest) entry for a specific CAPTCHA_TYPE.
     *
     * @param type
     *            The CAPTCHA_TYPE to search for
     * @return The most recent entry for this type or null if no entry was found
     * @throws IllegalArgumentException
     *             if type is null
     */
    public CaptchaHistoryEntry getLastUsedTimestampByCaptchaType(CAPTCHA_TYPE type) {
        if (type == null) {
            throw new IllegalArgumentException("type cannot be null");
        }
        final CaptchaHistoryEntryFilter filter = new CaptchaHistoryEntryFilter().withCaptchaType(type);
        return getLastEntryMatching(filter);
    }

    /**
     * Returns a copy of all entries.
     *
     * @return Copy of the history list
     */
    public List<CaptchaHistoryEntry> getAllEntries() {
        return new ArrayList<CaptchaHistoryEntry>(cleanupEntries());
    }

    /**
     * Clears all entries from the history.
     */
    public void clear() {
        entries.clear();
    }

    /**
     * Cleans up the history list: - Removes null entries - Removes entries older than 1 year - Maintains a maximum of MAX_ENTRIES entries
     * (removes oldest)
     *
     * Called internally by all methods that access the history.
     *
     * @param entries
     *            The list to clean up
     * @return true if any entries were removed, false if no changes were made
     */
    private CopyOnWriteArrayList<CaptchaHistoryEntry> cleanupEntries() {
        final CopyOnWriteArrayList<CaptchaHistoryEntry> entries = this.entries;
        if (entries.size() == 0) {
            return entries;
        }
        final long currentTime = System.currentTimeMillis();
        for (Iterator<CaptchaHistoryEntry> iterator = entries.iterator(); iterator.hasNext();) {
            final CaptchaHistoryEntry entry = iterator.next();
            if (entry == null) {
                // Remove null entries
                entries.remove(null);
            } else if (entry.getCaptcha_type() == null || StringUtils.isEmpty(entry.getDomain())) {
                /* Remove broken items */
                entries.remove(entry);
            } else if (entry.getTimestamp() + ONE_YEAR_IN_MILLIS < currentTime) {
                /* Remove old items */
                // Remove entries older than 1 year or with empty domain
                entries.remove(entry);
            }
        }
        // If we exceed MAX_ENTRIES, remove the oldest ones
        while (entries.size() > MAX_ENTRIES) {
            if (entries.size() > 0) {
                try {
                    entries.remove(0);
                } catch (IndexOutOfBoundsException ignoreAsyncPurge) {
                    break;
                }
            }
        }
        return entries;
    }

    /**
     * Returns statistics of stored entries per CAPTCHA_TYPE.
     *
     * @return Map with CAPTCHA_TYPE name() as key and entry count as value
     */
    public Map<String, Integer> getStatistics() {
        final List<CaptchaHistoryEntry> entries = cleanupEntries();
        final Map<String, Integer> stats = new HashMap<String, Integer>();
        final Iterator<CaptchaHistoryEntry> it = entries.iterator();
        while (it.hasNext()) {
            final CaptchaHistoryEntry entry = it.next();
            final CAPTCHA_TYPE type = entry.getCaptcha_type();
            final String typeId = type.name();
            final Integer count = stats.get(typeId);
            final int countValue = count != null ? count.intValue() + 1 : 1;
            stats.put(typeId, countValue);
        }
        return stats;
    }

    /**
     * Returns a filtered list of entries based on the provided filter criteria. Supports filtering by CAPTCHA type, domain, login status,
     * timestamp range, and result limit.
     *
     * @param filter
     *            The filter object specifying the criteria
     * @return A list of entries matching all filter criteria, or an empty list if no matches
     * @throws IllegalArgumentException
     *             if filter is null
     */
    public List<CaptchaHistoryEntry> getFilteredEntries(final CaptchaHistoryEntryFilter filter) {
        if (filter == null) {
            throw new IllegalArgumentException("filter cannot be null");
        }
        List<CaptchaHistoryEntry> result = new ArrayList<CaptchaHistoryEntry>();
        final Iterator<CaptchaHistoryEntry> it = cleanupEntries().iterator();
        while (it.hasNext()) {
            final CaptchaHistoryEntry entry = it.next();
            if (filter.matches(entry)) {
                // Apply filters
                result.add(entry);
            }
        }
        final Integer limit = filter.getLimit();
        if (limit != null && limit.intValue() > 0) {
            // Apply limit if specified
            final int resultSize = result.size();
            if (resultSize > limit.intValue()) {
                // Keep only the last 'limit' entries (most recent)
                result = result.subList(resultSize - limit.intValue(), resultSize);
            }
        }
        return result;
    }

    /**
     * Returns the most recent entry matching the filter criteria.
     *
     * @param filter
     *            The filter object
     * @return The most recent entry matching the filter, or null if no match
     * @throws IllegalArgumentException
     *             if filter is null
     */
    public CaptchaHistoryEntry getLastEntryMatching(CaptchaHistoryEntryFilter filter) {
        if (filter == null) {
            throw new IllegalArgumentException("filter cannot be null");
        }
        final List<CaptchaHistoryEntry> entries = getFilteredEntries(filter);
        if (entries.size() == 0) {
            return null;
        }
        return entries.get(entries.size() - 1);
    }
}