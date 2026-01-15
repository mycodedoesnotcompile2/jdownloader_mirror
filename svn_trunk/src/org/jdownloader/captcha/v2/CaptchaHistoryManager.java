package org.jdownloader.captcha.v2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.StringUtils;

import jd.controlling.captcha.CaptchaSettings;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * Singleton Manager for managing CaptchaHistoryEntry objects.
 *
 * Features: - Persists up to 500 entries in the config - Automatically removes null and stale entries (older than 1 year) - Provides method
 * to query the last timestamp per CAPTCHA_TYPE - Thread-safe operations using CopyOnWriteArrayList
 */
public class CaptchaHistoryManager {
    private static final CaptchaHistoryManager INSTANCE           = new CaptchaHistoryManager();
    private static final int                   MAX_ENTRIES        = 500;
    private static final long                  ONE_YEAR_IN_MILLIS = 365L * 24L * 60L * 60L * 1000L;
    protected final static CaptchaSettings     CAPTCHA_SETTINGS   = JsonConfig.create(CaptchaSettings.class);
    private final Object                       lock               = new Object();

    /**
     * Private constructor for singleton pattern.
     */
    private CaptchaHistoryManager() {
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
        String host = challenge.getHost();
        CAPTCHA_TYPE captchaType = CAPTCHA_TYPE.getCaptchaTypeForChallenge(challenge);
        boolean isLoginCaptcha = challenge.isAccountLogin();
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
        synchronized (lock) {
            CopyOnWriteArrayList<CaptchaHistoryEntry> entries = CAPTCHA_SETTINGS.getCaptchaHistoryEntries();
            if (entries == null) {
                entries = new CopyOnWriteArrayList<CaptchaHistoryEntry>();
                CAPTCHA_SETTINGS.setCaptchaHistoryEntries(entries);
            }
            entries.add(entry);
            if (cleanupEntries(entries)) {
                CAPTCHA_SETTINGS.setCaptchaHistoryEntries(entries);
            }
        }
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
        CaptchaHistoryEntry entry = new CaptchaHistoryEntry(challenge);
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
        CaptchaHistoryEntryFilter filter = new CaptchaHistoryEntryFilter().withCaptchaType(type);
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
        CaptchaHistoryEntryFilter filter = new CaptchaHistoryEntryFilter().withCaptchaType(type);
        return getLastEntryMatching(filter);
    }

    /**
     * Returns a copy of all entries.
     *
     * @return Copy of the history list
     */
    public List<CaptchaHistoryEntry> getAllEntries() {
        synchronized (lock) {
            CopyOnWriteArrayList<CaptchaHistoryEntry> entries = CAPTCHA_SETTINGS.getCaptchaHistoryEntries();
            if (entries == null) {
                return new ArrayList<CaptchaHistoryEntry>();
            }
            if (cleanupEntries(entries)) {
                CAPTCHA_SETTINGS.setCaptchaHistoryEntries(entries);
            }
            return new ArrayList<CaptchaHistoryEntry>(entries);
        }
    }

    /**
     * Clears all entries from the history.
     */
    public void clear() {
        synchronized (lock) {
            CAPTCHA_SETTINGS.setCaptchaHistoryEntries(new CopyOnWriteArrayList<CaptchaHistoryEntry>());
        }
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
    private boolean cleanupEntries(CopyOnWriteArrayList<CaptchaHistoryEntry> entries) {
        if (entries == null || entries.size() == 0) {
            return false;
        }
        boolean hasChanges = false;
        // Remove null entries
        for (Iterator<CaptchaHistoryEntry> iterator = entries.iterator(); iterator.hasNext();) {
            CaptchaHistoryEntry entry = iterator.next();
            if (entry == null) {
                iterator.remove();
                hasChanges = true;
            }
        }
        // Remove entries older than 1 year or with empty domain
        long currentTime = System.currentTimeMillis();
        for (Iterator<CaptchaHistoryEntry> iterator = entries.iterator(); iterator.hasNext();) {
            CaptchaHistoryEntry entry = iterator.next();
            if (entry.getTimestamp() + ONE_YEAR_IN_MILLIS < currentTime || StringUtils.isEmpty(entry.getDomain())) {
                iterator.remove();
                hasChanges = true;
            }
        }
        // If we exceed MAX_ENTRIES, remove the oldest ones
        while (entries.size() > MAX_ENTRIES) {
            if (entries.size() > 0) {
                entries.remove(0);
                hasChanges = true;
            }
        }
        return hasChanges;
    }

    /**
     * Returns statistics of stored entries per CAPTCHA_TYPE.
     *
     * @return Map with CAPTCHA_TYPE name() as key and entry count as value
     */
    public Map<String, Integer> getStatistics() {
        synchronized (lock) {
            CopyOnWriteArrayList<CaptchaHistoryEntry> entries = CAPTCHA_SETTINGS.getCaptchaHistoryEntries();
            if (entries == null || entries.size() == 0) {
                return new HashMap<String, Integer>();
            }
            Map<String, Integer> stats = new HashMap<String, Integer>();
            for (int i = 0; i < entries.size(); i++) {
                CaptchaHistoryEntry entry = entries.get(i);
                if (entry == null) {
                    continue;
                }
                CAPTCHA_TYPE type = entry.getCaptcha_type();
                if (type == null) {
                    continue;
                }
                String typeId = type.name();
                Integer count = stats.get(typeId);
                if (count == null) {
                    count = Integer.valueOf(0);
                }
                stats.put(typeId, Integer.valueOf(count.intValue() + 1));
            }
            if (cleanupEntries(entries)) {
                CAPTCHA_SETTINGS.setCaptchaHistoryEntries(entries);
            }
            return stats;
        }
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
    public List<CaptchaHistoryEntry> getFilteredEntries(CaptchaHistoryEntryFilter filter) {
        if (filter == null) {
            throw new IllegalArgumentException("filter cannot be null");
        }
        synchronized (lock) {
            CopyOnWriteArrayList<CaptchaHistoryEntry> entries = CAPTCHA_SETTINGS.getCaptchaHistoryEntries();
            if (entries == null || entries.size() == 0) {
                return new ArrayList<CaptchaHistoryEntry>();
            }
            List<CaptchaHistoryEntry> result = new ArrayList<CaptchaHistoryEntry>();
            // Apply filters
            for (int i = 0; i < entries.size(); i++) {
                CaptchaHistoryEntry entry = entries.get(i);
                if (entry != null && filter.matches(entry)) {
                    result.add(entry);
                }
            }
            // Apply limit if specified
            Integer limit = filter.getLimit();
            if (limit != null && limit.intValue() > 0) {
                int resultSize = result.size();
                if (resultSize > limit.intValue()) {
                    // Keep only the last 'limit' entries (most recent)
                    result = result.subList(resultSize - limit.intValue(), resultSize);
                }
            }
            if (cleanupEntries(entries)) {
                CAPTCHA_SETTINGS.setCaptchaHistoryEntries(entries);
            }
            return result;
        }
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
        List<CaptchaHistoryEntry> entries = getFilteredEntries(filter);
        if (entries == null || entries.size() == 0) {
            return null;
        }
        return entries.get(entries.size() - 1);
    }
}