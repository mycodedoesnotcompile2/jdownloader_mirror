package org.jdownloader.utils;

import java.io.File;
import java.util.HashMap;

/**
 * Small helper that caches file-system existence lookups to keep the number of {@link File#exists()} calls to a minimum. It also
 * covers the case where the same path is checked more than once (e.g. the same file referenced by two different sources).
 *
 * Two caches are combined:
 * <ul>
 * <li>a per-file cache (absolute path -> existence), and</li>
 * <li>a per-directory cache (parent path -> "may contain existing files").</li>
 * </ul>
 * A single {@code dir.list()} call determines both existence and emptiness of a directory: {@code null} means the directory does
 * not exist, an empty array means it is empty. In both cases none of the files inside can exist, so the per-file
 * {@link File#exists()} call is skipped entirely. This is correct on all platforms (no case-sensitivity assumptions): when the
 * directory is present and non-empty, the actual positive check is still delegated to {@link File#exists()}, which respects the
 * platform's case semantics.
 *
 * Not thread-safe: an instance is intended to be used from a single thread (e.g. one scan run).
 */
public class FileIoCache {
    /* Cache of absolute file path -> existence, to minimize File.exists calls for duplicate paths. */
    private final HashMap<String, Boolean> existsCache    = new HashMap<String, Boolean>();
    /*
     * Cache of parent directory path -> "may contain existing files". A single dir.list() call determines both existence and
     * emptiness of the directory: null means the directory does not exist, an empty array means it is empty. In both cases none of
     * the files inside can exist, so we can skip the per-file File.exists() call entirely. This is correct on all platforms (no
     * case-sensitivity assumptions).
     */
    private final HashMap<String, Boolean> dirUsableCache = new HashMap<String, Boolean>();

    /**
     * Checks whether the given file exists, using both a per-file existence cache and a per-directory cache. When the parent
     * directory is missing or empty, the result is derived from the (cached) directory listing without an extra
     * {@link File#exists()} call.
     */
    public boolean exists(final File file) {
        final String path = file.getAbsolutePath();
        final Boolean cachedExists = existsCache.get(path);
        if (cachedExists != null) {
            return cachedExists.booleanValue();
        }
        final File parent = file.getParentFile();
        boolean exists;
        if (parent == null) {
            exists = file.exists();
        } else {
            final String parentPath = parent.getAbsolutePath();
            Boolean dirUsable = dirUsableCache.get(parentPath);
            if (dirUsable == null) {
                final String[] entries = parent.list();
                dirUsable = Boolean.valueOf(entries != null && entries.length > 0);
                dirUsableCache.put(parentPath, dirUsable);
            }
            if (dirUsable.booleanValue()) {
                exists = file.exists();
            } else {
                /* Parent directory is missing or empty -> the file cannot exist. */
                exists = false;
            }
        }
        existsCache.put(path, Boolean.valueOf(exists));
        return exists;
    }

    /**
     * Deletes the given file. First consults the cache: if the file is known not to exist, nothing is deleted and false is
     * returned. Otherwise {@link File#delete()} is called and its result is returned unchanged - true only if the file was really
     * deleted, false otherwise. On a successful deletion the cache is updated so a later {@link #exists(File)} no longer reports
     * the file as present.
     */
    public boolean delete(final File file) {
        if (!exists(file)) {
            /* Cache says the file is not there -> nothing to delete. */
            return false;
        }
        final boolean deleted = file.delete();
        if (deleted) {
            /*
             * File is gone now: remember it as non-existent. The parent's dirUsableCache entry is intentionally left untouched - a
             * stale "usable" flag only means exists() falls back to a real File.exists() check, which stays correct.
             */
            existsCache.put(file.getAbsolutePath(), Boolean.FALSE);
        }
        return deleted;
    }

    /**
     * Drops all cached results. Use this when the underlying file system may have changed and the next lookup must hit disk again.
     */
    public void clear() {
        existsCache.clear();
        dirUsableCache.clear();
    }
}
