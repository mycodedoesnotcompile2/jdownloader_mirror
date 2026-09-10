package jd.controlling.downloadcontroller;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.List;

import org.appwork.utils.IO;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.CrossSystem.OperatingSystem;
import org.jdownloader.controlling.UniqueAlltimeID;

public class FilePathChecker {
    /**
     * The idea is that you can provide a list of flags to the folder create function so it knows to which extend it is allowed to perform
     * actions such as write-checks.
     */
    public static enum CheckFlag {
        CHECK_FOLDER_CREATE,
        CHECK_FILE_WRITE,
        CHECK_FILE_FOR_TOO_LONG_FILENAME,
        IS_FILE,
        ERROR_ON_ALREADY_EXIST,
        /**
         * Treat a failure to delete the temporary write-test file again as an error. </br>
         * By default a failed deletion is silently ignored (the write itself succeeded, which is what the check cares about).
         */
        ERROR_ON_WRITE_TEST_DELETION_FAILURE
    }

    public static void createFilePath(final File file) throws IOException {
        createFilePath(file, new CheckFlag[] { CheckFlag.IS_FILE, CheckFlag.CHECK_FILE_WRITE, CheckFlag.CHECK_FILE_FOR_TOO_LONG_FILENAME });
    }

    public static void createFolderPath(final File file) throws IOException {
        createFilePath(file, new CheckFlag[] { CheckFlag.CHECK_FOLDER_CREATE });
    }

    /**
     * Validates that the given path is usable as a destination WITHOUT creating or writing anything. </br>
     * Verifies that the root which must already exist is present (mount point on Linux/Mac, drive root or network share on Windows) and -
     * on Windows - that the total path length does not exceed the classic MAX_PATH limit. </br>
     * Returns normally if the path looks valid - this includes "normal" paths (e.g. inside the user home) that require no such root check.
     * </br>
     * Only {@link CheckFlag#IS_FILE} is evaluated: it decides whether the last path segment is treated as a file (an over-long name yields
     * {@link BadFilePathException.PathFailureReason#PATH_SEGMENT_TOO_LONG}) or a folder (an over-long name yields
     * {@link BadFilePathException.PathFailureReason#PATH_TOO_LONG}). All other flags are ignored here.
     *
     * @throws BadFilePathException
     *             with {@link BadFilePathException.PathFailureReason#PATH_TOO_LONG} if a folder segment / the whole path is too long,
     *             {@link BadFilePathException.PathFailureReason#PATH_SEGMENT_TOO_LONG} if the file name is too long, or
     *             {@link BadFilePathException.PathFailureReason#INVALID_DESTINATION} if the required destination root is missing/invalid.
     */
    public static void validatePath(final File file, final CheckFlag... flags) throws IOException {
        if (file == null) {
            throw new IllegalArgumentException("file can't be null");
        }
        /* Only IS_FILE is relevant here: it decides whether the last path segment is a file or a folder. */
        boolean isFile = false;
        if (flags != null) {
            for (final CheckFlag flag : flags) {
                if (flag == CheckFlag.IS_FILE) {
                    isFile = true;
                    break;
                }
            }
        }
        if (file.exists()) {
            /* Already exists -> valid path. */
            return;
        }
        final File parent = file.getParentFile();
        if (parent == null) {
            // OS root
            /* This should never happen! */
            throw new BadFilePathException(file, BadFilePathException.PathFailureReason.INVALID_DESTINATION);
        }
        /**
         * The root directory that must already exist for the target path to be valid. </br>
         * For removable/mounted media (Linux/Mac) this is the mount point, for Windows the drive root or the network share. </br>
         * We must not silently (re-)create such a root as a plain folder when the underlying medium is not present (e.g. unplugged USB
         * drive, offline network share, non-existent drive letter). </br>
         * It stays null for "normal" paths (e.g. inside the user home) for which no such root check is required.
         */
        File requiredExistingRoot = null;
        String[] folders;
        switch (CrossSystem.getOSFamily()) {
        case LINUX:
            folders = CrossSystem.getPathComponents(file);
            if (folders.length >= 3) {
                final String userName = System.getProperty("user.name");
                if (folders.length >= 5 && "run".equals(folders[1]) && "media".equals(folders[2]) && folders[3].equals(userName)) {
                    /* 0:/ | 1:run | 2:media | 3:user | 4:mounted volume */
                    requiredExistingRoot = new File("/run/media/" + userName + "/" + folders[4]);
                } else if ("media".equals(folders[1])) {
                    /* 0:/ | 1:media | 2:mounted volume */
                    requiredExistingRoot = new File("/media/" + folders[2]);
                } else if ("mnt".equals(folders[1])) {
                    /* 0:/ | 1:mnt | 2:mounted volume */
                    requiredExistingRoot = new File("/mnt/" + folders[2]);
                }
            }
            break;
        case MAC:
            folders = CrossSystem.getPathComponents(file);
            if (folders.length >= 3) {
                if ("media".equals(folders[1])) {
                    /* 0:/ | 1:media | 2:mounted volume */
                    requiredExistingRoot = new File("/media/" + folders[2]);
                } else if ("mnt".equals(folders[1])) {
                    /* 0:/ | 1:mnt | 2:mounted volume */
                    requiredExistingRoot = new File("/mnt/" + folders[2]);
                } else if ("Volumes".equals(folders[1])) {
                    /* 0:/ | 1:Volumes | 2:mounted volume */
                    requiredExistingRoot = new File("/Volumes/" + folders[2]);
                }
            }
            break;
        case WINDOWS:
            /*
             * We must detect over-long segments WITHOUT canonicalizing the path first, because File.getCanonicalPath() (used by
             * getPathComponents below) throws a raw "invalid parameter" IOException for over-long segments, which would mask the real
             * cause.
             */
            if (hasTooLongWindowsPathSegment(parent)) {
                /* One of the parent folder segments is too long -> the whole path is unusable. */
                throw new BadFilePathException(file, BadFilePathException.PathFailureReason.PATH_TOO_LONG);
            } else if (looksLikeTooLongWindowsPathSegment(file.getName())) {
                /* The last segment is too long -> classify it as file or folder depending on IS_FILE. */
                if (isFile) {
                    throw new BadFilePathException(file, BadFilePathException.PathFailureReason.PATH_SEGMENT_TOO_LONG);
                } else {
                    throw new BadFilePathException(file, BadFilePathException.PathFailureReason.PATH_TOO_LONG);
                }
            }
            if (CrossSystem.getOS().isMaximum(OperatingSystem.WINDOWS_NT) && file.getAbsolutePath().length() > 259) {
                // old windows API does not allow longer paths
                throw new BadFilePathException(file, BadFilePathException.PathFailureReason.PATH_TOO_LONG);
            }
            /* Intentional fall-through: continue with the generic drive-root / network-share validation below. */
        default:
            folders = CrossSystem.getPathComponents(file);
            if (folders.length > 0) {
                String root = folders[0];
                if (root.matches("^[a-zA-Z]{1}:\\\\$") || root.matches("^[a-zA-Z]{1}://$")) {
                    /* X:/ or X:\ */
                    requiredExistingRoot = new File(folders[0]);
                } else if (root.equals("\\\\")) {
                    if (folders.length >= 3) {
                        /* \\\\computer\\folder\\ in network */
                        requiredExistingRoot = new File(folders[0] + folders[1] + "\\" + folders[2]);
                    }
                }
            }
        }
        if (requiredExistingRoot == null) {
            /* No root that has to exist upfront -> nothing to validate here. */
            return;
        } else if (requiredExistingRoot.isDirectory()) {
            /* Required root (mount point / drive / share) is present -> valid path. */
            return;
        }
        /* Required root (mount point / drive / share) is missing -> invalid destination. */
        throw new BadFilePathException(file, BadFilePathException.PathFailureReason.INVALID_DESTINATION);
    }

    /**
     * Creates path of given File instance and performs write-test if wanted.
     *
     * @throws InterruptedException
     */
    public static void createFilePath(final File file, final CheckFlag... flags) throws IOException {
        if (file == null) {
            throw new IllegalArgumentException("file can't be null");
        }
        boolean shouldBeFile = false;
        boolean checkFileWrite = false;
        boolean checkFolderCreate = false;
        boolean allowCheckForTooLongFilename = false;
        boolean errorOnFileAlreadyExist = false;
        boolean errorOnWriteTestDeletionFailure = false;
        if (flags != null) {
            for (final CheckFlag flag : flags) {
                if (flag == CheckFlag.IS_FILE) {
                    shouldBeFile = true;
                } else if (flag == CheckFlag.CHECK_FILE_WRITE) {
                    checkFileWrite = true;
                } else if (flag == CheckFlag.CHECK_FOLDER_CREATE) {
                    checkFolderCreate = true;
                } else if (flag == CheckFlag.CHECK_FILE_FOR_TOO_LONG_FILENAME) {
                    allowCheckForTooLongFilename = true;
                } else if (flag == CheckFlag.ERROR_ON_ALREADY_EXIST) {
                    errorOnFileAlreadyExist = true;
                } else if (flag == CheckFlag.ERROR_ON_WRITE_TEST_DELETION_FAILURE) {
                    errorOnWriteTestDeletionFailure = true;
                }
            }
        }
        if (file.exists()) {
            /* Already exists -> No need to do anything. */
            final boolean isFile = file.isFile();
            if (shouldBeFile && !isFile) {
                throw new BadFilePathException(file, BadFilePathException.PathFailureReason.FILE_ALREADY_EXISTS_AS_FOLDER);
            }
            if (isFile && errorOnFileAlreadyExist) {
                throw new BadFilePathException(file, BadFilePathException.PathFailureReason.FILE_ALREADY_EXISTS);
            }
            if (checkFileWrite && !file.canWrite()) {
                /* File/folder exists but we are lacking write permissions. */
                if (isFile) {
                    throw new BadFilePathException(file, BadFilePathException.PathFailureReason.PERMISSION_PROBLEM_FILE);
                } else {
                    throw new BadFilePathException(file, BadFilePathException.PathFailureReason.PERMISSION_PROBLEM_FOLDER);
                }
            }
            return;
        }
        final File parentFolder = file.getParentFile();
        if (parentFolder == null) {
            // OS root
            /* This should never happen! */
            throw new BadFilePathException(file, BadFilePathException.PathFailureReason.INVALID_DESTINATION);
        }
        /* Validate path without writing anything. */
        validatePath(file, flags);
        if (!checkFolderCreate && !checkFileWrite) {
            /* No errors until now and we're not allowed to write -> Validation successful -> Call it success */
            return;
        }
        /**
         * Create a list of the full folder path structure.
         */
        final List<File> pathlist = new ArrayList<File>();
        int loop = 0;
        File next = file;
        int folderCreateStartSegmentIndex = -1;
        while (true) {
            pathlist.add(0, next);
            if (folderCreateStartSegmentIndex == -1 && !next.exists()) {
                /* Find first non-existent part of path. */
                folderCreateStartSegmentIndex = loop;
            }
            next = next.getParentFile();
            if (next == null) {
                /* We've reached the end. */
                break;
            }
            loop++;
        }
        if (folderCreateStartSegmentIndex != -1) {
            /**
             * Manually create all folders up until we are in our final folder where we want to write the file we want to download. </br>
             * This may look more complicated compared to File.mkdirs() but this way we can know exactly at which point a directory could
             * not be created which allows for better error handling on path issues.
             */
            folderCreateStartSegmentIndex = pathlist.size() - folderCreateStartSegmentIndex - 1;
            for (int index = folderCreateStartSegmentIndex; index < pathlist.size(); index++) {
                final boolean isLastItem = index == pathlist.size() - 1;
                if (shouldBeFile && isLastItem) {
                    /* Last path segment is file -> Do not create folder! */
                    break;
                }
                final File thisfolder = pathlist.get(index);
                if (!thisfolder.exists() && !thisfolder.mkdir() && !thisfolder.isDirectory()) {
                    /* Folder creation failed -> Check/assume why */
                    /* Check for Windows related path length problems. */
                    if (CrossSystem.isWindows() && looksLikeTooLongWindowsPathOrFilename(thisfolder)) {
                        /*
                         * Assume that path is too long. We could check it by writing a shorter folder but it would not change the end
                         * result: The path is not usable for us.
                         */
                        // controller.getLogger().severe("Looks like too long downloadpath for Windows: " + thisfolder.getAbsolutePath());
                        throw new BadFilePathException(thisfolder, BadFilePathException.PathFailureReason.PATH_TOO_LONG, index);
                    }
                    throw new BadFilePathException(thisfolder, BadFilePathException.PathFailureReason.PERMISSION_PROBLEM_FOLDER, index);
                }
            }
        }
        /* Check file writability if needed. */
        if (shouldBeFile && checkFileWrite) {
            /**
             * Cheap pre-check: if the OS already reports the target folder as non-writable, we can short-circuit without doing an actual
             * write-test. </br>
             * Note: File.canWrite() is only reliable in the negative direction here (especially on Windows, where it reflects the read-only
             * attribute rather than ACLs), so a positive result still falls through to the real write-test below.
             */
            if (parentFolder.isDirectory() && !parentFolder.canWrite()) {
                throw new BadFilePathException(file, BadFilePathException.PathFailureReason.PERMISSION_PROBLEM_FILE, pathlist.size() - 1);
            }
            try {
                fileWriteCheck(file, errorOnWriteTestDeletionFailure);
            } catch (final IOException e1) {
                if (e1 instanceof BadFilePathException && ((BadFilePathException) e1).getReason() == BadFilePathException.PathFailureReason.WRITE_TEST_DELETION_FAILURE) {
                    /*
                     * Write succeeded, only the test file could not be deleted again -> not a write/permission/length problem, let the
                     * caller handle it.
                     */
                    throw e1;
                }
                /* Check for a too long filename */
                if (!allowCheckForTooLongFilename) {
                    /** Filename too long could be the problem but we are not allowed to check --> Assume permission issue */
                    throw new BadFilePathException(file, BadFilePathException.PathFailureReason.PERMISSION_PROBLEM_FILE, pathlist.size() - 1);
                }
                /**
                 * Check if we can write in this folder. <br>
                 * If for some reason the file already exists, assume that we got write permissions.
                 */
                final File writeTestFile = new File(file.getParent(), "jd_accessCheck_" + new UniqueAlltimeID().getID());
                if (!writeTestFile.exists()) {
                    try {
                        fileWriteCheck(writeTestFile, errorOnWriteTestDeletionFailure);
                    } catch (final IOException e2) {
                        if (e2 instanceof BadFilePathException && ((BadFilePathException) e2).getReason() == BadFilePathException.PathFailureReason.WRITE_TEST_DELETION_FAILURE) {
                            /*
                             * Write succeeded, only the test file could not be deleted again -> not a permission problem, let the caller
                             * handle it.
                             */
                            throw e2;
                        }
                        /* Permission issue because we were unable to write any file in this directory. */
                        throw new BadFilePathException(file, BadFilePathException.PathFailureReason.PERMISSION_PROBLEM_FILE, pathlist.size() - 1);
                    }
                }
                /*
                 * We assume that the given filename is too long because writing a file with a shorter filename was successful.
                 */
                throw new BadFilePathException(file, BadFilePathException.PathFailureReason.PATH_SEGMENT_TOO_LONG, pathlist.size() - 1);
            }
        }
    }

    /**
     * Writes file and deletes it again.
     *
     * @param errorOnDeletionFailure
     *            if true, a failure to delete the test-written file again is reported as an {@link IOException}; if false, such a failure
     *            is silently ignored (the write itself succeeded, which is what the check cares about).
     */
    public static void fileWriteCheck(final File file, final boolean errorOnDeletionFailure) throws IOException {
        final RandomAccessFile raffile = IO.open(file, "rw");
        raffile.close();
        if (!file.delete() && errorOnDeletionFailure) {
            /* This should never happen! */
            throw new BadFilePathException(file, BadFilePathException.PathFailureReason.WRITE_TEST_DELETION_FAILURE);
        }
    }

    public static boolean looksLikeTooLongWindowsPathOrFilename(final File file) throws IOException {
        final String[] folders = CrossSystem.getPathComponents(file);
        for (final String folder : folders) {
            if (looksLikeTooLongWindowsPathSegment(folder)) {
                return true;
            }
        }
        return false;
    }

    public static boolean looksLikeTooLongWindowsPathSegment(final String str) throws IOException {
        return str.length() > 255;
    }

    /**
     * Checks - WITHOUT canonicalizing - whether any component of the given path exceeds the per-segment limit (255 characters on NTFS).
     * </br>
     * We must not canonicalize here: {@link File#getCanonicalPath()} throws a raw "invalid parameter" IOException for over-long segments,
     * which would mask the real cause. Walking {@link File#getParentFile()} / {@link File#getName()} avoids that.
     */
    public static boolean hasTooLongWindowsPathSegment(final File file) {
        File current = file;
        while (current != null) {
            if (current.getName().length() > 255) {
                return true;
            }
            current = current.getParentFile();
        }
        return false;
    }
}
