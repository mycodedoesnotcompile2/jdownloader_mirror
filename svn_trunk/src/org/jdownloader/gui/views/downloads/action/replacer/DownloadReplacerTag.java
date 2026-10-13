package org.jdownloader.gui.views.downloads.action.replacer;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.Files;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.controlling.Priority;
import org.jdownloader.controlling.contextmenu.ReplacerTag;
import org.jdownloader.controlling.contextmenu.ReplacerTagDefinition;
import org.jdownloader.extensions.extraction.Archive;
import org.jdownloader.extensions.extraction.ArchiveFile;
import org.jdownloader.extensions.extraction.bindings.crawledlink.CrawledLinkArchiveFile;
import org.jdownloader.extensions.extraction.bindings.downloadlink.DownloadLinkArchiveFile;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.ArchiveValidator;
import org.jdownloader.gui.views.components.packagetable.LinkTreeUtils;
import org.jdownloader.gui.views.downloads.columns.TaskColumn.ColumnHelper;
import org.jdownloader.gui.views.downloads.table.DownloadsTableModel;
import org.jdownloader.settings.GeneralSettings;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.UrlDisplayType;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.linkcrawler.CrawledPackageView;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.FilePackageView;
import jd.plugins.download.HashInfo;

/**
 * All tags that can be used in patterns for links and packages of the download list and the linkgrabber. One place for the definition
 * (possible replaces, description, text for missing data) and the logic of each tag. Each constant decides itself with instanceof which
 * kinds of objects it can handle (DownloadLink, CrawledLink, FilePackage, CrawledPackage). To support more, extend the constant. To add a
 * tag, add a constant and put it into the lists of the actions that offer it.
 */
public enum DownloadReplacerTag implements ReplacerTag {
    TYPE(new ReplacerTagDefinition("type", "Always \"Package\" for packages and \"Link\" for links", "").setAlwaysAvailable(true)) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            return isPackage(node) ? "Package" : "Link";
        }
    },
    /**
     * Legacy tag: The name of the object, whatever it is (file name of a link, name of a package). Not offered in the menu editor, use
     * {filename} or {packagename}.
     */
    NAME(new ReplacerTagDefinition("name", "Name of the link or package", "")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            return getName(node);
        }
    },
    /** The file name. Packages have none. */
    FILE_NAME(new ReplacerTagDefinition("filename", "File name, e.g. 'myvideofile.mp4'", "")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            if (!isLink(node)) {
                return UNSUPPORTED;
            }
            return getName(node);
        }
    },
    NAME_WITHOUT_EXTENSION(new ReplacerTagDefinition("name_noext", "File name without extension, e.g. 'myvideofile'", "")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            if (!isLink(node)) {
                return UNSUPPORTED;
            }
            return Files.getFileNameWithoutExtension(getName(node));
        }
    },
    EXTENSION(new ReplacerTagDefinition("ext", "File extension in upper case, e.g. MP4", "")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            if (!isLink(node)) {
                return UNSUPPORTED;
            }
            final String extension = Files.getExtension(getName(node));
            return extension != null ? extension.toUpperCase(Locale.ENGLISH) : null;
        }
    },
    /** The name of the package. For a link the package it is in. */
    PACKAGE_NAME(new ReplacerTagDefinition("packagename", "Name of the package. For a link the name of the package it is in", "")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            return getPackageName(node);
        }
    },
    PATH(new ReplacerTagDefinition("path", "Download folder, e.g. C:\\Downloads\\My Package", "").setAlwaysAvailable(true)) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            return StringUtils.valueOfOrNull(LinkTreeUtils.getDownloadDirectory(node));
        }
    },
    HOST(new ReplacerTagDefinition("host", "Host of the link, e.g. example.com", "")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            if (node instanceof DownloadLink) {
                return ((DownloadLink) node).getHost();
            } else if (node instanceof CrawledLink) {
                return ((CrawledLink) node).getHost();
            } else {
                return UNSUPPORTED;
            }
        }
    },
    COMMENT(new ReplacerTagDefinition("comment", "Comment", "")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            if (node instanceof DownloadLink) {
                return ((DownloadLink) node).getComment();
            } else if (node instanceof CrawledLink) {
                return ((CrawledLink) node).getComment();
            } else if (node instanceof FilePackage) {
                return ((FilePackage) node).getComment();
            } else if (node instanceof CrawledPackage) {
                return ((CrawledPackage) node).getComment();
            } else {
                return UNSUPPORTED;
            }
        }
    },
    AVAILABILITY(new ReplacerTagDefinition("availability", "Online status, e.g. ONLINE, OFFLINE, UNKNOWN", "").setAlwaysAvailable(true)) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            if (node instanceof DownloadLink) {
                return ((DownloadLink) node).getAvailableStatus();
            } else if (node instanceof CrawledLink) {
                return ((CrawledLink) node).getLinkState();
            } else {
                return UNSUPPORTED;
            }
        }
    },
    /** The same text that is shown in the status column. Crawled links (linkgrabber) use their availability state instead. */
    STATUS(new ReplacerTagDefinition("status", "Text of the status column, e.g. Finished, Finished(mirror). In the linkgrabber the availability text", "")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            if (!isLink(node)) {
                return UNSUPPORTED;
            }
            return getStatus(node);
        }
    },
    ENABLED(new ReplacerTagDefinition("enabled", "true or false", "").setAlwaysAvailable(true)) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            if (!isLink(node)) {
                return UNSUPPORTED;
            }
            return Boolean.toString(isEnabled(node));
        }
    },
    /** The effective priority. Links with default priority inherit the priority of their package. */
    PRIORITY(new ReplacerTagDefinition("priority", "LOWEST, LOWER, LOW, DEFAULT, HIGH, HIGHER or HIGHEST (links with default priority inherit the package priority)", "").setAlwaysAvailable(true)) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            if (!isLink(node)) {
                return UNSUPPORTED;
            }
            return getPriority(node);
        }
    },
    /**
     * {hash} is the checksum of the link whatever its type is. {hash_md5} and the other type specific tags only have a value if the
     * checksum is of exactly that type.
     */
    HASH(new ReplacerTagDefinition(HashTags.replaces(), "Checksum of the link. {hash} works for every type, {hash_<type>} only gives a value if the checksum is of exactly that type", "")) {
        @Override
        protected boolean isResponsibleFor(final String key) {
            return "hash".equals(key) || HashTags.TYPES.containsKey(key);
        }

        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            if (!isLink(node)) {
                return UNSUPPORTED;
            }
            final DownloadLink downloadLink = getDownloadLink(node);
            final HashInfo hashInfo = downloadLink != null ? downloadLink.getHashInfo() : null;
            if (hashInfo == null) {
                return null;
            } else if ("hash".equals(key) || hashInfo.getType() == HashTags.TYPES.get(key)) {
                return hashInfo.getHash();
            } else {
                return null;
            }
        }
    },
    /** The size of the link. For a package its total size. */
    FILESIZE(new ReplacerTagDefinition(new String[] { "filesize", "filesize_kib", "filesize_mib", "filesize_gib", "filesize_raw" }, "Size in bytes, KiB, MiB or GiB (the largest fitting unit up to the selected one). filesize_raw is the plain number of bytes.", "~").setEmptyCharacter("filesize_raw", "-1")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            final long size = getSize(node);
            if (size == UNKNOWN_TYPE) {
                return UNSUPPORTED;
            }
            return formatSize(size, key.substring("filesize".length()));
        }
    },
    /** The total size of the package. For a link the package it is in. */
    PACKAGE_SIZE(new ReplacerTagDefinition(new String[] { "packagesize", "packagesize_kib", "packagesize_mib", "packagesize_gib", "packagesize_raw" }, "Total size of the package (of the link), in bytes, KiB, MiB or GiB. packagesize_raw is the plain number of bytes.", "~").setEmptyCharacter("packagesize_raw", "-1")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            final AbstractNode pkg = getPackage(node);
            if (pkg == null) {
                return null;
            }
            return formatSize(getSize(pkg), key.substring("packagesize".length()));
        }
    },
    URL(new ReplacerTagDefinition(new String[] { "url", "url.container", "url.content", "url.origin", "url.referrer" }, "Displayed URL, container URL, content URL, origin URL or referrer URL", "")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            if (!isLink(node)) {
                return UNSUPPORTED;
            }
            final DownloadLink downloadLink = getDownloadLink(node);
            if ("url".equals(key)) {
                if (downloadLink != null) {
                    return downloadLink.getView().getDisplayUrl();
                } else {
                    /* Crawled link that has not been resolved to a DownloadLink yet */
                    return ((CrawledLink) node).getUrlLink();
                }
            } else if ("url.container".equals(key)) {
                return LinkTreeUtils.getUrlByType(UrlDisplayType.CONTAINER, node);
            } else if ("url.content".equals(key)) {
                return LinkTreeUtils.getUrlByType(UrlDisplayType.CONTENT, node);
            } else if ("url.origin".equals(key)) {
                return LinkTreeUtils.getUrlByType(UrlDisplayType.ORIGIN, node);
            } else {
                return LinkTreeUtils.getUrlByType(UrlDisplayType.REFERRER, node);
            }
        }
    },
    /** Looks up the archive of this one link. For many links use {@link DownloadReplacerTag#resolveArchivePasswords(List)}. */
    ARCHIVE_PASSWORD(new ReplacerTagDefinition("archive.password", "Archive password", "")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            if (!isLink(node)) {
                return UNSUPPORTED;
            }
            final List<Archive> archives = ArchiveValidator.getArchivesFromPackageChildren(Arrays.asList(new AbstractNode[] { node }), 1);
            if (archives != null && archives.size() == 1) {
                return archives.get(0).getFinalPassword();
            } else {
                return null;
            }
        }
    },
    PROPERTY(new ReplacerTagDefinition("jd:prop:yourWishedProperty", "Any link property, replace 'yourWishedProperty' e.g. with 'videoId'", "")) {
        @Override
        protected boolean isResponsibleFor(final String key) {
            return key.regionMatches(true, 0, PROPERTY_PREFIX, 0, PROPERTY_PREFIX.length());
        }

        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            if (!isLink(node)) {
                return UNSUPPORTED;
            }
            /* No DownloadLink available (unresolved crawled link): no data */
            final DownloadLink downloadLink = getDownloadLink(node);
            if (downloadLink == null) {
                return null;
            }
            return StringUtils.valueOfOrNull(downloadLink.getProperty(key.substring(PROPERTY_PREFIX.length())));
        }
    },
    /** The text after the underscore is a SimpleDateFormat pattern, e.g. {createdDate_yyyy-MM-dd HH:mm:ss}. */
    DATE(new ReplacerTagDefinition(new String[] { DateTags.NOW + DateTags.FORMAT, DateTags.CREATED + DateTags.FORMAT, DateTags.FINISHED + DateTags.FORMAT }, "Date and time: now, date added, date finished. The text after the underscore is a SimpleDateFormat pattern.", "").setAlwaysAvailable(DateTags.NOW + DateTags.FORMAT)) {
        @Override
        protected boolean isResponsibleFor(final String key) {
            return key.startsWith(DateTags.NOW) || key.startsWith(DateTags.CREATED) || key.startsWith(DateTags.FINISHED);
        }

        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            return formatDate(key, node);
        }
    },
    /** The modification time of the downloaded file. */
    MODIFIED_DATE(new ReplacerTagDefinition(DateTags.MODIFIED + DateTags.FORMAT, "Modification time of the downloaded file on disk. Empty if the file does not exist or if 'GeneralSettings.useoriginallastmodified' is disabled. The text after the underscore is a SimpleDateFormat pattern.", "")) {
        @Override
        protected boolean isResponsibleFor(final String key) {
            return key.startsWith(DateTags.MODIFIED);
        }

        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            if (!isLink(node)) {
                return UNSUPPORTED;
            }
            return formatDate(key, node);
        }
    },
    /** The comment of the package. For a link the package it is in. */
    PACKAGE_COMMENT(new ReplacerTagDefinition("packagecomment", "Comment of the package. For a link the comment of the package it is in", "")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            final AbstractNode pkg = getPackage(node);
            return pkg != null ? COMMENT.resolve("comment", pkg) : null;
        }
    },
    PACKAGE_STATUS(new ReplacerTagDefinition("packagestatus", "Status of the package. For a link the status of the package it is in", "")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            final AbstractNode pkg = getPackage(node);
            return pkg != null ? getStatus(pkg) : null;
        }
    },
    PACKAGE_ENABLED(new ReplacerTagDefinition("packageenabled", "true or false: Whether the package is enabled. For a link the package it is in", "").setAlwaysAvailable(true)) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            final AbstractNode pkg = getPackage(node);
            return pkg != null ? Boolean.toString(isEnabled(pkg)) : Boolean.toString(false);
        }
    },
    PACKAGE_PRIORITY(new ReplacerTagDefinition("packagepriority", "Priority of the package. For a link the package it is in", "")) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            final AbstractNode pkg = getPackage(node);
            return pkg != null ? getPriority(pkg) : null;
        }
    },
    /** The dates of the package. For a link the package it is in. The text after the underscore is a SimpleDateFormat pattern. */
    PACKAGE_DATE(new ReplacerTagDefinition(new String[] { DateTags.PACKAGE_CREATED + DateTags.FORMAT, DateTags.PACKAGE_FINISHED + DateTags.FORMAT, DateTags.PACKAGE_MODIFIED + DateTags.FORMAT }, "Date and time of the package (of the link): date added, date finished, modification time. The text after the underscore is a SimpleDateFormat pattern.", "")) {
        @Override
        protected boolean isResponsibleFor(final String key) {
            return key.startsWith(DateTags.PACKAGE_CREATED) || key.startsWith(DateTags.PACKAGE_FINISHED) || key.startsWith(DateTags.PACKAGE_MODIFIED);
        }

        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            final AbstractNode pkg = getPackage(node);
            if (pkg == null) {
                return null;
            }
            /* packageCreatedDate_... is the createdDate_... of the package */
            final String packageKey = key.substring("package".length());
            return formatDate(Character.toLowerCase(packageKey.charAt(0)) + packageKey.substring(1), pkg);
        }
    },
    NEWLINE(new ReplacerTagDefinition("newline", "Line break", "").setAlwaysAvailable(true)) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            return CrossSystem.getNewLine();
        }
    },
    TAB(new ReplacerTagDefinition("tab", "Tab character", "").setAlwaysAvailable(true)) {
        @Override
        protected Object resolve(final String key, final AbstractNode node) {
            return "\t";
        }
    };

    private static final String PROPERTY_PREFIX = "jd:prop:";
    /* Returned by resolve if the tag cannot be used with this kind of object */
    private static final Object UNSUPPORTED     = new Object();

    /**
     * The hash types and their tags, e.g. hash_md5. The tags without prefix, e.g. md5, are still supported for existing patterns.
     */
    private static final class HashTags {
        private static final Map<String, HashInfo.TYPE> TYPES = new HashMap<String, HashInfo.TYPE>();
        static {
            for (final HashInfo.TYPE hashType : HashInfo.TYPE.values()) {
                switch (hashType) {
                case NONE:
                    /* Not a real hash type: no tag */
                    break;
                default:
                    final String typeName = hashType.name().replace("-", "").toLowerCase(Locale.ENGLISH);
                    TYPES.put("hash_" + typeName, hashType);
                    /* Legacy format without prefix, e.g. {md5} */
                    TYPES.put(typeName, hashType);
                    break;
                }
            }
        }

        private static String[] replaces() {
            final List<String> replaces = new ArrayList<String>();
            replaces.add("hash");
            for (final HashInfo.TYPE hashType : HashInfo.TYPE.values()) {
                switch (hashType) {
                case NONE:
                    break;
                default:
                    replaces.add("hash_" + hashType.name().replace("-", "").toLowerCase(Locale.ENGLISH));
                    break;
                }
            }
            return replaces.toArray(new String[replaces.size()]);
        }
    }

    private static final class DateTags {
        private static final String FORMAT   = "yyyy-MM-dd HH:mm:ss";
        private static final String NOW      = "date_";
        private static final String CREATED  = "createdDate_";
        private static final String FINISHED = "finishedDate_";
        private static final String MODIFIED = "modifiedDate_";
        private static final String PACKAGE_CREATED  = "packageCreatedDate_";
        private static final String PACKAGE_FINISHED = "packageFinishedDate_";
        private static final String PACKAGE_MODIFIED = "packageModifiedDate_";
    }

    private final ReplacerTagDefinition definition;

    private DownloadReplacerTag(final ReplacerTagDefinition definition) {
        this.definition = definition;
    }

    @Override
    public List<String> getReplaces() {
        return definition.getReplaces();
    }

    @Override
    public String getDescription() {
        return definition.getDescription();
    }

    @Override
    public String getEmptyCharacter(final String replace) {
        return definition.getEmptyCharacter(replace);
    }

    @Override
    public boolean isAlwaysAvailable(final String replace) {
        return definition.isAlwaysAvailable(replace);
    }

    @Override
    public String toTag(final String replace, final String emptyCharacterToUse) {
        return definition.toTag(replace, emptyCharacterToUse);
    }

    /**
     * @return true if this tag handles the given tag. By default the tag has to be one of the replaces. Overridden by tags with a variable
     *         part.
     */
    protected boolean isResponsibleFor(final String key) {
        return definition.isResponsibleFor(key);
    }

    @Override
    public final String apply(final String replace, final Object object) {
        if (!(object instanceof AbstractNode) || !isResponsibleFor(replace)) {
            return null;
        }
        final Object result = resolve(replace, (AbstractNode) object);
        if (result == UNSUPPORTED) {
            return null;
        } else if (result == null) {
            /* No data */
            return "";
        } else {
            return result.toString();
        }
    }

    /**
     * @param key
     *            The tag as written in the pattern, e.g. "hash_md5"
     * @return The value, null if there is no data or UNSUPPORTED if the tag cannot be used with this kind of link or package
     */
    protected abstract Object resolve(String key, AbstractNode node);

    /* Returned by getSize if the object is neither a link nor a package */
    private static final long UNKNOWN_TYPE = Long.MIN_VALUE;

    /**
     * @return The package of a link, or the package itself. Null if a crawled link is in no package.
     */
    private static AbstractNode getPackage(final AbstractNode node) {
        if (node instanceof DownloadLink) {
            return ((DownloadLink) node).getFilePackage();
        } else if (node instanceof CrawledLink) {
            return ((CrawledLink) node).getParentNode();
        } else if (isPackage(node)) {
            return node;
        } else {
            return null;
        }
    }

    /**
     * @return The size of the link or the total size of the package. -1 if it is unknown or {@link #UNKNOWN_TYPE} for other objects.
     */
    private static long getSize(final AbstractNode node) {
        if (node instanceof DownloadLink) {
            return ((DownloadLink) node).getView().getBytesTotalEstimated();
        } else if (node instanceof CrawledLink) {
            return ((CrawledLink) node).getSize();
        } else if (node instanceof FilePackage) {
            final FilePackageView fpv = new FilePackageView((FilePackage) node);
            fpv.aggregate();
            return fpv.getSize();
        } else if (node instanceof CrawledPackage) {
            return new CrawledPackageView((CrawledPackage) node).aggregate().getFileSize();
        } else {
            return UNKNOWN_TYPE;
        }
    }

    /**
     * @param unit
     *            "" (bytes), "_kib", "_mib", "_gib" or "_raw"
     * @return The formatted size or null if the size is unknown
     */
    private static String formatSize(final long size, final String unit) {
        if (size < 0) {
            return null;
        } else if ("_raw".equals(unit)) {
            return Long.toString(size);
        } else if ("_kib".equals(unit)) {
            return SIZEUNIT.formatValue(SIZEUNIT.KiB, size);
        } else if ("_mib".equals(unit)) {
            return SIZEUNIT.formatValue(SIZEUNIT.MiB, size);
        } else if ("_gib".equals(unit)) {
            return SIZEUNIT.formatValue(SIZEUNIT.GiB, size);
        } else {
            return SIZEUNIT.formatValue(SIZEUNIT.B, size);
        }
    }

    /**
     * @return The text of the status column. For crawled links (linkgrabber) the availability. Null if there is none.
     */
    private static String getStatus(final AbstractNode node) {
        if (node instanceof DownloadLink || node instanceof FilePackage) {
            final ColumnHelper helper = new ColumnHelper();
            DownloadsTableModel.getInstance().getTaskColumn().fillColumnHelper(helper, node);
            return helper.getString();
        } else if (node instanceof CrawledLink) {
            final Object state = ((CrawledLink) node).getLinkState();
            return state != null ? state.toString() : null;
        } else {
            return null;
        }
    }

    private static boolean isEnabled(final AbstractNode node) {
        if (node instanceof FilePackage) {
            return ((FilePackage) node).getView().isEnabled();
        } else if (node instanceof CrawledPackage) {
            return ((CrawledPackage) node).getView().isEnabled();
        } else {
            return node.isEnabled();
        }
    }

    /**
     * @return The effective priority. Links with default priority inherit the priority of their package.
     */
    private static Priority getPriority(final AbstractNode node) {
        if (node instanceof DownloadLink) {
            final DownloadLink link = (DownloadLink) node;
            final Priority priority = link.getPriorityEnum();
            final FilePackage parent = link.getFilePackage();
            if (Priority.DEFAULT.equals(priority) && parent != null) {
                return parent.getPriorityEnum();
            }
            return priority;
        } else if (node instanceof CrawledLink) {
            final CrawledLink link = (CrawledLink) node;
            final Priority priority = link.getPriority();
            final CrawledPackage parent = link.getParentNode();
            if (Priority.DEFAULT.equals(priority) && parent != null) {
                return parent.getPriorityEnum();
            }
            return priority;
        } else if (node instanceof FilePackage) {
            return ((FilePackage) node).getPriorityEnum();
        } else if (node instanceof CrawledPackage) {
            return ((CrawledPackage) node).getPriorityEnum();
        } else {
            return null;
        }
    }

    /**
     * Resolves tags like date_yyyy-MM-dd. The text after the underscore is the date format.
     *
     * @param key
     *            date_..., createdDate_..., finishedDate_... or modifiedDate_...
     * @return The formatted date or null if it is unknown or the date format is invalid
     */
    private static String formatDate(final String key, final AbstractNode node) {
        final long timestamp;
        final String format;
        if (key.startsWith(DateTags.NOW)) {
            timestamp = System.currentTimeMillis();
            format = key.substring(DateTags.NOW.length());
        } else if (key.startsWith(DateTags.CREATED)) {
            timestamp = node.getCreated();
            format = key.substring(DateTags.CREATED.length());
        } else if (key.startsWith(DateTags.FINISHED)) {
            timestamp = getFinishedTimestamp(node);
            format = key.substring(DateTags.FINISHED.length());
        } else {
            timestamp = getModifiedTimestamp(node);
            format = key.substring(DateTags.MODIFIED.length());
        }
        if (timestamp <= 0) {
            return null;
        }
        try {
            return new SimpleDateFormat(format, Locale.ENGLISH).format(new Date(timestamp));
        } catch (final Throwable e) {
            /* Invalid date format */
            return null;
        }
    }

    private static boolean isPackage(final AbstractNode node) {
        return node instanceof FilePackage || node instanceof CrawledPackage;
    }

    private static boolean isLink(final AbstractNode node) {
        return node instanceof DownloadLink || node instanceof CrawledLink;
    }

    /**
     * @return The DownloadLink of the link. Null for packages and for crawled links that have not been resolved to a real link yet.
     */
    private static DownloadLink getDownloadLink(final AbstractNode node) {
        if (node instanceof DownloadLink) {
            return (DownloadLink) node;
        } else if (node instanceof CrawledLink) {
            return ((CrawledLink) node).getDownloadLink();
        } else {
            return null;
        }
    }

    /**
     * @return The name of the package or the file name of the link
     */
    private static String getName(final AbstractNode node) {
        if (node instanceof DownloadLink) {
            return ((DownloadLink) node).getView().getDisplayName();
        } else if (node instanceof CrawledLink) {
            return ((CrawledLink) node).getName();
        } else if (node instanceof FilePackage) {
            return ((FilePackage) node).getName();
        } else if (node instanceof CrawledPackage) {
            return ((CrawledPackage) node).getName();
        } else {
            return null;
        }
    }

    /**
     * @return The name of the package of the link or the name of the package itself
     */
    private static String getPackageName(final AbstractNode node) {
        if (node instanceof DownloadLink) {
            final FilePackage fp = ((DownloadLink) node).getFilePackage();
            return fp != null ? fp.getName() : null;
        } else if (node instanceof CrawledLink) {
            final CrawledPackage cp = ((CrawledLink) node).getParentNode();
            return cp != null ? cp.getName() : null;
        } else {
            return getName(node);
        }
    }

    private static long getFinishedTimestamp(final AbstractNode node) {
        if (node instanceof DownloadLink) {
            return ((DownloadLink) node).getFinishedDate();
        } else if (node instanceof CrawledLink) {
            return ((CrawledLink) node).getFinishedDate();
        } else if (node instanceof FilePackage) {
            return ((FilePackage) node).getFinishedDate();
        } else if (node instanceof CrawledPackage) {
            return ((CrawledPackage) node).getFinishedDate();
        } else {
            return -1;
        }
    }

    /**
     * Links: last modified timestamp of the file on disk. Packages: their modified timestamp. Returns -1 if not available.
     */
    private static long getModifiedTimestamp(final AbstractNode node) {
        if (node instanceof DownloadLink) {
            if (!JsonConfig.create(GeneralSettings.class).isUseOriginalLastModified()) {
                /**
                 * User has disabled writing last modified timestamp to files -> Makes no sense for us to even check for it. <br>
                 * Possible edge case: User has disabled last modified timestamp now but it was enabled before.
                 */
                return -1;
            }
            final DownloadLink link = (DownloadLink) node;
            final long lastModified = link.getLastModifiedTimestamp();
            if (lastModified > 0) {
                return lastModified;
            }
            /* Heavier operation: Get that timestamp from file. */
            return new File(link.getFileOutput()).lastModified();
        } else if (node instanceof AbstractPackageNode) {
            return ((AbstractPackageNode) node).getModified();
        } else {
            return -1;
        }
    }

    /**
     * The content URL of a link that is the only link of the selection: The content URL, or the plugin pattern if there is none. Returns
     * an empty string if there is no DownloadLink.
     */
    public static String getContentUrlOfSingleLink(final AbstractNode node) {
        final DownloadLink downloadLink = getDownloadLink(node);
        if (downloadLink == null) {
            return "";
        }
        final String contentUrl = downloadLink.getContentUrl();
        return contentUrl != null ? contentUrl : downloadLink.getPluginPatternMatcher();
    }

    /**
     * Resolves the archive passwords of all given links with a single archive lookup instead of one lookup per link (which is expensive
     * because of file IO). A link only gets a password if it belongs to exactly one archive.
     */
    public static Map<AbstractNode, String> resolveArchivePasswords(final List<? extends AbstractNode> nodes) {
        final List<AbstractNode> links = new ArrayList<AbstractNode>();
        for (final AbstractNode node : nodes) {
            if (isLink(node)) {
                links.add(node);
            }
        }
        final Map<AbstractNode, String> ret = new IdentityHashMap<AbstractNode, String>();
        if (links.size() == 0) {
            return ret;
        }
        final List<Archive> archives = ArchiveValidator.getArchivesFromPackageChildren(links);
        if (archives == null) {
            return ret;
        }
        final Map<Object, Archive> archiveByLink = new IdentityHashMap<Object, Archive>();
        final Set<Object> ambiguousLinks = Collections.newSetFromMap(new IdentityHashMap<Object, Boolean>());
        for (final Archive archive : archives) {
            for (final ArchiveFile archiveFile : archive.getArchiveFiles()) {
                final List<?> archiveFileLinks;
                if (archiveFile instanceof DownloadLinkArchiveFile) {
                    archiveFileLinks = ((DownloadLinkArchiveFile) archiveFile).getDownloadLinks();
                } else if (archiveFile instanceof CrawledLinkArchiveFile) {
                    archiveFileLinks = ((CrawledLinkArchiveFile) archiveFile).getLinks();
                } else {
                    continue;
                }
                for (final Object link : archiveFileLinks) {
                    final Archive existing = archiveByLink.put(link, archive);
                    if (existing != null && existing != archive) {
                        ambiguousLinks.add(link);
                    }
                }
            }
        }
        for (final AbstractNode link : links) {
            final Archive archive = archiveByLink.get(link);
            if (archive != null && !ambiguousLinks.contains(link)) {
                ret.put(link, archive.getFinalPassword());
            }
        }
        return ret;
    }
}
