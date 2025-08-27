package org.jdownloader.plugins.components.archiveorg;

import java.util.Set;

import org.appwork.storage.StorableValidatorIgnoresMissingSetter;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumArrayValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultOnNull;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.DevConfig;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "archive.org", type = Type.CRAWLER)
public interface ArchiveOrgConfig extends PluginConfigInterface {
    public static final TRANSLATION TRANSLATION = new TRANSLATION();

    public static class TRANSLATION {
        public String getFileCrawlerTypesToCrawl_label() {
            return "File crawler: Select types to crawl";
        }

        public String getDeselectedTypesLinksMode_label() {
            return "File crawler: How to treat links of deselected types?";
        }

        public String getFileCrawlerCrawlOnlyOriginalVersions_label() {
            return "File crawler: Add only original versions of files?";
        }

        public String getFileCrawlerCrawlMetadataFiles_label() {
            return "File crawler: Include metadata files (typically .xml & .sqlite files)?";
        }

        public String getFileCrawlerCrawlThumbnails_label() {
            return "File crawler: Crawl thumbnails?";
        }

        public String getSingleFilePathNotFoundMode_label() {
            return "File crawler: What to do when single file/folder-path is not found?";
        }

        public String getSingleFileAdoptFolderStructureMode_label() {
            return "File crawler: Single files: Adopt folder structure mode";
        }

        public String getPlaylistFilenameScheme_label() {
            return "Playlist filename scheme";
        }

        public String getBookImageQuality_label() {
            return "Book image quality (0 = highest, 10 = lowest)";
        }

        public String getBookCrawlMode_label() {
            return "Book crawl mode: Which items to return if the added link is a book?";
        }

        public String getMarkNonViewableBookPagesAsOfflineIfNoAccountIsAvailable_label() {
            return "Mark non downloadable book pages as offline if no account is available?";
        }

        public String getNonDownloadableBookPagesMode_label() {
            return "How to display non downloadable book pages in linkgrabber?";
        }

        public String getPlaylistCrawlMode202404_label() {
            return "Playlist crawl mode: Which files to add if the added link is a audio/video playlist?";
        }

        public String getSearchTermCrawlerMaxResultsLimit_label() {
            return "Search term and profile crawler: Limit max results [0 = disable this crawler]";
        }
    }

    @AboutConfig
    @DefaultOnNull
    @DefaultEnumArrayValue(value = { "ORIGINAL", "DERIVATIVE", "METADATA", "THUMBNAIL" })
    @Order(10)
    Set<ArchiveOrgType> getFileCrawlerTypesToCrawl();

    void setFileCrawlerTypesToCrawl(Set<ArchiveOrgType> list);

    @StorableValidatorIgnoresMissingSetter
    public enum ArchiveOrgType {
        ORIGINAL,
        DERIVATIVE,
        METADATA,
        THUMBNAIL
    }

    public static enum DeselectedTypesMode implements LabelInterface {
        ADD_DISABLED {
            @Override
            public String getLabel() {
                return "Add links disabled";
            }
        },
        DO_NOT_ADD_SKIP {
            @Override
            public String getLabel() {
                return "Do not add links (skip)";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("ADD_DISABLED")
    @Order(11)
    DeselectedTypesMode getDeselectedTypesLinksMode();

    void setDeselectedTypesLinksMode(final DeselectedTypesMode mode);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(10)
    @DevConfig
    boolean isFileCrawlerCrawlOnlyOriginalVersions();

    void setFileCrawlerCrawlOnlyOriginalVersions(boolean b);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(25)
    @DevConfig
    boolean isFileCrawlerCrawlMetadataFiles();

    void setFileCrawlerCrawlMetadataFiles(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("Crawl thumbnails?")
    @Order(26)
    @DevConfig
    boolean isFileCrawlerCrawlThumbnails();

    void setFileCrawlerCrawlThumbnails(boolean b);

    final SingleFilePathNotFoundMode default_SingleFilePathNotFoundMode = SingleFilePathNotFoundMode.ADD_ALL;

    public static enum SingleFilePathNotFoundMode implements LabelInterface {
        ADD_ALL {
            @Override
            public String getLabel() {
                return "Add all other items";
            }
        },
        ADD_NOTHING_AND_DISPLAY_ADDED_URL_AS_OFFLINE {
            @Override
            public String getLabel() {
                return "Display added URL as offline";
            }
        },
        DEFAULT {
            @Override
            public String getLabel() {
                return "Default: " + default_SingleFilePathNotFoundMode.getLabel();
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("DEFAULT")
    @Order(27)
    SingleFilePathNotFoundMode getSingleFilePathNotFoundMode();

    void setSingleFilePathNotFoundMode(final SingleFilePathNotFoundMode mode);

    final SingleFileAdoptFolderStructureMode default_SingleFileAdoptFolderStructureMode = SingleFileAdoptFolderStructureMode.ENABLE;

    public static enum SingleFileAdoptFolderStructureMode implements LabelInterface {
        DEFAULT {
            @Override
            public String getLabel() {
                return "Default: " + default_SingleFileAdoptFolderStructureMode.getLabel();
            }
        },
        ENABLE {
            @Override
            public String getLabel() {
                return "Allow adopt folder structure";
            }
        },
        DISABLE {
            @Override
            public String getLabel() {
                return "Do not allow adopt folder structure";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("DEFAULT")
    @Order(28)
    SingleFileAdoptFolderStructureMode getSingleFileAdoptFolderStructureMode();

    void setSingleFileAdoptFolderStructureMode(final SingleFileAdoptFolderStructureMode mode);

    public static enum PlaylistFilenameScheme implements LabelInterface {
        PLAYLIST_TITLE_WITH_TRACK_NUMBER {
            @Override
            public String getLabel() {
                return "Same as website: <TrackNumber>.<title> - <artist>.<fileExt>";
            }
        },
        ORIGINAL_FILENAME {
            @Override
            public String getLabel() {
                return "Original / serverside filenames";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("PLAYLIST_TITLE_WITH_TRACK_NUMBER")
    @Order(29)
    PlaylistFilenameScheme getPlaylistFilenameScheme();

    void setPlaylistFilenameScheme(final PlaylistFilenameScheme scheme);

    @AboutConfig
    @SpinnerValidator(min = 0, max = 10, step = 1)
    @DefaultIntValue(0)
    @Order(30)
    int getBookImageQuality();

    void setBookImageQuality(int i);

    public static enum BookCrawlMode implements LabelInterface {
        PREFER_ORIGINAL {
            @Override
            public String getLabel() {
                return "Original files if possible else loose book pages";
            }
        },
        ORIGINAL_AND_LOOSE_PAGES {
            @Override
            public String getLabel() {
                return "Original files if possible and loose book pages";
            }
        },
        LOOSE_PAGES {
            @Override
            public String getLabel() {
                return "Only loose book pages";
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("PREFER_ORIGINAL")
    @Order(40)
    BookCrawlMode getBookCrawlMode();

    void setBookCrawlMode(final BookCrawlMode bookCrawlerMode);

    @AboutConfig
    @DefaultBooleanValue(true)
    @Order(41)
    @DevConfig
    boolean isMarkNonViewableBookPagesAsOfflineIfNoAccountIsAvailable();

    void setMarkNonViewableBookPagesAsOfflineIfNoAccountIsAvailable(boolean b);

    @AboutConfig
    @DefaultEnumValue("SET_AVAILABLE_STATUS_OFFLINE")
    @Order(41)
    NonDownloadableBookPagesMode getNonDownloadableBookPagesMode();

    void setNonDownloadableBookPagesMode(final NonDownloadableBookPagesMode mode);

    public static enum NonDownloadableBookPagesMode implements LabelInterface {
        SET_AVAILABLE_STATUS_OFFLINE {
            @Override
            public String getLabel() {
                return "Display as offline";
            }
        },
        SET_AVAILABLE_STATUS_ONLINE {
            @Override
            public String getLabel() {
                return "Display as online (download will fail with error status)";
            }
        };
    }

    final PlaylistCrawlMode default_PlaylistCrawlMode = PlaylistCrawlMode.AUTO;

    public static enum PlaylistCrawlMode implements LabelInterface {
        PLAYLIST_ONLY {
            @Override
            public String getLabel() {
                return "Playlist only";
            }
        },
        PLAYLIST_AND_FILES {
            @Override
            public String getLabel() {
                return "Playlist and files";
            }
        },
        FILES_ONLY {
            @Override
            public String getLabel() {
                return "Files only";
            }
        },
        AUTO {
            @Override
            public String getLabel() {
                return "Auto";
            }
        },
        DEFAULT {
            @Override
            public String getLabel() {
                return "Default: " + default_PlaylistCrawlMode.getLabel();
            }
        };
    }

    @AboutConfig
    @DefaultEnumValue("DEFAULT")
    @Order(50)
    @DescriptionForConfigEntry("Handling for audio/video playlists.")
    PlaylistCrawlMode getPlaylistCrawlMode202404();

    void setPlaylistCrawlMode202404(final PlaylistCrawlMode bookCrawlerMode);

    @AboutConfig
    @SpinnerValidator(min = 0, max = 100000, step = 100)
    @DefaultIntValue(100)
    @Order(60)
    int getSearchTermCrawlerMaxResultsLimit();

    void setSearchTermCrawlerMaxResultsLimit(int i);
}