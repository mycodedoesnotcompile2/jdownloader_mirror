package org.jdownloader.plugins.components.archiveorg;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
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
        public String getFileCrawlerCrawlOnlyOriginalVersions_label() {
            return "File crawler: Add only original versions of files?";
        }

        public String getFileCrawlerCrawlMetadataFiles_label() {
            return "File crawler: Include metadata files (typically .xml & .sqlite files)?";
        }

        public String getFileCrawlerCrawlThumbnails_label() {
            return "File crawler: Crawl thumbnails?";
        }

        public String getSingleFilePathNonFoundMode_label() {
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
            return "Book crawl mode";
        }

        public String getMarkNonViewableBookPagesAsOfflineIfNoAccountIsAvailable_label() {
            return "Mark non viewable book pages as offline if no account is available?";
        }

        public String getPlaylistCrawlMode202404_label() {
            return "Audio/video playlist crawl mode";
        }

        public String getSearchTermCrawlerMaxResultsLimit_label() {
            return "Search term and profile crawler: Limit max results [0 = disable this crawler]";
        }
    }

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(10)
    boolean isFileCrawlerCrawlOnlyOriginalVersions();

    void setFileCrawlerCrawlOnlyOriginalVersions(boolean b);

    @AboutConfig
    @DefaultBooleanValue(false)
    @Order(25)
    boolean isFileCrawlerCrawlMetadataFiles();

    void setFileCrawlerCrawlMetadataFiles(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry("Crawl thumbnails?")
    @Order(26)
    boolean isFileCrawlerCrawlThumbnails();

    void setFileCrawlerCrawlThumbnails(boolean b);

    final SingleFilePathNotFoundMode default_SingleFilePathNotFoundMode = SingleFilePathNotFoundMode.ADD_ALL;

    public static enum SingleFilePathNotFoundMode implements LabelInterface {
        ADD_ALL {
            @Override
            public String getLabel() {
                return "Add all (other) items";
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
    SingleFilePathNotFoundMode getSingleFilePathNonFoundMode();

    void setSingleFilePathNonFoundMode(final SingleFilePathNotFoundMode mode);

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
                return "Like in playlist: <TrackNumber>.<title> - <artist>.<fileExt>";
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
    boolean isMarkNonViewableBookPagesAsOfflineIfNoAccountIsAvailable();

    void setMarkNonViewableBookPagesAsOfflineIfNoAccountIsAvailable(boolean b);

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