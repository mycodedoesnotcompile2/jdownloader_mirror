package org.jdownloader.plugins.components.config;

import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultStringValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginHost;
import org.jdownloader.plugins.config.Type;

@PluginHost(host = "drive.google.com", type = Type.HOSTER)
public interface GoogleConfig extends PluginConfigInterface {
    // ========================================
    // CONSTANTS
    // ========================================
    final String                    text_UserAgent                                                 = "User-Agent which will be used for all Google website http requests";
    final String                    text_PreferredVideoQuality                                     = "Preferred video stream quality.\r\nIf the preferred stream quality is not found, best stream quality will be downloaded instead.";
    final String                    text_AllowStreamDownloadAsFallbackIfFileDownloadQuotaIsReached = "Allow stream download as fallback if original file is quota limited?";
    final String                    text_AllowStreamDownloadAsFallbackIfOfficialDownloadIsDisabled = "Allow stream download as fallback if original file download is disabled?";
    final String                    text_GoogleDriveAPIKey                                         = "Google Drive API key see: developers.google.com/drive/api/v3/enable-drive-api\r\nIt will be used for GDrive folder crawling, linkchecking and downloading.";
    final String                    text_AddStreamQualityIdentifierToFilename                      = "Stream download: Add quality identifier to filename?";
    final String                    text_WaitOnQuotaReachedMinutes                                 = "Wait time minutes on quota limit reached";
    final String                    text_DebugAccountLogin                                         = "Debug: Website mode: Perform extended account check (enable = slower account check)?";
    final String                    text_DebugForceValidateLoginAlways                             = "Debug: Website mode: Force validate login on every linkcheck/download attempt (enable = slower linkcheck!)?";
    final String                    text_DebugWebsiteTrustQuickLinkcheckOfflineStatus              = "Debug: Website mode: Trust quick linkcheck offline status (enable = can speed up linkcheck)?";
    final String                    text_DebugWebsiteAlwaysPerformExtendedLinkcheck                = "Debug: Website mode: Always perform extended linkcheck? (enable = may slow down linkcheck)?";
    public static final TRANSLATION TRANSLATION                                                    = new TRANSLATION();

    // ========================================
    // ENUMS
    // ========================================
    public static enum PreferredVideoQuality implements LabelInterface {
        ORIGINAL {
            @Override
            public String getLabel() {
                return "Original file";
            }
        },
        STREAM_BEST {
            @Override
            public String getLabel() {
                return "Best stream";
            }
        },
        STREAM_360P {
            @Override
            public String getLabel() {
                return "360p";
            }
        },
        STREAM_480P {
            @Override
            public String getLabel() {
                return "480p";
            }
        },
        STREAM_720P {
            @Override
            public String getLabel() {
                return "720p";
            }
        },
        STREAM_1080P {
            @Override
            public String getLabel() {
                return "1080p";
            }
        };
    }

    public static enum GoogleDocumentFormatSelectionMode implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto: Prefer format from title if file title contains extension";
            }
        },
        USER_SETTING {
            @Override
            public String getLabel() {
                return "Prefer selected format";
            }
        };
    }

    public static enum GoogleDocumentExportFormat implements LabelInterface, FileExtensionInterface {
        ZIP {
            @Override
            public String getLabel() {
                return "Archive (.zip)";
            }

            @Override
            public String getFileExtension() {
                return "zip";
            }
        },
        DOCX {
            @Override
            public String getLabel() {
                return "Microsoft Word (.docx)";
            }

            @Override
            public String getFileExtension() {
                return "docx";
            }
        },
        ODT {
            @Override
            public String getLabel() {
                return "OpenDocument Format (.odt)";
            }

            @Override
            public String getFileExtension() {
                return "odt";
            }
        },
        RTF {
            @Override
            public String getLabel() {
                return "RTF File (.rtf)";
            }

            @Override
            public String getFileExtension() {
                return "rtf";
            }
        },
        PDF {
            @Override
            public String getLabel() {
                return "PDF File (.pdf)";
            }

            @Override
            public String getFileExtension() {
                return "pdf";
            }
        },
        TXT {
            @Override
            public String getLabel() {
                return "Plain Text File (.txt)";
            }

            @Override
            public String getFileExtension() {
                return "txt";
            }
        },
        HTML {
            @Override
            public String getLabel() {
                return "Website (.html)";
            }

            @Override
            public String getFileExtension() {
                return "html";
            }
        },
        EPUB {
            @Override
            public String getLabel() {
                return "EPUB Publication (.epub)";
            }

            @Override
            public String getFileExtension() {
                return "epub";
            }
        },
        MD {
            @Override
            public String getLabel() {
                return "Markdown (.md)";
            }

            @Override
            public String getFileExtension() {
                return "md";
            }
        };
    }

    public static enum APIDownloadMode implements LabelInterface {
        API_ONLY {
            @Override
            public String getLabel() {
                return "API only (except for stream downloads)";
            }
        },
        WEBSITE_IF_ACCOUNT_AVAILABLE {
            @Override
            public String getLabel() {
                return "Use website if account is available";
            }
        },
        WEBSITE_IF_ACCOUNT_AVAILABLE_AND_FILE_IS_QUOTA_LIMITED {
            @Override
            public String getLabel() {
                return "Use website if account is available and file is quota limited";
            }
        };
    }

    // ========================================
    // INTERFACES
    // ========================================
    interface FileExtensionInterface {
        String getFileExtension();
    }

    // ========================================
    // TRANSLATION CLASS
    // ========================================
    public static class TRANSLATION {
        public String getUserAgent_label() {
            return text_UserAgent;
        }

        public String getPreferredVideoQuality_label() {
            return text_PreferredVideoQuality;
        }

        public String getAllowStreamDownloadAsFallbackIfFileDownloadQuotaIsReached_label() {
            return text_AllowStreamDownloadAsFallbackIfFileDownloadQuotaIsReached;
        }

        public String getAllowStreamDownloadAsFallbackIfOfficialDownloadIsDisabled_label() {
            return text_AllowStreamDownloadAsFallbackIfOfficialDownloadIsDisabled;
        }

        public String getAddStreamQualityIdentifierToFilename_label() {
            return text_AddStreamQualityIdentifierToFilename;
        }

        public String getGoogleDriveAPIKey_label() {
            return text_GoogleDriveAPIKey;
        }

        public String getAPIDownloadMode_label() {
            return "API download mode";
        }

        public String getWaitOnQuotaReachedMinutes_label() {
            return text_WaitOnQuotaReachedMinutes;
        }

        public String getAllowMultihosterDownload_label() {
            return "Allow multihoster usage?";
        }

        public String getDebugAccountLogin_label() {
            return text_DebugAccountLogin;
        }

        public String getDebugForceValidateLoginAlways_label() {
            return text_DebugForceValidateLoginAlways;
        }

        public String getDebugWebsiteTrustQuickLinkcheckOfflineStatus_label() {
            return text_DebugWebsiteTrustQuickLinkcheckOfflineStatus;
        }

        public String getDebugWebsiteAlwaysPerformExtendedLinkcheck_label() {
            return text_DebugWebsiteAlwaysPerformExtendedLinkcheck;
        }

        public String getGoogleDocumentFormatSelectionMode_label() {
            return "Google document format selection mode";
        }

        public String getGoogleDocumentExportFormatForTypeDocument_label() {
            return "Export format for GDoc type document";
        }

        public String getGoogleDocumentExportFormatForTypePresentation_label() {
            return "Export format for GDoc type presentation";
        }

        public String getGoogleDocumentExportFormatForTypeSpreadsheet_label() {
            return "Export format for GDoc type spreadsheet";
        }
    }

    // ========================================
    // VIDEO/STREAM CONFIGURATION METHODS
    // ========================================
    @AboutConfig
    @DefaultEnumValue("ORIGINAL")
    @DescriptionForConfigEntry(text_PreferredVideoQuality)
    @Order(15)
    PreferredVideoQuality getPreferredVideoQuality();

    void setPreferredVideoQuality(final PreferredVideoQuality quality);

    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry(text_AllowStreamDownloadAsFallbackIfFileDownloadQuotaIsReached)
    @Order(16)
    boolean isAllowStreamDownloadAsFallbackIfFileDownloadQuotaIsReached();

    void setAllowStreamDownloadAsFallbackIfFileDownloadQuotaIsReached(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry(text_AllowStreamDownloadAsFallbackIfOfficialDownloadIsDisabled)
    @Order(17)
    boolean isAllowStreamDownloadAsFallbackIfOfficialDownloadIsDisabled();

    void setAllowStreamDownloadAsFallbackIfOfficialDownloadIsDisabled(boolean b);

    @AboutConfig
    @DefaultBooleanValue(true)
    @DescriptionForConfigEntry(text_AddStreamQualityIdentifierToFilename)
    @Order(18)
    boolean isAddStreamQualityIdentifierToFilename();

    void setAddStreamQualityIdentifierToFilename(boolean b);

    // ========================================
    // GOOGLE DOCUMENT CONFIGURATION METHODS
    // ========================================
    @AboutConfig
    @DefaultEnumValue("AUTO")
    @DescriptionForConfigEntry("Controls format selection behavior for all google document types. Example: If on auto and original file is a pdf, pdf format will be preferred.")
    @Order(60)
    GoogleDocumentFormatSelectionMode getGoogleDocumentFormatSelectionMode();

    void setGoogleDocumentFormatSelectionMode(final GoogleDocumentFormatSelectionMode mode);

    @AboutConfig
    @DefaultEnumValue("ZIP")
    @DescriptionForConfigEntry("Preferred download format for Google Document type document")
    @Order(61)
    GoogleDocumentExportFormat getGoogleDocumentExportFormatForTypeDocument();

    void setGoogleDocumentExportFormatForTypeDocument(final GoogleDocumentExportFormat format);

    @AboutConfig
    @DefaultEnumValue("ZIP")
    @DescriptionForConfigEntry("Preferred download format for Google Document type presentation")
    @Order(62)
    GoogleDocumentExportFormat getGoogleDocumentExportFormatForTypePresentation();

    void setGoogleDocumentExportFormatForTypePresentation(final GoogleDocumentExportFormat format);

    @AboutConfig
    @DefaultEnumValue("ZIP")
    @DescriptionForConfigEntry("Preferred download format for Google Document type spreadsheet")
    @Order(63)
    GoogleDocumentExportFormat getGoogleDocumentExportFormatForTypeSpreadsheet();

    void setGoogleDocumentExportFormatForTypeSpreadsheet(final GoogleDocumentExportFormat format);

    // ========================================
    // QUOTA AND TIMING CONFIGURATION METHODS
    // ========================================
    @AboutConfig
    @SpinnerValidator(min = 10, max = 360, step = 1)
    @DefaultIntValue(60)
    @DescriptionForConfigEntry(text_WaitOnQuotaReachedMinutes)
    @Order(70)
    int getWaitOnQuotaReachedMinutes();

    void setWaitOnQuotaReachedMinutes(int items);

    // ========================================
    // API CONFIGURATION METHODS
    // ========================================
    @AboutConfig
    @DefaultStringValue("")
    @DescriptionForConfigEntry(text_GoogleDriveAPIKey)
    @Order(80)
    String getGoogleDriveAPIKey();

    public void setGoogleDriveAPIKey(String apikey);

    @AboutConfig
    @DefaultEnumValue("WEBSITE_IF_ACCOUNT_AVAILABLE_AND_FILE_IS_QUOTA_LIMITED")
    @DescriptionForConfigEntry("Controls plugin behavior when API key is provided.")
    @Order(81)
    APIDownloadMode getAPIDownloadMode();

    void setAPIDownloadMode(final APIDownloadMode mode);

    // ========================================
    // MULTIHOSTER CONFIGURATION METHODS
    // ========================================
    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry("Enable this to allow Google Drive links to be downloaded via multihoster accounts.")
    @Order(98)
    boolean isAllowMultihosterDownload();

    void setAllowMultihosterDownload(boolean b);

    // ========================================
    // GENERAL CONFIGURATION METHODS
    // ========================================
    @AboutConfig
    @DefaultStringValue("JDDEFAULT")
    @DescriptionForConfigEntry(text_UserAgent)
    @Order(99)
    String getUserAgent();

    public void setUserAgent(final String userAgent);

    // ========================================
    // DEBUG CONFIGURATION METHODS
    // ========================================
    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry(text_DebugAccountLogin)
    @Order(100)
    boolean isDebugAccountLogin();

    void setDebugAccountLogin(boolean b);

    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry(text_DebugForceValidateLoginAlways)
    @Order(101)
    boolean isDebugForceValidateLoginAlways();

    void setDebugForceValidateLoginAlways(boolean b);

    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry(text_DebugWebsiteTrustQuickLinkcheckOfflineStatus)
    @Order(102)
    boolean isDebugWebsiteTrustQuickLinkcheckOfflineStatus();

    void setDebugWebsiteTrustQuickLinkcheckOfflineStatus(boolean b);

    @AboutConfig
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry(text_DebugWebsiteAlwaysPerformExtendedLinkcheck)
    @Order(103)
    boolean isDebugWebsiteAlwaysPerformExtendedLinkcheck();

    void setDebugWebsiteAlwaysPerformExtendedLinkcheck(boolean b);
}