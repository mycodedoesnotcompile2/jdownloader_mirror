//jDownloader - Downloadmanager
//Copyright (C) 2009  JD-Team support@jdownloader.org
//
//This program is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//This program is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import javax.imageio.ImageIO;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Hash;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.net.URLHelper;
import org.appwork.utils.parser.UrlQuery;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter;
import org.jdownloader.controlling.filter.CompiledFiletypeFilter.ExtensionsFilterInterface;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.plugins.components.archiveorg.ArchiveOrgConfig;
import org.jdownloader.plugins.components.archiveorg.ArchiveOrgConfig.PlaylistFilenameScheme;
import org.jdownloader.plugins.components.archiveorg.ArchiveOrgLendingInfo;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.seamless.util.io.IO;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.http.Browser;
import jd.http.Cookie;
import jd.http.Cookies;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.AccountInvalidException;
import jd.plugins.AccountRequiredException;
import jd.plugins.CryptedLink;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;
import jd.plugins.decrypter.ArchiveOrgCrawler;

@HostPlugin(revision = "$Revision: 51376 $", interfaceVersion = 3, names = { "archive.org" }, urls = { "https?://(?:[\\w\\.]+)?archive\\.org/download/[^/]+/[^/]+(/.+)?" })
public class ArchiveOrg extends PluginForHost {
    public ArchiveOrg(PluginWrapper wrapper) {
        super(wrapper);
        this.enablePremium("https://" + getHost() + "/account/login.createaccount.php");
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.getHeaders().put(HTTPConstants.HEADER_REQUEST_USER_AGENT, "JDownloader");
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.COOKIE_LOGIN_OPTIONAL, LazyPlugin.FEATURE.USERNAME_IS_EMAIL };
    }

    @Override
    public String getAGBLink() {
        return "https://" + getHost() + "/about/terms.php";
    }

    @Override
    public String getLinkID(final DownloadLink link) {
        if (isAudioPlaylistItem(link)) {
            /**
             * This enables us to add the same link twice, one time as playlist item (with different filenames) and one time as "original
             * file". </br>
             * We basically need to trick our own duplicate detection to allow the user to add a file as playlist item and "original" at the
             * same time.
             */
            return super.getLinkID(link) + "_audio_playlist_item";
        } else if (this.isBook(link)) {
            return this.getHost() + "://" + this.getBookID(link) + "/" + this.getBookSubPrefix(link) + "/" + this.getBookPageIndexNumber(link);
        } else {
            return super.getLinkID(link);
        }
    }

    @Override
    public boolean isSpeedLimited(DownloadLink link, Account account) {
        return false;
    }

    public static final String                            PROPERTY_BOOK_ID                                = "book_id";
    public static final String                            PROPERTY_BOOK_SUB_PREFIX                        = "book_sub_prefix";
    /* Book page by number by archive.org. Can start at 0 or 1. Do not use as a real page index! */
    public static final String                            PROPERTY_BOOK_PAGE                              = "book_page";
    public static final String                            PROPERTY_BOOK_PAGE_MAX                          = "book_page_max";
    /* Real page index */
    public static final String                            PROPERTY_BOOK_PAGE_INTERNAL_INDEX               = "book_page_internal_index";
    /* For officially downloadable files */
    public static final String                            PROPERTY_IS_ACCOUNT_REQUIRED                    = "is_account_required";
    /**
     * For files which are [temporarily] not downloadable at all.
     */
    public static final String                            PROPERTY_IS_NOT_DOWNLOADABLE_TIMESTAMP          = "is_not_downloadable_timestamp";
    /* For book page downloads */
    public static final String                            PROPERTY_IS_LENDING_REQUIRED                    = "is_lending_required";
    public static final String                            PROPERTY_IS_FREE_DOWNLOADABLE_BOOK_PREVIEW_PAGE = "is_free_downloadable_book_preview_page";
    public static final String                            PROPERTY_IS_BORROWED_UNTIL_TIMESTAMP            = "is_borrowed_until_timestamp";
    public static final String                            PROPERTY_PLAYLIST_POSITION                      = "position";
    public static final String                            PROPERTY_PLAYLIST_SIZE                          = "playlist_size";
    public static final String                            PROPERTY_TITLE                                  = "title";
    public static final String                            PROPERTY_FILENAME                               = "filename";
    public static final String                            PROPERTY_ARTIST                                 = "artist";
    public static final String                            PROPERTY_GENRE                                  = "genre";
    public static final String                            PROPERTY_TIMESTAMP_FROM_API_LAST_MODIFIED       = "timestamp_from_api_last_modified";
    /*
     * Important: Only set this for audio/video playlists! If this is set && a playlist-position is given that will be added to the
     * beginning of the filenames!
     */
    public static final String                            PROPERTY_FILETYPE                               = "filetype";
    public static final String                            FILETYPE_AUDIO                                  = "audio";
    public static final String                            FILETYPE_VIDEO                                  = "video";
    private final String                                  PROPERTY_ACCOUNT_TIMESTAMP_BORROW_LIMIT_REACHED = "timestamp_borrow_limit_reached";
    private static HashMap<String, ArchiveOrgLendingInfo> bookBorrowSessions                              = new HashMap<String, ArchiveOrgLendingInfo>();
    private static final String                           ERRORMSG_FILE_NOT_DOWNLOADABLE                  = "The item is not available due to issues with the item's content.";
    private static final String                           ERRORMSG_ACCOUNT_REQUIRED_TO_DOWNLOAD_BOOK_PAGE = "Free account required to download this book page";

    @Override
    public AvailableStatus requestFileInformation(final DownloadLink link) throws Exception {
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        return requestFileInformation(link, account, false);
    }

    public AvailableStatus requestFileInformation(final DownloadLink link, final Account account, final boolean isDownload) throws Exception {
        String filename = link.getStringProperty(PROPERTY_FILENAME);
        if (filename != null) {
            /*
             * Set cached filename otherwise resetted links will get the Content-Disposition filename which is not our desired file name for
             * archive.org items.
             */
            setFinalFilename(link, filename);
        }
        if (getCachedFileNotDownloadableTime(link) > 0) {
            /* Such items can neither be checked nor downloaded. */
            return AvailableStatus.UNCHECKABLE;
        }
        this.setBrowserExclusive();
        if (account != null) {
            login(account, false);
        }
        if (!isDownload) {
            if (this.requiresAccount(link) && account == null) {
                return AvailableStatus.UNCHECKABLE;
            } else if (this.isBookLendingRequired(link)) {
                /* Do not lend books during availablecheck */
                return AvailableStatus.UNCHECKABLE;
            }
            URLConnectionAdapter con = null;
            try {
                /* 2021-02-25: Do not use HEAD request anymore! */
                prepDownloadHeaders(br, link);
                con = br.openGetConnection(getDirectURL(link, account));
                connectionErrorhandling(con, link, account, null);
                filename = link.getStringProperty(PROPERTY_FILENAME, null);
                if (filename == null) {
                    filename = getFileNameFromConnection(con);
                    if (filename != null) {
                        filename = Encoding.htmlDecode(filename);
                    }
                }
                if (filename != null) {
                    setFinalFilename(link, filename);
                }
                if (con.getCompleteContentLength() > 0) {
                    if (con.isContentDecoded()) {
                        link.setDownloadSize(con.getCompleteContentLength());
                    } else {
                        link.setVerifiedFileSize(con.getCompleteContentLength());
                    }
                }
                return AvailableStatus.TRUE;
            } finally {
                if (con != null) {
                    con.disconnect();
                }
            }
        }
        return AvailableStatus.UNCHECKABLE;
    }

    private boolean isAudioPlaylistItem(final DownloadLink link) {
        final String filetype = link.getStringProperty(PROPERTY_FILETYPE, null);
        if (StringUtils.equals(filetype, FILETYPE_AUDIO) && link.getRelativeDownloadFolderPath() == null) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Use this whenever you wnt to set a filename especially if there is a chance that the item is part of an audio playlist or a video
     * streaming item.
     */
    public static void setFinalFilename(final DownloadLink link, final String originalFilename) {
        if (StringUtils.isEmpty(originalFilename)) {
            return;
        }
        final int position = link.getIntegerProperty(PROPERTY_PLAYLIST_POSITION, -1);
        String fileExtension = Plugin.getFileNameExtensionFromString(originalFilename);
        if (fileExtension != null && fileExtension.startsWith(".")) {
            fileExtension = fileExtension.substring(1);
        }
        final ExtensionsFilterInterface fileTypeByExtension = fileExtension != null ? CompiledFiletypeFilter.getExtensionsFilterInterface(fileExtension) : null;
        boolean isAudio = fileTypeByExtension != null ? CompiledFiletypeFilter.AudioExtensions.MP3.isSameExtensionGroup(fileTypeByExtension) : false;
        boolean isVideo = fileTypeByExtension != null ? CompiledFiletypeFilter.VideoExtensions.MP4.isSameExtensionGroup(fileTypeByExtension) : false;
        final String filetype = link.getStringProperty(PROPERTY_FILETYPE, null);
        if (StringUtils.equals(filetype, FILETYPE_AUDIO)) {
            isAudio = true;
        } else if (StringUtils.equals(filetype, FILETYPE_VIDEO)) {
            isVideo = true;
        }
        if (position != -1 && (isAudio || isVideo) && link.getRelativeDownloadFolderPath() == null) {
            final int playlistSize = link.getIntegerProperty(PROPERTY_PLAYLIST_SIZE, -1);
            final String positionFormatted;
            if (playlistSize != -1) {
                final int padLength = StringUtils.getPadLength(playlistSize);
                positionFormatted = StringUtils.formatByPadLength(padLength, position);
            } else {
                positionFormatted = Integer.toString(position);
            }
            if (isAudio) {
                /* File is part of audio playlist. Format: <positionFormatted>.<rawFilename> */
                String title = link.getStringProperty(PROPERTY_TITLE);
                if (title == null) {
                    /* Title is not always given. Use filename without extension as title */
                    title = originalFilename.substring(0, originalFilename.lastIndexOf("."));
                }
                if (PluginJsonConfig.get(ArchiveOrgConfig.class).getPlaylistFilenameScheme() == PlaylistFilenameScheme.PLAYLIST_TITLE_WITH_TRACK_NUMBER && title != null) {
                    final String artist = link.getStringProperty(PROPERTY_ARTIST);
                    String playlistFilename = positionFormatted + "." + title;
                    if (artist != null) {
                        playlistFilename += " - " + artist;
                    }
                    playlistFilename += "." + fileExtension;
                    link.setFinalFileName(playlistFilename);
                } else {
                    link.setFinalFileName(originalFilename);
                }
            } else {
                /* Video streaming file. Format: <rawFilenameWithoutExt>_<positionFormatted>.mp4 */
                final String filenameWithoutExt = originalFilename.substring(0, originalFilename.lastIndexOf("."));
                link.setFinalFileName(filenameWithoutExt + "_" + positionFormatted + ".mp4");
            }
        } else {
            link.setFinalFileName(originalFilename);
        }
    }

    private boolean isFreeDownloadableBookPreviewPage(final DownloadLink link) {
        if (link.hasProperty(PROPERTY_IS_FREE_DOWNLOADABLE_BOOK_PREVIEW_PAGE)) {
            return true;
        } else {
            return false;
        }
    }

    private boolean isBook(final DownloadLink link) {
        if (link.hasProperty("is_book")) {
            /* Legacy */
            return true;
        } else if (link.hasProperty(PROPERTY_BOOK_ID)) {
            return true;
        } else {
            return false;
        }
    }

    /** Returns true if link leads to a compressed archive file. */
    private boolean isArchive(final DownloadLink link) {
        if (link.getPluginPatternMatcher().matches("(?i).+\\.(zip|rar|7z|gz|tar|lz)/.+")) {
            return true;
        } else {
            return false;
        }
    }

    private boolean isBookLendingRequired(final DownloadLink link) {
        if (link.hasProperty(PROPERTY_IS_LENDING_REQUIRED)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Returns true if this book page is borrowed at this moment. </br>
     * This information is only useful with the combination of the borrow-cookies and can become invalid at any point of time if e.g. the
     * user returns the book manually via browser.
     */
    private boolean isLendAtThisMoment(final DownloadLink link) {
        final long borrowedUntilTimestamp = link.getLongProperty(PROPERTY_IS_BORROWED_UNTIL_TIMESTAMP, -1);
        if (borrowedUntilTimestamp > System.currentTimeMillis()) {
            return true;
        } else {
            return false;
        }
    }

    private int getBookPageIndexNumber(final DownloadLink link) {
        final int internalBookPageIndex = link.getIntegerProperty(PROPERTY_BOOK_PAGE_INTERNAL_INDEX, -1);
        if (internalBookPageIndex != -1) {
            return internalBookPageIndex;
        }
        /* All of this is legacy. TODO: Remove in 01-2023 */
        final int archiveOrgBookPageNumber = link.getIntegerProperty(PROPERTY_BOOK_PAGE, -1);
        if (archiveOrgBookPageNumber != -1) {
            return archiveOrgBookPageNumber;
        } else {
            /* Legacy handling for older items */
            final String pageStr = new Regex(link.getContentUrl(), "(?i).*/page/n?(\\d+)").getMatch(0);
            if (pageStr != null) {
                return Integer.parseInt(pageStr) - 1;
            } else {
                /* Fallback: This should never happen */
                return 1;
            }
        }
    }

    /**
     * A special string that is the same as the bookID but different for multi volume books. </br>
     * ...thus only relevant for multi volume books.
     */
    private String getBookSubPrefix(final DownloadLink link) {
        return link.getStringProperty(PROPERTY_BOOK_SUB_PREFIX);
    }

    private boolean requiresAccount(final DownloadLink link) {
        if (this.isBookLendingRequired(link) && !this.isFreeDownloadableBookPreviewPage(link)) {
            return true;
        } else {
            return false;
        }
    }

    private String getBookID(final DownloadLink link) {
        return link.getStringProperty(PROPERTY_BOOK_ID);
    }

    private void connectionErrorhandling(final URLConnectionAdapter con, final DownloadLink link, final Account account, final ArchiveOrgLendingInfo oldLendingInfo) throws Exception {
        final boolean isBook = this.isBook(link);
        if (isBook) {
            /* Check for URL- and header based problems */
            if (StringUtils.containsIgnoreCase(con.getURL().toExternalForm(), "preview-unavailable.png")) {
                // https://archive.org/bookreader/static/preview-unavailable.png
                /* This page of a book is only available when book is borrowed by user. */
                if (account == null) {
                    throw new AccountRequiredException(ERRORMSG_ACCOUNT_REQUIRED_TO_DOWNLOAD_BOOK_PAGE);
                } else {
                    throw new PluginException(LinkStatus.ERROR_FATAL, "Book preview unavailable");
                }
            }
        }
        if (!this.looksLikeDownloadableContent(con, link)) {
            if (isBook) {
                final int responsecode = con.getResponseCode();
                if (account != null && isBookLendingRequired(link) && (responsecode == 403 || responsecode == 404)) {
                    synchronized (bookBorrowSessions) {
                        final ArchiveOrgLendingInfo lendingInfo = getLendingInfo(this.getBookID(link), account);
                        if (oldLendingInfo != lendingInfo) {
                            /* Info has been modified in the meanwhile --> Retry */
                            throw new PluginException(LinkStatus.ERROR_RETRY, "Retry after auto re-loan of other download candidate");
                        }
                        /* Protection against re-loaning the same book over and over again in a short period of time. */
                        final Long timeUntilNextLoanAllowed = lendingInfo != null ? lendingInfo.getTimeUntilNextLoanAllowed() : null;
                        if (timeUntilNextLoanAllowed != null) {
                            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Wait until this book can be auto re-loaned", timeUntilNextLoanAllowed.longValue());
                        } else {
                            this.borrowBook(br, account, this.getBookID(link), false);
                            throw new PluginException(LinkStatus.ERROR_RETRY, "Retry after auto re-loan of current download candidate");
                        }
                    }
                }
                /* Unknown reason of failure */
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error", 3 * 60 * 1000l);
            }
            br.followConnection(true);
            if (con.getResponseCode() == 401) {
                throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 401");
            } else if (con.getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            if (ArchiveOrg.isItemUnavailable(br)) {
                if (ArchiveOrg.isAccountRequired(br)) {
                    checkCachedFileNotDownloadable(link);
                    if (account != null) {
                        /* Error happened while we're logged in -> Dead end --> File isn't even downloadable via account at this moment. */
                        link.setProperty(PROPERTY_IS_NOT_DOWNLOADABLE_TIMESTAMP, System.currentTimeMillis());
                        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, ERRORMSG_FILE_NOT_DOWNLOADABLE, 5 * 60 * 1000l);
                    } else {
                        /* Make user try again with account */
                        throw new AccountRequiredException("Item is currently not downloadable or only for logged in users");
                    }
                } else {
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Server error 403: Item not available");
                }
            }
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (link.getHashInfo() != null) {
            /**
             * Check if we can use the current HashInfo. Delete it if we can't be sure that the file we are going to download is the same
             * file/version which was initially crawled. </br>
             * Getting the current/new file-hash would also be too much effort and in most of all cases the files won't have changed until
             * download is initiated but it is relly important to clear the hash if in doubt otherwise the user may get a "CRC check failed"
             * error message for no reason.
             */
            boolean deleteHashInfo = false;
            final long lastModifiedTimestampFromAPI = link.getLongProperty(PROPERTY_TIMESTAMP_FROM_API_LAST_MODIFIED, -1);
            if (lastModifiedTimestampFromAPI == -1) {
                /* This will happen for all items which were added until (including) crawler revision 48316. */
                logger.info("Deleting HashInfo because: No Last-Modified timestamp from API given");
                deleteHashInfo = true;
            } else {
                final Date lastModifiedDate = TimeFormatter.parseDateString(con.getHeaderField(HTTPConstants.HEADER_RESPONSE_LAST_MODFIED));
                if (lastModifiedDate == null) {
                    logger.info("Deleting HashInfo because: Last-Modified header is not present or broken");
                    deleteHashInfo = true;
                } else if (lastModifiedDate.getTime() != lastModifiedTimestampFromAPI * 1000) {
                    logger.info("Deleting HashInfo because: Timestamp from Last-Modified header differs from stored timestamp so most likely the file has changed and the hash we know can't be used to check against the new version of that file");
                    deleteHashInfo = true;
                }
            }
            if (deleteHashInfo) {
                link.setHashInfo(null);
            }
        }
    }

    /**
     * Throws exception if file is currently not downloadable according to cached value.
     *
     * @throws PluginException
     */
    private void checkCachedFileNotDownloadable(final DownloadLink link) throws PluginException {
        final long fileNotDownloadableTime = getCachedFileNotDownloadableTime(link);
        if (fileNotDownloadableTime > 0) {
            throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, ERRORMSG_FILE_NOT_DOWNLOADABLE, fileNotDownloadableTime);
        }
    }

    /**
     * Returns time in milliseconds that indicates how long that link is not downloadable (= not even downloadable when user is logged in).
     */
    private long getCachedFileNotDownloadableTime(final DownloadLink link) {
        final int waitMillis = 3 * 60 * 60 * 1000;
        return (link.getLongProperty(PROPERTY_IS_NOT_DOWNLOADABLE_TIMESTAMP, 0) + waitMillis) - System.currentTimeMillis();
    }

    public static boolean isItemUnavailable(final Browser br) {
        if (br.containsHTML(">\\s*Item not available")) {
            return true;
        } else {
            return false;
        }
    }

    public static boolean isAccountRequired(final Browser br) {
        if (br.containsHTML(">\\s*You must log in to view this content|>\\s*The item is not available due to issues")) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception, PluginException {
        handleDownload(null, link);
    }

    private static Object DEOBFUSCATE_LOCK = new Object();

    /**
     * https://github.com/justimm/Archive.org-Downloader/blob/main/archive-org-downloader.py
     */
    private void deObfuscate(final File file, final String url, final String XObfuscateHeader, final String XencrypteddataHeader) throws Exception {
        synchronized (DEOBFUSCATE_LOCK) {
            if (XObfuscateHeader == null) {
                logger.info("Book page is not obfuscated -> Doing nothing");
                return;
            }
            final String XObfuscate[] = XObfuscateHeader.split("\\|");
            if (!"1".equals(XObfuscate[0])) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported X-Obfuscate:" + XObfuscate[0]);
            }
            final byte[] counter_b64 = Base64.decode(XObfuscate[1]);
            final String aesKey = url.replaceFirst("https://.*?/", "/");
            final byte[] key = Arrays.copyOf(HexFormatter.hexToByteArray(Hash.getSHA1(aesKey)), 16);
            final byte[] prefix = Arrays.copyOf(counter_b64, 8);
            final long initial_value = ByteBuffer.wrap(counter_b64, 8, 8).order(ByteOrder.BIG_ENDIAN).getLong();
            final byte[] iv = new byte[16];
            System.arraycopy(prefix, 0, iv, 0, 8);
            for (int i = 0; i < 8; i++) {
                iv[15 - i] = (byte) (initial_value >>> (8 * i));
            }
            final Cipher cipher = javax.crypto.Cipher.getInstance("AES/CTR/NoPadding");
            final IvParameterSpec ivSpec = new IvParameterSpec(iv);
            final SecretKeySpec keySpec = new SecretKeySpec(key, "AES");
            cipher.init(Cipher.DECRYPT_MODE, keySpec, ivSpec);
            final int encryptedDataLength = XencrypteddataHeader != null ? Integer.parseInt(XencrypteddataHeader) : 1024;
            byte[] decrypted = null;
            if (true) {
                // verify that de-obfuscated image can be loaded
                final byte[] data = IO.readBytes(file);
                try {
                    decrypted = cipher.doFinal(Arrays.copyOf(data, encryptedDataLength));
                    System.arraycopy(decrypted, 0, data, 0, encryptedDataLength);
                    final BufferedImage image = ImageIO.read(new ByteArrayInputStream(data));
                    if (image == null) {
                        throw new IOException("No image?!");
                    } else {
                        image.flush();
                    }
                } catch (Exception e) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, null, e);
                }
            }
            final RandomAccessFile raf = new RandomAccessFile(file, "rw");
            try {
                raf.seek(0);
                if (decrypted == null) {
                    byte[] encrypted = new byte[encryptedDataLength];
                    raf.readFully(encrypted);
                    decrypted = cipher.doFinal(encrypted);
                    raf.seek(0);
                }
                raf.write(decrypted);
            } finally {
                raf.close();
            }
        }
    }

    private void handleDownload(final Account account, final DownloadLink link) throws Exception, PluginException {
        requestFileInformation(link, account, true);
        checkCachedFileNotDownloadable(link);
        ArchiveOrgLendingInfo lendingInfoForBeforeDownload = null;
        final String bookID = this.getBookID(link);
        if (account != null) {
            this.login(account, false);
            lendingInfoForBeforeDownload = this.getLendingInfo(link, account);
        } else {
            if (this.requiresAccount(link)) {
                throw new AccountRequiredException(ERRORMSG_ACCOUNT_REQUIRED_TO_DOWNLOAD_BOOK_PAGE);
            }
        }
        cleanupBorrowSessionMap();
        prepDownloadHeaders(br, link);
        final String directurl = getDirectURL(link, account);
        dl = jd.plugins.BrowserAdapter.openDownload(br, link, directurl, isResumeable(link, account), getMaxChunks(link, account));
        try {
            connectionErrorhandling(br.getHttpConnection(), link, account, lendingInfoForBeforeDownload);
        } catch (final Exception e) {
            br.getHttpConnection().disconnect();
            throw e;
        }
        if (dl.startDownload()) {
            /* Return books once all pages of a book have been downloaded. */
            if (isBook(link)) {
                /* Deobfuscate downloaded book page if necessary */
                try {
                    deObfuscate(new File(link.getFileOutput()), directurl, dl.getConnection().getHeaderField("X-Obfuscate"), dl.getConnection().getHeaderField("X-encrypted-data"));
                } finally {
                    gracefullyEndBorrowSession: try {
                        synchronized (bookBorrowSessions) {
                            /* lendingInfo could have changed in the meantime */
                            final ArchiveOrgLendingInfo lendingInfoForAfterDownload = this.getLendingInfo(link, account);
                            if (lendingInfoForAfterDownload == null) {
                                break gracefullyEndBorrowSession;
                            }
                            lendingInfoForAfterDownload.setBookPageDownloadStatus(this.getBookPageIndexNumber(link), true);
                            if (!lendingInfoForAfterDownload.looksLikeBookDownloadIsComplete()) {
                                logger.info("Book page download progress: " + lendingInfoForAfterDownload.getNumberOfDownloadedPages() + "/" + lendingInfoForAfterDownload.getTotalNumberofPages());
                                break gracefullyEndBorrowSession;
                            }
                            try {
                                logger.info("All book pages have been downloaded");
                                if (!lendingInfoForAfterDownload.isLoanSessionActive()) {
                                    logger.info("Loan session is not active -> No need to return book");
                                    break gracefullyEndBorrowSession;
                                }
                                logger.info("Loan session looks to be active -> Returning book");
                                final UrlQuery query = new UrlQuery();
                                query.appendEncoded("action", "return_loan");
                                query.appendEncoded("identifier", bookID);
                                br.postPage("https://" + this.getHost() + "/services/loans/loan", query);
                                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                                if (Boolean.TRUE.equals(entries.get("success"))) {
                                    logger.info("Successfully returned book " + bookID);
                                } else {
                                    logger.info("Failed to return book " + bookID + " json response: " + br.getRequest().getHtmlCode());
                                }
                            } catch (final Throwable wtf) {
                                logger.log(wtf);
                                logger.warning("Failed to return book: Exception happened");
                            } finally {
                                /* Remove from cache */
                                bookBorrowSessions.remove(getLendingInfoKey(bookID, account));
                            }
                        }
                    } catch (final Exception ignore) {
                        logger.log(ignore);
                    }
                }
            }
        }
    }

    /** Removes expired entries from bookBorrowSessions. */
    private void cleanupBorrowSessionMap() {
        synchronized (bookBorrowSessions) {
            final Iterator<Entry<String, ArchiveOrgLendingInfo>> iterator = bookBorrowSessions.entrySet().iterator();
            while (iterator.hasNext()) {
                final Entry<String, ArchiveOrgLendingInfo> entry = iterator.next();
                final ArchiveOrgLendingInfo lendingInfo = entry.getValue();
                if (!lendingInfo.isValid() || lendingInfo.looksLikeBookDownloadIsComplete()) {
                    iterator.remove();
                }
            }
        }
    }

    private String getDirectURL(final DownloadLink link, final Account account) throws Exception {
        if (this.isBook(link)) {
            final boolean isFreeDownloadablePage = this.isFreeDownloadableBookPreviewPage(link);
            if (!isFreeDownloadablePage && account == null) {
                throw new AccountRequiredException(ERRORMSG_ACCOUNT_REQUIRED_TO_DOWNLOAD_BOOK_PAGE);
            }
            String directurl = null;
            if (account == null) {
                /* Directurl can be downloaded without the need of an account and special cookies. */
                directurl = link.getPluginPatternMatcher();
            } else {
                /* Account + cookies required and the book has to be borrowed to download this page. */
                synchronized (bookBorrowSessions) {
                    final String bookID = this.getBookID(link);
                    ArchiveOrgLendingInfo lendingInfo = this.getLendingInfo(bookID, account);
                    if (lendingInfo != null) {
                        directurl = lendingInfo.getPageURL(this.getBookPageIndexNumber(link));
                    }
                    if (isFreeDownloadablePage) {
                        directurl = link.getPluginPatternMatcher();
                        if (lendingInfo == null) {
                            logger.info("No borrow needed -> Creating ArchiveOrgLendingInfo anyways to track downloaded pages");
                            /**
                             * Book does not need to be borrowed -> Add "dummy" ArchiveOrgLendingInfo in order to keep track of downloaded
                             * pages. <br>
                             * This is important since some pages may be downloadable without the need to borrow the book so it may get
                             * borrowed e.g. after downloading a few pages without having borrowed it.
                             */
                            final ArchiveOrgLendingInfo newLendingInfo = new ArchiveOrgLendingInfo(null);
                            final int bookPageIndex = this.getBookPageIndexNumber(link);
                            final int maxPage = link.getIntegerProperty(ArchiveOrg.PROPERTY_BOOK_PAGE_MAX, 1);
                            final List<String> urls = new ArrayList<String>();
                            for (int i = 0; i < maxPage; i++) {
                                if (i == bookPageIndex) {
                                    /* Add the book page we are processing at this moment */
                                    urls.add(link.getPluginPatternMatcher());
                                } else {
                                    /* Add dummy element */
                                    urls.add(null);
                                }
                            }
                            newLendingInfo.setPageURLs(urls);
                            bookBorrowSessions.put(getLendingInfoKey(bookID, account), newLendingInfo);
                        }
                    } else if (lendingInfo != null && directurl != null) {
                        /* Use existing session */
                        logger.info("Using existing borrow session");
                        br.setCookies(lendingInfo.getCookies());
                    } else {
                        /* Create new book lending session */
                        this.borrowBook(br, account, bookID, false);
                        lendingInfo = this.getLendingInfo(bookID, account);
                        directurl = lendingInfo.getPageURL(this.getBookPageIndexNumber(link));
                        if (StringUtils.isEmpty(directurl)) {
                            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                        }
                    }
                }
            }
            final ArchiveOrgConfig cfg = PluginJsonConfig.get(ArchiveOrgConfig.class);
            final UrlQuery query = UrlQuery.parse(directurl);
            query.appendEncoded("rotate", "0");
            /* This one defines the image quality. This may only work for borrowed books but we'll append it to all book URLs regardless. */
            query.appendEncoded("scale", Integer.toString(cfg.getBookImageQuality()));
            /* Get url without parameters */
            String newURL = URLHelper.getUrlWithoutParams(directurl);
            /* Append our new query */
            newURL += "?" + query.toString();
            return newURL;
        } else {
            /* Normal file download */
            return link.getPluginPatternMatcher();
        }
    }

    private void prepDownloadHeaders(final Browser br, final DownloadLink link) {
        if (this.isBook(link)) {
            br.getHeaders().put("Referer", "https://" + getHost() + "/");
            br.getHeaders().put("Accept", "image/avif,image/webp,image/apng,image/*,*/*;q=0.8");
            br.getHeaders().put("Sec-Fetch-Site", "name-site");
            br.getHeaders().put("Sec-Fetch-Mode", "no-cors");
            br.getHeaders().put("Sec-Fetch-Dest", "image");
        }
    }

    @Override
    public boolean isResumeable(final DownloadLink link, final Account account) {
        if (this.isBook(link)) {
            return false;
        } else if (this.isArchive(link)) {
            return false;
        } else {
            return true;
        }
    }

    public int getMaxChunks(final DownloadLink link, final Account account) {
        if (this.isBook(link)) {
            return 1;
        } else if (this.isArchive(link)) {
            return 1;
        } else {
            return 0;
        }
    }

    @Override
    public int getMaxSimultanFreeDownloadNum() {
        return Integer.MAX_VALUE;
    }

    public void login(final Account account, final boolean force) throws Exception {
        synchronized (account) {
            br.setCookiesExclusive(true);
            /* 2021-08-09: Added this as alternative method e.g. for users that have registered on archive.org via Google login. */
            final Cookies userCookies = account.loadUserCookies();
            if (userCookies != null) {
                /* Use cookies supplied by user */
                if (!force) {
                    /* Do not check cookies */
                    br.setCookies(userCookies);
                    return;
                }
                if (!this.checkLogin(this.br, account, userCookies)) {
                    if (account.hasEverBeenValid()) {
                        throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_expired());
                    } else {
                        throw new AccountInvalidException(_GUI.T.accountdialog_check_cookies_invalid());
                    }
                }
                /* Success */
                return;
            }
            final Cookies cookies = account.loadCookies("");
            if (cookies != null) {
                logger.info("Attempting cookie login");
                br.setCookies(cookies);
                if (!force) {
                    /* Do not check cookies */
                    return;
                } else {
                    if (this.checkLogin(this.br, account, cookies)) {
                        logger.info("Cookie login successful");
                        account.saveCookies(br.getCookies(br.getHost()), "");
                        return;
                    } else {
                        /* Delete invalid cookies */
                        logger.info("Cookie login failed");
                        account.clearCookies("");
                        br.clearCookies(null);
                    }
                }
            }
            logger.info("Performing full login");
            br.getPage("https://" + this.getHost() + "/account/login");
            br.postPageRaw(br.getURL(), "remember=true&referer=https%3A%2F%2Farchive.org%2FCREATE%2F&login=true&submit_by_js=true&username=" + Encoding.urlEncode(account.getUser()) + "&password=" + Encoding.urlEncode(account.getPass()));
            final String logincookie = br.getCookie(br.getHost(), "logged-in-sig", Cookies.NOTDELETEDPATTERN);
            if (logincookie == null) {
                throw new AccountInvalidException();
            }
            /* Double-check if login has worked */
            this.checkLogin(br, account, null);
            account.saveCookies(br.getCookies(br.getHost()), "");
        }
    }

    /**
     * Borrows given bookID which gives us a token we can use to download all pages of that book. </br>
     * It is typically valid for one hour.
     */
    private void borrowBook(final Browser br, final Account account, final String bookID, final boolean skipAllExceptLastStep) throws Exception {
        if (account == null) {
            /* Account is required to borrow books. */
            throw new AccountRequiredException("Account required to allow for downloading books that need to be borrowed");
        } else if (bookID == null) {
            /* Developer mistake */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* Check for reached limit so we don't waste more http requests when we know that this limit has been reached. */
        final long timestampBorrowLimitReached = account.getLongProperty(PROPERTY_ACCOUNT_TIMESTAMP_BORROW_LIMIT_REACHED, 0);
        final long timePassedSinceLimitReached = Time.systemIndependentCurrentJVMTimeMillis() - timestampBorrowLimitReached;
        if (timePassedSinceLimitReached < 1 * 60 * 60 * 1000l) {
            this.exceptionReachedAccountBorrowLimit();
        }
        synchronized (bookBorrowSessions) {
            final UrlQuery query = new UrlQuery();
            query.add("identifier", Encoding.urlEncode(bookID));
            Map<String, Object> entries = null;
            final String urlBase = "https://" + this.getHost();
            br.setAllowedResponseCodes(400);
            if (!skipAllExceptLastStep) {
                query.add("action", "grant_access");
                br.postPage(urlBase + "/services/loans/loan/searchInside.php", query);
                entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                query.addAndReplace("action", "browse_book");
                br.postPage("/services/loans/loan/", query);
                entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final String error = (String) entries.get("error");
                if (error != null) {
                    if (StringUtils.equalsIgnoreCase(error, "This book is not available to borrow at this time. Please try again later.")) {
                        /**
                         * Happens if you try to borrow a book that can't be borrowed or if you try to borrow a book while too many
                         * (2022-08-31: max 10) books per hour have already been borrowed with the current account. </br>
                         * With setting this timestamp we can ensure not to waste more http requests on trying to borrow books but simply
                         * set error status on all future links [for the next 60 minutes].
                         */
                        account.setProperty(PROPERTY_ACCOUNT_TIMESTAMP_BORROW_LIMIT_REACHED, Time.systemIndependentCurrentJVMTimeMillis());
                        /*
                         * Remove session of book associated with current book page [if present] so that there will be no further download
                         * attempts and all pages will run into this error directly.
                         */
                        bookBorrowSessions.remove(getLendingInfoKey(bookID, account));
                        exceptionReachedAccountBorrowLimit();
                        /* This code should never be reached */
                        throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                    } else {
                        // throw new PluginException(LinkStatus.ERROR_FATAL, "Book borrow failure: " + error);
                        throw new PluginException(LinkStatus.ERROR_FATAL, "Book borrow failure: " + error);
                    }
                }
            }
            /* This should set a cookie called "br-load-<bookID>" */
            query.addAndReplace("action", "create_token");
            br.postPage(urlBase + "/services/loans/loan/", query);
            entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final String borrowToken = (String) entries.get("token");
            if (StringUtils.isEmpty(borrowToken)) {
                throw new PluginException(LinkStatus.ERROR_FATAL, "Book borrow failure #2");
            }
            if (skipAllExceptLastStep) {
                logger.info("Successfully checked borrow status of book: " + bookID);
            } else {
                logger.info("Successfully borrowed book: " + bookID);
            }
            // account.saveCookies(br.getCookies(br.getHost()), "");
            final Cookies borrowCookies = new Cookies();
            for (final Cookie cookie : br.getCookies(br.getHost()).getCookies()) {
                /* Collect borrow cookies and save them separately */
                if (cookie.getKey().matches("(br|loan)-.*")) {
                    borrowCookies.add(cookie);
                }
            }
            if (borrowCookies.isEmpty()) {
                /* This should never happen */
                logger.warning("WTF book was borrowed but no borrow-cookies are present!");
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final ArchiveOrgCrawler crawler = (ArchiveOrgCrawler) this.getNewPluginForDecryptInstance(this.getHost());
            final String bookURL = ArchiveOrgCrawler.generateBookContentURL(bookID);
            br.getPage(bookURL);
            if (br.getHttpConnection().getResponseCode() == 404) {
                throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
            }
            final List<String> pageURLs = new ArrayList<String>();
            final ArrayList<DownloadLink> results = crawler.crawlBookWebsite(br, new CryptedLink(bookURL), account);
            for (final DownloadLink result : results) {
                if (!this.isLendAtThisMoment(result)) {
                    /* This should never happen */
                    throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Book lending failure: Loaned book is still not viewable");
                }
                pageURLs.add(result.getPluginPatternMatcher());
            }
            /* Keep track of download progress even if book needs to be lend again in the middle of downloading! */
            final ArchiveOrgLendingInfo existingLendingInfo = this.getLendingInfo(bookID, account);
            if (existingLendingInfo != null) {
                logger.info("Updated existing ArchiveOrgLendingInfo");
                /* Set new borrow-cookies */
                existingLendingInfo.setCookies(borrowCookies);
                /* Update page URLKs although they should not have changed. */
                existingLendingInfo.updateOrAddBookPages(pageURLs);
            } else {
                logger.info("Added new ArchiveOrgLendingInfo");
                final ArchiveOrgLendingInfo newLendingInfo = new ArchiveOrgLendingInfo(borrowCookies);
                newLendingInfo.setPageURLs(pageURLs);
                bookBorrowSessions.put(getLendingInfoKey(bookID, account), newLendingInfo);
            }
        }
    }

    private void exceptionReachedAccountBorrowLimit() throws PluginException {
        throw new PluginException(LinkStatus.ERROR_TEMPORARILY_UNAVAILABLE, "Reached max borrow limit with this account: Refresh account and stop/start downloads to retry immediately", 5 * 60 * 1000l);
    }

    public ArchiveOrgLendingInfo getLendingInfo(final DownloadLink link, final Account account) {
        return getLendingInfo(this.getBookID(link), account);
    }

    /** Returns LendingInfo/session for given bookID + acccount. */
    public ArchiveOrgLendingInfo getLendingInfo(final String bookID, final Account account) {
        if (account == null || bookID == null) {
            return null;
        }
        final String key = getLendingInfoKey(bookID, account);
        synchronized (bookBorrowSessions) {
            final ArchiveOrgLendingInfo ret = bookBorrowSessions.get(key);
            return ret;
        }
    }

    private static String getLendingInfoKey(final String bookID, final Account account) {
        return account.getUser() + "_" + bookID;
    }

    /** Checks if given cookies grant us access to given account. */
    private boolean checkLogin(final Browser br, final Account account, final Cookies cookies) throws IOException {
        if (cookies != null) {
            br.setCookies(account.getHoster(), cookies);
        }
        br.getPage("https://" + this.getHost() + "/services/user.php?op=whoami");
        /*
         * E.g. success response:
         * {"success":true,"value":{"username":"bbla@bla.tld","itemname":"@username","screenname":"username","privs":[],"image_info":{
         * "mtime":"","size":"","format":""}}}
         */
        final Map<String, Object> entries = JSonStorage.restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        if (Boolean.TRUE.equals(entries.get("success"))) {
            logger.info("Cookie login successful");
            return true;
        } else {
            logger.info("Cookie login failed");
            return false;
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        final AccountInfo ai = new AccountInfo();
        login(account, true);
        ai.setUnlimitedTraffic();
        /* This host does not provide any kind of paid accounts. */
        account.setType(AccountType.FREE);
        cleanupBorrowSessionMap();
        account.removeProperty(PROPERTY_ACCOUNT_TIMESTAMP_BORROW_LIMIT_REACHED);
        return ai;
    }

    @Override
    public void handlePremium(final DownloadLink link, final Account account) throws Exception {
        handleDownload(account, link);
    }

    @Override
    public boolean looksLikeDownloadableContent(final URLConnectionAdapter con) {
        return looksLikeDownloadableContent(con, false);
    }

    public boolean looksLikeDownloadableContent(final URLConnectionAdapter con, final DownloadLink link) {
        if (this.isBook(link)) {
            return looksLikeDownloadableContent(con, false);
        } else {
            return looksLikeDownloadableContent(con, true);
        }
    }

    public boolean looksLikeDownloadableContent(final URLConnectionAdapter con, final boolean isOfficialDownloadurl) {
        if (super.looksLikeDownloadableContent(con)) {
            return true;
        }
        if (con.getResponseCode() == 200 || con.getResponseCode() == 206) {
            if (isOfficialDownloadurl) {
                /**
                 * It's an official downloadurl but they're not necessarily sending a Content-Disposition header so checks down below could
                 * e.g. fail for .html files. </br>
                 */
                return true;
            } else if (StringUtils.containsIgnoreCase(con.getURL().getPath(), ".xml")) {
                /* 2021-02-15: Special handling for .xml files */
                return StringUtils.containsIgnoreCase(con.getContentType(), "xml");
            } else if (con.getURL().getPath().matches("(?i).*\\.(txt|log)$")) {
                /* 2021-05-03: Special handling for .txt files */
                return StringUtils.containsIgnoreCase(con.getContentType(), "text/plain");
            } else if (StringUtils.containsIgnoreCase(con.getURL().getPath(), ".html")) {
                /* 2023-02-13: Special handling for .html files */
                return StringUtils.containsIgnoreCase(con.getContentType(), "html") || StringUtils.containsIgnoreCase(con.getContentType(), "text/plain");
            } else {
                /* MimeType file-extension and extension at the end of the URL are the same -> Also accept as downloadable content. */
                final String extension = getExtensionFromMimeType(con);
                if (StringUtils.endsWithCaseInsensitive(con.getURL().getPath(), "." + extension)) {
                    return true;
                } else {
                    return false;
                }
            }
        }
        return false;
    }

    @Override
    public int getMaxSimultanPremiumDownloadNum() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void reset() {
    }

    @Override
    public boolean canHandle(final DownloadLink link, final Account account) throws Exception {
        if (account == null && link.getBooleanProperty(PROPERTY_IS_ACCOUNT_REQUIRED, false) == true) {
            return false;
        } else {
            return super.canHandle(link, account);
        }
    }

    @Override
    public void resetDownloadlink(final DownloadLink link) {
        if (link == null) {
            return;
        }
        /*
         * Remove this property otherwise there is the possibility that the user gets a permanent error for certain files while they might
         * just be temporarily unavailable (this should never happen...)!
         */
        link.removeProperty(PROPERTY_IS_NOT_DOWNLOADABLE_TIMESTAMP);
        /* If this is a book page: Reset downloaded-flag for book page to prevent returning the book too early. */
        final ArrayList<Account> accounts = AccountController.getInstance().getValidAccounts(this.getHost());
        if (accounts != null && accounts.size() > 0) {
            synchronized (bookBorrowSessions) {
                for (final Account account : accounts) {
                    final ArchiveOrgLendingInfo lendingInfo = this.getLendingInfo(link, account);
                    if (lendingInfo != null) {
                        lendingInfo.setBookPageDownloadStatus(this.getBookPageIndexNumber(link), false);
                    }
                }
            }
        }
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return ArchiveOrgConfig.class;
    }
}