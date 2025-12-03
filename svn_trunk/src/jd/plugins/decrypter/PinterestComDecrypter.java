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
package jd.plugins.decrypter;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.scripting.JavaScriptEngineFactory;

import jd.PluginWrapper;
import jd.controlling.AccountController;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.Request;
import jd.http.requests.GetRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginDependencies;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.PluginForHost;
import jd.plugins.hoster.PinterestCom;

@DecrypterPlugin(revision = "$Revision: 51919 $", interfaceVersion = 3, names = {}, urls = {})
@PluginDependencies(dependencies = { PinterestCom.class })
public class PinterestComDecrypter extends PluginForDecrypt {
    public PinterestComDecrypter(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.IMAGE_GALLERY, LazyPlugin.FEATURE.IMAGE_HOST, LazyPlugin.FEATURE.BUBBLE_NOTIFICATION };
    }

    public static List<String[]> getPluginDomains() {
        return PinterestCom.getPluginDomains();
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:(?:www|[a-z]{2})\\.)?" + buildHostsPatternPart(domains) + "/.+");
        }
        return ret.toArray(new String[0]);
    }

    private boolean              enable_crawl_alternative_URL = false;
    public static final Pattern  PATTERN_PIN                  = Pattern.compile("https?://[^/]+/pin/([A-Za-z0-9\\-_]+)/?", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_USER                 = Pattern.compile("https?://[^/]+/([^/]+)/?$", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_BOARD                = Pattern.compile("https?://[^/]+/([^/]+)/([^/]+)/?", Pattern.CASE_INSENSITIVE);
    private static final Pattern PATTERN_BOARD_SECTION        = Pattern.compile("https?://[^/]+/([^/]+)/([^/]+)/([^/]+)/?", Pattern.CASE_INSENSITIVE);

    @SuppressWarnings({ "deprecation" })
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        currentUsername = null;
        currentBoardSlug = null;
        currentBoardPath = null;
        final PluginForHost hostPlugin = this.getNewPluginForHostInstance(this.getHost());
        enable_crawl_alternative_URL = hostPlugin.getPluginConfig().getBooleanProperty(PinterestCom.ENABLE_CRAWL_ALTERNATIVE_SOURCE_URLS, PinterestCom.defaultENABLE_CRAWL_ALTERNATIVE_SOURCE_URLS);
        final String url = param.getCryptedUrl();
        final Regex singlepinregex = (new Regex(url, PATTERN_PIN));
        if (singlepinregex.patternFind()) {
            return crawlSinglePIN(singlepinregex.getMatch(0));
        } else if (new Regex(url, PATTERN_BOARD_SECTION).patternFind()) {
            return this.crawlSection(param);
        } else {
            return crawlAllOtherItems(param.getCryptedUrl());
        }
    }

    private String currentUsername  = null;
    private String currentBoardSlug = null;
    private String currentBoardPath = null;

    /**
     * One function which can handle _any_ type of supported pinterest link (except for single PIN links). </br>
     * WORK IN PROGRESS
     */
    private ArrayList<DownloadLink> crawlAllOtherItems(final String contenturl) throws Exception {
        /* Login whenever possible to be able to crawl private pinterest boards. */
        final Account account = AccountController.getInstance().getValidAccount(this.getHost());
        if (account != null) {
            final PinterestCom hostPlugin = (PinterestCom) this.getNewPluginForHostInstance(this.getHost());
            hostPlugin.login(account, false);
        }
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String redirect = br.getRegex("window\\.location = \"([^\"]+)").getMatch(0);
        if (StringUtils.containsIgnoreCase(redirect, "show_error=true")) {
            /* Item offline or private */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("\"httpStatus\":404")) {
            /* 2025-07-03 e.g. ro.pinterest.com/%26hl%3Den%26sl%3Dro%26tl%3Den%26client%3Dsearch */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String[] jsons = br.getRegex("<script[^>]*type=\"application/json\">(.*?)</script>").getColumn(0);
        Map<String, Object> initialReduxState = null;
        Map<String, Object> resources = null;
        Map<String, Object> resourcesBoardResource = null;
        Map<String, Object> boardsections = null;
        for (final String json : jsons) {
            final Object rootO = restoreFromString(json, TypeRef.OBJECT);
            if (!(rootO instanceof Map)) {
                /* Skip invalid items */
                continue;
            }
            final Map<String, Object> root = (Map<String, Object>) rootO;
            final Map<String, Object> resource = (Map<String, Object>) root.get("resource");
            initialReduxState = (Map<String, Object>) JavaScriptEngineFactory.walkJson(root, "initialReduxState");
            if (initialReduxState != null) {
                /* Logged out state. When user is logged in, initialReduxState is also available but contains mostly empty maps. */
                boardsections = (Map<String, Object>) initialReduxState.get("boardsections");
                resources = (Map<String, Object>) initialReduxState.get("resources");
                resourcesBoardResource = (Map<String, Object>) JavaScriptEngineFactory.walkJson(resources, "BoardResource/{0}/data");
            } else if (resource != null) {
                /* Logged in state */
                final String resource_name = resource.get("name").toString();
                if (resource_name.equalsIgnoreCase("BoardResource")) {
                    resourcesBoardResource = (Map<String, Object>) JavaScriptEngineFactory.walkJson(root, "resource_response/data");
                    logger.info("Found json in logged in state");
                    if (account == null) {
                        logger.warning("Found json in logged in state WHILE NOT LOGGED IN");
                    }
                    break;
                }
            }
        }
        // final Map<String, Object> resourcesUserResource = (Map<String, Object>) resources.get("UserResource");
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        int expectedNumberofItems = 0;
        if (resourcesBoardResource != null) {
            /* This is a board -> Crawl all PINs from this board */
            currentBoardSlug = "";
            currentBoardPath = resourcesBoardResource.get("url").toString();
            final Map<String, Object> boardOwner = (Map<String, Object>) resourcesBoardResource.get("owner");
            final String username = boardOwner.get("username").toString();
            final String boardName = resourcesBoardResource.get("name").toString();
            final String boardID = resourcesBoardResource.get("id").toString();
            final int boardTotalPinCount = ((Number) resourcesBoardResource.get("pin_count")).intValue();
            final int boardSectionCount = ((Number) resourcesBoardResource.get("section_count")).intValue();
            final HashSet<String> boardSectionsPIN_IDs = new HashSet<String>();
            crawlBoardSections: if (boardSectionCount > 0) {
                logger.info("Crawling all sections of board" + boardName + " | " + boardSectionCount);
                crawlPreviewBoardSections: if (boardsections != null && boardsections.size() == boardSectionCount) {
                    /*
                     * Crawl "preview pins" -> For items that do not have a lot of sections, this may enable us to crawl all PINs from all
                     * sections without the need of any additional http requests
                     */
                    logger.info("Trying to crawl sections from preview data");
                    final HashSet<String> temp_boardSectionsPIN_IDs = new HashSet<String>();
                    final ArrayList<DownloadLink> temp_boardSectionsPIN_results = new ArrayList<DownloadLink>();
                    final Iterator<Entry<String, Object>> iterator = boardsections.entrySet().iterator();
                    while (iterator.hasNext()) {
                        final Entry<String, Object> entry = iterator.next();
                        final Map<String, Object> section = (Map<String, Object>) entry.getValue();
                        final String sectionName = section.get("title").toString();
                        final int this_section_pin_count = ((Number) section.get("pin_count")).intValue();
                        final List<Map<String, Object>> preview_pins = (List<Map<String, Object>>) section.get("preview_pins");
                        if (preview_pins.size() != this_section_pin_count) {
                            logger.info("Failed to find all section PINs via PIN preview map due to section: " + sectionName + " | pin_count=" + this_section_pin_count + " | preview_pins=" + preview_pins.size());
                            break crawlPreviewBoardSections;
                        }
                        final FilePackage fp = this.getSectionFilePackage(username, boardName, boardName, boardID, boardID);
                        for (final Map<String, Object> pin : preview_pins) {
                            final List<DownloadLink> results = this.proccessMap(pin, boardID, fp, false);
                            for (final DownloadLink result : results) {
                                final String pinStr = new Regex(result.getPluginPatternMatcher(), "(?i)/pin/(\\d+)").getMatch(0);
                                if (pinStr != null) {
                                    boardSectionsPIN_IDs.add(pinStr);
                                }
                                temp_boardSectionsPIN_results.add(result);
                            }
                        }
                    }
                    logger.info("Successfully crawled all section PINs from preview data");
                    boardSectionsPIN_IDs.addAll(temp_boardSectionsPIN_IDs);
                    ret.addAll(temp_boardSectionsPIN_results);
                    break crawlBoardSections;
                }
                /* 2025-03-18: The other way doesn't work anymore */
                this.displayBubbleNotification("Board " + boardName + " | sections", "Crawling all " + boardSectionCount + " sections of board " + boardName);
                final boolean useAsyncHandling = false;
                if (useAsyncHandling) {
                    /* This will return a DownloadLink object for each section so that it can be crawled separately. */
                    expectedNumberofItems += boardSectionCount;
                    final Map<String, Object> postDataOptions = new HashMap<String, Object>();
                    postDataOptions.put("board_id", boardID);
                    final Map<String, Object> postData = new HashMap<String, Object>();
                    postData.put("options", postDataOptions);
                    postData.put("context", new HashMap<String, Object>());
                    final Map<String, Object> resourcesBoardSectionsResource = (Map<String, Object>) JavaScriptEngineFactory.walkJson(resources, "BoardSectionsResource/{0}");
                    ret.addAll(this.crawlPaginationGeneric("BoardSectionsResource", resourcesBoardSectionsResource, postData, boardSectionCount, null, true));
                } else {
                    /*
                     * The hard way: First crawl all PINs in sections so that we can ignore those when crawling all sectionless board PINs
                     */
                    final List<DownloadLink> sectionPINsResult = this.crawlSections(username, boardID, boardName, br.cloneBrowser(), contenturl);
                    for (final DownloadLink result : sectionPINsResult) {
                        final String pinStr = new Regex(result.getPluginPatternMatcher(), "(?i)/pin/(\\d+)").getMatch(0);
                        if (pinStr != null) {
                            boardSectionsPIN_IDs.add(pinStr);
                        }
                        ret.add(result);
                    }
                    logger.info("Total found PINs inside sections: " + boardSectionsPIN_IDs.size() + "/" + boardTotalPinCount);
                }
            }
            final int sectionlessBoardPinCount = boardTotalPinCount - boardSectionsPIN_IDs.size();
            if (sectionlessBoardPinCount > 0) {
                /* Crawl all loose/sectionless PINs */
                logger.info("Crawling all sectionless PINs of board \"" + boardName + "\" --> Number of sectionless items: " + sectionlessBoardPinCount);
                this.displayBubbleNotification("Board " + boardName + " | Sectionless PINs", "Crawling " + sectionlessBoardPinCount + " sectionless PINs of board " + boardName);
                expectedNumberofItems += sectionlessBoardPinCount;
                final int maxItemsPerPage = 15;
                final Map<String, Object> postDataOptions = new HashMap<String, Object>();
                postDataOptions.put("add_vase", true);
                postDataOptions.put("board_id", boardID);
                postDataOptions.put("field_set_key", "react_grid_pin");
                postDataOptions.put("filter_section_pins", false);
                postDataOptions.put("is_react", true);
                postDataOptions.put("prepend", false);
                postDataOptions.put("page_size", maxItemsPerPage);
                postDataOptions.put("board_feed_ranking_group", "");
                final Map<String, Object> postData = new HashMap<String, Object>();
                postData.put("options", postDataOptions);
                postData.put("context", new HashMap<String, Object>());
                final FilePackage fp = FilePackage.getInstance();
                fp.setName(boardName);
                String boardDescription = (String) resourcesBoardResource.get("seo_description");
                if (StringUtils.isEmpty(boardDescription)) {
                    boardDescription = (String) resourcesBoardResource.get("description");
                }
                if (!StringUtils.isEmpty(boardDescription)) {
                    fp.setComment(boardDescription);
                }
                fp.setPackageKey("pinterest://board/" + boardID);
                /* Grab prefetched data which isn't always available (e.g. unavailable when logged in). */
                final Map<String, Object> resourcesBoardFeedResource = (Map<String, Object>) JavaScriptEngineFactory.walkJson(resources, "BoardFeedResource/{0}");
                ret.addAll(this.crawlPaginationGeneric("BoardFeedResource", resourcesBoardFeedResource, postData, boardTotalPinCount, fp, true));
            } else {
                logger.info("Skipping board " + boardName + " because it does not contain any [sectionless] items.");
            }
        }
        final Regex urluser = new Regex(br.getURL(), PATTERN_USER);
        if (urluser.patternFind()) {
            final Map<String, Object> resourcesUserPinsResource = (Map<String, Object>) JavaScriptEngineFactory.walkJson(resources, "UserPinsResource/{0}");
            if (resourcesUserPinsResource != null) {
                /* This is a user/profile -> Crawl all loose PINs from this profile */
                /* Find user-map */
                final Map<String, Object> usersmap = (Map<String, Object>) initialReduxState.get("users");
                Map<String, Object> usermap = null;
                if (usersmap != null) {
                    for (final Object mapO : usersmap.values()) {
                        final Map<String, Object> map = (Map<String, Object>) mapO;
                        if (map.containsKey("full_name")) {
                            usermap = map;
                            break;
                        }
                    }
                }
                if (usermap == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final String username = usermap.get("username").toString();
                // final String userID = usermap.get("id").toString();
                final int userPinCount = ((Number) usermap.get("pin_count")).intValue();
                final int userBoardCount = ((Number) usermap.get("board_count")).intValue();
                if (userPinCount > 0) {
                    logger.info("Crawling all loose PINs: " + userPinCount);
                    this.displayBubbleNotification("Profile " + username, "Crawling all " + userPinCount + " loose PINs of profile " + username);
                    expectedNumberofItems += userPinCount;
                    final Map<String, Object> postDataOptions = new HashMap<String, Object>();
                    postDataOptions.put("add_vase", true);
                    postDataOptions.put("field_set_key", "mobile_grid_item");
                    postDataOptions.put("is_own_profile_pins", false);
                    postDataOptions.put("username", username);
                    final Map<String, Object> postData = new HashMap<String, Object>();
                    postData.put("options", postDataOptions);
                    postData.put("context", new HashMap<String, Object>());
                    final FilePackage fp = FilePackage.getInstance();
                    fp.setName(username);
                    final String description = (String) usermap.get("seo_description");
                    if (!StringUtils.isEmpty(description)) {
                        fp.setComment(description);
                    }
                    fp.setPackageKey("pinterest://profile_sectionless_pins/" + username);
                    ret.addAll(this.crawlPaginationGeneric("UserPinsResource", resourcesUserPinsResource, postData, userPinCount, fp, true));
                } else {
                    logger.info("Skipping profile " + username + " PINs because this profile does not contain any items.");
                }
                /* TODO: 2024-03-19: Maybe add a setting for this */
                final boolean allowCrawlAllBoardsOfUser = false;
                if (userBoardCount > 0 && allowCrawlAllBoardsOfUser) {
                    logger.info("Crawling all boards: " + userBoardCount);
                    this.displayBubbleNotification("Profile " + username, "Crawling all " + userBoardCount + " boards of profile " + username);
                    final Map<String, Object> resourcesBoardsFeedResource = (Map<String, Object>) JavaScriptEngineFactory.walkJson(resources, "BoardsFeedResource/{0}");
                    expectedNumberofItems += userBoardCount;
                    final Map<String, Object> postDataOptions = new HashMap<String, Object>();
                    postDataOptions.put("field_set_key", "profile_grid_item");
                    postDataOptions.put("filter_stories", false);
                    postDataOptions.put("sort", "last_pinned_to");
                    postDataOptions.put("username", username);
                    final Map<String, Object> postData = new HashMap<String, Object>();
                    postData.put("options", postDataOptions);
                    postData.put("context", new HashMap<String, Object>());
                    ret.addAll(this.crawlPaginationGeneric("BoardsFeedResource", resourcesBoardsFeedResource, postData, userBoardCount, null, true));
                }
            } else {
                /*
                 * No user related information given in html -> It is possible that we're logged in since the pinterest website works in a
                 * different way for logged-in users.
                 */
                final String username = urluser.getMatch(0);
                logger.info("Crawling all loose PINs of user: " + username);
                this.displayBubbleNotification("Profile " + username, "Crawling all loose PINs of profile " + username);
                final Map<String, Object> postDataOptions = new HashMap<String, Object>();
                postDataOptions.put("add_vase", true);
                postDataOptions.put("field_set_key", "mobile_grid_item");
                postDataOptions.put("is_own_profile_pins", false);
                postDataOptions.put("username", username);
                final Map<String, Object> postData = new HashMap<String, Object>();
                postData.put("options", postDataOptions);
                postData.put("context", new HashMap<String, Object>());
                final FilePackage fp = FilePackage.getInstance();
                fp.setName(username);
                fp.setPackageKey("pinterest://profile_sectionless_pins/" + username);
                ret.addAll(this.crawlPaginationGeneric("UserPinsResource", resourcesUserPinsResource, postData, -1, fp, true));
            }
        }
        if (ret.size() == 0 && expectedNumberofItems == 0) {
            /* Empty board/profile */
            throw new DecrypterRetryException(RetryReason.EMPTY_PROFILE);
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlPaginationGeneric(final String resourceType, final Map<String, Object> startMap, final Map<String, Object> postData, final int expectedNumberofItems, final FilePackage fp, final boolean distributeResults) throws Exception {
        final String source_url = br._getURL().getPath();
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String nextbookmark = null;
        List<Map<String, Object>> itemsList = null;
        if (startMap != null) {
            itemsList = (List<Map<String, Object>>) startMap.get("data");
            if (itemsList == null) {
                /* This should never happen! */
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            nextbookmark = startMap.get("nextBookmark").toString();
        }
        final Map<String, Object> postDataOptions = (Map<String, Object>) postData.get("options");
        final String boardID = (String) postDataOptions.get("board_id");
        /**
         * A page size is not always given. It is controlled serverside via the "bookmark" parameter. </br>
         * Any page can have any number of items.
         */
        final Number page_sizeO = (Number) postDataOptions.get("page_size");
        final int maxItemsPerPage = page_sizeO != null ? page_sizeO.intValue() : -1;
        int page = 1;
        int crawledItems = 0;
        final List<Integer> pagesWithPossiblyMissingItems = new ArrayList<Integer>();
        do {
            if (startMap == null || page > 1) {
                /* Load first page or next page */
                if (nextbookmark != null) {
                    postDataOptions.put("bookmarks", new String[] { nextbookmark });
                }
                final GetRequest request = prepWebapiRequest(br.createGetRequest("/resource/" + resourceType + "/get/?source_url=" + Encoding.urlEncode(source_url) + "&data=" + URLEncode.encodeURIComponent(JSonStorage.serializeToJson(postData)) + "&_=" + System.currentTimeMillis()), "www/[username]/[slug].js");
                br.getPage(request);
                final Map<String, Object> entries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
                final Map<String, Object> resource_response = (Map<String, Object>) entries.get("resource_response");
                nextbookmark = (String) resource_response.get("bookmark");
                itemsList = (List<Map<String, Object>>) resource_response.get("data");
            }
            if (itemsList.size() < maxItemsPerPage) {
                /* Fail-safe */
                logger.info("Found page with possibly missing items: " + page);
                pagesWithPossiblyMissingItems.add(page);
            }
            for (final Map<String, Object> item : itemsList) {
                final List<DownloadLink> results = proccessMap(item, boardID, fp, distributeResults);
                ret.addAll(results);
            }
            crawledItems += itemsList.size();
            logger.info("Crawled page: " + page + " | " + crawledItems + "/" + expectedNumberofItems + " items crawled | retSize=" + ret.size() + " | nextbookmark= " + nextbookmark);
            if (this.isAbort()) {
                logger.info("Stopping because: Aborted by user");
                throw new InterruptedException();
            } else if (StringUtils.isEmpty(nextbookmark) || nextbookmark.equalsIgnoreCase("-end-")) {
                logger.info("Stopping because: Reached end");
                break;
            } else if (expectedNumberofItems != -1 && crawledItems >= expectedNumberofItems) {
                /* Fail-safe */
                logger.info("Stopping because: Found all sectionless items");
                break;
            } else {
                /* Continue to next page */
                page += 1;
            }
        } while (!this.isAbort());
        final long numberofMissingItems = expectedNumberofItems != -1 ? expectedNumberofItems - crawledItems : 0;
        if (numberofMissingItems > 0) {
            /*
             * 2024-02-13: Sometimes items are missing for unknown reasons e.g. one is missing here:
             * https://www.pinterest.de/josielindatoth/deserts/
             */
            String msg = "Missing items: " + numberofMissingItems;
            if (pagesWithPossiblyMissingItems.size() > 0) {
                msg += "\nPages where those items should be located: " + pagesWithPossiblyMissingItems.toString();
            }
            this.displayBubbleNotification("Missing PINs in package " + fp.getName(), msg);
        }
        return ret;
    }

    private ArrayList<DownloadLink> crawlSinglePIN(final String pinID) throws Exception {
        if (pinID == null) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String contenturl = "https://www." + this.getHost() + "/pin/" + pinID + "/";
        final DownloadLink singlePIN = this.createDownloadlink(contenturl);
        if (enable_crawl_alternative_URL) {
            /* The more complicated way (if wished by user). */
            /**
             * 2021-03-02: PINs may redirect to other PINs in very rare cases -> Handle that </br>
             * If that wasn't the case, we could rely on API-only!
             */
            br.getPage(contenturl);
            checkSinglePINOffline(br);
            final Map<String, Object> pinMap = getPINMap(this.br, br.getURL());
            setInfoOnDownloadLink(singlePIN, pinMap);
            final String externalURL = getAlternativeExternalURLInPINMap(pinMap);
            if (externalURL != null) {
                ret.add(this.createDownloadlink(externalURL));
            }
        }
        ret.add(singlePIN);
        return ret;
    }

    public static void checkSinglePINOffline(final Browser br) throws PluginException, IOException {
        String redirect = br.getRegex("window\\.location\\s*=\\s*\"([^\"]+)\"").getMatch(0);
        if (redirect != null) {
            /* We want the full URL. */
            redirect = br.getURL(redirect).toExternalForm();
        }
        if (!new Regex(br.getURL(), PinterestComDecrypter.PATTERN_PIN).patternFind()) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (redirect != null && redirect.contains("show_error=true")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.containsHTML("\"__isError\":\"PinNotFound\"")) {
            /* 2025-07-03 e.g. /pin/679832506287009723/ */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
    }

    public static void setInfoOnDownloadLink(final DownloadLink dl, final Map<String, Object> map) {
        final Map<String, Object> data = map.containsKey("data") ? (Map<String, Object>) map.get("data") : map;
        // final String directlink = getDirectlinkFromPINMap(data);
        final List<String> directurlsList = getDirectlinkFromPINMap(data);
        final String description = (String) data.get("description");
        if (!StringUtils.isEmpty(description)) {
            if (StringUtils.isEmpty(dl.getComment())) {
                dl.setComment(description);
            }
            dl.setProperty(PinterestCom.PROPERTY_DESCRIPTION, description);
        }
        if (directurlsList != null && !directurlsList.isEmpty()) {
            dl.setProperty(PinterestCom.PROPERTY_DIRECTURL_LIST, directurlsList);
        }
        dl.setAvailable(true);
        dl.setProperty(PinterestCom.PROPERTY_TITLE, data.get("title"));
        PinterestCom.setFilename(dl);
    }

    /** Accesses pinterest API and return map of PIN. */
    public static Map<String, Object> getPINMap(final Browser br, final String pinURL) throws Exception {
        final String pinID = PinterestCom.getPinID(pinURL);
        if (pinID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String[] jssnippets = br.getRegex("<script[^>]*type=\"application/json\"[^>]*>(.*?)</script>").getColumn(0);
        if (jssnippets != null && jssnippets.length > 0) {
            /* Try to find json source in html code */
            for (final String jssnippet : jssnippets) {
                final Object parsedjson = JavaScriptEngineFactory.jsonToJavaObject(jssnippet);
                if (!(parsedjson instanceof Map)) {
                    continue;
                }
                final Map<String, Object> map = (Map<String, Object>) parsedjson;
                Map<String, Object> pin_root = (Map<String, Object>) JavaScriptEngineFactory.walkJson(map, "response/data/v3GetPinQuery/data");
                if (pin_root == null) {
                    /* 2025-10-30: When logged in */
                    pin_root = (Map<String, Object>) JavaScriptEngineFactory.walkJson(map, "initialReduxState/resources/PinResource/{0}/data");
                }
                if (pin_root != null) {
                    return pin_root;
                }
            }
        }
        List<Object> resource_data_cache = null;
        final GetRequest request = prepWebapiRequest(br.createGetRequest("https://www.pinterest.com/resource/PinResource/get/?source_url=%2Fpin%2F" + pinID + "%2F&data=%7B%22options%22%3A%7B%22field_set_key%22%3A%22detailed%22%2C%22ptrf%22%3Anull%2C%22fetch_visual_search_objects%22%3Atrue%2C%22id%22%3A%22" + pinID + "%22%7D%2C%22context%22%3A%7B%7D%7D&module_path=Pin(show_pinner%3Dtrue%2C+show_board%3Dtrue%2C+is_original_pin_in_related_pins_grid%3Dtrue)&_=" + System.currentTimeMillis()), "www/pin/[id].js");
        br.getPage(request);
        final Map<String, Object> root = (Map<String, Object>) JavaScriptEngineFactory.jsonToJavaObject(br.getRequest().getHtmlCode());
        if (root.containsKey("resource_data_cache")) {
            resource_data_cache = (List) root.get("resource_data_cache");
        } else {
            /* 2020-02-17 */
            final Object pinO = root.get("resource_response");
            if (pinO != null) {
                resource_data_cache = new ArrayList<Object>();
                resource_data_cache.add(pinO);
            }
        }
        if (resource_data_cache == null) {
            return null;
        }
        for (final Object resource_object : resource_data_cache) {
            final Map<String, Object> map = (Map<String, Object>) resource_object;
            final String this_pin_id = (String) JavaScriptEngineFactory.walkJson(map, "data/id");
            if (StringUtils.equals(this_pin_id, pinID) || resource_data_cache.size() == 1) {
                /* We've reached our goal */
                return map;
            }
        }
        /* PIN does not exist(?) */
        return null;
    }

    /** Returns highest resolution image URL inside given PIN Map. */
    public static List<String> getDirectlinkFromPINMap(final Map<String, Object> map) {
        final List<String> ret = new ArrayList<String>();
        // TODO: Return list of possible URLs here since sometimes e.g. one/the "best" image quality is unavailable while another one is
        // available.
        /* First check if we have a video */
        final Map<String, Object> video_list = (Map<String, Object>) (JavaScriptEngineFactory.walkJson(map, "videos/video_list"));
        if (video_list != null) {
            for (final String knownVideoQualities : new String[] { "V_1080P", "V_720P", "V_480P" }) {
                final Map<String, Object> video = (Map<String, Object>) video_list.get(knownVideoQualities);
                if (video == null) {
                    /* Video quality doesn't exist */
                    continue;
                }
                final String videourl = (String) video.get("url");
                if (!StringUtils.isEmpty(videourl)) {
                    ret.add(videourl.toString());
                    return ret;
                }
            }
            /* No known video quality was found */
        }
        /* No video --> Must be photo item */
        final Map<String, Object> imagesmap = (Map<String, Object>) map.get("images");
        if (imagesmap != null) {
            String originalImageURL = (String) imagesmap.get("url");
            if (originalImageURL != null) {
                /* V3GetPin */
                ret.add(originalImageURL);
                return ret;
            }
            /* Original image NOT available --> Take the best we can find */
            String bestNonOriginalImage = null;
            int bestHeight = -1;
            final Iterator<Entry<String, Object>> it = imagesmap.entrySet().iterator();
            while (it.hasNext()) {
                final Entry<String, Object> entry = it.next();
                final String label = entry.getKey();
                final Map<String, Object> imagemap = (Map<String, Object>) entry.getValue();
                final int height = ((Number) imagemap.get("height")).intValue();
                final String imageurl = imagemap.get("url").toString();
                if (label.equalsIgnoreCase("orig")) {
                    originalImageURL = imageurl;
                }
                if (bestNonOriginalImage == null || height > bestHeight) {
                    bestNonOriginalImage = imageurl;
                    bestHeight = height;
                }
            }
            if (originalImageURL != null) {
                ret.add(originalImageURL);
            }
            ret.add(bestNonOriginalImage);
            return ret;
        }
        return null;
    }

    /** Returns e.g. an alternative, probably higher quality imgur.com URL to the same image which we have as Pinterest PIN here. */
    private String getAlternativeExternalURLInPINMap(final Map<String, Object> pinMap) {
        String externalURL = null;
        try {
            String path;
            if (pinMap.containsKey("data")) {
                path = "data/rich_metadata/url";
            } else {
                path = "rich_metadata/url";
            }
            externalURL = (String) JavaScriptEngineFactory.walkJson(pinMap, path);
        } catch (final Throwable e) {
        }
        return externalURL;
    }

    /** Crawls a section for CryptedLink items which have properties set which are needed to crawl a section (new method 2024). */
    private ArrayList<DownloadLink> crawlSection(final CryptedLink param) throws Exception {
        final String url = param.getCryptedUrl();
        final Regex boardSectionRegex = new Regex(url, PATTERN_BOARD_SECTION);
        if (!boardSectionRegex.patternFind()) {
            /* Developer mistake */
            throw new IllegalArgumentException();
        }
        final DownloadLink sourceItem = param.getDownloadLink();
        if (sourceItem == null) {
            logger.info("Section URL without DownloadLink context");
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        /* Obtain cached data */
        final String boardID = sourceItem.getStringProperty("board_id");
        final String sectionID = sourceItem.getStringProperty("section_id");
        if (boardID == null || sectionID == null) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String username = boardSectionRegex.getMatch(0);
        final String board_slug = boardSectionRegex.getMatch(1);
        final String section_slug = boardSectionRegex.getMatch(2);
        final FilePackage fp = getSectionFilePackage(username, board_slug, section_slug, boardID, sectionID);
        return this.crawlSection(br, url, username, board_slug, section_slug, boardID, sectionID, fp);
    }

    /** 2025-03-18: This doesn't work anymore */
    private ArrayList<DownloadLink> crawlSection(final Browser br, final String source_url, final String username, final String board_slug, final String section_slug, final String boardID, final String sectionID, final FilePackage fp) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        int processedPINCounter = 0;
        int pageCounter = 1;
        /* Single section pagination */
        // final String url_section = "https://www.pinterest.com/" + source_url + section_title + "/";
        final int maxPINsPerRequest = 25;
        final Map<String, Object> pinPaginationPostDataOptions = new HashMap<String, Object>();
        pinPaginationPostDataOptions.put("isPrefetch", false);
        pinPaginationPostDataOptions.put("currentFilter", -1);
        pinPaginationPostDataOptions.put("field_set_key", "react_grid_pin");
        pinPaginationPostDataOptions.put("is_own_profile_pins", false);
        pinPaginationPostDataOptions.put("page_size", maxPINsPerRequest);
        pinPaginationPostDataOptions.put("redux_normalize_feed", true);
        pinPaginationPostDataOptions.put("section_id", sectionID);
        pinPaginationPostDataOptions.put("no_fetch_context_on_resource", false);
        final Map<String, Object> pinPaginationpostDataContext = new HashMap<String, Object>();
        Map<String, Object> pinPaginationPostData = new HashMap<String, Object>();
        pinPaginationPostData.put("options", pinPaginationPostDataOptions);
        pinPaginationPostData.put("context", pinPaginationpostDataContext);
        pagination: do {
            String url = "/resource/BoardSectionPinsResource/get/?source_url=" + URLEncode.encodeURIComponent(source_url) + "&data=" + URLEncode.encodeURIComponent(JSonStorage.serializeToJson(pinPaginationPostData)) + "&_=" + System.currentTimeMillis();
            if (br.getRequest() == null) {
                /* First request */
                url = "https://" + getHost() + url;
            }
            final GetRequest request = prepWebapiRequest(br.createGetRequest(url), "www/[username]/[slug].js");
            br.getPage(request);
            final Map<String, Object> sectionPaginationInfo = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final Object bookmarksO = JavaScriptEngineFactory.walkJson(sectionPaginationInfo, "resource/options/bookmarks");
            final String bookmarks = (String) JavaScriptEngineFactory.walkJson(sectionPaginationInfo, "resource/options/bookmarks/{0}");
            final List<Map<String, Object>> pins = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(sectionPaginationInfo, "resource_response/data");
            int numberofNewItemsThisPage = 0;
            for (final Map<String, Object> pinmap : pins) {
                final List<DownloadLink> thisRet = proccessMap(pinmap, boardID, fp, true);
                ret.addAll(thisRet);
                numberofNewItemsThisPage++;
            }
            processedPINCounter += pins.size();
            logger.info("Crawled section " + sectionID + " page: " + pageCounter + " | Processed items on this page: " + numberofNewItemsThisPage + " | Processed PINs so far: " + processedPINCounter);
            if (this.isAbort()) {
                /* Aborted by user */
                throw new InterruptedException();
            } else if (StringUtils.isEmpty(bookmarks) || bookmarks.equals("-end-") || bookmarksO == null) {
                /* Looks as if we've reached the end */
                logger.info("Stopping because: Reached end");
                break pagination;
            } else if (numberofNewItemsThisPage == 0) {
                /* Fail safe */
                logger.info("Stopping because: Current page did not contain any new items");
                break pagination;
            }
            /* Continue to next page */
            pinPaginationPostDataOptions.put("bookmarks", bookmarksO);
            pageCounter++;
        } while (!this.isAbort());
        logger.info("Number of PINs in current section: " + processedPINCounter);
        return ret;
    }

    /**
     * Crawls single PIN from given Map.
     *
     * @throws IOException
     */
    private List<DownloadLink> proccessMap(final Map<String, Object> map, final String board_id, final FilePackage fp, final boolean distributeResults) throws PluginException, IOException {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final String type = map.get("type").toString();
        if (type.equals("pin") || type.equals("interest")) {
            final Map<String, Object> user = (Map<String, Object>) map.get("pinner");
            final String pin_id = map.get("id").toString();
            final String username = user != null ? user.get("username").toString() : null;
            final DownloadLink dl = this.createDownloadlink("https://www." + this.getHost() + "/pin/" + pin_id + "/");
            if (!StringUtils.isEmpty(board_id)) {
                dl.setProperty("boardid", board_id);
            }
            if (!StringUtils.isEmpty(username)) {
                dl.setProperty("username", username);
            }
            setInfoOnDownloadLink(dl, map);
            if (fp != null) {
                dl._setFilePackage(fp);
            }
            ret.add(dl);
            String externalURL = null;
            if (this.enable_crawl_alternative_URL && (externalURL = getAlternativeExternalURLInPINMap(map)) != null) {
                ret.add(this.createDownloadlink(externalURL));
            }
        } else if (type.equals("board")) {
            final String boardID = map.get("id").toString();
            final String boardURL = map.get("url").toString();
            final String fullurl = br.getURL(boardURL).toExternalForm();
            final DownloadLink dl = this.createDownloadlink(fullurl);
            dl.setProperty("board_id", boardID);
            ret.add(dl);
        } else if (type.equals("board_section")) {
            if (currentBoardPath == null) {
                /* Developer mistake */
                throw new IllegalArgumentException();
            }
            final String sectionSlug = map.get("slug").toString();
            final DownloadLink section = this.createDownloadlink("https://" + getHost() + currentBoardPath + sectionSlug);
            /* Important for next crawl-round */
            section.setProperty("board_id", board_id);
            section.setProperty("section_id", map.get("id"));
            ret.add(section);
        } else {
            logger.info("Ignoring invalid type: " + type);
        }
        if (distributeResults) {
            distribute(ret);
        }
        return ret;
    }

    /** Recursive function to find the ID of a sectionSlug. */
    private String recursiveFindSectionID(final Object jsono, final String sectionSlug) throws PluginException {
        if (jsono instanceof Map) {
            final Map<String, Object> map = (Map<String, Object>) jsono;
            final Object slugO = map.get("slug");
            if (slugO != null && slugO instanceof String && slugO.toString().equals(sectionSlug)) {
                return map.get("id").toString();
            }
            final Iterator<Entry<String, Object>> iterator = map.entrySet().iterator();
            while (iterator.hasNext()) {
                final Entry<String, Object> entry = iterator.next();
                // final String key = entry.getKey();
                final Object value = entry.getValue();
                if (value instanceof List || value instanceof Map) {
                    final String result = recursiveFindSectionID(value, sectionSlug);
                    if (result != null) {
                        return result;
                    }
                }
            }
        } else if (jsono instanceof ArrayList) {
            final List<Object> ressourcelist = (List<Object>) jsono;
            for (final Object arrayo : ressourcelist) {
                if (arrayo instanceof List || arrayo instanceof Map) {
                    final String result = recursiveFindSectionID(arrayo, sectionSlug);
                    if (result != null) {
                        return result;
                    }
                }
            }
        }
        return null;
    }

    /**
     * @return: true: target section was found and only this will be crawler false: failed to find target section - in this case we should
     *          crawl everything we find </br>
     *          This can return a lot of results e.g. a board contains 1000 sections, each section contains 1000 PINs...
     */
    private ArrayList<DownloadLink> crawlSections(final String username, final String boardID, final String boardName, final Browser ajax, final String contenturl) throws Exception {
        if (username == null || boardID == null || boardName == null) {
            throw new IllegalArgumentException();
        }
        final Map<String, Object> postDataOptions = new HashMap<String, Object>();
        final String source_url = new URL(contenturl).getPath();
        // postDataOptions.put("isPrefetch", false);
        postDataOptions.put("board_id", boardID);
        postDataOptions.put("redux_normalize_feed", true);
        // postDataOptions.put("no_fetch_context_on_resource", false);
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final Map<String, Object> postData = new HashMap<String, Object>();
        postData.put("options", postDataOptions);
        postData.put("context", new HashMap<String, Object>());
        int sectionPage = -1;
        GetRequest request = prepWebapiRequest(ajax.createRequest("/resource/BoardSectionsResource/get/?source_url=" + Encoding.urlEncode(source_url) + "&data=" + URLEncode.encodeURIComponent(JSonStorage.serializeToJson(postData)) + "&_=" + System.currentTimeMillis()), "www/[username]/[slug].js");
        ajax.getPage(request);
        final int maxSectionsPerPage = 25;
        sectionPagination: do {
            sectionPage += 1;
            logger.info("Crawling sections page: " + (sectionPage + 1));
            final Map<String, Object> sectionsData = restoreFromString(ajax.getRequest().getHtmlCode(), TypeRef.MAP);
            final List<Map<String, Object>> sections = (List<Map<String, Object>>) JavaScriptEngineFactory.walkJson(sectionsData, "resource_response/data");
            int sectionCounter = 1;
            for (final Map<String, Object> entries : sections) {
                final String section_title = entries.get("title").toString();
                // final String sectionSlug = (String) entries.get("slug");
                // final long section_total_pin_count = ((Number) entries.get("pin_count")).longValue();
                final String sectionID = entries.get("id").toString();
                logger.info("Crawling section " + sectionCounter + " of " + sections.size() + " --> ID = " + sectionID);
                final FilePackage fp = FilePackage.getInstance();
                fp.setName(username + " - " + boardName + " - " + Encoding.htmlDecode(section_title));
                fp.setPackageKey("pinterest://board/" + boardID + "/section/" + sectionID);
                // TODO: Add board_slug and section_slug
                ret.addAll(crawlSection(ajax, source_url, username, null, null, boardID, sectionID, fp));
                sectionCounter += 1;
                if (this.isAbort()) {
                    throw new InterruptedException();
                }
            }
            final String sectionsNextBookmark = (String) JavaScriptEngineFactory.walkJson(sectionsData, "resource_response/bookmark");
            if (StringUtils.isEmpty(sectionsNextBookmark) || sectionsNextBookmark.equalsIgnoreCase("-end-")) {
                logger.info("Stopping sections crawling because: Reached end");
                break sectionPagination;
            } else if (sections.size() < maxSectionsPerPage) {
                /* Fail safe */
                logger.info("Stopping because: Current page contains less than " + maxSectionsPerPage + " items");
                break sectionPagination;
            } else {
                postDataOptions.put("bookmarks", new String[] { sectionsNextBookmark });
                request = prepWebapiRequest(ajax.createRequest("/resource/BoardSectionsResource/get/?source_url=" + Encoding.urlEncode(source_url) + "&data=" + URLEncode.encodeURIComponent(JSonStorage.serializeToJson(postData)) + "&_=" + System.currentTimeMillis()), "www/[username]/[slug].js");
                ajax.getPage(request);
            }
        } while (!this.isAbort());
        logger.info("Section crawler done");
        return ret;
    }

    private FilePackage getSectionFilePackage(final String username, final String boardname, final String sectionname, final String boardID, final String sectionID) {
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(username + " - " + boardname + " - " + sectionname);
        if (boardID != null && sectionID != null) {
            fp.setPackageKey("pinterest://board/" + boardID + "/section/" + sectionID);
        } else {
            fp.setPackageKey("pinterest://board/" + username + "/section/" + sectionname);
        }
        return fp;
    }

    protected static <T extends Request> T prepWebapiRequest(final T request, String pwsHandler) {
        request.getHeaders().put("Accept", "application/json, text/javascript, */*, q=0.01");
        request.getHeaders().put("Referer", "https://www.pinterest.com/");
        // br.getHeaders().put("x-app-version", "d406622");
        // br.getHeaders().put("x-pinterest-appstate", "background");
        request.getHeaders().put("x-pinterest-pws-handler", pwsHandler);
        // br.getHeaders().put("x-pinterest-source-url", "/username/boardname/");
        request.getHeaders().put("x-requested-with", "XMLHttpRequest");
        return request;
    }
}
