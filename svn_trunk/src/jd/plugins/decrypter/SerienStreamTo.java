//jDownloader - Downloadmanager
//Copyright (C) 2012  JD-Team support@jdownloader.org
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.AbstractCloudflareTurnstileCaptcha;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperCrawlerPluginCloudflareTurnstile;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperCrawlerPluginRecaptchaV2;
import org.jdownloader.plugins.components.config.SerienStreamToConfig;
import org.jdownloader.plugins.components.config.SerienStreamToConfig.SeasonCrawlMode;
import org.jdownloader.plugins.config.PluginConfigInterface;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.gui.UserIO;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DecrypterRetryException;
import jd.plugins.DecrypterRetryException.RetryReason;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 52603 $", interfaceVersion = 3, names = {}, urls = {})
public class SerienStreamTo extends PluginForDecrypt {
    @SuppressWarnings("deprecation")
    public SerienStreamTo(final PluginWrapper wrapper) {
        super(wrapper);
    }

    /**
     * Path patterns down below <br>
     * Marked as old: aniworld urls, formerly also used for serienstream <br>
     * Marked as new: serienstream urls new 2026-01-29
     */
    private static final Pattern SERIES_REGEX                                 = Pattern.compile("([\\w-]+)(/staffel-(\\d+)(/episode-(\\d+))?)?", Pattern.CASE_INSENSITIVE);
    /*
     * 2026-03-06: The "/filme..." urls are only available on aniworld.com. The other website s.to is using dummy season number "0" for
     * movies that are part of a series.
     */
    private static final Pattern MOVIES_REGEX                                 = Pattern.compile("([\\w-]+)/filme(/film-(\\d+))?", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_SERIES_OLD_MOVIES                       = Pattern.compile("/anime/stream/" + MOVIES_REGEX.pattern(), Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_SERIES_OLD                              = Pattern.compile("/anime/stream/" + SERIES_REGEX.pattern(), Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_SERIES_NEW                              = Pattern.compile("/serie/" + SERIES_REGEX.pattern(), Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_SINGLE_REDIRECT_OLD                     = Pattern.compile("/redirect/(\\d+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_SINGLE_REDIRECT_NEW                     = Pattern.compile("/r\\?t=(ey[a-zA-Z0-9_/\\+\\=\\-%]+)", Pattern.CASE_INSENSITIVE);
    private static final String  PROPERTY_IS_PART_OF_COMPLETE_SEASON_CRAWLING = "is_part_of_complete_season_crawling";

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "s.to", "serienstream.sx", "serienstream.to", "serienstream.ch", "serienstream.stream", "serien.sx", "serien.domains", "186.2.175.5" });
        ret.add(new String[] { "aniworld.to" });
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        final List<String> ret = new ArrayList<String>();
        int i = 0;
        for (final String[] domains : getPluginDomains()) {
            if (i == 0) {
                /* serienstream domains */
                ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_SERIES_NEW.pattern() + "|" + TYPE_SINGLE_REDIRECT_NEW.pattern() + ")");
            } else {
                /* aniworld domains */
                /* The order of these patterns is important!! */
                ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_SERIES_OLD_MOVIES.pattern() + "|" + TYPE_SERIES_OLD.pattern() + "|" + TYPE_SINGLE_REDIRECT_OLD.pattern() + ")");
            }
            i++;
        }
        return ret.toArray(new String[0]);
    }

    @SuppressWarnings("deprecation")
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, final ProgressController progress) throws Exception {
        final String contenturl = param.getCryptedUrl();
        if (new Regex(param.getCryptedUrl(), TYPE_SINGLE_REDIRECT_OLD).patternFind() || new Regex(param.getCryptedUrl(), TYPE_SINGLE_REDIRECT_NEW).patternFind()) {
            final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
            ret.add(this.crawlSingleRedirect(br, contenturl));
            return ret;
        } else {
            return this.crawlMirrors(param, contenturl);
        }
    }

    private DownloadLink crawlSingleRedirect(final Browser br, String url) throws PluginException, InterruptedException, DecrypterException, IOException, DecrypterRetryException {
        /* Enforce https */
        url = url.replaceFirst("^(?i)http://", "https://");
        br.setFollowRedirects(false);
        final String initialHost = Browser.getHost(url, true);
        String redirectPage = br.getPage(url);
        String finallink = null;
        if (br.getRedirectLocation() != null) {
            finallink = br.getRedirectLocation();
        } else if (AbstractRecaptchaV2.containsRecaptchaV2Class(br) || br.containsHTML("grecaptcha")) {
            br.setFollowRedirects(true);
            final Form captcha = br.getForm(0);
            if (captcha == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String sitekey = new Regex(redirectPage, "grecaptcha\\.execute\\('([^']+)'").getMatch(0);
            final String response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br, sitekey) {
                @Override
                public TYPE getType() {
                    return TYPE.INVISIBLE;
                }
            }.getToken();
            captcha.put("original", "");
            captcha.put("token", Encoding.urlEncode(response));
            redirectPage = br.submitForm(captcha);
            finallink = br.getURL();
        } else if (AbstractCloudflareTurnstileCaptcha.containsCloudflareTurnstileClass(br) || br.containsHTML("cf-turnstile")) {
            br.setFollowRedirects(true);
            final Form captcha = br.getForm(0);
            if (captcha == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final String response = new CaptchaHelperCrawlerPluginCloudflareTurnstile(this, br).getToken();
            captcha.put("original", "");
            captcha.put("cf-turnstile-response", Encoding.urlEncode(response));
            redirectPage = br.submitForm(captcha);
            finallink = br.getURL();
        }
        if (br.getHttpConnection().getResponseCode() == 404 || br.getHttpConnection().getResponseCode() == 410) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        if (finallink == null) {
            if (br.containsHTML(" var t = \"")) {
                /**
                 * 2026-04-01: Cloudflare Turnstile + Altcaptcha required (we do not support Altcaptcha) <br>
                 * See: https://board.jdownloader.org/showthread.php?p=556817#post556817 * <br>
                 * Screenshot: https://snipboard.io/Bk79ul.jpg/
                 */
                throw new DecrypterRetryException(RetryReason.HOST_RATE_LIMIT);
            }
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (this.canHandle(finallink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (Browser.getHost(finallink, true).equalsIgnoreCase(initialHost)) {
            /* E.g. redirect to mainpage */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return createDownloadlink(finallink);
    }

    private ArrayList<DownloadLink> crawlMirrors(final CryptedLink param, final String contenturl) throws PluginException, InterruptedException, DecrypterException, IOException, DecrypterRetryException {
        br.setFollowRedirects(true);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final SerienStreamToConfig cfg = (SerienStreamToConfig) get(this.getConfigInterface());
        final boolean isNewWebsite = new Regex(contenturl, TYPE_SERIES_NEW).patternFind();
        String seriesTitle = null;
        String episodeTitle = null;
        if (isNewWebsite) {
            seriesTitle = br.getRegex("id=\"seasonsEpisodesModalLabel\">([^<]+)</h5>").getMatch(0);
            if (seriesTitle == null) {
                seriesTitle = br.getRegex("<h5 class=\"modal-title\" id=\"trailerModalLabel\"[^>]*>([^<]+) Trailer</h5>").getMatch(0);
            }
            episodeTitle = br.getRegex("<h2 class=\"h4 mb-1\"[^>]*>([^<]+)</h2>").getMatch(0);
        } else {
            seriesTitle = br.getRegex("class=\"hostSeriesTitle\"[^>]*>\\s*<strong>([^<]+)</strong>").getMatch(0);
        }
        if (episodeTitle != null) {
            episodeTitle = Encoding.htmlDecode(episodeTitle).trim();
        }
        /* The order of these patterns is important!! */
        final Regex urlinfo_movies = new Regex(br._getURL().getPath(), MOVIES_REGEX.pattern() + "$");
        final Regex urlinfo_series = new Regex(br._getURL().getPath(), SERIES_REGEX.pattern() + "$");
        final String seriesTitleSlug;
        final String seasonNumberStrFromURL;
        final String episodeNumberStr;
        if (urlinfo_movies.patternFind()) {
            seriesTitleSlug = urlinfo_movies.getMatch(0);
            seasonNumberStrFromURL = "filme";
            episodeNumberStr = urlinfo_movies.getMatch(2);
        } else if (urlinfo_series.patternFind()) {
            seriesTitleSlug = urlinfo_series.getMatch(0);
            seasonNumberStrFromURL = urlinfo_series.getMatch(2);
            episodeNumberStr = urlinfo_series.getMatch(4);
        } else {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        if (seriesTitle == null) {
            logger.warning("Failed to find seriesTitle -> Using fallback: " + seriesTitleSlug);
            seriesTitle = seriesTitleSlug.replace("-", " ").trim();
        }
        seriesTitle = Encoding.htmlDecode(seriesTitle).trim();
        if (episodeNumberStr == null) {
            /* No single episode number given -> Crawl current- or selected seasons */
            if (seasonNumberStrFromURL != null) {
                /* Crawl all episodes from season */
                return this.crawlCurrentSeason(param, contenturl, seriesTitleSlug, seasonNumberStrFromURL);
            }
            /* Crawl seasons selected by user */
            /*
             * No season slug given -> Use season 1 as default to get same results as website which displays episodes of season 1 if no
             * season is given in URL.
             */
            /* Crawl all user selected seasons */
            final String seriesTitleSlugQuoted = Pattern.quote(seriesTitleSlug);
            final String[] seasonNumbers = br.getRegex(seriesTitleSlugQuoted + "/staffel-(\\d+)").getColumn(0);
            final List<String> seasonNumbersWithoutDupes = new ArrayList<String>();
            boolean hasFilms = false;
            if (seasonNumbers != null && seasonNumbers.length > 0) {
                for (final String seasonNumber : seasonNumbers) {
                    if (seasonNumbersWithoutDupes.contains(seasonNumber)) {
                        continue;
                    }
                    if (seasonNumber.equals("0")) {
                        hasFilms = true;
                    }
                    seasonNumbersWithoutDupes.add(seasonNumber);
                }
            }
            /* IMPORTANT: Dev if u stupid and add non-number items, exception will happen down below!! */
            Collections.sort(seasonNumbersWithoutDupes, new Comparator<String>() {
                @Override
                public int compare(final String a, final String b) {
                    return Integer.parseInt(a) - Integer.parseInt(b);
                }
            });
            if (!hasFilms && br.containsHTML(seriesTitleSlugQuoted + "/filme")) {
                seasonNumbersWithoutDupes.add(0, "Filme");
                hasFilms = true;
            }
            if (seasonNumbersWithoutDupes.isEmpty()) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final SeasonCrawlMode mode = cfg.getSeasonCrawlMode();
            final List<String> selectedSeasonNumbers = new ArrayList<String>();
            boolean crawlOnlyCurrentlyActiveSeason = false;
            if (seasonNumbersWithoutDupes.size() > 1 && mode == SeasonCrawlMode.ASK) {
                int defaultSelectedItemIndex = 0;
                if (hasFilms) {
                    /* Select season 1 by default, not "Filme", same behavior as in browser. */
                    defaultSelectedItemIndex = 1;
                }
                final String[] userOptions = new String[seasonNumbersWithoutDupes.size()];
                for (int i = 0; i < seasonNumbersWithoutDupes.size(); i++) {
                    final String seasonNumberStr = seasonNumbersWithoutDupes.get(i);
                    if (seasonNumberStr.equalsIgnoreCase("Filme") || seasonNumberStr.equals("0")) {
                        /* Special season called "Filme" */
                        userOptions[i] = "Filme";
                    } else {
                        userOptions[i] = "Staffel " + seasonNumbersWithoutDupes.get(i);
                    }
                }
                final String text = "Staffelauswahl für " + seriesTitle + "\r\nVorauswahl über " + getHost() + " Plugineinstellungen möglich";
                final int[] selectedItems = UserIO.getInstance().requestMultiSelectionDialog(0, episodeTitle, text, userOptions, new int[] { defaultSelectedItemIndex }, null, "Ausgewählte Staffeln crawlen", "Nichts crawlen", null);
                if (selectedItems == null) {
                    logger.info("User aborted season crawl dialog -> Crawling nothing");
                    return ret;
                } else if (selectedItems.length == 0) {
                    logger.info("User selected zero seasons to crawl in season crawl dialog -> Crawling nothing");
                    return ret;
                }
                for (final int selectedItemIndex : selectedItems) {
                    selectedSeasonNumbers.add(seasonNumbersWithoutDupes.get(selectedItemIndex));
                }
            } else if (mode == SeasonCrawlMode.FIRST_SEASON_PRESENTED_IN_BROWSER) {
                /* Crawl first season only */
                /* Important: First season does not have to be season 1! It can also be season 0!! */
                selectedSeasonNumbers.add(seasonNumbersWithoutDupes.get(0));
                crawlOnlyCurrentlyActiveSeason = true;
            } else {
                /* Crawl all seasons */
                selectedSeasonNumbers.addAll(seasonNumbersWithoutDupes);
            }
            logger.info("Selected seasons by user: " + selectedSeasonNumbers.size() + "/" + seasonNumbersWithoutDupes.size() + " | " + selectedSeasonNumbers);
            /* Crawl season that is currently open/visible in browser */
            String activeSeasonNumberStr = br.getRegex("aria-current=\"page\" title=\"Staffel (\\d+)\"").getMatch(0); // s.to
            if (activeSeasonNumberStr == null) {
                activeSeasonNumberStr = br.getRegex("class=\"active\"[^>]*title=\"Staffel (\\d+)\"").getMatch(0); // aniworld
            }
            if (activeSeasonNumberStr != null && (crawlOnlyCurrentlyActiveSeason || selectedSeasonNumbers.remove(activeSeasonNumberStr))) {
                final ArrayList<DownloadLink> episodes = this.crawlCurrentSeason(param, contenturl, seriesTitleSlug, activeSeasonNumberStr);
                ret.addAll(episodes);
                if (selectedSeasonNumbers.isEmpty()) {
                    /*
                     * User wants only first season -> Episodes of that season are already in html code so we can crawl them right away
                     * without the need to do any additional html requests
                     */
                    /* Early return */
                    logger.info("User selected only the season that is already currently active -> Crawled all episodes already -> Early return");
                    return ret;
                }
            }
            for (final String seasonNumStr : selectedSeasonNumbers) {
                final String url;
                if (seasonNumStr.equalsIgnoreCase("Filme")) {
                    url = br.getURL() + "/filme";
                } else {
                    url = br.getURL() + "/staffel-" + seasonNumStr;
                }
                final DownloadLink link = this.createDownloadlink(url);
                ret.add(link);
            }
            return ret;
        }
        /* 2026-01-30: Movies are listed as season 0 and then have "normal" episode numbers starting from 1. */
        // final boolean isMovie = seriesSeasonNumberStr.equals("0");
        /* Assume we got a single episode or film -> Crawl all mirrors */
        /* Check for offline status */
        if (br.containsHTML("Diese Episode wurde noch nicht veröffentlicht")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Diese Episode wurde noch nicht veröffentlicht");
        } else if (br.containsHTML(">\\s*😰 Kein Videoplayer für diese Episode verfügbar|>\\s*Bitte schaue später noch einmal vorbei oder melde dieses Problem")) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, "Kein Videoplayer für diese Episode verfügbar");
        }
        final Set<String> dupes = new HashSet<String>();
        final List<String> allRedirectURLs = new ArrayList<String>();
        /* Collect and group mirrors */
        String[] newsite_languages = null;
        String[] newsite_language_ids = null;
        final Map<String, List<String>> redirectURLsByHoster = new HashMap<String, List<String>>();
        final Map<String, List<String>> redirectURLsByLanguageID = new HashMap<String, List<String>>();
        final String[] episodeMirrorsHTMLs = br.getRegex("<li class=\"[^\"]*episodeLink\\d+\"(.*?)</a>").getColumn(0);
        if (episodeMirrorsHTMLs != null && episodeMirrorsHTMLs.length != 0) {
            /* Old website (aniworld) */
            for (final String episodeHTML : episodeMirrorsHTMLs) {
                final String redirectURL = new Regex(episodeHTML, "href=\"([^\"]+redirect[^\"]+)\" target=\"_blank\"").getMatch(0);
                final String languageID = new Regex(episodeHTML, "data-lang-key=\"(\\d+)\"").getMatch(0);
                final String hoster = new Regex(episodeHTML, "title=\"Hoster ([^\"]+)\"").getMatch(0).toLowerCase(Locale.ROOT);
                if (redirectURL == null || languageID == null || hoster == null) {
                    logger.warning("Something is null: redirectURL =" + redirectURL + " | languageKey = " + languageID + " | hoster = " + hoster);
                    /* Skip invalid items */
                    continue;
                }
                if (!dupes.add(redirectURL)) {
                    /* Skip duplicates */
                    continue;
                }
                /* Update language-packages */
                if (redirectURLsByLanguageID.containsKey(languageID)) {
                    redirectURLsByLanguageID.get(languageID).add(redirectURL);
                } else {
                    final List<String> newList = new ArrayList<String>();
                    newList.add(redirectURL);
                    redirectURLsByLanguageID.put(languageID, newList);
                }
                /* Update hoster packages */
                if (redirectURLsByHoster.containsKey(hoster)) {
                    redirectURLsByHoster.get(hoster).add(redirectURL);
                } else {
                    final List<String> newList = new ArrayList<String>();
                    newList.add(redirectURL);
                    redirectURLsByHoster.put(hoster, newList);
                }
                allRedirectURLs.add(redirectURL);
            }
        } else {
            /* New website */
            newsite_languages = br.getRegex("data-language-label=\"([^\"]+)").getColumn(0);
            newsite_language_ids = br.getRegex("data-language-id=\"(\\d+)").getColumn(0);
            final String[] newsite_redirect_urls = br.getRegex("data-play-url=\"(/r[^\"]+)").getColumn(0);
            final String[] newsite_hosters = br.getRegex("data-provider-name=\"([^\"]+)").getColumn(0);
            if (newsite_languages == null || newsite_languages.length == 0 || newsite_language_ids == null || newsite_redirect_urls == null || newsite_hosters == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            final int len = newsite_languages.length;
            /* Ensure that all lists are the same size */
            if (newsite_language_ids.length != len || newsite_redirect_urls.length != len || newsite_hosters.length != len) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            for (int i = 0; i < len; i++) {
                final String redirectURL = newsite_redirect_urls[i];
                final String languageID = newsite_language_ids[i];
                String hoster = newsite_hosters[i];
                if (redirectURL == null || languageID == null || hoster == null) {
                    logger.warning("Something is null: redirectURL =" + redirectURL + " | languageKey = " + languageID + " | hoster = " + hoster);
                    /* Skip invalid items */
                    continue;
                }
                if (!dupes.add(redirectURL)) {
                    /* Skip duplicates */
                    continue;
                }
                hoster = hoster.toLowerCase(Locale.ROOT);
                /* Update language-packages */
                if (redirectURLsByLanguageID.containsKey(languageID)) {
                    redirectURLsByLanguageID.get(languageID).add(redirectURL);
                } else {
                    final List<String> newList = new ArrayList<String>();
                    newList.add(redirectURL);
                    redirectURLsByLanguageID.put(languageID, newList);
                }
                /* Update hoster packages */
                if (redirectURLsByHoster.containsKey(hoster)) {
                    redirectURLsByHoster.get(hoster).add(redirectURL);
                } else {
                    final List<String> newList = new ArrayList<String>();
                    newList.add(redirectURL);
                    redirectURLsByHoster.put(hoster, newList);
                }
                allRedirectURLs.add(redirectURL);
            }
        }
        if (allRedirectURLs.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        logger.info("Found links total: " + allRedirectURLs.size() + " | Hosters: " + redirectURLsByHoster.size() + " | Languages: " + redirectURLsByLanguageID.size());
        /* Some items need to stay in order but we don't want duplicates -> Use LinkedHashSet */
        final Set<String> userLanguageIDsPrioList = new LinkedHashSet<String>();
        final Set<String> userHosterPrioList = new LinkedHashSet<String>();
        /* Collect language name -> ID mapping if needed */
        final String userLanguagePrioListStr = cfg.getLanguagePriorityString();
        final Map<String, String> languageIdTitleMap = new HashMap<String, String>();
        findUserPreferredAndExistingLanguages: if (userLanguagePrioListStr != null) {
            /* Find existing languages and their internal IDs */
            final List<String> userAllowedLanguageTitles = new ArrayList<String>();
            userAllowedLanguageTitles.addAll(Arrays.asList(userLanguagePrioListStr.split(",")));
            final String languageFlagsHTML = br.getRegex("<div class=\"changeLanguage\">(.*?)</div>").getMatch(0);
            if (languageFlagsHTML != null) {
                /* Old website */
                final String[][] languageTitleIDMappings = new Regex(languageFlagsHTML, "<img[^>]*data-lang-key=\"(\\d+)\" title=\"([^\"]+)\"").getMatches();
                if (languageTitleIDMappings == null || languageTitleIDMappings.length == 0) {
                    logger.warning("Failed to find any languageTitleIDMappings");
                    break findUserPreferredAndExistingLanguages;
                }
                /* Put languageTitleIDMappings into HashMap */
                for (final String[] mapping : languageTitleIDMappings) {
                    languageIdTitleMap.put(mapping[0], mapping[1]);
                }
            } else {
                /* New website */
                final String[] languages = br.getRegex("data-language-label=\"([^\"]+)").getColumn(0);
                final String[] language_ids = br.getRegex("data-language-id=\"(\\d+)").getColumn(0);
                if (languages == null || languages.length == 0 || language_ids == null || language_ids.length == 0) {
                    logger.warning("Failed to find available languages #0");
                    break findUserPreferredAndExistingLanguages;
                } else if (languages.length != language_ids.length) {
                    logger.warning("Failed to find available languages #1");
                    break findUserPreferredAndExistingLanguages;
                }
                for (int i = 0; i < languages.length; i++) {
                    languageIdTitleMap.put(language_ids[i], languages[i]);
                }
            }
            if (languageIdTitleMap.isEmpty()) {
                logger.warning("Failed to find available languages");
                break findUserPreferredAndExistingLanguages;
            }
            /* List of languageIDs created by weaker checks -> To be used as fallback */
            final Set<String> userLanguageIDsPrioListGreedy = new LinkedHashSet<String>();
            /* Iterate through user preferred languages */
            for (final String userPreferredLanguageTitle : userAllowedLanguageTitles) {
                final String[] userPreferredLanguageTitleMatcher = userPreferredLanguageTitle.split("\\s+");
                final Iterator<String> languageIdIterator = languageIdTitleMap.keySet().iterator();
                while (languageIdIterator.hasNext()) {
                    final String language_id = languageIdIterator.next();
                    final String language_title_text = languageIdTitleMap.get(language_id);
                    final String[] language_titles = language_title_text.split("/"); // Covers strings like "Deutsch/German"
                    userPreferredLanguageTitleMatcherLoop: for (final String userPreferredLanguageTitleMatch : userPreferredLanguageTitleMatcher) {
                        if (language_titles != null && language_titles.length > 1) {
                            for (final String language_title : language_titles) {
                                if (StringUtils.equalsIgnoreCase(language_title, userPreferredLanguageTitleMatch)) {
                                    /* Precise match */
                                    userLanguageIDsPrioList.add(language_id);
                                    logger.info("Found precise language match: " + language_title);
                                    break userPreferredLanguageTitleMatcherLoop;
                                }
                            }
                        }
                        if (StringUtils.equalsIgnoreCase(language_title_text, userPreferredLanguageTitleMatch)) {
                            /* Precise match */
                            logger.info("Found precise language match: " + language_title_text);
                            userLanguageIDsPrioList.add(language_id);
                            break userPreferredLanguageTitleMatcherLoop;
                        } else if (StringUtils.containsIgnoreCase(language_title_text, userPreferredLanguageTitleMatch)) {
                            /* Collect greedy matches so we can use them as a fallback later. */
                            userLanguageIDsPrioListGreedy.add(language_id);
                            break userPreferredLanguageTitleMatcherLoop;
                        }
                    }
                }
            }
            if (userLanguageIDsPrioList.size() == 0 && userLanguageIDsPrioListGreedy.size() > 0) {
                logger.info("Failed to find precise language matches -> Fallback to greedy list: " + userLanguageIDsPrioListGreedy);
                userLanguageIDsPrioList.addAll(userLanguageIDsPrioListGreedy);
            }
            logger.info("Found " + userLanguageIDsPrioList.size() + " user preferred languages: " + userLanguageIDsPrioList);
        }
        String userHosterPrioListStr = cfg.getHosterPriorityString();
        if (!StringUtils.isEmpty(userHosterPrioListStr)) {
            userHosterPrioListStr = userHosterPrioListStr.replace(" ", "").toLowerCase(Locale.ROOT);
            userHosterPrioList.addAll(Arrays.asList(userHosterPrioListStr.split(",")));
        }
        /* Now check which URLs/mirrors our user prefers --> If configured properly, user will need to enter less captchas this way */
        List<String> urlsToProcess = new ArrayList<String>();
        if (userHosterPrioList.size() > 0) {
            /* Get user preferred mirrors by host (+ language if desired) */
            final List<String> mirrorsByHost = new ArrayList<String>();
            final List<String> mirrorsByHostAndLanguage = new ArrayList<String>();
            String chosenPrioHoster = null;
            String chosenPrioLanguage = null;
            findMirrorMatchingPrioHosterAndLanguage: for (final String allowedHoster : userHosterPrioList) {
                final List<String> thisMirrorsByHost = redirectURLsByHoster.get(allowedHoster);
                if (thisMirrorsByHost == null || thisMirrorsByHost.size() == 0) {
                    continue;
                }
                if (mirrorsByHost.isEmpty()) {
                    mirrorsByHost.addAll(thisMirrorsByHost);
                    chosenPrioHoster = allowedHoster;
                }
                /* Combine this with users' language priority if given -> */
                for (final String allowedLanguageID : userLanguageIDsPrioList) {
                    final List<String> preferredMirrorsByLanguage = redirectURLsByLanguageID.get(allowedLanguageID);
                    if (preferredMirrorsByLanguage == null) {
                        continue;
                    }
                    /* Collect all links from preferred hoster && preferred language */
                    for (final String preferredMirrorByHost : thisMirrorsByHost) {
                        if (preferredMirrorsByLanguage.contains(preferredMirrorByHost)) {
                            mirrorsByHostAndLanguage.add(preferredMirrorByHost);
                        }
                    }
                    chosenPrioHoster = allowedHoster;
                    chosenPrioLanguage = languageIdTitleMap.get(allowedLanguageID);
                    /* Early return */
                    break findMirrorMatchingPrioHosterAndLanguage;
                }
            }
            if (mirrorsByHostAndLanguage.size() > 0) {
                logger.info("Found " + mirrorsByHostAndLanguage.size() + " user priorized mirrors by host " + chosenPrioHoster + " and language " + chosenPrioLanguage);
                urlsToProcess.addAll(mirrorsByHostAndLanguage);
            } else if (mirrorsByHost.size() > 0) {
                logger.info("Found " + mirrorsByHost.size() + " user priorized mirrors by host " + chosenPrioHoster);
                urlsToProcess.addAll(mirrorsByHost);
            } else {
                logger.info("Failed to find user priorized mirrors by host prio: " + userHosterPrioList + " | language prio: " + userLanguageIDsPrioList);
            }
        } else if (!userLanguageIDsPrioList.isEmpty()) {
            /* Get user preferred mirrors by language only (ALL mirrors for preferred language) */
            for (final String languageKey : userLanguageIDsPrioList) {
                if (redirectURLsByLanguageID.containsKey(languageKey)) {
                    urlsToProcess = redirectURLsByLanguageID.get(languageKey);
                }
            }
            if (urlsToProcess.size() > 0) {
                logger.info("Found " + urlsToProcess.size() + " user priorized mirrors by language");
            } else {
                logger.info("Failed to find user priorized mirrors by language prio: " + userLanguageIDsPrioList);
            }
        }
        /* Check if user wished mirrors were found */
        if (urlsToProcess.size() > 0) {
            logger.info("Crawling " + urlsToProcess.size() + "/" + allRedirectURLs.size() + " URLs");
        } else {
            /* Fallback: Users' settings would have filtered all items -> Return all items instead */
            logger.info("Crawling ALL URLs: " + allRedirectURLs.size());
            urlsToProcess = allRedirectURLs;
        }
        final DownloadLink parent = param.getDownloadLink();
        final FilePackage fp = FilePackage.getInstance();
        String seasonHumanReadable = null;
        if (seasonNumberStrFromURL != null) {
            if (seasonNumberStrFromURL.equalsIgnoreCase("filme")) {
                seasonHumanReadable = "Filme";
            } else {
                seasonHumanReadable = "S" + seasonNumberStrFromURL;
            }
        }
        if (parent != null && parent.hasProperty(PROPERTY_IS_PART_OF_COMPLETE_SEASON_CRAWLING)) {
            fp.setName(seriesTitle + " " + seasonHumanReadable);
        } else if (episodeTitle != null) {
            fp.setName(seriesTitle + " " + episodeTitle);
        } else {
            /* Fallback: Set series title only */
            fp.setName(seriesTitle + " E" + episodeNumberStr);
        }
        fp.setPackageKey(getHost() + "/series/" + seriesTitleSlug + "/season/" + seasonNumberStrFromURL);
        fp.setAllowMerge(true);
        fp.setAllowInheritance(true);
        int index = -1;
        for (String redirectURL : urlsToProcess) {
            index++;
            logger.info("Working on item " + (index + 1) + "/" + urlsToProcess.size());
            final Browser br2 = br.cloneBrowser();
            final DownloadLink link = crawlSingleRedirect(br2.cloneBrowser(), redirectURL);
            link._setFilePackage(fp);
            ret.add(link);
            distribute(link);
            if (this.isAbort()) {
                throw new InterruptedException();
            }
        }
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    /** Crawls all episodes of season which is present in html code of current public browser instance. */
    private ArrayList<DownloadLink> crawlCurrentSeason(final CryptedLink param, final String contenturl, final String seriesTitleSlug, final String seasonNumberStr) throws PluginException, InterruptedException, DecrypterException, IOException {
        final Set<String> dupes = new HashSet<String>();
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        final boolean isNewWebsite = new Regex(contenturl, TYPE_SERIES_NEW).patternFind();
        final String[] urls;
        final String baseurl;
        if ("filme".equalsIgnoreCase(seasonNumberStr)) {
            urls = br.getRegex(Pattern.quote(seriesTitleSlug) + "/filme/film-\\d+").getColumn(-1);
            baseurl = "/anime/stream/";
        } else {
            urls = br.getRegex(Pattern.quote(seriesTitleSlug) + "/staffel-" + seasonNumberStr + "/episode-\\d+").getColumn(-1);
            if (isNewWebsite) {
                baseurl = "/serie/";
            } else {
                baseurl = "/anime/stream/";
            }
        }
        if (urls == null || urls.length == 0) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        int skippedNotYetReleasedItems = 0;
        for (String path : urls) {
            path = baseurl + path;
            final boolean isTBA = br.containsHTML("class=\"episode-row upcoming\\s*\"\\s*onclick=\"window\\.location='" + Pattern.quote(path));
            final String url = br.getURL(path).toExternalForm();
            if (!dupes.add(url)) {
                /* Avoid duplicates */
                continue;
            }
            if (isTBA) {
                logger.info("Skipping TBA item: " + url);
                skippedNotYetReleasedItems++;
                continue;
            }
            final DownloadLink link = createDownloadlink(url);
            link.setProperty(PROPERTY_IS_PART_OF_COMPLETE_SEASON_CRAWLING, true);
            ret.add(link);
        }
        logger.info("Crawled season " + seasonNumberStr + " | Found episodes: " + ret.size() + " | Skipped not yet released items: " + skippedNotYetReleasedItems);
        /* Log edge case */
        if (ret.isEmpty() && skippedNotYetReleasedItems > 0) {
            logger.warning("Looks like all episodes of this season are offline");
        }
        return ret;
    }

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return SerienStreamToConfig.class;
    }
}