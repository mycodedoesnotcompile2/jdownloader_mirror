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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.appwork.utils.DebugMode;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.AbstractRecaptchaV2;
import org.jdownloader.captcha.v2.challenge.recaptcha.v2.CaptchaHelperCrawlerPluginRecaptchaV2;
import org.jdownloader.plugins.components.config.SerienStreamToConfig;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginJsonConfig;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterException;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 52218 $", interfaceVersion = 3, names = {}, urls = {})
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
    private static final Pattern SERIES_REGEX             = Pattern.compile("([\\w-]+)(/staffel-(\\d+)(/episode-(\\d+))?)?", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_SINGLE_STREAM_OLD   = Pattern.compile("/anime/stream/" + SERIES_REGEX.pattern(), Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_SINGLE_STREAM_NEW   = Pattern.compile("/serie/" + SERIES_REGEX.pattern(), Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_SINGLE_REDIRECT_OLD = Pattern.compile("/redirect/(\\d+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_SINGLE_REDIRECT_NEW = Pattern.compile("/r\\?t=(ey[a-zA-Z0-9_/\\+\\=\\-%]+)", Pattern.CASE_INSENSITIVE);

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
                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && !true) {
                    // TODO: Finish implementation of "crawl all episodes of season", then remove this IDE-only check
                    ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_SINGLE_STREAM_NEW.pattern() + "|" + TYPE_SINGLE_REDIRECT_NEW.pattern() + ")");
                } else {
                    ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_SINGLE_STREAM_NEW.pattern() + "|[\\w-]+/staffel-\\d+/episode-\\d+)");
                }
            } else {
                /* aniworld domains */
                ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_SINGLE_STREAM_OLD.pattern() + "|" + TYPE_SINGLE_REDIRECT_OLD.pattern() + ")");
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
            ret.add(this.crawlSingleRedirect(contenturl));
            return ret;
        } else {
            return this.crawlMirrors(contenturl);
        }
    }

    private DownloadLink crawlSingleRedirect(String url) throws PluginException, InterruptedException, DecrypterException, IOException {
        /* Enforce https */
        url = url.replaceFirst("^(?i)http://", "https://");
        final Browser br = this.br.cloneBrowser();
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
            final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br, sitekey) {
                @Override
                public TYPE getType() {
                    return TYPE.INVISIBLE;
                }
            }.getToken();
            captcha.put("original", "");
            captcha.put("token", Encoding.urlEncode(recaptchaV2Response));
            redirectPage = br.submitForm(captcha);
            finallink = br.getURL();
        }
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (finallink == null || this.canHandle(finallink)) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        } else if (Browser.getHost(finallink, true).equalsIgnoreCase(initialHost)) {
            /* E.g. redirect to mainpage */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        return createDownloadlink(finallink);
    }

    private ArrayList<DownloadLink> crawlMirrors(final String contenturl) throws PluginException, InterruptedException, DecrypterException, IOException {
        br.setFollowRedirects(true);
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Set<String> dupes = new HashSet<String>();
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String titleOfEpisodeOrMovie = br.getRegex("class=\"episodeGermanTitle\"[^>]*>([^<]+)</span>").getMatch(0);
        if (titleOfEpisodeOrMovie != null) {
            titleOfEpisodeOrMovie = Encoding.htmlDecode(titleOfEpisodeOrMovie).trim();
        }
        String seriesTitle = br.getRegex("<meta property=\"og:title\" content=\"(?:Episode\\s*\\d+\\s|Staffel\\s*\\d+\\s|Filme?\\s*\\d*\\s|von\\s)+([^\"]+)\"/>").getMatch(0);
        final Regex urlinfo_series = new Regex(br._getURL().getPath(), SERIES_REGEX.pattern() + "$");
        final Regex urlinfo_movies = new Regex(br.getURL(), "https?://[^/]+/([^/]+)/([^/]+)/([^/]+)/filme(/film-(\\d+))?");
        final boolean isSeriesMoviesLink = urlinfo_movies.patternFind();
        final String seriesTitleSlug = urlinfo_series.getMatch(0);
        String seriesSeasonNumberStr = urlinfo_series.getMatch(2);
        if (seriesSeasonNumberStr == null) {
            /*
             * No season slug given -> Use season 1 as default to get same results as website which displays episodes of season 1 if no
             * season is given in URL.
             */
            seriesSeasonNumberStr = "1";
        }
        final String filmNumberStr = urlinfo_movies.getMatch(4);
        final String seriesSeasonEpisodeNumberStr = urlinfo_series.getMatch(4);
        if (seriesTitle == null && seriesTitleSlug != null) {
            logger.warning("Failed to find title -> Using fallback: " + seriesTitleSlug);
            seriesTitle = seriesTitleSlug.replace("-", " ").trim();
        }
        if (seriesTitle != null) {
            seriesTitle = Encoding.htmlDecode(seriesTitle).trim();
        }
        /* Videos are on external sites (not in embeds), so harvest those if we can get our hands on them. */
        final String[] episodeMirrorsHTMLs = br.getRegex("<li class=\"[^\"]*episodeLink\\d+\"(.*?)</a>").getColumn(0);
        if (filmNumberStr == null && urlinfo_movies.patternFind()) {
            /* Find all films of a series */
            // TODO: Check if such film links still exist
            final String[][] filmLinks = br.getRegex("href=\"([^\"]+" + Pattern.quote(seriesTitleSlug) + "/filme/film-\\d+)\"").getMatches();
            if (filmLinks == null || filmLinks.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            for (final String[] filmLink : filmLinks) {
                final String url = br.getURL(Encoding.htmlDecode(filmLink[0])).toExternalForm();
                if (dupes.add(url)) {
                    ret.add(createDownloadlink(url));
                }
            }
            logger.info("Found " + ret.size() + " films of series: " + seriesTitleSlug);
            /* Early return */
            return ret;
        }
        crawlEpisodesOfSeason: if (seriesSeasonEpisodeNumberStr == null && filmNumberStr == null) {
            /* No specific episode/film -> Crawl all episodes of a series */
            final String[][] itemLinks = br.getRegex("href=\"([^\"]+" + Pattern.quote(seriesTitleSlug) + "/staffel-" + seriesSeasonNumberStr + "/[^\"]+)\"").getMatches();
            if (itemLinks == null || itemLinks.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            for (final String[] itemLink : itemLinks) {
                final String url = br.getURL(Encoding.htmlDecode(itemLink[0])).toExternalForm();
                if (dupes.add(url)) {
                    ret.add(createDownloadlink(url));
                }
            }
            logger.info("Found " + ret.size() + " episodes of season: " + seriesSeasonNumberStr);
            return ret;
        }
        /* Assume we got a single episode or film -> Crawl all mirrors */
        final List<String> allRedirectURLs = new ArrayList<String>();
        /* Collect and group mirrors */
        String[] newsite_languages = null;
        String[] newsite_language_ids = null;
        final Map<String, List<String>> redirectURLsByHoster = new HashMap<String, List<String>>();
        final Map<String, List<String>> redirectURLsByLanguageID = new HashMap<String, List<String>>();
        if (episodeMirrorsHTMLs != null && episodeMirrorsHTMLs.length != 0) {
            for (final String episodeHTML : episodeMirrorsHTMLs) {
                final String redirectURL = new Regex(episodeHTML, "(?i)href=\"([^\"]+redirect[^\"]+)\" target=\"_blank\"").getMatch(0);
                final String languageID = new Regex(episodeHTML, "(?i)data-lang-key=\"(\\d+)\"").getMatch(0);
                final String hoster = new Regex(episodeHTML, "(?i)title=\"Hoster ([^\"]+)\"").getMatch(0).toLowerCase(Locale.ENGLISH);
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
                final String hoster = newsite_hosters[i];
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
        }
        if (allRedirectURLs.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        logger.info("Found links total: " + allRedirectURLs.size() + " | Hosters: " + redirectURLsByHoster.size() + " | Languages: " + redirectURLsByLanguageID.size());
        /* Some items need to stay in order but we don't want duplicates -> Use LinkedHashSet */
        final Set<String> userLanguageIDsPrioList = new LinkedHashSet<String>();
        final Set<String> userHosterPrioList = new LinkedHashSet<String>();
        /* Collect language name -> ID mapping if needed */
        final String userLanguagePrioListStr = PluginJsonConfig.get(SerienStreamToConfig.class).getLanguagePriorityString();
        findUserPreferredLanguages: if (userLanguagePrioListStr != null) {
            /* Find existing languages and their internal IDs */
            final List<String> userAllowedLanguageTitles = new ArrayList<String>();
            userAllowedLanguageTitles.addAll(Arrays.asList(userLanguagePrioListStr.split(",")));
            final String languageFlagsHTML = br.getRegex("<div class=\"changeLanguage\">(.*?)</div>").getMatch(0);
            final Map<String, String> languageIdTitleMap = new HashMap<String, String>();
            if (languageFlagsHTML != null) {
                /* Old website */
                final String[][] languageTitleIDMappings = new Regex(languageFlagsHTML, "<img[^>]*data-lang-key=\"(\\d+)\" title=\"([^\"]+)\"").getMatches();
                if (languageTitleIDMappings == null || languageTitleIDMappings.length == 0) {
                    logger.warning("Failed to find any languageTitleIDMappings");
                    break findUserPreferredLanguages;
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
                    break findUserPreferredLanguages;
                } else if (languages.length != language_ids.length) {
                    logger.warning("Failed to find available languages #1");
                    break findUserPreferredLanguages;
                }
                for (int i = 0; i < languages.length; i++) {
                    languageIdTitleMap.put(language_ids[i], languages[i]);
                }
            }
            if (languageIdTitleMap.isEmpty()) {
                logger.warning("Failed to find available languages");
                break findUserPreferredLanguages;
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
        String userHosterPrioListStr = PluginJsonConfig.get(SerienStreamToConfig.class).getHosterPriorityString();
        if (!StringUtils.isEmpty(userHosterPrioListStr)) {
            userHosterPrioListStr = userHosterPrioListStr.replace(" ", "").toLowerCase(Locale.ENGLISH);
            userHosterPrioList.addAll(Arrays.asList(userHosterPrioListStr.split(",")));
        }
        /* Now check which URLs/mirrors our user prefers --> If configured properly, user will need to enter less captchas this way */
        List<String> urlsToProcess = null;
        if (userHosterPrioList.size() > 0) {
            /* Get user preferred mirrors by host (+ language if desired) */
            // TODO: Refactor this to make it easier to understand
            for (final String userAllowedHoster : userHosterPrioList) {
                if (urlsToProcess != null && urlsToProcess.size() > 0) {
                    /* Stop early if preferred hoster was found */
                    break;
                }
                final List<String> preferredMirrorsByHost = redirectURLsByHoster.get(userAllowedHoster);
                if (preferredMirrorsByHost == null || preferredMirrorsByHost.size() == 0) {
                    continue;
                }
                logger.info("Found user priorized mirrors by host:" + userAllowedHoster);
                /* Combine this with users' language priority if given -> */
                for (final String languageKey : userLanguageIDsPrioList) {
                    final List<String> preferredMirrorsByLanguage = redirectURLsByLanguageID.get(languageKey);
                    if (preferredMirrorsByLanguage != null) {
                        /* Collect all links from preferred hoster && preferred language */
                        logger.info("Found perfect match by host + language:" + userAllowedHoster + "|" + languageKey);
                        urlsToProcess = new ArrayList<String>();
                        for (final String preferredMirrorByHost : preferredMirrorsByHost) {
                            if (preferredMirrorsByLanguage.contains(preferredMirrorByHost)) {
                                urlsToProcess.add(preferredMirrorByHost);
                            }
                        }
                        break;
                    } else {
                        /* Set fallback value (preference by mirror only, not by language). */
                        urlsToProcess = preferredMirrorsByHost;
                    }
                }
            }
            if (urlsToProcess == null) {
                logger.info("Failed to find user priorized mirrors by host: " + userHosterPrioList);
            }
        } else if (!userLanguageIDsPrioList.isEmpty()) {
            /* Get user preferred mirrors by language only (ALL mirrors for preferred language) */
            for (final String languageKey : userLanguageIDsPrioList) {
                if (redirectURLsByLanguageID.containsKey(languageKey)) {
                    urlsToProcess = redirectURLsByLanguageID.get(languageKey);
                }
            }
            if (urlsToProcess != null) {
                logger.info("Found user priorized mirrors by language");
            } else {
                logger.info("Failed to find user priorized mirrors by language");
            }
        }
        /* Check if user wished mirrors were found */
        if (urlsToProcess != null) {
            logger.info("Crawling " + urlsToProcess.size() + "/" + allRedirectURLs.size() + " URLs");
        } else {
            /* Fallback: Users' settings would have filtered all items -> Return all items instead */
            logger.info("Crawling ALL URLs: " + allRedirectURLs.size());
            urlsToProcess = allRedirectURLs;
        }
        final FilePackage fp;
        if (seriesTitle != null) {
            fp = FilePackage.getInstance();
            if (isSeriesMoviesLink) {
                /* We are crawling a single- or all "movie episodes" of a series -> Set name of movie as package name if possible */
                if (filmNumberStr != null && titleOfEpisodeOrMovie != null) {
                    fp.setName(seriesTitle + " - " + titleOfEpisodeOrMovie);
                    fp.setPackageKey(getHost() + "/series/" + seriesTitleSlug + "/film/" + filmNumberStr);
                } else {
                    /* Fallback or all films of a series */
                    fp.setName(seriesTitle + " S0");
                    fp.setPackageKey(getHost() + "/series/" + seriesTitleSlug + "/films");
                }
            } else {
                /* Single series episode or all episodes of a series */
                fp.setName(seriesTitle + " S" + seriesSeasonNumberStr);
                fp.setPackageKey(getHost() + "/series/" + seriesTitleSlug + "/season/" + seriesSeasonNumberStr);
            }
            fp.setAllowMerge(true);
            fp.setAllowInheritance(true);
        } else {
            fp = null;
        }
        int index = -1;
        for (String redirectURL : urlsToProcess) {
            index++;
            logger.info("Working on item " + index + "/" + urlsToProcess.size());
            final Browser br2 = br.cloneBrowser();
            br2.setFollowRedirects(false);
            redirectURL = br.getURL(redirectURL).toExternalForm();
            String redirectPage = br2.getPage(redirectURL);
            if (br2.getRedirectLocation() != null) {
                redirectURL = br2.getRedirectLocation();
            } else if (br2.containsHTML("grecaptcha")) {
                final Form captcha = br2.getForm(0);
                if (captcha == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
                }
                final String sitekey = new Regex(redirectPage, "grecaptcha\\.execute\\('([^']+)'").getMatch(0);
                final String recaptchaV2Response = new CaptchaHelperCrawlerPluginRecaptchaV2(this, br2, sitekey) {
                    @Override
                    public TYPE getType() {
                        return TYPE.INVISIBLE;
                    }
                }.getToken();
                captcha.put("original", "");
                captcha.put("token", Encoding.urlEncode(recaptchaV2Response));
                try {
                    redirectPage = br2.submitForm(captcha);
                } catch (IOException e) {
                    logger.log(e);
                }
                redirectURL = br2.getURL();
            }
            final DownloadLink link = createDownloadlink(redirectURL);
            if (fp != null) {
                fp.add(link);
            }
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

    @Override
    public Class<? extends PluginConfigInterface> getConfigInterface() {
        return SerienStreamToConfig.class;
    }
}