//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.decrypter;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import javax.imageio.ImageIO;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Regex;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickedPoint;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.plugins.CaptchaException.CaptchaRefreshException;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;

@DecrypterPlugin(revision = "$Revision: 53899 $", interfaceVersion = 2, names = {}, urls = {})
public class JustPasteIt extends AbstractPastebinCrawler {
    public JustPasteIt(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        br.setLoadLimit(3 * br.getLoadLimit());
        br.setAllowedResponseCodes(451);
        return br;
    }

    @Override
    String getFID(final String url) {
        return new Regex(url, this.getSupportedLinks()).getMatch(0);
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        // each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain
        ret.add(new String[] { "justpaste.it", "jpst.it" });
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
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/([A-Za-z0-9]+)");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public PastebinMetadata crawlMetadata(final CryptedLink param, final Browser br) throws Exception {
        final String contenturl = param.getCryptedUrl();
        final String contentID = this.getFID(contenturl);
        final boolean contentIDContainsNumbers = !contentID.replaceAll("\\d", "").equals(contentID);
        if (contentID.toLowerCase(Locale.ENGLISH).equals(contentID) && !contentIDContainsNumbers) {
            /* Lowercase contentID without numbers -> Invalid e.g.: https://justpaste.it/about */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 403) {
            /**
             * e.g. <div class="card-header alert-danger"><strong>Article unavailable</strong></div> <div class="card-body"> Article has
             * been removed by the author
             */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (br.getHttpConnection().getResponseCode() == 451) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        } else if (!this.canHandle(br.getURL())) {
            /* Redirect to unsupported URL e.g. mainpage, example: https://jpst.it/xxxTestYyy */
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String json = br.getRegex("window\\.barOptions\\s*=\\s*(\\{.*?\\});").getMatch(0);
        Map<String, Object> entries = null;
        try {
            entries = restoreFromString(json, TypeRef.MAP);
        } catch (final Throwable e) {
            logger.log(e);
            logger.info("Failed to parse pastebin json");
        }
        final String pastebinText;
        if (br.containsHTML("\"isCaptchaRequired\":true") && !br.containsHTML("\"isCaptchaEntered\":true")) {
            if (entries == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            logger.info("Captcha required");
            final String captchaTestID = solveClickCaptcha(param, br, (Map<String, Object>) entries.get("captchaSettings"));
            pastebinText = crawlArticleContentAfterCaptcha(br, captchaTestID, (String) entries.get("linkSharingSecret"));
        } else {
            pastebinText = br.getRegex("<div[^>]+id=\"articleContent\"[^>]*>(.*?)</div>").getMatch(0);
        }
        if (pastebinText == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final PastebinMetadata metadata = new PastebinMetadata(param, contentID);
        if (entries != null) {
            /* TODO: Add support for password protected items, 2026-11-04: Only premium users can create such links. */
            if (Boolean.TRUE.equals(entries.get("isPasswordProtected"))) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Password protected items are not yet supported");
            }
            /* Format e.g. "2023-04-20T19:02:54+00:00" -> remove the colon in the timezone offset so that pattern "Z" can parse it. */
            final String createdText = entries.get("createdText").toString().replaceFirst("([+-]\\d{2}):(\\d{2})$", "$1$2");
            final long createdTimestamp = TimeFormatter.getMilliSeconds(createdText, "yyyy-MM-dd'T'HH:mm:ssZ", Locale.ENGLISH);
            if (createdTimestamp > 0) {
                metadata.setDate(new Date(createdTimestamp));
            } else {
                logger.warning("Failed to parse createdText: " + createdText);
            }
        }
        metadata.setPastebinText(pastebinText);
        // metadata.setUsername("@anonymous");
        final String title = br.getRegex("class=\"articleFirstTitle\"[^>]*>([^<>\"]+)</h1>").getMatch(0);
        if (title != null) {
            metadata.setTitle(title);
        } else {
            logger.warning("Failed to find pastebin title");
        }
        return metadata;
    }

    private static final int CAPTCHA_GRID_SIZE        = 3;
    private static final int CAPTCHA_TILE_SIZE_SCALED = 150;
    private static final int CAPTCHA_TILE_GAP         = 6;
    private static final int CAPTCHA_MIN_CLICKS       = 2;
    private static final int CAPTCHA_MAX_CLICKS       = 6;
    private static final int CAPTCHA_MAX_ATTEMPTS     = 3;
    /*
     * Max. number of captcha refreshes (user clicks refresh button) before the crawl process is stopped with a retry. Same limit as in the
     * browser: There the active challenge can also be refreshed 5 times, after that it cannot be refreshed anymore without reloading the
     * whole page.
     */
    private static final int CAPTCHA_MAX_REFRESHES    = 5;

    /** Sends a JSON POST request and returns the parsed JSON response. */
    private Map<String, Object> postJson(final Browser br, final String url, final Map<String, Object> body) throws Exception {
        final Browser brc = br.cloneBrowser();
        brc.setAllowedResponseCodes(new int[] { 403, 422, 429 });
        brc.getHeaders().put("Accept", "application/json, text/plain, */*");
        brc.getHeaders().put("Content-Type", "application/json");
        brc.getHeaders().put("X-Requested-With", "XMLHttpRequest");
        brc.postPageRaw(url, JSonStorage.serializeToJson(body));
        return restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
    }

    /**
     * Solves the click captcha of justpaste.it. </br>
     * 1. Loads the captcha image(s). 2. Merges them to one image. 3. Asks the user to click the matching images. 4. Sends the answer.
     *
     * @return ID of the solved captcha test which is needed to access the article.
     */
    private String solveClickCaptcha(final CryptedLink param, final Browser br, final Map<String, Object> captchaSettings) throws Exception {
        final String createTestUrl = captchaSettings.get("createTestUrl").toString();
        final String nextTestUrl = captchaSettings.get("nextTestUrl").toString();
        final String verifyTestCaseUrl = captchaSettings.get("verifyTestCaseUrl").toString();
        logger.info("Creating captcha test: " + createTestUrl);
        Map<String, Object> test = postJson(br, createTestUrl, new HashMap<String, Object>());
        int refreshCount = 0;
        for (int attempt = 1; attempt <= CAPTCHA_MAX_ATTEMPTS; attempt++) {
            if (this.isAbort()) {
                throw new InterruptedException();
            }
            final String testID = test.get("id").toString();
            final Map<String, Object> testCase = (Map<String, Object>) test.get("testCase");
            final Map<String, Object> cryptoChallenge = (Map<String, Object>) testCase.get("cryptoChallenge");
            final Map<String, Object> imageChallenge = (Map<String, Object>) testCase.get("imageChallenge");
            final String correctWord = imageChallenge.get("correctWord").toString();
            final String imageUrl = imageChallenge.get("imageUrl").toString();
            logger.info("Captcha attempt " + attempt + "/" + CAPTCHA_MAX_ATTEMPTS + " | testID: " + testID + " | word: " + correctWord);
            final String cryptoAnswer = generateCryptoAnswer(cryptoChallenge.get("source").toString(), cryptoChallenge.get("salt").toString(), ((Number) cryptoChallenge.get("iterations")).intValue());
            logger.info("Solved crypto challenge: " + cryptoAnswer);
            final File captchaFile = getClickCaptchaImage(br, imageUrl);
            final MultiClickCaptchaChallenge challenge = new MultiClickCaptchaChallenge(captchaFile, "Select all images with: " + correctWord, this, CAPTCHA_MIN_CLICKS, CAPTCHA_MAX_CLICKS);
            /* Ask the user to click the matching images. Result = click positions in pixels of the merged captcha image. */
            final MultiClickedPoint clicks;
            try {
                clicks = handleCaptchaChallenge(challenge);
            } catch (final CaptchaRefreshException e) {
                /*
                 * User clicked the refresh button of the captcha dialog: Show a new captcha instead of failing the whole crawl process. The
                 * old challenge is done and must not be reused. A refresh is no failed attempt so it does not count as one.
                 */
                refreshCount++;
                logger.info("User requested new captcha (refresh " + refreshCount + "/" + CAPTCHA_MAX_REFRESHES + ")");
                if (refreshCount > CAPTCHA_MAX_REFRESHES) {
                    logger.warning("Too many captcha refreshes");
                    throw e;
                }
                test = loadNextCaptchaTest(br, nextTestUrl, testID);
                attempt--;
                continue;
            }
            /*
             * Determine selected images by click positions. Each image can only be selected once: Clicking the same image multiple times
             * counts as one click. Key = image number (1-9), value = how often the user clicked on this image (keeps order of first click).
             */
            final Map<Integer, Integer> clicksPerImage = new LinkedHashMap<Integer, Integer>();
            for (int i = 0; i < clicks.getX().length; i++) {
                /* Convert click position to image number. */
                final int tile = getClickedTile(clicks.getX()[i], clicks.getY()[i]);
                logger.info("Click " + (i + 1) + ": x=" + clicks.getX()[i] + " y=" + clicks.getY()[i] + " -> image " + tile);
                final Integer count = clicksPerImage.get(tile);
                clicksPerImage.put(tile, count == null ? 1 : count + 1);
            }
            /* Log images that were clicked more than once. They are only allowed to be selected once, so they count as one click. */
            for (final Map.Entry<Integer, Integer> entry : clicksPerImage.entrySet()) {
                if (entry.getValue() > 1) {
                    logger.warning("Duplicate clicks: Image " + entry.getKey() + " was clicked " + entry.getValue() + " times -> counts as 1 click");
                }
            }
            /* Number of different images must still reach the minimum, otherwise the server would reject the answer anyway. */
            if (clicksPerImage.size() < CAPTCHA_MIN_CLICKS) {
                /*
                 * Invalid answer: Do not send it. The challenge object must not be reused (it counts as answered), so the next round gets
                 * a completely new captcha test from the server.
                 */
                logger.warning("Invalid answer: Only " + clicksPerImage.size() + " different image(s) selected (" + clicks.getX().length + " clicks), minimum is " + CAPTCHA_MIN_CLICKS + " -> Not sending answer, requesting new captcha");
                if (attempt < CAPTCHA_MAX_ATTEMPTS) {
                    test = loadNextCaptchaTest(br, nextTestUrl, testID);
                }
                continue;
            }
            final Set<Integer> selected = clicksPerImage.keySet();
            final Map<String, Object> imageAnswer = new LinkedHashMap<String, Object>();
            for (int tile = 1; tile <= CAPTCHA_GRID_SIZE * CAPTCHA_GRID_SIZE; tile++) {
                imageAnswer.put(Integer.toString(tile), selected.contains(tile));
            }
            final Map<String, Object> body = new LinkedHashMap<String, Object>();
            body.put("cryptoAnswer", cryptoAnswer);
            body.put("imageAnswer", imageAnswer);
            final Map<String, Object> result = postJson(br, verifyTestCaseUrl.replace("{testId}", testID), body);
            logger.info("Captcha verify response: " + result);
            if (Boolean.TRUE.equals(result.get("success"))) {
                logger.info("Captcha solved successfully");
                return testID;
            }
            logger.info("Captcha answer was wrong");
            if (!Boolean.TRUE.equals(result.get("canSolveTest"))) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            }
            if (!Boolean.TRUE.equals(result.get("canLoadNextTest"))) {
                logger.info("Cannot load next captcha test");
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            }
            if (attempt < CAPTCHA_MAX_ATTEMPTS) {
                test = loadNextCaptchaTest(br, nextTestUrl, testID);
            }
        }
        throw new PluginException(LinkStatus.ERROR_CAPTCHA);
    }

    /** Requests a new captcha test from the server. Needed after every invalid or wrong answer: A used test cannot be answered again. */
    private Map<String, Object> loadNextCaptchaTest(final Browser br, final String nextTestUrl, final String testID) throws Exception {
        logger.info("Requesting next captcha test | previous testID: " + testID);
        final Map<String, Object> next = postJson(br, nextTestUrl.replace("{testId}", testID), new HashMap<String, Object>());
        final Map<String, Object> nextTest = (Map<String, Object>) next.get("test");
        if (nextTest == null) {
            logger.warning("Failed to load next captcha test: " + next);
            throw new PluginException(LinkStatus.ERROR_CAPTCHA);
        }
        return nextTest;
    }

    /** Loads the captcha image(s) and merges them into one new image with a 3x3 grid. */
    private File getClickCaptchaImage(final Browser br, final String imageUrl) throws Exception {
        final Browser img = br.cloneBrowser();
        img.getHeaders().put("Accept", "image/avif,image/webp,image/apng,image/svg+xml,image/*,*/*;q=0.8");
        logger.info("Downloading captcha image 1/1 | " + imageUrl);
        final URLConnectionAdapter con = img.openGetConnection(imageUrl);
        final BufferedImage sprite;
        try {
            sprite = ImageIO.read(con.getInputStream());
        } finally {
            try {
                con.disconnect();
            } catch (final Throwable ignore) {
            }
        }
        if (sprite == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Failed to load captcha image");
        }
        /* The server sends all 9 images as one sprite (3x3 grid, row by row). Split it up, then merge the tiles again with gaps. */
        final int tileWidth = sprite.getWidth() / CAPTCHA_GRID_SIZE;
        final int tileHeight = sprite.getHeight() / CAPTCHA_GRID_SIZE;
        final int total = CAPTCHA_GRID_SIZE * CAPTCHA_TILE_SIZE_SCALED + (CAPTCHA_GRID_SIZE - 1) * CAPTCHA_TILE_GAP;
        final BufferedImage stitched = new BufferedImage(total, total, BufferedImage.TYPE_INT_RGB);
        final Graphics2D g = stitched.createGraphics();
        try {
            g.setColor(Color.WHITE);
            g.fillRect(0, 0, total, total);
            g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
            logger.info("Merging " + (CAPTCHA_GRID_SIZE * CAPTCHA_GRID_SIZE) + " captcha images into one image");
            for (int index = 0; index < CAPTCHA_GRID_SIZE * CAPTCHA_GRID_SIZE; index++) {
                final int col = index % CAPTCHA_GRID_SIZE;
                final int row = index / CAPTCHA_GRID_SIZE;
                final BufferedImage tile = sprite.getSubimage(col * tileWidth, row * tileHeight, tileWidth, tileHeight);
                final int x = col * (CAPTCHA_TILE_SIZE_SCALED + CAPTCHA_TILE_GAP);
                final int y = row * (CAPTCHA_TILE_SIZE_SCALED + CAPTCHA_TILE_GAP);
                g.drawImage(tile, x, y, CAPTCHA_TILE_SIZE_SCALED, CAPTCHA_TILE_SIZE_SCALED, null);
            }
        } finally {
            g.dispose();
        }
        final File output = this.getLocalCaptchaFile(".png");
        ImageIO.write(stitched, "png", output);
        return output;
    }

    /** Returns the number (1-9) of the image at the given position of the merged image. */
    private int getClickedTile(final int x, final int y) {
        final int cell = CAPTCHA_TILE_SIZE_SCALED + CAPTCHA_TILE_GAP;
        final int col = Math.max(0, Math.min(CAPTCHA_GRID_SIZE - 1, x / cell));
        final int row = Math.max(0, Math.min(CAPTCHA_GRID_SIZE - 1, y / cell));
        return row * CAPTCHA_GRID_SIZE + col + 1;
    }

    /** Solves the crypto challenge: PBKDF2-HMAC-SHA256 with 32 bytes output, result as lowercase hex string. */
    private static String generateCryptoAnswer(final String source, final String salt, final int iterations) throws Exception {
        final Mac mac = Mac.getInstance("HmacSHA256");
        mac.init(new SecretKeySpec(source.getBytes("UTF-8"), "HmacSHA256"));
        final byte[] saltBytes = salt.getBytes("UTF-8");
        final byte[] firstInput = new byte[saltBytes.length + 4];
        System.arraycopy(saltBytes, 0, firstInput, 0, saltBytes.length);
        /* Block index 1 as big endian int */
        firstInput[firstInput.length - 1] = 1;
        byte[] u = mac.doFinal(firstInput);
        final byte[] result = u.clone();
        for (int i = 1; i < iterations; i++) {
            u = mac.doFinal(u);
            for (int j = 0; j < result.length; j++) {
                result[j] ^= u[j];
            }
        }
        final StringBuilder sb = new StringBuilder();
        for (final byte b : result) {
            final String hex = Integer.toHexString(b & 0xff);
            if (hex.length() < 2) {
                sb.append('0');
            }
            sb.append(hex);
        }
        return sb.toString();
    }

    /** Loads the article content which is only available after the captcha has been solved. */
    private String crawlArticleContentAfterCaptcha(final Browser br, final String captchaTestID, final String linkSharingSecret) throws Exception {
        final String articleID = br.getRegex("window\\.article\\s*=\\s*\\{\"id\":(\\d+)").getMatch(0);
        if (articleID == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Map<String, Object> body = new LinkedHashMap<String, Object>();
        body.put("articleId", Long.parseLong(articleID));
        body.put("captchaCode", captchaTestID);
        if (linkSharingSecret != null) {
            body.put("linkSharingSecret", linkSharingSecret);
        }
        final Map<String, Object> result = postJson(br, "/api/v1/article-dynamic", body);
        if (!"display".equals(result.get("action"))) {
            logger.warning("Unexpected article-dynamic response: " + result);
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return result.get("articleContent").toString();
    }
}