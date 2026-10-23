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

import java.io.ByteArrayInputStream;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.Inflater;
import java.util.zip.InflaterInputStream;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Hash;
import org.appwork.utils.IO;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.logging2.LogInterface;
import org.jdownloader.captcha.v2.challenge.altcha.CaptchaHelperCrawlerPluginAltcha;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.AbstractCloudflareTurnstileCaptcha;
import org.jdownloader.captcha.v2.challenge.cloudflareturnstile.CaptchaHelperCrawlerPluginCloudflareTurnstile;
import org.jdownloader.plugins.controller.PluginClassLoader;
import org.jdownloader.plugins.controller.PluginClassLoader.PluginClassLoaderChild;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.requests.PostRequest;
import jd.nutils.encoding.Encoding;
import jd.parser.html.Form;
import jd.parser.html.HTMLParser;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.Plugin;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

@DecrypterPlugin(revision = "$Revision: 53711 $", interfaceVersion = 3, names = {}, urls = {})
public class HdencodeOrg extends PluginForDecrypt {
    public HdencodeOrg(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public Browser createNewBrowserInstance() {
        final Browser br = super.createNewBrowserInstance();
        br.setFollowRedirects(true);
        return br;
    }

    public static List<String[]> getPluginDomains() {
        final List<String[]> ret = new ArrayList<String[]>();
        ret.add(new String[] { "hdencode.org", "hdencode.com", "hdencode.ro" });
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
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "/(?!rss)[a-z0-9]+-[a-z0-9\\-]+/?");
        }
        return ret.toArray(new String[0]);
    }

    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        br.getPage(param.getCryptedUrl());
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        String title = br.getRegex("<title>([^<]+)</title>").getMatch(0);
        if (title != null) {
            title = Encoding.htmlDecode(title).trim();
            /* Title contains file-size -> Remove that */
            title = title.replaceFirst("\\s*– [0-9\\.]+ ?(KB|MB|GB|TB)$", "");
        }
        final String[] fnames = br.getRegex("Filename\\.*: ([^\\r\\n\"]+)").getColumn(0);
        /** Use the longest filename (most complete one). HTML code can contain some dummy titles like "...". */
        String nfoArchiveContentFilename = null;
        if (fnames != null && fnames.length > 0) {
            for (final String fname : fnames) {
                if (nfoArchiveContentFilename == null || fname.trim().length() > nfoArchiveContentFilename.length()) {
                    nfoArchiveContentFilename = fname.trim();
                }
            }
        }
        final Form captchaform = br.getFormByRegex(".*content-protector-access-form.*");
        if (captchaform != null) {
            this.handleContentProtectorCaptchas(param, captchaform);
            br.submitForm(captchaform);
            if (br.getFormByRegex(".*content-protector-access-form.*") != null) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA);
            }
        }
        final String[] htmls = br.getRegex("<blockquote>(.*?)</blockquote>").getColumn(0);
        if (htmls == null || htmls.length == 0) {
            /* This should never happen. */
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        for (final String html : htmls) {
            final String[] urls = HTMLParser.getHttpLinks(html, br.getURL());
            if (urls == null || urls.length == 0) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            for (final String url : urls) {
                ret.add(createDownloadlink(url));
            }
        }
        logger.info("Number of results: " + ret.size());
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        /* We want all results to go into one package */
        final FilePackage fp = FilePackage.getInstance();
        if (nfoArchiveContentFilename != null) {
            if (nfoArchiveContentFilename.contains(".")) {
                /* Package name = Filename without file-extension */
                fp.setName(nfoArchiveContentFilename.substring(0, nfoArchiveContentFilename.lastIndexOf(".")));
            } else {
                fp.setName(nfoArchiveContentFilename);
            }
        } else if (title != null) {
            fp.setName(title);
        } else {
            /* Fallback */
            fp.setName(br._getURL().getPath());
        }
        fp.setCleanupPackageName(false);
        fp.addLinks(ret);
        return ret;
    }

    /**
     * WordPress plugin "content-protector": The form requires up to four different captchas. <br>
     * - Cap (https://capjs.js.org/): always required (form field 'cap-token') -> solved locally, see CapLocalSolver <br>
     * - ALTCHA (v2, PBKDF2): see '/wp-json/content-protector/v1/security' -> "require_altcha" <br>
     * - Cloudflare Turnstile (invisible): only if "require_turnstile" is true <br>
     * - Image captcha (6 characters): only if "require_image_captcha" is true ("too many requests from this IP") <br>
     * Order: <br>
     * 1. Cap: solved first and separately: It costs nothing and if it fails, no money is spent on the captcha solvers. <br>
     * 2. Image captcha: solved next without threading because its solution is probably valid the longest (the tokens of the other captchas
     * might expire while the user is entering it). It runs in the crawler thread (user interaction, skip/blacklist handling of
     * handleCaptchaChallenge only works in LinkCrawlerThreads). <br>
     * 3. ALTCHA and Turnstile: solved at the same time in separate threads like the website does.
     */
    private void handleContentProtectorCaptchas(final CryptedLink param, final Form captchaform) throws Exception {
        if (captchaform.hasInputFieldByName("cap-token")) {
            final String capApiEndpoint = CapLocalSolver.findApiEndpoint(br);
            if (capApiEndpoint == null) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Cap apiEndpoint not found");
            }
            captchaform.put("cap-token", Encoding.urlEncode(new CapLocalSolver(this, br, capApiEndpoint).getToken()));
        }
        /* Detect required captcha types and collect all data needed to solve them (sequential requests). */
        final Browser brc = br.cloneBrowser();
        brc.getPage("/wp-json/content-protector/v1/security");
        /* e.g. {"require_altcha":true,"require_turnstile":false,"require_image_captcha":false} */
        final Map<String, Object> captchainfo = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
        final boolean requiresImageCaptcha = Boolean.TRUE.equals(captchainfo.get("require_image_captcha"));
        final boolean requiresAltcha = Boolean.TRUE.equals(captchainfo.get("require_altcha"));
        final boolean requiresTurnstile = Boolean.TRUE.equals(captchainfo.get("require_turnstile"));
        logger.info("Required captchas: image=" + requiresImageCaptcha + "|altcha=" + requiresAltcha + "|turnstile=" + requiresTurnstile);
        Map<String, Object> imagecaptcha = null;
        String imagecaptchaUrl = null;
        if (requiresImageCaptcha) {
            brc.getPage("/imagecaptcha6charx/create.php");
            /* e.g. {"id":"a833e0a8...","image":"\/image.php?id=a833e0a8..."}, image URL is "/imagecaptcha6charx" + image */
            imagecaptcha = restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            imagecaptchaUrl = brc.getURL("/imagecaptcha6charx" + imagecaptcha.get("image").toString()).toString();
        }
        String imagecaptchaCode = null;
        if (requiresImageCaptcha) {
            /*
             * Solve the image captcha first and without threading: Its solution is probably valid the longest, the tokens of the other
             * captchas might expire while the user is entering the image captcha. For the same reason, the data for the other captchas
             * (e.g. ALTCHA challenge) is only fetched afterwards.
             */
            imagecaptchaCode = this.getCaptchaCode(brc, getHost(), imagecaptchaUrl, param);
        }
        final List<CaptchaSolverThread> threads = new ArrayList<CaptchaSolverThread>();
        CaptchaSolverThread altchaThread = null;
        String altchaChallengeJson = null;
        if (requiresAltcha) {
            brc.getPage("/wp-json/content-protector/v1/altcha");
            final String challengeJson = brc.getRequest().getHtmlCode();
            altchaChallengeJson = challengeJson;
            altchaThread = new CaptchaSolverThread("ALTCHA") {
                @Override
                protected String solve() throws Exception {
                    return new CaptchaHelperCrawlerPluginAltcha(HdencodeOrg.this, br, null, challengeJson).getToken();
                }
            };
            threads.add(altchaThread);
        }
        CaptchaSolverThread turnstileThread = null;
        if (requiresTurnstile) {
            /* Rendered via turnstile.render("#turnstile-container-...", {sitekey: "...", size: "invisible", ...}) */
            final String siteKey = br.getRegex("sitekey\\s*:\\s*\"(" + AbstractCloudflareTurnstileCaptcha.apiKeyRegex + ")\"").getMatch(0);
            turnstileThread = new CaptchaSolverThread("Turnstile") {
                @Override
                protected String solve() throws Exception {
                    return new CaptchaHelperCrawlerPluginCloudflareTurnstile(HdencodeOrg.this, br, siteKey).getToken();
                }
            };
            threads.add(turnstileThread);
        }
        /* Solve all other captchas at the same time */
        try {
            if (threads.size() > 0) {
                final ThreadGroup threadGroup = new ThreadGroup(Thread.currentThread().getThreadGroup(), getHost() + ":captchas");
                /* Thread group gets destroyed automatically once all its threads have finished. */
                threadGroup.setDaemon(true);
                final PluginClassLoaderChild classLoaderChild = PluginClassLoader.getThreadPluginClassLoaderChild();
                for (final CaptchaSolverThread thread : threads) {
                    thread.start(threadGroup, classLoaderChild);
                }
            }
            for (final CaptchaSolverThread thread : threads) {
                /* Throws the exception of the thread if solving failed -> Remaining threads get interrupted in finally block. */
                thread.waitForResult();
            }
        } finally {
            for (final CaptchaSolverThread thread : threads) {
                thread.interrupt();
            }
        }
        /* Disabled unless image captcha is required -> Disabled fields are not sent by the browser. */
        captchaform.remove("captcha_answer");
        captchaform.remove("captcha_id");
        captchaform.put("content-protector-submit", Encoding.urlEncode("View links"));
        if (requiresImageCaptcha) {
            captchaform.put("captcha_answer", Encoding.urlEncode(imagecaptchaCode));
            captchaform.put("captcha_id", Encoding.urlEncode(imagecaptcha.get("id").toString()));
        }
        if (altchaThread != null) {
            /*
             * The website does not submit the standard base64 payload {"challenge":{...},"solution":{...}} but its own format:
             * JSON.stringify({...challenge, ...solution}) -> Convert the solution of the captcha solver into that format.
             */
            final Map<String, Object> payload = restoreFromString(Base64.decodeToString(altchaThread.getResult()), TypeRef.MAP);
            final Map<String, Object> solution = (Map<String, Object>) payload.get("solution");
            final Map<String, Object> altchapayload = new LinkedHashMap<String, Object>(restoreFromString(altchaChallengeJson, TypeRef.MAP));
            altchapayload.putAll(solution);
            captchaform.put("altcha-payload", Encoding.urlEncode(JSonStorage.serializeToJson(altchapayload)));
        }
        if (turnstileThread != null) {
            /* Turnstile widget adds this field to the form automatically. */
            captchaform.put("cf-turnstile-response", Encoding.urlEncode(turnstileThread.getResult()));
        }
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /*
         * Try to avoid rate limit plus this plugin will request a lot of captchas so working on more than 1 link at the same time is kind
         * of pointless.
         */
        return 1;
    }

    /** Solves one captcha in its own thread, result/exception is handed over to the crawler thread via {@link #waitForResult()}. */
    private static abstract class CaptchaSolverThread implements Runnable {
        private final String                    name;
        private volatile PluginClassLoaderChild classLoaderChild;
        private volatile String                 result;
        private volatile Throwable              throwable;
        private Thread                          thread;

        protected CaptchaSolverThread(final String name) {
            this.name = name;
        }

        protected abstract String solve() throws Exception;

        /** Starts solving within the given thread group using the plugin class loader of the calling (crawler) thread. */
        private void start(final ThreadGroup threadGroup, final PluginClassLoaderChild classLoaderChild) {
            this.classLoaderChild = classLoaderChild;
            thread = new Thread(threadGroup, this, threadGroup.getName() + ":" + name);
            thread.setDaemon(true);
            thread.start();
        }

        @Override
        public void run() {
            /* Own thread group -> removing the group mapping (null) does not affect other threads. */
            PluginClassLoader.setThreadPluginClassLoaderChild(classLoaderChild, null);
            try {
                result = solve();
            } catch (final Throwable e) {
                throwable = e;
            } finally {
                PluginClassLoader.setThreadPluginClassLoaderChild(null, null);
            }
        }

        /** Interrupts solving, e.g. because another captcha failed or the crawler got aborted. */
        private void interrupt() {
            final Thread thread = this.thread;
            if (thread != null && thread.isAlive()) {
                thread.interrupt();
            }
        }

        /** Waits until the captcha has been solved and throws the exception of the solving thread, if any. */
        private void waitForResult() throws Exception {
            thread.join();
            final Throwable e = throwable;
            if (e instanceof Exception) {
                throw (Exception) e;
            } else if (e instanceof Error) {
                throw (Error) e;
            } else if (result == null) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA, name + " not solved");
            }
        }

        private String getResult() {
            return result;
        }
    }

    /**
     * Java port of the HashWX proof-of-work hash function by tevador (interpreted mode, no JIT). <br>
     * Specification: https://github.com/tevador/hashwx/blob/master/doc/specification.md <br>
     * Reference implementation: https://github.com/tevador/hashwx/tree/master/src (program.c, program_exec.c, siphash_rng.c, hashwx.c) <br>
     * Test vectors (specification Appendix F): seed 5468697320697320612074657374207365656420666f72206861736877780000, nonce 0 ->
     * 0x973684176f8ee362, nonce 123456 -> 0x401983bb07d69b07. <br>
     * All arithmetic is modulo 2^64, Java long overflow semantics match the unsigned C implementation. Instances are not thread safe.
     */
    private static class HashWX {
        public static final int      SEED_SIZE        = 32;
        private static final int     NUM_PROGRAMS     = 32;
        private static final int     PROGRAM_SIZE     = 11;
        private static final int     WRITABLE_SIZE    = 16384;
        private static final int     MEMORY_SIZE      = WRITABLE_SIZE + 8;
        private static final int     NUM_REPEATS      = 4;
        private static final int     BRANCH_COUNT     = 32;
        /* Opcodes, see specification table 1.4.2 */
        private static final int     MULOR            = 0;
        private static final int     MULXOR           = 1;
        private static final int     MULADD           = 2;
        private static final int     RMCG             = 3;
        private static final int     XORROR           = 4;
        private static final int     ADDROR           = 5;
        private static final int     SUBROR           = 6;
        private static final int     XORASR           = 7;
        private static final int     ADDASR           = 8;
        private static final int     SUBASR           = 9;
        private static final int     XORLSR           = 10;
        private static final int     ADDLSR           = 11;
        private static final int     SUBLSR           = 12;
        private static final int     CBRANCH          = 13;
        private static final int     UBRANCH          = 14;
        private static final int     STORE            = 15;
        private static final int     HALT             = 16;
        private static final int[]   MUL_IMMS         = new int[] { 1, 9, 33 };
        private static final int[]   XAS_LOOKUP       = new int[] { XORROR, ADDROR, SUBROR, XORASR, ADDASR, SUBASR, XORLSR, ADDLSR, SUBLSR };
        /* Appendix B: Bit i set = MUL opcode at program index i + 2, otherwise XAS opcode. */
        private static final int[]   OPCODE_TEMPLATES = new int[] { 0x07, 0x0b, 0x13, 0x23, 0x43, 0x0d, 0x15, 0x25, 0x45, 0x19, 0x29, 0x49, 0x31, 0x51, 0x61, 0x0e, 0x16, 0x26, 0x46, 0x1a, 0x2a, 0x4a, 0x32, 0x52, 0x62, 0x1c, 0x2c, 0x4c, 0x34, 0x54, 0x64, 0x38, 0x58, 0x68, 0x70 };
        /* Appendix D.1: 256 shallow source permutations */
        private static final int[][] SRC_SHALLOW      = new int[][] { { 0, 2, 1, 4, 3, 6, 7, 5 }, { 0, 3, 4, 1, 6, 7, 5, 2 }, { 0, 4, 5, 6, 7, 1, 2, 3 }, { 0, 6, 3, 7, 5, 2, 1, 4 }, { 1, 0, 4, 5, 2, 3, 7, 6 }, { 1, 3, 0, 2, 5, 4, 7, 6 }, { 1, 3, 5, 2, 0, 4, 7, 6 }, { 1, 3, 6, 2, 7, 0, 5, 4 }, { 1, 3, 7, 2, 0, 6, 4, 5 }, { 1, 3, 7, 2, 0, 6, 5, 4 }, { 1, 3, 7, 2, 5, 4, 0, 6 }, { 1, 3, 7, 2, 6, 0, 5, 4 }, { 1, 4, 5, 0, 3, 2, 7, 6 }, { 1, 4, 5, 7, 2, 3, 0, 6 }, { 1, 4, 6, 0, 3, 7, 2, 5 }, { 1, 4, 6, 7, 2, 0, 3, 5 }, { 1, 4, 6, 7, 2, 0, 5, 3 }, { 1, 4, 7, 0, 2, 6, 5, 3 }, { 1, 4, 7, 0, 3, 6, 2, 5 }, { 1, 4, 7, 5, 2, 3, 0, 6 }, { 1, 4, 7, 5, 3, 2, 0, 6 }, { 1, 4, 7, 6, 3, 0, 5, 2 }, { 1, 5, 6, 7, 0, 2, 3, 4 }, { 1, 5, 6, 7, 0, 2, 4, 3 }, { 1, 5, 7, 0, 6, 2, 3, 4 }, { 1, 5, 7, 0, 6, 2, 4, 3 }, { 1, 5, 7, 6, 0, 3, 2, 4 }, { 1, 5, 7, 6, 0, 4, 2, 3 }, { 1, 6, 4, 7, 2, 0, 5, 3 },
                { 1, 6, 5, 7, 0, 2, 4, 3 }, { 1, 7, 3, 2, 0, 6, 5, 4 }, { 1, 7, 3, 2, 6, 0, 4, 5 }, { 1, 7, 3, 2, 6, 0, 5, 4 }, { 1, 7, 4, 6, 2, 0, 3, 5 }, { 1, 7, 4, 6, 3, 0, 5, 2 }, { 1, 7, 5, 0, 6, 2, 3, 4 }, { 1, 7, 5, 0, 6, 2, 4, 3 }, { 1, 7, 5, 6, 0, 2, 3, 4 }, { 1, 7, 5, 6, 0, 2, 4, 3 }, { 1, 7, 6, 0, 5, 2, 4, 3 }, { 1, 7, 6, 4, 2, 0, 5, 3 }, { 1, 7, 6, 4, 3, 0, 5, 2 }, { 1, 7, 6, 5, 0, 2, 3, 4 }, { 2, 0, 4, 6, 3, 7, 1, 5 }, { 2, 3, 0, 1, 7, 6, 4, 5 }, { 2, 3, 1, 0, 6, 7, 4, 5 }, { 2, 3, 1, 7, 0, 6, 4, 5 }, { 2, 3, 1, 7, 0, 6, 5, 4 }, { 2, 4, 0, 7, 1, 6, 5, 3 }, { 2, 4, 1, 7, 3, 6, 5, 0 }, { 2, 4, 5, 7, 1, 3, 0, 6 }, { 2, 4, 5, 7, 3, 1, 0, 6 }, { 2, 4, 6, 7, 1, 0, 3, 5 }, { 2, 4, 7, 5, 3, 1, 0, 6 }, { 2, 5, 0, 6, 7, 3, 4, 1 }, { 2, 5, 1, 6, 7, 0, 3, 4 }, { 2, 5, 1, 6, 7, 3, 0, 4 }, { 2, 5, 1, 7, 0, 6, 3, 4 }, { 2, 5, 1, 7, 6, 0, 3, 4 }, { 2, 5, 6, 0, 7, 1, 3, 4 },
                { 2, 5, 6, 7, 0, 1, 3, 4 }, { 2, 5, 7, 6, 0, 1, 4, 3 }, { 2, 5, 7, 6, 0, 3, 1, 4 }, { 2, 5, 7, 6, 0, 3, 4, 1 }, { 2, 5, 7, 6, 0, 4, 1, 3 }, { 2, 6, 1, 5, 7, 3, 0, 4 }, { 2, 6, 1, 5, 7, 4, 0, 3 }, { 2, 6, 1, 5, 7, 4, 3, 0 }, { 2, 6, 1, 7, 5, 3, 4, 0 }, { 2, 6, 1, 7, 5, 4, 3, 0 }, { 2, 7, 1, 0, 5, 6, 4, 3 }, { 2, 7, 1, 4, 3, 6, 0, 5 }, { 2, 7, 1, 4, 3, 6, 5, 0 }, { 2, 7, 1, 5, 0, 6, 4, 3 }, { 2, 7, 1, 5, 6, 0, 3, 4 }, { 2, 7, 1, 5, 6, 3, 0, 4 }, { 2, 7, 1, 5, 6, 3, 4, 0 }, { 2, 7, 1, 6, 5, 3, 0, 4 }, { 2, 7, 1, 6, 5, 4, 0, 3 }, { 2, 7, 3, 1, 0, 6, 5, 4 }, { 2, 7, 6, 5, 0, 3, 4, 1 }, { 3, 0, 4, 1, 6, 7, 2, 5 }, { 3, 0, 4, 2, 6, 7, 5, 1 }, { 3, 0, 5, 2, 6, 7, 4, 1 }, { 3, 0, 6, 1, 5, 4, 7, 2 }, { 3, 2, 0, 1, 6, 7, 4, 5 }, { 3, 2, 0, 1, 7, 6, 4, 5 }, { 3, 2, 1, 0, 5, 4, 7, 6 }, { 3, 2, 1, 0, 6, 7, 4, 5 }, { 3, 2, 1, 0, 7, 6, 5, 4 }, { 3, 2, 1, 5, 7, 4, 0, 6 },
                { 3, 2, 1, 6, 0, 7, 5, 4 }, { 3, 2, 7, 1, 6, 0, 4, 5 }, { 3, 2, 7, 1, 6, 0, 5, 4 }, { 3, 4, 0, 7, 1, 6, 5, 2 }, { 3, 4, 6, 1, 7, 0, 2, 5 }, { 3, 4, 6, 2, 7, 0, 5, 1 }, { 3, 4, 6, 7, 2, 0, 5, 1 }, { 3, 4, 7, 2, 0, 6, 5, 1 }, { 3, 5, 0, 7, 6, 4, 2, 1 }, { 3, 5, 6, 0, 7, 1, 2, 4 }, { 3, 5, 6, 2, 7, 0, 4, 1 }, { 3, 5, 6, 7, 0, 1, 2, 4 }, { 3, 5, 6, 7, 0, 2, 4, 1 }, { 3, 5, 6, 7, 0, 4, 2, 1 }, { 3, 5, 7, 1, 6, 0, 4, 2 }, { 3, 5, 7, 2, 6, 0, 1, 4 }, { 3, 5, 7, 6, 0, 1, 2, 4 }, { 3, 6, 5, 0, 7, 1, 2, 4 }, { 3, 6, 7, 0, 5, 4, 2, 1 }, { 3, 6, 7, 1, 5, 4, 0, 2 }, { 3, 7, 4, 1, 2, 6, 5, 0 }, { 3, 7, 5, 1, 6, 0, 4, 2 }, { 3, 7, 5, 1, 6, 4, 0, 2 }, { 3, 7, 6, 1, 5, 0, 4, 2 }, { 3, 7, 6, 1, 5, 2, 4, 0 }, { 3, 7, 6, 1, 5, 4, 0, 2 }, { 4, 0, 3, 2, 1, 6, 7, 5 }, { 4, 0, 3, 2, 6, 7, 1, 5 }, { 4, 2, 0, 5, 1, 3, 7, 6 }, { 4, 2, 0, 6, 3, 7, 1, 5 }, { 4, 2, 0, 6, 3, 7, 5, 1 },
                { 4, 2, 1, 0, 3, 6, 7, 5 }, { 4, 2, 1, 0, 7, 6, 3, 5 }, { 4, 2, 1, 5, 3, 0, 7, 6 }, { 4, 2, 1, 7, 6, 0, 5, 3 }, { 4, 2, 6, 7, 3, 0, 1, 5 }, { 4, 2, 7, 0, 3, 6, 1, 5 }, { 4, 2, 7, 5, 1, 3, 0, 6 }, { 4, 3, 0, 1, 2, 6, 7, 5 }, { 4, 3, 0, 1, 6, 7, 5, 2 }, { 4, 3, 0, 2, 5, 1, 7, 6 }, { 4, 3, 0, 2, 6, 7, 1, 5 }, { 4, 3, 0, 2, 7, 6, 1, 5 }, { 4, 3, 0, 2, 7, 6, 5, 1 }, { 4, 3, 0, 6, 2, 7, 1, 5 }, { 4, 3, 5, 2, 7, 1, 0, 6 }, { 4, 3, 5, 7, 2, 1, 0, 6 }, { 4, 3, 6, 0, 2, 7, 1, 5 }, { 4, 3, 6, 2, 0, 7, 5, 1 }, { 4, 3, 6, 2, 7, 0, 1, 5 }, { 4, 3, 6, 2, 7, 0, 5, 1 }, { 4, 5, 0, 6, 1, 7, 3, 2 }, { 4, 5, 3, 0, 2, 1, 7, 6 }, { 4, 5, 3, 1, 7, 2, 0, 6 }, { 4, 5, 3, 2, 7, 1, 0, 6 }, { 4, 5, 3, 7, 2, 1, 0, 6 }, { 4, 6, 3, 2, 0, 7, 1, 5 }, { 4, 6, 3, 2, 7, 0, 1, 5 }, { 4, 6, 3, 7, 1, 0, 5, 2 }, { 4, 6, 7, 5, 1, 2, 3, 0 }, { 4, 7, 3, 0, 1, 6, 2, 5 }, { 4, 7, 3, 0, 2, 6, 5, 1 },
                { 4, 7, 5, 6, 1, 2, 0, 3 }, { 5, 0, 3, 2, 6, 4, 7, 1 }, { 5, 0, 3, 4, 1, 2, 7, 6 }, { 5, 0, 3, 4, 2, 1, 7, 6 }, { 5, 0, 4, 6, 1, 2, 7, 3 }, { 5, 0, 4, 6, 1, 7, 3, 2 }, { 5, 0, 4, 6, 3, 7, 2, 1 }, { 5, 0, 4, 7, 3, 6, 1, 2 }, { 5, 0, 7, 4, 2, 6, 3, 1 }, { 5, 2, 0, 4, 1, 3, 7, 6 }, { 5, 2, 0, 4, 3, 1, 7, 6 }, { 5, 2, 0, 6, 7, 3, 4, 1 }, { 5, 2, 0, 6, 7, 4, 1, 3 }, { 5, 2, 1, 0, 6, 7, 3, 4 }, { 5, 2, 3, 1, 7, 4, 0, 6 }, { 5, 2, 4, 0, 3, 1, 7, 6 }, { 5, 2, 7, 4, 3, 1, 0, 6 }, { 5, 2, 7, 6, 0, 3, 4, 1 }, { 5, 3, 0, 2, 6, 7, 4, 1 }, { 5, 3, 4, 0, 2, 1, 7, 6 }, { 5, 3, 4, 2, 0, 1, 7, 6 }, { 5, 3, 7, 4, 1, 2, 0, 6 }, { 5, 3, 7, 4, 2, 1, 0, 6 }, { 5, 4, 0, 6, 2, 7, 1, 3 }, { 5, 4, 0, 6, 3, 1, 7, 2 }, { 5, 4, 3, 0, 2, 1, 7, 6 }, { 5, 4, 3, 1, 7, 2, 0, 6 }, { 5, 4, 6, 0, 7, 1, 2, 3 }, { 5, 4, 6, 0, 7, 1, 3, 2 }, { 5, 4, 7, 6, 1, 3, 2, 0 }, { 5, 6, 0, 4, 1, 7, 2, 3 },
                { 5, 6, 0, 4, 3, 7, 1, 2 }, { 5, 6, 0, 4, 3, 7, 2, 1 }, { 5, 6, 4, 0, 2, 7, 1, 3 }, { 5, 6, 4, 0, 3, 7, 2, 1 }, { 5, 6, 7, 4, 1, 2, 0, 3 }, { 6, 0, 3, 1, 5, 4, 7, 2 }, { 6, 0, 3, 1, 5, 7, 2, 4 }, { 6, 0, 3, 2, 5, 4, 7, 1 }, { 6, 0, 3, 7, 5, 4, 1, 2 }, { 6, 0, 4, 5, 1, 2, 7, 3 }, { 6, 0, 4, 5, 1, 3, 7, 2 }, { 6, 0, 4, 5, 1, 7, 2, 3 }, { 6, 0, 4, 5, 1, 7, 3, 2 }, { 6, 0, 4, 5, 2, 3, 7, 1 }, { 6, 0, 4, 5, 2, 7, 3, 1 }, { 6, 0, 4, 7, 5, 3, 1, 2 }, { 6, 0, 4, 7, 5, 3, 2, 1 }, { 6, 0, 5, 4, 1, 2, 7, 3 }, { 6, 0, 5, 4, 2, 1, 7, 3 }, { 6, 0, 5, 4, 2, 7, 3, 1 }, { 6, 0, 5, 4, 3, 1, 7, 2 }, { 6, 0, 5, 4, 3, 2, 7, 1 }, { 6, 0, 7, 4, 5, 3, 1, 2 }, { 6, 2, 0, 4, 3, 7, 5, 1 }, { 6, 2, 1, 5, 7, 3, 4, 0 }, { 6, 2, 4, 0, 3, 7, 1, 5 }, { 6, 3, 0, 1, 5, 4, 7, 2 }, { 6, 3, 5, 2, 7, 4, 0, 1 }, { 6, 4, 3, 1, 7, 0, 2, 5 }, { 6, 4, 5, 0, 2, 3, 7, 1 }, { 6, 4, 5, 7, 1, 2, 3, 0 },
                { 6, 4, 5, 7, 1, 3, 2, 0 }, { 6, 4, 5, 7, 2, 1, 3, 0 }, { 6, 5, 0, 4, 2, 1, 7, 3 }, { 6, 5, 0, 4, 7, 3, 2, 1 }, { 6, 7, 3, 1, 5, 2, 0, 4 }, { 6, 7, 3, 1, 5, 4, 0, 2 }, { 6, 7, 3, 1, 5, 4, 2, 0 }, { 6, 7, 5, 4, 1, 2, 3, 0 }, { 7, 0, 4, 5, 2, 6, 1, 3 }, { 7, 0, 5, 4, 3, 6, 1, 2 }, { 7, 2, 1, 4, 3, 6, 5, 0 }, { 7, 2, 1, 6, 5, 3, 4, 0 }, { 7, 2, 1, 6, 5, 4, 3, 0 }, { 7, 2, 5, 6, 0, 1, 3, 4 }, { 7, 3, 4, 1, 5, 2, 0, 6 }, { 7, 3, 5, 1, 6, 4, 2, 0 }, { 7, 3, 5, 2, 6, 4, 1, 0 }, { 7, 3, 6, 1, 5, 4, 0, 2 }, { 7, 3, 6, 5, 0, 4, 1, 2 }, { 7, 4, 6, 5, 1, 0, 2, 3 }, { 7, 4, 6, 5, 1, 2, 3, 0 }, { 7, 5, 0, 4, 1, 6, 2, 3 }, { 7, 5, 3, 1, 6, 2, 4, 0 }, { 7, 5, 3, 1, 6, 4, 2, 0 }, { 7, 5, 3, 2, 6, 1, 4, 0 }, { 7, 6, 0, 4, 5, 2, 1, 3 }, { 7, 6, 0, 4, 5, 3, 1, 2 }, { 7, 6, 3, 0, 5, 4, 1, 2 }, { 7, 6, 3, 1, 5, 4, 2, 0 }, { 7, 6, 3, 2, 5, 4, 1, 0 }, { 7, 6, 4, 5, 1, 0, 3, 2 },
                { 7, 6, 4, 5, 1, 2, 3, 0 }, { 7, 6, 4, 5, 1, 3, 2, 0 }, { 7, 6, 4, 5, 2, 1, 3, 0 }, { 7, 6, 4, 5, 2, 3, 1, 0 }, { 7, 6, 4, 5, 3, 1, 2, 0 }, { 7, 6, 4, 5, 3, 2, 1, 0 }, { 7, 6, 5, 4, 0, 3, 1, 2 }, { 7, 6, 5, 4, 1, 2, 3, 0 }, { 7, 6, 5, 4, 1, 3, 2, 0 }, { 7, 6, 5, 4, 2, 1, 3, 0 }, };
        /* Appendix D.2: 24 deep source permutations */
        private static final int[][] SRC_DEEP         = new int[][] { { 0, 2, 7, 1, 3, 4, 5, 6 }, { 0, 3, 1, 7, 2, 4, 5, 6 }, { 0, 4, 1, 2, 7, 3, 5, 6 }, { 0, 5, 1, 2, 3, 7, 4, 6 }, { 0, 6, 1, 2, 3, 4, 7, 5 }, { 1, 0, 7, 2, 3, 4, 5, 6 }, { 1, 7, 0, 2, 3, 4, 5, 6 }, { 2, 0, 7, 1, 3, 4, 5, 6 }, { 2, 7, 0, 1, 3, 4, 5, 6 }, { 3, 0, 1, 7, 2, 4, 5, 6 }, { 3, 7, 1, 0, 2, 4, 5, 6 }, { 4, 0, 1, 2, 7, 3, 5, 6 }, { 4, 7, 1, 2, 0, 3, 5, 6 }, { 5, 0, 1, 2, 3, 7, 4, 6 }, { 5, 7, 1, 2, 3, 0, 4, 6 }, { 6, 0, 1, 2, 3, 4, 7, 5 }, { 6, 7, 1, 2, 3, 4, 0, 5 }, { 6, 7, 1, 2, 3, 4, 5, 0 }, { 7, 2, 0, 1, 3, 4, 5, 6 }, { 7, 3, 1, 0, 2, 4, 5, 6 }, { 7, 4, 1, 2, 0, 3, 5, 6 }, { 7, 5, 1, 2, 3, 0, 4, 6 }, { 7, 6, 1, 2, 3, 4, 0, 5 }, { 7, 6, 1, 2, 3, 4, 5, 0 }, };
        private final int[]          programOpcode    = new int[NUM_PROGRAMS * PROGRAM_SIZE];
        private final int[]          programDst       = new int[NUM_PROGRAMS * PROGRAM_SIZE];
        private final int[]          programSrc       = new int[NUM_PROGRAMS * PROGRAM_SIZE];
        private final int[]          programImm       = new int[NUM_PROGRAMS * PROGRAM_SIZE];
        private final long           k0;
        private final long           k1;
        private final ByteBuffer     memory           = ByteBuffer.allocate(MEMORY_SIZE).order(ByteOrder.LITTLE_ENDIAN);
        private final long[]         r                = new long[9];
        private int                  sp;

        /** Siphash based pseudorandom number generator, see specification Appendix A. */
        private static class SiphashRng {
            private final long   k0;
            private final long   k1;
            private final long[] state = new long[4];
            private int          count;

            private SiphashRng(final long k0, final long k1, final long salt) {
                this.k0 = k0;
                this.k1 = k1;
                state[0] = 0x736f6d6570736575L ^ k0;
                state[1] = 0x646f72616e646f6dL ^ k1;
                state[2] = 0x6c7967656e657261L ^ k0;
                state[3] = 0x7465646279746573L ^ k1;
                state[3] ^= salt;
                sipround(state, 0);
                state[0] ^= salt;
                state[2] ^= 0xbb;
                sipround(state, 0);
                sipround(state, 0);
                sipround(state, 0);
                count = 4;
            }

            private long next() {
                if (count == 0) {
                    state[0] ^= k0;
                    state[1] ^= k1;
                    state[2] ^= k0;
                    state[3] ^= k1;
                    sipround(state, 0);
                    sipround(state, 0);
                    sipround(state, 0);
                    sipround(state, 0);
                    count = 4;
                }
                count--;
                return state[count];
            }
        }

        private static void sipround(final long[] v, final int o) {
            v[o] += v[o + 1];
            v[o + 2] += v[o + 3];
            v[o + 1] = Long.rotateLeft(v[o + 1], 13);
            v[o + 3] = Long.rotateLeft(v[o + 3], 16);
            v[o + 1] ^= v[o];
            v[o + 3] ^= v[o + 2];
            v[o] = Long.rotateLeft(v[o], 32);
            v[o + 2] += v[o + 1];
            v[o] += v[o + 3];
            v[o + 1] = Long.rotateLeft(v[o + 1], 17);
            v[o + 3] = Long.rotateLeft(v[o + 3], 21);
            v[o + 1] ^= v[o + 2];
            v[o + 3] ^= v[o];
            v[o + 2] = Long.rotateLeft(v[o + 2], 32);
        }

        /** Returns x modulo m with x interpreted as unsigned 64-bit integer (Long.remainderUnsigned is not available in Java 1.6). */
        private static int unsignedMod(final long x, final int m) {
            if (x >= 0) {
                return (int) (x % m);
            }
            final long quotient = ((x >>> 1) / m) << 1;
            long rem = x - quotient * m;
            if (rem >= m) {
                rem -= m;
            }
            return (int) rem;
        }

        /** Generates the HashWX instance (32 programs + siphash key) from a 32 byte seed, see specification section 2. */
        public HashWX(final byte[] seed) {
            if (seed == null || seed.length != SEED_SIZE) {
                throw new IllegalArgumentException("Seed must be " + SEED_SIZE + " bytes");
            }
            final ByteBuffer bb = ByteBuffer.wrap(seed).order(ByteOrder.LITTLE_ENDIAN);
            this.k0 = bb.getLong(16);
            this.k1 = bb.getLong(24);
            final SiphashRng rng = new SiphashRng(bb.getLong(0), bb.getLong(8), -1L);
            for (int i = 0; i < NUM_PROGRAMS - 1; i++) {
                /* Program is deep if i is divisible by 6 */
                generateProgram(rng, i, false, (i % 6) == 0);
            }
            generateProgram(rng, NUM_PROGRAMS - 1, true, false);
        }

        private static int generateImmediate(final int opcode, final long select) {
            if (opcode <= MULADD) {
                return MUL_IMMS[unsignedMod(select, 3)];
            } else if (opcode <= SUBROR) {
                return 1 + unsignedMod(select, 63);
            } else {
                return 1 + unsignedMod(select, 3);
            }
        }

        private void generateProgram(final SiphashRng rng, final int program, final boolean isLast, final boolean isDeep) {
            final long[] gen = new long[8];
            for (int i = 0; i < 8; i++) {
                gen[i] = rng.next();
            }
            final int base = program * PROGRAM_SIZE;
            final int seq = OPCODE_TEMPLATES[unsignedMod(gen[7], OPCODE_TEMPLATES.length)];
            programOpcode[base] = STORE;
            programOpcode[base + 1] = RMCG;
            final long[] dstSelect = new long[7];
            for (int i = 0; i < 7; i++) {
                final long selectLo = gen[i] & 0xFFFFFFFFL;
                dstSelect[i] = gen[i] >>> 32;
                if (((seq >> i) & 1) != 0) {
                    programOpcode[base + i + 2] = (int) (selectLo % 3);
                } else {
                    programOpcode[base + i + 2] = XAS_LOOKUP[(int) (selectLo % XAS_LOOKUP.length)];
                }
            }
            /* Destinations: Fisher-Yates shuffle of R0-R7 */
            final int[] dst = new int[8];
            dst[0] = 0;
            for (int i = 1; i < 8; i++) {
                dst[i] = i;
                final int j = (int) (dstSelect[i - 1] % (i + 1));
                dst[i] = dst[j];
                dst[j] = i;
            }
            /* Sources: permutation of the destinations, RMCG always uses R8 */
            final int[] perm = isDeep ? SRC_DEEP[unsignedMod(gen[7], SRC_DEEP.length)] : SRC_SHALLOW[unsignedMod(gen[7], SRC_SHALLOW.length)];
            final int[] src = new int[8];
            src[0] = 8;
            for (int i = 1; i < 8; i++) {
                src[i] = dst[perm[i]];
            }
            for (int i = 0; i < 8; i++) {
                final int index = base + i + 1;
                programDst[index] = dst[i];
                programSrc[index] = src[i];
                programImm[index] = generateImmediate(programOpcode[index], rng.next());
            }
            programOpcode[base + 9] = isLast ? UBRANCH : CBRANCH;
            programOpcode[base + 10] = HALT;
        }

        /** Calculates the 64-bit hash of the given nonce, see specification section 3. */
        public long hash(final long nonce) {
            final SiphashRng rng = new SiphashRng(k0, k1, nonce);
            for (int i = 0; i < 8; i++) {
                r[i] = rng.next();
            }
            /* Adjust R8 to be 3 mod 8 */
            r[8] = ((r[0] ^ r[4]) & -8L) | 3;
            memory.putLong(WRITABLE_SIZE, r[8]);
            sp = WRITABLE_SIZE;
            /* Memory write phase */
            for (int rep = 0; rep < NUM_REPEATS; rep++) {
                int branchCounter = BRANCH_COUNT;
                for (int p = 0; p < NUM_PROGRAMS; p++) {
                    branchCounter = execute(p, branchCounter, false);
                }
            }
            /* Memory read phase */
            for (int rep = 0; rep < NUM_REPEATS; rep++) {
                int branchCounter = BRANCH_COUNT;
                for (int p = 0; p < NUM_PROGRAMS; p++) {
                    branchCounter = execute(p, branchCounter, true);
                }
            }
            /* Finalization */
            sipround(r, 0);
            sipround(r, 4);
            return r[3] ^ r[7] ^ r[8];
        }

        private long source(final int src, final boolean memoryFlag) {
            if (memoryFlag) {
                /* Unaligned little endian load at address (src % 16384) */
                return memory.getLong((int) (r[src] & (WRITABLE_SIZE - 1)));
            } else {
                return r[src];
            }
        }

        private int execute(final int program, int branchCounter, final boolean memoryFlag) {
            final int base = program * PROGRAM_SIZE;
            int branchFlag = 0;
            int ic = 0;
            while (true) {
                final int index = base + ic;
                ic++;
                final int dst = programDst[index];
                final int src = programSrc[index];
                final int imm = programImm[index];
                switch (programOpcode[index]) {
                case MULOR:
                    r[dst] = (r[dst] | imm) * source(src, memoryFlag);
                    break;
                case MULXOR:
                    r[dst] = (r[dst] ^ imm) * source(src, memoryFlag);
                    break;
                case MULADD:
                    r[dst] = (r[dst] + imm) * source(src, memoryFlag);
                    break;
                case RMCG:
                    /* Always uses the register source operand R8 */
                    final long temp = Long.rotateRight(r[dst] * r[src], imm);
                    r[dst] = temp;
                    branchFlag = (int) temp;
                    break;
                case XORROR:
                    r[dst] = Long.rotateRight(r[dst], imm) ^ source(src, memoryFlag);
                    break;
                case ADDROR:
                    r[dst] = Long.rotateRight(r[dst], imm) + source(src, memoryFlag);
                    break;
                case SUBROR:
                    r[dst] = Long.rotateRight(r[dst], imm) - source(src, memoryFlag);
                    break;
                case XORASR:
                    r[dst] = (r[dst] >> imm) ^ source(src, memoryFlag);
                    break;
                case ADDASR:
                    r[dst] = (r[dst] >> imm) + source(src, memoryFlag);
                    break;
                case SUBASR:
                    r[dst] = (r[dst] >> imm) - source(src, memoryFlag);
                    break;
                case XORLSR:
                    r[dst] = (r[dst] >>> imm) ^ source(src, memoryFlag);
                    break;
                case ADDLSR:
                    r[dst] = (r[dst] >>> imm) + source(src, memoryFlag);
                    break;
                case SUBLSR:
                    r[dst] = (r[dst] >>> imm) - source(src, memoryFlag);
                    break;
                case CBRANCH:
                    if (branchCounter != 0 && (branchFlag & 32) == 0) {
                        branchCounter--;
                        ic = 0;
                    }
                    break;
                case UBRANCH:
                    if (branchCounter != 0) {
                        branchCounter--;
                        ic = 0;
                    }
                    break;
                case STORE:
                    if (!memoryFlag) {
                        /* R7 is stored at SP, R0 at SP + 56 */
                        sp -= 64;
                        for (int j = 0; j < 8; j++) {
                            memory.putLong(sp + 8 * (7 - j), r[j]);
                        }
                    }
                    break;
                case HALT:
                    return branchCounter;
                default:
                    throw new IllegalStateException("Unknown opcode:" + programOpcode[index]);
                }
            }
        }
    }

    /**
     * Solves Cap (https://capjs.js.org/, https://github.com/tiagozip/cap) locally without browser and returns the token which the widget
     * puts into the form field 'cap-token'. <br>
     * Flow: POST {apiEndpoint}challenge -> solve all challenges -> POST {apiEndpoint}redeem {"token":...,"solutions":[...]} -> token <br>
     * Supported "format 2" protocols: <br>
     * - hashwx: Find nonce N with HashWX(sha256(C || u64le(N / n)), N) <= (2^64 - 1) / d, see core/src/hashwx.js <br>
     * - sha256-pow: Find nonce N with sha256(salt + N) starting with the hex prefix target <br>
     * - instrumentation: Server generated JS (deflate raw + base64) which computes four integers via a fixed set of operations (see
     * core/src/instrumentation.js) and collects browser facts ("probe"). The operations are evaluated here, the probe is filled with values
     * of a regular desktop browser. Only works as long as the script is not obfuscated (obfuscationLevel <= 3). <br>
     * Not supported: "rsw" (deprecated) and legacy format 1.
     */
    private static class CapLocalSolver {
        private static final int     MAX_HASHWX_NONCES_PER_HASH = 1048576;
        /*
         * Limits against endless/extremely long loops caused by server provided values: Cap allows a difficulty of up to 1e9 per challenge
         * which would take hours. Websites use much lower values (2026-10-22: hdencode.org: 4x 25000).
         */
        private static final int     MAX_HASHWX_DIFFICULTY      = 10000000;
        private static final int     MAX_SHA256_TARGET_LENGTH   = 8;
        private static final int     MAX_CHALLENGES             = 70;
        private static final int     MAX_INSTRUMENTATION_SIZE   = 1024 * 1024;
        /*
         * Give up if no solution was found after this many times the expected number of attempts (probability: e^-50).
         */
        private static final int     MAX_ATTEMPTS_FACTOR        = 50;
        private static final long    SOLVE_TIMEOUT_MILLIS       = 3 * 60 * 1000l;
        /*
         * Common desktop screen geometries (maximized browser window on Windows) for the instrumentation probe: {screen.width,
         * screen.height, window.outerHeight, window.innerHeight}. One is picked randomly per instance so that not all users send the same
         * fingerprint and no real data of the user (e.g. the real screen resolution) is sent. Constraints of core/src/detect.js:
         * outerHeight must not exceed screen.height and the viewport must not be equal to the screen size.
         */
        private static final int[][] SCREEN_PROFILES            = new int[][] { { 1920, 1080, 1032, 945 }, { 2560, 1440, 1392, 1305 }, { 1366, 768, 728, 641 }, { 1536, 864, 816, 729 }, { 1440, 900, 852, 765 }, { 1680, 1050, 1002, 915 }, { 1280, 720, 672, 585 } };
        /* Height of the bookmarks bar which reduces window.innerHeight if visible */
        private static final int     BOOKMARKS_BAR_HEIGHT       = 33;
        private final Plugin         plugin;
        private final Browser        br;
        private final String         apiEndpoint;
        private final LogInterface   logger;
        private long                 deadline;
        private final Random         random                     = new Random();
        private final int            screenWidth;
        private final int            screenHeight;
        private final int            outerHeight;
        private final int            innerHeight;
        private final int            deviceMemory;
        private final double         fontWidthBase;
        private final double         fontWidthStep;

        /**
         * @param apiEndpoint
         *            Value of the widget/Cap option 'apiEndpoint' e.g. "https://cap.example.com/abcdef1234/" (with trailing slash).
         */
        public CapLocalSolver(final Plugin plugin, final Browser br, final String apiEndpoint) {
            this.plugin = plugin;
            this.br = br;
            this.apiEndpoint = apiEndpoint.endsWith("/") ? apiEndpoint : apiEndpoint + "/";
            this.logger = plugin.getLogger();
            final int[] profile = SCREEN_PROFILES[random.nextInt(SCREEN_PROFILES.length)];
            this.screenWidth = profile[0];
            this.screenHeight = profile[1];
            this.outerHeight = profile[2];
            this.innerHeight = profile[3] - (random.nextBoolean() ? BOOKMARKS_BAR_HEIGHT : 0);
            /* Chromium reports at most 8 (GB) */
            this.deviceMemory = random.nextInt(4) == 0 ? 4 : 8;
            this.fontWidthBase = 560 + random.nextDouble() * 120;
            this.fontWidthStep = 18 + random.nextDouble() * 8;
        }

        /** Finds the value of the option 'apiEndpoint' e.g. new Cap({apiEndpoint: "..."}) or <cap-widget data-cap-api-endpoint="...">. */
        public static String findApiEndpoint(final Browser br) {
            String endpoint = br.getRegex("apiEndpoint\\s*:\\s*[\"'](https?://[^\"']+)[\"']").getMatch(0);
            if (endpoint == null) {
                endpoint = br.getRegex("data-cap-api-endpoint\\s*=\\s*[\"'](https?://[^\"']+)[\"']").getMatch(0);
            }
            return endpoint;
        }

        /** @return Token for the form field 'cap-token'. */
        public String getToken() throws Exception {
            final Browser brc = br.cloneBrowser();
            final String origin = br._getURL().getProtocol() + "://" + br._getURL().getHost();
            brc.getHeaders().put("Origin", origin);
            brc.getHeaders().put("Accept", "*/*");
            brc.postPageRaw(apiEndpoint + "challenge", "");
            final Map<String, Object> challengeResponse = JSonStorage.restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (((Number) challengeResponse.get("format")).intValue() != 2) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported Cap challenge format:" + challengeResponse.get("format"));
            }
            final List<Map<String, Object>> challenges = (List<Map<String, Object>>) challengeResponse.get("challenges");
            if (challenges.size() > MAX_CHALLENGES) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Too many Cap challenges:" + challenges.size());
            }
            final List<Map<String, Object>> solutions = new ArrayList<Map<String, Object>>();
            final long start = System.currentTimeMillis();
            deadline = start + SOLVE_TIMEOUT_MILLIS;
            for (final Map<String, Object> challenge : challenges) {
                final String protocol = challenge.get("protocol").toString();
                final Map<String, Object> payload = (Map<String, Object>) challenge.get("payload");
                final Map<String, Object> solution = new LinkedHashMap<String, Object>();
                if ("hashwx".equals(protocol)) {
                    solution.put("nonce", solveHashWX(payload.get("c").toString(), ((Number) payload.get("d")).intValue(), ((Number) payload.get("n")).intValue()));
                } else if ("sha256-pow".equals(protocol)) {
                    solution.put("nonce", solveSha256Pow(payload.get("salt").toString(), payload.get("target").toString()));
                } else if ("instrumentation".equals(protocol)) {
                    solution.put("instr", solveInstrumentation(payload.get("blob").toString(), brc.getHeaders().get("User-Agent")));
                } else {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported Cap protocol:" + protocol);
                }
                solutions.add(solution);
            }
            logger.info("Solved " + solutions.size() + " Cap challenges in " + (System.currentTimeMillis() - start) + "ms");
            final Map<String, Object> redeem = new HashMap<String, Object>();
            redeem.put("token", challengeResponse.get("token"));
            redeem.put("solutions", solutions);
            final PostRequest req = brc.createJSonPostRequest(apiEndpoint + "redeem", redeem);
            brc.getPage(req);
            /* e.g. {"success":true,"token":"...","expires":1792666000000} or {"success":false,"error":"Invalid solution"} */
            final Map<String, Object> redeemResponse = JSonStorage.restoreFromString(brc.getRequest().getHtmlCode(), TypeRef.MAP);
            if (!Boolean.TRUE.equals(redeemResponse.get("success"))) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA, "Cap redeem failed:" + redeemResponse.get("error") + "|" + redeemResponse.get("reason"));
            }
            return redeemResponse.get("token").toString();
        }

        /** Called periodically by all solving loops: Allows aborting and ensures that solving cannot take forever. */
        private void checkInterruptedAndTimeout() throws InterruptedException, PluginException {
            if (Thread.currentThread().isInterrupted()) {
                throw new InterruptedException();
            } else if (System.currentTimeMillis() > deadline) {
                throw new PluginException(LinkStatus.ERROR_CAPTCHA, "Cap solving timeout");
            }
        }

        /** Unsigned 64-bit comparison a <= b (Long.compareUnsigned is not available in Java 1.6). */
        private static boolean lessOrEqualUnsigned(final long a, final long b) {
            return (a ^ Long.MIN_VALUE) <= (b ^ Long.MIN_VALUE);
        }

        /** HashWX seed for a block of nonces: sha256(challenge || u64le(block)) */
        private static byte[] hashWXSeed(final byte[] challenge, final long block) throws Exception {
            final ByteBuffer buf = ByteBuffer.allocate(challenge.length + 8).order(ByteOrder.LITTLE_ENDIAN);
            buf.put(challenge);
            buf.putLong(block);
            return MessageDigest.getInstance("SHA-256").digest(buf.array());
        }

        protected long solveHashWX(final String challengeHex, final int difficulty, final int noncesPerHash) throws Exception {
            if (!challengeHex.matches("(?i)^[0-9a-f]{64}$") || difficulty < 1 || difficulty > MAX_HASHWX_DIFFICULTY || noncesPerHash < 1 || noncesPerHash > MAX_HASHWX_NONCES_PER_HASH) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Malformed hashwx challenge");
            }
            final byte[] challenge = HexFormatter.hexToByteArray(challengeHex);
            /* target = (2^64 - 1) / d as unsigned 64-bit value */
            final long target = BigInteger.ONE.shiftLeft(64).subtract(BigInteger.ONE).divide(BigInteger.valueOf(difficulty)).longValue();
            HashWX hashwx = null;
            long block = -1;
            /* Expected work is d hashes. */
            final long maxNonces = (long) difficulty * MAX_ATTEMPTS_FACTOR;
            for (long nonce = 0; nonce < maxNonces; nonce++) {
                final long nonceBlock = nonce / noncesPerHash;
                if (nonceBlock != block) {
                    checkInterruptedAndTimeout();
                    block = nonceBlock;
                    hashwx = new HashWX(hashWXSeed(challenge, block));
                }
                if (lessOrEqualUnsigned(hashwx.hash(nonce), target)) {
                    return nonce;
                }
                if ((nonce & 0x3ff) == 0) {
                    checkInterruptedAndTimeout();
                }
            }
            throw new PluginException(LinkStatus.ERROR_CAPTCHA, "No hashwx solution found");
        }

        protected long solveSha256Pow(final String salt, final String target) throws Exception {
            final String prefix = target.toLowerCase();
            /* A non-hex or too long prefix would never/practically never match. */
            if (!prefix.matches("^[0-9a-f]{1," + MAX_SHA256_TARGET_LENGTH + "}$")) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Malformed sha256-pow challenge");
            }
            /* Expected work is 16^length hashes. */
            final long maxNonces = (1L << (4 * prefix.length())) * MAX_ATTEMPTS_FACTOR;
            for (long nonce = 0; nonce < maxNonces; nonce++) {
                if (Hash.getSHA256(salt + nonce).startsWith(prefix)) {
                    return nonce;
                }
                if ((nonce & 0x3ff) == 0) {
                    checkInterruptedAndTimeout();
                }
            }
            throw new PluginException(LinkStatus.ERROR_CAPTCHA, "No sha256-pow solution found");
        }

        private static int toInt32(final long value) {
            return (int) value;
        }

        /** Server side equivalent: domSumMock in core/src/instrumentation.js */
        private static int domSum(final int x, final int y, final int z) {
            long sum = 0;
            for (final int value : new int[] { x, y, z }) {
                int v = value;
                for (int i = 0; i < 8; i++) {
                    if ((v & 1) == 0) {
                        sum += v;
                    }
                    v = v >> 1;
                }
            }
            return toInt32(sum % 256);
        }

        private static String inflateRaw(final byte[] data) throws Exception {
            final Inflater inflater = new Inflater(true);
            try {
                /* Reads at most maxSize bytes (protection against decompression bombs), truncated data results in an EOFException. */
                final byte[] inflated = IO.readStream(MAX_INSTRUMENTATION_SIZE + 1, new InflaterInputStream(new ByteArrayInputStream(data), inflater));
                if (inflated.length > MAX_INSTRUMENTATION_SIZE) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Cap instrumentation script too large");
                }
                return new String(inflated, "UTF-8");
            } finally {
                inflater.end();
            }
        }

        /**
         * Evaluates the instrumentation script. Its main computation chain consists of (vars: four random identifiers):
         * <ul>
         * <li>var A=123; (initial values)</li>
         * <li>A = A ^ (navigator.userAgent ? correctKey : badKey);</li>
         * <li>20x one of: A = ~(A &amp; B); A = A ^ B; A = A | B; A = A &amp; B; A = fnHelper(B, C, A); A = domHelper(B, C, D);</li>
         * <li>4x A=((A^salt)&amp;0x7FFFFFFF)%900000+100000;</li>
         * </ul>
         */
        protected Map<String, Object> solveInstrumentation(final String blob, final String userAgent) throws Exception {
            final String js = inflateRaw(Base64.decode(blob));
            final String[][] outKeys = new Regex(js, "\\w+\\[\"(\\w+)\"\\]=\\1;").getMatches();
            final String fnHelper = new Regex(js, "function (\\w+)\\(a,b,c\\)\\{function F\\(d\\)").getMatch(0);
            final String domHelper = new Regex(js, "function (\\w+)\\(x,y,z\\)\\{var d=document\\.createElement").getMatch(0);
            final String id = new Regex(js, "result:\\{i:\"([0-9a-f]+)\"").getMatch(0);
            final String fontStacks = new Regex(js, "var \\w+=\\[(\"[^\\]]*)\\];var out=\\[\\];").getMatch(0);
            if (outKeys == null || outKeys.length != 4 || fnHelper == null || domHelper == null || id == null || fontStacks == null) {
                /* E.g. obfuscated script */
                logger.info("Unsupported instrumentation script:" + js);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported Cap instrumentation script");
            }
            final String[] vars = new String[4];
            final Map<String, Integer> values = new HashMap<String, Integer>();
            for (int i = 0; i < 4; i++) {
                vars[i] = outKeys[i][0];
                final String init = new Regex(js, "var " + Pattern.quote(vars[i]) + "=(-?\\d+);").getMatch(0);
                if (init == null) {
                    throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported Cap instrumentation script");
                }
                values.put(vars[i], Integer.valueOf(init));
            }
            final String v = "(" + Pattern.quote(vars[0]) + "|" + Pattern.quote(vars[1]) + "|" + Pattern.quote(vars[2]) + "|" + Pattern.quote(vars[3]) + ")";
            final Pattern statement = Pattern.compile(
                    /* 1-2: A = A ^ (navigator.userAgent ? correctKey : badKey); */
                    v + "\\s*=\\s*\\1\\s*\\^\\s*\\(navigator\\.userAgent\\s*\\?\\s*(\\d+)\\s*:\\s*\\d+\\)\\s*;"
                    /* 3-5: A = ~(A & B); */
                            + "|" + v + "\\s*=\\s*~\\(\\s*" + v + "\\s*&\\s*" + v + "\\s*\\)\\s*;"
                            /* 6-9: A = A ^ B; A = A | B; A = A & B; */
                            + "|" + v + "\\s*=\\s*" + v + "\\s*([\\^|&])\\s*" + v + "\\s*;"
                            /* 10-13: A = fnHelper(B, C, A); */
                            + "|" + v + "\\s*=\\s*" + Pattern.quote(fnHelper) + "\\(\\s*" + v + "\\s*,\\s*" + v + "\\s*,\\s*" + v + "\\s*\\)\\s*;"
                            /* 14-17: A = domHelper(B, C, D); */
                            + "|" + v + "\\s*=\\s*" + Pattern.quote(domHelper) + "\\(\\s*" + v + "\\s*,\\s*" + v + "\\s*,\\s*" + v + "\\s*\\)\\s*;"
                            /* 18-20: A=((A^salt)&0x7FFFFFFF)%900000+100000; */
                            + "|" + v + "\\s*=\\s*\\(\\(\\s*" + v + "\\s*\\^\\s*(\\d+)\\s*\\)\\s*&\\s*0x7FFFFFFF\\s*\\)\\s*%\\s*900000\\s*\\+\\s*100000\\s*;");
            final Matcher matcher = statement.matcher(js);
            int numberofStatements = 0;
            while (matcher.find()) {
                if (matcher.group(1) != null) {
                    values.put(matcher.group(1), values.get(matcher.group(1)) ^ Integer.parseInt(matcher.group(2)));
                } else if (matcher.group(3) != null) {
                    values.put(matcher.group(3), ~(values.get(matcher.group(4)) & values.get(matcher.group(5))));
                } else if (matcher.group(6) != null) {
                    final int a = values.get(matcher.group(7));
                    final int b = values.get(matcher.group(9));
                    final char op = matcher.group(8).charAt(0);
                    final int result;
                    if (op == '^') {
                        result = a ^ b;
                    } else if (op == '|') {
                        result = a | b;
                    } else {
                        result = a & b;
                    }
                    values.put(matcher.group(6), result);
                } else if (matcher.group(10) != null) {
                    /* function F(d){this.v=function(){return this.k^d;}} ... return i.v()|(new F(b)).v(); -> (a ^ b) | (c ^ b) */
                    final int a = values.get(matcher.group(11));
                    final int b = values.get(matcher.group(12));
                    final int c = values.get(matcher.group(13));
                    values.put(matcher.group(10), (a ^ b) | (c ^ b));
                } else if (matcher.group(14) != null) {
                    values.put(matcher.group(14), domSum(values.get(matcher.group(15)), values.get(matcher.group(16)), values.get(matcher.group(17))));
                } else {
                    final long salted = (values.get(matcher.group(19)) ^ Integer.parseInt(matcher.group(20))) & 0x7FFFFFFFL;
                    values.put(matcher.group(18), toInt32(salted % 900000 + 100000));
                }
                numberofStatements++;
            }
            if (numberofStatements != 25) {
                logger.info("Unexpected number of instrumentation statements:" + numberofStatements + "|" + js);
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT, "Unsupported Cap instrumentation script");
            }
            final Map<String, Object> state = new LinkedHashMap<String, Object>();
            for (final String var : vars) {
                state.put(var, values.get(var));
            }
            final int numberofFontStacks = new Regex(fontStacks, "\"[^\"]*\"").getColumn(-1).length;
            final Map<String, Object> result = new LinkedHashMap<String, Object>();
            result.put("i", id);
            result.put("state", state);
            result.put("p", buildProbe(userAgent, numberofFontStacks));
            result.put("ts", System.currentTimeMillis());
            /* Added by the widget: [window.innerWidth, window.innerHeight] */
            final List<Object> viewport = new ArrayList<Object>();
            viewport.add(screenWidth);
            viewport.add(innerHeight);
            result.put("vp", viewport);
            return result;
        }

        /** Browser facts collected by the probe of the instrumentation script, checked by core/src/detect.js. */
        protected Map<String, Object> buildProbe(final String userAgent, final int numberofFontStacks) {
            final boolean gecko = StringUtils.containsIgnoreCase(userAgent, "Firefox/");
            final Map<String, Object> probe = new LinkedHashMap<String, Object>();
            if (!gecko) {
                probe.put("deviceMemory", deviceMemory);
                final Map<String, Object> uaData = new LinkedHashMap<String, Object>();
                uaData.put("mobile", Boolean.FALSE);
                final String chromeVersion = new Regex(userAgent, "Chrome/(\\d+)").getMatch(0);
                final List<Object> brands = new ArrayList<Object>();
                brands.add("Chromium/" + (chromeVersion != null ? chromeVersion : "140"));
                brands.add("Not=A?Brand/24");
                brands.add("Google Chrome/" + (chromeVersion != null ? chromeVersion : "140"));
                uaData.put("brands", brands);
                probe.put("uaData", uaData);
            } else {
                probe.put("uaData", null);
            }
            probe.put("pdfViewerEnabled", Boolean.TRUE);
            /*
             * Real browsers return fractional text widths, whole pixel values are considered as automated browser ("geometry_quantized").
             */
            final List<Object> fontWidths = new ArrayList<Object>();
            for (int i = 0; i < numberofFontStacks; i++) {
                double width = Math.round((fontWidthBase + fontWidthStep * i + random.nextDouble() * 3) * 100) / 100.0d;
                if (width == Math.floor(width)) {
                    width += 0.01d;
                }
                fontWidths.add(width);
            }
            probe.put("fontWidths", fontWidths);
            probe.put("ua", userAgent);
            final Map<String, Object> engine = new LinkedHashMap<String, Object>();
            engine.put("hasMozInnerScreenX", gecko);
            engine.put("hasChrome", !gecko);
            probe.put("engine", engine);
            final Map<String, Object> screen = new LinkedHashMap<String, Object>();
            screen.put("width", screenWidth);
            screen.put("height", screenHeight);
            probe.put("screen", screen);
            probe.put("productSub", gecko ? "20100101" : "20030107");
            probe.put("oscpu", gecko ? "Windows NT 10.0; Win64; x64" : "__undefined");
            final List<Object> outerWH = new ArrayList<Object>();
            outerWH.add(screenWidth);
            outerWH.add(outerHeight);
            probe.put("outerWH", outerWH);
            probe.put("uaDataPresent", !gecko);
            probe.put("webdriver", Boolean.FALSE);
            probe.put("isExtended", Boolean.FALSE);
            final Map<String, Object> tamper = new LinkedHashMap<String, Object>();
            tamper.put("getParameterWebGL", Boolean.TRUE);
            tamper.put("toDataURL", Boolean.TRUE);
            tamper.put("getImageData", Boolean.TRUE);
            tamper.put("permissionsQuery", Boolean.TRUE);
            tamper.put("fnToString", Boolean.TRUE);
            probe.put("tamper", tamper);
            final Map<String, Object> plugins = new LinkedHashMap<String, Object>();
            plugins.put("length", 5);
            probe.put("plugins", plugins);
            return probe;
        }
    }
}
