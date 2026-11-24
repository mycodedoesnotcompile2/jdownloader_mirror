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

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.storage.TypeRef;
import org.appwork.utils.DebugMode;
import org.appwork.utils.IO;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.parser.UrlQuery;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.http.Browser;
import jd.http.URLConnectionAdapter;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.Account;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;
import jd.plugins.hoster.DirectHTTP;
import jd.plugins.hoster.GenericM3u8;

@DecrypterPlugin(revision = "$Revision: 54111 $", interfaceVersion = 3, names = {}, urls = {})
public class VidsrcCrawler extends PluginForDecrypt {
    public VidsrcCrawler(PluginWrapper wrapper) {
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
        /* each entry in List<String[]> will result in one PluginForDecrypt, Plugin.getHost() will return String[0]->main domain */
        ret.add(new String[] { "vidsrc.sh", "vidsrc.net", "vidsrcme.ru", "vidsrcme.su", "vsrc.su", "vidsrc-embed.ru", "vidsrc-embed.su", "vsdash.net" });
        return ret;
    }

    /** All domains except the main domain "vidsrc.sh" are dead. Links with dead domains are changed to the main domain. */
    protected List<String> getDeadDomains() {
        final ArrayList<String> deadDomains = new ArrayList<String>();
        /* 2026-11-23 */
        deadDomains.add("vidsrc.net");
        deadDomains.add("vidsrcme.ru");
        deadDomains.add("vidsrcme.su");
        deadDomains.add("vsrc.su");
        deadDomains.add("vidsrc-embed.ru");
        deadDomains.add("vidsrc-embed.su");
        deadDomains.add("vsdash.net");
        return deadDomains;
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

    /** Possible types of links: See examples in API docs: https://vidsrc-embed.ru/api/ */
    private static final Pattern TYPE_MOVIE   = Pattern.compile("/embed/movie/((tt)?\\d+).*", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_MOVIE_2 = Pattern.compile("/embed/movie\\?(imdb|tmdb)=.+", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_TV      = Pattern.compile("/embed/tv/((tt)?\\d+)(/(\\d+)-(\\d+))?.*", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_TV_2    = Pattern.compile("/embed/tv\\?(imdb|tmdb)=.+", Pattern.CASE_INSENSITIVE);

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:www\\.)?" + buildHostsPatternPart(domains) + "(" + TYPE_MOVIE.pattern() + "|" + TYPE_MOVIE_2.pattern() + "|" + TYPE_TV.pattern() + "|" + TYPE_TV_2.pattern() + ")");
        }
        return ret.toArray(new String[0]);
    }

    private static String getOrigin(final String url) {
        return new Regex(url, "^(https?://[^/]+)").getMatch(0);
    }

    /**
     * 2026-11-23: Flow: embed page -> "data-api" (JSON with "src") -> landing page (CFG with "playerUrl" and "metaApi") -> player page
     * (CONFIG with "api"/"streamBase" and a single-use "apiToken") -> stream API (encrypted "stream_urls", decryptor is a small WASM module
     * which changes every 5 minutes).
     */
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>();
        String contenturl = param.getCryptedUrl();
        final String addedLinkDomain = Browser.getHost(contenturl, true);
        if (getDeadDomains().contains(addedLinkDomain)) {
            /* Replace dead domain with the working main domain */
            contenturl = contenturl.replaceFirst(Pattern.quote(addedLinkDomain), this.getHost());
            logger.info("Corrected domain in added URL: " + addedLinkDomain + " --> " + this.getHost());
        }
        br.getPage(contenturl);
        if (br.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final String embedOrigin = getOrigin(br.getURL());
        final String dataApi = br.getRegex("data-api=\"([^\"]+)\"").getMatch(0);
        if (dataApi == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String dataApiDecoded = Encoding.htmlOnlyDecode(dataApi);
        br.getPage(dataApiDecoded);
        final Map<String, Object> srcMap = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
        /* Landing page of the player host */
        br.getPage((String) srcMap.get("src"));
        final String playerOrigin = getOrigin(br.getURL());
        final String cfgJson = br.getRegex("window\\.CFG\\s*=\\s*(\\{.*?\\});\\s*</script>").getMatch(0);
        if (cfgJson == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Map<String, Object> cfg = restoreFromString(cfgJson, TypeRef.MAP);
        if (((Boolean) cfg.get("isTvIndex")).booleanValue()) {
            /* TV show without season/episode -> Collect links to all episodes which will be crawled separately */
            br.getPage((String) cfg.get("metaApi"));
            final Map<String, Object> metaEntries = restoreFromString(br.getRequest().getHtmlCode(), TypeRef.MAP);
            final Map<String, Object> metaData = (Map<String, Object>) metaEntries.get("data");
            final Map<String, Object> episodes = (Map<String, Object>) metaData.get("eps");
            final String id = UrlQuery.parse(dataApiDecoded).get("id");
            final String idType = id.matches("(?i)tt\\d+") ? "imdb" : "tmdb";
            for (final Map.Entry<String, Object> season : episodes.entrySet()) {
                for (final Object episode : (List<Object>) season.getValue()) {
                    ret.add(this.createDownloadlink(embedOrigin + "/embed/tv?" + idType + "=" + id + "&season=" + season.getKey() + "&episode=" + episode));
                }
            }
            final FilePackage fp = FilePackage.getInstance();
            fp.setName(metaData.get("title").toString());
            fp.addLinks(ret);
            if (ret.isEmpty()) {
                throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
            }
            return ret;
        }
        /* We expect a single video stream -> Crawl it */
        br.getHeaders().put("Referer", playerOrigin + "/");
        br.getPage(playerOrigin + (String) cfg.get("playerUrl"));
        final String configJson = br.getRegex("window\\.CONFIG\\s*=\\s*(\\{.*?\\});\\s*</script>").getMatch(0);
        if (configJson == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Map<String, Object> config = restoreFromString(configJson, TypeRef.MAP);
        final String apiToken = (String) config.get("apiToken");
        if (apiToken == null) {
            /* Player is protected by a captcha gate which is not supported (yet). */
            logger.warning("Missing apiToken -> Captcha gate?");
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        String api = (String) config.get("api");
        if (api == null) {
            /* TV episode */
            api = config.get("streamBase") + "&season=" + config.get("season") + "&episode=" + config.get("episode") + "&stream_urls";
        }
        api += "&api_token=" + Encoding.urlEncode(apiToken);
        final Browser brapi = br.cloneBrowser();
        brapi.getHeaders().put("Origin", playerOrigin);
        brapi.getHeaders().put("Referer", playerOrigin + "/");
        brapi.getPage(api);
        if (brapi.getHttpConnection().getResponseCode() == 404) {
            throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND);
        }
        final Map<String, Object> entries = restoreFromString(brapi.getRequest().getHtmlCode(), TypeRef.MAP);
        if (!"200".equals(String.valueOf(entries.get("status_code")))) {
            logger.warning("Unexpected status_code: " + entries.get("status_code") + " | " + entries.get("error"));
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final Map<String, Object> data = (Map<String, Object>) entries.get("data");
        String title = data.get("title").toString();
        if (data.get("season") != null && data.get("episode") != null) {
            title += String.format(" S%02dE%02d", Integer.parseInt(data.get("season").toString()), Integer.parseInt(data.get("episode").toString()));
        }
        /*
         * Obtain stream URLs: They are encrypted using ChaCha20, the decryptor is a WASM module. Only done in debug mode because the HLS
         * stream URLs are blocked by Cloudflare anyway and therefore do not work.
         */
        final List<String> streamUrls = new ArrayList<String>();
        final Object streamUrlsObject = data.get("stream_urls");
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            logger.info("Skipping stream URL decryption: HLS stream URLs are blocked by Cloudflare and do not work");
        } else if (streamUrlsObject instanceof List) {
            for (final Object url : (List<Object>) streamUrlsObject) {
                streamUrls.add(url.toString());
            }
        } else {
            final Map<String, Object> vs = (Map<String, Object>) entries.get("vs");
            final byte[] wasm;
            if (vs.containsKey("wasm_url")) {
                final Browser brwasm = brapi.cloneBrowser();
                final URLConnectionAdapter con = brwasm.openGetConnection((String) vs.get("wasm_url"));
                try {
                    wasm = IO.readStream(-1, con.getInputStream());
                } finally {
                    con.disconnect();
                }
            } else {
                wasm = Base64.decode((String) vs.get("wasm"));
            }
            final String decrypted = new VidsrcWasm(wasm).decrypt((String) streamUrlsObject);
            for (final String url : decrypted.split("\n")) {
                if (url.length() > 0) {
                    streamUrls.add(url);
                    br.getPage(url);
                }
            }
        }
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            /* Do not add any stream link. */
        } else if (streamUrls.isEmpty()) {
            logger.warning("Failed to find video stream link");
        } else {
            /* All URLs are mirrors of the same stream -> Use the first one. */
            final DownloadLink video = this.createDownloadlink(streamUrls.get(0) + "/master.m3u8");
            video.setReferrerUrl(playerOrigin + "/");
            video.setProperty(GenericM3u8.PRESET_NAME_PROPERTY, title);
            /*
             * TODO: Every playlist- and segment-request needs a token ("?token=...", see "<streamhost>/generate.php?api_token=...") which
             * GenericM3u8 does not add. Downloads of this link will fail with HTTP 403 until this is implemented.
             */
            logger.warning("Stream requires a token for every request -> Download will most likely fail");
            ret.add(video);
        }
        /* Subtitles (optional) */
        final List<Map<String, Object>> subs = (List<Map<String, Object>>) entries.get("default_subs");
        if (subs != null) {
            for (final Map<String, Object> sub : subs) {
                final String url_sub = sub.get("url").toString();
                final DownloadLink subtitle = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(url_sub));
                subtitle.setFinalFileName(title + "." + getLastPathSegment(url_sub));
                ret.add(subtitle);
            }
        }
        /* Poster (optional) */
        final String url_poster = (String) data.get("backdrop");
        if (url_poster != null) {
            final DownloadLink image = this.createDownloadlink(DirectHTTP.createURLForThisPlugin(url_poster));
            image.setAvailable(true);
            ret.add(image);
        }
        final FilePackage fp = FilePackage.getInstance();
        fp.setName(title);
        fp.addLinks(ret);
        if (ret.isEmpty()) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        return ret;
    }

    private static String getLastPathSegment(final String url) {
        return url.substring(url.lastIndexOf('/') + 1);
    }

    @Override
    public boolean hasCaptcha(CryptedLink link, Account acc) {
        return false;
    }

    @Override
    public int getMaxConcurrentProcessingInstances() {
        /* 2025-10-15: Try to avoid Cloudflare blocks */
        return 1;
    }

    /**
     * Minimal WebAssembly interpreter (i32 subset, single memory, no imports) which runs the per-window decryptor module of vidsrc. The
     * module exports "alloc(len) -> ptr" and "decrypt(ptr, len) -> plaintextLen". The input is "nonce(12) || ChaCha20 ciphertext", the
     * plaintext is written to "ptr + 12". The decryption key is calculated by the module itself and changes with every module, therefore
     * the module has to be executed instead of re-implementing it.
     */
    private static final class VidsrcWasm {
        private static final int MAX_MEMORY  = 64 * 1024 * 1024;
        private final byte[]     w;
        private int              pos;
        private int[]            typeParams;
        private int[]            typeResults;
        private int[]            funcType;
        private int[]            codeStart;
        private int[]            codeEnd;
        private int[]            localCount;
        private int[]            globalInit  = new int[0];
        private int              memPages    = 0;
        private int              startFunc   = -1;
        private int              allocFunc   = -1;
        private int              decryptFunc = -1;
        private int[]            dataOffset;
        private int[]            dataPos;
        private int[]            dataLen;
        private final int[]      endOf;
        private final int[]      elseOf;
        private byte[]           mem;
        private int[]            globals;

        private VidsrcWasm(final byte[] wasm) {
            this.w = wasm;
            this.endOf = new int[wasm.length];
            this.elseOf = new int[wasm.length];
            parse();
        }

        private int u32() {
            int r = 0;
            int s = 0;
            int b;
            do {
                b = w[pos++] & 0xff;
                r |= (b & 0x7f) << s;
                s += 7;
            } while ((b & 0x80) != 0);
            return r;
        }

        private int s32() {
            int r = 0;
            int s = 0;
            int b;
            do {
                b = w[pos++] & 0xff;
                r |= (b & 0x7f) << s;
                s += 7;
            } while ((b & 0x80) != 0);
            if (s < 32 && (b & 0x40) != 0) {
                r |= -1 << s;
            }
            return r;
        }

        private void skipLeb() {
            while ((w[pos++] & 0x80) != 0) {
            }
        }

        /** Reads the block type and returns the number of result values. */
        private int readBlockArity() {
            final int b = w[pos] & 0xff;
            if (b == 0x40) {
                pos++;
                return 0;
            } else if (b >= 0x6f && b <= 0x7f) {
                pos++;
                return 1;
            } else {
                return typeResults[s32()];
            }
        }

        private void skipImmediates(final int op) {
            if (op >= 0x28 && op <= 0x3e) {
                u32();
                u32();
                return;
            }
            switch (op) {
            case 0x0c:
            case 0x0d:
            case 0x10:
            case 0x20:
            case 0x21:
            case 0x22:
            case 0x23:
            case 0x24:
                u32();
                break;
            case 0x0e:
                final int n = u32();
                for (int i = 0; i <= n; i++) {
                    u32();
                }
                break;
            case 0x11:
                u32();
                u32();
                break;
            case 0x3f:
            case 0x40:
                pos++;
                break;
            case 0x41:
                s32();
                break;
            case 0x42:
                skipLeb();
                break;
            case 0x43:
                pos += 4;
                break;
            case 0x44:
                pos += 8;
                break;
            case 0xfc:
                final int sub = u32();
                if (sub == 10) {
                    pos += 2;
                } else if (sub == 11) {
                    pos++;
                } else {
                    throw new IllegalStateException("Unsupported 0xfc opcode: " + sub);
                }
                break;
            default:
                break;
            }
        }

        private void parse() {
            if (w.length < 8 || w[0] != 0 || w[1] != 'a' || w[2] != 's' || w[3] != 'm') {
                throw new IllegalStateException("Not a wasm module");
            }
            pos = 8;
            while (pos < w.length) {
                final int id = w[pos++] & 0xff;
                final int size = u32();
                final int end = pos + size;
                switch (id) {
                case 1: {
                    final int n = u32();
                    typeParams = new int[n];
                    typeResults = new int[n];
                    for (int i = 0; i < n; i++) {
                        pos++;
                        typeParams[i] = u32();
                        pos += typeParams[i];
                        typeResults[i] = u32();
                        pos += typeResults[i];
                    }
                    break;
                }
                case 2:
                    throw new IllegalStateException("Imports are not supported");
                case 3: {
                    final int n = u32();
                    funcType = new int[n];
                    for (int i = 0; i < n; i++) {
                        funcType[i] = u32();
                    }
                    break;
                }
                case 5: {
                    u32();
                    u32();
                    memPages = u32();
                    break;
                }
                case 6: {
                    final int n = u32();
                    globalInit = new int[n];
                    for (int i = 0; i < n; i++) {
                        pos += 2;
                        if ((w[pos++] & 0xff) != 0x41) {
                            throw new IllegalStateException("Unsupported global init");
                        }
                        globalInit[i] = s32();
                        pos++;
                    }
                    break;
                }
                case 7: {
                    final int n = u32();
                    for (int i = 0; i < n; i++) {
                        final int len = u32();
                        final String name = new String(w, pos, len, StandardCharsets.UTF_8);
                        pos += len;
                        final int kind = w[pos++] & 0xff;
                        final int idx = u32();
                        if (kind == 0) {
                            if ("alloc".equals(name)) {
                                allocFunc = idx;
                            } else if ("decrypt".equals(name)) {
                                decryptFunc = idx;
                            }
                        }
                    }
                    break;
                }
                case 8:
                    startFunc = u32();
                    break;
                case 10: {
                    final int n = u32();
                    codeStart = new int[n];
                    codeEnd = new int[n];
                    localCount = new int[n];
                    for (int i = 0; i < n; i++) {
                        final int bodySize = u32();
                        final int bodyEnd = pos + bodySize;
                        final int groups = u32();
                        int locals = 0;
                        for (int g = 0; g < groups; g++) {
                            locals += u32();
                            pos++;
                        }
                        localCount[i] = locals;
                        codeStart[i] = pos;
                        codeEnd[i] = bodyEnd;
                        scan(pos, bodyEnd);
                        pos = bodyEnd;
                    }
                    break;
                }
                case 11: {
                    final int n = u32();
                    dataOffset = new int[n];
                    dataPos = new int[n];
                    dataLen = new int[n];
                    for (int i = 0; i < n; i++) {
                        final int flag = u32();
                        if (flag == 2) {
                            u32();
                        } else if (flag != 0) {
                            throw new IllegalStateException("Unsupported data segment flag: " + flag);
                        }
                        if ((w[pos++] & 0xff) != 0x41) {
                            throw new IllegalStateException("Unsupported data offset");
                        }
                        dataOffset[i] = s32();
                        pos++;
                        dataLen[i] = u32();
                        dataPos[i] = pos;
                        pos += dataLen[i];
                    }
                    break;
                }
                default:
                    break;
                }
                pos = end;
            }
            if (allocFunc < 0 || decryptFunc < 0) {
                throw new IllegalStateException("Missing exports");
            }
        }

        /** Finds the matching "end"/"else" positions of all blocks of a function body. */
        private void scan(final int start, final int end) {
            final int[] stack = new int[512];
            int sp = 0;
            pos = start;
            while (pos < end) {
                final int opStart = pos;
                final int op = w[pos++] & 0xff;
                switch (op) {
                case 0x02:
                case 0x03:
                case 0x04:
                    readBlockArity();
                    stack[sp++] = opStart;
                    break;
                case 0x05:
                    elseOf[stack[sp - 1]] = opStart;
                    break;
                case 0x0b:
                    if (sp > 0) {
                        endOf[stack[--sp]] = opStart;
                    }
                    break;
                default:
                    skipImmediates(op);
                    break;
                }
            }
        }

        public String decrypt(final String encryptedBase64) {
            mem = new byte[memPages * 65536];
            globals = globalInit.clone();
            for (int i = 0; i < dataOffset.length; i++) {
                System.arraycopy(w, dataPos[i], mem, dataOffset[i], dataLen[i]);
            }
            if (startFunc >= 0) {
                call(startFunc, new int[0]);
            }
            final byte[] enc = Base64.decode(encryptedBase64);
            final int ptr = call(allocFunc, new int[] { enc.length });
            System.arraycopy(enc, 0, mem, ptr, enc.length);
            final int len = call(decryptFunc, new int[] { ptr, enc.length });
            /* The first 12 bytes are the nonce, the plaintext follows */
            return new String(mem, ptr + 12, len, StandardCharsets.UTF_8);
        }

        private int address(final int base, final int offset, final int size) {
            final long a = (base & 0xffffffffL) + (offset & 0xffffffffL);
            if (a + size > mem.length) {
                throw new IllegalStateException("Out of bounds memory access");
            }
            return (int) a;
        }

        private int load(final int a, final int size) {
            int r = 0;
            for (int i = size - 1; i >= 0; i--) {
                r = (r << 8) | (mem[a + i] & 0xff);
            }
            return r;
        }

        private void store(final int a, final int size, final int value) {
            for (int i = 0; i < size; i++) {
                mem[a + i] = (byte) (value >>> (8 * i));
            }
        }

        private static int binary(final int op, final int a, final int b) {
            switch (op) {
            case 0x46:
                return a == b ? 1 : 0;
            case 0x47:
                return a != b ? 1 : 0;
            case 0x48:
                return a < b ? 1 : 0;
            case 0x49:
                return Integer.compareUnsigned(a, b) < 0 ? 1 : 0;
            case 0x4a:
                return a > b ? 1 : 0;
            case 0x4b:
                return Integer.compareUnsigned(a, b) > 0 ? 1 : 0;
            case 0x4c:
                return a <= b ? 1 : 0;
            case 0x4d:
                return Integer.compareUnsigned(a, b) <= 0 ? 1 : 0;
            case 0x4e:
                return a >= b ? 1 : 0;
            case 0x4f:
                return Integer.compareUnsigned(a, b) >= 0 ? 1 : 0;
            case 0x6a:
                return a + b;
            case 0x6b:
                return a - b;
            case 0x6c:
                return a * b;
            case 0x6d:
                return a / b;
            case 0x6e:
                return Integer.divideUnsigned(a, b);
            case 0x6f:
                return a % b;
            case 0x70:
                return Integer.remainderUnsigned(a, b);
            case 0x71:
                return a & b;
            case 0x72:
                return a | b;
            case 0x73:
                return a ^ b;
            case 0x74:
                return a << b;
            case 0x75:
                return a >> b;
            case 0x76:
                return a >>> b;
            case 0x77:
                return Integer.rotateLeft(a, b);
            case 0x78:
                return Integer.rotateRight(a, b);
            default:
                throw new IllegalStateException("Unsupported opcode: 0x" + Integer.toHexString(op));
            }
        }

        private int call(final int func, final int[] args) {
            final int type = funcType[func];
            final int[] locals = new int[typeParams[type] + localCount[func]];
            System.arraycopy(args, 0, locals, 0, typeParams[type]);
            final int[] st = new int[4096];
            int sp = 0;
            /* Label stack. Index 0 is the implicit label of the function body. */
            final int[] lStart = new int[512];
            final int[] lCont = new int[512];
            final int[] lHeight = new int[512];
            final int[] lArity = new int[512];
            final boolean[] lLoop = new boolean[512];
            int lp = 1;
            lStart[0] = -1;
            lCont[0] = codeEnd[func];
            lArity[0] = typeResults[type];
            final int end = codeEnd[func];
            pos = codeStart[func];
            while (pos < end) {
                final int opStart = pos;
                final int op = w[pos++] & 0xff;
                int branch = -1;
                switch (op) {
                case 0x00:
                    throw new IllegalStateException("unreachable executed");
                case 0x01:
                    break;
                case 0x02:
                case 0x03:
                case 0x04: {
                    final int arity = readBlockArity();
                    final int cond = op == 0x04 ? st[--sp] : 1;
                    lStart[lp] = opStart;
                    lLoop[lp] = op == 0x03;
                    lCont[lp] = op == 0x03 ? pos : endOf[opStart] + 1;
                    lHeight[lp] = sp;
                    lArity[lp] = arity;
                    lp++;
                    if (op == 0x04 && cond == 0) {
                        pos = elseOf[opStart] != 0 ? elseOf[opStart] + 1 : endOf[opStart];
                    }
                    break;
                }
                case 0x05:
                    pos = endOf[lStart[lp - 1]];
                    break;
                case 0x0b:
                    lp--;
                    break;
                case 0x0c:
                    branch = u32();
                    break;
                case 0x0d: {
                    final int depth = u32();
                    if (st[--sp] != 0) {
                        branch = depth;
                    }
                    break;
                }
                case 0x0e: {
                    final int n = u32();
                    final int[] targets = new int[n + 1];
                    for (int i = 0; i <= n; i++) {
                        targets[i] = u32();
                    }
                    final int idx = st[--sp];
                    branch = targets[idx < 0 || idx >= n ? n : idx];
                    break;
                }
                case 0x0f:
                    branch = lp - 1;
                    break;
                case 0x10: {
                    final int callee = u32();
                    final int calleeType = funcType[callee];
                    final int[] calleeArgs = new int[typeParams[calleeType]];
                    for (int i = calleeArgs.length - 1; i >= 0; i--) {
                        calleeArgs[i] = st[--sp];
                    }
                    final int saved = pos;
                    final int r = call(callee, calleeArgs);
                    pos = saved;
                    if (typeResults[calleeType] > 0) {
                        st[sp++] = r;
                    }
                    break;
                }
                case 0x1a:
                    sp--;
                    break;
                case 0x1b: {
                    final int c = st[--sp];
                    final int b = st[--sp];
                    final int a = st[--sp];
                    st[sp++] = c != 0 ? a : b;
                    break;
                }
                case 0x20:
                    st[sp++] = locals[u32()];
                    break;
                case 0x21:
                    locals[u32()] = st[--sp];
                    break;
                case 0x22:
                    locals[u32()] = st[sp - 1];
                    break;
                case 0x23:
                    st[sp++] = globals[u32()];
                    break;
                case 0x24:
                    globals[u32()] = st[--sp];
                    break;
                case 0x28:
                case 0x2c:
                case 0x2d:
                case 0x2e:
                case 0x2f: {
                    u32();
                    final int offset = u32();
                    final int size = op == 0x28 ? 4 : (op == 0x2c || op == 0x2d) ? 1 : 2;
                    int v = load(address(st[--sp], offset, size), size);
                    if (op == 0x2c) {
                        v = (byte) v;
                    } else if (op == 0x2e) {
                        v = (short) v;
                    }
                    st[sp++] = v;
                    break;
                }
                case 0x36:
                case 0x3a:
                case 0x3b: {
                    u32();
                    final int offset = u32();
                    final int size = op == 0x36 ? 4 : op == 0x3a ? 1 : 2;
                    final int v = st[--sp];
                    store(address(st[--sp], offset, size), size, v);
                    break;
                }
                case 0x3f:
                    pos++;
                    st[sp++] = mem.length / 65536;
                    break;
                case 0x40: {
                    pos++;
                    final int delta = st[--sp];
                    final int old = mem.length / 65536;
                    final long newSize = (old + (delta & 0xffffffffL)) * 65536L;
                    if (newSize > MAX_MEMORY) {
                        st[sp++] = -1;
                    } else {
                        mem = Arrays.copyOf(mem, (int) newSize);
                        st[sp++] = old;
                    }
                    break;
                }
                case 0x41:
                    st[sp++] = s32();
                    break;
                case 0x45:
                    st[sp - 1] = st[sp - 1] == 0 ? 1 : 0;
                    break;
                case 0x67:
                    st[sp - 1] = Integer.numberOfLeadingZeros(st[sp - 1]);
                    break;
                case 0x68:
                    st[sp - 1] = Integer.numberOfTrailingZeros(st[sp - 1]);
                    break;
                case 0x69:
                    st[sp - 1] = Integer.bitCount(st[sp - 1]);
                    break;
                default:
                    if ((op >= 0x46 && op <= 0x4f) || (op >= 0x6a && op <= 0x78)) {
                        final int b = st[--sp];
                        final int a = st[--sp];
                        st[sp++] = binary(op, a, b);
                    } else {
                        throw new IllegalStateException("Unsupported opcode: 0x" + Integer.toHexString(op));
                    }
                    break;
                }
                if (branch >= 0) {
                    final int target = lp - 1 - branch;
                    final int arity = lLoop[target] ? 0 : lArity[target];
                    System.arraycopy(st, sp - arity, st, lHeight[target], arity);
                    sp = lHeight[target] + arity;
                    if (lLoop[target]) {
                        lp = target + 1;
                    } else {
                        lp = target;
                    }
                    pos = lCont[target];
                }
            }
            return typeResults[type] > 0 ? st[sp - 1] : 0;
        }
    }
}
