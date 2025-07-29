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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import jd.PluginWrapper;
import jd.controlling.ProgressController;
import jd.nutils.encoding.Encoding;
import jd.parser.Regex;
import jd.plugins.CryptedLink;
import jd.plugins.DecrypterPlugin;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForDecrypt;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.encoding.Base64;
import org.appwork.utils.formatter.HexFormatter;
import org.mozilla.javascript.ClassShutter;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.ContextFactory;
import org.mozilla.javascript.Scriptable;

@DecrypterPlugin(revision = "$Revision: 51264 $", interfaceVersion = 3, names = { "dummycnl.jdownloader.org" }, urls = { "https?://dummycnl\\.jdownloader\\.org/#?[a-f0-9A-F]+" })
public class DummyCNL extends PluginForDecrypt {
    public DummyCNL(final PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    protected DownloadLink createDownloadlink(String link) {
        DownloadLink ret = super.createDownloadlink(link);
        try {
            ret.setUrlProtection(org.jdownloader.controlling.UrlProtection.PROTECTED_DECRYPTER);
        } catch (Throwable e) {
        }
        return ret;
    }

    public static DownloadLink createDummyCNL(String crypted, final String jk, String k, final String source) throws Exception {
        final Map<String, String> infos = new HashMap<String, String>();
        infos.put("crypted", crypted);
        if (jk != null) {
            infos.put("jk", jk);
        }
        if (k != null) {
            infos.put("k", k);
        }
        if (source != null) {
            infos.put("source", source);
        }
        final String json = JSonStorage.toString(infos);
        return new DownloadLink(null, null, "dummycnl.jdownloader.org", "https://dummycnl.jdownloader.org/#" + HexFormatter.byteArrayToHex(json.getBytes("UTF-8")), true);
    }

    @Override
    public ArrayList<DownloadLink> decryptIt(final CryptedLink param, final ProgressController progress) throws Exception {
        final ArrayList<DownloadLink> decryptedLinks = new ArrayList<DownloadLink>();
        final String parameter = param.toString();
        final String hex = new Regex(parameter, "https?://dummycnl\\.jdownloader\\.org/#?([a-f0-9A-F]+)").getMatch(0);
        final Map<String, String> params = restoreFromString(new String(HexFormatter.hexToByteArray(hex), "UTF-8"), TypeRef.HASHMAP_STRING);
        final String crypted = params.get("crypted");
        if (crypted == null || crypted.trim().length() == 0) {
            return decryptedLinks;
        }
        final String decrypted = decrypt(crypted, params.get("jk"), params.get("k"));
        if (decrypted != null) {
            // we want to format all protocols not just common ones.. otherwise this will be a pain in the ass to maintain.
            decrypted.replaceAll(" ((?:[a-z]+)://)", "\r\n$1");
        }
        final String source = params.get("source");
        final String packageName = params.get("package");
        final FilePackage fp;
        if (packageName != null) {
            fp = FilePackage.getInstance();
            fp.setAllowMerge(true);
            fp.setName(packageName);
        } else {
            fp = null;
        }
        for (final String s : Regex.getLines(decrypted)) {
            final DownloadLink dl = createDownloadlink(s);
            // respect the source url as container url assuming another plugin hasn't set this field.
            if (source != null && dl.getContainerUrl() == null) {
                dl.setReferrerUrl(Encoding.urlDecode(source, false));
            }
            if (fp != null) {
                fp.add(dl);
            }
            distribute(dl);
            decryptedLinks.add(dl);
        }
        return decryptedLinks;
    }

    /* decrypt given crypted string with js encrypted aes key */
    private String decrypt(String crypted, final String jk, String k) throws Exception {
        byte[] key = null;
        if (jk != null) {
            try {
                Context cx = null;
                try {
                    cx = ContextFactory.getGlobal().enterContext();
                    cx.setClassShutter(new ClassShutter() {
                        public boolean visibleToScripts(String className) {
                            if (className.startsWith("adapter")) {
                                return true;
                            } else {
                                throw new RuntimeException("Security Violation");
                            }
                        }
                    });
                } catch (java.lang.SecurityException e) {
                    /* in case classshutter already set */
                }
                Scriptable scope = cx.initStandardObjects();
                String fun = jk + "  f()";
                Object result = cx.evaluateString(scope, fun, "<cmd>", 1, null);
                key = HexFormatter.hexToByteArray(Context.toString(result));
            } finally {
                try {
                    Context.exit();
                } catch (final Throwable e) {
                }
            }
        } else {
            key = HexFormatter.hexToByteArray(k);
        }
        /* workaround for wrong relink post encoding! */
        byte[] baseDecoded = Base64.decode(crypted.trim().replaceAll("\\s", "+"));
        if (baseDecoded == null) {
            baseDecoded = Base64.decode(crypted.replaceAll("\\s", "+"));
        }
        if (baseDecoded == null) {
            throw new PluginException(LinkStatus.ERROR_PLUGIN_DEFECT);
        }
        final String ret = decrypt(baseDecoded, key);
        if (ret != null) {
            return ret.trim();
        } else {
            return null;
        }
    }

    private String decrypt(byte[] b, byte[] key) {
        try {
            final IvParameterSpec ivSpec = new IvParameterSpec(key);
            final SecretKeySpec skeySpec = new SecretKeySpec(key, "AES");
            final Cipher cipher = Cipher.getInstance("AES/CBC/NoPadding");
            cipher.init(Cipher.DECRYPT_MODE, skeySpec, ivSpec);
            return new String(cipher.doFinal(b), "UTF-8");
        } catch (Throwable e) {
            logger.log(e);
            return null;
        }
    }

    @Override
    public Boolean siteTesterDisabled() {
        return Boolean.TRUE;
    }
}