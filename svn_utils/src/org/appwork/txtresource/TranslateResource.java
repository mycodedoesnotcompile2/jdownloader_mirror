/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.txtresource;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.net.URL;

import org.appwork.utils.URLStream;

/**
 * @author thomas
 *
 */
public class TranslateResource {
    /**
     *
     */
    private TranslateData data;
    private final String  name;
    private URL           primaryUrl;
    private URL           fallbackURL;
    private boolean       fallbackRead;

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        if (primaryUrl == null) {
            return "TranslateResource " + name + " (Interface Fallback)";
        }
        return "TranslateResource " + name + " (" + primaryUrl + "/" + fallbackURL + ")";
    }

    /**
     * @param urlFileSystem
     * @param urlClasspath
     */
    public TranslateResource(final URL urlFileSystem, URL urlClassPath, final String name) {
        if (urlFileSystem != null) {
            this.primaryUrl = urlFileSystem;
            if (urlClassPath != null) {
                this.fallbackURL = urlClassPath;
            }
        } else {
            this.primaryUrl = urlClassPath;
        }
        this.name = name;
    }

    public TranslateData getData() {
        if (this.data == null) {
            if (primaryUrl != null) {
                try {
                    final String txt = this.read(primaryUrl);
                    this.data = TranslationUtils.restoreFromString(txt, TranslateData.class);
                } catch (final Throwable e) {
                    org.appwork.loggingv3.LogV3.severe("Error in Translation File: " + primaryUrl);
                    org.appwork.loggingv3.LogV3.log(e);
                    this.data = new TranslateData();
                }
            }
        }
        return this.data;
    }

    /**
     * @param method
     * @return
     * @throws IOException
     * @throws UnsupportedEncodingException
     */
    public TranslatedEntry getEntry(final Method method) throws UnsupportedEncodingException, IOException {
        // if we have no translation files, but only defaults, read them
        // directly
        if (primaryUrl == null) {
            return this.readDefaults(method);
        } else {
            final TranslatedEntry ret = getEntryFromFile(method);
            if (ret == null) {
                return this.readDefaults(method);
            } else {
                return ret;
            }
        }
    }

    protected TranslatedEntry getEntryFromFile(final Method method) {
        TranslatedEntry ret = null;
        ret = this.getData().get(method.getName());
        if (ret == null) {
            if (fallbackURL != null && !fallbackRead) {
                fallbackRead = true;
                try {
                    final String txt = this.read(fallbackURL);
                    data.merge(TranslationUtils.restoreFromString(txt, TranslateData.class));
                } catch (final Throwable e) {
                    org.appwork.loggingv3.LogV3.severe("Error in Translation File: " + fallbackURL);
                    org.appwork.loggingv3.LogV3.log(e);
                }
            }
        }
        return ret;
    }

    /**
     * Use this method to check the origin of the translation. the method returns null (not found) or a TranslationSource object.
     *
     * @param method
     * @return
     */
    public TranslationSource getSource(Method method) {
        if (primaryUrl == null) {
            final Default lngAn = method.getAnnotation(Default.class);
            if (lngAn != null) {
                for (int i = 0; i < lngAn.lngs().length; i++) {
                    if (lngAn.lngs()[i].equals(this.name)) {
                        return new TranslationSource(this, method, false);
                    }
                }
            }
            return null;
        }
        if (this.getEntryFromFile(method) != null) {
            return new TranslationSource(this, method, true);
        }
        final Default lngAn = method.getAnnotation(Default.class);
        if (lngAn != null) {
            for (int i = 0; i < lngAn.lngs().length; i++) {
                if (lngAn.lngs()[i].equals(this.name)) {
                    return new TranslationSource(this, method, false);
                }
            }
        }
        return null;
    }

    /**
     * @return
     */
    public String getName() {
        return this.name;
    }

    /**
     * @param url
     * @return
     * @throws IOException
     * @throws UnsupportedEncodingException
     */
    private String read(final URL url) throws UnsupportedEncodingException, IOException {
        BufferedReader f = null;
        InputStreamReader isr = null;
        InputStream is = null;
        try {
            is = URLStream.openStream(url);
            f = new BufferedReader(isr = new InputStreamReader(is, "UTF8"));
            String line;
            final StringBuilder ret = new StringBuilder();
            final String sep = System.getProperty("line.separator");
            while ((line = f.readLine()) != null) {
                if (ret.length() == 0 && line.startsWith("\uFEFF")) {
                    /*
                     * Workaround for this bug: http://bugs.sun.com/view_bug.do?bug_id=4508058
                     * http://bugs.sun.com/view_bug.do?bug_id=6378911
                     */
                    org.appwork.loggingv3.LogV3.warning(url + " is UTF-8 with BOM. Please remove BOM");
                    line = line.substring(1);
                }
                // ignore comments
                if (line.trim().startsWith("//")) {
                    continue;
                }
                if (ret.length() > 0) {
                    ret.append(sep);
                }
                ret.append(line);
            }
            return ret.toString();
        } finally {
            try {
                is.close();
            } catch (final Throwable e) {
            }
            try {
                f.close();
            } catch (final Throwable e) {
            }
            try {
                isr.close();
            } catch (final Throwable e) {
            }
        }
    }

    /**
     * @param method
     * @return
     */
    public TranslatedEntry readDefaults(final Method method) {
        // check Annotations. this is slow, but ok for dev enviroment.
        // for release, we always should create real translation files
        // instead of using the annotations
        final Default lngAn = method.getAnnotation(Default.class);
        if (lngAn == null) {
            org.appwork.loggingv3.LogV3.warning("Default missing for: " + method);
            return null;
        }
        for (int i = 0; i < lngAn.lngs().length; i++) {
            if (lngAn.lngs()[i].equals(this.name)) {
                // TODO: think about createing a cache. Since default values should hardly be used on production state, this is not THAT
                // important
                return new TranslatedEntry(lngAn.values()[i]);
            }
        }
        return null;
    }

    /**
     * @deprecated only used by jd translater extension
     * @return
     */
    public URL getUrl() {
        return primaryUrl;
    }
}
