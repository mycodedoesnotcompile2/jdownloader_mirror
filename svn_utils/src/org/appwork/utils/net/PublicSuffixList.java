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
package org.appwork.utils.net;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.appwork.utils.Application;
import org.appwork.utils.ByteArrayMapKey;
import org.appwork.utils.ByteArrayWrapper;
import org.appwork.utils.Regex;
import org.appwork.utils.URLStream;

/**
 * @author daniel
 *
 *         https://publicsuffix.org/list/
 */
public class PublicSuffixList {
    private static PublicSuffixList INSTANCE;
    static {
        try {
            PublicSuffixList.INSTANCE = new PublicSuffixList();
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }

    public static PublicSuffixList getInstance() {
        return PublicSuffixList.INSTANCE;
    }

    /**
     * Implementation for https://publicsuffix.org/list/
     */
    private final Map<ByteArrayMapKey, List<ByteArrayWrapper>> map;
    private final Charset                                      UTF8;

    public PublicSuffixList(URL publicSuffixList) throws IOException {
        UTF8 = Charset.forName("UTF-8");
        this.map = this.parse(publicSuffixList);
    }

    private PublicSuffixList() throws IOException {
        this(Application.getRessourceURL("org/appwork/utils/net/effective_tld_names.dat"));
    }

    public String getSubDomain(final String fullDomain) {
        final String domain = getDomain(fullDomain);
        if (domain != null) {
            final String pattern = "(.+)\\." + Pattern.quote(domain);
            final String subDomain = new Regex(fullDomain, pattern).getMatch(0);
            return subDomain;
        }
        return null;
    }

    public String getDomain(String fullDomain) {
        final String topLeveLDomain = this.getTopLevelDomain(fullDomain);
        if (topLeveLDomain != null) {
            final String pattern = "([^\\.]+\\." + Pattern.quote(topLeveLDomain) + ")";
            final String domain = new Regex(fullDomain, pattern).getMatch(0);
            return domain;
        }
        return null;
    }

    public String getTopLevelDomain(String fullDomain) {
        if (fullDomain != null) {
            final int tldIndex = fullDomain.lastIndexOf('.');
            if (tldIndex > 0 && tldIndex + 1 < fullDomain.length()) {
                final String tld = fullDomain.substring(tldIndex + 1);
                final ByteArrayMapKey tldbA = new ByteArrayMapKey(tld.getBytes(UTF8));
                final List<ByteArrayWrapper> list = this.map.get(tldbA);
                if (list != null) {
                    if (list.size() == 0) {
                        return tld;
                    }
                    final List<String> hits = new ArrayList<String>();
                    hits.add(tld);
                    for (ByteArrayWrapper entry : list) {
                        final String item = entry.toString(UTF8);
                        if (item.startsWith("*")) {
                            final String pattern = "([^\\.]+" + Pattern.quote(item.substring(1)) + ")";
                            final String domain = new Regex(fullDomain, pattern).getMatch(0);
                            if (domain != null) {
                                hits.add(domain);
                            }
                        } else if (item.startsWith("!") && fullDomain.contains(item.substring(1))) {
                            return item.substring(item.indexOf('.') + 1);
                        } else if (fullDomain.contains(item)) {
                            hits.add(item);
                        }
                    }
                    Collections.sort(hits, new Comparator<String>() {
                        public int compare(int x, int y) {
                            return x < y ? 1 : x == y ? 0 : -1;
                        }

                        @Override
                        public int compare(String o1, String o2) {
                            return this.compare(o1.length(), o2.length());
                        }
                    });
                    return hits.get(0);
                }
            }
        }
        return null;
    }

    protected Map<ByteArrayMapKey, List<ByteArrayWrapper>> parse(final URL publicSuffixList) throws IOException {
        final HashMap<ByteArrayMapKey, List<ByteArrayWrapper>> map = new HashMap<ByteArrayMapKey, List<ByteArrayWrapper>>();
        if (publicSuffixList != null) {
            final InputStream is = URLStream.openStream(publicSuffixList);
            if (is != null) {
                try {
                    final BufferedReader reader = new BufferedReader(new InputStreamReader(is, "UTF-8"));
                    final List<ByteArrayWrapper> emptyList = new ArrayList<ByteArrayWrapper>(0);
                    String line = null;
                    while ((line = reader.readLine()) != null) {
                        if (!line.startsWith("//") && line.length() > 0) {
                            final String tld;
                            final int tldIndex = line.lastIndexOf('.');
                            if (tldIndex == -1) {
                                tld = line;
                            } else {
                                tld = line.substring(tldIndex + 1);
                            }
                            final ByteArrayMapKey tldbA = new ByteArrayMapKey(tld.getBytes(UTF8));
                            List<ByteArrayWrapper> list = map.get(tldbA);
                            if (list == null) {
                                list = emptyList;
                                map.put(tldbA, list);
                            }
                            if (tldIndex > 0) {
                                if (list == emptyList) {
                                    list = new ArrayList<ByteArrayWrapper>();
                                    map.put(tldbA, list);
                                }
                                list.add(new ByteArrayWrapper(line.getBytes(UTF8), false));
                            }
                        }
                    }
                    for (List<ByteArrayWrapper> list : map.values()) {
                        if (list != emptyList && list instanceof ArrayList) {
                            ((ArrayList) list).trimToSize();
                        }
                    }
                } finally {
                    is.close();
                }
            }
        }
        return map;
    }
}
