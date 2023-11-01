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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CopyOnWriteArraySet;

/**
 * @author daniel
 *
 */
public class HeaderCollection implements Iterable<HTTPHeader> {
    private final CopyOnWriteArrayList<HTTPHeader> collection            = new CopyOnWriteArrayList<HTTPHeader>();
    private final CopyOnWriteArraySet<String>      allowedDuplicatedKeys = new CopyOnWriteArraySet<String>();

    public HeaderCollection() {
        this.allowedDuplicatedKeys.add("Set-Cookie".toLowerCase(Locale.ENGLISH));
    }

    public HTTPHeader add(final HTTPHeader header) {
        final HTTPHeader existingHeader = this.get(header.getKey());
        if (existingHeader != null) {
            if (!this.allowedDuplicatedKeys.contains(header.getKey().toLowerCase(Locale.ENGLISH))) {
                if (existingHeader.isAllowOverwrite()) {
                    final int index = indexOf(existingHeader);
                    return collection.set(index, header);
                } else {
                    return null;
                }
            }
        }
        this.collection.add(header);
        return null;
    }

    public void clear() {
        this.collection.clear();
    }

    @Override
    public HeaderCollection clone() {
        final HeaderCollection ret = new HeaderCollection();
        ret.allowedDuplicatedKeys.clear();
        ret.allowedDuplicatedKeys.addAll(this.getAllowedDuplicatedKeys());
        ret.collection.addAll(this.collection);
        return ret;
    }

    public HTTPHeader get(final int index) {
        return this.collection.get(index);
    }

    public int indexOf(HTTPHeader header) {
        return collection.indexOf(header);
    }

    public HTTPHeader get(final String key) {
        if (key == null) {
            return null;
        }
        for (final HTTPHeader header : this.collection) {
            if (header.getKey() == null) {
                if (key == null) {
                    return header;
                }
            } else {
                if (header.getKey().equalsIgnoreCase(key)) {
                    return header;
                }
            }
        }
        return null;
    }

    public List<HTTPHeader> getAll(final String key) {
        final ArrayList<HTTPHeader> ret = new ArrayList<HTTPHeader>();
        for (final HTTPHeader header : this.collection) {
            if (header.getKey().equalsIgnoreCase(key)) {
                ret.add(header);
            }
        }
        if (ret.size() > 0) {
            return ret;
        }
        return null;
    }

    public CopyOnWriteArraySet<String> getAllowedDuplicatedKeys() {
        return this.allowedDuplicatedKeys;
    }

    public String getValue(final String key) {
        final HTTPHeader ret = this.get(key);
        if (ret != null) {
            return ret.getValue();
        } else {
            return null;
        }
    }

    @Override
    public Iterator<HTTPHeader> iterator() {
        return this.collection.iterator();
    }

    public HTTPHeader remove(final HTTPHeader header) {
        if (this.collection.remove(header)) {
            return header;
        } else {
            return this.remove(header.getKey());
        }
    }

    public HTTPHeader remove(final String key) {
        final HTTPHeader existingHeader = this.get(key);
        if (existingHeader != null) {
            if (this.collection.remove(existingHeader)) {
                return existingHeader;
            }
        }
        return null;
    }

    public int size() {
        return this.collection.size();
    }

    @Override
    public String toString() {
        return this.collection.toString();
    }

    /**
     * @param httpHeader
     * @return
     */
    public boolean addIfAbsent(HTTPHeader httpHeader) {
        if (get(httpHeader.getKey()) == null) {
            add(httpHeader);
            return true;
        } else {
            return false;
        }
    }
}
