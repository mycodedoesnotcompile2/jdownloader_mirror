/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils;

import java.util.Collection;
import java.util.HashSet;

/**
 * @author thomas
 * @date 11.11.2024
 *
 */
public class Joiner {
    private String separator;

    public String getSeparator() {
        return separator;
    }

    public void setSeparator(String separator) {
        this.separator = separator;
    }

    /**
     * @param separator
     */
    public Joiner(String separator) {
        this.separator = separator;
    }

    private boolean ignoreNull  = false;
    private boolean ignoreEmpty = false;

    /**
     * @return the ignoreEmpty
     */
    public boolean isIgnoreEmpty() {
        return ignoreEmpty;
    }

    /**
     * @param ignoreEmpty
     *            the ignoreEmpty to set
     */
    public void setIgnoreEmpty(boolean ignoreEmpty) {
        this.ignoreEmpty = ignoreEmpty;
    }

    public Joiner ignoreEmpty(boolean ignoreEmpty) {
        this.ignoreEmpty = ignoreEmpty;
        return this;
    }

    /**
     * @return the ignoreNull
     */
    public boolean isIgnoreNull() {
        return ignoreNull;
    }

    public Joiner ignoreNull(boolean ignoreNull) {
        setIgnoreNull(ignoreNull);
        return this;
    }

    /**
     * @param ignoreNull
     *            the ignoreNull to set
     */
    public void setIgnoreNull(boolean ignoreNull) {
        this.ignoreNull = ignoreNull;
    }

    /**
     * add the separator at the start - a leading separator
     */
    private boolean prefix = false;

    public boolean isPrefix() {
        return prefix;
    }

    public void setPrefix(boolean prefix) {
        this.prefix = prefix;
    }

    public Joiner prefix(boolean prefix) {
        this.prefix = prefix;
        return this;
    }

    public boolean isPostfix() {
        return postfix;
    }

    public Joiner postfix(boolean postfix) {
        this.postfix = postfix;
        return this;
    }

    public void setPostfix(boolean postfix) {
        this.postfix = postfix;
    }

    /**
     * add the separator at the end - a trailing separator
     */
    private boolean postfix     = false;
    private boolean ignoreDupes = false;

    public String join(Collection<?> params) {
        return joinInternal(params.toArray(new Object[0]));
    }

    public String join(int[] parameters) {
        Object[] ar = new Object[parameters.length];
        for (int i = 0; i < parameters.length; i++) {
            ar[i] = parameters[i];
        }
        return joinInternal(ar);
    }

    public <T> String join(T... parameters) {
        return joinInternal(parameters);
    }

    public String joinInternal(Object[] parameters) {
        StringBuilder sb = new StringBuilder();
        int added = 0;
        HashSet<Object> dupes = isIgnoreDupes() ? new HashSet<Object>() : null;
        for (Object s : parameters) {
            if (dupes != null && !dupes.add(s)) {
                continue;
            }
            if (skip(s)) {
                continue;
            }
            addSeparator(added, s, parameters, sb);
            String toString = elementToString(s);
            added++;
            sb.append(toString);
        }
        if (isPostfix()) {
            sb.append(getSeparator(added, null, parameters, sb));
        }
        if (isPrefix()) {
            sb.insert(0, getSeparator(-1, null, parameters, sb));
        }
        return sb.toString();
    }

    /**
     * @param s
     * @return
     */
    protected boolean skip(Object s) {
        if (isIgnoreNull() && s == null) {
            return true;
        }
        if (s instanceof String) {
            if (isIgnoreEmpty() && StringUtils.isEmpty((String) s)) {
                return true;
            }
        }
        return false;
    }

    protected Object getSeparator(int addedElements, Object element, Object[] parameters, StringBuilder sb) {
        return separator;
    };

    /**
     * @param s
     * @return
     */
    protected String elementToString(Object s) {
        return String.valueOf(s);
    }

    /**
     * @param i
     * @param s
     * @param parameters
     * @param sb
     */
    protected void addSeparator(int addedElements, Object element, Object[] parameters, StringBuilder sb) {
        if (addedElements > 0) {
            sb.append(elementToString(getSeparator(addedElements, element, parameters, sb)));
        }
    }

    /**
     * @param b
     * @return
     */
    public Joiner ignoreDupes(boolean ignoreDupes) {
        this.ignoreDupes = ignoreDupes;
        return this;
    }

    /**
     * @return the ignoreDupes
     */
    public boolean isIgnoreDupes() {
        return ignoreDupes;
    }

    /**
     * @param ignoreDupes
     *            the ignoreDupes to set
     */
    public void setIgnoreDupes(boolean ignoreDupes) {
        this.ignoreDupes = ignoreDupes;
    }
}
