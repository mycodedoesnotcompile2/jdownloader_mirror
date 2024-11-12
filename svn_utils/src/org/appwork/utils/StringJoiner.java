/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
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

/**
 * @author thomas
 * @date 11.11.2024
 *
 */
public class StringJoiner {
    private String separator;

    /**
     * @param separator
     */
    public StringJoiner(String separator) {
        this.separator = separator;
    }

    private boolean skipEmptyOrNullElements = false;

    /**
     * @return the skipEmptyOrNullElements
     */
    public boolean isSkipEmptyOrNullElements() {
        return skipEmptyOrNullElements;
    }

    /**
     * @param skipEmptyOrNullElements
     *            the skipEmptyOrNullElements to set
     * @return
     */
    public StringJoiner skipEmptyOrNullElements(boolean skipEmptyOrNullElements) {
        this.skipEmptyOrNullElements = skipEmptyOrNullElements;
        return this;
    }

    /**
     * @param skipEmptyOrNullElements
     *            the skipEmptyOrNullElements to set
     */
    public void setSkipEmptyOrNullElements(boolean skipEmptyOrNullElements) {
        this.skipEmptyOrNullElements = skipEmptyOrNullElements;
    }

    public String join(Collection<?> params) {
        return join(params.toArray(new Object[0]));
    }

    /**
     * @param parameters
     * @return
     */
    public String join(Object... parameters) {
        StringBuilder sb = new StringBuilder();
        int added = 0;
        for (Object s : parameters) {
            if (skip(s)) {
                continue;
            }
            addSeparator(added, s, parameters, sb);
            String toString = elementToString(s);
            added++;
            sb.append(toString);
        }
        return sb.toString();
    }

    /**
     * @param s
     * @return
     */
    protected boolean skip(Object s) {
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
}
