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
package org.appwork.storage.commonInterface;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Arrays;
import java.util.HashSet;

import org.appwork.exceptions.WTFException;
import org.appwork.serializer.SC;
import org.appwork.storage.TypeRef;

/**
 * @author thomas
 * @date 15.09.2023
 *
 */
public abstract class AbstractSerializer implements SerializerInterface {
    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.SerializerInterface#toByteArray(java.lang.Object, boolean)
     */
    @Override
    public byte[] toByteArray(Object o, Object... context) throws SerializerException {
        try {
            final ByteArrayOutputStream bout = new ByteArrayOutputStream();
            toStream(o, bout, true, context);
            return bout.toByteArray();
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        }
    }

    /**
     * @param context
     * @param withDocumentation
     * @return
     */
    protected boolean contextContainsAll(Object[] context, Object... query) {
        if (context == null) {
            return false;
        }
        HashSet<Object> set = new HashSet<Object>(Arrays.asList(query));
        for (Object o : context) {
            set.remove(o);
            if (set.size() == 0) {
                return true;
            }
        }
        return false;
    }

    public boolean isContextPretty(Object... context) {
        boolean pretty = false;
        boolean noNewLine = false;
        for (Object c : context) {
            if (c == SC.READABLE || c == SC.CONFIG_FILE || c == SC.LOG_MULTILINE || c == SC.WITH_DOCUMENTATION || c == SC.LOG_MULTILINE) {
                pretty = true;
            }
            if (c == SC.SINGLE_LINE) {
                noNewLine = true;
            }
        }
        if (pretty && noNewLine) {
            throw new WTFException("Missleading Serializer Context: " + Arrays.asList(context));
        }
        return pretty;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.commonInterface.SerializerInterface#fromByteArray(byte[], org.appwork.storage.TypeRef)
     */
    @Override
    public <T> T fromByteArray(byte[] byteArray, TypeRef<T> type, Object... context) throws SerializerException {
        try {
            return fromStream(new ByteArrayInputStream(byteArray), type, context);
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        }
    }
}
