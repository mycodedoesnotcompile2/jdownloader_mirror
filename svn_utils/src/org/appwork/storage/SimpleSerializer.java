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
package org.appwork.storage;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.appwork.serializer.SC;
import org.appwork.storage.commonInterface.AbstractSerializer;
import org.appwork.storage.commonInterface.SerializerException;
import org.appwork.storage.commonInterface.SerializerInterface;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.reflection.Clazz;

/**
 * @author thomas
 * @date 15.09.2023
 *
 */
public class SimpleSerializer extends AbstractSerializer implements SerializerInterface {
    private SimpleMapper mapperMinified;
    private SimpleMapper mapperPretty;

    /**
     *
     */
    public SimpleSerializer() {
        mapperMinified = new SimpleMapper();
        mapperMinified.setPrettyPrintEnabled(false);
        mapperPretty = new SimpleMapper();
        mapperPretty.setPrettyPrintEnabled(true);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.commonInterface.SerializerInterface#toString(java.lang.Object, boolean)
     */
    @Override
    public String toString(Object o, Object... context) throws SerializerException {
        try {
            return getMapper(context).objectToString(o);
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        }
    }

    /**
     * @param pretty
     * @return
     */
    private SimpleMapper getMapper(Object... context) {
        if (isContextPretty(context)) {
            return mapperPretty;
        } else {
            return mapperMinified;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.commonInterface.SerializerInterface#fromString(java.lang.String, org.appwork.storage.TypeRef)
     */
    @Override
    public <T> T fromString(String json, TypeRef<T> type, Object... context) throws SerializerException {
        try {
            return mapperMinified.stringToObject(json, type);
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.commonInterface.SerializerInterface#fromStream(java.io.InputStream, org.appwork.storage.TypeRef)
     */
    @Override
    public <T> T fromStream(InputStream stream, TypeRef<T> type, Object... context) throws SerializerException {
        try {
            return mapperMinified.inputStreamToObject(stream, type);
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.commonInterface.SerializerInterface#convert(java.lang.Object, boolean, org.appwork.storage.TypeRef)
     */
    @Override
    public <T> T convert(Object o, TypeRef<T> type, Object... context) throws SerializerException {
        try {
            boolean ensureNewInstances = false;
            for (Object c : context) {
                if (c == SC.ENSURE_NEW_INSTANCES) {
                    ensureNewInstances = true;
                    break;
                }
            }
            if (!ensureNewInstances && Clazz.isInstanceof(o.getClass(), ReflectionUtils.getRaw(type.getType()))) {
                return (T) o;
            }
            return mapperMinified.convert(o, type);
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.commonInterface.SerializerInterface#toStream(java.lang.Object, java.io.OutputStream, boolean, boolean)
     */
    @Override
    public void toStream(Object o, OutputStream os, boolean closeOutputStream, Object... context) throws SerializerException {
        try {
            getMapper(context).writeObject(os, o);
        } catch (Exception e) {
            throw SerializerException.wrap(e);
        } finally {
            if (closeOutputStream) {
                try {
                    os.close();
                } catch (IOException e) {
                    throw new SerializerException(e);
                }
            }
        }
    }
}
