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
package org.appwork.storage.config.test;

import java.util.ArrayList;
import java.util.Date;

import org.appwork.storage.StorableValidatorIgnoresMissingGetter;
import org.appwork.storage.StorableValidatorIgnoresMissingSetter;
import org.appwork.storage.config.ConfigInterface;
import org.appwork.storage.config.annotations.CryptedStorage;
import org.appwork.storage.config.annotations.DefaultBooleanArrayValue;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultByteArrayValue;
import org.appwork.storage.config.annotations.DefaultByteValue;
import org.appwork.storage.config.annotations.DefaultDoubleArrayValue;
import org.appwork.storage.config.annotations.DefaultDoubleValue;
import org.appwork.storage.config.annotations.DefaultEnumArrayValue;
import org.appwork.storage.config.annotations.DefaultEnumValue;
import org.appwork.storage.config.annotations.DefaultFloatArrayValue;
import org.appwork.storage.config.annotations.DefaultFloatValue;
import org.appwork.storage.config.annotations.DefaultIntArrayValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultJsonObject;
import org.appwork.storage.config.annotations.DefaultLongArrayValue;
import org.appwork.storage.config.annotations.DefaultLongValue;
import org.appwork.storage.config.annotations.DefaultStringArrayValue;
import org.appwork.storage.config.annotations.DefaultStringValue;
import org.appwork.storage.config.annotations.PlainStorage;

/**
 * @author thomas
 *
 */
@PlainStorage
@StorableValidatorIgnoresMissingSetter
@StorableValidatorIgnoresMissingGetter
public interface BadInterface extends ConfigInterface {
    @DefaultBooleanValue(value = true)
    public boolean getB2();

    @DefaultBooleanArrayValue(value = { true, false, true })
    public boolean[] getBooleanArray();

    @DefaultByteValue(value = 0)
    public byte getByte();

    @DefaultByteArrayValue(value = { 1, 2, 3 })
    public byte[] getByteArray();

    @DefaultDoubleValue(value = 0.1d)
    public double getD();

    @DefaultDoubleArrayValue(value = { 1.0d, 2.0d, 3.0d })
    public double[] getDoubleArray();

    @DefaultEnumValue(value = "A")
    public Type getEnum();

    @DefaultFloatValue(value = 0.5f)
    public float getFloat();

    @DefaultFloatArrayValue(value = { 1.0f, 2.0f, 3.0f })
    public Float[] getFloatArray();

    /**
     * @return
     */
    /*
     * BAD: BadTestObject contains a bad datatype
     */
    @CryptedStorage(key = { 0x00, 0x02, 0x11, 0x01, 0x01, 0x54, 0x01, 0x01, 0x01, 0x01, 0x12, 0x01, 0x01, 0x01, 0x22, 0x01 })
    public ArrayList<BadTestObject> getGenericList();

    @DefaultIntValue(value = 0)
    public int getInt();

    @DefaultIntArrayValue(value = { 1, 2, 3 })
    public int[] getIntArray();

    @DefaultLongValue(value = 0l)
    public long getL();

    @DefaultLongArrayValue(value = { 1, 2, 3 })
    public long[] getLongArray();

    @DefaultJsonObject(value = "{\"a\":5}")
    public BadTestObject getObject();

    public ArrayList<BadTestObject[]> getStorableArrayList();

    // public Object[] getObjectArray();
    @DefaultStringValue(value = "test")
    public String getString();

    /*
     * BAD:Annotation<-->Return Type mismatch
     */
    @DefaultStringArrayValue(value = { "test", "testb" })
    public String getStringArray();

    @DefaultEnumArrayValue(value = { "org.appwork.storage.config.test.Type.A", "org.appwork.storage.config.test.Type.B" })
    public Type[] getTypeArray();

    /*
     * BAD:Invalid Type Date
     */
    public void setDate(Date d);

    /**
     * @param list
     */
    /*
     * BAD:Cryptkey mismatch
     */
    @CryptedStorage(key = { 0x01, 0x02, 0x11, 0x01, 0x01, 0x54, 0x01, 0x01, 0x01, 0x01, 0x12, 0x01, 0x01, 0x01, 0x22, 0x01 })
    public void setGenericList(ArrayList<BadTestObject> list);

    public int setInt(int i);

    /**
     * @param is
     */
    public void setIntArray(int[] is);

    /**
     * @param o
     */
    public void setObject(BadTestObject o);
}
