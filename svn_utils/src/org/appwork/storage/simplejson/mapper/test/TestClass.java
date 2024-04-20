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
package org.appwork.storage.simplejson.mapper.test;

import java.util.ArrayList;
import java.util.HashMap;

import org.appwork.remoteapi.annotations.ApiDoc;
import org.appwork.remoteapi.annotations.ApiDocExample;
import org.appwork.storage.Storable;
import org.appwork.storage.StorableDoc;
import org.appwork.storage.StorableExample;
import org.appwork.utils.KeyValueEntry;
import org.appwork.utils.KeyValueLong;
import org.appwork.utils.KeyValueStringEntry;

/**
 * @author thomas
 *
 */
@ApiDocExample("\"This is my Testclass example\"")
@ApiDoc("My TestClass Doc")
public class TestClass implements Storable {
    private enum AnyEnum {
        @StorableDoc("Test Enum Entry")
        TEST,
        @StorableDoc("Use BLUMM or BOOM")
        BLUMM
    }

    private enum EnumOverriddenToString {
        @StorableDoc("error")
        OVERRIDDEN_TOSTRING() {
            @Override
            public String toString() {
                return "ERROR...";
            }
        }
    }

    public static java.util.List<TestClass> createList() {
        final java.util.List<TestClass> ret = new ArrayList<TestClass>();
        ret.add(TestClass.createObject());
        ret.add(TestClass.createObject());
        ret.add(TestClass.createObject());
        ret.add(TestClass.createObject());
        return ret;
    }

    public static TestClass createObject() {
        final TestClass ret1 = new TestClass("1");
        final TestClass ret2 = new TestClass("2");
        final TestClass ret3 = new TestClass("3");
        final TestClass ret4 = new TestClass("4");
        final TestClass ret5 = new TestClass("5");
        ret1.getList().add(1);
        ret1.getList().add(2);
        ret1.getList().add(3);
        ret1.getMap().put("2", ret2);
        ret1.getMap().put("3", ret3);
        ret1.getMap().put("4", ret4);
        ret1.getMap().put("5", ret5);
        return ret1;
    }

    @StorableDoc("A double")
    private double                          pDouble                           = 0.5d;
    private float                           pFloat                            = 0.4f;
    private KeyValueEntry<Integer, Boolean> genericDefinitionByContainerClass = new KeyValueEntry<Integer, Boolean>(1, true);

    public KeyValueEntry<Integer, Boolean> getGenericDefinitionByContainerClass() {
        return this.genericDefinitionByContainerClass;
    }

    public void setGenericDefinitionByContainerClass(final KeyValueEntry<Integer, Boolean> genericDefinitionByContainerClass) {
        this.genericDefinitionByContainerClass = genericDefinitionByContainerClass;
    }

    public KeyValueStringEntry getGenericDefinitionBySuperclass() {
        return this.genericDefinitionBySuperclass;
    }

    public void setGenericDefinitionBySuperclass(final KeyValueStringEntry genericDefinitionBySuperclass) {
        this.genericDefinitionBySuperclass = genericDefinitionBySuperclass;
    }

    private KeyValueStringEntry genericDefinitionBySuperclass = new KeyValueStringEntry("mykey", "myValue");
    private KeyValueStringEntry genericClass                  = new KeyValueStringEntry("mykey", "myValue");
    private KeyValueLong        genericLongClass              = new KeyValueLong(Long.valueOf(1), Long.valueOf(2));

    public KeyValueLong getGenericLongClass() {
        return this.genericLongClass;
    }

    public void setGenericLongClass(final KeyValueLong genericLongClass) {
        this.genericLongClass = genericLongClass;
    }

    /**
     * @return the genericClass
     */
    public KeyValueStringEntry getGenericClass() {
        return this.genericClass;
    }

    /**
     * @param genericClass
     *            the genericClass to set
     */
    public void setGenericClass(final KeyValueStringEntry genericClass) {
        this.genericClass = genericClass;
    }

    private long                       pLong    = 43543l;
    private int                        pInt     = 43253;
    private byte                       pByte    = 0x24;
    private char                       pChar    = 0x12;
    private boolean                    pBoolean = true;
    private String                     string   = "affe";
    private Double                     oDouble  = 0.5d;
    @StorableDoc("This is a float")
    @StorableExample("0.32")
    private Float                      oFloat   = 0.4f;
    private Long                       oLong    = 43543l;
    private Integer                    oInt     = 43253;
    private Byte                       oByte    = 0x24;
    private Character                  oChar    = 0x12;
    @StorableDoc("Default: \"TEST\"")
    private AnyEnum                    num      = AnyEnum.TEST;
    private Boolean                    oBoolean = true;
    private int[]                      intArray = new int[] { 1, 2 };
    private TestClass[]                objArray = null;
    private HashMap<String, TestClass> map      = new HashMap<String, TestClass>();
    private java.util.List<Integer>    list     = new ArrayList<Integer>();
    private TestClass                  obj;
    private EnumOverriddenToString     numtoString;

    public EnumOverriddenToString getNumtoString() {
        return this.numtoString;
    }

    public void setNumtoString(final EnumOverriddenToString numtoString) {
        this.numtoString = numtoString;
    }

    public TestClass() {
    }

    /**
     * @param string2
     */
    public TestClass(final String string2) {
        this.string = string2;
        this.objArray = new TestClass[] { new TestClass(), new TestClass(), new TestClass() };
        this.pDouble = 0.3d;
        this.intArray = new int[] { 3, 2, 1 };
        this.num = AnyEnum.BLUMM;
        this.pFloat = 0.423f;
        this.pLong = 4355543543l;
        this.pInt = 2435253;
        this.numtoString = EnumOverriddenToString.OVERRIDDEN_TOSTRING;
        this.pByte = 0x14;
        this.pChar = 0x13;
        this.pBoolean = false;
        this.string = "affe232";
        this.oDouble = 0.52d;
        this.oFloat = 0.4123f;
        this.oLong = 5435443543l;
        this.oInt = 45343253;
        this.oByte = 0x44;
        this.oChar = 0x10;
        this.oBoolean = false;
    }

    public int[] getIntArray() {
        return this.intArray;
    }

    public java.util.List<Integer> getList() {
        return this.list;
    }

    public HashMap<String, TestClass> getMap() {
        return this.map;
    }

    public AnyEnum getNum() {
        return this.num;
    }

    public TestClass getObj() {
        return this.obj;
    }

    public TestClass[] getObjArray() {
        return this.objArray;
    }

    public Boolean getoBoolean() {
        return this.oBoolean;
    }

    public Byte getoByte() {
        return this.oByte;
    }

    public Character getoChar() {
        return this.oChar;
    }

    public Double getoDouble() {
        return this.oDouble;
    }

    public Float getoFloat() {
        return this.oFloat;
    }

    public Integer getoInt() {
        return this.oInt;
    }

    public Long getoLong() {
        return this.oLong;
    }

    public byte getpByte() {
        return this.pByte;
    }

    public char getpChar() {
        return this.pChar;
    }

    public double getpDouble() {
        return this.pDouble;
    }

    public float getpFloat() {
        return this.pFloat;
    }

    public int getpInt() {
        return this.pInt;
    }

    public long getpLong() {
        return this.pLong;
    }

    public String getString() {
        return this.string;
    }

    public boolean ispBoolean() {
        return this.pBoolean;
    }

    public void setIntArray(final int[] intArray) {
        this.intArray = intArray;
    }

    public void setList(final java.util.List<Integer> list) {
        this.list = list;
    }

    public void setMap(final HashMap<String, TestClass> map) {
        this.map = map;
    }

    public void setNum(final AnyEnum num) {
        this.num = num;
    }

    public void setObj(final TestClass obj) {
        this.obj = obj;
    }

    public void setObjArray(final TestClass[] objArray) {
        this.objArray = objArray;
    }

    public void setoBoolean(final Boolean oBoolean) {
        this.oBoolean = oBoolean;
    }

    public void setoByte(final Byte oByte) {
        this.oByte = oByte;
    }

    public void setoChar(final Character oChar) {
        this.oChar = oChar;
    }

    public void setoDouble(final Double oDouble) {
        this.oDouble = oDouble;
    }

    public void setoFloat(final Float oFloat) {
        this.oFloat = oFloat;
    }

    public void setoInt(final Integer oInt) {
        this.oInt = oInt;
    }

    public void setoLong(final Long oLong) {
        this.oLong = oLong;
    }

    public void setpBoolean(final boolean pBoolean) {
        this.pBoolean = pBoolean;
    }

    public void setpByte(final byte pByte) {
        this.pByte = pByte;
    }

    public void setpChar(final char pChar) {
        this.pChar = pChar;
    }

    public void setpDouble(final double pDouble) {
        this.pDouble = pDouble;
    }

    public void setpFloat(final float pFloat) {
        this.pFloat = pFloat;
    }

    public void setpInt(final int pInt) {
        this.pInt = pInt;
    }

    public void setpLong(final long pLong) {
        this.pLong = pLong;
    }

    public void setString(final String string) {
        this.string = string;
    }
}
