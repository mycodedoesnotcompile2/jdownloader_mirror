package org.appwork.storage.flexijson.tests;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.appwork.remoteapi.annotations.AllowNonStorableObjects;
import org.appwork.storage.Storable;
import org.appwork.storage.StorableDoc;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiInterfaceDefault;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiStorableInterface;
import org.appwork.storage.flexijson.mapper.typemapper.DateMapper;
import org.appwork.utils.duration.InvalidTimeSpanException;
import org.appwork.utils.duration.TimeSpan;

public class ObjectContainsAll implements Storable {
    public static enum EntryType {
        DIR,
        FILE
    }

    private ArrayList<String> arrayList          = new ArrayList<String>(Arrays.asList("eins", "drei"));
    private boolean           booleanPrimitive   = false;
    private Boolean           booleanWrapper     = Boolean.TRUE;
    private Boolean           booleanWrapperNull = null;
    private CharSequence      charsequence       = new StringBuilder("char seq");
    private CharSequence      charsequenceNull   = null;

    public CharSequence getCharsequence() {
        return charsequence;
    }

    public void setCharsequence(CharSequence charsequence) {
        this.charsequence = charsequence;
    }

    public CharSequence getCharsequenceNull() {
        return charsequenceNull;
    }

    public void setCharsequenceNull(CharSequence charsequenceNull) {
        this.charsequenceNull = charsequenceNull;
    }

    private byte   bytePrimitive  = Byte.MAX_VALUE;
    private Object objectWithByte = Byte.MAX_VALUE;

    @AllowNonStorableObjects
    public Object getObjectWithByte() {
        return objectWithByte;
    }

    public void setObjectWithByte(Object objectWithByte) {
        this.objectWithByte = objectWithByte;
    }

    @AllowNonStorableObjects
    public Object getObjectWithFloat() {
        return objectWithFloat;
    }

    public void setObjectWithFloat(Object objectWithFloat) {
        this.objectWithFloat = objectWithFloat;
    }

    public List getListWithoutGen() {
        return listWithoutGen;
    }

    public void setListWithoutGen(List listWithoutGen) {
        this.listWithoutGen = listWithoutGen;
    }

    public Map getMapWithoutGen() {
        return mapWithoutGen;
    }

    public void setMapWithoutGen(Map mapWithoutGen) {
        this.mapWithoutGen = mapWithoutGen;
    }

    public Set getSetWithoutGen() {
        return setWithoutGen;
    }

    public void setSetWithoutGen(Set setWithoutGen) {
        this.setWithoutGen = setWithoutGen;
    }

    @AllowNonStorableObjects
    public Object getObjectHashSet() {
        return objectHashSet;
    }

    public void setObjectHashSet(Object objectHashSet) {
        this.objectHashSet = objectHashSet;
    }

    private static interface MyExampleInterface extends FlexiStorableInterface {
        @FlexiInterfaceDefault("true")
        public boolean getBool();

        public void setBool(boolean b);
    }

    private MyExampleInterface interfaceField = new MyExampleInterface() {
                                                  private boolean bool = true;

                                                  @Override
                                                  public boolean getBool() {
                                                      return bool;
                                                  }

                                                  @Override
                                                  public void setBool(boolean b) {
                                                      this.bool = b;
                                                  }
                                              };

    public MyExampleInterface getInterfaceField() {
        return interfaceField;
    }

    public void setInterfaceField(MyExampleInterface interfaceField) {
        this.interfaceField = interfaceField;
    }

    private Byte                     byteWrapper       = Byte.MIN_VALUE;
    private Byte                     byteWrapperNull   = null;
    private Character                character         = '\u25cb';
    private Character                characterNull     = null;
    private char                     charPrimitive     = Character.MAX_VALUE;
    private double                   doublePrimitive   = Double.MAX_VALUE;
    private Double                   doubleWrapper     = Double.MIN_VALUE;
    private Double                   doubleWrapperNull = null;
    private EntryType                enumField         = EntryType.DIR;
    private EntryType                enumFieldNull     = null;
    private float                    floatPrimitive    = Float.MAX_VALUE;
    private Float                    floatWrapper      = Float.MIN_VALUE;
    private Object                   objectWithFloat   = Float.MIN_VALUE;
    private Float                    floatWrapperNull  = null;
    private HashMap<String, Integer> hashMap           = createHashMap();
    private HashSet<Integer>         hashSet           = new HashSet<Integer>(Arrays.asList(1, 4, 7));
    //
    @StorableDoc("simple primitive int array")
    private int[]                    intArray          = new int[] { 1, 2, 3 };
    private Integer[]                integerArray      = new Integer[] { 1, null, 3 };
    private int                      intPrimitive      = Integer.MAX_VALUE;
    private Integer                  intWrapper        = Integer.MIN_VALUE;
    private Integer                  intWrapperNull    = null;
    private List<String>             list              = new ArrayList<String>(Arrays.asList("eins", "drei"));
    private List                     listWithoutGen    = new ArrayList<String>(Arrays.asList("eins", "drei"));
    private LinkedList               nullList          = null;

    public LinkedList getNullList() {
        return nullList;
    }

    public void setNullList(LinkedList nullList) {
        this.nullList = nullList;
    }

    public HashSet getNullSet() {
        return nullSet;
    }

    public void setNullSet(HashSet nullSet) {
        this.nullSet = nullSet;
    }

    public LinkedHashMap<String, String> getNullMap() {
        return nullMap;
    }

    public void setNullMap(LinkedHashMap<String, String> nullMap) {
        this.nullMap = nullMap;
    }

    private HashSet                       nullSet         = null;
    private LinkedHashMap<String, String> nullMap         = null;
    private long                          longPrimitive   = Long.MIN_VALUE;
    private Long                          longWrapper     = Long.MAX_VALUE;
    private Long                          longWrapperNull = null;
    private Map<String, Integer>          map             = createHashMap();
    private Map                           mapWithoutGen   = createHashMap();
    private Set<Integer>                  set             = new HashSet<Integer>(Arrays.asList(1, 4, 7));
    private Set                           setWithoutGen   = new HashSet<Integer>(Arrays.asList(1, 4, 7));
    private Object                        objectHashSet   = new HashSet<Integer>(Arrays.asList(1, 4, 7));
    private Short                         shortNull       = null;
    private short                         shortPrimitive  = Short.MAX_VALUE;
    private Short                         shortWrapper    = Short.MIN_VALUE;
    private String                        stringField     = "fsdajbfdshbdfs98h";
    private String                        stringFieldNull = null;
    private Date                          date            = new DateMapper().parseString("2022-01-02T01:02:03.123+0100");

    @AllowNonStorableObjects(Date.class)
    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    @AllowNonStorableObjects(TimeSpan.class)
    public TimeSpan getTimespan() {
        return timespan;
    }

    public void setTimespan(TimeSpan timespan) {
        this.timespan = timespan;
    }

    private TimeSpan timespan;

    public ObjectContainsAll() {
        try {
            timespan = TimeSpan.parse("10h15m");
        } catch (NumberFormatException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (InvalidTimeSpanException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * @return
     */
    private HashMap<String, Integer> createHashMap() {
        HashMap<String, Integer> ret = new HashMap<String, Integer>();
        ret.put("eins", 1);
        ret.put("zwei", 2);
        return ret;
    }

    public ArrayList<String> getArrayList() {
        return arrayList;
    }

    public Boolean getBooleanWrapper() {
        return booleanWrapper;
    }

    public Boolean getBooleanWrapperNull() {
        return booleanWrapperNull;
    }

    public byte getBytePrimitive() {
        return bytePrimitive;
    }

    public Byte getByteWrapper() {
        return byteWrapper;
    }

    public Byte getByteWrapperNull() {
        return byteWrapperNull;
    }

    public Character getCharacter() {
        return character;
    }

    public Character getCharacterNull() {
        return characterNull;
    }

    public char getCharPrimitive() {
        return charPrimitive;
    }

    public double getDoublePrimitive() {
        return doublePrimitive;
    }

    public Double getDoubleWrapper() {
        return doubleWrapper;
    }

    public Double getDoubleWrapperNull() {
        return doubleWrapperNull;
    }

    public EntryType getEnumField() {
        return enumField;
    }

    public EntryType getEnumFieldNull() {
        return enumFieldNull;
    }

    public float getFloatPrimitive() {
        return floatPrimitive;
    }

    public Float getFloatWrapper() {
        return floatWrapper;
    }

    public Float getFloatWrapperNull() {
        return floatWrapperNull;
    }

    public HashMap<String, Integer> getHashMap() {
        return hashMap;
    }

    public HashSet<Integer> getHashSet() {
        return hashSet;
    }

    public int[] getIntArray() {
        return intArray;
    }

    public Integer[] getIntegerArray() {
        return integerArray;
    }

    public int getIntPrimitive() {
        return intPrimitive;
    }

    public Integer getIntWrapper() {
        return intWrapper;
    }

    public Integer getIntWrapperNull() {
        return intWrapperNull;
    }

    @AllowNonStorableObjects
    public List<String> getList() {
        return list;
    }

    public long getLongPrimitive() {
        return longPrimitive;
    }

    public Long getLongWrapper() {
        return longWrapper;
    }

    public Long getLongWrapperNull() {
        return longWrapperNull;
    }

    public Map<String, Integer> getMap() {
        return map;
    }

    public Set<Integer> getSet() {
        return set;
    }

    public Short getShortNull() {
        return shortNull;
    }

    public short getShortPrimitive() {
        return shortPrimitive;
    }

    public Short getShortWrapper() {
        return shortWrapper;
    }

    public String getStringField() {
        return stringField;
    }

    public String getStringFieldNull() {
        return stringFieldNull;
    }

    public boolean isBooleanPrimitive() {
        return booleanPrimitive;
    }

    public void setArrayList(ArrayList<String> arrayList) {
        this.arrayList = arrayList;
    }

    public void setBooleanPrimitive(boolean booleanPrimitive) {
        this.booleanPrimitive = booleanPrimitive;
    }

    public void setBooleanWrapper(Boolean booleanWrapper) {
        this.booleanWrapper = booleanWrapper;
    }

    public void setBooleanWrapperNull(Boolean booleanWrapperNull) {
        this.booleanWrapperNull = booleanWrapperNull;
    }

    public void setBytePrimitive(byte bytePrimitive) {
        this.bytePrimitive = bytePrimitive;
    }

    public void setByteWrapper(Byte byteWrapper) {
        this.byteWrapper = byteWrapper;
    }

    public void setByteWrapperNull(Byte byteWrapperNull) {
        this.byteWrapperNull = byteWrapperNull;
    }

    public void setCharacter(Character character) {
        this.character = character;
    }

    public void setCharacterNull(Character characterNull) {
        this.characterNull = characterNull;
    }

    public void setCharPrimitive(char charPrimitive) {
        this.charPrimitive = charPrimitive;
    }

    public void setDoublePrimitive(double doublePrimitive) {
        this.doublePrimitive = doublePrimitive;
    }

    public void setDoubleWrapper(Double doubleWrapper) {
        this.doubleWrapper = doubleWrapper;
    }

    public void setDoubleWrapperNull(Double doubleWrapperNull) {
        this.doubleWrapperNull = doubleWrapperNull;
    }

    public void setEnumField(EntryType enumField) {
        this.enumField = enumField;
    }

    public void setEnumFieldNull(EntryType enumFieldNull) {
        this.enumFieldNull = enumFieldNull;
    }

    public void setFloatPrimitive(float floatPrimitive) {
        this.floatPrimitive = floatPrimitive;
    }

    public void setFloatWrapper(Float floatWrapper) {
        this.floatWrapper = floatWrapper;
    }

    public void setFloatWrapperNull(Float floatWrapperNull) {
        this.floatWrapperNull = floatWrapperNull;
    }

    public void setHashMap(HashMap<String, Integer> hashMap) {
        this.hashMap = hashMap;
    }

    public void setHashSet(HashSet<Integer> hashSet) {
        this.hashSet = hashSet;
    }

    public void setIntArray(int[] intArray) {
        this.intArray = intArray;
    }

    public void setIntegerArray(Integer[] integerArray) {
        this.integerArray = integerArray;
    }

    public void setIntPrimitive(int intPrimitive) {
        this.intPrimitive = intPrimitive;
    }

    public void setIntWrapper(Integer intWrapper) {
        this.intWrapper = intWrapper;
    }

    public void setIntWrapperNull(Integer intWrapperNull) {
        this.intWrapperNull = intWrapperNull;
    }

    public void setList(List<String> list) {
        this.list = list;
    }

    public void setLongPrimitive(long longPrimitive) {
        this.longPrimitive = longPrimitive;
    }

    public void setLongWrapper(Long longWrapper) {
        this.longWrapper = longWrapper;
    }

    public void setLongWrapperNull(Long longWrapperNull) {
        this.longWrapperNull = longWrapperNull;
    }

    public void setMap(Map<String, Integer> map) {
        this.map = map;
    }

    public void setSet(Set<Integer> set) {
        this.set = set;
    }

    public void setShortNull(Short shortNull) {
        this.shortNull = shortNull;
    }

    public void setShortPrimitive(short shortPrimitive) {
        this.shortPrimitive = shortPrimitive;
    }

    public void setShortWrapper(Short shortWrapper) {
        this.shortWrapper = shortWrapper;
    }

    public void setStringField(String stringField) {
        this.stringField = stringField;
    }

    public void setStringFieldNull(String stringFieldNull) {
        this.stringFieldNull = stringFieldNull;
    }
}
