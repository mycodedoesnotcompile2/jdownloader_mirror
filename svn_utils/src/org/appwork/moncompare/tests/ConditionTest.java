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
package org.appwork.moncompare.tests;

import static org.appwork.moncompare.Condition.$AND;
import static org.appwork.moncompare.Condition.$DIVIDE;
import static org.appwork.moncompare.Condition.$EQ;
import static org.appwork.moncompare.Condition.$EXISTS;
import static org.appwork.moncompare.Condition.$GT;
import static org.appwork.moncompare.Condition.$GTE;
import static org.appwork.moncompare.Condition.$IN;
import static org.appwork.moncompare.Condition.$LT;
import static org.appwork.moncompare.Condition.$LTE;
import static org.appwork.moncompare.Condition.$MAX;
import static org.appwork.moncompare.Condition.$MIN;
import static org.appwork.moncompare.Condition.$MULTIPLY;
import static org.appwork.moncompare.Condition.$NE;
import static org.appwork.moncompare.Condition.$NIN;
import static org.appwork.moncompare.Condition.$NOT;
import static org.appwork.moncompare.Condition.$OPTIONS;
import static org.appwork.moncompare.Condition.$OR;
import static org.appwork.moncompare.Condition.$REGEX;
import static org.appwork.moncompare.Condition.$SUBTRACT;
import static org.appwork.moncompare.Condition.$SUM;
import static org.appwork.moncompare.Condition.$TYPE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.regex.PatternSyntaxException;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.moncompare.Condition;
import org.appwork.moncompare.ConditionException;
import org.appwork.moncompare.Conditions;
import org.appwork.moncompare.fromjson.FlexiCondition;
import org.appwork.moncompare.helper.AnyElementMatches;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.typemapper.ConditionMapper;
import org.appwork.storage.flexijson.mapper.typemapper.DateMapper;
import org.appwork.testframework.AWTest;
import org.appwork.txtresource.LocaleMap;
import org.appwork.utils.duration.TimeSpan;

/**
 * @author Thomas
 * @date 06.05.2019
 *
 */
public class ConditionTest extends AWTest {
    /**
     *
     */
    /**
     *
     */
    public static class EmbededObject {
        public ArrayList<String> list = new ArrayList<String>();

        public ArrayList<String> getList() {
            return list;
        }

        public void setList(ArrayList<String> list) {
            this.list = list;
        }

        public ArrayList<String[]> getList2() {
            return list2;
        }

        public void setList2(ArrayList<String[]> list2) {
            this.list2 = list2;
        }

        public boolean isC() {
            return c;
        }

        public void setC(boolean c) {
            this.c = c;
        }

        public int getI() {
            return i;
        }

        public void setI(int i) {
            this.i = i;
        }

        {
            this.list.add("s1");
            this.list.add("s2");
        }
        public ArrayList<String[]> list2 = new ArrayList<String[]>();
        {
            this.list2.add(new String[] { "a", "b" });
            this.list2.add(new String[] { "a", "b" });
        }
        private boolean b = false;
        private boolean c = false;

        /**
         * @return the b
         */
        public boolean isB() {
            return this.b;
        }

        /**
         * @param b
         *            the b to set
         */
        public void setB(boolean b) {
            this.b = b;
        }

        private int i = 3;
    }

    /**
     * @param string
     * @param string2
     * @return
     */
    private static Object[] list(Object... strings) {
        return strings;
    }

    private static void eval(Object soll, Object ist) {
        if (!soll.toString().equals(String.valueOf(ist))) {
            throw new WTFException();
        }
    }

    public static void main(String[] args) {
        run();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @SuppressWarnings("unchecked")
    @Override
    public void runTest() throws Exception {
        Condition.THREAD_DEBUG.set(true);
        Condition.THREAD_LOGGER.set(LogV3.defaultLogger());
        HashMap<String, Object> regex = new HashMap<String, Object>();
        regex.put("a", "(.");
        regex.put("valid", "abc.def");
        regex.put("valid2", "abc\\ddef");
        regex.put("valid", "abc.def");
        regex.put("set", new ArrayList<String>(Arrays.asList("123", "643dfbg")));
        regex.put("list", new Object[] { true, 1, "string" });
        regex.put("listWithNull", new Object[] { true, 1, "string", null });
        {
            HashMap<String, Object> map2 = new HashMap<String, Object>();
            map2.put("a", true);
            map2.put("b", false);
            HashMap<String, Object> map3 = new HashMap<String, Object>();
            map3.put("a", true);
            map3.put("b", true);
            HashMap<String, Object> map = new HashMap<String, Object>();
            map.put("el1", map2);
            map.put("el2", map3);
            map.put("el3", map2);
            map.put("el4", map3);
            regex.put("map", map);
        }
        eval(true, FlexiCondition.parse("{§each:{§isRegex:false}}").matches(new String[] { "(abc" }));
        // this is never null, thus §any must resolve to false.
        eval(true, FlexiCondition.parse("{§each:{§isRegex:true}}").matches(new String[] { ".*" }));
        eval(false, FlexiCondition.parse("{§each:{§isRegex:true}}").matches(new String[] { ".*", "(abc" }));
        eval(false, FlexiCondition.parse("{§any:[{§§THIS:null}]}").matches(new LocaleMap("Tester")));
        eval(true, FlexiCondition.parse("{§keys:{§each:{§or:[{§regex:\"\\\\p{Lower}{2}\"},{§regex:\"\\\\p{Lower}{2}_\\\\p{Upper}{2}\"},{§regex:\"\\\\p{Lower}{2}_\\\\p{Upper}{2}_\\\\S+\"}]}}}").matches(new LocaleMap("Tester")));
        eval(true, FlexiCondition.parse("{\"§§THIS.map[{§project:{b:false}}].§first.a\":true}").matches(regex));
        eval(false, Conditions.THIS_DOES_NOT_EXIST.matches(null));
        eval(true, Conditions.THIS_EXISTS.matches(null));
        eval(false, FlexiCondition.parse("{§§THIS:{§eq:Meintext}}").matches("MeinText"));
        eval(true, FlexiCondition.parse("{§§THIS:{§options:{caseInsensitive:true},§eq:Meintext}}").matches("MeinText"));
        FlexiJSonMapper.addDefaultMapper(new ConditionMapper());
        eval(true, FlexiCondition.parse("{set.§first:\"123\"}").matches(regex));
        // empty object ALWAYS matches.
        eval(true, FlexiCondition.parse("{}").matches(regex));
        Condition selfCOndition = FlexiCondition.parse(FlexiUtils.serializeMinimized(regex));
        eval(true, selfCOndition.matches(regex));
        eval(true, new Condition("§§THIS", selfCOndition).matches(regex));
        selfCOndition.remove("a");
        eval(false, new Condition("§§THIS", selfCOndition).matches(regex));
        eval(false, FlexiCondition.parse("{$$THIS:{}}").matches(regex));
        eval(true, FlexiCondition.parse("{$eq:{}}").matches(new HashMap<String, Object>()));
        eval(true, FlexiCondition.parse("{$$THIS:{}}").matches(new HashMap<String, Object>()));
        eval(true, FlexiCondition.parse("{§§THIS:{}}").matches(new HashMap<String, Object>()));
        eval(true, FlexiCondition.parse("{§eq:{}}").matches(new HashMap<String, Object>()));
        eval(false, FlexiCondition.parse("{$eq:{}}").matches(regex));
        eval(false, FlexiCondition.parse("{§§THIS:{}}").matches(regex));
        eval(false, FlexiCondition.parse("{§§THIS:{$eq:{}}}").matches(regex));
        eval(true, FlexiCondition.parse("{$ne:{}}").matches(regex));
        eval(true, FlexiCondition.parse("{§NOW:{§gt:1}}").matches(regex));
        eval(true, FlexiCondition.parse("{set.§size:2}").matches(regex));
        eval(true, FlexiCondition.parse("{set[1]:\"643dfbg\"}").matches(regex));
        eval(true, "a".equals(JSPath.fromPathString("a.§key").resolve(regex)));
        eval(true, FlexiCondition.parse("{\"§§THIS\":1M!365}").matches(TimeSpan.parse("1M!365")));
        eval(true, FlexiCondition.parse("{\"§§THIS\":2021-10-07T00:00CET}").matches(DateMapper.parse("2021-10-07T00:00CET")));
        eval(true, FlexiCondition.parse("{\"§§THIS\":{§gt:2021-10-07T00:00CET}}").matches(new Date()));
        eval(true, FlexiCondition.parse("{\"§§THIS\":{§gte:2021-10-07T00:00CET}}").matches(new Date()));
        eval(false, FlexiCondition.parse("{\"§§THIS\":{§gt:2121-10-07T00:00CET}}").matches(new Date()));
        eval(false, FlexiCondition.parse("{\"§§THIS\":{§gte:2121-10-07T00:00CET}}").matches(new Date()));
        eval(false, FlexiCondition.parse("{\"§§THIS\":{§lt:2021-10-07T00:00CET}}").matches(new Date()));
        eval(false, FlexiCondition.parse("{\"§§THIS\":{§lte:2021-10-07T00:00CET}}").matches(new Date()));
        eval(true, FlexiCondition.parse("{\"§§THIS\":{§lt:2121-10-07T00:00CET}}").matches(new Date()));
        eval(true, FlexiCondition.parse("{\"§§THIS\":{§lte:2121-10-07T00:00CET}}").matches(new Date()));
        eval(true, FlexiCondition.parse("{\"§§THIS\":{§lt:9W}}").matches(TimeSpan.parse("1M")));
        Object eval = Condition.resolve("§§THIS[{§project:{§key:{§regex:^valid.*}}}]", regex);
        eval(true, FlexiCondition.parse("{\"§§THIS\":{§gt:1M}}").matches(TimeSpan.parse("1Y!365d")));
        eval(true, FlexiCondition.parse("{\"§§THIS\":{§lt:2Y}}").matches(TimeSpan.parse("1Y!365d")));
        eval(true, FlexiCondition.parse("{\"§§THIS\":1Y}").matches(TimeSpan.parse("1Y")));
        eval(true, FlexiCondition.parse("{\"§§THIS\":{§gt:1M}}").matches(TimeSpan.parse("1Y!365d")));
        eval(true, FlexiCondition.parse("{\"§§THIS[{§project:{§key:{§regex:^valid.*}}}].§size\":2}").matches(regex));
        eval(true, FlexiCondition.parse("{\"listWithNull[{§project:{§or:[{§§THIS:string},{§key:0}]}}].§size\":2}").matches(regex));
        eval(true, FlexiCondition.parse("{\"listWithNull[{§concat:[\\\"§[0]\\\",\\\"§[1]\\\"]}]\":\"true1\"}").matches(regex));
        eval(true, FlexiCondition.parse("{\"listWithNull\":{§options:{filter:{§§THIS:string}},§each:{§§THIS:string}}}").matches(regex));
        eval(true, FlexiCondition.parse("{\"listWithNull[{§project:{§§THIS:string}}]\":{§each:{§§THIS:string}}}").matches(regex));
        eval(true, FlexiCondition.parse("{§options:{aggregate:true},§eq:[abc.def_(.,{§concat:[§valid,_,§a]}]}").matches(regex));
        Condition testCache = FlexiCondition.parse("{§options:{aggregate:true},§eq:[c.d,{§regexFindOne:[§valid,ab(.*?)ef]}]}");
        eval(true, testCache.matches(regex));
        eval(true, testCache.matches(regex));
        eval(true, testCache.matches(regex));
        eval(true, FlexiCondition.parse("{§options:{aggregate:true},§eq:[c.d,{§regexFindOne:[§valid,a(.)(.*?)ef,2]}]}").matches(regex));
        eval(false, FlexiCondition.parse("{§options:{aggregate:true},§eq:[c.d,{§regexFindOne:[§valid,a(.)(.*?)ef,1]}]}").matches(regex));
        eval(true, FlexiCondition.parse("{b:{§or:[{§exists:false},{§eq:null}]}}").matches(regex));
        eval(true, /* this is true, because b does not exist and this is NOT null. */FlexiCondition.parse("{b:{§ne:null}}").matches(regex));
        eval(true, FlexiCondition.parse("{§ne:null}").matches(new Object[] { "a", null }));
        eval(true, new Condition(Condition.$EACH, new Condition(Condition.$NE, null)).matches(regex));
        EmbededObject inst = new EmbededObject();
        eval(true, FlexiCondition.parse("{§each:{§ne:null}}").matches(inst));
        eval(true, FlexiCondition.parse("{a:{§each:{§ne:null}}}").matches(regex));
        eval(true, FlexiCondition.parse("{list:{§each:{§ne:null}}}").matches(regex));
        eval(false, FlexiCondition.parse("{listWithNull:{§each:{§ne:null}}}").matches(regex));
        eval(true, FlexiCondition.parse("{§keys:{§each:{§ne:null}}}").matches(regex));
        regex.put(null, "null key");
        eval(false, FlexiCondition.parse("{§keys:{§each:{§ne:null}}}").matches(regex));
        eval(true, FlexiCondition.parse("{valid:{§regex:§§this}}").matches(regex));
        eval(false, FlexiCondition.parse("{valid2:{§regex:§§this}}").matches(regex));
        // fail(true, FlexiCondition.parse("{valid:{§ne:{§regex:§§this}}}").matches(regex));
        eval(true, FlexiCondition.parse("{§ne:[{§regex:§§this.valid},astring],§options:{aggregate:true}}").matches(regex));
        try {
            FlexiCondition.parse("{§ne:[{§regex:§§this.a},true],§options:{aggregate:true}}").matches(regex);
            throw new WTFException("Excpected bad pattern exception");
        } catch (ConditionException e) {
            if (e.getCause() instanceof PatternSyntaxException) {
                // expected
            } else {
                throw e;
            }
        }
        long time = System.currentTimeMillis();
        eval(true, new Condition(Condition.$GT, new Date(time)).matches(new Date(time + 1)));
        eval(false, new Condition(Condition.$LT, new Date(time)).matches(new Date(time + 1)));
        HashMap<String, Object> map = new HashMap<String, Object>();
        map.put("date", new Date(time + 1));
        eval(true, new Condition("date", new Condition(Condition.$GT, new Date(time))).matches(map));
        eval(false, new Condition("date", new Condition(Condition.$LT, new Date(time))).matches(map));
        HashMap<String, Object> test = new HashMap<String, Object>();
        test.put("a", 1);
        test.put("b", 1);
        test.put("c", 2);
        test.put("d", 8);
        test.put("boo", true);
        test.put("110", 110);
        test.put("120", 120);
        test.put("130", 130);
        test.put("leer", 0);
        test.put("string", "ich bin ein string");
        HashMap<String, Object> test2 = new HashMap<String, Object>();
        test2.put("a", 1);
        test2.put("b", 1);
        test2.put("c", 2);
        test2.put("sum", new int[] { 0, 0, 1 });
        test.put("test2", test2);
        HashMap<String, Object> test3 = new HashMap<String, Object>();
        test3.put("a", 1);
        test3.put("sa", new String[] { "a", "b" });
        test3.put("obj", new EmbededObject());
        test3.put("list", new int[] { 1, 2, 4, 6, 7, 8, 9 });
        test3.put("list3", new Object[] { new int[] { 3, 6 }, 2, 4, 6, 7, 8, 9 });
        // fails because §gt runs in aggregation mode and thus returns true. The result of §gt is compared against the value of c 2!=true
        // -->false
        eval(false, FlexiCondition.parse("{c:{§gt:[{§sum:[§§this,1]},0]}}").matches(test2));
        // no access to getClass or getSimpleName
        eval(false, new Condition("obj.getClass.getSimpleName", new Condition("§regex", ".*embed.*").append("§options", "ims")).matches(test3));
        eval(false, FlexiCondition.parse("{c:2,a:2}").matches(test2));
        eval(false, FlexiCondition.parse("{c:{§ne:null},d:{§exists:true,$ne:null}}").matches(test2));
        eval(false, FlexiCondition.parse("{§ne:[c,null],d:{§exists:true,$ne:null}}").matches(test2));
        // better
        eval(true, FlexiCondition.parse("{§gt:[{§sum:[§c,1]},0]}").matches(test2));
        eval(false, FlexiCondition.parse("{§§THIS[0].§size:{§gt:0}}").matches(new String[] { "" }));
        eval(true, FlexiCondition.parse("{§§THIS[0].§size:{§gt:0}}").matches(new String[] { "abc" }));
        eval(true, FlexiCondition.parse("{[0].§size:{§gt:0}}").matches(new String[] { "abc" }));
        eval(true, FlexiCondition.parse("{§gt:[\"§[0].§size\",0]}").matches(new String[] { "abc" }));
        eval(false, new Condition("§§THIS[0].§size", new Condition(Condition.$GT, 0)).matches(new String[] { "" }));
        eval(true, new Condition("c", new Condition($EXISTS, new Condition($SUM, new Object[] { "§PARENT.a", "§PARENT.leer", 0, -2, "§§THIS" }))).append($LT, new Object[] { new Condition($SUM, new Object[] { "§test2.sum" }), 10 }).matches(test));
        eval(true, new Condition("a", new Condition($EQ, new Condition($SUM, Arrays.asList(new Object[] { 1, "§§ROOT.leer" })))).matches(test));
        eval(true, new Condition("boo", new Condition($EQ, Arrays.asList(new Object[] { 1, "§§ROOT.a" })).append($OPTIONS, new Condition(Condition.OPTIONS_AGGREGATE, true))).matches(test));
        eval(true, new Condition($AND, list(new Condition($EXISTS, list("§c", 1)), new Condition($LT, new Object[] { new Condition($SUM, new Object[] { "§§ROOT.test2.sum" }), 10 }))).matches(test));
        eval(true, new Condition("obj." + $TYPE + ".§PARENT.§type", new Condition($REGEX, ".*Embed.*")).matches(test3));
        eval(true, new Condition("list", new Condition($EQ, 1)).matches(test3));
        eval(true, new Condition("list", 1).matches(test3));
        eval(false, new Condition("list", new Condition($EQ, "a")).matches(test3));
        // any list element matches §parent.a
        eval(true, new Condition("list", new AnyElementMatches("§PARENT.§PARENT.a")).matches(test3));
        // the eq on a list as actually handled as "does any element match the expression". this is due to the mongo eq works like this.
        // however the scope under which each comparism works is list[*]. §parent is list and §parent.§parent is the root that contains a.
        eval(true, new Condition("list", new Condition($EQ, "§PARENT.§PARENT.a")).matches(test3));
        eval(true, new Condition("sa", list(new Condition($REGEX, "a.?"), new Condition($REGEX, ".?b"))).matches(test3));
        Condition query = new Condition();
        ArrayList<Condition> cond = new ArrayList<Condition>();
        cond.add(new Condition("list", new Condition($IN, new int[] { 1 })));
        cond.add(new Condition("list", new Condition($NIN, new int[] { 5 })));
        cond.add(new Condition("list", new Condition($OR, new Condition[] { new Condition($IN, new int[] { 2 }), new Condition($IN, new int[] { 3 }) })));
        query.append($AND, cond);
        System.out.println(JSonStorage.serializeToJson(test3));
        eval(true, query.matches(test3));
        // Relative
        eval(true, new Condition("c", new Condition($EXISTS, new Condition($SUM, new Object[] { "§PARENT.a", "§PARENT.leer", 0, -2, Condition.$$THIS }))).append($LT, new Object[] { new Condition($SUM, new Object[] { "§test2.sum" }), 10 }).matches(test));
        eval(true, new Condition("a", new Condition($NIN, new int[] { 4, 5, 6 })).matches(test3));
        eval(true, new Condition($AND, list(new Condition($EXISTS, list("§c", 1)), new Condition($LT, new Object[] { new Condition($SUM, new Object[] { "§§ROOT.test2.sum" }), 10 }))).matches(test));
        eval(true, new Condition("c", new Condition($EXISTS, 1)).append($LT, new Object[] { new Condition($SUM, new Object[] { "§§ROOT.test2.sum" }), 10 }).matches(test));
        eval(false, new Condition("c", new Condition($EXISTS, 0)).matches(test));
        // eq tests from the mongo docu https://docs.mongodb.com/manual/reference/operator/query/eq/
        eval(true, new Condition("sa", list(new Condition($REGEX, "a.?"), new Condition($REGEX, ".?b"))).matches(test3));
        eval(true, new Condition($OR, list(new Condition($REGEX, list("a.?", "§sa[0]")), new Condition($REGEX, list("b.?", "§sa[1]")))).matches(test3));
        eval(true, new Condition("sa", new Condition($REGEX, "(a|b)")).matches(test3));
        eval(false, new Condition("sa", new Condition($REGEX, "\\d")).matches(test3));
        eval(true, new Condition("list", new Condition($EQ, 1)).matches(test3));
        eval(false, new Condition("list", new Condition($EQ, 10)).matches(test3));
        eval(true, new Condition("a", new Condition($EQ, 1)).matches(test3));
        eval(false, new Condition("a", new Condition($EQ, 3)).matches(test3));
        eval(true, new Condition("list", new Condition($EQ, list(1, 2, 4, 6, 7, 8, 9))).matches(test3));
        eval(false, new Condition("list", new Condition($EQ, list(1, 2))).matches(test3));
        eval(true, new Condition("list3", new Condition($EQ, list(3, 6))).matches(test3));
        eval(true, new Condition("obj.list", new Condition($EQ, new String[] { "s1", "s2" })).append("obj.list2", new Condition($EQ, new Object[] { new Condition($REGEX, ".*").append($OPTIONS, "m"), "b" })).matches(test3));
        // pseudo: test3.obj.$type.matches(.*Embeded.*)
        // §§ROOT.test2.sum resolves to a list, and $sum treats sublists as non-numeric - ignores them
        eval(119, new Condition($SUM, Arrays.asList(new Object[] { 0, 0, "§§ROOT.test2.sum", 119 })).evaluate(test));
        eval(true, new Condition("obj.§type", new Condition($REGEX, ".*Embed.*")).matches(test3));
        // pseudo: test.string.matches(.*ein.*)
        eval(true, new Condition("string", new Condition($REGEX, ".*ein.*")).matches(test));
        // pseudo: Regex.match(.*ein.*,test.string)
        eval(true, new Condition($REGEX, list(".*ein.*", "§string")).matches(test));
        eval(true, new Condition($REGEX, new Object[] { ".*Map.*", $TYPE }).matches(test3));
        if (true) {
            // result from lt = true && c!=true << don't forget implicit EQ Op
            eval(false, new Condition("c", new Condition($EXISTS, 1).append($LT, new Object[] { new Condition($SUM, new Object[] { "§§ROOT.test2.sum" }), 10 })).matches(test));
            eval(false, new Condition("c", new Condition($EXISTS, 1).append($LT, new Object[] { new Condition($SUM, new Object[] { "§§ROOT.test2.sum" }), 0 })).matches(test));
            if (true) {
                eval(true, new Condition("c", new Condition($EQ, new Condition($MAX, Arrays.asList(new Object[] { 1, 2 })))).matches(test));
                eval(true, new Condition("c", new Condition($EQ, new Condition($MIN, Arrays.asList(new Object[] { 2, 3, 4 })))).matches(test));
                // or (exists:feldA->summe(feldA)>10 , exists:feldB->Summe(feldB)>20);
                // EQ is used as aggregate operator
                eval(true, new Condition("boo", new Condition($EQ, Arrays.asList(new Object[] { 1, "§§ROOT.a" })).append($OPTIONS, new Condition(Condition.OPTIONS_AGGREGATE, true))).matches(test));
                eval(true, new Condition("c", new Condition($MAX, Arrays.asList(new Object[] { 1, 2 }))).matches(test));
                eval(true, new Condition("d", new Condition($GT, new Condition($MULTIPLY, Arrays.asList(new Object[] { 4, 1 })))).matches(test));
                eval(true, new Condition("c", new Condition($EQ, new Condition($DIVIDE, Arrays.asList(new Object[] { 4, 2 })))).matches(test));
                eval(true, new Condition("d", new Condition($EQ, new Condition($MULTIPLY, Arrays.asList(new Object[] { 4, 2 })))).matches(test));
                eval(true, new Condition("d", new Condition($LTE, new Condition($MULTIPLY, Arrays.asList(new Object[] { 8 })))).matches(test));
                // "§§ROOT.test2.sum" is an int array, and sum treats sub-lists as non numeric and does not count them.
                eval(true, new Condition("120", new Condition($EQ, new Condition($SUM, Arrays.asList(new Object[] { 0, 1, "§§ROOT.test2.sum", 119 })))).matches(test));
            }
            // test aggregation lt
            eval(true, new Condition($LT, Arrays.asList(new Object[] { 110, new Condition($MAX, Arrays.asList(new Object[] { new Condition($SUBTRACT, new Object[] { 2, 2 }), new Condition($MAX, new Object[] { 4, "§§ROOT.c", new Condition($SUM, Arrays.asList(new Object[] { 1, 25, 99, -5 })) }) })) })).matches(test));
            // test conditional lt
            eval(true, new Condition("110", new Condition($LT, new Condition($MAX, Arrays.asList(new Object[] { new Condition($SUBTRACT, new Object[] { 2, 2 }), new Condition($MAX, new Object[] { 4, "§§ROOT.c", new Condition($SUM, Arrays.asList(new Object[] { 1, 25, 99, -5 })) }) })))).matches(test));
            // test aggregation gt
            eval(true, new Condition($GT, Arrays.asList(new Object[] { 130, new Condition($MAX, Arrays.asList(new Object[] { new Condition($SUBTRACT, new Object[] { 2, 2 }), new Condition($MAX, new Object[] { 4, "§§ROOT.c", new Condition($SUM, Arrays.asList(new Object[] { 1, 25, 99, -5 })) }) })) })).matches(test));
            // test conditional gt
            eval(true, new Condition("130", new Condition($GT, new Condition($MAX, Arrays.asList(new Object[] { new Condition($SUBTRACT, new Object[] { 2, 2 }), new Condition($MAX, new Object[] { 4, "§§ROOT.c", new Condition($SUM, Arrays.asList(new Object[] { 1, 25, 99, -5 })) }) })))).matches(test));
            // test aggregation $LTE
            eval(true, new Condition($LTE, Arrays.asList(new Object[] { 120, new Condition($MAX, Arrays.asList(new Object[] { new Condition($SUBTRACT, new Object[] { 2, 2 }), new Condition($MAX, new Object[] { 4, "§§ROOT.c", new Condition($SUM, Arrays.asList(new Object[] { 1, 25, 99, -5 })) }) })) })).matches(test));
            // test conditional $LTE
            eval(true, new Condition("120", new Condition($LTE, new Condition($MAX, Arrays.asList(new Object[] { new Condition($SUBTRACT, new Object[] { 2, 2 }), new Condition($MAX, new Object[] { 4, "§§ROOT.c", new Condition($SUM, Arrays.asList(new Object[] { 1, 25, 99, -5 })) }) })))).matches(test));
            // test aggregation $$GTE
            eval(true, new Condition($GTE, Arrays.asList(new Object[] { 120, new Condition($MAX, Arrays.asList(new Object[] { new Condition($SUBTRACT, new Object[] { 2, 2 }), new Condition($MAX, new Object[] { 4, "§§ROOT.c", new Condition($SUM, Arrays.asList(new Object[] { 1, 25, 99, -5 })) }) })) })).matches(test));
            // test conditional $$GTE
            eval(true, new Condition("120", new Condition($GTE, new Condition($MAX, Arrays.asList(new Object[] { new Condition($SUBTRACT, new Object[] { 2, 2 }), new Condition($MAX, new Object[] { 4, "§§ROOT.c", new Condition($SUM, Arrays.asList(new Object[] { 1, 25, 99, -5 })) }) })))).matches(test));
            // test aggregation equals
            eval(true, new Condition($EQ, Arrays.asList(new Object[] { 120, new Condition($MAX, Arrays.asList(new Object[] { new Condition($SUBTRACT, new Object[] { 2, 2 }), new Condition($MAX, new Object[] { 4, "§§ROOT.c", new Condition($SUM, Arrays.asList(new Object[] { 1, 25, 99, -5 })) }) })) })).append($OPTIONS, new Condition(Condition.OPTIONS_AGGREGATE, true)).matches(test));
            // test conditional equals
            eval(true, new Condition("120", new Condition($EQ, new Condition($MAX, Arrays.asList(new Object[] { new Condition($SUBTRACT, new Object[] { 2, 2 }), new Condition($MAX, new Object[] { 4, "§§ROOT.c", new Condition($SUM, Arrays.asList(new Object[] { 1, 25, 99, -5 })) }) })))).matches(test));
            // test evaluate
            eval(120, new Condition($MAX, Arrays.asList(new Object[] { new Condition($SUBTRACT, new Object[] { 2, 2 }), new Condition($MAX, new Object[] { 4, "§§ROOT.c", new Condition($SUM, Arrays.asList(new Object[] { 1, 25, 99, -5 })) }) })).evaluate(test));
            eval(true, new Condition("a", new Condition($EQ, new Condition($SUM, Arrays.asList(new Object[] { 1, "§§ROOT.leer" })))).matches(test));
            eval(true, new Condition("a", new Condition($EQ, new Condition($SUM, Arrays.asList(new Object[] { 1, "§§ROOT.leer" })))).matches(test));
            eval(true, new Condition("c", new Condition($EQ, new Condition($SUM, Arrays.asList(new Object[] { 1, 1 })))).matches(test));
            eval(true, new Condition("c", new Condition($EQ, new Condition($SUM, Arrays.asList(new Object[] { "§§ROOT.a", "§§ROOT.b" })))).matches(test));
            if (false) {
                return;
            }
        }
        if (true) {
            eval(true, new Condition("§§THIS[0]", new Condition(Condition.$EXISTS, true)).matches(new String[] { "" }));
            eval(false, new Condition("§§THIS[0]", new Condition(Condition.$REGEX, "^https?://.+$")).matches(new String[] { "" }));
            System.out.println(JSonStorage.serializeToJson(JSonStorage.serializeToJson(new Condition("§§THIS[0]", new Condition(Condition.$REGEX, "^https?://.+$")))));
            eval(true, new Condition("§§THIS[0]", new Condition(Condition.$REGEX, "^https?://.+$")).matches(new String[] { "http://www.google.de" }));
            eval(false, new Condition("§§THIS[0].§size", new Condition(Condition.$GT, 0)).matches(new String[] { "" }));
            eval(true, new Condition("§§THIS[0].§size", new Condition(Condition.$GT, 0)).matches(new String[] { "1" }));
            eval(true, new Condition(Condition.$$THIS + "." + Condition.$SIZE, new Condition($GT, 0)).matches(new String[] { "" }));
            eval(false, new Condition(Condition.$$THIS + "." + Condition.$SIZE, new Condition($GT, 0)).matches(new String[] {}));
        }
        Condition con = new Condition("obj.b", false);
        long started = System.currentTimeMillis();
        boolean success = true;
        for (int i = 0; i < 1; i++) {
            success &= con.matches(test3);
        }
        System.out.println(success + " " + (System.currentTimeMillis() - started));
        eval(true, new Condition($TYPE, HashMap.class.getName()).matches(test3));
        eval(true, new Condition("obj.b", new Condition($EQ, false)).append("obj.a", new Condition($EXISTS, false)).matches(test3));
        eval(true, new Condition("obj.b", new Condition($EQ, false)).append("obj.a", new Condition($EXISTS, false)).matches(test3));
        eval(true, new Condition("obj.list", new Condition($EQ, new String[] { "s1", "s2" })).append("obj.list2", new Condition($EQ, new Object[] { new Condition($REGEX, ".*").append($OPTIONS, "m"), "b" })).matches(test3));
        eval(true, new Condition("a", new Condition($IN, new int[] { 3, 2, 1 })).matches(test3));
        eval(true, new Condition("a", new Condition($NIN, new int[] { 4, 5, 6 })).matches(test3));
        eval(true, new Condition("a", 1).matches(test3));
        eval(true, new Condition($NOT, new Condition("a", 2)).matches(test3));
        eval(true, new Condition("a", new Condition($NE, 2)).append("c", new Condition($NE, 1)).matches(test3));
        eval(true, new Condition($OR, new Condition[] { new Condition("a", 2), new Condition("a", new Condition($NE, 2)).append("c", new Condition($NE, 1)) }).matches(test3));
        eval(true, new Condition($AND, new Condition[] { new Condition("a", 1), new Condition("a", new Condition($NE, 2)).append("c", new Condition($NE, 1)) }).matches(test3));
        eval(true, new Condition("a", new Condition($EQ, 1)).matches(test3));
        eval(true, new Condition("a", new Condition($GTE, 1).append($LT, 2)).matches(test3));
        eval(true, new Condition("sa[0]", "a").matches(test3));
        eval(true, new Condition("sa", new Condition($IN, new String[] { "h", "b" })).matches(test3));
        eval(true, new Condition("sa", new Condition($NIN, new String[] { "c", "d" })).matches(test3));
        String json = JSonStorage.serializeToJson(new Condition("sa", new Condition($IN, new String[] { "h", "b" })));
        System.out.println(json);
        query = JSonStorage.restoreFromString(json, Condition.TYPE);
        System.out.println(query.matches(test3));
    }
}
