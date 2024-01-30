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
import java.util.HashSet;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.annotations.SpinnerValidator;
import org.appwork.storage.config.events.ConfigEventListener;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.storage.config.handler.StorageHandler;
import org.appwork.testframework.AWTest;

/**
 * @author thomas
 *
 */
public class Test extends AWTest {
    public static void main(final String[] args) throws InterruptedException {
        run();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {

        // new PerformanceObserver().start();
        JsonConfig.create(MyInterface.class);
        /*
         * 1. Define an INterface with all required getters and setters. Use Annotations to give defaultvalues or cryptinfos
         */
        /*
         * 2. Create your storage. The Factory will check your interface and throw Exceptions if it is malformed. This helps to find Errors
         * immediately. The Check not only checks directly used Datatypes, but runs through the whole TypeStructure of the interface.
         */

        final MyInterface jc = JsonConfig.create(MyInterface.class);

        HashSet<String> storedSet = jc.getSet();
        if (storedSet == null) {
            storedSet = new HashSet<String>();
        }
        storedSet.add(System.currentTimeMillis() + "");
        jc.setSet(storedSet);
        //
        jc._getStorageHandler().getEventSender().addListener(new ConfigEventListener() {

            @Override
            public void onConfigValidatorError(final KeyHandler<Object> keyHandler, final Object invalidValue, final ValidationException validateException) {
        	}

            @Override
            public void onConfigValueModified(final KeyHandler<Object> keyHandler, final Object newValue) {
                LogV3.info("New value: " + keyHandler);
            }

        });
        MyInterface.INT.getEventSender().addListener(new GenericConfigEventListener<Integer>() {

            @Override
            public void onConfigValidatorError(final KeyHandler<Integer> keyHandler, final Integer invalidValue, final ValidationException validateException) {
           	}

            @Override
            public void onConfigValueModified(final KeyHandler<Integer> keyHandler, final Integer newValue) {
			}

        });

        double[] ar = jc.getDoubleArray();
        jc.setDoubleArray(new double[] { 1.2, 3.4, 5.6 });
        LogV3.info(JSonStorage.serializeToJson(jc.getObject()));
        ar = jc.getDoubleArray();
        /*
         * 3. Use getters and setters as if your storage would be a normal instance.
         */
        final TestObject o = new TestObject();
        o.setA(36287);
        jc.setObject(o);
        jc.setIntArray(new int[] { 1, 2, 3, 4, 5 });

        final ArrayList<TestObject> list = new ArrayList<TestObject>();
        list.add(o);
        list.add(new TestObject());
        jc.setGenericList(list);
        jc.setObject(o);
        LogV3.info(JSonStorage.serializeToJson(jc.getIntArray()));
        LogV3.info(JSonStorage.serializeToJson(jc.getObject()));
        LogV3.info(JSonStorage.serializeToJson(jc.getGenericList()));

        LogV3.info(JSonStorage.serializeToJson(jc.getStringArray()));

        /*
         * 4. get values by key
         */

        final StorageHandler<?> storageHandler = MyInterface.CFG._getStorageHandler();
        LogV3.info("" + storageHandler.getValue("Float"));
        LogV3.info("" + MyInterface.CFG.getInt());
        // Set Statics in the interface to use compiletime checks
        /**
         * Validators
         */
        try {
            MyInterface.CFG.setInt(2000);
        } catch (final ValidationException e) {
            LogV3.info("OK. 2000 is not valid for " + MyInterface.INT.getAnnotation(SpinnerValidator.class));
        }
        LogV3.info("TEST SUCCESSFULL");
        /**
         * Defaults;
         *
         *
         *
         */
        LogV3.info("" + MyInterface.CFG.getDefault());

    }
}
