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
package org.appwork.storage.config.test;

import org.appwork.storage.config.ConfigInterface;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.BooleanKeyHandler;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.storage.config.swing.ValueProvider;
import org.appwork.storage.config.swing.ValueProviderListener;
import org.appwork.storage.config.swing.models.KeyHandlerProviderBridge;
import org.appwork.storage.config.swing.models.PropertyHandlerProviderBridge;
import org.appwork.storage.flexijson.config.FlexiConfigFromJsonConfigBuilder;
import org.appwork.storage.flexijson.mapper.interfacestorage.InterfaceStorage;
import org.appwork.storage.flexijson.mapper.interfacestorage.PropertyHandler;
import org.appwork.storage.flexijson.mapper.interfacestorage.PropertyHandlerImpl;
import org.appwork.storage.flexijson.mapper.interfacestorage.PropertyHandlerListener;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Application;

/**
 * @author thomas
 * @date 22.08.2022
 *
 */
public class WeakValueProviderBridgeTest extends AWTest {
    public static void main(String[] args) {
        run();
    }

    /**
     * @author thomas
     * @date 22.08.2022
     *
     */
    public static class TestListener implements GenericConfigEventListener<Boolean> {
        @Override
        public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
            System.out.println(newValue);
        }

        @Override
        public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
        }
    }

    public static interface TestConfigInterface extends ConfigInterface {
        public boolean getBoolean();

        public void setBoolean(boolean b);
    }

    protected boolean loopResult;
    protected boolean loopResultProvider;
    protected boolean loopResultProvider2;

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        TestConfigInterface instance = JsonConfig.create(TestConfigInterface.class);
        BooleanKeyHandler keyHandler = instance._getStorageHandler().getKeyHandler("boolean", BooleanKeyHandler.class);
        testWeak(keyHandler);
        testStrong(keyHandler);
        TestConfigInterface storage = new FlexiConfigFromJsonConfigBuilder<TestConfigInterface>(TestConfigInterface.class, Application.getResource("test.json")).getStorage();
        PropertyHandler<TestConfigInterface, Boolean> propertyHandler = InterfaceStorage.getPropertyHandler(storage, "boolean", boolean.class);
        testWeak(propertyHandler);
        testStrong(propertyHandler);
    }

    /**
     * @param propertyHandler
     * @throws Exception
     */
    private void testStrong(PropertyHandler<TestConfigInterface, Boolean> keyHandler) throws Exception {
        TestListener tl;
        keyHandler.getEventSender().addListener(new PropertyHandlerListener<WeakValueProviderBridgeTest.TestConfigInterface, Boolean>() {
            @Override
            public void onInterfaceValueSet(TestConfigInterface storage, PropertyHandlerImpl<TestConfigInterface, Boolean> property, Boolean oldValue, Boolean newValue) {
                loopResult = newValue == Boolean.TRUE;
            }
        }, false);
        //
        PropertyHandlerProviderBridge<Boolean> bridge = new PropertyHandlerProviderBridge<Boolean>(keyHandler);
        bridge.register(new ValueProviderListener<Boolean>() {
            @Override
            public void onValueModified(ValueProvider<Boolean> owner, Boolean newValue) {
                loopResultProvider = newValue == Boolean.TRUE;
            }

            @Override
            public void onValueValidationError(ValueProvider<Boolean> owner, Boolean invalidValue, Exception exception) {
			}
        }, false);
        ValueProviderListener<Boolean> listener;
        bridge.register(listener = new ValueProviderListener<Boolean>() {
            @Override
            public void onValueModified(ValueProvider<Boolean> owner, Boolean newValue) {
                loopResultProvider2 = newValue == Boolean.TRUE;
            }

            @Override
            public void onValueValidationError(ValueProvider<Boolean> owner, Boolean invalidValue, Exception exception) {
			}
        }, true);
        bridge.register(listener, false);
        bridge = null;
        keyHandler.set(false);
        loopResult = false;
        keyHandler.set(true);
        assertTrue(loopResult);
        assertTrue(loopResultProvider);
        assertTrue(loopResultProvider2);
        gcForSure();
        keyHandler.set(false);
        assertFalse(loopResult);
        assertFalse(loopResultProvider);
        assertFalse(loopResultProvider2);
        gcForSure();
        keyHandler.set(true);
        assertTrue(loopResult);
        assertTrue(loopResultProvider);
        assertTrue(loopResultProvider2);
    }

    /**
     * @param propertyHandler
     * @throws Exception
     */
    private void testWeak(PropertyHandler<TestConfigInterface, Boolean> propertyHandler) throws Exception {
        TestListener tl;
        PropertyHandlerListener<TestConfigInterface, Boolean> listener;
        propertyHandler.getEventSender().addListener(listener = new PropertyHandlerListener<WeakValueProviderBridgeTest.TestConfigInterface, Boolean>() {
            @Override
            public void onInterfaceValueSet(TestConfigInterface storage, PropertyHandlerImpl<TestConfigInterface, Boolean> property, Boolean oldValue, Boolean newValue) {
                loopResult = newValue == Boolean.TRUE;
            }
        }, true);
        //
        PropertyHandlerProviderBridge<Boolean> bridge = new PropertyHandlerProviderBridge<Boolean>(propertyHandler);
        ValueProviderListener<Boolean> listenerProvider = new ValueProviderListener<Boolean>() {
            @Override
            public void onValueModified(ValueProvider<Boolean> owner, Boolean newValue) {
                loopResultProvider = newValue == Boolean.TRUE;
            }

            @Override
            public void onValueValidationError(ValueProvider<Boolean> owner, Boolean invalidValue, Exception exception) {
            }
        };
        bridge.register(listenerProvider, true);
        bridge = null;
        propertyHandler.set(false);
        loopResult = false;
        propertyHandler.set(true);
        assertTrue(loopResult);
        assertTrue(loopResultProvider);
        gcForSure();
        propertyHandler.set(false);
        assertFalse(loopResult);
        assertFalse(loopResultProvider);
        listener = null;
        listenerProvider = null;
        gcForSure();
        propertyHandler.set(true);
        gcForSure();
        assertFalse(loopResult);
        assertFalse(loopResultProvider);
        assertFalse(propertyHandler.getEventSender().hasListener());
    }

    public void testStrong(BooleanKeyHandler keyHandler) throws Exception, InterruptedException {
        TestListener tl;
        keyHandler.getEventSender().addListener(new GenericConfigEventListener<Boolean>() {
            @Override
            public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
                loopResult = newValue == Boolean.TRUE;
            }

            @Override
            public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
			}
        }, false);
        //
        KeyHandlerProviderBridge bridge = new KeyHandlerProviderBridge(keyHandler);
        bridge.register(new ValueProviderListener<Boolean>() {
            @Override
            public void onValueModified(ValueProvider<Boolean> owner, Boolean newValue) {
                loopResultProvider = newValue == Boolean.TRUE;
            }

            @Override
            public void onValueValidationError(ValueProvider<Boolean> owner, Boolean invalidValue, Exception exception) {
           	}
        }, false);
        ValueProviderListener<Boolean> listener;
        bridge.register(listener = new ValueProviderListener<Boolean>() {
            @Override
            public void onValueModified(ValueProvider<Boolean> owner, Boolean newValue) {
                loopResultProvider2 = newValue == Boolean.TRUE;
            }

            @Override
            public void onValueValidationError(ValueProvider<Boolean> owner, Boolean invalidValue, Exception exception) {
            }
        }, true);
        bridge.register(listener, false);
        bridge = null;
        keyHandler.setValue(false);
        loopResult = false;
        keyHandler.setValue(true);
        assertTrue(loopResult);
        assertTrue(loopResultProvider);
        assertTrue(loopResultProvider2);
        gcForSure();
        keyHandler.setValue(false);
        assertFalse(loopResult);
        assertFalse(loopResultProvider);
        assertFalse(loopResultProvider2);
        gcForSure();
        keyHandler.setValue(true);
        assertTrue(loopResult);
        assertTrue(loopResultProvider);
        assertTrue(loopResultProvider2);
    }

    public void testWeak(BooleanKeyHandler keyHandler) throws Exception, InterruptedException {
        TestListener tl;
        GenericConfigEventListener<Boolean> listener;
        keyHandler.getEventSender().addListener(listener = new GenericConfigEventListener<Boolean>() {
            @Override
            public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
                loopResult = newValue == Boolean.TRUE;
            }

            @Override
            public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
            }
        }, true);
        //
        KeyHandlerProviderBridge bridge = new KeyHandlerProviderBridge(keyHandler);
        ValueProviderListener<Boolean> listenerProvider = new ValueProviderListener<Boolean>() {
            @Override
            public void onValueModified(ValueProvider<Boolean> owner, Boolean newValue) {
                loopResultProvider = newValue == Boolean.TRUE;
            }

            @Override
            public void onValueValidationError(ValueProvider<Boolean> owner, Boolean invalidValue, Exception exception) {
            }
        };
        bridge.register(listenerProvider, true);
        bridge = null;
        keyHandler.setValue(false);
        loopResult = false;
        keyHandler.setValue(true);
        assertTrue(loopResult);
        assertTrue(loopResultProvider);
        gcForSure();
        keyHandler.setValue(false);
        assertFalse(loopResult);
        assertFalse(loopResultProvider);
        listener = null;
        listenerProvider = null;
        gcForSure();
        keyHandler.setValue(true);
        gcForSure();
        assertFalse(loopResult);
        assertFalse(loopResultProvider);
        assertFalse(keyHandler.getEventSender().hasListener());
    }

    public void gcForSure() throws InterruptedException {
        for (int i = 0; i < 20; i++) {
            Thread.sleep(10);
            System.gc();
        }
    }
}
