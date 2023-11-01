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

import org.appwork.storage.config.JsonConfig;


/**
 * @author thomas
 * 
 */
public class PerformanceTest {
    public static void main(final String[] args) throws InterruptedException {
        PerformanceObserver po = null;
        // po = new PerformanceObserver();

        try {
            final MyInterface jc = JsonConfig.create(MyInterface.class);

            long rounds = 1000000;
            run(rounds, true);
            po = new PerformanceObserver();
            run(rounds, false);
            po.print();
            System.out.println("TEST SUCCESSFULL");
        } catch (final RuntimeException e) {
            // seems like the interface is malformed
            org.appwork.loggingv3.LogV3.log(e);

            System.out.println("TEST FAILED");
        }

        // System.out.println("ConfigTime: "+MyInterface.SH.getNanoTime()+" "+JsonConfig.create(MyInterface.class).getStorageHandler().getNanoTime());

        // System.exit(1);

    }

    /**
     * @param rounds
     * @param print
     */
    private static void run(long rounds, boolean print) {
        long t;

        t = System.nanoTime();
        for (long x = 0; x < rounds; x++) {
            JsonConfig.create(MyInterface.class).getInt();
        }
        if (print) System.out.println((System.nanoTime() - t) / rounds + "\t ns READ JSonConfig Mapping+Proxy");

        t = System.nanoTime();
        for (long x = 0; x < rounds; x++) {
            MyInterface.SH.getKeyHandler("Int").getValue();
        }
        if (print) System.out.println((System.nanoTime() - t) / rounds + "\t ns READ by String key");

        t = System.nanoTime();
        for (long x = 0; x < rounds; x++) {
            MyInterface.CFG.getInt();
        }
        if (print) System.out.println((System.nanoTime() - t) / rounds + "\t ns READ JSonConfig bypass - Static Instance");

        t = System.nanoTime();
        for (long x = 0; x < rounds; x++) {
            MyInterface.INT.getValue();
        }
        if (print) System.out.println((System.nanoTime() - t) / rounds + "\t ns READ Proxy Bypass - static Keyhandler");
        TestObject o = new TestObject();
        t = System.nanoTime();
        for (long x = 0; x < rounds; x++) {
            o.getA();
        }
        if (print) System.out.println((System.nanoTime() - t) / rounds + "\t ns READ Direct");

        t = System.nanoTime();
        for (long x = 0; x < rounds; x++) {
            JsonConfig.create(MyInterface.class).setInt(5);
        }
        if (print) System.out.println((System.nanoTime() - t) / rounds + "\t ns WRITE JSonConfig Mapping+Proxy");

        t = System.nanoTime();
        for (long x = 0; x < rounds; x++) {
            MyInterface.SH.getKeyHandler("Int").setValue(5);
        }
        if (print) System.out.println((System.nanoTime() - t) / rounds + "\t ns WRITE by String key");

        t = System.nanoTime();
        for (long x = 0; x < rounds; x++) {
            MyInterface.CFG.setInt(5);
        }
        if (print) System.out.println((System.nanoTime() - t) / rounds + "\t ns WRITE JSonConfig bypass - Static Instance");

        t = System.nanoTime();
        for (long x = 0; x < rounds; x++) {
            MyInterface.INT.setValue(5);
        }
        if (print) System.out.println((System.nanoTime() - t) / rounds + "\t ns WRITE Proxy Bypass - static Keyhandler");

        t = System.nanoTime();
        for (long x = 0; x < rounds; x++) {
            o.setA(5);
        }
        if (print) System.out.println((System.nanoTime() - t) / rounds + "\t ns WRITE Direct");

    }
}
