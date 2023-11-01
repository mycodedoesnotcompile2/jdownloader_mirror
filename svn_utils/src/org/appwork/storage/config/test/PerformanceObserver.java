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
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;

import org.appwork.storage.config.handler.StorageHandler;

/**
 * @author Thomas
 * 
 */
public class PerformanceObserver extends Thread {
public PerformanceObserver(){
    StorageHandler.PROFILER_MAP = new HashMap<String, Long>();
    StorageHandler.PROFILER_CALLNUM_MAP=new HashMap<String, Long>();
   
}

    private boolean profileMethods    = true;
    private long interval=30000;

    public long getInterval() {
        return interval;
    }

    public void setInterval(long interval) {
        this.interval = interval;
    }



    public boolean isProfileMethods() {
        return profileMethods;
    }

    public void setProfileMethods(boolean profileMethods) {
        this.profileMethods = profileMethods;
    }

    public void run() {

        while (true) {
     
            print();

        }
    }

    /**
     * 
     */
    public void print() {
        if(profileMethods){
            // /
            java.util.List<Entry<String, Long>> entries = new ArrayList<Entry<String, Long>>();
            for (Iterator<Entry<String, Long>> it = StorageHandler.PROFILER_MAP.entrySet().iterator(); it.hasNext();) {
                entries.add(it.next());
            }
            Collections.sort(entries, new Comparator<Entry<String, Long>>() {

                @Override
                public int compare(Entry<String, Long> o1, Entry<String, Long> o2) {

                    return o1.getValue().compareTo(o2.getValue());
                }

            });

            for (Entry<String, Long> i : entries) {
              Long invocations = StorageHandler.PROFILER_CALLNUM_MAP.get(i.getKey());
              
              
                System.out.println((i.getValue() / 1000) / 1000f + "ms \t"+invocations+"#\t"+(i.getValue()/invocations)+"ns/i  " + i.getKey());
            }
            }
            try {
                Thread.sleep(interval);
            } catch (InterruptedException e) {                
                e.printStackTrace();
            }
    }

    public void start() {
       super.start();
    }
}
