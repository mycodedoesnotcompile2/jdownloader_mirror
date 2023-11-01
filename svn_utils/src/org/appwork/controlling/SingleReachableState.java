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
package org.appwork.controlling;

import java.util.ArrayList;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.NullsafeAtomicReference;

/**
 * @author daniel
 *
 */
public class SingleReachableState {

    private final NullsafeAtomicReference<ArrayList<Runnable>> stateMachine;

    private final String                                       name;

    public SingleReachableState(final String name) {

        this.stateMachine = new NullsafeAtomicReference<ArrayList<Runnable>>(new ArrayList<Runnable>());
        this.name = name;
    }

    public void executeWhen(final Runnable reached, final Runnable notreached) {
        if (reached == null && notreached == null) {
            return;
        }
        while (true) {
            final ArrayList<Runnable> runnables = this.stateMachine.get();
            if (runnables == null) {
                this.run(reached);
                return;
            }
            if (reached != null) {
                final ArrayList<Runnable> newRunnables = new ArrayList<Runnable>(runnables);
                newRunnables.add(reached);
                if (this.stateMachine.compareAndSet(runnables, newRunnables)) {
                    this.run(notreached);
                    return;
                }
            } else {
                this.run(notreached);
                return;
            }
        }
    }

    public void executeWhenReached(final Runnable run) {
        this.executeWhen(run, null);
    }

    public boolean isReached() {
        return this.stateMachine.get() == null;
    }

    public void waitForReached() throws InterruptedException {
        if (this.stateMachine.get() != null) {
            while (true) {
                synchronized (stateMachine) {
                    if (this.stateMachine.get() == null) {
                        break;
                    }
                    stateMachine.wait();
                }
            }
        }
    }

    private void run(final Runnable run) {
        try {
            if (run != null) {
                run.run();
            }
        } catch (final Throwable e) {
            LogV3.I().getDefaultLogger().log(e);
        }
    }

    public void setReached() {
        final ArrayList<Runnable> runnables;
        synchronized (stateMachine) {
            runnables = this.stateMachine.getAndSet(null);
            stateMachine.notifyAll();
        }
        if (runnables != null) {
            for (final Runnable run : runnables) {
                this.run(run);
            }
        }
    }

    @Override
    public String toString() {
        return "SingleReachableState: " + this.name + " reached: " + (this.stateMachine.get() != null);
    }
}
