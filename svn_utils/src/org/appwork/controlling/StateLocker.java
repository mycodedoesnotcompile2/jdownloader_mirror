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

public class StateLocker implements StateEventListener {

    private StateMachine[] stateMachines;
    private int counter;
    private State waitState;
    private State[] exceptions;
    private State interruptState = null;
    private StateMachine interruptStatemachine;

    /**
     * @param machines
     */
    public StateLocker(StateMachine... machines) {
        stateMachines = machines.clone();
        counter = 0;
    }

    /**
     * @param stoppedState
     * @throws InterruptedException
     * @throws StateExceptionException
     */
    public void lockUntilAllHavePassed(final State state, final State... exceptions) throws InterruptedException, StateExceptionException {
        waitState = state;
        this.exceptions = exceptions;
        if (stateMachines == null || stateMachines.length == 0) return;
        try {
            for (StateMachine st : stateMachines) {
                for (State e : exceptions) {
                    if (st.hasPassed(e)) throw new StateExceptionException(st, e);
                }

                if (st.hasPassed(state)) {
                    increaseCounter();

                } else {
                    st.addListener(this);
                }
            }

            main: while (counter < stateMachines.length) {
                synchronized (this) {
                    this.wait(2000);
                }
                if (interruptState != null) { throw new StateExceptionException(interruptStatemachine, interruptState); }
                for (StateMachine st : stateMachines) {
                    if (!st.hasPassed(state)) {
                        continue main;
                    }
                }

                break;
            }
        } finally {
            for (StateMachine st : stateMachines) {
                st.removeListener(this);

            }

        }
    }

    /**
     * 
     */
    private synchronized int increaseCounter() {
        return ++counter;
    }

    public void onStateChange(StateEvent event) {
        if (event.getNewState() == waitState) {
            if (increaseCounter() == stateMachines.length) {
                synchronized (this) {
                    this.notify();
                }
                event.getStateMachine().removeListener(this);
            }
        } else {
            for (State s : exceptions) {
                if (event.getNewState() == s) {
                    this.interruptState = s;
                    this.interruptStatemachine = event.getStateMachine();
                    synchronized (this) {
                        this.notify();
                    }
                    return;
                }
            }
        }
    }

    public void onStateUpdate(StateEvent event) {

    }

}
