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
package org.appwork.utils;

import java.util.concurrent.atomic.AtomicReference;

/**
 * @author daniel
 * 
 *         NullSafeAtomicReference
 * 
 *         AtomicReference which can differ between set/null/empty (tri-state)
 * 
 */
public class NullsafeAtomicReference<V> {

    private static final NullObject       NULL      = new NullObject();
    private static final Object           EMPTY     = new Object();
    private final AtomicReference<Object> reference = new AtomicReference<Object>(NullsafeAtomicReference.EMPTY);

    public NullsafeAtomicReference() {
    }

    public NullsafeAtomicReference(final V initialValue) {
        this.set(initialValue);
    }

    public final boolean compareAndSet(final V expect, final V update) {
        Object nullSafeUpdate = update;
        if (NullsafeAtomicReference.NULL.equals(nullSafeUpdate)) {
            nullSafeUpdate = NullsafeAtomicReference.NULL;
        }
        if (NullsafeAtomicReference.NULL.equals(expect)) {
            return this.reference.compareAndSet(NullsafeAtomicReference.NULL, nullSafeUpdate) || this.reference.compareAndSet(NullsafeAtomicReference.EMPTY, nullSafeUpdate);
        } else {
            return this.reference.compareAndSet(expect, nullSafeUpdate);
        }

    }

    public final V get() {
        final Object get = this.reference.get();
        if (NullsafeAtomicReference.NULL.equals(get) || NullsafeAtomicReference.EMPTY == get) { return null; }
        return (V) get;
    }

    public V getAndClear() {
        final Object get = this.reference.getAndSet(NullsafeAtomicReference.EMPTY);
        if (NullsafeAtomicReference.NULL.equals(get) || NullsafeAtomicReference.EMPTY == get) { return null; }
        return (V) get;
    }

    public final V getAndSet(final V newValue) {
        Object set = newValue;
        if (NullsafeAtomicReference.NULL.equals(set)) {
            set = NullsafeAtomicReference.NULL;
        }
        final Object get = this.reference.getAndSet(set);
        if (NullsafeAtomicReference.NULL.equals(get) || NullsafeAtomicReference.EMPTY == get) { return null; }
        return (V) get;
    }

    public boolean isValueSet() {
        return this.reference.get() != NullsafeAtomicReference.EMPTY;
    }

    public final void set(final V newValue) {
        if (NullsafeAtomicReference.NULL.equals(newValue)) {
            this.reference.set(NullsafeAtomicReference.NULL);
        } else {
            this.reference.set(newValue);
        }

    }

}
