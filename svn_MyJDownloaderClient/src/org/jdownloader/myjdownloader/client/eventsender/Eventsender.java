/**
 * 
 * ====================================================================================================================================================
 *         "My JDownloader Client" License
 *         The "My JDownloader Client" will be called [The Product] from now on.
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
package org.jdownloader.myjdownloader.client.eventsender;


import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.EventListener;

/**
 * The Eventsenderclass is the core of the Eventsystem. it can be used to design new Eventbroadcaster Systems easily.
 * 
 * Guidelines:<br>
 * 1. CReate a new MyEventSender extends org.appwork.utils.event.Eventsender<ListenerType, EventType> <br>
 * 2. Create MyListenerType extends java.util.EventListener<br>
 * 3. CReate MyEvent extends org.appwork.utils.event.SimpleEvent<CallerType, ParameterType, TypeEnumType><br>
 * 
 * <br>
 * TypeEnumType is usually a intern enum which defines all available eventtypes
 * 
 * @author $Author: unknown$
 * 
 */

public abstract class Eventsender<ListenerType extends EventListener, EventType extends DefaultEvent> {
   

    /**
     * List of registered Eventlistener
     */

    transient volatile protected java.util.List<ListenerType>                strongListeners = null;
    transient volatile protected java.util.List<WeakReference<ListenerType>> weakListener    = null;

    private final Object                                                     LOCK            = new Object();

    /**
     * List of Listeners that are requested for removal
     * 
     */

    /**
     * Creates a new Eventsender Instance
     */
    public Eventsender() {
        this.strongListeners = new ArrayList<ListenerType>();
        this.weakListener = new ArrayList<WeakReference<ListenerType>>();

    }

    /**
     * Adds a list of listeners
     * 
     * @param listener
     */
    public void addAllListener(final java.util.List<ListenerType> listener) {
        this.addAllListener(listener, false);
    }

    public void addAllListener(final java.util.List<ListenerType> listener, final boolean weak) {
        for (final ListenerType l : listener) {
            this.addListener(l, weak);
        }
    }

    public void addListener(final ListenerType t) {
        this.addListener(t, false);
    }

    /**
     * Add a single Listener
     * 
     * @param listener
     */
    public void addListener(final ListenerType t, final boolean weak) {
        if (t == null) { return; }
        synchronized (this.LOCK) {
            boolean added = false;
            if (weak == false) {
                /* update strong listeners */
                final java.util.List<ListenerType> newStrongListener = new ArrayList<ListenerType>(this.strongListeners);
                if (!newStrongListener.contains(t)) {
                    newStrongListener.add(t);
                }
                this.strongListeners = newStrongListener;
            }
            /* update weak listeners */
            ListenerType l = null;
            final java.util.List<WeakReference<ListenerType>> newWeakListener = new ArrayList<WeakReference<ListenerType>>(this.weakListener.size());
            for (final WeakReference<ListenerType> listener : this.weakListener) {
                if ((l = listener.get()) == null) {
                    /* remove weak listener because it is gone */
                } else if (l == t) {
                    /* list already contains t, no need to add it again */
                    added = true;
                    newWeakListener.add(listener);
                } else {
                    newWeakListener.add(listener);
                }
            }
            if (added == false) {
                newWeakListener.add(new WeakReference<ListenerType>(t));
            }
            this.weakListener = newWeakListener;
        }
    }

    public void cleanup() {
        synchronized (this.LOCK) {
            final java.util.List<WeakReference<ListenerType>> newWeakListener = new ArrayList<WeakReference<ListenerType>>(this.weakListener.size());
            for (final WeakReference<ListenerType> listener : this.weakListener) {
                if (listener.get() == null) {
                    /* weak item is gone */
                    continue;
                } else {
                    newWeakListener.add(listener);
                }
            }
            this.weakListener = newWeakListener;
        }
    }

    public boolean containsListener(final ListenerType t) {
        if (t == null) { return false; }
        synchronized (this.LOCK) {
            for (final ListenerType tmp : this.strongListeners) {
                if (tmp == t) { return true; }
            }
            ListenerType l = null;
            for (final WeakReference<ListenerType> listener : this.weakListener) {
                if ((l = listener.get()) == null) {
                    /* weak item is gone */
                    continue;
                } else if (l == t) { return true; }
            }
            return false;
        }
    }

    final public void fireEvent(final EventType event) {
        if (event == null) { return; }
        ListenerType t = null;
        boolean cleanup = false;
        final java.util.List<WeakReference<ListenerType>> listeners = this.weakListener;
        for (final WeakReference<ListenerType> listener : listeners) {
            t = listener.get();
            if (t == null) {
                cleanup = true;
                continue;
            }
            this.fireEvent(t, event);
        }
        if (cleanup && listeners.size() > 0) {
            this.cleanup();
        }
    }

    /**
     * Fires an Event to all registered Listeners
     * 
     * @param event
     * @return
     */
    final public void fireEvent(final int id, final Object... parameters) {
        ListenerType t = null;
        boolean cleanup = false;
        final java.util.List<WeakReference<ListenerType>> listeners = this.weakListener;
        for (final WeakReference<ListenerType> listener : listeners) {
            t = listener.get();
            if (t == null) {
                cleanup = true;
                continue;
            }
            this.fireEvent(t, id, parameters);
        }
        if (cleanup && listeners.size() > 0) {
            this.cleanup();
        }
    }

    /**
     * Abstract fire Event Method.
     * 
     * @param listener
     * @param event
     */
    protected abstract void fireEvent(ListenerType listener, EventType event);

    /**
     * 
     * @param t
     * @param id
     * @param parameters
     */
    protected void fireEvent(final ListenerType listener, final int id, final Object... parameters) {
        throw new RuntimeException("Not implemented. Overwrite org.appwork.utils.event.Eventsender.fireEvent(T, int, Object...) to use this");

    }

    public java.util.List<ListenerType> getListener() {
        final java.util.List<WeakReference<ListenerType>> listeners = this.weakListener;
        boolean cleanup = true;
        final java.util.List<ListenerType> ret = new ArrayList<ListenerType>(listeners.size());
        ListenerType t = null;
        for (final WeakReference<ListenerType> listener : listeners) {
            t = listener.get();
            if (t != null) {
                ret.add(t);
            } else {
                cleanup = true;
            }
        }
        if (cleanup && listeners.size() > 0) {
            this.cleanup();
        }
        return ret;
    }

    public boolean hasListener() {
        final java.util.List<WeakReference<ListenerType>> listeners = this.weakListener;
        for (final WeakReference<ListenerType> listener : listeners) {
            if (listener.get() != null) { return true; }
        }
        return false;
    }

    public void removeListener(final ListenerType t) {
        if (t == null) { return; }
        synchronized (this.LOCK) {
            ListenerType l = null;
            final java.util.List<WeakReference<ListenerType>> newWeakListener = new ArrayList<WeakReference<ListenerType>>(this.weakListener.size());
            final java.util.List<ListenerType> newStrongListener = new ArrayList<ListenerType>(this.strongListeners);
            for (final WeakReference<ListenerType> listener : this.weakListener) {
                if ((l = listener.get()) == null) {
                    /* weak item is gone */
                    continue;
                } else if (l != t) {
                    newWeakListener.add(listener);
                }
            }
            /* remove strong item */
            newStrongListener.remove(t);
            this.weakListener = newWeakListener;
            this.strongListeners = newStrongListener;
        }
    }
}
