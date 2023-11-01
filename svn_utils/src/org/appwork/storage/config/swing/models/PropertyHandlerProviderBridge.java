package org.appwork.storage.config.swing.models;

import java.lang.annotation.Annotation;
import java.lang.ref.WeakReference;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.WeakHashMap;

import org.appwork.storage.config.swing.ValueProvider;
import org.appwork.storage.config.swing.ValueProviderListener;
import org.appwork.storage.flexijson.mapper.interfacestorage.PropertyHandler;
import org.appwork.storage.flexijson.mapper.interfacestorage.PropertyHandlerImpl;
import org.appwork.storage.flexijson.mapper.interfacestorage.PropertyHandlerListener;

public class PropertyHandlerProviderBridge<T> implements ValueProvider<T> {
    private static class ListenerBridge<T> implements PropertyHandlerListener<Object, T> {
        public final WeakReference<ValueProviderListener<T>> listener;
        final private PropertyHandlerProviderBridge<T>       owner;
        private ValueProviderListener<T>                     strongRef;

        public ListenerBridge(PropertyHandlerProviderBridge<T> providerBridge, ValueProviderListener<T> listener2) {
            this.owner = providerBridge;
            this.listener = new WeakReference<ValueProviderListener<T>>(listener2);
        }

        public ValueProviderListener<T> getListener() {
            ValueProviderListener<T> sr = strongRef;
            return sr == null ? listener.get() : sr;
        }

        private void cleanup(PropertyHandlerImpl<Object, T> property) {
            property.getEventSender().removeListener(this);
            synchronized (org.appwork.storage.config.swing.models.PropertyHandlerProviderBridge.REFS_LINK) {
                org.appwork.storage.config.swing.models.PropertyHandlerProviderBridge.REFS_LINK.isEmpty();
            }
        }

        @Override
        public void onInterfaceValueSet(Object storage, PropertyHandlerImpl<Object, T> property, T oldValue, T newValue) {
            ValueProviderListener<T> actual = getListener();
            if (actual != null) {
                actual.onValueModified(owner, newValue);
            } else {
                cleanup(property);
            }
        }
    }

    public final PropertyHandler<Object, T>                                      keyHandler;
    private static WeakHashMap<ValueProviderListener<?>, Set<ListenerBridge<?>>> REFS_LINK = new WeakHashMap<ValueProviderListener<?>, Set<PropertyHandlerProviderBridge.ListenerBridge<?>>>();

    public PropertyHandlerProviderBridge(PropertyHandler<?, T> propertyHandler) {
        this.keyHandler = (PropertyHandler<Object, T>) propertyHandler;
    }

    @Override
    public void unregister(final ValueProviderListener<T> listener) {
        synchronized (keyHandler.getEventSender().LOCK) {
            ListenerBridge<T> newListener = searchListener(listener);
            if (newListener != null) {
                synchronized (REFS_LINK) {
                    Set<ListenerBridge<?>> set = REFS_LINK.get(listener);
                    if (set != null) {
                        set.remove(newListener);
                        if (set.size() == 0) {
                            REFS_LINK.remove(listener);
                        }
                    }
                }
                keyHandler.getEventSender().removeListener(newListener);
            }
        }
    }
    // /**
    // * detect if the listener is a componnent that looses its root window
    // *
    // * @param listener
    // */
    // private void linkWindowClosedListener(final ValueProviderListener<T> listener) {
    // if (listener instanceof JComponent) {
    // final WindowAdapter wl = new WindowAdapter() {
    // @Override
    // public void windowClosing(WindowEvent e) {
    // }
    //
    // /*
    // * (non-Javadoc)
    // *
    // * @see java.awt.event.WindowAdapter#windowClosed(java.awt.event.WindowEvent)
    // */
    // @Override
    // public void windowClosed(WindowEvent e) {
    // LogV3.info("Root of Component was closed. Unlink Dataprovider: " + listener);
    // unregister(listener);
    // }
    // };
    // ((JComponent) listener).addAncestorListener(new AncestorListener() {
    // private Window window;
    //
    // @Override
    // public void ancestorRemoved(AncestorEvent event) {
    // setWindow(SwingUtils.getWindowForComponent(event.getComponent()));
    // }
    //
    // private void setWindow(Window newRoot) {
    // if (window == newRoot) {
    // return;
    // }
    // if (window != null) {
    // // changed window? is this possible?
    // window.removeWindowListener(wl);
    // }
    // window = newRoot;
    // if (newRoot != null) {
    // newRoot.addWindowListener(wl);
    // }
    // }
    //
    // @Override
    // public void ancestorMoved(AncestorEvent event) {
    // setWindow(SwingUtils.getWindowForComponent(event.getComponent()));
    // }
    //
    // @Override
    // public void ancestorAdded(AncestorEvent event) {
    // setWindow(SwingUtils.getWindowForComponent(event.getComponent()));
    // }
    // });
    // }
    // }

    @Override
    public void register(final ValueProviderListener<T> listener, boolean weak) {
        synchronized (keyHandler.getEventSender().LOCK) {
            ListenerBridge<T> newListener = searchListener(listener);
            if (newListener == null) {
                newListener = new ListenerBridge<T>(this, listener);
                synchronized (REFS_LINK) {
                    Set<ListenerBridge<?>> set = REFS_LINK.get(listener);
                    if (set == null) {
                        set = new HashSet<PropertyHandlerProviderBridge.ListenerBridge<?>>();
                    }
                    set.add(newListener);
                    REFS_LINK.put(listener, set);
                }
            }
            if (weak) {
                newListener.strongRef = null;
            } else {
                newListener.strongRef = listener;
            }
            keyHandler.getEventSender().addListener(newListener, weak);
        }
    }

    public ListenerBridge<T> searchListener(final ValueProviderListener<T> listener) {
        synchronized (keyHandler.getEventSender().LOCK) {
            ListenerBridge<T> newListener = null;
            for (PropertyHandlerListener<?, T> l : keyHandler.getEventSender().getListener()) {
                if (l instanceof ListenerBridge && ((ListenerBridge<?>) l).listener == listener) {
                    newListener = (ListenerBridge<T>) l;
                    break;
                }
            }
            return newListener;
        }
    }

    @Override
    public T get() {
        return keyHandler.get();
    }

    @Override
    public void set(T value) {
        keyHandler.set(value);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.config.swing.ValueProvider#getAnnotations(java.lang.Class)
     */
    @Override
    public <AnnoType extends Annotation> List<AnnoType> getAnnotations(Class<AnnoType> type) {
        return keyHandler.getAnnotations(type);
    }
}
