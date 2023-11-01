package org.appwork.storage.config.swing.models;

import java.lang.annotation.Annotation;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.WeakHashMap;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.storage.config.swing.ValueProvider;
import org.appwork.storage.config.swing.ValueProviderListener;

public class KeyHandlerProviderBridge<T> implements ValueProvider<T> {
    private static class ListenerBridge<T> implements GenericConfigEventListener<T> {

        public final WeakReference<ValueProviderListener<T>> listener;
        final private KeyHandlerProviderBridge<T>            owner;
        private ValueProviderListener<T>                     strongRef;

        public ListenerBridge(KeyHandlerProviderBridge<T> providerBridge, ValueProviderListener<T> listener2) {
            this.owner = providerBridge;
            this.listener = new WeakReference<ValueProviderListener<T>>(listener2);

        }

        public ValueProviderListener<T> getListener() {
            ValueProviderListener<T> sr = strongRef;
            return sr == null ? listener.get() : sr;
        }

        @Override
        public void onConfigValidatorError(KeyHandler<T> keyHandler, T invalidValue, ValidationException validateException) {
            ValueProviderListener<T> actual = getListener();
            if (actual != null) {
                actual.onValueValidationError(owner, invalidValue, validateException);
            } else {
                cleanup(keyHandler);
            }
        }

        private void cleanup(KeyHandler<T> keyHandler) {
            keyHandler.getEventSender().removeListener(this);
            synchronized (org.appwork.storage.config.swing.models.KeyHandlerProviderBridge.REFS_LINK) {
                org.appwork.storage.config.swing.models.KeyHandlerProviderBridge.REFS_LINK.isEmpty();
            }

        }

        @Override
        public void onConfigValueModified(KeyHandler<T> keyHandler, T newValue) {
            ValueProviderListener<T> actual = getListener();
            if (actual != null) {
                actual.onValueModified(owner, newValue);
            } else {
                cleanup(keyHandler);
            }
        }
    }

    public final KeyHandler<T>                                                   keyHandler;

    private static WeakHashMap<ValueProviderListener<?>, Set<ListenerBridge<?>>> REFS_LINK = new WeakHashMap<ValueProviderListener<?>, Set<KeyHandlerProviderBridge.ListenerBridge<?>>>();

    public KeyHandlerProviderBridge(KeyHandler<T> keyHandler) {
        this.keyHandler = keyHandler;

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

    @Override
    public void register(final ValueProviderListener<T> listener, boolean weak) {
        synchronized (keyHandler.getEventSender().LOCK) {

            ListenerBridge<T> newListener = searchListener(listener);
            if (newListener == null) {
                newListener = new ListenerBridge<T>(this, listener);
                synchronized (REFS_LINK) {
                    Set<ListenerBridge<?>> set = REFS_LINK.get(listener);
                    if (set == null) {
                        set = new HashSet<KeyHandlerProviderBridge.ListenerBridge<?>>();
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
            for (GenericConfigEventListener<T> l : keyHandler.getEventSender().getListener()) {
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
        return keyHandler.getValue();
    }

    @Override
    public void set(T value) {
        keyHandler.setValue(value);
    }

    @Override
    public <AnnoType extends Annotation> List<AnnoType> getAnnotations(Class<AnnoType> type) {
        AnnoType anno = keyHandler.getAnnotation(type);
        if (anno == null) {
            return null;
        }
        ArrayList<AnnoType> ret = new ArrayList<AnnoType>();
        ret.add(anno);
        return ret;
    }
}
