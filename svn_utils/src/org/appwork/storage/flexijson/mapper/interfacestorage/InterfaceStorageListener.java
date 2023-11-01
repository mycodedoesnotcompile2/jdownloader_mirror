package org.appwork.storage.flexijson.mapper.interfacestorage;

import java.util.EventListener;

public interface InterfaceStorageListener<InterfaceType> extends EventListener {

    /**
     * @param interfaceInvocationHandler
     * @param key
     * @param old
     * @param object
     */
    void onInterfaceValueSet(InterfaceType storage, String key, Object oldValue, Object newValue);

}