package org.jdownloader.myjdownloader.client.bindings.interfaces;

import org.jdownloader.myjdownloader.client.bindings.ClientApiNameSpace;

@ClientApiNameSpace(ReconnectInterface.NAMESPACE)
public interface ReconnectInterface {
    public static final String NAMESPACE = "reconnect";

    public void doReconnect();
}
