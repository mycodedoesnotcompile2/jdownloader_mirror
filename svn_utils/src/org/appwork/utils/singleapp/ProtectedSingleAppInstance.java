package org.appwork.utils.singleapp;

import java.io.File;
import java.io.IOException;
import java.net.Socket;

import org.appwork.utils.net.httpserver.DefaultSocketAddressValidator;
import org.appwork.utils.net.httpserver.SocketAddressValidator;

/**
 * {@link SingleAppInstance} that accepts only connections from trusted IPC peers: same Windows user and (when this
 * process is elevated) only from elevated clients. Uses a {@link SocketAddressValidator} (default
 * {@link DefaultSocketAddressValidator#get()}) to reject connections from other users or non-elevated processes.
 */
public class ProtectedSingleAppInstance extends SingleAppInstance {

    private volatile SocketAddressValidator socketAddressValidator = DefaultSocketAddressValidator.get();

    public ProtectedSingleAppInstance(final String appID, final File directory, final IncommingMessageListener listenr) {
        super(appID, directory, listenr);
    }

    @Override
    protected boolean isIncomingSocketAllowed(final Socket socket) {
        final SocketAddressValidator validator = this.getSocketAddressValidator();
        if (validator == null) {
            return true;
        }
        try {
            return validator.isAllowed(socket, this);
        } catch (final IOException e) {
            return false;
        }
    }

    /**
     * @return the socket address validator used for IPC trust checks; null means all sockets are allowed
     */
    public SocketAddressValidator getSocketAddressValidator() {
        return socketAddressValidator;
    }

    /**
     * @param socketAddressValidator
     *            the validator to use; null means all sockets are allowed
     */
    public void setSocketAddressValidator(final SocketAddressValidator socketAddressValidator) {
        this.socketAddressValidator = socketAddressValidator;
    }
}
