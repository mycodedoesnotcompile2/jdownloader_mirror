package org.appwork.utils.net.httpconnection;

import java.io.IOException;

/**
 * Called by {@link org.appwork.utils.net.BasicHTTP.BasicHTTP} after a request failed, before the error is returned to the caller.
 * <p>
 * Not part of the TLS handshake. Return true to run the same connection attempt once more.
 */
public interface CertificateTrustRecovery {
    boolean recover(HTTPConnection connection, IOException error);
}
