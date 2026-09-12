/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.utils.os.windows.execute;

/**
 * Thrown when no usable interactive owner WTS session can be determined (no Explorer consensus, no {@code WTSActive} desktop session, no
 * usable console). Typical for session-0 callers such as LocalSystem silent installers; those may soft-skip so the product can start later
 * via user autostart/logon.
 */
public class NoInteractiveOwnerSessionException extends IllegalStateException {
    private static final long serialVersionUID = 1L;

    public NoInteractiveOwnerSessionException(final String message) {
        super(message);
    }
}
