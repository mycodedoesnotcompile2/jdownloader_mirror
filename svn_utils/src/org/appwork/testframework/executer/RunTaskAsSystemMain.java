/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 * ====================================================================================================================================================
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.executer;

/**
 * Backward-compatible entrypoint alias. Prefer {@link RunSerializedTaskMain}.
 */
public final class RunTaskAsSystemMain {
    public static void main(String[] args) {
        RunSerializedTaskMain.main(args);
    }
}
