package org.appwork.jna.windows;

import com.sun.jna.win32.StdCallLibrary.StdCallCallback;

public interface HANDLER_ROUTINE extends StdCallCallback {
    public static final int CTRL_CLOSE_EVENT    = 2;
    public static final int CTRL_LOGOFF_EVENT   = 5;
    public static final int CTRL_SHUTDOWN_EVENT = 6;

    long callback(long dwCtrlType);
}
