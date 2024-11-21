package org.appwork.jna.windows.wmi;

public class WMIQueryFailedException extends Exception {
    public WMIQueryFailedException(final Throwable e) {
        super(e);
    }
}
