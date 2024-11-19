package org.appwork.jna.wmi;

public class WMIQueryFailedException extends Exception {
    public WMIQueryFailedException(final Throwable e) {
        super(e);
    }
}
