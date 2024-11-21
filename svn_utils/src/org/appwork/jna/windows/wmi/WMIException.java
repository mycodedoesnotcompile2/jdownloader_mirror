/**
 *
 */
package org.appwork.jna.windows.wmi;

/**
 *
 */
public class WMIException extends Exception {
    /**
     * @param e
     */
    public WMIException(final Exception e) {
        super(e);
    }

    /**
     * @param string
     */
    public WMIException(String string) {
        super(string);
    }

    /**
     * @param e
     * @return
     */
    public static WMIException wrap(final Exception e) {
        if (e instanceof WMIException) {
            return (WMIException) e;
        }
        return new WMIException(e);
    }
}
