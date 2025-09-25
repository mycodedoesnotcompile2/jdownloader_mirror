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
     * @param string
     */
    public WMIException(Exception e, String string) {
        super(string, e);
    }

    /**
     * @param e
     * @param string
     * @return
     */
    public static WMIException wrap(final Exception e, String string) {
        if (e instanceof WMIException) {
            return (WMIException) e;
        }
        return new WMIException(e, string);
    }
}
