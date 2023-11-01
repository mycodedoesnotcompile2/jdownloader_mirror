package jd.http;

import java.io.IOException;

public class NoGateWayException extends IOException {
    private final ProxySelectorInterface selector;

    public ProxySelectorInterface getSelector() {
        return this.selector;
    }

    public NoGateWayException(ProxySelectorInterface selector, String string) {
        super(string);
        this.selector = selector;
    }

    public NoGateWayException(ProxySelectorInterface selector, String string, Throwable e) {
        super(string, e);
        this.selector = selector;
    }

    public NoGateWayException(ProxySelectorInterface selector, Throwable e) {
        super(e);
        this.selector = selector;
    }
}
