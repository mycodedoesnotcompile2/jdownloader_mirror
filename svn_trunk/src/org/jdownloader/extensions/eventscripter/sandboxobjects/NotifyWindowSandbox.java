package org.jdownloader.extensions.eventscripter.sandboxobjects;

import java.util.concurrent.atomic.AtomicReference;

import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.gui.notify.BasicNotify;

public class NotifyWindowSandbox {

    protected final AtomicReference<BasicNotify> ref;

    public NotifyWindowSandbox(AtomicReference<BasicNotify> ref) {
        this.ref = ref;
    }

    public boolean isClosed() {
        final BasicNotify notify = ref.get();
        if (notify == null) {
            return true;
        }
        return notify.isClosed();
    }

    public boolean isShown() {
        return ref.get() != null;
    }

    public void close() {
        final BasicNotify notify = ref.get();
        if (notify == null) {
            return;
        }
        new EDTRunner() {

            @Override
            protected void runInEDT() {
                notify.setVisible(false);
            }
        };
    }
}
