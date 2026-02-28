package org.jdownloader.extensions.eventscripter.sandboxobjects;

import java.util.concurrent.atomic.AtomicReference;

import org.appwork.utils.swing.EDTHelper;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.gui.notify.BasicNotify;

public class NotifyWindowSandbox {

    protected final AtomicReference<BasicNotify> ref;

    public NotifyWindowSandbox(final AtomicReference<BasicNotify> ref) {
        this.ref = ref;
    }

    public boolean isClosed() {
        final BasicNotify notify = getBasicNotify();
        if (notify == null) {
            return true;
        }
        return notify.isClosed();
    }

    private BasicNotify getBasicNotify() {
        if (ref == null) {
            return null;
        } else {
            return ref.get();
        }
    }

    public boolean isAdded() {
        return ref != null;
    }

    public boolean isShown() {
        return getBasicNotify() != null;
    }

    public boolean isVisible() {
        final BasicNotify notify = getBasicNotify();
        if (notify == null) {
            return false;
        }
        return Boolean.TRUE.equals(new EDTHelper<Boolean>() {

            @Override
            public Boolean edtRun() {
                return notify.isVisible();
            }
        }.getReturnValue());
    }

    public void close() {
        final BasicNotify notify = getBasicNotify();
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
