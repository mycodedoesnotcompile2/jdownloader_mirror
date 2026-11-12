package jd.gui.swing.jdgui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.WeakHashMap;

import javax.swing.SwingUtilities;
import javax.swing.Timer;

import org.appwork.exceptions.WTFException;

public class FlashController implements ActionListener {
    private final WeakHashMap<Flashable, Object> flashableMap = new WeakHashMap<Flashable, Object>();
    private Timer                                timer;
    private long                                 counter      = 0l;

    public FlashController() {
    }

    public void register(Flashable button) {
        if (!SwingUtilities.isEventDispatchThread()) {
            throw new WTFException("This has to be done in the EDT");
        }
        synchronized (flashableMap) {
            if (flashableMap.containsKey(button)) {
                return;
            }
            flashableMap.put(button, this);
        }
        button.onFlashRegister(counter);
        if (timer == null) {
            timer = new Timer(1000, this);
            timer.setRepeats(true);
            timer.setInitialDelay(1000);
            timer.start();
        }
    }

    public void unregister(Flashable button) {
        if (!SwingUtilities.isEventDispatchThread()) {
            throw new WTFException("This has to be done in the EDT");
        }
        final boolean isEmpty;
        synchronized (flashableMap) {
            if (flashableMap.remove(button) == null) {
                return;
            }
            isEmpty = flashableMap.isEmpty();
        }
        if (isEmpty && timer != null) {
            timer.stop();
            timer = null;
        }
        button.onFlashUnRegister(counter);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        counter++;
        final List<Flashable> toFlash = new ArrayList<Flashable>();
        synchronized (flashableMap) {
            toFlash.addAll(flashableMap.keySet());
        }
        final Iterator<Flashable> it = toFlash.iterator();
        while (it.hasNext()) {
            final Flashable cur = it.next();
            if (cur.onFlash(counter)) {
                it.remove();
            }
        }
        synchronized (flashableMap) {
            flashableMap.keySet().removeAll(toFlash);
            if (!flashableMap.isEmpty()) {
                return;
            }
        }
        if (timer != null) {
            timer.stop();
            timer = null;
        }
    }

    public boolean isRegistered(Flashable button) {
        synchronized (flashableMap) {
            return button != null && flashableMap.containsKey(button);
        }
    }
}
