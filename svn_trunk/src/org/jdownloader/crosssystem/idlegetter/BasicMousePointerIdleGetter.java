package org.jdownloader.crosssystem.idlegetter;

import java.awt.AWTEvent;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.AWTEventListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Timer;

import org.appwork.swing.components.tooltips.ToolTipController;
import org.appwork.utils.Time;

public class BasicMousePointerIdleGetter extends IdleGetter implements AWTEventListener, ActionListener {
    private volatile Point lastLocation = null;
    private volatile long  lastChange   = System.currentTimeMillis();

    public BasicMousePointerIdleGetter() {
        Toolkit.getDefaultToolkit().addAWTEventListener(this, AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_MOTION_EVENT_MASK | AWTEvent.KEY_EVENT_MASK);
        final Timer timer = new Timer(5000, this);
        timer.setRepeats(true);
        timer.start();
    }

    @Override
    public long getIdleTimeSinceLastUserInput() {
        return Time.systemIndependentCurrentJVMTimeMillis() - lastChange;
    }

    @Override
    public void eventDispatched(AWTEvent event) {
        lastChange = Time.systemIndependentCurrentJVMTimeMillis();

    }

    @Override
    public void actionPerformed(ActionEvent e) {
        final Point location = ToolTipController.getMouseLocation();
        if (lastLocation == null || !lastLocation.equals(location)) {
            lastChange = Time.systemIndependentCurrentJVMTimeMillis();
            lastLocation = location;
        }
    }

}
