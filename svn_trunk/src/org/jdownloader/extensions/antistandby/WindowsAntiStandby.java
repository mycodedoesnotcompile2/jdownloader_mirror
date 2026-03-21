//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package org.jdownloader.extensions.antistandby;

import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.utils.logging2.LogSource;
import org.jdownloader.logging.LogController;

import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.WinBase;

public class WindowsAntiStandby extends Thread implements Runnable {
    private static final int           sleep    = 5000;
    private final AntiStandbyExtension jdAntiStandby;
    private final AtomicInteger        lastFlag = new AtomicInteger(0);

    public WindowsAntiStandby(final AntiStandbyExtension jdAntiStandby) {
        super();
        this.jdAntiStandby = jdAntiStandby;
        this.setDaemon(true);
        setName("WindowsAntiStandby");
    }

    @Override
    public void run() {
        final LogSource logger = LogController.CL(WindowsAntiStandby.class);
        try {
            while (jdAntiStandby.isAntiStandbyThread()) {
                enableAntiStandby(logger, jdAntiStandby.requiresAntiStandby());
                sleep(sleep);
            }
        } catch (Throwable e) {
            logger.log(e);
        } finally {
            try {
                enableAntiStandby(logger, false);
            } catch (final Throwable e) {
            } finally {
                logger.fine("JDAntiStandby: Terminated");
                logger.close();
            }
        }
    }

    private void enableAntiStandby(final LogSource logger, final boolean enabled) {
        // https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setthreadexecutionstate
        int flags = WinBase.ES_CONTINUOUS;
        if (enabled) {
            flags = flags | WinBase.ES_SYSTEM_REQUIRED;
            if (jdAntiStandby.getSettings().isDisplayRequired()) {
                flags = flags | WinBase.ES_DISPLAY_REQUIRED;
            }
        }
        if (lastFlag.getAndSet(flags) != flags) {
            Kernel32.INSTANCE.SetThreadExecutionState(flags);
            logger.fine("JDAntiStandby: new flags=" + flags);
        }
    }
}
