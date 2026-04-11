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

import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.utils.logging2.LogSource;
import org.jdownloader.logging.LogController;

import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.WinBase;

public class WindowsAntiStandby extends Thread implements Runnable {
    private static final int           sleep    = 1000;
    private final AntiStandbyExtension jdAntiStandby;
    private final AtomicInteger        lastFlag = new AtomicInteger(0);
    private volatile Set<Condition>    conditions;

    public Set<Condition> getConditions() {
        if (isAlive()) {
            return conditions;
        }
        return null;
    }

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
                enableAntiStandby(logger, null);
            } catch (final Throwable e) {
            } finally {
                logger.fine("JDAntiStandby: Terminated");
                logger.close();
            }
        }
    }

    private void enableAntiStandby(final LogSource logger, final Set<Condition> conditions) {
        this.conditions = conditions;
        // https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setthreadexecutionstate
        int flags = WinBase.ES_CONTINUOUS;
        if (conditions != null && conditions.size() > 0) {
            flags = flags | WinBase.ES_SYSTEM_REQUIRED;
            if (jdAntiStandby.getSettings().isDisplayRequired()) {
                flags = flags | WinBase.ES_DISPLAY_REQUIRED;
            }
        }
        /**
         * One from Microsoft, 'This is by design. ES_CONTINUOUS is implemented as power requests. Prior to Windows 11 power requests were
         * held an additional 2 minutes after they were dropped by the application. This caused issue as some applications would
         * periodically take a power request for a short period keep systems out of sleep indefinitely. This behavior was removed in Windows
         * 11 and now systems are eligible to sleep as soon as the last power request is dropped.'
         */
        Kernel32.INSTANCE.SetThreadExecutionState(flags);
        if (lastFlag.getAndSet(flags) != flags) {
            logger.fine("JDAntiStandby: new flags=" + flags);
        }
    }
}
