/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils;

import java.io.File;
import java.io.IOException;

import org.appwork.loggingv3.LogV3;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.storage.flexijson.mapper.typemapper.DateMapper;
import org.appwork.utils.IO.SYNC;

/**
 * @author thomas
 * @date 29.07.2024
 *
 */
public class IncreasingNonce {
    private final File   file;
    private long         value;
    private long         lastSavedValue;
    private final Thread saver;
    private boolean      inShutdown;

    /**
     * @param resource
     */
    public IncreasingNonce(File file) {
        this.file = file;
        saver = new Thread("IncreasingNonce " + file) {
            {
                setDaemon(true);
            }

            /**
             * @see java.lang.Thread#run()
             */
            @Override
            public void run() {
                try {
                    while (true) {
                        Thread.sleep(getSaveDelay());
                        save(false);
                    }
                } catch (InterruptedException e) {
                    return;
                }
            }
        };
        final long now = getTimestamp();
        try {
            boolean savedOnShutdown = false;
            // this.offset = 0l;
            this.value = now;
            if (this.file.isFile()) {
                final String content = IO.readFileToString(this.file);
                final String[] parts = content.split(";");
                if (parts != null && "v2".equals(parts[0]) && parts.length == 3) {
                    this.value = Long.parseLong(parts[1]);
                    savedOnShutdown = "true".equals(parts[2]);
                }
                if (!savedOnShutdown) {
                    // crash - there might be more events - up to ~ EVENT_COUNTER_SAVE_MAX_DELAY
                    // 5000 extra ms to take into account that saving and sync in the delayer might take a while
                    this.value += getSaveDelay() + 5000;
                }
            }
        } catch (final Throwable e) {
            this.value = now;
            onException(e);
        }
        ShutdownController.getInstance().addShutdownEvent(new ShutdownEvent() {
            {
                this.setHookPriority(Integer.MIN_VALUE + 100);
            }

            @Override
            public void onShutdown(final ShutdownRequest shutdownRequest) {
                save(true);
                saver.interrupt();
            }
        });
        // to ensure the onShutdown flag is set to false;
        saver.start();
        save(false);
    }

    protected long getTimestamp() {
        return Time.now();
    }

    /**
     * @param e
     */
    private void onException(Throwable e) {
        LogV3.exception(this, e);
        DebugMode.debugger();
    }

    /**
     * @return
     */
    private long getSaveDelay() {
        return 5 * 60000l;
    }

    private void save(boolean onShutDownSave) {
        synchronized (this) {
            if (lastSavedValue == value || inShutdown != onShutDownSave) {
                return;
            }
            this.inShutdown |= onShutDownSave;
            try {
                IO.secureWrite(this.file, "v2;" + this.value + ";" + onShutDownSave + ";", SYNC.META_AND_DATA);
                this.lastSavedValue = this.value;
                LogV3.info("Saved EventCounter onShutdown: " + inShutdown + ": " + DateMapper.formatJsonDefault(this.value));
            } catch (final Exception e) {
                onException(e);
            }
        }
    }

    public long incAndGet() {
        synchronized (this) {
            this.value++;
            long now = getTimestamp();
            if (now > value) {
                this.value = now;
            }
            final long ret = this.value;
            LogV3.info("EventCount: " + ret + "(" + DateMapper.formatJsonDefault(ret) + ")" + " (Offset " + (now - ret) + ")");
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                try {
                    final File file = Application.getResource("tmp/eventCountDebug");
                    if (file.isFile()) {
                        final long last = Long.parseLong(IO.readFileToString(file));
                        final long dif = ret - last;
                        DebugMode.breakIf(last >= ret);
                    }
                    IO.secureWrite(file, String.valueOf(ret), SYNC.META_AND_DATA);
                } catch (final IOException e) {
                    LogV3.exception(this, e);
                }
            }
            if (value - lastSavedValue >= getSaveDelay() || inShutdown) {
                // sync save required to ensure that we do not miss anything.
                // this can actually only happen, if there is a "jump" in getTimestamp - like changing system time, or external server sync
                save(inShutdown);
            }
            return ret;
        }
    }
}
