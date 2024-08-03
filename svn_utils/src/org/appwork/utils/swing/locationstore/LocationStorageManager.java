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
package org.appwork.utils.swing.locationstore;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Point;
import java.awt.Window;
import java.io.File;
import java.io.IOException;

import org.appwork.loggingv3.LogV3;
import org.appwork.scheduler.DelayedRunnable;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.StorageException;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.swing.EDTHelper;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.windowmanager.WindowManager;
import org.appwork.utils.swing.windowmanager.WindowManager.WindowExtendedState;

/**
 * This class is meant to be used in EDT and thus os NOT Threadsafe in any way
 *
 * @date 22.02.2021
 *
 */
public class LocationStorageManager extends ShutdownEvent {
    protected LocationStorageContainer data;
    protected boolean                  write      = false;
    protected final DelayedRunnable    saver;
    protected final Object             fileLock   = new Object();
    protected volatile Dimension       difference = null;

    /**
     *
     */
    private LocationStorageManager() {
        if (System.getProperty("LocationStorageManager.safe-delayer") != null) {
            saver = new DelayedRunnable(5000, 30000) {
                @Override
                public void delayedrun() {
                    save(false);
                }
            };
        } else {
            saver = null;
        }
        ShutdownController.getInstance().addShutdownEvent(this);
    }

    protected File getStorageFile() {
        return Application.getResource("cfg/locations.json");
    }

    public static final LocationStorageManager INSTANCE = new LocationStorageManager();

    /**
     * @param storageID
     * @return
     */
    public LocationStorage get(final Window window, final String storageID) {
        ensureStorage();
        LocationStorage ret = data.getData().get(storageID);
        if (ret == null) {
            ret = new LocationStorage();
            data.getData().put(storageID, ret);
        }
        ret.setLastAccess(System.currentTimeMillis());
        onUpdate(ret);
        return ret;
    }

    public boolean reset(final String storageID) {
        ensureStorage();
        final boolean reset;
        if (data.getData().size() > 0) {
            if (storageID == null) {
                data.getData().clear();
                reset = true;
            } else {
                reset = data.getData().remove(storageID) != null;
            }
        } else {
            reset = false;
        }
        if (reset) {
            onUpdate(null);
        }
        return reset;
    }

    public boolean reset() {
        return reset(null);
    }

    /**
     *
     */
    private void ensureStorage() {
        if (data == null) {
            final File file = getStorageFile();
            if (file.isFile()) {
                try {
                    data = JSonStorage.restoreFromString(IO.readFileToString(file), LocationStorageContainer.TYPE);
                } catch (Throwable e) {
                    LogV3.log(e);
                }
            }
            if (data == null) {
                data = new LocationStorageContainer();
            }
        }
    }

    /**
     * @param LocationStorage
     */
    public void save(final boolean onShutDown) {
        try {
            if (!write) {
                return;
            } else {
                final byte[] json = new EDTHelper<byte[]>() {
                    @Override
                    public byte[] edtRun() {
                        try {
                            // serialization within edt to avoid concurrent modification of data object
                            return JSonStorage.serializeToJsonByteArray(data);
                        } catch (StorageException e) {
                            LogV3.log(e);
                            return null;
                        }
                    }
                }.getReturnValue();
                if (json != null) {
                    synchronized (fileLock) {
                        IO.secureWrite(getStorageFile(), json, onShutDown ? SYNC.META_AND_DATA : SYNC.NONE);
                    }
                    write = false;
                }
            }
        } catch (IOException e) {
            LogV3.log(e);
        }
    }

    public void onUpdate(final Window window, final Point locationOnScreen, final Dimension windowSize, final LocationStorage cfg, final String type) {
        if (cfg != null) {
            new EDTRunner(false) {
                @Override
                protected void runInEDT() {
                    boolean write = false;
                    if (locationOnScreen != null) {
                        write |= cfg.update(true, type, locationOnScreen.x, locationOnScreen.y);
                    } else if (windowSize != null) {
                        write |= cfg.update(true, type, windowSize.width, windowSize.height);
                    } else if (window != null) {
                        write |= cfg.update(true, type, window.getWidth(), window.getHeight());
                    }
                    if (window != null && window instanceof Frame) {
                        WindowExtendedState newState = WindowManager.getInstance().getExtendedState(((Frame) window));
                        WindowExtendedState is = cfg.getExtendedState();
                        if (newState != is) {
                            cfg.setExtendedState(newState);
                            write = true;
                        }
                    }                  
                    if (write) {
                        onUpdate(cfg);
                    }
                    if (write && saver != null) {
                        saver.resetAndStart();
                    }
                }
            };
        }
    }

    /**
     * is called when a write should be done
     *
     * @param cfg
     */
    protected void onUpdate(LocationStorage cfg) {
        write = true;
        if (saver != null) {
            saver.resetAndStart();
        }
    }

    @Override
    public void onShutdown(ShutdownRequest shutdownRequest) {
        save(true);
    }
}
