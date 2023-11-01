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
package org.appwork.utils.processes.command;

import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * @author daniel
 * @date Aug 2, 2019
 *
 */
public abstract class ProcessStream extends InputStream {
    protected final Process process;

    public Process getProcess() {
        return process;
    }

    protected final AtomicBoolean processAliveFlag = new AtomicBoolean(true);
    protected final InputStream   is;

    protected ProcessStream(final Process process, InputStream is) {
        this.process = process;
        this.is = is;
        new Thread(getClass().getName() + "WaitForProcess:" + process) {
            {
                setDaemon(true);
            }

            public void run() {
                try {
                    notifyProcessTerminated(getProcess().waitFor());
                } catch (InterruptedException e) {
                }

            };
        }.start();
    }

    protected void notifyProcessTerminated(int exitCode) {
        if (processAliveFlag.compareAndSet(true, false)) {
            synchronized (processAliveFlag) {
                processAliveFlag.notifyAll();
            }
        }
    }

    @Override
    public int read() throws IOException {
        final byte[] ret = new byte[1];
        while (true) {
            final int read = read(ret, 0, 1);
            if (read == -1) {
                return -1;
            } else if (read == 1) {
                return ret[0] & 255;
            } else if (read == 0) {
                try {
                    synchronized (processAliveFlag) {
                        if (processAliveFlag.get()) {
                            processAliveFlag.wait(10);
                        }
                    }
                } catch (InterruptedException e) {
                    throw new IOException(e);
                }
            }
        }
    }

    public boolean isProcessAlive() {
        if (!processAliveFlag.get()) {
            return false;
        } else {
            try {
                notifyProcessTerminated(getProcess().exitValue());
                return false;
            } catch (IllegalThreadStateException e) {
                return true;
            }
        }
    }

    @Override
    public int available() throws IOException {
        final int ret = is.available();
        if (ret > 0) {
            return ret;
        } else if (!isProcessAlive()) {
            throw new IOException("process is terminated");
        } else {
            return ret;
        }
    }

    protected final AtomicBoolean closedFlag = new AtomicBoolean(false);

    @Override
    public void close() throws IOException {
        if (closedFlag.compareAndSet(false, true)) {
            final Thread thread = new Thread(getClass().getName() + "WaitForClose:") {
                {
                    setDaemon(true);
                }

                @Override
                public void run() {
                    try {
                        is.close();
                    } catch (IOException ignore) {
                    }
                }
            };
            thread.start();
            try {
                thread.join(1000);
            } catch (InterruptedException ignore) {
            }
        }
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException {
        final int next;
        try {
            next = available();
        } catch (final IOException e) {
            if (isProcessAlive()) {
                throw e;
            } else {
                return -1;
            }
        }
        if (next <= 0) {
            if (isProcessAlive()) {
                return 0;
            } else {
                return -1;
            }
        } else {
            return is.read(b, off, Math.min(next, len));
        }
    }

}
