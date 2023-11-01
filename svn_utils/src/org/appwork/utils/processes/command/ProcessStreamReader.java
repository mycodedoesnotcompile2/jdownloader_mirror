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
import java.io.OutputStream;
import java.util.concurrent.atomic.AtomicBoolean;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.Time;
import org.appwork.utils.processes.ProcessBuilderFactory;

/**
 * @author Thomas
 * @date 17.10.2018
 *
 */
public class ProcessStreamReader extends Thread implements AsyncInputStreamHandler {

    protected final Process process;

    public Process getProcess() {
        return process;
    }

    protected final InputStream is;

    public InputStream getInputStream() {
        return is;
    }

    public OutputStream getOutputStream() {
        return os;
    }

    protected final OutputStream  os;
    protected final AtomicBoolean processExitedFlag  = new AtomicBoolean(false);
    protected volatile long       processReadCurrent = 0;

    public ProcessStreamReader(String name, final Process process, final InputStream input, OutputStream output) {
        super(name);
        this.process = process;
        this.is = input;
        this.os = output;
        this.setDaemon(true);
        new Thread("ProcessStreamReaderWaitFor:" + process) {
            {
                setDaemon(true);
            }

            public void run() {
                try {
                    final int exitCode = getProcess().waitFor();
                    onExit(exitCode);
                } catch (InterruptedException e) {
                }
            };

        }.start();
    }

    protected int getReadBufferSize() {
        return 8 * 1024;
    }

    protected boolean isProcessAlive() {
        if (processExitedFlag.get()) {
            return false;
        } else {
            try {
                getProcess().exitValue();
                notifyProcessExited();
                return false;
            } catch (IllegalThreadStateException e) {
                return true;
            }
        }
    }

    @Override
    public void run() {
        Exception thrown = null;
        try {
            final byte[] buf = new byte[getReadBufferSize()];
            while (true) {
                final int available;
                try {
                    if ((available = is.available()) <= 0 && !processExitedFlag.get()) {
                        synchronized (processExitedFlag) {
                            processExitedFlag.wait(10);
                        }
                        continue;
                    }
                } catch (IOException e) {
                    if (isProcessAlive()) {
                        LogV3.logger(ProcessBuilderFactory.class).log(e);
                        thrown = e;
                    }
                    break;
                } catch (InterruptedException e) {
                    thrown = e;
                    break;
                }
                try {
                    final int read;
                    if (available <= 0 && processExitedFlag.get()) {
                        read = -1;
                    } else {
                        read = is.read(buf, 0, Math.min(buf.length, available));
                    }
                    if (read <= 0 && processExitedFlag.get()) {
                        break;
                    } else {
                        processReadCurrent += read;
                        try {
                            os.write(buf, 0, read);
                        } catch (IOException e) {
                            if (!ignoreOutputStreamException(e)) {
                                throw e;
                            }
                        }
                    }
                } catch (IOException e) {
                    if (!processExitedFlag.get()) {
                        LogV3.logger(ProcessBuilderFactory.class).log(e);
                    }
                    thrown = e;
                    break;
                }
            }
        } finally {
            if (killProcessInFinally(thrown)) {
                try {
                    process.exitValue();
                } catch (IllegalThreadStateException e2) {
                    process.destroy();
                }
            }
        }
    }

    protected boolean ignoreOutputStreamException(IOException e) {
        return false;
    }

    protected boolean killProcessInFinally(Exception e) {
        return true;
    }

    @Override
    public void interrupt() {
        try {
            try {
                is.close();
            } catch (IOException ignore) {
            }
        } finally {
            super.interrupt();
        }
    }

    public void notifyProcessExited() {
        if (processExitedFlag.compareAndSet(false, true)) {
            synchronized (processExitedFlag) {
                processExitedFlag.notifyAll();
            }
        }
    }

    protected long getProcessRead() {
        return processReadCurrent;
    }

    protected int getMaxWaitFor() {
        return 10000;
    }

    public void waitFor() throws InterruptedException {
        notifyProcessExited();
        long processReadLast = getProcessRead();
        long timeStamp = Time.systemIndependentCurrentJVMTimeMillis();
        try {
            while (isAlive()) {
                final long now = getProcessRead();
                if (processReadLast == now) {
                    if (Time.systemIndependentCurrentJVMTimeMillis() - timeStamp > getMaxWaitFor()) {
                        break;
                    }
                } else {
                    processReadLast = now;
                    timeStamp = Time.systemIndependentCurrentJVMTimeMillis();
                }
                Thread.sleep(50);
            }
        } finally {
            interrupt();
        }
    }

    @Override
    public void onExit(int exitCode) {
        notifyProcessExited();
    }
}