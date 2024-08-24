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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.singleapp;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.charset.Charset;
import java.security.InvalidParameterException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.loggingv3.LogV3;
import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.utils.Application;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Exceptions;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.HexFormatter;
import org.appwork.utils.net.ChunkedInputStream;
import org.appwork.utils.net.ChunkedOutputStream;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;

/**
 * @author daniel
 *
 */
public class SingleAppInstance {
    /**
     *
     */
    public static final String MISSING_DONE_RESPONSE_SERVER_SHUT_DOWN = "Missing DONE Response. Server shut down?";
    /**
    *
    */
    public static final String GO_AWAY_BYE                            = "INTERNAL_BYE";
    /**
    *
    */
    public static final String GO_AWAY_INVALID_ID                     = "INTERNAL_INVALID_ID";
    public static final String CLIENT_ID_OK                           = "INTERNAL_VALID_ID";
    /**
     *
     */
    public static final String DONE                                   = "INTERNAL_DONE";
    /**
     *
     */
    public static final String KEEP_ALIVE                             = "INTERNAL_KEEP_ALIVE";
    /**
     *
     */
    public static final String EXCEPTION                              = "INTERNAL_EXCEPTION";
    /**
     *
     */
    public static final String NEWLINE                                = "\r\n";

    public static class GoAwayException extends IOException {
        /**
         * @param message
         */
        public GoAwayException(String message) {
            super(message);
        }
    }

    /**
     * @author thomas
     * @date 26.03.2023
     *
     */
    public static class InvalidResponseID extends GoAwayException {
        /**
         * @param message
         */
        public InvalidResponseID(String message) {
            super(message);
        }
    }

    /**
     * @author thomas
     * @date 26.03.2023
     *
     */
    public static class NoPortFileException extends IOException {
        /**
         * @param message
         */
        public NoPortFileException(String message) {
            super(message);
        }

        public NoPortFileException(Throwable e) {
            super(e);
        }

        public NoPortFileException(String message, Throwable e) {
            super(message, e);
        }
    }

    private final String                      appID;
    private volatile IncommingMessageListener listener                                     = null;
    private final File                        lockFile;
    private FileLock                          fileLock                                     = null;
    private FileChannel                       lockChannel                                  = null;
    private boolean                           alreadyUsed                                  = false;
    private ServerSocket                      serverSocket                                 = null;
    protected final String                    singleApp                                    = "SingleAppInstance";
    private final AtomicReference<Thread>     daemon                                       = new AtomicReference<Thread>(null);
    private static final int                  DEFAULTPORT                                  = 9665;
    private boolean                           forwardMessageDirectIfNoOtherInstanceIsFound = true;
    private InetSocketAddress                 address;
    private int                               port                                         = -1;
    private ShutdownEvent                     shutdownEvent;
    private final static Charset              UTF8                                         = Charset.forName("UTF-8");

    public boolean isForwardMessageDirectIfNoOtherInstanceIsFound() {
        return forwardMessageDirectIfNoOtherInstanceIsFound;
    }

    public void setForwardMessageDirectIfNoOtherInstanceIsFound(boolean forwardMessageDirectIfNoOtherInstanceIsFound) {
        this.forwardMessageDirectIfNoOtherInstanceIsFound = forwardMessageDirectIfNoOtherInstanceIsFound;
    }

    public SingleAppInstance(final String appID, IncommingMessageListener listenr) {
        this(appID, new File(Application.getHome()), listenr);
    }

    public SingleAppInstance(final String appID, final File directory, IncommingMessageListener listenr) {
        this.appID = appID;
        directory.mkdirs();
        this.lockFile = new File(directory, appID + ".lock");
        this.listener = listenr;
    }

    public IncommingMessageListener getListener() {
        return listener;
    }

    public void setListener(IncommingMessageListener listener) {
        this.listener = listener;
    }

    private synchronized void cannotStart(Throwable cause) throws UncheckableInstanceException {
        closeLock();
        throw new UncheckableInstanceException(cause);
    }

    public synchronized Thread exit() {
        return exit(false);
    }

    public synchronized Thread exit(final boolean closeClientConnections) {
        try {
            if (this.fileLock == null) {
                if (closeClientConnections) {
                    closeAllConnections();
                }
                return null;
            } else {
                final Thread daemon = this.daemon.getAndSet(null);
                if (daemon != null) {
                    daemon.interrupt();
                }
                try {
                    closeServer();
                    closeLock();
                } finally {
                    deleteLockFile(1000);
                }
                if (closeClientConnections) {
                    closeAllConnections();
                }
                return daemon;
            }
        } finally {
            if (!ShutdownController.getInstance().isAlive()) {
                // do not remove if we are already in shutdown
                ShutdownController.getInstance().removeShutdownEvent(shutdownEvent);
            }
        }
    }

    protected boolean deleteLockFile(int maxWaitMs) {
        final File lockFile = this.lockFile;
        if (lockFile.exists() && !lockFile.delete()) {
            int waitLockFileDelete = maxWaitMs;
            while (lockFile.exists() && waitLockFileDelete > 0) {
                try {
                    Thread.sleep(10);
                    waitLockFileDelete -= 10;
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }
        return lockFile.exists();
    }

    public synchronized boolean isRunning() {
        return serverSocket != null && !serverSocket.isClosed();
    }

    private synchronized void closeServer() {
        if (serverSocket != null) {
            try {
                serverSocket.close();
            } catch (Throwable ignore) {
            }
        }
    }

    private synchronized void closeLock() {
        if (fileLock != null) {
            try {
                fileLock.release();
            } catch (Throwable ignore) {
            } finally {
                this.fileLock = null;
            }
        }
        if (lockChannel != null) {
            try {
                lockChannel.close();
            } catch (Throwable ignore) {
            } finally {
                this.lockChannel = null;
            }
        }
    }

    private synchronized void foundRunningInstanceAndFailedToConnect() {
        closeLock();
    }

    /**
     * TODO: update to support IPv4 and IPv6 at the same time
     *
     * @return
     */
    protected InetAddress getLocalHost() throws UnknownHostException {
        return HTTPConnectionUtils.getLoopback(IPVERSION.IPV4_IPV6)[0];
    }

    protected String readLine(final BufferedInputStream in) throws IOException, InterruptedException {
        if (in == null) {
            return "";
        } else {
            final ByteArrayOutputStream buf = new ByteArrayOutputStream() {
                public synchronized byte[] toByteArray() {
                    return this.buf;
                };
            };
            in.mark(1);
            if (in.read() == -1) {
                return null;
            } else {
                in.reset();
            }
            int read;
            while ((read = in.read()) >= 0) {
                if (Thread.interrupted()) {
                    throw new InterruptedException();
                }
                if (read == 0 || read == 10 || read == 13) {
                    break;
                } else {
                    buf.write(read);
                }
            }
            if (read == 13) {
                in.mark(1);
                if (in.read() != 10) {
                    in.reset();
                }
            }
            return unescapeLine(new String(buf.toByteArray(), 0, buf.size(), UTF8));
        }
    }

    protected int readPortFromPortFile() throws IOException {
        try {
            if (!lockFile.isFile()) {
                throw new NoPortFileException("No Port File");
            }
            int retry = 10;
            while (retry-- >= 0) {
                final String port;
                final FileInputStream fis = new FileInputStream(lockFile);
                try {
                    fis.getChannel().position(1);
                    final byte[] bytes = IO.readStream(-1, fis);
                    port = new String(bytes, UTF8);
                } finally {
                    fis.close();
                }
                // check for newline to make sure we read the complete port number
                if (port.matches("(?s)^\\s*\\d+\\s+.*")) {
                    final String firstPortLine = port.split("\r\n")[0];
                    return Integer.parseInt(String.valueOf(firstPortLine).trim());
                } else {
                    Thread.sleep(100);
                }
            }
            throw new NoPortFileException("Invalid PortFile:" + lockFile);
        } catch (final FileNotFoundException e) {
            throw new NoPortFileException("PortFile does not exist:" + lockFile, e);
        } catch (final InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new NoPortFileException(e);
        } catch (NoPortFileException e) {
            throw e;
        } catch (final Exception e) {
            throw new NoPortFileException("Failed to parse PortFile:" + lockFile, e);
        }
    }

    public static class ErrorReadingResponseException extends IOException {
        /**
         * @param e
         */
        public ErrorReadingResponseException(IOException e) {
            super(e);
        }

        /**
         * @param string
         */
        public ErrorReadingResponseException(String string) {
            super(string);
        }
    }

    public static class IncompleteResponseException extends ErrorReadingResponseException {
        public final boolean clientOKWasValid;

        /**
         * @param done
         * @param clientIDOK
         */
        public IncompleteResponseException(boolean clientIDOK) {
            super("The response was not terminated by a done message");
            this.clientOKWasValid = clientIDOK;
        }
    }

    public void sendToRunningInstance(ResponseListener callback, final String... message) throws IOException, ErrorReadingResponseException, InvalidResponseID, NoPortFileException, InterruptedException, ExceptionInRunningInstance {
        if (port < 0) {
            port = readPortFromPortFile();
        }
        final InetSocketAddress con = getAddress();
        if (con != null) {
            Socket socket = null;
            try {
                socket = new Socket();
                // long connectStart = Time.systemIndependentCurrentJVMTimeMillis();
                socket.connect(con, 5000);
                // System.out.println("Connect: " + (Time.systemIndependentCurrentJVMTimeMillis() - connectStart));
                // LogV3.info("Set Readtimeout: " + getReadtimeoutForReadingResponses());
                socket.setSoTimeout(getReadtimeoutForReadingResponses());
                final ChunkedInputStream chunkedIn = new ChunkedInputStream(socket.getInputStream());
                final ChunkedOutputStream chunkedOut = new ChunkedOutputStream(new BufferedOutputStream(socket.getOutputStream()));
                final BufferedInputStream bufferedIn = new BufferedInputStream(chunkedIn);
                this.writeLine(chunkedOut, getClientID());
                if (message == null || message.length == 0) {
                    this.writeLine(chunkedOut, "0");
                    if (callback != null) {
                        callback.onConnected(message);
                    }
                } else {
                    this.writeLine(chunkedOut, message.length + "");
                    if (callback != null) {
                        callback.onConnected(message);
                    }
                    for (final String msg : message) {
                        this.writeLine(chunkedOut, msg);
                    }
                }
                try {
                    // signal server EOF
                    chunkedOut.sendEOF();
                    socket.shutdownOutput();
                    readResponses(callback, bufferedIn);
                } catch (ErrorReadingResponseException e) {
                    throw e;
                } catch (InvalidResponseID e) {
                    throw e;
                } catch (IOException e) {
                    throw new ErrorReadingResponseException(e);
                }
                // waits for -1 of server(closing connection) or read timeout
                try {
                    String line = null;
                    while ((line = SingleAppInstance.this.readLine(bufferedIn)) != null) {
                        onIncommingTrailingMessage(line);
                    }
                } catch (IOException ignore) {
                    ignore.printStackTrace();
                }
            } finally {
                if (socket != null) {
                    try {
                        socket.shutdownInput();
                    } catch (final Throwable e) {
                    }
                    try {
                        socket.shutdownOutput();
                    } catch (final Throwable e) {
                    }
                    try {
                        socket.close();
                    } catch (final Throwable e) {
                    }
                }
            }
        }
    }

    public void readResponses(ResponseListener callback, final BufferedInputStream in) throws InterruptedException, IOException, ExceptionInRunningInstance {
        ExceptionInRunningInstance exception = null;
        boolean done = false;
        boolean clientIDOK = false;
        while (true) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            final String type = readLine(in);
            if (type == null) {
                // connection close
                break;
            }
            final String responseMessage = readLine(in);
            if (responseMessage == null) {
                // connection close
                break;
            }
            final Response response = new Response(type, responseMessage);
            if (GO_AWAY_BYE.equals(response.getType())) {
                done = true;
                break;
            } else if (GO_AWAY_INVALID_ID.equals(response.getType())) {
                throw new InvalidResponseID(response.getMessage());
            } else if (DONE.equals(response.getType())) {
                done = true;
                // lagacy
                clientIDOK = true;
                break;
            } else if (KEEP_ALIVE.equals(response.getType())) {
                // lagacy
                clientIDOK = true;
                continue;
            } else if (EXCEPTION.equals(response.getType())) {
                exception = new ExceptionInRunningInstance(response.getMessage());
                continue;
            } else if (CLIENT_ID_OK.equals(response.getType())) {
                clientIDOK = true;
                continue;
            } else if (callback != null) {
                callback.onReceivedResponse(response);
            }
        }
        if (exception != null) {
            throw exception;
        } else {
            if (!done) {
                throw new IncompleteResponseException(clientIDOK);
            }
        }
    }

    /**
     * @return
     * @throws UnknownHostException
     */
    protected InetSocketAddress getAddress() throws IOException {
        if (address == null) {
            if (port != 0) {
                address = new InetSocketAddress(this.getLocalHost(), port);
            }
        }
        return address;
    }

    /**
     * @return
     */
    protected int getReadtimeoutForReadingResponses() {
        return 5000;
    }

    /**
     * @return
     */
    protected int getReadtimeoutForReadingIncommingMessages() {
        return 5000;
    }

    /**
     * @param singleApp2
     * @param appID2
     * @param root
     * @return
     */
    protected String createID(String singleApp, String appID, String root) {
        return singleApp + "." + appID + "." + root;
    }

    public synchronized void start() throws AnotherInstanceRunningException, UncheckableInstanceException, AnotherInstanceRunningButFailedToConnectException, ErrorReadingResponseException, InterruptedException, ExceptionInRunningInstance {
        start(null, (String[]) null);
    }

    public synchronized void start(final ResponseListener responseListener, final String... message) throws AnotherInstanceRunningException, UncheckableInstanceException, AnotherInstanceRunningButFailedToConnectException, ErrorReadingResponseException, InterruptedException, ExceptionInRunningInstance {
        if (this.fileLock != null) {
            return;
        }
        if (this.alreadyUsed) {
            this.cannotStart(new IllegalStateException("create new instance!"));
        }
        this.alreadyUsed = true;
        try {
            GoAwayException goAwayException = null;
            ErrorReadingResponseException errorReadingException = null;
            IOException ioException = null;
            try {
                // try to connect to running instance
                this.port = readPortFromPortFile();
                this.sendToRunningInstance(responseListener, message);
                throw new AnotherInstanceRunningException(this.appID);
            } catch (IncompleteResponseException e) {
                if (e.clientOKWasValid) {
                    /// There is actually a running instance, but communication failed.no reason to start an own session
                    throw e;
                }
                errorReadingException = e;
            } catch (ErrorReadingResponseException e) {
                // e.g. readtimeout? - another e.g. incompatible instance
                errorReadingException = e;
            } catch (NoPortFileException e) {
                // e.printStackTrace();
                // probably no other instance
            } catch (GoAwayException e) {
                goAwayException = e;// throw new AnotherInstanceRunningButFailedToConnectException(this.appID, e);
            } catch (IOException e) {
                // e.printStackTrace();
                // portfile available, but nobody answered - probably the portfile was not removed on exit
                ioException = e;
            }
            this.lockChannel = new RandomAccessFile(this.lockFile, "rw").getChannel();
            boolean closeLockFlag = true;
            try {
                // EXCLUSIVE LOCKED first byte=SPACE
                this.fileLock = this.lockChannel.tryLock(0, 1, false);
                if (this.fileLock == null) {
                    throw new AnotherInstanceRunningButFailedToConnectException(this.appID);
                } else {
                    closeLockFlag = false;
                }
                if (goAwayException != null) {
                    LogV3.info("Single Instance Issue: Process sent GoAway. Maybe another process with different ID is listening on port " + port + " - change port");
                }
                if (errorReadingException != null) {
                    LogV3.info("Single Instance Issue: Response ReadingError. Maybe an incompatible process is listening on port " + port + " - change port");
                }
                if (ioException != null) {
                    LogV3.info("Single Instance Issue: Response IOException. Maybe an incompatible process is listening on port " + port + " - change port");
                }
            } catch (final OverlappingFileLockException e) {
                handleExceptions(goAwayException, errorReadingException, ioException, e);
                throw new AnotherInstanceRunningButFailedToConnectException(this.appID, e);
            } catch (final IOException e) {
                handleExceptions(goAwayException, errorReadingException, ioException, e);
                throw new AnotherInstanceRunningButFailedToConnectException(this.appID, e);
            } finally {
                if (closeLockFlag) {
                    this.foundRunningInstanceAndFailedToConnect();
                }
            }
            final InetAddress localHost = this.getLocalHost();
            this.serverSocket = new ServerSocket();
            SocketAddress socketAddress = null;
            try {
                if (port <= 0) {
                    port = SingleAppInstance.DEFAULTPORT;
                }
                // try old port first
                socketAddress = new InetSocketAddress(localHost, port);
                this.serverSocket.bind(socketAddress);
            } catch (final IOException e) {
                try {
                    this.serverSocket.close();
                } catch (final Throwable e2) {
                }
                this.serverSocket = new ServerSocket();
                socketAddress = new InetSocketAddress(localHost, 0);
                this.serverSocket.bind(socketAddress);
            }
            try {
                port = serverSocket.getLocalPort();
                onServerOpened(serverSocket);
                // SPACE(EXCLUSIVE LOCKED)+PORT+NEWLINE
                final byte[] portStringBytes = (" " + port + "\r\n").getBytes(UTF8);
                lockChannel.position(0);
                lockChannel.write(ByteBuffer.wrap(portStringBytes));
                lockChannel.force(true);
                lockFile.deleteOnExit();
                shutdownEvent = new ShutdownEvent() {
                    @Override
                    public void onShutdown(ShutdownRequest shutdownRequest) {
                        exit(true);
                    }
                };
                ShutdownController.getInstance().addShutdownEvent(shutdownEvent);
                this.startDaemon();
                final IncommingMessageListener listener = this.getListener();
                if (listener != null && responseListener != null && isForwardMessageDirectIfNoOtherInstanceIsFound()) {
                    // we are not connected to another instance - do not call the method
                    // responseListener.onConnected(message);
                    listener.onIncommingMessage(new ResponseSender() {
                        @Override
                        public void sendResponse(final Response response) {
                            responseListener.onReceivedResponse(response);
                        }
                    }, message);
                }
                return;
            } catch (final Throwable t) {
                this.cannotStart(t);
            }
        } catch (final FileNotFoundException e) {
            this.cannotStart(e);
        } catch (ErrorReadingResponseException e) {
            throw e;
        } catch (final IOException e) {
            try {
                this.serverSocket.close();
            } catch (final Throwable t) {
            }
            this.cannotStart(e);
        }
    }

    protected void handleExceptions(GoAwayException goAwayException, ErrorReadingResponseException errorReadingException, IOException ioException, final Exception e) throws AnotherInstanceRunningButFailedToConnectException, ErrorReadingResponseException {
        if (goAwayException != null) {
            Exceptions.addSuppressed(goAwayException, e);
            throw new AnotherInstanceRunningButFailedToConnectException(this.appID, goAwayException);
        }
        if (errorReadingException != null) {
            throw Exceptions.addSuppressed(errorReadingException, e);
        }
        if (ioException != null) {
            Exceptions.addSuppressed(e, ioException);
        }
    }

    /**
     * @param serverSocket2
     */
    protected void onServerOpened(ServerSocket serverSocket2) {
    }

    private synchronized void startDaemon() {
        Thread daemon = this.daemon.get();
        if (daemon != null && daemon.isAlive()) {
            return;
        }
        daemon = new Thread(new Runnable() {
            public void run() {
                try {
                    while (Thread.currentThread() == SingleAppInstance.this.daemon.get()) {
                        if (Thread.currentThread().isInterrupted()) {
                            break;
                        }
                        try {
                            /* accept new request */
                            final Socket client = SingleAppInstance.this.serverSocket.accept();
                            final Thread thread = new Thread("SingleAppInstanceClient: " + SingleAppInstance.this.appID + " - " + client.getRemoteSocketAddress()) {
                                {
                                    setDaemon(true);
                                }

                                public void run() {
                                    try {
                                        handleIncommingConnection(client);
                                    } catch (InterruptedException e) {
                                        DebugMode.breakIf(true, "It is actually not possble to reach this code");
                                        LogV3.log(e);
                                    } catch (Throwable e) {
                                        onUncaughtExceptionDuringHandlingIncommingConnections(e);
                                    }
                                }
                            };
                            thread.start();
                            onNewIncommingConnection(client, thread);
                        } catch (final IOException e) {
                            if (Thread.currentThread() == SingleAppInstance.this.daemon.get()) {
                                // onUncaughtExceptionDuringHandlingIncommingConnections(e);
                            }
                        }
                    }
                } finally {
                    closeServer();
                    synchronized (SingleAppInstance.this) {
                        SingleAppInstance.this.daemon.compareAndSet(Thread.currentThread(), null);
                    }
                }
            }
        });
        daemon.setName("SingleAppInstanceServer: " + this.appID);
        /* set daemonmode so java does not wait for this thread */
        daemon.setDaemon(true);
        this.daemon.set(daemon);
        daemon.start();
    }

    /**
     * @param client
     * @param thread
     */
    protected void onNewIncommingConnection(Socket client, Thread thread) {
    }

    protected void writeLine(final OutputStream outputStream, final String line) throws IOException {
        if (outputStream != null) {
            synchronized (outputStream) {
                outputStream.write(escapeLine(line).getBytes(UTF8));
                outputStream.write(NEWLINE.getBytes(UTF8));
                outputStream.flush();
            }
        }
    }

    protected String unescapeLine(String message) {
        if (message == null || "null".equals(message)) {
            return null;
        } else {
            return new String(HexFormatter.hexToByteArray(message), UTF8);
        }
    }

    protected String escapeLine(String responseMessage) {
        if (responseMessage == null) {
            return "null";
        } else {
            return HexFormatter.byteArrayToHex(responseMessage.getBytes(UTF8));
        }
    }

    protected void onUncaughtExceptionDuringHandlingIncommingConnections(Throwable e) {
        LogV3.log(e);
    };

    protected void sendResponse(OutputStream out, Response response) throws IOException {
        if (out != null && response != null) {
            final String type = response.getType();
            final String responseMessage = response.getMessage();
            synchronized (out) {
                writeLine(out, type);
                writeLine(out, responseMessage);
            }
        }
    }

    protected class ClientConnection {
        private final Socket              socket;
        private final BufferedInputStream bufferedIn;
        private final ChunkedInputStream  chunkedIn;
        private final ChunkedOutputStream chunkedOut;

        protected ClientConnection(Socket socket) throws IOException {
            this.socket = socket;
            chunkedIn = new ChunkedInputStream(socket.getInputStream());
            chunkedOut = new ChunkedOutputStream(new BufferedOutputStream(socket.getOutputStream()));
            bufferedIn = new BufferedInputStream(chunkedIn);
        }

        public String readLine() throws IOException, InterruptedException {
            return SingleAppInstance.this.readLine(bufferedIn);
        }

        public void sendResponse(Response response) throws IOException {
            SingleAppInstance.this.sendResponse(chunkedOut, response);
        }

        public boolean isOutputShutdown() {
            return socket.isOutputShutdown();
        }

        public void shutdownOutput() throws IOException {
            // signal client EOF
            synchronized (chunkedOut) {
                chunkedOut.sendEOF();
            }
            socket.shutdownOutput();
        }
    }

    private final Map<Socket, ClientConnection> connections          = new HashMap<Socket, ClientConnection>();
    private volatile long                       lastClosedConnection = -1;
    private volatile long                       lastOpenedConnection = -1;

    /**
     * Return the ms since the last connection or 0 if there is a connection or -1 if there has never been a connection
     */
    public long getIdleTime() {
        synchronized (connections) {
            if (connections.size() > 0) {
                return 0;
            } else if (lastClosedConnection < 0) {
                return -1;
            } else {
                return Time.systemIndependentCurrentJVMTimeMillis() - lastClosedConnection;
            }
        }
    }

    /**
     * Returns ms since the last connection was opened or -1 of there has never been a connection
     *
     * @return
     */
    public long getTimeSinceLastMessage() {
        if (lastOpenedConnection > 0) {
            return Time.systemIndependentCurrentJVMTimeMillis() - lastOpenedConnection;
        } else {
            return -1;
        }
    }

    protected ClientConnection buildConnection(final Socket socket) throws IOException {
        socket.setSoTimeout(getReadtimeoutForReadingIncommingMessages());
        return new ClientConnection(socket);
    }

    public void handleIncommingConnection(final Socket socket) throws InterruptedException, IOException {
        synchronized (connections) {
            connections.put(socket, null);
        }
        try {
            final ClientConnection client = buildConnection(socket);
            synchronized (connections) {
                if (connections.containsKey(socket)) {
                    connections.put(socket, client);
                }
                lastOpenedConnection = Time.systemIndependentCurrentJVMTimeMillis();
            }
            final String clientID = client.readLine();
            if (!StringUtils.equals(clientID, getServerID())) {
                client.sendResponse(new Response(GO_AWAY_INVALID_ID, "Bad clientID"));
            } else {
                client.sendResponse(new Response(CLIENT_ID_OK));
                String line = client.readLine();
                String[] message = null;
                if (line != null && line.length() > 0) {
                    if (line.matches("^\\d+$")) {
                        try {
                            final int lines = Integer.parseInt(line);
                            message = new String[lines];
                            for (int index = 0; index < lines; index++) {
                                message[index] = client.readLine();
                            }
                            final IncommingMessageListener listener = SingleAppInstance.this.getListener();
                            if (listener != null) {
                                try {
                                    final Thread keepAliveThread = new Thread("SingleInstance KeepAlive: " + appID) {
                                        {
                                            setDaemon(true);
                                        }

                                        @Override
                                        public void run() {
                                            while (!client.isOutputShutdown()) {
                                                try {
                                                    // System.out.println("Sleep " + (getReadtimeoutForReadingResponses() - 1000));
                                                    Thread.sleep(getReadtimeoutForReadingResponses() - 1000);
                                                } catch (InterruptedException e) {
                                                    return;
                                                }
                                                // System.out.println("PING");
                                                try {
                                                    client.sendResponse(new Response(KEEP_ALIVE, String.valueOf(Time.now())));
                                                } catch (IOException e) {
                                                    // LogV3.severe("Failed to send Keep-Alive");
                                                    return;
                                                }
                                            }
                                        }
                                    };
                                    keepAliveThread.start();
                                    try {
                                        listener.onIncommingMessage(new ResponseSender() {
                                            @Override
                                            public void sendResponse(Response response) throws FailedToSendResponseException {
                                                if (GO_AWAY_BYE.equals(response.getType())) {
                                                    throw new InvalidParameterException(GO_AWAY_BYE + " is reserved for internal usage");
                                                } else if (GO_AWAY_INVALID_ID.equals(response.getType())) {
                                                    throw new InvalidParameterException(GO_AWAY_INVALID_ID + " is reserved for internal usage");
                                                } else if (KEEP_ALIVE.equals(response.getType())) {
                                                    throw new InvalidParameterException(KEEP_ALIVE + " is reserved for internal usage");
                                                } else if (EXCEPTION.equals(response.getType())) {
                                                    throw new InvalidParameterException(EXCEPTION + " is reserved for internal usage");
                                                } else if (DONE.equals(response.getType())) {
                                                    throw new InvalidParameterException(DONE + " is reserved for internal usage");
                                                } else if (CLIENT_ID_OK.equals(response.getType())) {
                                                    throw new InvalidParameterException(CLIENT_ID_OK + " is reserved for internal usage");
                                                } else {
                                                    try {
                                                        client.sendResponse(response);
                                                    } catch (IOException e) {
                                                        throw new FailedToSendResponseException(response, e);
                                                    }
                                                }
                                            }
                                        }, message);
                                    } finally {
                                        keepAliveThread.interrupt();
                                    }
                                } catch (final Throwable e) {
                                    client.sendResponse(new Response(EXCEPTION, Exceptions.getStackTrace(e)));
                                }
                            }
                        } finally {
                            sendDone(client);
                        }
                    } else {
                        onIncommingInvalidMessage(line);
                        while ((line = client.readLine()) != null) {
                            onIncommingInvalidMessage(line);
                        }
                    }
                }
            }
            try {
                client.shutdownOutput();
                // waits for -1 of client(closing connection) or read timeout
                String line = null;
                while ((line = client.readLine()) != null) {
                    onIncommingTrailingMessage(line);
                }
            } catch (IOException ignore) {
                throw ignore;
            }
        } catch (IOException e) {
            synchronized (connections) {
                if (!connections.containsKey(socket)) {
                    // ignore IOException because socket got closed via closeAllConnections()
                    // throw e;
                } else {
                    throw e;
                }
            }
        } finally {
            try {
                socket.close();
            } catch (IOException ignore) {
            } finally {
                synchronized (connections) {
                    connections.remove(socket);
                }
            }
            lastClosedConnection = Time.systemIndependentCurrentJVMTimeMillis();
        }
    }

    protected void sendDone(final ClientConnection client) throws IOException {
        client.sendResponse(new Response(DONE));
    }

    protected void onIncommingInvalidMessage(String message) {
        org.appwork.loggingv3.LogV3.info("invalid SingleAppInstanceClient message:" + message);
    }

    protected void onIncommingTrailingMessage(String message) {
        org.appwork.loggingv3.LogV3.info("trailing Message:" + message);
    };

    /**
     * @throws IOException
     *
     */
    public void closeAllConnections() {
        while (true) {
            final Socket socket;
            final ClientConnection client;
            synchronized (connections) {
                if (connections.size() == 0) {
                    break;
                } else {
                    final Iterator<Entry<Socket, ClientConnection>> it = connections.entrySet().iterator();
                    final Entry<Socket, ClientConnection> next = it.next();
                    socket = next.getKey();
                    client = next.getValue();
                    it.remove();
                }
            }
            if (socket != null) {
                boolean closeSocket = true;
                if (client != null) {
                    try {
                        closeSocket = closeConnection(client);
                    } catch (IOException ignore) {
                    }
                }
                if (closeSocket) {
                    try {
                        socket.close();
                    } catch (IOException ignore) {
                    }
                }
            }
        }
    }

    protected boolean closeConnection(ClientConnection client) throws IOException {
        client.sendResponse(new Response(GO_AWAY_BYE));
        client.shutdownOutput();
        return true;
    }

    public String getClientID() {
        return createID(singleApp, appID, Application.getRoot(SingleAppInstance.class));
    }

    public String getServerID() {
        return createID(singleApp, appID, Application.getRoot(SingleAppInstance.class));
    }

    /**
     * @return
     */
    public int getPort() {
        return port;
    }
}
