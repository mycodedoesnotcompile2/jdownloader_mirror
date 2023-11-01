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
package org.appwork.utils.net.ftpserver;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;

import org.appwork.controlling.State;
import org.appwork.controlling.StateConflictException;
import org.appwork.controlling.StateMachine;
import org.appwork.controlling.StateMachineInterface;
import org.appwork.utils.Regex;


/**
 * @author daniel
 * 
 */
public class FtpConnection implements Runnable, StateMachineInterface {

    public static enum COMMAND {
        /* commands starting with X are experimental, see RFC1123 */
        ABOR(true, 0),
        REST(true, 1),
        RNTO(true, 1, -1),
        RNFR(true, 1, -1),
        DELE(true, 1, -1),
        XRMD(true, 1, -1),
        RMD(true, 1, -1),
        SIZE(true, 1, -1), /* rfc3659 */
        STRU(true, 1),
        MODE(true, 1),
        ALLO(true, 1, -1),
        APPE(true, 1, -1),
        STOR(true, 1, -1),
        XMKD(true, 1, -1),
        MKD(true, 1, -1),
        NLST(true, 1, -1),
        EPRT(true, 1, 1), /* RFC 2428 */
        EPSV(true, 0), /* RFC 2428 */
        RETR(true, 1, -1),
        TYPE(true, 1, 2),
        LIST(true, 0, 1),
        XCUP(true, 0),
        CDUP(true, 0),
        XCWD(true, 1, -1),
        CWD(true, 1, -1),
        XPWD(true, 0),
        PWD(true, 0),
        NOOP(false, 0),
        PASV(true, 0),
        PASS(false, 1),
        QUIT(true, 0),
        SYST(true, 0),
        PORT(true, 1),
        USER(false, 1);

        private int     paramSize;
        private int     maxSize;
        private boolean needLogin;

        private COMMAND(final boolean needLogin, final int paramSize) {
            this(needLogin, paramSize, paramSize);
        }

        private COMMAND(final boolean needLogin, final int paramSize, final int maxSize) {
            this.paramSize = paramSize;
            this.needLogin = needLogin;
            this.maxSize = maxSize;
        }

        public boolean match(final int length) {
            if (length == this.paramSize) { return true; }
            if (length == this.maxSize) { return true; }
            if (this.maxSize == -1) { return true; }
            return false;
        }

        public boolean needsLogin() {
            return this.needLogin;
        }
    }

    private static enum TYPE {
        ASCII,
        BINARY;
    }

    private static final State       IDLE         = new State("IDLE");
    private static final State       USER         = new State("USER");
    private static final State       PASS         = new State("USER");
    private static final State       LOGIN        = new State("USER");
    private static final State       LOGOUT       = new State("LOGOUT");
    private static final State       IDLEEND      = new State("IDLEEND");
    static {
        FtpConnection.IDLE.addChildren(FtpConnection.USER);
        FtpConnection.USER.addChildren(FtpConnection.PASS, FtpConnection.LOGIN, FtpConnection.LOGOUT);
        FtpConnection.PASS.addChildren(FtpConnection.LOGIN, FtpConnection.LOGOUT);
        FtpConnection.LOGIN.addChildren(FtpConnection.LOGOUT);
        FtpConnection.LOGOUT.addChildren(FtpConnection.IDLEEND);
    }

    private final FtpServer          ftpServer;
    private final Socket             controlSocket;
    private BufferedReader           reader;
    private BufferedWriter           writer;

    private StateMachine             stateMachine = null;

    private Thread                   thread       = null;

    private String                   passiveIP    = null;
    private int                      passivePort  = 0;
    private TYPE                     type         = TYPE.BINARY;
    private final FtpConnectionState connectionState;
    private Socket                   dataSocket   = null;
    private ServerSocket             serverSocket = null;

    /**
     * @param ftpServer
     * @param clientSocket
     * @throws IOException
     */
    public FtpConnection(final FtpServer ftpServer, final Socket clientSocket) throws IOException {
        this.stateMachine = new StateMachine(this, FtpConnection.IDLE, FtpConnection.IDLEEND);
        this.connectionState = ftpServer.getFtpCommandHandler().createNewConnectionState();
        this.ftpServer = ftpServer;
        this.controlSocket = clientSocket;
        this.controlSocket.setSoTimeout(20 * 1000);
        try {
            this.reader = new BufferedReader(new InputStreamReader(this.controlSocket.getInputStream()));
            this.writer = new BufferedWriter(new OutputStreamWriter(this.controlSocket.getOutputStream()));
            this.thread = new Thread(ftpServer.getThreadGroup(), this) {
                @Override
                public void interrupt() {
                    /* also close all connections on interrupt */
                    FtpConnection.this.closeDataConnection();
                    try {
                        FtpConnection.this.controlSocket.close();
                    } catch (final Throwable e) {
                    }
                    super.interrupt();
                }
            };
            this.thread.setName("FTPConnectionThread: " + this);
            this.thread.start();
        } catch (final IOException e) {
            try {
                this.controlSocket.close();
            } catch (final Throwable e2) {
            }
            this.closeDataConnection();
            throw e;
        }
    }

    private String buildParameter(final String[] commandParts) {
        if (commandParts == null) { return null; }
        String param = "";
        for (int index = 1; index < commandParts.length; index++) {
            if (param.length() > 0) {
                param += " ";
            }
            param += commandParts[index];
        }
        return param;
    }

    private void closeDataConnection() {
        try {
            this.dataSocket.close();
        } catch (final Throwable e) {
        } finally {
            this.dataSocket = null;
        }
        try {
            this.serverSocket.close();
        } catch (final Throwable e) {
        } finally {
            this.serverSocket = null;
        }
    }

    public StateMachine getStateMachine() {
        return this.stateMachine;
    }

    /**
     * @param command
     * @throws IOException
     */
    private void handleCommand(final String command) throws IOException {
        try {
            final String commandParts[] = command.split(" ");
            COMMAND commandEnum = null;
            try {
                commandEnum = COMMAND.valueOf(commandParts[0]);
            } catch (final IllegalArgumentException e) {
                commandEnum = null;
            }
            try {
                if (commandEnum != null) {
                    if (commandEnum.needLogin) {
                        /* checks if this command needs valid login */
                        if (!this.stateMachine.isState(FtpConnection.LOGIN)) { throw new FtpNotLoginException(); }
                    }
                    if (!commandEnum.match(commandParts.length - 1)) {
                        /* checks if the parameter syntax is okay */
                        throw new FtpCommandSyntaxException();
                    }
                    /* this checks RNFR,RNTO command sequence */
                    if (this.connectionState.getRenameFile() != null && !commandEnum.equals(COMMAND.RNTO)) {
                        /* when renameFile is set, a RNTO command MUST follow */
                        this.connectionState.setRenameFile(null);
                        throw new FtpBadSequenceException();
                    }
                    switch (commandEnum) {
                    case ABOR:
                        this.onABOR();
                        break;
                    case REST:
                        this.onREST(commandParts);
                        break;
                    case PASV:
                        this.onPASV();
                        break;
                    case RNTO:
                        this.onRNTO(commandParts);
                        break;
                    case RNFR:
                        this.onRNFR(commandParts);
                        break;
                    case XRMD:
                    case RMD:
                        this.onRMD(commandParts);
                        break;
                    case DELE:
                        this.onDELE(commandParts);
                        break;
                    case SIZE:
                        this.onSIZE(commandParts);
                        break;
                    case STRU:
                        this.onSTRU(commandParts);
                        break;
                    case MODE:
                        this.onMODE(commandParts);
                        break;
                    case ALLO:
                        this.onALLO();
                        break;
                    case APPE:
                        this.onSTOR(commandParts, true);
                        break;
                    case STOR:
                        this.onSTOR(commandParts, false);
                        break;
                    case XMKD:
                    case MKD:
                        this.onMKD(commandParts);
                        break;
                    case NLST:
                        this.onNLST(commandParts);
                        break;
                    case EPSV:
                        this.onEPSV(commandParts);
                        break;
                    case EPRT:
                        this.onEPRT(commandParts);
                        break;
                    case RETR:
                        this.onRETR(commandParts);
                        break;
                    case LIST:
                        this.onLIST(commandParts);
                        break;
                    case USER:
                        this.onUSER(commandParts);
                        break;
                    case PORT:
                        this.onPORT(commandParts);
                        break;
                    case SYST:
                        this.onSYST();
                        break;
                    case QUIT:
                        this.onQUIT();
                        break;
                    case PASS:
                        this.onPASS(commandParts);
                        break;
                    case NOOP:
                        this.onNOOP();
                        break;
                    case XPWD:
                    case PWD:
                        this.onPWD();
                        break;
                    case XCWD:
                    case CWD:
                        this.onCWD(commandParts);
                        break;
                    case XCUP:
                    case CDUP:
                        this.onCDUP();
                        break;
                    case TYPE:
                        this.onTYPE(commandParts);
                        break;
                    }
                } else {
                    throw new FtpCommandNotImplementedException();
                }
            } catch (final StateConflictException e) {
                throw new FtpBadSequenceException();
            }
        } catch (final FtpException e) {
            this.write(e.getCode(), e.getMessage());
        } catch (final Throwable e) {
            this.write(550, e.getMessage());
        }
    }

    /**
     * @throws IOException
     * 
     */
    private void onABOR() throws IOException {
        this.write(226, "Command okay");
    }

    private void onALLO() throws IOException {
        this.write(200, "Command okay");
    }

    private void onCDUP() throws IOException, FtpException {
        this.ftpServer.getFtpCommandHandler().onDirectoryUp(this.connectionState);
        this.write(200, "Command okay.");
    }

    private void onCWD(final String params[]) throws IOException, FtpException {
        this.ftpServer.getFtpCommandHandler().setCurrentDirectory(this.connectionState, this.buildParameter(params));
        // this.write(250, "\"" + this.connectionState.getCurrentDir() +
        // "\" is cwd.");
        this.write(250, "Directory successfully changed.");
    }

    /**
     * @param commandParts
     * @throws FtpException
     * @throws FtpFileNotExistException
     * @throws IOException
     */
    private void onDELE(final String[] commandParts) throws FtpFileNotExistException, FtpException, IOException {
        this.ftpServer.getFtpCommandHandler().removeFile(this.connectionState, this.buildParameter(commandParts));
        this.write(250, "\"" + this.buildParameter(commandParts) + "\" removed.");
    }

    /**
     * @param commandParts
     * @throws IOException
     */
    /**
     * RFC2428
     * 
     * @throws FtpException
     * 
     * @throws FtpNotLoginException
     **/
    private void onEPRT(final String[] commandParts) throws IOException, FtpException {
        final String parts[] = commandParts[1].split("\\|");
        this.closeDataConnection();
        if (parts.length != 4) { throw new FtpCommandSyntaxException(); }
        if (!"1".equals(parts[1])) {
            /* 2 equals IPV6 */
            throw new FtpException(522, "Network protocol not supported, use (1)");
        }
        this.passiveIP = parts[2];
        this.passivePort = Integer.parseInt(parts[3]);
        this.write(200, "PORT command successful");
    }

    /**
     * @param commandParts
     * @throws FtpException
     */
    private void onEPSV(final String[] commandParts) throws FtpException {
        boolean okay = false;
        this.closeDataConnection();
        try {
            this.serverSocket = new ServerSocket();
            SocketAddress socketAddress = null;
            if (this.ftpServer.isLocalhostOnly()) {
                /* bind socket to localhost */
                socketAddress = new InetSocketAddress(this.ftpServer.getLocalHost(), 0);
            }
            this.serverSocket.bind(socketAddress);
            okay = true;
            final int port = this.serverSocket.getLocalPort();
            this.write(229, "Entering Extended Passive Mode (|||" + port + "|)");
            return;
        } catch (final IOException e) {
            throw new FtpException(421, "could not open port");
        } finally {
            if (!okay) {
                this.closeDataConnection();
            }
        }
    }

    private void onLIST(final String params[]) throws IOException, FtpException {
        try {
            try {
                this.openDataConnection();
            } catch (final IOException e) {
                throw new FtpException(425, "Can't open data connection");
            }
            this.write(150, "Opening XY mode data connection for file list");
            try {
                final java.util.List<? extends FtpFile> list = this.ftpServer.getFtpCommandHandler().getFileList(this.connectionState, this.buildParameter(params));
                this.dataSocket.getOutputStream().write(this.ftpServer.getFtpCommandHandler().formatFileList(list).getBytes("UTF-8"));
                this.dataSocket.getOutputStream().flush();
            } catch (final FtpFileNotExistException e) {
                /* need another error code here */
                throw new FtpException(450, "Requested file action not taken; File unavailable");
            } catch (final FtpException e) {
                throw e;
            } catch (final Exception e) {
                throw new FtpException(451, "Requested action aborted: local error in processing");
            }
            /* we close the passive port after command */
            this.write(226, "Transfer complete.");
        } finally {
            this.closeDataConnection();
        }
    }

    /**
     * @param commandParts
     * @throws IOException
     * @throws FtpFileNotExistException
     */
    private void onMKD(final String[] commandParts) throws IOException, FtpException {
        this.ftpServer.getFtpCommandHandler().makeDirectory(this.connectionState, this.buildParameter(commandParts));
        this.write(257, "\"" + this.buildParameter(commandParts) + "\" created.");
    }

    private void onMODE(final String[] commandParts) throws IOException, FtpCommandParameterException {
        if ("S".equalsIgnoreCase(commandParts[1])) {
            this.write(200, "Command okay.");
        } else {
            throw new FtpCommandParameterException();
        }
    }

    /**
     * @param commandParts
     * @throws IOException
     * @throws FtpException
     */
    private void onNLST(final String[] commandParts) throws IOException, FtpException {
        try {
            try {
                this.openDataConnection();
            } catch (final IOException e) {
                throw new FtpException(425, "Can't open data connection");
            }
            this.write(150, "Opening XY mode data connection for file list");
            try {
                final java.util.List<? extends FtpFile> list = this.ftpServer.getFtpCommandHandler().getFileList(this.connectionState, this.buildParameter(commandParts));
                final StringBuilder sb = new StringBuilder();
                for (final FtpFile file : list) {
                    sb.append(file.getName());
                    sb.append("\r\n");
                }
                this.dataSocket.getOutputStream().write(sb.toString().getBytes("UTF-8"));
                this.dataSocket.getOutputStream().flush();
            } catch (final FtpFileNotExistException e) {
                /* need another error code here */
                throw new FtpException(450, "Requested file action not taken; File unavailable");
            } catch (final FtpException e) {
                throw e;
            } catch (final Exception e) {
                throw new FtpException(451, "Requested action aborted: local error in processing");
            }
            /* we close the passive port after command */
            this.write(226, "Transfer complete.");
        } finally {
            this.closeDataConnection();
        }
    }

    private void onNOOP() throws IOException {
        this.write(200, "Command okay");
    }

    private void onPASS(final String params[]) throws IOException, FtpException {
        this.stateMachine.setStatus(FtpConnection.PASS);
        if (this.connectionState.getUser() == null) {
            throw new FtpBadSequenceException();
        } else {
            if (this.connectionState.getUser().getPassword() != null) {
                if (this.connectionState.getUser().getPassword().equals(params[1])) {
                    final String message = this.ftpServer.getFtpCommandHandler().onLoginSuccessRequest(this.connectionState);
                    if (message != null) {
                        this.write(230, message, true);
                    }
                    this.write(230, "User logged in, proceed");
                    this.stateMachine.setStatus(FtpConnection.LOGIN);
                } else {
                    final String message = this.ftpServer.getFtpCommandHandler().onLoginFailedMessage(this.connectionState);
                    if (message != null) {
                        this.write(530, message, true);
                    }
                    this.stateMachine.setStatus(FtpConnection.LOGOUT);
                    this.stateMachine.setStatus(FtpConnection.IDLEEND);
                    this.stateMachine.reset(false);
                    throw new FtpNotLoginException();
                }
            } else {
                throw new RuntimeException("THIS MUST NOT HAPPEN!");
            }
        }
    }

    /**
     * @throws FtpException
     * @throws IOException
     * 
     */
    private void onPASV() throws FtpException {
        boolean okay = false;
        this.closeDataConnection();
        try {
            this.serverSocket = new ServerSocket();
            SocketAddress socketAddress = null;
            if (this.ftpServer.isLocalhostOnly()) {
                /* bind socket to localhost */
                socketAddress = new InetSocketAddress(this.ftpServer.getLocalHost(), 0);
            }
            this.serverSocket.bind(socketAddress);
            okay = true;
            final int port = this.serverSocket.getLocalPort();
            final int p1 = port / 256;
            final int p2 = port - p1 * 256;
            if (this.ftpServer.isLocalhostOnly()) {
                /* localhost only */
                this.write(227, "Entering Passive Mode. (127,0,0,1," + p1 + "," + p2 + ").");
            } else {
                if (this.controlSocket.getLocalAddress().isLoopbackAddress()) {
                    this.write(227, "Entering Passive Mode. (127,0,0,1," + p1 + "," + p2 + ").");
                } else {
                    String ip = this.controlSocket.getLocalAddress().getHostAddress();
                    ip = ip.replaceAll("\\.", ",");
                    this.write(227, "Entering Passive Mode. (" + ip + "," + p1 + "," + p2 + ").");
                }
            }
            return;
        } catch (final IOException e) {
            throw new FtpException(421, "could not open port");
        } finally {
            if (!okay) {
                this.closeDataConnection();
            }
        }
    }

    private void onPORT(final String params[]) throws IOException, FtpCommandSyntaxException {
        try {
            /* close old maybe existing data connection */
            this.dataSocket.close();
        } catch (final Throwable e) {
        } finally {
            this.dataSocket = null;
        }
        final String parts[] = params[1].split(",");
        if (parts.length != 6) { throw new FtpCommandSyntaxException(); }
        this.passiveIP = parts[0] + "." + parts[1] + "." + parts[2] + "." + parts[3];
        this.passivePort = Integer.parseInt(parts[4]) * 256 + Integer.parseInt(parts[5]);
        this.write(200, "PORT command successful");
    }

    private void onPWD() throws IOException, FtpException {
        this.write(257, "\"" + this.connectionState.getCurrentDir() + "\" is cwd.");
    }

    private void onQUIT() throws IOException, FtpException {
        this.stateMachine.setStatus(FtpConnection.LOGOUT);
        this.write(221, this.ftpServer.getFtpCommandHandler().onLogoutRequest(this.connectionState));
        this.stateMachine.setStatus(FtpConnection.IDLEEND);
    }

    /**
     * @param commandParts
     * @throws FtpException
     * @throws IOException
     */
    private void onREST(final String[] commandParts) throws FtpException, IOException {
        try {
            final long position = Long.parseLong(commandParts[1]);
            this.ftpServer.getFtpCommandHandler().onREST(this.connectionState, position);
            this.write(350, "Restarting at " + position + ". Send STORE or RETRIEVE");
        } catch (final NumberFormatException e) {
            this.write(554, "Requested action not taken: invalid REST parameter.");
        }
    }

    private void onRETR(final String[] commandParts) throws IOException, FtpException {
        try {
            try {
                this.openDataConnection();
            } catch (final IOException e) {
                throw new FtpException(425, "Can't open data connection");
            }
            /*
             * we need to make sure that the file exists before opening data
             * connection, see http://cr.yp.to/ftp/retr.html, RFC 959
             * 
             * this will cause the 550 file not found before opening the data
             * connection
             */
            this.ftpServer.getFtpCommandHandler().getSize(this.connectionState, this.buildParameter(commandParts));
            this.write(150, "Opening XY mode data connection for transfer");
            long bytesWritten = 0;
            try {
                bytesWritten = this.ftpServer.getFtpCommandHandler().onRETR(this.dataSocket.getOutputStream(), this.connectionState, this.buildParameter(commandParts));
                this.dataSocket.getOutputStream().flush();
                this.dataSocket.shutdownOutput();
            } catch (final FtpFileNotExistException e) {
                /* need another error code here */
                throw new FtpException(450, "Requested file action not taken; File unavailable");
            } catch (final FtpException e) {
                throw e;
            } catch (final IOException e) {
                throw new FtpException(426, e.getMessage());
            } catch (final Exception e) {
                throw new FtpException(451, e.getMessage());
            }
            /* we close the passive port after command */
            this.write(226, "Transfer complete. " + bytesWritten + " bytes transfered!");
        } finally {
            this.closeDataConnection();
        }

    }

    /**
     * @param commandParts
     * @throws IOException
     * @throws FtpException
     * @throws FtpFileNotExistException
     */
    private void onRMD(final String[] commandParts) throws IOException, FtpException {
        this.ftpServer.getFtpCommandHandler().removeDirectory(this.connectionState, this.buildParameter(commandParts));
        this.write(250, "\"" + this.buildParameter(commandParts) + "\" removed.");
    }

    /**
     * @param commandParts
     * @throws FtpBadSequenceException
     * @throws FtpFileNotExistException
     * @throws IOException
     */
    private void onRNFR(final String[] commandParts) throws FtpException, IOException {
        if (this.connectionState.getRenameFile() != null) {
            this.connectionState.setRenameFile(null);
            throw new FtpBadSequenceException();
        }
        try {
            this.ftpServer.getFtpCommandHandler().renameFile(this.connectionState, this.buildParameter(commandParts));
        } catch (final FtpException e) {
            this.connectionState.setRenameFile(null);
            throw e;
        }
        this.write(350, "\"" + this.buildParameter(commandParts) + "\" rename pending.");
    }

    /**
     * @param commandParts
     * @throws FtpBadSequenceException
     * @throws FtpFileNotExistException
     * @throws IOException
     */
    private void onRNTO(final String[] commandParts) throws IOException, FtpException {
        if (this.connectionState.getRenameFile() == null) {
            /* a renameFile must exist, RNFR must be the command before RNTO */
            this.connectionState.setRenameFile(null);
            throw new FtpBadSequenceException();
        }
        try {
            this.ftpServer.getFtpCommandHandler().renameFile(this.connectionState, this.buildParameter(commandParts));
        } finally {
            this.connectionState.setRenameFile(null);
        }
        this.write(250, "\"" + this.buildParameter(commandParts) + "\" rename successful.");
    }

    /**
     * @param commandParts
     * @throws IOException
     * @throws FtpFileNotExistException
     */
    private void onSIZE(final String[] commandParts) throws FtpException, IOException {
        this.write(213, "" + this.ftpServer.getFtpCommandHandler().getSize(this.connectionState, this.buildParameter(commandParts)));
    }

    private void onSTOR(final String[] commandParts, final boolean append) throws IOException, FtpException {
        try {
            try {
                this.openDataConnection();
            } catch (final IOException e) {
                throw new FtpException(425, "Can't open data connection");
            }
            this.write(150, "Opening XY mode data connection for transfer");
            long bytesRead = 0;
            try {
                bytesRead = this.ftpServer.getFtpCommandHandler().onSTOR(this.dataSocket.getInputStream(), this.connectionState, append, this.buildParameter(commandParts));
                this.dataSocket.shutdownInput();
            } catch (final FtpFileNotExistException e) {
                /* need another error code here */
                throw new FtpException(450, "Requested file action not taken; File unavailable");
            } catch (final FtpException e) {
                throw e;
            } catch (final IOException e) {
                throw new FtpException(426, e.getMessage());
            } catch (final Exception e) {
                throw new FtpException(451, e.getMessage());
            }
            /* we close the passive port after command */
            this.write(226, "Transfer complete. " + bytesRead + " bytes received!");
        } finally {
            this.closeDataConnection();
        }
    }

    private void onSTRU(final String[] commandParts) throws IOException, FtpCommandParameterException {
        if ("F".equalsIgnoreCase(commandParts[1])) {
            this.write(200, "Command okay.");
        } else {
            throw new FtpCommandParameterException();
        }
    }

    private void onSYST() throws IOException {
        this.write(215, "UNIX Type: L8");
    }

    private void onTYPE(final String[] commandParts) throws IOException, FtpCommandParameterException {
        final String type = commandParts[1];
        if ("A".equalsIgnoreCase(type)) {
            this.type = TYPE.ASCII;
        } else if ("I".equalsIgnoreCase(type)) {
            this.type = TYPE.BINARY;
        } else if ("L".equalsIgnoreCase(type)) {
            if (commandParts.length == 3 && "8".equals(commandParts[2])) {
                this.type = TYPE.BINARY;
            } else {
                throw new FtpCommandParameterException();
            }
        } else {
            throw new FtpCommandParameterException();
        }
        this.write(200, "Command okay");
    }

    private void onUSER(final String params[]) throws IOException, FtpException {
        if (this.stateMachine.isFinal()) {
            this.stateMachine.reset(false);
        }
        this.stateMachine.setStatus(FtpConnection.USER);
        this.connectionState.setUser(this.ftpServer.getFtpCommandHandler().getUser(params[1]));
        if (this.connectionState.getUser() != null) {
            if (this.connectionState.getUser().getPassword() == null) {
                final String message = this.ftpServer.getFtpCommandHandler().onLoginSuccessRequest(this.connectionState);
                if (message != null) {
                    this.write(230, message, true);
                }
                this.write(230, "User logged in, proceed");
                this.stateMachine.setStatus(FtpConnection.LOGIN);
            } else {
                this.write(331, "User name okay, need password");
            }
        } else {
            final String message = this.ftpServer.getFtpCommandHandler().onLoginFailedMessage(this.connectionState);
            if (message != null) {
                this.write(530, message, true);
            }
            this.stateMachine.setStatus(FtpConnection.LOGOUT);
            this.stateMachine.setStatus(FtpConnection.IDLEEND);
            this.stateMachine.reset(false);
            throw new FtpNotLoginException();
        }
    }

    private void openDataConnection() throws IOException {
        if (this.dataSocket == null || !this.dataSocket.isConnected()) {
            if (this.serverSocket != null && this.serverSocket.isBound()) {
                /* PASV */
                this.dataSocket = this.serverSocket.accept();
            } else {
                /* PORT */
                this.dataSocket = new Socket(this.passiveIP, this.passivePort);
            }
        }
    }

    public void run() {
        try {
            this.writeMultiLineAuto(220, this.ftpServer.getFtpCommandHandler().getWelcomeMessage(this.connectionState));
            while (true) {
                final String command = this.reader.readLine();
                if (command == null) {
                    break;
                }
                if (this.ftpServer.isDebug()) {
                          org.appwork.loggingv3.LogV3.info("REQ: " + command);
                }
                this.handleCommand(command);
            }
        } catch (final IOException e) {
            try {
                this.onQUIT();
            } catch (final IOException e1) {                
                e1.printStackTrace();
            } catch (final FtpException e1) {                
                e1.printStackTrace();
            }
        } finally {
            this.closeDataConnection();
            try {
                this.controlSocket.close();
            } catch (final Throwable e2) {
            }
        }
    }

    private void write(final int code, final String message) throws IOException {
        if (this.ftpServer.isDebug()) {
                  org.appwork.loggingv3.LogV3.info("RESP: " + code + " " + message);
        }
        this.write(code, message, false);
    }

    private void write(final int code, final String message, final boolean multiLine) throws IOException {
        if (multiLine) {
            this.writer.write(code + "-" + message + "\r\n");
        } else {
            this.writer.write(code + " " + message + "\r\n");
        }
        this.writer.flush();
    }

    private void writeMultiLineAuto(final int code, final String message) throws IOException {
        final String lines[] = Regex.getLines(message);
        if (lines != null) {
            for (int line = 0; line < lines.length; line++) {
                if (line == lines.length - 1) {
                    this.writer.write(code + " " + lines[line] + "\r\n");
                } else {
                    this.writer.write(code + "-" + lines[line] + "\r\n");
                }
            }
        }
        this.writer.flush();
    }
}
