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
package org.appwork.utils.net.usenet;

import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.nio.charset.Charset;

import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.httpconnection.HTTPProxy;
import org.appwork.utils.net.httpconnection.JavaSSLSocketStreamFactory;
import org.appwork.utils.net.socketconnection.SocketConnection;

public abstract class SimpleUseNet {
    public static final Charset UTF8 = Charset.forName("UTF-8");

    public static enum COMMAND {
        HEAD {
            @Override
            public boolean isMultiLineResponse(int code) {
                return code == 221;
            }
        },
        AUTHINFO_USER {
            @Override
            public String getCommand() {
                return "AUTHINFO USER";
            }
        },
        AUTHINFO_PASS {
            @Override
            public String getCommand() {
                return "AUTHINFO PASS";
            }
        },
        DATE,
        BODY,
        HELP {
            @Override
            public boolean isMultiLineResponse(int code) {
                return code == 100;
            }
        },
        STAT,
        QUIT;
        public boolean isMultiLineResponse(int code) {
            return false;
        };

        public String getCommand() {
            return this.name();
        }
    }

    /**
     * rfc3977 nntp
     */
    private volatile Socket socket = null;

    public static class CommandResponse {
        private final int responseCode;

        public int getResponseCode() {
            return responseCode;
        }

        public String getMessage() {
            return message;
        }

        private final String message;

        private CommandResponse(final int responseCode, final String message) {
            this.responseCode = responseCode;
            this.message = message;
        }

        @Override
        public String toString() {
            return getResponseCode() + ":" + getMessage();
        }
    }

    public Socket getSocket() {
        return socket;
    }

    private volatile OutputStream outputStream = null;
    private volatile InputStream  inputStream  = null;
    private final byte[]          CRLF         = "\r\n".getBytes();
    private final LogInterface    logger;
    private final HTTPProxy       proxy;

    public HTTPProxy getProxy() {
        return proxy;
    }

    public SimpleUseNet(HTTPProxy proxy, LogInterface logger) {
        this.proxy = proxy;
        if (logger != null) {
            this.logger = logger;
        } else {
            this.logger = LogV3.I().getDefaultLogger();
        }
    }

    protected abstract Socket createSocket();

    public synchronized void connect(String server, int port, boolean ssl, String username, String password) throws IOException {
        connect(new InetSocketAddress(server, port), ssl, username, password);
    }

    protected SSLSocketFactory getSSLSocketFactory(final String sniHostName) throws IOException {
        return JavaSSLSocketStreamFactory.getInstance().getSSLSocketFactory(null, sniHostName);
    }

    protected boolean useSNIWorkaround() {
        return false;
    }

    /**
     * https://tools.ietf.org/html/rfc3977#section-12.5
     *
     * The default character set is changed from US-ASCII [ANSI1986] to UTF-8
     *
     * @return
     */
    public Charset getCharSet() {
        return UTF8;
    }

    public synchronized void connect(SocketAddress socketAddress, final boolean ssl, String username, String password) throws IOException {
        try {
            if (!ssl) {
                socket = createSocket();
                socket.setSoTimeout(getReadTimeout());
                socket.connect(socketAddress, getConnectTimeout());
            } else {
                boolean useSNIWorkaround = useSNIWorkaround();
                while (true) {
                    socket = createSocket();
                    socket.setSoTimeout(getReadTimeout());
                    socket.connect(socketAddress, getConnectTimeout());
                    try {
                        final SSLSocket sslSocket;
                        if (useSNIWorkaround) {
                            final SSLSocketFactory sslSocketFactory = getSSLSocketFactory(null);
                            /* wrong configured SNI at serverSide */
                            sslSocket = (SSLSocket) sslSocketFactory.createSocket(socket, "", socket.getPort(), true);
                        } else {
                            final String hostName = SocketConnection.getHostName(socketAddress);
                            if (hostName != null) {
                                final SSLSocketFactory sslSocketFactory = getSSLSocketFactory(hostName);
                                sslSocket = (SSLSocket) sslSocketFactory.createSocket(socket, hostName, socket.getPort(), true);
                            } else {
                                throw new IOException("no hostname for ssl available");
                            }
                        }
                        sslSocket.startHandshake();
                        socket = sslSocket;
                        break;
                    } catch (final IOException e) {
                        silentDisconnect(e);
                        if (useSNIWorkaround == false && e.getMessage().contains("unrecognized_name")) {
                            useSNIWorkaround = true;
                            continue;
                        }
                        throw e;
                    }
                }
            }
            outputStream = socket.getOutputStream();
            inputStream = socket.getInputStream();
            final CommandResponse response = readCommandResponse(null);
            switch (response.getResponseCode()) {
            case 200:
                // Service available, posting allowed
            case 201:
                // Service available, posting prohibited
                break;
            case 400:
                throw new ServerErrorResponseException(response);
            case 500:
                throw new ServerErrorResponseException(response);
            default:
                throw new UnknownResponseException(response);
            }
            if (username != null || password != null) {
                authenticate(username, password);
            }
        } catch (final IOException e) {
            silentDisconnect(e);
            throw e;
        }
    }

    private void checkInvalidAuth(final CommandResponse response) throws InvalidAuthException {
        final String message = response.getMessage();
        final int code = response.getResponseCode();
        switch (code) {
        case 481:
            throw new InvalidAuthException(code + "|" + message);
        case 482:
            if (StringUtils.containsIgnoreCase(message, "incorrect")) {
                throw new InvalidAuthException(code + "|" + message);
            } else if (StringUtils.containsIgnoreCase(message, "expired")) {
                throw new InvalidAuthException(code + "|" + message);
            } else if (StringUtils.containsIgnoreCase(message, "suspended")) {
                throw new InvalidAuthException(code + "|" + message);
            }
            break;
        case 502:
            if (StringUtils.containsIgnoreCase(message, "Authentication Failed") || StringUtils.containsIgnoreCase(message, "Authentication details unknown") || StringUtils.containsIgnoreCase(message, "Please check your login") || StringUtils.contains(message, "Access Denied")) {
                // user/pass incorrect
                throw new InvalidAuthException(code + "|" + message);
            }
        }
    }

    private void authenticate(String username, String password) throws IOException {
        final String user = username != null ? username : "";
        CommandResponse response = sendCmd(COMMAND.AUTHINFO_USER, user);
        switch (response.getResponseCode()) {
        case 281:
            // user correct/no pass required
            return;
        case 381:
            // pass required
            final String pass = password != null ? password : "";
            response = sendCmd(COMMAND.AUTHINFO_PASS, pass);
            switch (response.getResponseCode()) {
            case 281:
                // user/pass correct
                return;
            default:
                checkInvalidAuth(response);
                break;
            }
            break;
        default:
            checkInvalidAuth(response);
            break;
        }
        throw new UnknownResponseException(response);
    }

    private final ByteArrayOutputStream lineBuffer = new ByteArrayOutputStream() {
                                                       @Override
                                                       public synchronized byte[] toByteArray() {
                                                           return buf;
                                                       };
                                                   };

    protected synchronized String readLine() throws IOException {
        return readLine(lineBuffer);
    }

    protected synchronized String readLine(final ByteArrayOutputStream buffer) throws IOException {
        try {
            buffer.reset();
            final int length = readLine(getInputStream(), buffer);
            if (length == -1) {
                throw new EOFException();
            }
            final String ret = new String(buffer.toByteArray(), 0, length, getCharSet());
            logger.info("Read Response:" + ret);
            return ret;
        } catch (final IOException e) {
            silentDisconnect(e);
            throw e;
        }
    }

    protected int readLine(final InputStream inputStream, final OutputStream buffer) throws IOException {
        try {
            int c = 0;
            int length = 0;
            boolean CR = false;
            while (true) {
                c = inputStream.read();
                if (c == -1) {
                    if (length > 0) {
                        return length;
                    }
                    return -1;
                } else if (c == 13) {
                    if (CR) {
                        throw new IOException("CRCR!?");
                    } else {
                        CR = true;
                    }
                } else if (c == 10) {
                    if (CR) {
                        break;
                    } else {
                        throw new IOException("LF!?");
                    }
                } else {
                    if (CR) {
                        throw new IOException("CRXX!?");
                    }
                    buffer.write(c);
                    length++;
                }
            }
            return length;
        } catch (final IOException e) {
            silentDisconnect(e);
            throw e;
        }
    }

    private synchronized CommandResponse readCommandResponse(COMMAND command) throws IOException {
        String line = readLine();
        final int code = Integer.parseInt(line.substring(0, 3));
        if (command != null && command.isMultiLineResponse(code)) {
            final StringBuilder sb = new StringBuilder();
            sb.append(line);
            while (true) {
                line = readLine();
                if (".".equals(line)) {
                    break;
                } else {
                    sb.append("\r\n");
                    sb.append(line);
                }
            }
            return new CommandResponse(code, sb.toString());
        } else {
            return new CommandResponse(code, line.substring(3));
        }
    }

    protected InputStream getInputStream() {
        return inputStream;
    }

    protected String wrapMessageID(final String messageID) {
        String ret = messageID.trim();
        if (!ret.startsWith("<")) {
            ret = "<".concat(ret);
        }
        if (!ret.endsWith(">")) {
            ret = ret.concat(">");
        }
        return ret;
    }

    public synchronized boolean isMessageExisting(final String messageID) throws IOException {
        final CommandResponse response = sendCmd(COMMAND.STAT, wrapMessageID(messageID));
        switch (response.getResponseCode()) {
        case 223:
            return true;
        case 430:
            return false;
        default:
            throw new UnknownResponseException(response);
        }
    }

    public synchronized InputStream requestMessageBodyAsInputStream(final String messageID) throws IOException {
        return requestMessageBodyAsInputStream(messageID, true);
    }

    public synchronized InputStream requestMessageBodyAsInputStream(final String messageID, boolean autoDecode) throws IOException {
        final CommandResponse response = sendCmd(COMMAND.BODY, wrapMessageID(messageID));
        switch (response.getResponseCode()) {
        case 220:
            // nzbget --nserv responds with 220
            break;
        case 222:
            break;
        case 430:
            throw new MessageBodyNotFoundException(messageID);
        default:
            throw new UnknownResponseException(response);
        }
        if (!autoDecode) {
            return new BodyInputStream(this);
        } else {
            final ByteArrayOutputStream buffer = new ByteArrayOutputStream(1024 * 1024) {
                @Override
                public synchronized byte[] toByteArray() {
                    return buf;
                };
            };
            while (true) {
                buffer.reset();
                final int lineLength = readLine(getInputStream(), buffer);
                if (lineLength > 0) {
                    final String line = new String(buffer.toByteArray(), 0, lineLength, getCharSet());
                    logger.info("Read Response:" + line);
                    if (line.startsWith("=ybegin")) {
                        logger.info("yEnc Body detected");
                        return newYEncInputStream(this, messageID, buffer);
                    }
                    if (line.matches("^begin \\d{3} .+")) {
                        logger.info("uuEncode Body detected");
                        return new UUInputStream(this, buffer);
                    }
                } else if (lineLength == -1) {
                    break;
                }
            }
            throw new IOException("Unknown Body Format");
        }
    }

    protected YEncInputStream newYEncInputStream(SimpleUseNet simpleUseNet, String messageID, ByteArrayOutputStream buffer) throws IOException {
        return new YEncInputStream(simpleUseNet, messageID, buffer);
    }

    private synchronized void sendCommand(String request) throws IOException {
        if (!isConnected()) {
            throw new IOException("not connected");
        }
        try {
            logger.info("Send Command:" + request);
            outputStream.write(request.getBytes(getCharSet()));
            outputStream.write(CRLF);
            outputStream.flush();
        } catch (IOException e) {
            silentDisconnect(e);
            throw e;
        }
    }

    public int getReadTimeout() {
        return 30 * 1000;
    }

    public int getConnectTimeout() {
        return 60 * 1000;
    }

    public CommandResponse sendCmd(COMMAND command) throws IOException {
        return sendCmd(command, null);
    }

    public synchronized CommandResponse sendCmd(COMMAND command, String parameter) throws IOException {
        if (parameter != null) {
            sendCommand(command.getCommand() + " " + parameter);
        } else {
            sendCommand(command.getCommand());
        }
        final CommandResponse response = readCommandResponse(command);
        switch (response.getResponseCode()) {
        case 400:
            throw new UnrecognizedCommandException(command, parameter);
        case 480:
            throw new AuthRequiredException();
        }
        return response;
    }

    public boolean isConnected() {
        return socket != null;
    }

    public void quit() throws IOException {
        try {
            final CommandResponse response = sendCmd(COMMAND.QUIT, null);
            if (response.getResponseCode() != 205) {
                throw new UnexpectedResponseException(response);
            }
        } finally {
            disconnect();
        }
    }

    public void disconnect() throws IOException {
        final Socket socket = this.socket;
        this.socket = null;
        if (socket != null) {
            socket.close();
        }
    }

    private void silentDisconnect(Exception e) {
        try {
            disconnect();
        } catch (final IOException ignore) {
            if (e != null) {
                e.addSuppressed(ignore);
            }
        }
    }
}
