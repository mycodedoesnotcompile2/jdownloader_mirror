//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
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
package jd.controlling.reconnect.pluginsinc.liveheader.recorder;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Vector;

import javax.net.SocketFactory;
import javax.net.ssl.SSLSocketFactory;

import jd.utils.JDHexUtils;

import org.appwork.utils.JDK8BufferHelper;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;

public class Proxy extends Thread {
    public final static int FORWARD       = 1 << 1;
    public final static int RECORD_HEADER = 1 << 2;
    public final static int CHANGE_HEADER = 1 << 3;
    private Socket          socket;
    private Vector<String>  steps         = null;
    private String          serverip;
    private int             port;
    private boolean         ishttps       = false;
    private boolean         israw         = false;

    public Proxy(Socket socket, Vector<String> steps, String serverip, int port, boolean ishttps, boolean israw) {
        super("JDProxy");
        this.socket = socket;
        this.steps = steps;
        this.serverip = serverip;
        this.port = port;
        this.ishttps = ishttps;
        this.israw = israw;
    }

    @Override
    public void run() {
        Socket incoming = socket;
        Socket outgoing = null;
        try {
            if (!ishttps) {
                outgoing = new Socket(serverip, port);
            } else {
                SocketFactory socketFactory = SSLSocketFactory.getDefault();
                outgoing = socketFactory.createSocket(serverip, port);
            }
            ProxyThread thread1 = new ProxyThread(incoming, outgoing, CHANGE_HEADER | RECORD_HEADER, steps, ishttps, israw);
            thread1.setHost(serverip);
            thread1.setName("Client2Router");
            thread1.start();
            ProxyThread thread2 = new ProxyThread(outgoing, incoming, CHANGE_HEADER, steps, ishttps, israw);
            thread2.setHost(serverip);
            thread2.setName("Router2Client");
            thread2.start();
            thread2.join();
            try {
                outgoing.shutdownInput();
            } catch (Exception e) {
            }
            try {
                incoming.shutdownInput();
            } catch (Exception e) {
            }
            try {
                outgoing.shutdownOutput();
            } catch (Exception e) {
            }
            try {
                incoming.shutdownOutput();
            } catch (Exception e) {
            }
        } catch (Exception e) {
        }
    }
}

class ProxyThread extends Thread {
    final private Socket         incoming;
    final private Socket         outgoing;
    final private Vector<String> steps;
    private int                  dowhat = 0;
    final private boolean        ishttps;
    final private boolean        israw;
    private String               host   = null;

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        this.host = host;
    }

    boolean renewbuffer = false;
    String  buffer;

    public ProxyThread(Socket incoming, Socket outgoing, int dowhat, Vector<String> steps, boolean ishttps, boolean israw) {
        this.incoming = incoming;
        this.outgoing = outgoing;
        this.dowhat = dowhat;
        this.steps = steps;
        this.ishttps = ishttps;
        this.israw = israw;
    }

    public boolean dothis(int dothis) {
        return (this.dowhat & dothis) > 0;
    }

    @Override
    public void run() {
        byte[] minibuffer = new byte[2048];
        ByteBuffer headerbuffer = null;
        byte[] postData = null;
        OutputStream toClient = null;
        InputStream fromClient;
        try {
            toClient = outgoing.getOutputStream();
            fromClient = incoming.getInputStream();
            if (dothis(Proxy.CHANGE_HEADER) || dothis(Proxy.RECORD_HEADER)) {
                headerbuffer = HTTPConnectionUtils.readheader(fromClient, false);
            }
            if (dothis(Proxy.RECORD_HEADER)) {
                try {
                    InputStream k = Utils.newInputStream(headerbuffer);
                    BufferedInputStream reader2 = new BufferedInputStream(k);
                    String line = null;
                    LinkedHashMap<String, String> headers = new LinkedHashMap<String, String>();
                    while ((line = Utils.readline(reader2)) != null && line.trim().length() > 0) {
                        String key = null;
                        String value = null;
                        if (line.indexOf(": ") > 0) {
                            key = line.substring(0, line.indexOf(": ")).toLowerCase();
                            value = line.substring(line.indexOf(": ") + 2);
                        } else {
                            key = null;
                            value = line;
                        }
                        headers.put(key, value);
                    }
                    if (headers.containsKey("content-type")) {
                        final String contentType = headers.get("content-type");
                        if (StringUtils.containsIgnoreCase(contentType, "x-www-form-urlencoded") || StringUtils.containsIgnoreCase(contentType, "text") || StringUtils.containsIgnoreCase(contentType, "json")) {
                            if (headers.containsKey("content-length")) {
                                final int post_len = Integer.parseInt(headers.get("content-length"));
                                int post_len_toread = post_len;
                                int post_len_read = 0;
                                postData = new byte[post_len];
                                int indexstart = 0;
                                while (post_len_toread > 0) {
                                    if ((post_len_read = fromClient.read(postData, indexstart, post_len_toread)) == -1) {
                                        break;
                                    }
                                    indexstart = indexstart + post_len_read;
                                    post_len_toread = post_len_toread - post_len_read;
                                }
                            }
                        }
                    }
                    Utils.createStep(headers, postData, steps, ishttps, israw);
                } catch (Exception e) {
                }
            }
            if (dothis(Proxy.CHANGE_HEADER) || dothis(Proxy.RECORD_HEADER)) {
                JDK8BufferHelper.position(headerbuffer, 0);
                renewbuffer = false;
                byte[] b = new byte[headerbuffer.limit()];
                headerbuffer.get(b);
                buffer = JDHexUtils.getHexString(b);
                if (dothis(Proxy.CHANGE_HEADER)) {
                    Utils.rewriteConnectionHeader(this);
                    Utils.rewriteLocationHeader(this);
                    Utils.rewriteHostHeader(this);
                    Utils.rewriteRefererHeader(this);
                }
                if (renewbuffer == true) {
                    headerbuffer = ByteBuffer.wrap(JDHexUtils.getByteArray(buffer));
                } else {
                    headerbuffer = ByteBuffer.wrap(b);
                }
                try {
                    InputStream k = Utils.newInputStream(headerbuffer.duplicate());
                    BufferedInputStream reader2 = new BufferedInputStream(k);
                    String line = null;
                    HashMap<String, String> headers = new HashMap<String, String>();
                    while ((line = Utils.readline(reader2)) != null && line.trim().length() > 0) {
                        String key = null;
                        String value = null;
                        if (line.indexOf(": ") > 0) {
                            key = line.substring(0, line.indexOf(": ")).toLowerCase();
                            value = line.substring(line.indexOf(": ") + 2);
                        } else {
                            key = null;
                            value = line;
                        }
                        headers.put(key, value);
                    }
                } catch (Exception e) {
                }
                final InputStream fromClient2 = Utils.newInputStream(headerbuffer);
                while (true) {
                    final int numberRead = fromClient2.read(minibuffer);
                    if (numberRead == -1) {
                        break;
                    } else {
                        toClient.write(minibuffer, 0, numberRead);
                    }
                }
                if (postData != null) {
                    toClient.write(postData);
                }
            }
            try {
                while (true) {
                    final int numberRead = fromClient.read(minibuffer);
                    if (numberRead == -1) {
                        break;
                    } else {
                        toClient.write(minibuffer, 0, numberRead);
                    }
                }
            } catch (Exception e) {
            }
        } catch (Exception e) {
        }
    }
}
