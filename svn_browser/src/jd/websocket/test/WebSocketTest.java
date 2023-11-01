package jd.websocket.test;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import jd.http.Browser;
import jd.websocket.WebSocketClient;

import org.appwork.utils.Application;
import org.appwork.utils.logging2.extmanager.LoggerFactory;
import org.appwork.utils.net.websocket.ReadWebSocketFrame;
import org.appwork.utils.net.websocket.WebSocketFrame;

public class WebSocketTest {
    public static void main(String[] args) throws IOException {
        Application.setApplication(".websocketTest");
        final Browser br = new Browser();
        br.setLogger(LoggerFactory.getDefaultLogger());
        br.setVerbose(true);
        br.setDebug(true);
        br.setFollowRedirects(true);
        WebSocketClient wsc = new WebSocketClient(br, new URL("http://echo.websocket.org/?encoding=text")) {
            @Override
            protected ReadWebSocketFrame readWebSocketFrame(InputStream is) throws IOException {
                if (is.available() > 0) {
                    return super.readWebSocketFrame(is);
                } else {
                    return null;
                }
            }
        };
        wsc.connect();
        wsc.writeFrame(wsc.buildPingFrame("test".getBytes()));
        WebSocketFrame frame = wsc.readNextFrame();
        System.out.println(frame);
        try {
            while (true) {
                frame = wsc.readNextFrame();
                if (frame != null) {
                    System.out.println(frame);
                } else {
                    break;
                }
            }
        } catch (final Throwable e) {
            e.printStackTrace();
        }
        wsc.writeFrame(wsc.buildUTF8TextFrame("Test2"));
        frame = wsc.readNextFrame();
        System.out.println(frame);
        wsc.close();
    }
}
