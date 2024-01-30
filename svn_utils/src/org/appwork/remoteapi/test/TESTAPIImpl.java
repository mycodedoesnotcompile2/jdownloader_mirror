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
package org.appwork.remoteapi.test;

import java.awt.Color;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.LinkedList;

import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.remoteapi.RemoteAPIRequest;
import org.appwork.remoteapi.RemoteAPIResponse;
import org.appwork.txtresource.TranslationFactory;
import org.appwork.utils.net.HTTPHeader;

/**
 * @author daniel
 * 
 */
public class TESTAPIImpl implements TESTAPI, TestApiInterface, bla, JSONP {

    private final LinkedList<Color> colors = new LinkedList<Color>();
    private volatile boolean        stop   = false;

    // private final int counter = 0;

    @Override
    public void async(final RemoteAPIRequest request, final RemoteAPIResponse response) throws UnsupportedEncodingException, IOException {
        response.setResponseCode(ResponseCode.SUCCESS_OK);
        System.out.println("connect");
        // response.getResponseHeaders().add(new
        // HTTPHeader(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING,
        // HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED));
        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/html"));
        // response.getResponseHeaders().add(new
        // HTTPHeader(HTTPConstants.HEADER_REQUEST_CONNECTION, "keep-alive"));
        // final ChunkedOutputStream cos = new
        // ChunkedOutputStream(response.getOutputStream());
        final OutputStream os = response.getOutputStream(true);
        os.write("<html><div id='news'>ddd</div></html>".getBytes());
        // cos.flush();
        while (true) {
            final String kk = "<script type=\"text/javascript\">document.getElementById('news').innerHTML = \"" + System.currentTimeMillis() + "\";</script>\r\n";
            os.write(kk.getBytes());
            os.flush();
            try {
                Thread.sleep(10);
            } catch (final InterruptedException e) {
                return;
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.remoteapi.test.JSONP#color(int, int, int)
     */
    @Override
    public boolean color(final int r, final int g, final int b) {
        System.out.println(r + " " + g + " " + b);
        synchronized (this.colors) {
            if (r < 0 || g < 0 || b < 0) {
                this.stop = true;
                this.colors.clear();
            } else {
                this.colors.add(new Color(r, g, b));
            }
            this.colors.notify();
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.remoteapi.test.bla#getTranslation()
     */
    @Override
    public HashMap<String, String> getTranslation() {
        final Webinterface ti = TranslationFactory.create(Webinterface.class);
        final Method[] methods = ti._getHandler().getMethods();

        final HashMap<String, String> map = new HashMap<String, String>();
        for (final Method m : methods) {
            map.put(m.getName(), ti._getHandler().getTranslation(m));
        }
        return map;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.remoteapi.test.TestApiInterface#iAmGod(int,
     * org.appwork.remoteapi.RemoteAPIRequest, int,
     * org.appwork.remoteapi.RemoteAPIResponse, int)
     */
    @Override
    public void iAmGod(final int b, final RemoteAPIRequest request, final int a, final RemoteAPIResponse response, final int c) throws UnsupportedEncodingException, IOException {
        response.setResponseCode(ResponseCode.SUCCESS_OK);
        final String text = "You called god?" + b + "-" + a + "-" + c;

        final int length = text.getBytes().length;
        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, length + ""));
        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text"));
        response.getOutputStream(true).write(text.getBytes("UTF-8"));

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.remoteapi.test.TestApiInterface#merge(java.lang.String,
     * java.lang.String, int, boolean)
     */
    @Override
    public String merge(final String a, final String b, final int a2, final boolean b2) {        
        return a + b + a2 + b2;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.remoteapi.test.Ping#ping(java.lang.String,
     * java.lang.String)
     */

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.remoteapi.test.TestApiInterface#sum(int, int)
     */
    @Override
    public int sum(final long a, final Byte b) {        
        return (int) (a + b);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.remoteapi.test.TESTAPI#test()
     */
    @Override
    public String test() {
        return "TestSucessfull";
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.appwork.remoteapi.test.JSONP#test(org.appwork.remoteapi.RemoteAPIRequest
     * , org.appwork.remoteapi.RemoteAPIResponse)
     */
    @Override
    public void test(final String callback, final long id, final long timestamp, final RemoteAPIRequest request, final RemoteAPIResponse response) throws UnsupportedEncodingException, IOException {
        System.out.println(callback + " " + id + " " + timestamp);
        response.setResponseCode(ResponseCode.SUCCESS_OK);
        // response.getResponseHeaders().add(new
        // HTTPHeader(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING,
        // HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED));
        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "application/javascript"));
        // response.getResponseHeaders().add(new
        // HTTPHeader(HTTPConstants.HEADER_REQUEST_CONNECTION, "keep-alive"));

        // cos.flush();

        Color next = null;
        while (true) {
            synchronized (this.colors) {
                if (!this.colors.isEmpty()) {
                    next = this.colors.removeFirst();
                } else {
                    if (!this.stop) {
                        try {
                            this.colors.wait();
                        } catch (final InterruptedException e) {                            
                            e.printStackTrace();
                        }
                    }
                }
            }
            if (next != null || this.stop == true) {
                if (this.stop == true) {
                    this.stop = false;
                    final String ret = callback + "({\"id\":" + (id + 1) + ",\"_\":\"system\",\"action\":\"disconnect\"});";
                    System.out.println(ret);
                    response.getOutputStream(true).write(ret.getBytes());
                } else {
                    final String value = Integer.toHexString(next.getRGB()).substring(2);
                    final String ret = callback + "({\"id\":" + (id + 1) + ",\"rgb\":\"" + value + "\"});";
                    System.out.println(ret);
                    response.getOutputStream(true).write(ret.getBytes());
                }
                response.getOutputStream(true).flush();
                break;
            }
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.remoteapi.test.bla#test1()
     */
    @Override
    public String test1() {
        return "super";
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.remoteapi.test.TestApiInterface#toggle(boolean)
     */
    @Override
    public boolean toggle(final boolean b) {     
        return !b;
    }

}
