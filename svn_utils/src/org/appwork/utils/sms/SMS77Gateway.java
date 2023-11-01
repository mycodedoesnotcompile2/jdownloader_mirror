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
package org.appwork.utils.sms;

import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Locale;

import org.appwork.utils.net.SimpleHTTP;

/**
 * @author daniel API-DOCS: https://www.sms77.de/api.pdf
 */
public class SMS77Gateway {

    public static final String API_URL  = "http://gateway.sms77.de/";
    private boolean            useHTTPS = false;

    private SimpleHTTP         br;

    private final String       userName;

    private final String       userPass;

    public SMS77Gateway(final String userName, final String userPass) {
        this.br = new SimpleHTTP();
        this.userName = userName;
        this.userPass = userPass;
    }

    private synchronized String[] callAPI(final String cmd, final SMS77GatewayParameter... parameters) throws SMS77GatewayException {
        final StringBuilder sb = new StringBuilder();
        try {
            sb.append("?u=" + URLEncoder.encode(this.userName, "UTF-8") + "&p=" + URLEncoder.encode(this.userPass, "UTF-8"));
        } catch (final UnsupportedEncodingException e) {
            org.appwork.loggingv3.LogV3.log(e);
            sb.append("?u=" + this.userName + "&p=" + this.userPass);
        }

        for (final SMS77GatewayParameter param : parameters) {
            if (param != null) {
                if (sb.length() > 0) {
                    sb.append('&');
                }
                sb.append(param.toString());
            }
        }
        try {
            String url = SMS77Gateway.API_URL;
            if (this.useHTTPS) {
                url = url.replaceFirst("http:", "https:");
            }
            /* post seems not to work */
            System.out.println(url + cmd + sb.toString());
            final String rets[] = this.br.getPage(new URL(url + cmd + sb.toString())).split("[\r\n]+");
            final String ret = rets[0];
            if ("900".equals(ret)) { throw new SMS77GatewayException("Benutzer/Passwort-Kombination falsch"); }
            if ("902".equals(ret)) { throw new SMS77GatewayException("http API für diesen Account deaktiviert"); }
            if ("903".equals(ret)) { throw new SMS77GatewayException("Server IP ist falsch"); }
            if ("700".equals(ret)) { throw new SMS77GatewayException("Unbekannter Fehler"); }
            return rets;
        } catch (final Throwable e) {
            if (e instanceof SMS77GatewayException) { throw (SMS77GatewayException) e; }
            throw new SMS77GatewayException(e.getMessage(), e);
        }
    }

    /*
     * returns current balance of the account
     */
    public double getBalance() throws SMS77GatewayException {
        final String ret = this.callAPI("balance.php")[0];
        return Double.parseDouble(ret);
    }

    public SimpleHTTP getBrowser() {
        return this.br;
    }

    /*
     * returns status of given messageID
     */
    public SMS77MsgStatus getSMSStatus(final String messageID) throws SMS77GatewayException {
        if (messageID == null) { throw new NullPointerException("messageID is null"); }
        final String ret[] = this.callAPI("status.php", SMS77GatewayParameter.create("msg_id", messageID));
        if ("901".equals(ret[0])) { throw new SMS77GatewayException("Ungültige Msg ID"); }
        try {
            return new SMS77MsgStatus(ret);
        } catch (final Throwable e) {
            throw new SMS77GatewayException(e.getMessage(), e);
        }
    }

    public boolean isUseHTTPS() {
        return this.useHTTPS;
    }

    /*
     * sends a message to given receiver and returns the messageID
     */
    public String sendSMS(final SMS77Message message, final String receiver) throws SMS77GatewayException {
        if (message == null) { throw new NullPointerException("message is null"); }
        if (receiver == null) { throw new NullPointerException("receiver is null"); }
        if (message.getMessage().length() > 1555) { throw new SMS77GatewayException("Message too long"); }
        final java.util.List<SMS77GatewayParameter> params = new ArrayList<SMS77GatewayParameter>();
        params.add(SMS77GatewayParameter.create("to", receiver));
        params.add(SMS77GatewayParameter.create("text", message.getMessage()));
        params.add(SMS77GatewayParameter.create("type", message.getType().name().toLowerCase(Locale.ENGLISH)));
        /* TODO: is basicplus really the right type here */
        if (message.getSender() != null && !message.getType().equals(SMS77Message.TYPE.BASICPLUS)) {
            params.add(SMS77GatewayParameter.create("from", message.getSender()));
        }
        params.add(SMS77GatewayParameter.create("return_msg_id", "1"));
        final String[] rets = this.callAPI("", params.toArray(new SMS77GatewayParameter[params.size()]));
        final String ret = rets[0];
        if ("400".equals(ret)) { throw new SMS77GatewayException("type invalid"); }
        if ("402".equals(ret)) { throw new SMS77GatewayException("Reload lock"); }
        if ("306".equals(ret)) { throw new SMS77GatewayException("Sendernumber invalid"); }
        if ("202".equals(ret)) { throw new SMS77GatewayException("Receiver Number invalid"); }
        if ("201".equals(ret)) { throw new SMS77GatewayException("Countrycode not valid for this sms type. Please send as basic sms"); }
        if ("101".equals(ret)) { throw new SMS77GatewayException("Sending failed for at least one receiver"); }
        if ("600".equals(ret)) { throw new SMS77GatewayException("Carrier error"); }

        return rets[1];
    }

    public void setBrowser(final SimpleHTTP br) {
        this.br = br;
    }

    public void setUseHTTPS(final boolean useHTTPS) {
        this.useHTTPS = useHTTPS;
    }
}
