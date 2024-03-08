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
package org.appwork.utils.os.hardware;

import java.io.File;
import java.net.HttpURLConnection;
import java.net.Proxy;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.regex.Pattern;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSession;

import org.appwork.utils.IO;
import org.appwork.utils.Regex;
import org.appwork.utils.net.httpconnection.JavaSSLSocketStreamFactory;
import org.appwork.utils.os.CrossSystem;

/**
 * @author daniel
 * @date Oct 2, 2020
 *
 */
public abstract class QNAP implements HardwareTypeInterface {
    @Override
    public ID getHardwareType() {
        return ID.QNAP;
    }

    public static QNAP getQNAPDetails() {
        if (CrossSystem.isLinux()) {
            /*
             * /sbin/getcfg system version
             *
             * /sbin/getcfg system model
             *
             * /sbin/get_display_name
             */
            final boolean hasShellCommands = new File("/sbin/get_display_name").isFile() || new File("/sbin/getcfg").isFile();
            final boolean hasDataMount = new File("/share/HDA_DATA").isDirectory() || new File("/share/MD0_DATA").isDirectory() || new File("/share/CACHEDEV1_DATA").isDirectory() || new File("/share/CE_CACHEDEV1_DATA").isDirectory();
            if (hasShellCommands && hasDataMount) {
                QNAP qnap = QNAP.parseConfFiles();
                if (qnap != null) {
                    return qnap;
                }
                // add default ports
                final ArrayList<String> hosts = new ArrayList<String>(Arrays.asList(new String[] { "http://127.0.0.1:8080", "https://127.0.0.1:443" }));
                final int webinterfaceHTTPsPort = findWebinterfaceHTTPsPort();
                if (webinterfaceHTTPsPort != -1 && webinterfaceHTTPsPort != 443) {
                    hosts.add(0, "https://127.0.0.1:" + webinterfaceHTTPsPort);
                }
                final int webinterfaceHTTPPort = findWebinterfaceHTTPPort();
                if (webinterfaceHTTPPort != -1 && webinterfaceHTTPPort != 8080) {
                    hosts.add(0, "http://127.0.0.1:" + webinterfaceHTTPPort);
                }
                for (final String host : hosts) {
                    try {
                        final URL url = new URL(host + "/cgi-bin/authLogin.cgi");
                        final HttpURLConnection connection = (HttpURLConnection) url.openConnection(Proxy.NO_PROXY);
                        if (connection instanceof HttpsURLConnection) {
                            ((HttpsURLConnection) connection).setSSLSocketFactory(JavaSSLSocketStreamFactory.getInstance().getSSLSocketFactory(null, url.getHost()));
                            ((HttpsURLConnection) connection).setHostnameVerifier(new HostnameVerifier() {
                                @Override
                                public boolean verify(String hostname, SSLSession session) {
                                    return true;
                                }
                            });
                        }
                        connection.setConnectTimeout(2500);
                        connection.setReadTimeout(5000);
                        final byte[] response;
                        try {
                            connection.connect();
                            response = IO.readStream(8192, connection.getInputStream());
                        } finally {
                            connection.disconnect();
                        }
                        qnap = QNAP.parse(new String(response, "UTF-8"));
                        if (qnap != null) {
                            return qnap;
                        }
                    } catch (Exception ignore) {
                        ignore.printStackTrace();
                    }
                }
            }
        }
        return null;
    }

    private static QNAP parseConfFiles() {
        final File uLinuxConf = new File("/etc/config/uLinux.conf");
        final File platformConf = new File("/etc/platform.conf");
        String model = readValue(platformConf, "DISPLAY_NAME");
        if (model == null) {
            model = readValue(uLinuxConf, "Model");
        }
        if (model != null) {
            final String version = readValue(uLinuxConf, "Version");
            final String platform = readValue(platformConf, "Platform");
            final String finalModel = model;
            return new QNAP() {
                @Override
                public String getVersion() {
                    return version;
                }

                @Override
                public String getPlatform() {
                    return platform;
                }

                @Override
                public String getModel() {
                    return finalModel;
                }
            };
        } else {
            return null;
        }
    }

    private final static String readValue(final File file, String key) {
        try {
            if (file.isFile()) {
                final String content = IO.readFileToString(file);
                final String value = new Regex(content, Pattern.quote(key) + "\\s=\\s*([^\r\n]+)").getMatch(0);
                return value;
            }
        } catch (Exception ignore) {
        }
        return null;
    }

    private final static String getXMLFieldValue(final String xml, String key) {
        return new Regex(xml, "<" + Pattern.quote(key) + ">\\s*<\\!\\[CDATA\\[(.*?)\\]]>").getMatch(0);
    }

    private static int findWebinterfaceHTTPsPort() {
        try {
            final File apacheSSLConf = new File("/etc/apache-sys-proxy-ssl.conf");
            if (apacheSSLConf.isFile()) {
                try {
                    final String html = IO.readFileToString(apacheSSLConf);
                    final String port = new Regex(html, "Listen\\s+(\\d+)").getMatch(0);
                    if (port != null) {
                        return Integer.parseInt(port);
                    }
                } catch (Exception ignore) {
                }
            }
        } catch (Exception ignore) {
        }
        return -1;
    }

    private static int findWebinterfaceHTTPPort() {
        try {
            final File apacheConf = new File("/etc/apache-sys-proxy.conf");
            if (apacheConf.isFile()) {
                try {
                    final String html = IO.readFileToString(apacheConf);
                    final String port = new Regex(html, "Listen\\s+(\\d+)").getMatch(0);
                    if (port != null) {
                        return Integer.parseInt(port);
                    }
                } catch (Exception ignore) {
                }
            }
            final File qhttpdConf = new File("/home/Qhttpd/index.html");
            if (qhttpdConf.isFile()) {
                try {
                    final String html = IO.readFileToString(qhttpdConf);
                    final String port = new Regex(html, "\\s*\\+\\s*(\\d+)\\s*\\+").getMatch(0);
                    if (port != null) {
                        return Integer.parseInt(port);
                    }
                } catch (Exception ignore) {
                }
            }
            final URL url = new URL("http://127.0.0.1");
            final HttpURLConnection connection = (HttpURLConnection) url.openConnection(Proxy.NO_PROXY);
            try {
                connection.setInstanceFollowRedirects(true);
                connection.setConnectTimeout(2500);
                connection.setReadTimeout(5000);
                connection.connect();
                final String html = IO.readStreamToString(connection.getInputStream(), -1, true);
                final String port = new Regex(html, "\\s*\\+\\s*(\\d+)\\s*\\+").getMatch(0);
                if (port != null) {
                    return Integer.parseInt(port);
                }
            } finally {
                connection.disconnect();
            }
        } catch (Exception ignore) {
        }
        return -1;
    }

    private static QNAP parse(final String xml) {
        String model = getXMLFieldValue(xml, "displayModelName");
        if (model == null) {
            model = getXMLFieldValue(xml, "modelName");
            if (model == null) {
                model = getXMLFieldValue(xml, "internalModelName");
            }
        }
        if (model != null) {
            final String version = getXMLFieldValue(xml, "version");
            final String platform = getXMLFieldValue(xml, "platform");
            final String finalModel = model;
            return new QNAP() {
                @Override
                public String getPlatform() {
                    return platform;
                }

                @Override
                public String getModel() {
                    return finalModel;
                }

                @Override
                public String getVersion() {
                    return version;
                }
            };
        } else {
            return null;
        }
    }

    public String toString() {
        return "QNAP|Model:" + getModel() + "|Version:" + getVersion() + "|Platform:" + getPlatform();
    }

    private QNAP() {
    }

    public abstract String getPlatform();

    public abstract String getModel();

    public abstract String getVersion();
}
