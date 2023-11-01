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
package org.appwork.utils.net.httpconnection;

import org.appwork.storage.Storable;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.locale._AWU;

/**
 * @author daniel {"password":null,"address":null,"connectMethodPrefered":false,"port"
 *         :-1,"type":null,"preferNativeImplementation":false,"username":null}
 */
public class HTTPProxyStorable implements Storable {
    public static enum TYPE implements LabelInterface {
        NONE {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_none();
            }
        },
        DIRECT {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_direct();
            }
        },
        SOCKS4 {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_socks4();
            }
        },
        SOCKS4A {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_socks4a();
            }
        },
        SOCKS5 {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_socks5();
            }
        },
        HTTP {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_http();
            }
        },
        HTTPS {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_https();
            }
        },
        AUTO {
            @Override
            public String getLabel() {
                return _AWU.T.proxy_type_auto_detect();
            }
        };
    }

    private String  username                   = null;
    private String  password                   = null;
    private int     port                       = -1;
    private String  address                    = null;
    private TYPE    type                       = null;
    private boolean useConnectMethod           = false;
    private boolean preferNativeImplementation = false;
    private boolean resolveHostName            = false;

    // private boolean keepAliveSupported = true;
    //
    // public boolean isKeepAliveSupported() {
    // return keepAliveSupported;
    // }
    //
    // public void setKeepAliveSupported(boolean keepAliveSupported) {
    // this.keepAliveSupported = keepAliveSupported;
    // }

    public boolean isResolveHostName() {
        return resolveHostName;
    }

    public void setResolveHostName(boolean resolveHostName) {
        this.resolveHostName = resolveHostName;
    }

    public HTTPProxyStorable(/* storable */) {
    }

    public String getAddress() {
        return this.address;
    }

    public String getPassword() {
        return this.password;
    }

    public int getPort() {
        return this.port;
    }

    public TYPE getType() {
        return this.type;
    }

    public String getUsername() {
        return this.username;
    }

    public boolean isConnectMethodPrefered() {
        return this.useConnectMethod;
    }

    public boolean isPreferNativeImplementation() {
        return this.preferNativeImplementation;
    }

    public void setAddress(final String address) {
        this.address = address;
    }

    public void setConnectMethodPrefered(final boolean value) {
        this.useConnectMethod = value;
    }

    public void setPassword(final String password) {
        this.password = password;
    }

    public void setPort(final int port) {
        this.port = port;
    }

    public void setPreferNativeImplementation(final boolean preferNativeImplementation) {
        this.preferNativeImplementation = preferNativeImplementation;
    }

    public void setType(final TYPE type) {
        this.type = type;
    }

    public void setUsername(final String username) {
        this.username = username;
    }

    protected boolean _doNotStoreInstance = false;

    public boolean _isDoNotStoreInstance() {
        return _doNotStoreInstance;
    }

    /**
     * Do not save the proxy to disk at all. This is usefull if the proxy was found By some proxy script
     *
     * @param doNotStoreInstance
     */
    public void _setDoNotStoreInstance(boolean doNotStoreInstance) {
        this._doNotStoreInstance = doNotStoreInstance;
    }

}
