package org.jdownloader.plugins.components.usenet;

import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.Storable;
import org.appwork.utils.StringUtils;

/**
 * Represents a Usenet server with host, port, and SSL configuration.
 */
public record UsenetServer(String host, int port, int connections, boolean ssl) implements Storable {

    public UsenetServer() {
        this(null, -1, -1, false);
    }

    public UsenetServer(final String host, final int port) {
        this(host, port, -1, false);
    }

    public UsenetServer(final String host, final int port, final boolean ssl) {
        this(host, port, -1, ssl);
    }

    public String getHost() {
        return host;
    }

    public int getPort() {
        return port > 0 ? port : (isSSL() ? 563 : 119);
    }


    public boolean isSSL() {
        return ssl;
    }


    public int getConnections() {
        return connections;
    }

    // Public Methods
    public boolean validate() {
        return getPort() > 0 && StringUtils.isNotEmpty(getHost());
    }

    @Override
    public int hashCode() {
        return (getHost() + ":" + getPort() + ":" + isSSL()).hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj == null || !(obj instanceof UsenetServer)) {
            return false;
        }
        final UsenetServer other = (UsenetServer) obj;
        return isSSL() == other.isSSL() && getPort() == other.getPort() && StringUtils.equalsIgnoreCase(getHost(), other.getHost());
    }

    @Override
    public String toString() {
        return String.format("Host:%s|Port:%d|Connections:%d|SSL:%b", getHost(), getPort(), getConnections(), isSSL());
    }

    // Static Factory Methods
    public static List<UsenetServer> createServerList(final String host, final boolean ssl, final int... ports) {
        if (ssl) {
            return createServerList(host, new int[0], ports);
        } else {
            return createServerList(host, ports, new int[0]);
        }
    }

    public static List<UsenetServer> createServerList(final String host, final int[] nonSSLPorts, final int[] SSLPorts) {
        final List<UsenetServer> servers = new ArrayList<UsenetServer>();
        for (int port : nonSSLPorts) {
            servers.add(new UsenetServer(host, port, false));
        }
        for (int port : SSLPorts) {
            servers.add(new UsenetServer(host, port, true));
        }
        return servers;
    }
}
