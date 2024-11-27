package org.jdownloader.plugins.components.usenet;

import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.Storable;
import org.appwork.utils.StringUtils;

/**
 * Represents a Usenet server with host, port, and SSL configuration.
 */
public class UsenetServer implements Storable {
    private String  host        = null;
    private int     port        = -1;
    private int     connections = -1;
    private boolean ssl         = false;

    public UsenetServer() {
        // Required for Storable
    }

    public UsenetServer(final String host, final int port) {
        this(host, port, false);
    }

    public UsenetServer(final String host, final int port, final boolean ssl) {
        this.host = host;
        this.port = port;
        this.ssl = ssl;
    }

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        this.host = host;
    }

    public int getPort() {
        return port > 0 ? port : (isSSL() ? 563 : 119);
    }

    public void setPort(int port) {
        this.port = port;
    }

    public boolean isSSL() {
        return ssl;
    }

    public void setSSL(boolean ssl) {
        this.ssl = ssl;
    }

    public int getConnections() {
        return connections;
    }

    public void setConnections(int connections) {
        this.connections = connections;
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
