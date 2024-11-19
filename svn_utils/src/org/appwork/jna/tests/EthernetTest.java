package org.appwork.jna.tests;

import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.Collections;
import java.util.Enumeration;
import java.util.NoSuchElementException;

public class EthernetTest {
    public static void main(final String args[]) throws SocketException {
        final Enumeration<NetworkInterface> nets = NetworkInterface.getNetworkInterfaces();
        for (final NetworkInterface netint : Collections.list(nets)) {
            displayInterfaceInformation(netint);
        }
    }

    static void displayInterfaceInformation(final NetworkInterface netint) {
        try {
            final NetworkInterface nif = netint;
            if (nif == null) {
                System.err.println("Error getting the Network Interface");
                return;
            }
            final Enumeration<InetAddress> nifAddresses = nif.getInetAddresses();
            final InetSocketAddress inetAddr = new InetSocketAddress(nifAddresses.nextElement(), 0);
            System.out.println("Interface: " + nif.getName() + " - " + nif.getDisplayName());
            final DatagramSocket socket = new DatagramSocket(inetAddr);
            System.out.println(socket.getLocalAddress());
            System.out.println("");
        } catch (final SocketException ex) {
            System.out.println(ex.toString());
        } catch (final NoSuchElementException ex) {
            // System.out.println(ex.toString());
        }
    }
}
