package org.appwork.io.streams;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import javax.crypto.Mac;

public class MacOutputStream extends FilterOutputStream {

    private Mac mac;

    /**
     * @param sha256_HMAC
     */
    public MacOutputStream(Mac mac, OutputStream os) {
        super(os);
        this.mac = mac;
    }

    public Mac getMac() {
        return mac;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.io.FilterOutputStream#write(int)
     */
    @Override
    public void write(int b) throws IOException {
        // System.out.println("MACC 1 " + b);
        mac.update((byte) b);
        out.write(b);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.io.FilterOutputStream#write(byte[], int, int)
     */
    @Override
    public void write(byte[] b, int off, int len) throws IOException {
        // byte[] str = new byte[len];
        // System.arraycopy(b, off, str, 0, len);
        // System.out.println("MACC 3 " + Arrays.toString(str));
        mac.update(b, off, len);
        out.write(b, off, len);
    }
}
