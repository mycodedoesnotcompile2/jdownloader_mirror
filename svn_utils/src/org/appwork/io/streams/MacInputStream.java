package org.appwork.io.streams;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.crypto.Mac;

public class MacInputStream extends FilterInputStream {

    private Mac mac;

    /**
     * @param sha256_HMAC
     */
    public MacInputStream(Mac mac, InputStream is) {
        super(is);
        this.mac = mac;
    }

    public Mac getMac() {
        return mac;
    }

    public int read() throws IOException {
        int b = in.read();

        if (b >= 0) {
            // System.out.println("MACC 1 " + b);
            mac.update((byte) b);

        }
        return b;
    }

    public int read(byte[] b, int off, int len) throws IOException {
        int n = in.read(b, off, len);
        if (n > 0) {

            // byte[] str = new byte[n];
            // System.arraycopy(b, off, str, 0, n);
            // System.out.println("MACC 3 " + Arrays.toString(str));
            mac.update(b, off, n);
        }
        return n;
    }
}
