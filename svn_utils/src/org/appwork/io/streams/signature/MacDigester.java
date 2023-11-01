package org.appwork.io.streams.signature;

import javax.crypto.Mac;

/**
 * Wrapper to provide a common interface for MAC and MessageDigest
 *
 * @author thomas
 * @date 06.10.2021
 *
 */
public class MacDigester implements DigestInterface {
    private final Mac mac;

    public MacDigester(final Mac mac) {
        this.mac = mac;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return getLength() + "b " + mac.getAlgorithm();
    }

    @Override
    public void update(final byte[] b) {
        mac.update(b);
    }

    @Override
    public void update(final byte b) {
        mac.update(b);
    }

    @Override
    public void update(final byte[] b, final int off, final int len) {
        mac.update(b, off, len);
    }

    @Override
    public byte[] doFinal() {
        return mac.doFinal();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.io.streams.signature.DigestInterface#getSignatureLength()
     */
    @Override
    public int getLength() {

        return mac.getMacLength();
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#clone()
     */
    @Override
    public MacDigester clone() throws CloneNotSupportedException {
        Mac interna = (Mac) mac.clone();
        return new MacDigester(interna);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.io.streams.signature.DigestInterface#supportsCloning()
     */
    @Override
    public boolean supportsCloning() {

        return mac instanceof Cloneable;
    }
}
