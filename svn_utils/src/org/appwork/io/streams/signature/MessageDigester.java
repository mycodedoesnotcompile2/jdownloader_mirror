package org.appwork.io.streams.signature;

import java.security.MessageDigest;

/**
 * Wrapper to provide a common interface for MAC and MessageDigest
 *
 * @author thomas
 * @date 06.10.2021
 *
 */
public class MessageDigester implements DigestInterface {
    private final MessageDigest digest;

    public MessageDigester(final MessageDigest digest) {
        this.digest = digest;
    }

    @Override
    public void update(final byte[] b) {
        digest.update(b);
    }

    @Override
    public void update(final byte b) {
        digest.update(b);
    }

    @Override
    public void update(final byte[] b, final int off, final int len) {
        digest.update(b, off, len);
    }

    @Override
    public byte[] doFinal() {
        return digest.digest();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.io.streams.signature.DigestInterface#getLength()
     */
    @Override
    public int getLength() {

        return digest.getDigestLength();
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return digest.toString();
    }

    @Override
    public MessageDigester clone() throws CloneNotSupportedException {
        MessageDigest interna = (MessageDigest) digest.clone();
        return new MessageDigester(interna);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.io.streams.signature.DigestInterface#supportsCloning()
     */
    @Override
    public boolean supportsCloning() {
        return digest instanceof Cloneable;
    }
}
