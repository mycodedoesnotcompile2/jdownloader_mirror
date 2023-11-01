package org.appwork.io.streams.signature;

/**
 * Wrapper to provide a common interface for MAC and MessageDigest
 *
 * @author thomas
 * @date 06.10.2021
 *
 */
public interface DigestInterface {
    void update(byte[] nonceBytes);

    void update(byte b);

    void update(byte[] b, int off, int toWrite);

    byte[] doFinal();

    /**
     * @return
     */
    int getLength();

    /**
     * @return
     */
    boolean supportsCloning();

    /**
     *
     */
    DigestInterface clone() throws CloneNotSupportedException;
}
