/**
 * ====================================================================================================================================================
 * "My JDownloader Client" License
 * The "My JDownloader Client" will be called [The Product] from now on.
 * ====================================================================================================================================================
 * Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 * Schwabacher Straße 117
 * 90763 Fürth
 * Germany
 * === Preamble ===
 * This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 * The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 * These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 * <p>
 * === 3rd Party Licences ===
 * Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 * to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 * <p>
 * === Definition: Commercial Usage ===
 * If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 * If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 * If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 * Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 * If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 * "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 * <p>
 * If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ====================================================================================================================================================
 */
package org.jdownloader.myjdownloader.client;

import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.lang.reflect.Type;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicLong;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.Mac;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.jdownloader.myjdownloader.client.bindings.ClientApiNameSpace;
import org.jdownloader.myjdownloader.client.bindings.interfaces.Linkable;
import org.jdownloader.myjdownloader.client.exceptions.MyJDownloaderException;

public abstract class AbstractMyJDClientForBasicJVM extends AbstractMyJDClient<Type> {
    private static AtomicLong RID_COUNTER = new AtomicLong(System.currentTimeMillis());
    private final HashMap<String, AbstractMyJDDeviceClient> deviceClients = new HashMap<String, AbstractMyJDDeviceClient>();

    public AbstractMyJDClientForBasicJVM(final String appKey) {
        super(appKey);
    }

    public <T> T callAction(final String deviceID, final String action, final Class<T> returnType, final Object... args) throws MyJDownloaderException {
        return (T) super.callAction(deviceID, action, returnType, args);
    }

    protected AbstractMyJDDeviceClient createDeviceClient(final String deviceID) {
        return new AbstractMyJDDeviceClient(deviceID, this);
    }

    @Override
    protected byte[] createSecret(final String username, final String password, final String domain) throws MyJDownloaderException {
        MessageDigest md;
        try {
            md = MessageDigest.getInstance("SHA-256");
            return md.digest((username.toLowerCase(Locale.ENGLISH) + password + domain.toLowerCase(Locale.ENGLISH)).getBytes("UTF-8"));
        } catch (final NoSuchAlgorithmException e) {
            throw MyJDownloaderException.get(e);
        } catch (final UnsupportedEncodingException e) {
            throw MyJDownloaderException.get(e);
        }
    }

    @Override
    protected byte[] decrypt(final byte[] crypted, final byte[] keyAndIV) throws MyJDownloaderException {
        try {
            final Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
            final byte[] iv = new byte[16];
            final byte[] key = new byte[16];
            System.arraycopy(keyAndIV, 0, iv, 0, 16);
            System.arraycopy(keyAndIV, 16, key, 0, 16);
            final IvParameterSpec ivSpec = new IvParameterSpec(iv);
            final SecretKeySpec skeySpec = new SecretKeySpec(key, "AES");
            cipher.init(Cipher.DECRYPT_MODE, skeySpec, ivSpec);
            return cipher.doFinal(crypted);
        } catch (final NoSuchAlgorithmException e) {
            throw MyJDownloaderException.get(e);

        } catch (final NoSuchPaddingException e) {
            throw MyJDownloaderException.get(e);

        } catch (final InvalidKeyException e) {
            throw MyJDownloaderException.get(e);

        } catch (final InvalidAlgorithmParameterException e) {
            throw MyJDownloaderException.get(e);

        } catch (final IllegalBlockSizeException e) {
            throw MyJDownloaderException.get(e);

        } catch (final BadPaddingException e) {
            throw MyJDownloaderException.get(e);

        }
    }

    @Override
    protected byte[] encrypt(final byte[] data, final byte[] keyAndIV) throws MyJDownloaderException {
        try {
            final Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
            final byte[] iv = new byte[16];
            final byte[] key = new byte[16];
            System.arraycopy(keyAndIV, 0, iv, 0, 16);
            System.arraycopy(keyAndIV, 16, key, 0, 16);
            final IvParameterSpec ivSpec = new IvParameterSpec(iv);
            final SecretKeySpec skeySpec = new SecretKeySpec(key, "AES");
            cipher.init(Cipher.ENCRYPT_MODE, skeySpec, ivSpec);

            return cipher.doFinal(data);

        } catch (final NoSuchAlgorithmException e) {
            throw MyJDownloaderException.get(e);

        } catch (final NoSuchPaddingException e) {
            throw MyJDownloaderException.get(e);

        } catch (final InvalidKeyException e) {
            throw MyJDownloaderException.get(e);

        } catch (final InvalidAlgorithmParameterException e) {
            throw MyJDownloaderException.get(e);

        } catch (final IllegalBlockSizeException e) {
            throw MyJDownloaderException.get(e);

        } catch (final BadPaddingException e) {
            throw MyJDownloaderException.get(e);

        }
    }

    public AbstractMyJDDeviceClient getDeviceClient(final String deviceID) {
        synchronized (this.deviceClients) {
            AbstractMyJDDeviceClient client = this.deviceClients.get(deviceID);
            if (client == null) {
                client = this.createDeviceClient(deviceID);
                this.deviceClients.put(deviceID, client);
            }
            return client;
        }
    }

    @Override
    protected long getUniqueRID() {
        return AbstractMyJDClientForBasicJVM.RID_COUNTER.incrementAndGet();
    }

    /**
     * Calculates a HmacSHA256 of content with the key
     *
     * @param key
     * @param content
     * @return
     * @throws MyJDownloaderException
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeyException
     */
    @Override
    protected byte[] hmac(final byte[] key, final byte[] content) throws MyJDownloaderException {
        try {
            final Mac sha256_HMAC = Mac.getInstance("HmacSHA256");
            final SecretKeySpec secret_key = new SecretKeySpec(key, "HmacSHA256");
            sha256_HMAC.init(secret_key);
            return sha256_HMAC.doFinal(content);

        } catch (final NoSuchAlgorithmException e) {
            throw MyJDownloaderException.get(e);

        } catch (final InvalidKeyException e) {
            throw MyJDownloaderException.get(e);

        }
    }

    public <T extends Linkable> T link(final Class<T> class1, final String deviceID) {
        final ClientApiNameSpace ann = class1.getAnnotation(ClientApiNameSpace.class);
        if (ann == null) {
            throw new NullPointerException("ApiNameSpace missing in " + class1.getName());
        }

        return this.link(class1, ann.value(), deviceID);
    }

    @SuppressWarnings("unchecked")
    /**
     * Link an API INterface  and call methods directly
     * @param class1
     * @param namespace
     * @return
     */
    public <T extends Linkable> T link(final Class<T> class1, final String namespace, final String deviceID) {
        final AbstractMyJDDeviceClient finalClient = this.getDeviceClient(deviceID);
        return (T) Proxy.newProxyInstance(class1.getClassLoader(), new Class<?>[]{class1}, new InvocationHandler() {

            @Override
            public Object invoke(final Object proxy, final Method method, final Object[] args) throws Throwable {
                try {
                    final String action = "/" + namespace + "/" + method.getName();
                    final Type returnType = method.getGenericReturnType();
                    return finalClient.callAction(action, returnType, args);
                } catch (final Throwable e) {
                    final Class<?>[] exceptions = method.getExceptionTypes();
                    if (exceptions != null) {
                        for (final Class<?> c : exceptions) {
                            if (c.isAssignableFrom(e.getClass())) {
                                throw e;
                            }

                        }
                    }
                    throw new RuntimeException(e);
                }

            }

        });
    }

    @Override
    protected byte[] updateEncryptionToken(final byte[] oldSecret, final byte[] update) throws MyJDownloaderException {
        MessageDigest md;
        try {
            md = MessageDigest.getInstance("SHA-256");
            md.update(oldSecret);
            md.update(update);
            return md.digest();
        } catch (final NoSuchAlgorithmException e) {
            throw MyJDownloaderException.get(e);

        }
    }

}
