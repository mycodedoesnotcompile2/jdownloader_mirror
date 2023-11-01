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
package org.appwork.utils.net.ssl;

import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.Provider;
import java.security.Security;

/**
 * This SSL Trustprovider accepts all ssl certificates
 * 
 * @author $Author: unknown$
 * 
 */
public final class TrustProvider extends Provider {

    /**
     * 
     */
    private static final long serialVersionUID = 3537609645240163218L;

    private final static String PROVIDER_NAME = "AppWTrust";

    private static String origAlgorithm;

    /**
     * @return the {@link TrustProvider#providerName}
     * @see TrustProvider#providerName
     */
    public static String getProviderName() {
        return PROVIDER_NAME;
    }

    /**
     * @return the {@link TrustProvider#providerDescription}
     * @see TrustProvider#providerDescription
     */
    public static String getProviderDescription() {
        return PROVIDER_DESCRIPTION;
    }

    private final static String PROVIDER_DESCRIPTION = "AppWork TrustProvider";

    public TrustProvider() {
        super(PROVIDER_NAME, 0.1D, PROVIDER_DESCRIPTION);
        init();

    }

    private void init() {
        AccessController.doPrivileged(new PrivilegedAction<Object>() {
            public Object run() {
                put("TrustManagerFactory." + CryptServiceProvider.getAlgorithm(), CryptServiceProvider.class.getName());
                return null;
            }
        });
    }

    /**
     * Registers the Trustprovider
     */
    public static void register() {
        if (Security.getProvider(PROVIDER_NAME) == null) {
            // saves old status to be able to restore it
            Security.insertProviderAt(new TrustProvider(), 2);
            origAlgorithm = System.getProperty("ssl.TrustManagerFactory.algorithm");
            Security.setProperty("ssl.TrustManagerFactory.algorithm", CryptServiceProvider.getAlgorithm());
        }
    }

    /**
     * restores the old algorithm
     */
    public static void unRegister() {
        if (origAlgorithm != null) {
            Security.setProperty("ssl.TrustManagerFactory.algorithm", origAlgorithm);
            origAlgorithm = null;
            Security.removeProvider(getProviderName());

        }
    }

}
