/**
 * 
 * ====================================================================================================================================================
 *         "My JDownloader Client" License
 *         The "My JDownloader Client" will be called [The Product] from now on.
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
package org.jdownloader.myjdownloader.client.bindings.events.json;

/**
 * Copyright (c) 2009 - 2013 AppWork UG(haftungsbeschränkt) <e-mail@appwork.org>
 * 
 * This file is part of org.appwork.remoteapi.events.json
 * 
 * This software is licensed under the Artistic License 2.0,
 * see the LICENSE file or http://www.opensource.org/licenses/artistic-license-2.0.php
 * for details
 */

/**
 * @author daniel
 * 
 */
public class SubscriptionResponse {

    protected long     subscriptionid = -1;

    protected boolean  subscribed     = false;

    protected String[] subscriptions  = null;
    protected String[] exclusions     = null;

    protected long     maxPolltimeout = -1;
    protected long     maxKeepalive   = -1;

    public SubscriptionResponse(/* Storable */) {
    }

    public String[] getExclusions() {
        return this.exclusions;
    }

    public long getMaxKeepalive() {
        return this.maxKeepalive;
    }

    /**
     * @return the maxPolltimeout
     */
    public long getMaxPolltimeout() {
        return this.maxPolltimeout;
    }

    /**
     * @return the subscriptionid
     */
    public long getSubscriptionid() {
        return this.subscriptionid;
    }

    /**
     * @return the subscriptions
     */
    public String[] getSubscriptions() {
        return this.subscriptions;
    }

    /**
     * @return the subscribed
     */
    public boolean isSubscribed() {
        return this.subscribed;
    }

    public void setExclusions(final String[] exclusions) {
        this.exclusions = exclusions;
    }

    public void setMaxKeepalive(final long maxKeepalive) {
        this.maxKeepalive = maxKeepalive;
    }

    /**
     * @param maxPolltimeout
     *            the maxPolltimeout to set
     */
    public void setMaxPolltimeout(final long maxPolltimeout) {
        this.maxPolltimeout = maxPolltimeout;
    }

    /**
     * @param subscribed
     *            the subscribed to set
     */
    public void setSubscribed(final boolean subscribed) {
        this.subscribed = subscribed;
    }

    /**
     * @param subscriptionid
     *            the subscriptionid to set
     */
    public void setSubscriptionid(final long subscriptionid) {
        this.subscriptionid = subscriptionid;
    }

    /**
     * @param subscriptions
     *            the subscriptions to set
     */
    public void setSubscriptions(final String[] subscriptions) {
        this.subscriptions = subscriptions;
    }
}
