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
package org.jdownloader.myjdownloader.client.bindings;

import org.jdownloader.myjdownloader.client.json.AbstractJsonData;

public abstract class AbstractQuery extends AbstractJsonData {

    private boolean bytesTotal = false;
    private boolean comment    = false;
    private boolean status     = false;
    private boolean priority   = false;

    public boolean isStatus() {
        return status;
    }

    public void setStatus(boolean status) {
        this.status = status;
    }

    private boolean enabled = false;

    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    private int    maxResults = -1;
    private int    startAt    = 0;
    private long[] packageUUIDs;

    public boolean isPriority() {
        return priority;
    }

    public void setPriority(boolean priority) {
        this.priority = priority;
    }

    public int getMaxResults() {
        return maxResults;
    }

    public int getStartAt() {
        return startAt;
    }

    public boolean isBytesTotal() {
        return bytesTotal;
    }

    public boolean isComment() {
        return comment;
    }

    public void setStartAt(final int startAt) {
        this.startAt = startAt;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setBytesTotal(final boolean size) {
        bytesTotal = size;
    }

    public void setComment(final boolean comment) {
        this.comment = comment;
    }

    public void setMaxResults(final int maxResults) {
        this.maxResults = maxResults;
    }

    public long[] getPackageUUIDs() {
        return packageUUIDs;
    }

    public void setPackageUUIDs(final long[] packageUUIDs) {
        this.packageUUIDs = packageUUIDs;
    }

}
