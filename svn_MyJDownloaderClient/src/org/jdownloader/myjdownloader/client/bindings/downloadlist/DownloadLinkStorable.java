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
package org.jdownloader.myjdownloader.client.bindings.downloadlist;

import org.jdownloader.myjdownloader.client.bindings.AbstractLinkStorable;

public class DownloadLinkStorable extends AbstractLinkStorable {

    private long bytesLoaded = -1;

    private long eta         = -1;
    
    public long getFinishedDate() {
        return this.finishedDate;
    }
    
    public void setFinishedDate(long finishedDate) {
        this.finishedDate = finishedDate;
    }
    
    private long    finishedDate     = -1;

    private String  extractionStatus = null;

    private boolean finished         = false;
    private boolean running          = false;
    private String  status           = null;

    public String getStatus() {
        return this.status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    private String statusIconKey = null;

    public String getStatusIconKey() {
        return this.statusIconKey;
    }

    public void setStatusIconKey(String statusIconKey) {
        this.statusIconKey = statusIconKey;
    }

    private boolean skipped = false;
    private long    speed   = -1;

    public DownloadLinkStorable(/* Storable */) {

    }

    public long getBytesLoaded() {
        return this.bytesLoaded;
    }

    public long getEta() {
        return this.eta;
    }

    public String getExtractionStatus() {
        return this.extractionStatus;
    }

    public long getSpeed() {
        return this.speed;
    }

    public boolean isFinished() {
        return this.finished;
    }

    public boolean isRunning() {
        return this.running;
    }

    public boolean isSkipped() {
        return this.skipped;
    }

    public void setBytesLoaded(final long bytesLoaded) {
        this.bytesLoaded = bytesLoaded;
    }

    public void setEta(final long eta) {
        this.eta = eta;
    }

    public void setExtractionStatus(final String extractionStatus) {
        this.extractionStatus = extractionStatus;
    }

    public void setFinished(final boolean finished) {
        this.finished = finished;
    }

    public void setRunning(final boolean running) {
        this.running = running;
    }

    public void setSkipped(final boolean skipped) {
        this.skipped = skipped;
    }

    public void setSpeed(final long speed) {
        this.speed = speed;
    }

}
