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

public class AddLinksQuery extends AbstractJsonData {
    public AddLinksQuery(/* storable */) {
    }

    /**
     * @param autostart
     * @param links
     * @param packageName
     * @param extractPassword
     * @param downloadPassword
     * @param destinationFolder
     */
    public AddLinksQuery(final Boolean autostart, final String links, final String packageName, final String extractPassword, final String downloadPassword, final String destinationFolder) {
        super();
        this.autostart = autostart;
        this.links = links;
        this.packageName = packageName;
        this.extractPassword = extractPassword;
        this.downloadPassword = downloadPassword;
        this.destinationFolder = destinationFolder;
    }

    private Boolean  autostart                = null;
    private Boolean  deepDecrypt              = null;
    private Boolean  autoExtract              = null;
    private Boolean  overwritePackagizerRules = null;
    private String   links                    = null;
    private String[] dataURLs                 = null;
    private String   packageName              = null;
    private String   extractPassword          = null;
    private String   sourceUrl                = null;
    private String   downloadPassword         = null;
    private String   destinationFolder        = null;
    private String   comment                  = null;
    private Boolean  assignJobID              = null;

    public Boolean isAssignJobID() {
        return this.assignJobID;
    }

    public void setAssignJobID(final Boolean assignJobID) {
        this.assignJobID = assignJobID;
    }

    public Boolean isAutostart() {
        return this.autostart;
    }

    public void setAutostart(final Boolean autostart) {
        this.autostart = autostart;
    }

    public String getLinks() {
        return this.links;
    }

    public void setLinks(final String links) {
        this.links = links;
    }

    public String getPackageName() {
        return this.packageName;
    }

    public void setPackageName(final String packageName) {
        this.packageName = packageName;
    }

    public String getExtractPassword() {
        return this.extractPassword;
    }

    public void setExtractPassword(final String extractPassword) {
        this.extractPassword = extractPassword;
    }

    public String getDownloadPassword() {
        return this.downloadPassword;
    }

    public void setDownloadPassword(final String downloadPassword) {
        this.downloadPassword = downloadPassword;
    }

    public String getDestinationFolder() {
        return this.destinationFolder;
    }

    public void setDestinationFolder(final String destinationFolder) {
        this.destinationFolder = destinationFolder;
    }

    private PriorityStorable priority = PriorityStorable.DEFAULT;

    public PriorityStorable getPriority() {
        return this.priority;
    }

    public void setPriority(final PriorityStorable priority) {
        this.priority = priority;
    }

    public String getSourceUrl() {
        return this.sourceUrl;
    }

    public void setSourceUrl(final String sourceUrl) {
        this.sourceUrl = sourceUrl;
    }

    public Boolean isAutoExtract() {
        return this.autoExtract;
    }

    public void setAutoExtract(final Boolean autoExtract) {
        this.autoExtract = autoExtract;
    }

    public Boolean isDeepDecrypt() {
        return this.deepDecrypt;
    }

    public void setDeepDecrypt(final Boolean deepDecrypt) {
        this.deepDecrypt = deepDecrypt;
    }

    public String[] getDataURLs() {
        return this.dataURLs;
    }

    public void setDataURLs(final String[] dataURLs) {
        this.dataURLs = dataURLs;
    }

    public Boolean isOverwritePackagizerRules() {
        return this.overwritePackagizerRules;
    }

    public void setOverwritePackagizerRules(final Boolean overwritePackagizerRules) {
        this.overwritePackagizerRules = overwritePackagizerRules;
    }

    public String getComment() {
        return comment;
    }

    public void setComment(final String comment) {
        this.comment = comment;
    }
}