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
package org.jdownloader.myjdownloader.client.bindings.interfaces;

import java.util.HashMap;
import java.util.List;

import org.jdownloader.myjdownloader.client.bindings.CleanupActionOptions;
import org.jdownloader.myjdownloader.client.bindings.ClientApiNameSpace;
import org.jdownloader.myjdownloader.client.bindings.PriorityStorable;
import org.jdownloader.myjdownloader.client.bindings.SkipReasonStorable;
import org.jdownloader.myjdownloader.client.bindings.UrlDisplayTypeStorable;
import org.jdownloader.myjdownloader.client.bindings.downloadlist.DownloadLinkQuery;
import org.jdownloader.myjdownloader.client.bindings.downloadlist.DownloadLinkStorable;
import org.jdownloader.myjdownloader.client.bindings.downloadlist.DownloadPackageQuery;
import org.jdownloader.myjdownloader.client.bindings.downloadlist.DownloadPackageStorable;

@ClientApiNameSpace("downloadsV2")
public interface DownloadsListInterface extends Linkable {
    
    void setEnabled(boolean enabled, long[] linkIds, long[] packageIds);
    
    void setStopMark(long linkId, long packageId);
    
    void removeStopMark();
    
    long getStopMark();
    
    int packageCount();
    
    List<DownloadPackageStorable> queryPackages(DownloadPackageQuery queryParams);
    
    void removeLinks(final long[] linkIds, final long[] packageIds);
    
    void renamePackage(Long packageId, String newName);
    
    void renameLink(Long linkId, String newName);
    
    void resetLinks(long[] linkIds, long[] packageIds);
    
    /**
     * Returns the new Counter if the counter does not equal oldCounterValue If the value changed, we should update the structure. Use this method to check
     * whether a structure update is required or not
     *
     * @param oldCounterValue
     * @return
     */
    long getStructureChangeCounter(long oldCounterValue);
    
    void movePackages(long[] packageIds, long afterDestPackageId);
    
    void moveLinks(long[] linkIds, long afterLinkID, long destPackageID);
    
    void setComment(long[] linkIds, long[] packageIds, boolean allPackageLinks, String comment);
    
    /**
     * Set the priority for the given link or package ids
     *
     * @param priority
     * @param linkIds
     * @param packageIds
     */
    void setPriority(PriorityStorable priority, long[] linkIds, long[] packageIds);
    
    /**
     * Query Packages links in downloads
     *
     * Example: http://localhost:3128/downloads/queryLinks?{"packageUUIDs":[ 1358496436106,1358496436107],"enabled":true,"size":true,"host":true
     * ,"startAt":0,"maxResults":10}
     *
     * Default fields returned: name, uuid, packageUUID
     *
     * @param queryParams
     *            Hashmap with the following allowed values:
     *
     *            Optional selectors: packageUUIDs, long[], links contained in the packages with given uuids are returned, if empty all links are returned
     *            startAt, Integer, index of first element to be returned maxResults, Integer, total number of elements to be returned
     *
     *            Optional fields (Boolean): host size done enabled
     *
     * @return
     */
    List<DownloadLinkStorable> queryLinks(DownloadLinkQuery queryParams);
    
    void resumeLinks(long[] linkIds, long[] pkgIds);
    
    void setDownloadDirectory(String directory, long[] pkgIds);
    
    DownloadLinkStorable getStopMarkedLink();
    
    void startOnlineStatusCheck(long[] linkIds, long[] packageIds);
    
    HashMap<String, Long[]> getDownloadUrls(long[] linkIds, long[] packageIds, UrlDisplayTypeStorable[] urlDisplayTypes);
    
    void movetoNewPackage(long[] linkIds, long[] pkgIds, String newPkgName, String downloadPath);
    
    void splitPackageByHoster(long[] linkIds, long[] pkgIds);
    
    void cleanup(final long[] linkIds, final long[] packageIds, final CleanupActionOptions.Action action, final CleanupActionOptions.Mode mode, final CleanupActionOptions.SelectionType selectionType);
    
    boolean setDownloadPassword(long[] linkIds, long[] packageIds, String pass);
    
    boolean forceDownload(long[] linkIds, long[] packageIds);
    
    boolean unskip(long[] packageIds, long[] linkIds, SkipReasonStorable.Reason filterByReason);
}
