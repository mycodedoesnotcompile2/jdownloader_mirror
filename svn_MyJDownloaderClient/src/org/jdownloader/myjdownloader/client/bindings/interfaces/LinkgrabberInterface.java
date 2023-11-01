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

import org.jdownloader.myjdownloader.client.bindings.AddLinksQuery;
import org.jdownloader.myjdownloader.client.bindings.CleanupActionOptions.Action;
import org.jdownloader.myjdownloader.client.bindings.CleanupActionOptions.Mode;
import org.jdownloader.myjdownloader.client.bindings.CleanupActionOptions.SelectionType;
import org.jdownloader.myjdownloader.client.bindings.ClientApiNameSpace;
import org.jdownloader.myjdownloader.client.bindings.JobLinkCrawlerStorable;
import org.jdownloader.myjdownloader.client.bindings.LinkCollectingJobStorable;
import org.jdownloader.myjdownloader.client.bindings.LinkCrawlerJobQuery;
import org.jdownloader.myjdownloader.client.bindings.LinkVariantStorable;
import org.jdownloader.myjdownloader.client.bindings.PriorityStorable;
import org.jdownloader.myjdownloader.client.bindings.UrlDisplayTypeStorable;
import org.jdownloader.myjdownloader.client.bindings.linkgrabber.CrawledLinkQuery;
import org.jdownloader.myjdownloader.client.bindings.linkgrabber.CrawledLinkStorable;
import org.jdownloader.myjdownloader.client.bindings.linkgrabber.CrawledPackageQuery;
import org.jdownloader.myjdownloader.client.bindings.linkgrabber.CrawledPackageStorable;

@ClientApiNameSpace("linkgrabberv2")
public interface LinkgrabberInterface extends Linkable {

    CrawledPackageStorable[] queryPackages(CrawledPackageQuery queryParams);

    CrawledLinkStorable[] queryLinks(CrawledLinkQuery queryParams);

    void moveToDownloadlist(long[] linkIds, long[] packageIds);

    void removeLinks(long[] linkIds, long[] packageIds);

    void setEnabled(boolean enabled, long[] linkIds, long[] packageIds);

    void renameLink(long linkId, String newName);

    void renamePackage(long packageId, String newName);

    long getChildrenChanged(long structureWatermark);

    String[] getDownloadFolderHistorySelectionBase();

    int getPackageCount();

    void movePackages(long[] packageIds, long afterDestPackageId);

    void moveLinks(long[] linkIds, long afterLinkID, long destPackageID);

    LinkCollectingJobStorable addLinks(AddLinksQuery query);

    LinkCollectingJobStorable addContainer(String type, String content);

    LinkVariantStorable[] getVariants(long linkid);

    void setVariant(long linkid, String variantID);

    void addVariantCopy(long linkid, long destinationAfterLinkID, long destinationPackageID, String variantID);

    /**
     * Set the priority for the given link or package ids
     *
     * @param priority
     * @param linkIds
     * @param packageIds
     */
    void setPriority(PriorityStorable priority, long[] linkIds, long[] packageIds);

    void startOnlineStatusCheck(long[] linkIds, long[] packageIds);

    HashMap<String, Long[]> getDownloadUrls(long[] linkIds, long[] packageIds, UrlDisplayTypeStorable[] urlDisplayTypes);

    void movetoNewPackage(long[] linkIds, long[] pkgIds, String newPkgName, String downloadPath);

    void splitPackageByHoster(long[] linkIds, long[] pkgIds);

    void cleanup(long[] linkIds, long[] packageIds, Action action, Mode mode, SelectionType selectionType);

    void setDownloadDirectory(String directory, long[] packageIds);

    boolean clearList();

    boolean setDownloadPassword(long[] linkIds, long[] packageIds, String pass);

    boolean abort();

    boolean isCollecting();

    boolean abort(long jobId);

    List<JobLinkCrawlerStorable> queryLinkCrawlerJobs(final LinkCrawlerJobQuery query);

    void setComment(long[] linkIds, long[] packageIds, boolean allPackageLinks, String comment);
}
