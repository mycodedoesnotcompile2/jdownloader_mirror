package jd.controlling.linkcollector;

import org.jdownloader.translate._JDT;

import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.linkcrawler.CrawledLink;

/**
 * @deprecated Superseded by {@code LinkEnabledFilter} and {@code DownloadListDupeFilter}. Kept only so old configs
 *             referencing these flags can be deserialized and migrated (see {@code FilterRule#_migrateLegacyFilters()}).
 */
@Deprecated
public enum VariousCrawledLinkFlags implements MatchesInterface<CrawledLink> {
    DOWNLOAD_LIST_DUPE(_JDT.T.DOWNLOAD_LIST_DUPE()) {
        public boolean matches(CrawledLink link) {
            return link != null && DownloadController.getInstance().hasDownloadLinkByID(link.getLinkID());
        };
    },
    IS_ENABLED(_JDT.T.LinkFilterSettings_condition_link_enabled()) {
        @Override
        public boolean matches(CrawledLink link) {
            return link != null && link.isEnabled();
        }
    };

    private final String translation;

    private VariousCrawledLinkFlags(String translation) {
        this.translation = translation;
    }

    public String getTranslation() {
        return translation;
    }
}
