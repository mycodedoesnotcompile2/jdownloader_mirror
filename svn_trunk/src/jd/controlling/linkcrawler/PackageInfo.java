package jd.controlling.linkcrawler;

import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import org.appwork.utils.StringUtils;
import org.jdownloader.controlling.UniqueAlltimeID;
import org.jdownloader.controlling.packagizer.PackagizerController;
import org.jdownloader.controlling.packagizer.PackagizerRuleWrapper;

import jd.controlling.linkcollector.LinknameCleaner;

public class PackageInfo {
    private UniqueAlltimeID uniqueId               = null;
    private Set<String>     matchedPackagizerRules = new CopyOnWriteArraySet<String>();
    private Boolean         ignoreVarious          = null;
    private Boolean         allowInheritance       = null;
    private String          packageKey             = null;
    private String          comment                = null;

    public String getComment() {
        return comment;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }

    public String getPackageKey() {
        return packageKey;
    }

    public boolean isPackagenameSubfolderOnly() {
        return getDestinationFolderRoot() == null && PackagizerController.PACKAGETAG.equals(getDestinationFolder());
    }

    public boolean isPackageSet() {
        return StringUtils.isNotEmpty(getName()) || StringUtils.isNotEmpty(getPackageKey());
    }

    public boolean isIgnoreSpecialPackages() {
        return Boolean.TRUE.equals(isIgnoreVarious()) || isPackageSet() || (isPackagizerRuleMatched() && !isPackagenameSubfolderOnly());
    }

    public void setPackageKey(String packageKey) {
        this.packageKey = packageKey;
    }

    public Boolean isAllowInheritance() {
        return allowInheritance;
    }

    public void setAllowInheritance(Boolean allowInheritance) {
        this.allowInheritance = allowInheritance;
    }

    public UniqueAlltimeID getUniqueId() {
        return uniqueId;
    }

    public void setUniqueId(UniqueAlltimeID uniqueId) {
        this.uniqueId = uniqueId;
    }

    public String getName() {
        return name;
    }

    public static void setName(final CrawledLink link, final String name) {
        PackageInfo dpi = link.getDesiredPackageInfo();
        if (dpi == null) {
            dpi = new PackageInfo();
        }
        dpi.setName(name);
        link.setDesiredPackageInfo(dpi);
    }

    public PackageInfo getCopy() {
        final PackageInfo ret = new PackageInfo();
        ret.name = this.name;// avoid cleanPackagename
        ret.setDestinationFolder(getDestinationFolder());
        ret.setDestinationFolderRoot(getDestinationFolderRoot());
        ret.setIgnoreVarious(isIgnoreVarious());
        ret.matchedPackagizerRules = new CopyOnWriteArraySet<>(getMatchingPackagizerRules());
        ret.setUniqueId(getUniqueId());
        ret.setAllowInheritance(isAllowInheritance());
        ret.setPackageKey(getPackageKey());
        ret.setComment(getComment());
        return ret;
    }

    public void setName(String name) {
        name = LinknameCleaner.cleanPackagename(name, true);
        if (StringUtils.isEmpty(name)) {
            this.name = null;
        } else {
            this.name = name;
        }
    }

    public String getDestinationFolder() {
        return destinationFolder;
    }

    public void setDestinationFolder(String destinationFolder) {
        if (StringUtils.isEmpty(destinationFolder)) {
            this.destinationFolder = null;
        } else {
            this.destinationFolder = destinationFolder;
        }
    }

    public String getDestinationFolderRoot() {
        return destinationFolderRoot;
    }

    public void setDestinationFolderRoot(String destinationFolderRoot) {
        if (StringUtils.isEmpty(destinationFolderRoot)) {
            this.destinationFolderRoot = null;
        } else {
            this.destinationFolderRoot = destinationFolderRoot;
        }
    }

    private String name                  = null;
    private String destinationFolder     = null;
    private String destinationFolderRoot = null;

    /**
     * @return the packagizerRuleMatched
     */
    public boolean isPackagizerRuleMatched() {
        return !matchedPackagizerRules.isEmpty();
    }

    /**
     * @param packagizerRuleMatched
     *            the packagizerRuleMatched to set
     */
    public void addMatchingPackagizerRule(PackagizerRuleWrapper rule) {
        matchedPackagizerRules.add(rule.getRule().getId());
    }

    public Set<String> getMatchingPackagizerRules() {
        return matchedPackagizerRules;
    }

    /**
     * @return the ignoreVarious
     */
    public Boolean isIgnoreVarious() {
        return ignoreVarious;
    }

    /**
     * @param ignoreVarious
     *            the ignoreVarious to set
     */
    public void setIgnoreVarious(Boolean ignoreVarious) {
        this.ignoreVarious = ignoreVarious;
    }

    public boolean isEmpty() {
        return !isNotEmpty();
    }

    public boolean isNotEmpty() {
        return ignoreVarious != null || packageKey != null || uniqueId != null || destinationFolder != null || destinationFolderRoot != null || name != null || isPackagizerRuleMatched();
    }
}
