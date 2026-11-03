package org.jdownloader.controlling.packagizer;

import org.appwork.storage.Storable;
import org.jdownloader.controlling.Priority;
import org.jdownloader.controlling.filter.FilterRule;
import org.jdownloader.translate._JDT;

import jd.controlling.linkcollector.LinkCollector.MoveToDownloadlistMode;

public class PackagizerRule extends FilterRule implements Storable {
    // Fields sorted alphabetically by type, then by name
    /**
     * @deprecated Legacy flag, kept solely so old configs can still be deserialized and migrated to {@link #moveToDownloadlistMode} via
     *             {@link #_migrateLegacyFilters()}. Do not use for new code.
     */
    @Deprecated
    private Boolean  autoAddEnabled;
    private Boolean  autoExtractionEnabled;
    /**
     * @deprecated see {@link #autoAddEnabled}
     */
    @Deprecated
    private Boolean  autoForcedStartEnabled;
    /**
     * @deprecated see {@link #autoAddEnabled}
     */
    @Deprecated
    private Boolean  autoStartEnabled;
    private MoveToDownloadlistMode moveToDownloadlistMode = null;
    private boolean  stopAfterThisRule = false;
    private Boolean  linkEnabled;
    private int      chunks;
    private int      order             = 0;
    private Priority priority          = null;
    private String   comment;
    private String   downloadDestination;
    private String   filename;
    private String   moveto            = null;
    private String   packageKey        = null;
    private String   packageName       = null;
    private String   rename            = null;

    public PackagizerRule() {
        // required by Storable
    }

    public PackagizerRule duplicate() {
        final PackagizerRule ret = new PackagizerRule();
        ret.setEnabled(isEnabled());
        ret.setCreated(System.currentTimeMillis());
        ret.setIconKey(getIconKey());
        ret.setName(_JDT.T.LinkgrabberFilterRule_duplicate(getName()));
        ret.setMatchAlwaysFilter(getMatchAlwaysFilter());
        ret.setFilenameFilter(getFilenameFilter());
        ret.setPackagenameFilter(getPackagenameFilter());
        ret.setFilesizeFilter(getFilesizeFilter());
        ret.setFiletypeFilter(getFiletypeFilter());
        ret.setHosterURLFilter(getHosterURLFilter());
        ret.setSourceURLFilter(getSourceURLFilter());
        ret.setOriginFilter(getOriginFilter());
        ret.setLinkEnabledFilter(getLinkEnabledFilter());
        ret.setDownloadListDupeFilter(getDownloadListDupeFilter());
        ret.setLinkgrabberDupeFilter(getLinkgrabberDupeFilter());
        ret.setOnlineStatusFilter(getOnlineStatusFilter());
        ret.setPluginStatusFilter(getPluginStatusFilter());
        ret.setDownloadDestination(getDownloadDestination());
        ret.setPriority(getPriority());
        ret.setPackageName(getPackageName());
        ret.setFilename(getFilename());
        ret.setComment(getComment());
        ret.setChunks(getChunks());
        ret.setAutoExtractionEnabled(isAutoExtractionEnabled());
        ret.setMoveToDownloadlistMode(getMoveToDownloadlistMode());
        ret.setLinkEnabled(getLinkEnabled());
        ret.setMoveto(getMoveto());
        ret.setRename(getRename());
        ret.setTestUrl(getTestUrl());
        ret.setPackageKey(getPackageKey());
        ret.setStopAfterThisRule(isStopAfterThisRule());
        return ret;
    }

    public PackagizerRule init() {
        return this;
    }

    public PackagizerRuleWrapper compile() {
        return new PackagizerRuleWrapper(this);
    }

    // Getters and Setters (sorted alphabetically by field name)
    /**
     * @deprecated only used by Storable (JSON key "autoAddEnabled") and {@link #_migrateLegacyFilters()}. Returns the raw (possibly null)
     *             stored value on purpose.
     */
    @Deprecated
    public Boolean isAutoAddEnabled() {
        return autoAddEnabled;
    }

    /**
     * @deprecated only used by Storable and {@link #_migrateLegacyFilters()}.
     */
    @Deprecated
    public void setAutoAddEnabled(Boolean autoAddEnabled) {
        this.autoAddEnabled = autoAddEnabled;
    }

    public Boolean isAutoExtractionEnabled() {
        return autoExtractionEnabled;
    }

    public void setAutoExtractionEnabled(Boolean autoExtractionEnabled) {
        this.autoExtractionEnabled = autoExtractionEnabled;
    }

    /**
     * @deprecated only used by Storable (JSON key "autoForcedStartEnabled") and {@link #_migrateLegacyFilters()}. Returns the raw
     *             (possibly null) stored value on purpose.
     */
    @Deprecated
    public Boolean isAutoForcedStartEnabled() {
        return autoForcedStartEnabled;
    }

    /**
     * @deprecated only used by Storable and {@link #_migrateLegacyFilters()}.
     */
    @Deprecated
    public void setAutoForcedStartEnabled(Boolean autoForcedStartEnabled) {
        this.autoForcedStartEnabled = autoForcedStartEnabled;
    }

    /**
     * @deprecated only used by Storable (JSON key "autoStartEnabled") and {@link #_migrateLegacyFilters()}. Returns the raw (possibly
     *             null) stored value on purpose.
     */
    @Deprecated
    public Boolean isAutoStartEnabled() {
        return autoStartEnabled;
    }

    /**
     * @deprecated only used by Storable and {@link #_migrateLegacyFilters()}.
     */
    @Deprecated
    public void setAutoStartEnabled(Boolean autoStartEnabled) {
        this.autoStartEnabled = autoStartEnabled;
    }

    /**
     * Migrates the deprecated auto add / auto start / forced auto start flags into {@link #moveToDownloadlistMode}. Auto start and forced
     * auto start imply a confirm, because a download can only be started once the link is in the downloadlist. A legacy "disabled" value
     * has no effect on its own and results in no mode (null). Idempotent: after migration the legacy flags are cleared so subsequent calls
     * (and the next save) are no-ops.
     */
    @Override
    public void _migrateLegacyFilters() {
        super._migrateLegacyFilters();
        if (autoAddEnabled == null && autoStartEnabled == null && autoForcedStartEnabled == null) {
            return;
        }
        if (moveToDownloadlistMode == null) {
            if (Boolean.TRUE.equals(autoForcedStartEnabled)) {
                moveToDownloadlistMode = MoveToDownloadlistMode.CONFIRM_AND_START_FORCED;
            } else if (Boolean.TRUE.equals(autoStartEnabled)) {
                moveToDownloadlistMode = MoveToDownloadlistMode.CONFIRM_AND_START;
            } else if (Boolean.TRUE.equals(autoAddEnabled)) {
                moveToDownloadlistMode = MoveToDownloadlistMode.CONFIRM;
            }
        }
        autoAddEnabled = null;
        autoStartEnabled = null;
        autoForcedStartEnabled = null;
    }

    /**
     * @return null if this rule does not change how the link is moved to the downloadlist.
     */
    public MoveToDownloadlistMode getMoveToDownloadlistMode() {
        return moveToDownloadlistMode;
    }

    public void setMoveToDownloadlistMode(MoveToDownloadlistMode moveToDownloadlistMode) {
        this.moveToDownloadlistMode = moveToDownloadlistMode;
    }

    public int getChunks() {
        return chunks;
    }

    public void setChunks(int chunks) {
        this.chunks = chunks;
    }

    public String getComment() {
        return comment;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }

    public String getDownloadDestination() {
        return downloadDestination;
    }

    public void setDownloadDestination(String downloadDestination) {
        this.downloadDestination = downloadDestination;
    }

    public String getFilename() {
        return filename;
    }

    public void setFilename(String filename) {
        this.filename = filename;
    }

    public Boolean getLinkEnabled() {
        return linkEnabled;
    }

    public void setLinkEnabled(Boolean linkEnabled) {
        this.linkEnabled = linkEnabled;
    }

    public String getMoveto() {
        return moveto;
    }

    public void setMoveto(String moveto) {
        this.moveto = moveto;
    }

    public int getOrder() {
        return order;
    }

    public void setOrder(int order) {
        this.order = order;
    }

    public String getPackageKey() {
        return packageKey;
    }

    public void setPackageKey(String packageKey) {
        this.packageKey = packageKey;
    }

    public String getPackageName() {
        return packageName;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public Priority getPriority() {
        return priority;
    }

    public void setPriority(Priority priority) {
        this.priority = priority;
    }

    public String getRename() {
        return rename;
    }

    public void setRename(String rename) {
        this.rename = rename;
    }

    public boolean isStopAfterThisRule() {
        return stopAfterThisRule;
    }

    public void setStopAfterThisRule(boolean isStopAfterThisRule) {
        this.stopAfterThisRule = isStopAfterThisRule;
    }
}