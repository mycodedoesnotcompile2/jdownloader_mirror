package org.jdownloader.controlling.packagizer;

import org.appwork.storage.Storable;
import org.jdownloader.controlling.Priority;
import org.jdownloader.controlling.filter.FilterRule;
import org.jdownloader.translate._JDT;

public class PackagizerRule extends FilterRule implements Storable {
    // Fields sorted alphabetically by type, then by name
    private Boolean  autoAddEnabled;
    private Boolean  autoExtractionEnabled;
    private Boolean  autoForcedStartEnabled;
    private Boolean  autoStartEnabled;
    private boolean  isStopAfterThisRule = false;
    private Boolean  linkEnabled;
    private int      chunks;
    private int      order               = 0;
    private Priority priority            = null;
    private String   comment;
    private String   downloadDestination;
    private String   filename;
    private String   moveto              = null;
    private String   packageKey          = null;
    private String   packageName         = null;
    private String   rename              = null;

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
        ret.setConditionFilter(getConditionFilter());
        ret.setOnlineStatusFilter(getOnlineStatusFilter());
        ret.setPluginStatusFilter(getPluginStatusFilter());
        ret.setDownloadDestination(getDownloadDestination());
        ret.setPriority(getPriority());
        ret.setPackageName(getPackageName());
        ret.setFilename(getFilename());
        ret.setComment(getComment());
        ret.setChunks(getChunks());
        ret.setAutoExtractionEnabled(isAutoExtractionEnabled());
        ret.setAutoAddEnabled(isAutoAddEnabled());
        ret.setAutoStartEnabled(isAutoStartEnabled());
        ret.setAutoForcedStartEnabled(isAutoForcedStartEnabled());
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
    public Boolean isAutoAddEnabled() {
        return autoAddEnabled;
    }

    public void setAutoAddEnabled(Boolean autoAddEnabled) {
        this.autoAddEnabled = autoAddEnabled;
    }

    public Boolean isAutoExtractionEnabled() {
        return autoExtractionEnabled;
    }

    public void setAutoExtractionEnabled(Boolean autoExtractionEnabled) {
        this.autoExtractionEnabled = autoExtractionEnabled;
    }

    public Boolean isAutoForcedStartEnabled() {
        return autoForcedStartEnabled;
    }

    public void setAutoForcedStartEnabled(Boolean autoForcedStartEnabled) {
        this.autoForcedStartEnabled = autoForcedStartEnabled;
    }

    public Boolean isAutoStartEnabled() {
        return autoStartEnabled;
    }

    public void setAutoStartEnabled(Boolean autoStartEnabled) {
        this.autoStartEnabled = autoStartEnabled;
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
        return isStopAfterThisRule;
    }

    public void setStopAfterThisRule(boolean isStopAfterThisRule) {
        this.isStopAfterThisRule = isStopAfterThisRule;
    }
}