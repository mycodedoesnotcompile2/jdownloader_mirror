package jd.plugins;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.appwork.utils.Time;
import org.appwork.utils.formatter.SizeFormatter;

public class MultiHostHost {
    /** How long shall we block this host if a limit gets reached? Until the next day/hour? */
    // public enum LimitResetMode {
    // DAILY;
    // }
    /** Why was this host blocked? Because of too many errored out tries via JD or because of the multihost/multihost limits? */
    public enum MultihosterHostStatus {
        WORKING,
        WORKING_UNSTABLE,
        DEACTIVATED_JDOWNLOADER,
        DEACTIVATED_JDOWNLOADER_UNSUPPORTED,
        DEACTIVATED_JDOWNLOADER_NOT_ALLOWED_BY_ORIGINAL_PLUGIN,
        DEACTIVATED_MULTIHOST,
        DEACTIVATED_MULTIHOST_NOT_FOR_THIS_ACCOUNT_TYPE,
        DEACTIVATED_MULTIHOST_LIMIT_REACHED;
    }

    private String                name                            = null;
    private List<String>          domains                         = new ArrayList<String>();
    private Boolean               isUnlimitedTraffic              = null;
    private Boolean               isUnlimitedLinks                = null;
    private long                  linksLeft                       = -1;
    private long                  linksMax                        = -1;
    private long                  trafficLeft                     = -1;
    private long                  trafficMax                      = -1;
    private String                unavailableMessage              = null;
    private long                  unavailableUntilTimestamp       = -1;
    private Short                 trafficCalculationFactorPercent = null;
    private int                   maxChunks                       = 0;
    private int                   maxDownloads                    = -1;
    private Boolean               resume                          = null;
    private String                statusText                      = null;
    private MultihosterHostStatus status                          = null;
    private static final long     MAX_UNAVAILABLE_TIME            = 5 * 60 * 1000;

    public MultiHostHost() {
    }

    public MultiHostHost(final String domain) {
        this.name = domain;
        this.setDomain(domain);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    protected void setDomain(final String domain) {
        if (domain == null) {
            throw new IllegalArgumentException();
        }
        this.domains.clear();
        this.domains.add(domain);
    }

    public void addDomain(final String domain) {
        if (domain == null) {
            throw new IllegalArgumentException();
        }
        if (!this.domains.contains(domain)) {
            this.domains.add(domain);
        }
    }

    public void addDomains(final List<String> domains) {
        if (domains == null) {
            throw new IllegalArgumentException();
        }
        for (final String domain : domains) {
            this.addDomain(domain);
        }
    }

    public void setDomains(final List<String> domains) {
        if (domains == null) {
            throw new IllegalArgumentException();
        }
        this.domains = domains;
    }

    public long getLinksLeft() {
        return linksLeft;
    }

    public void setLinksLeft(long num) {
        this.linksLeft = num;
        this.isUnlimitedLinks = false;
    }

    public long getLinksMax() {
        return linksMax;
    }

    public void setLinksMax(long num) {
        this.linksMax = num;
        this.isUnlimitedLinks = false;
    }

    /** Only use this when linksMax is given!! */
    public void setLinksUsed(long num) {
        this.linksLeft = this.linksMax - num;
        this.isUnlimitedLinks = false;
    }

    public long getTrafficLeft() {
        return trafficLeft;
    }

    public void setTrafficLeft(long trafficLeft) {
        // TODO: review this
        this.trafficLeft = trafficLeft;
        this.isUnlimitedTraffic = false;
    }

    public long getTrafficMax() {
        return trafficMax;
    }

    public void setTrafficMax(long bytes) {
        // TODO: review this
        this.trafficMax = bytes;
        this.isUnlimitedTraffic = false;
    }

    /** Only use this if trafficMax has been set before!! */
    public void setTrafficUsed(long bytes) {
        // TODO: review this
        this.trafficLeft = this.trafficMax - bytes;
        this.isUnlimitedTraffic = false;
    }

    /**
     * How much traffic is needed- and credited from the account when downloading from this host? </br>
     * 500 = 5 times the size of the downloaded file.
     */
    public short getTrafficCalculationFactorPercent() {
        if (trafficCalculationFactorPercent == null) {
            return 100;
        } else {
            return trafficCalculationFactorPercent;
        }
    }

    public void setTrafficCalculationFactorPercent(short num) {
        this.trafficCalculationFactorPercent = num;
    }

    public boolean isUnlimitedLinks() {
        return isUnlimitedLinks == null || isUnlimitedLinks.booleanValue();
    }

    public boolean isUnlimitedTraffic() {
        return isUnlimitedTraffic == null || isUnlimitedTraffic.booleanValue();
    }

    @Deprecated
    public boolean canDownload(final DownloadLink link) {
        if (isUnlimitedTraffic || isUnlimitedLinks) {
            return true;
        } else if (this.linksLeft <= 0) {
            return false;
        } else if (this.trafficLeft <= 0) {
            return false;
        } else if (link.getView().getBytesTotal() != -1 && this.trafficLeft < link.getView().getBytesTotal()) {
            /* Not enough traffic to download this link */
            return false;
        } else {
            return true;
        }
    }

    /** Returns custom set status text. Typically used to describe why this host is currently not working. */
    public String getStatusText() {
        return statusText;
    }

    public void setStatusText(String statusText) {
        this.statusText = statusText;
    }

    /** Returns title for this entry which is either its' name, the first entry in the domain list or null. */
    public String getTitle() {
        if (this.name != null) {
            return this.name;
        } else if (this.domains != null && this.domains.size() > 0) {
            return this.domains.get(0);
        } else {
            return null;
        }
    }

    public MultihosterHostStatus getStatus() {
        // TODO: Update this to simply return status without any evaluation
        if (this.unavailableUntilTimestamp > Time.systemIndependentCurrentJVMTimeMillis()) {
            return MultihosterHostStatus.DEACTIVATED_JDOWNLOADER;
        } else if (status == null) {
            /* Default */
            return MultihosterHostStatus.WORKING;
        } else {
            return status;
        }
    }

    public void setStatus(MultihosterHostStatus status) {
        this.status = status;
    }

    public int getMaxChunks() {
        return maxChunks;
    }

    public void setMaxChunks(int maxChunks) {
        this.maxChunks = maxChunks;
    }

    public boolean isResume() {
        if (resume == null) {
            return true;
        } else {
            return resume;
        }
    }

    public void setResume(boolean resume) {
        this.resume = resume;
    }

    public boolean supportsDomain(String domain) {
        if (domain == null) {
            return false;
        }
        domain = domain.toLowerCase(Locale.ENGLISH);
        if (this.domains.contains(domain)) {
            return true;
        } else {
            return false;
        }
    }

    public List<String> getDomains() {
        return this.domains;
    }

    public String getUnavailableMessage() {
        return unavailableMessage;
    }

    public void setUnavailableMessage(String unavailableMessage) {
        this.unavailableMessage = unavailableMessage;
    }

    public long getUnavailableUntilTimestamp() {
        return unavailableUntilTimestamp;
    }

    public void setUnavailableTime(long milliseconds) {
        milliseconds = Math.min(milliseconds, MAX_UNAVAILABLE_TIME);
        this.unavailableUntilTimestamp = Time.systemIndependentCurrentJVMTimeMillis() + milliseconds;
    }

    public int getMaxDownloads() {
        return maxDownloads;
    }

    public void setMaxDownloads(int maxDownloads) {
        this.maxDownloads = maxDownloads;
    }

    @Override
    public String toString() {
        final String title = getTitle();
        return title + " | Status: " + this.getStatus() + " | StatusText: " + this.getStatusText() + " | LinksAvailable: " + this.getLinksLeft() + "/" + this.getLinksMax() + " | Traffic: " + SizeFormatter.formatBytes(this.getTrafficLeft()) + "/" + SizeFormatter.formatBytes(this.getTrafficMax()) + " | Chunks: " + this.getMaxChunks() + " | Resume: " + this.isResume();
    }
}
