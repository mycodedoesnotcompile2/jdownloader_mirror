package jd.plugins;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.SizeFormatter;

public class MultiHostHost {
    /** How long shall we block this host if a limit gets reached? Until the next day/hour? */
    // public enum LimitResetMode {
    // DAILY;
    // }
    /** Why was this host blocked? Because of too many errored out tries via JD or because of the multihost/multihost limits? */
    public enum MultihosterHostStatus implements LabelInterface {
        WORKING {
            @Override
            public String getLabel() {
                return "Working";
            }
        },
        WORKING_UNSTABLE {
            @Override
            public String getLabel() {
                return "Unstable";
            }
        },
        DEACTIVATED_USER {
            @Override
            public String getLabel() {
                return "Deactivated by user";
            }
        },
        DEACTIVATED_JDOWNLOADER {
            @Override
            public String getLabel() {
                return "Temporarily deactivated by JD";
            }
        },
        DEACTIVATED_JDOWNLOADER_UNSUPPORTED {
            @Override
            public String getLabel() {
                return "JD doesn't support this host";
            }
        },
        DEACTIVATED_JDOWNLOADER_NOT_ALLOWED_BY_ORIGINAL_PLUGIN {
            @Override
            public String getLabel() {
                return "MOCH usage not allowed by original plugin";
            }
        },
        DEACTIVATED_MULTIHOST {
            @Override
            public String getLabel() {
                return "Flagged as not working by MOCH";
            }
        },
        DEACTIVATED_MULTIHOST_NOT_FOR_THIS_ACCOUNT_TYPE {
            @Override
            public String getLabel() {
                return "Cannot be used with your current account type";
            }
        };
    }

    private String                name                            = null;
    private List<String>          domains                         = new ArrayList<String>();
    private Boolean               isUnlimitedTraffic              = null;
    private Boolean               isUnlimitedLinks                = null;
    private long                  linksLeft                       = -1;
    private long                  linksMax                        = -1;
    private long                  trafficLeft                     = -1;
    private long                  trafficMax                      = -1;
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

    /** Sets domain. Overwrites previously set values! */
    protected void setDomain(final String domain) {
        if (domain == null) {
            throw new IllegalArgumentException();
        }
        this.domains.clear();
        this.domains.add(domain);
    }

    /** Sets domains. Overwrites previously set values! */
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

    public long getTrafficLeft() {
        return trafficLeft;
    }

    public void setTrafficLeft(long trafficLeft) {
        this.trafficLeft = trafficLeft;
        this.isUnlimitedTraffic = false;
    }

    public long getTrafficMax() {
        return trafficMax;
    }

    public void setTrafficMax(long bytes) {
        this.trafficMax = bytes;
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

    public void setUnlimitedLinks(Boolean param) {
        this.isUnlimitedLinks = param;
        if (param == null || param.equals(Boolean.TRUE)) {
            this.linksLeft = -1;
            this.linksMax = -1;
        }
    }

    public boolean isUnlimitedTraffic() {
        return isUnlimitedTraffic == null || isUnlimitedTraffic.booleanValue();
    }

    public void setUnlimitedTraffic(Boolean param) {
        this.isUnlimitedTraffic = param;
        if (param == null || param.equals(Boolean.TRUE)) {
            this.trafficLeft = -1;
            this.trafficMax = -1;
        }
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

    public void setErrorStatus(final String text, final long waitMillis) {
        this.setStatus(MultihosterHostStatus.DEACTIVATED_JDOWNLOADER);
        this.setStatusText(text);
        this.setUnavailableTime(waitMillis);
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

    /** Returns first domain of list of supported domains if list size is > 0. */
    public String getDomain() {
        if (this.getDomains().size() > 0) {
            return this.getDomains().get(0);
        } else {
            return null;
        }
    }

    public List<String> getDomains() {
        return this.domains;
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
