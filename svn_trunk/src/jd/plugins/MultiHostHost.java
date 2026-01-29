package jd.plugins;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicBoolean;

import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.packagecontroller.AbstractNode;

import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.SizeFormatter;
import org.jdownloader.DomainInfo;
import org.jdownloader.controlling.download.DownloadControllerListener;
import org.jdownloader.gui.translate._GUI;

public class MultiHostHost implements DownloadControllerListener {
    public enum MultihosterHostStatus implements LabelInterface {
        WORKING {
            @Override
            public String getLabel() {
                return _GUI.T.multihost_single_host_object_status_working();
            }
        },
        WORKING_UNSTABLE {
            @Override
            public String getLabel() {
                return _GUI.T.multihost_single_host_object_status_working_unstable();
            }
        },
        DEACTIVATED_JDOWNLOADER {
            @Override
            public String getLabel() {
                return _GUI.T.multihost_single_host_object_status_deactivated_jdownloader();
            }
        },
        DEACTIVATED_JDOWNLOADER_UNSUPPORTED {
            @Override
            public String getLabel() {
                return _GUI.T.multihost_single_host_object_status_deactivated_jdownloader_unsupported();
            }
        },
        DEACTIVATED_JDOWNLOADER_NOT_ALLOWED_BY_ORIGINAL_PLUGIN {
            @Override
            public String getLabel() {
                return _GUI.T.multihost_single_host_object_status_deactivated_jdownloader_not_allowed_by_original_plugin();
            }
        },
        DEACTIVATED_MULTIHOST {
            @Override
            public String getLabel() {
                return _GUI.T.multihost_single_host_object_status_deactivated_multihost();
            }
        },
        DEACTIVATED_MULTIHOST_NOT_FOR_THIS_ACCOUNT_TYPE {
            @Override
            public String getLabel() {
                return _GUI.T.multihost_single_host_object_status_deactivated_multihost_not_for_this_account_type();
            }
        };
    }

    protected Boolean               enabled                         = null;
    protected String                name                            = null;
    protected List<String>          domains                         = new ArrayList<String>();
    protected Boolean               isUnlimitedTraffic              = null;
    protected Boolean               isUnlimitedLinks                = null;
    protected Long                  linksLeft                       = null;
    protected Long                  linksMax                        = null;
    protected Long                  trafficLeft                     = null;
    protected Long                  trafficMax                      = null;
    protected String                unavailableStatusText           = null;
    protected Long                  unavailableUntilTimestamp       = null;
    protected Short                 trafficCalculationFactorPercent = null;
    protected Integer               maxChunks                       = null;
    protected Integer               maxDownloads                    = null;
    protected Boolean               resume                          = null;
    protected String                statusText                      = null;
    protected MultihosterHostStatus status                          = null;
    private static final long       MAX_UNAVAILABLE_TIME            = 5 * 60 * 1000;
    protected AccountInfo           accountInfo                     = null;
    protected final AtomicBoolean   propertyListenerEnabled         = new AtomicBoolean(false);
    protected long                  createdTimestamp                = System.currentTimeMillis();

    public AccountInfo getAccountInfo() {
        return accountInfo;
    }

    protected void setAccountInfo(AccountInfo accountInfo) {
        this.accountInfo = accountInfo;
    }

    public MultiHostHost() {
    }

    public MultiHostHost(final String domain) {
        this.name = domain;
        this.setDomain(domain);
    }

    private boolean isEnabled(final Boolean booleanValue) {
        return booleanValue == null || booleanValue.booleanValue();
    }

    private String getEnabledProperty() {
        return "multihost_" + getDomain() + "_enabled";
    }

    public boolean isEnabled() {
        final Account ac = getAccount();
        final boolean ret = ac == null ? isEnabled(enabled) : ac.getBooleanProperty(getEnabledProperty(), isEnabled(enabled));
        return ret;
    }

    protected Account getAccount() {
        final AccountInfo ai = getAccountInfo();
        if (ai == null) {
            return null;
        }
        return ai.getAccount();
    }

    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
        final Account ac = getAccount();
        if (ac != null) {
            ac.setProperty(getEnabledProperty(), enabled);
        }
        if (enabled) {
            clearErrorStatus();
        }
    }

    /**
     * Returns the name of this item e.g. "Rapidgator". If no name was explicitly set, this will return the first domain in the list of
     * domains.
     */
    public String getName() {
        final String name = this.name;
        if (name != null) {
            return name;
        } else {
            return this.getDomain();
        }
    }

    public void setName(String name) {
        this.name = name;
    }

    public void addDomain(final String domain) {
        if (domain == null) {
            throw new IllegalArgumentException();
        }
        final List<String> domains = getDomains();
        if (!domains.contains(domain)) {
            domains.add(domain);
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
    public void setDomain(final String domain) {
        if (domain == null) {
            throw new IllegalArgumentException();
        }
        this.domains = new ArrayList<String>(Arrays.asList(domain));
    }

    /** Sets domains. Overwrites previously set values! */
    public void setDomains(final List<String> domains) {
        if (domains == null) {
            throw new IllegalArgumentException();
        }
        this.domains = new ArrayList<String>(domains);
    }

    public long getLinksLeft() {
        final Long value = linksLeft;
        return value != null ? value : -1;
    }

    public void setLinksLeft(long num) {
        this.linksLeft = num;
        setUnlimitedLinks(false);
    }

    public long getLinksMax() {
        final Long value = linksMax;
        return value != null ? value : -1;
    }

    public void setLinksMax(long num) {
        this.linksMax = num;
        setUnlimitedLinks(false);
    }

    /** Set traffic left and max with one call. */
    public void setLinksLeftAndMax(long left, final long max) {
        this.setLinksLeft(left);
        this.setLinksMax(max);
    }

    public long getTrafficLeft() {
        final Long value = trafficLeft;
        return value != null ? value : -1;
    }

    public void setTrafficLeft(long trafficLeft) {
        this.trafficLeft = trafficLeft;
        setUnlimitedTraffic(false);
    }

    public long getTrafficMax() {
        final Long value = trafficMax;
        return value != null ? value : -1;
    }

    public void setTrafficMax(long num) {
        this.trafficMax = Math.max(0, num);
        setUnlimitedTraffic(false);
    }

    /** Set traffic left and max with one call. */
    public void setTrafficLeftAndMax(long left, final long max) {
        this.setTrafficLeft(left);
        this.setTrafficMax(max);
    }

    /**
     * How much traffic is needed- and credited from the account when downloading from this host? </br> 500 = 5 times the size of the
     * downloaded file.
     */
    public short getTrafficCalculationFactorPercent() {
        final Short value = trafficCalculationFactorPercent;
        return value != null ? value : 100;
    }

    public void setTrafficCalculationFactorPercent(short num) {
        this.trafficCalculationFactorPercent = num;
    }

    public boolean isUnlimitedLinks() {
        return isEnabled(isUnlimitedLinks);
    }

    public void setUnlimitedLinks(Boolean param) {
        this.isUnlimitedLinks = param;
        if (isEnabled(param)) {
            this.linksLeft = null;
            this.linksMax = null;
        }
    }

    public boolean isUnlimitedTraffic() {
        return isEnabled(isUnlimitedTraffic);
    }

    public void setUnlimitedTraffic(Boolean param) {
        this.isUnlimitedTraffic = param;
        if (isEnabled(param)) {
            this.trafficLeft = null;
            this.trafficMax = null;
        }
    }

    /**
     * Returns custom set status text. </br> Typically used to describe why this host is currently not working but can also be used as an
     * informative field.
     */
    public String getStatusText() {
        return statusText;
    }

    public void setStatusText(String statusText) {
        this.statusText = statusText;
    }

    /** Returns title for this entry which is either its' name, the first entry in the domain list or null. */
    public String getTitle() {
        final String name = this.name;
        final List<String> domains;
        if (name != null) {
            return name;
        } else if ((domains = getDomains()) != null && domains.size() > 0) {
            return domains.get(0);
        } else {
            return null;
        }
    }

    public void setErrorStatus(final String text, final long waitMillis) {
        this.setUnavailableStatusText(text);
        this.setUnavailableTime(waitMillis);
        if (waitMillis > 0 && propertyListenerEnabled.compareAndSet(false, true)) {
            DownloadController.getInstance().getEventSender().addListener(this, true);
        }
    }

    public void clearErrorStatus() {
        // this.setStatus(MultihosterHostStatus.WORKING);
        this.setUnavailableStatusText(null);
        this.setUnavailableTimestamp(null);
        if (propertyListenerEnabled.compareAndSet(true, false)) {
            DownloadController.getInstance().getEventSender().removeListener(this);
        }
    }

    public MultihosterHostStatus getStatus() {
        final MultihosterHostStatus status = this.status;
        if (status != null) {
            return status;
        } else {
            /* Default */
            return MultihosterHostStatus.WORKING;
        }
    }

    public void setStatus(MultihosterHostStatus status) {
        if (MultihosterHostStatus.WORKING.equals(status)) {
            this.status = null;// see getStatus
        } else {
            this.status = status;
        }
    }

    public int getMaxChunks() {
        final Integer value = maxChunks;
        return value != null ? value : 0;
    }

    public void setMaxChunks(int maxChunks) {
        this.maxChunks = maxChunks;
    }

    public boolean isResumable() {
        return isEnabled(resume);
    }

    public void setResumable(boolean resume) {
        this.resume = resume;
    }

    public boolean supportsDomain(String domain) {
        if (domain == null) {
            return false;
        }
        domain = domain.toLowerCase(Locale.ENGLISH);
        if (this.getDomains().contains(domain)) {
            return true;
        } else {
            return false;
        }
    }

    /** Returns first domain of list of supported domains if list size is > 0. */
    public String getDomain() {
        final List<String> domains = getDomains();
        if (domains.size() > 0) {
            return domains.get(0);
        } else {
            return null;
        }
    }

    public List<String> getDomains() {
        return this.domains;
    }

    public String getUnavailableStatusText() {
        return unavailableStatusText;
    }

    private void setUnavailableStatusText(String unavailableStatusText) {
        this.unavailableStatusText = unavailableStatusText;
    }

    public long getUnavailableUntilTimestamp() {
        final Long value = unavailableUntilTimestamp;
        return value != null ? value : -1;
    }

    private void setUnavailableTimestamp(final Long num) {
        this.unavailableUntilTimestamp = num;
    }

    private void setUnavailableTime(long milliseconds) {
        final long timestampFinal = Math.min(milliseconds, MAX_UNAVAILABLE_TIME);
        setUnavailableTimestamp(Time.systemIndependentCurrentJVMTimeMillis() + timestampFinal);
    }

    /**
     * Returns time this item is unavailable for. </br> This can return negative values.
     */
    public long getUnavailableTimeMillis() {
        final long unavailableTimestamp = this.getUnavailableUntilTimestamp();
        return Math.max(0, unavailableTimestamp - Time.systemIndependentCurrentJVMTimeMillis());
    }

    public int getMaxDownloads() {
        final Integer value = maxDownloads;
        return value != null ? value : Integer.MAX_VALUE;
    }

    public void setMaxDownloads(int maxDownloads) {
        this.maxDownloads = maxDownloads;
    }

    public long getCreatedTimestamp() {
        return createdTimestamp;
    }

    public void setCreatedTimestamp(long createdTimestamp) {
        this.createdTimestamp = createdTimestamp;
    }

    protected DomainInfo domainInfo = null;

    public DomainInfo getDomainInfo() {
        final DomainInfo domainInfo = this.domainInfo;
        if (domainInfo == null || !StringUtils.equalsIgnoreCase(getDomain(), domainInfo.getDomain())) {
            return this.domainInfo = DomainInfo.getInstance(getDomain());
        }
        return domainInfo;
    }

    @Override
    public String toString() {
        final String title = getTitle();
        return title + " | Status: " + this.getStatus() + " | StatusText: " + this.getStatusText() + " | UnavailableStatusText: " + this.getUnavailableStatusText() + " | LinksAvailable: " + this.getLinksLeft() + "/" + this.getLinksMax() + " | Traffic: " + SizeFormatter.formatBytes(this.getTrafficLeft()) + "/" + SizeFormatter.formatBytes(this.getTrafficMax()) + " | Chunks: " + this.getMaxChunks() + " | Resume: " + this.isResumable();
    }

    @Override
    public void onDownloadControllerAddedPackage(FilePackage pkg) {
    }

    @Override
    public void onDownloadControllerStructureRefresh(FilePackage pkg) {
    }

    @Override
    public void onDownloadControllerStructureRefresh() {
    }

    @Override
    public void onDownloadControllerStructureRefresh(AbstractNode node, Object param) {
    }

    @Override
    public void onDownloadControllerRemovedPackage(FilePackage pkg) {
    }

    @Override
    public void onDownloadControllerRemovedLinklist(List<DownloadLink> list) {
    }

    @Override
    public void onDownloadControllerUpdatedData(DownloadLink downloadlink, DownloadLinkProperty property) {
        if (downloadlink != null && DownloadLinkProperty.Property.RESET.equals(property.getProperty()) && downloadlink.getHost().equals(getDomain())) {
            clearErrorStatus();
        }
    }

    @Override
    public void onDownloadControllerUpdatedData(FilePackage pkg, FilePackageProperty property) {
    }

    @Override
    public void onDownloadControllerUpdatedData(DownloadLink downloadlink) {
    }

    @Override
    public void onDownloadControllerUpdatedData(FilePackage pkg) {
    }
}