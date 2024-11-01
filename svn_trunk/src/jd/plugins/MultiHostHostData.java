package jd.plugins;

import java.util.ArrayList;
import java.util.List;

import jd.plugins.MultiHostHost.MultihosterHostStatus;

import org.appwork.storage.Storable;

public class MultiHostHostData implements Storable {
    boolean              enabled;
    private String       name;
    private List<String> domains;
    private Boolean      unlimitedTraffic;

    public Boolean isUnlimitedTraffic() {
        return unlimitedTraffic;
    }

    public void setUnlimitedTraffic(Boolean unlimitedTraffic) {
        this.unlimitedTraffic = unlimitedTraffic;
    }

    public Boolean isUnlimitedLinks() {
        return unlimitedLinks;
    }

    public void setUnlimitedLinks(Boolean unlimitedLinks) {
        this.unlimitedLinks = unlimitedLinks;
    }

    private Boolean               unlimitedLinks;
    private long                  linksLeft;
    private long                  linksMax;
    private long                  trafficLeft;
    private long                  trafficMax;
    private Short                 trafficCalculationFactorPercent;
    private int                   maxChunks;
    private int                   maxDownloads;
    private Boolean               resume;
    private String                statusText;
    private MultihosterHostStatus status;
    private String                unavailableStatusText;
    private long                  unavailableUntilTimestamp;

    public MultiHostHostData() {
        this.domains = new ArrayList<String>();
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<String> getDomains() {
        return domains;
    }

    public void setDomains(List<String> domains) {
        this.domains = domains;
    }

    public long getLinksLeft() {
        return linksLeft;
    }

    public void setLinksLeft(long linksLeft) {
        this.linksLeft = linksLeft;
    }

    public long getLinksMax() {
        return linksMax;
    }

    public void setLinksMax(long linksMax) {
        this.linksMax = linksMax;
    }

    public long getTrafficLeft() {
        return trafficLeft;
    }

    public void setTrafficLeft(long trafficLeft) {
        this.trafficLeft = trafficLeft;
    }

    public long getTrafficMax() {
        return trafficMax;
    }

    public void setTrafficMax(long trafficMax) {
        this.trafficMax = trafficMax;
    }

    public Short getTrafficCalculationFactorPercent() {
        return trafficCalculationFactorPercent;
    }

    public void setTrafficCalculationFactorPercent(Short trafficCalculationFactorPercent) {
        this.trafficCalculationFactorPercent = trafficCalculationFactorPercent;
    }

    public int getMaxChunks() {
        return maxChunks;
    }

    public void setMaxChunks(int maxChunks) {
        this.maxChunks = maxChunks;
    }

    public int getMaxDownloads() {
        return maxDownloads;
    }

    public void setMaxDownloads(int maxDownloads) {
        this.maxDownloads = maxDownloads;
    }

    public Boolean getResume() {
        return resume;
    }

    public void setResume(Boolean resume) {
        this.resume = resume;
    }

    public String getStatusText() {
        return statusText;
    }

    public void setStatusText(String statusText) {
        this.statusText = statusText;
    }

    public MultihosterHostStatus getStatus() {
        return status;
    }

    public void setStatus(MultihosterHostStatus status) {
        this.status = status;
    }

    public String getUnavailableStatusText() {
        return unavailableStatusText;
    }

    public void setUnavailableStatusText(String unavailableStatusText) {
        this.unavailableStatusText = unavailableStatusText;
    }

    public long getUnavailableUntilTimestamp() {
        return unavailableUntilTimestamp;
    }

    public void setUnavailableUntilTimestamp(long unavailableUntilTimestamp) {
        this.unavailableUntilTimestamp = unavailableUntilTimestamp;
    }

    public static List<MultiHostHostData> createFromMultiHostHostList(final List<MultiHostHost> sourcelist) {
        if (sourcelist == null || sourcelist.size() == 0) {
            return null;
        }
        final List<MultiHostHostData> ret = new ArrayList<MultiHostHostData>();
        for (final MultiHostHost mhost : sourcelist) {
            ret.add(createFromMultiHostHost(mhost));
        }
        return ret;
    }

    public static MultiHostHostData createFromMultiHostHost(MultiHostHost source) {
        MultiHostHostData hostData = new MultiHostHostData();
        hostData.setEnabled(source.isEnabled());
        hostData.setName(source.getName());
        hostData.setDomains(source.getDomains());
        hostData.setUnlimitedTraffic(source.isUnlimitedTraffic());
        hostData.setUnlimitedLinks(source.isUnlimitedLinks());
        hostData.setLinksLeft(source.getLinksLeft());
        hostData.setLinksMax(source.getLinksMax());
        hostData.setTrafficLeft(source.getTrafficLeft());
        hostData.setTrafficMax(source.getTrafficMax());
        hostData.setTrafficCalculationFactorPercent(source.getTrafficCalculationFactorPercent());
        hostData.setMaxChunks(source.getMaxChunks());
        hostData.setMaxDownloads(source.getMaxDownloads());
        hostData.setResume(source.isResume());
        hostData.setStatusText(source.getStatusText());
        hostData.setStatus(source.getStatus());
        /* Ignore error states -> Purposely lose this information */
        // hostData.setUnavailableStatusText(source.getUnavailableStatusText());
        // hostData.setUnavailableUntilTimestamp(source.getUnavailableUntilTimestamp());
        return hostData;
    }

    public MultiHostHost toMultiHostHost() {
        final MultiHostHost mhost = new MultiHostHost();
        mhost.setEnabled(this.isEnabled());
        mhost.setName(this.getName());
        mhost.setDomains(this.getDomains());
        mhost.setLinksLeft(this.getLinksLeft());
        mhost.setLinksMax(this.getLinksMax());
        mhost.setTrafficLeft(this.getTrafficLeft());
        mhost.setTrafficMax(this.getTrafficMax());
        mhost.setUnlimitedTraffic(this.isUnlimitedTraffic());
        mhost.setUnlimitedLinks(this.isUnlimitedLinks());
        mhost.setTrafficCalculationFactorPercent(this.getTrafficCalculationFactorPercent());
        mhost.setMaxChunks(this.getMaxChunks());
        mhost.setMaxDownloads(this.getMaxDownloads());
        mhost.setResume(this.getResume());
        mhost.setStatusText(this.getStatusText());
        mhost.setStatus(this.getStatus());
        // host.setUnavailableStatusText(this.getUnavailableStatusText());
        // host.setUnavailableTimestamp(this.getUnavailableUntilTimestamp());
        return mhost;
    }
}
