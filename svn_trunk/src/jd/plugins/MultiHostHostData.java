package jd.plugins;

import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.Storable;

import jd.plugins.MultiHostHost.MultihosterHostStatus;

public class MultiHostHostData implements Storable {
    private String                name;
    private List<String>          domains;
    private Boolean               isUnlimitedTraffic;
    private Boolean               isUnlimitedLinks;
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

    public Boolean getIsUnlimitedTraffic() {
        return isUnlimitedTraffic;
    }

    public void setIsUnlimitedTraffic(Boolean isUnlimitedTraffic) {
        this.isUnlimitedTraffic = isUnlimitedTraffic;
    }

    public Boolean getIsUnlimitedLinks() {
        return isUnlimitedLinks;
    }

    public void setIsUnlimitedLinks(Boolean isUnlimitedLinks) {
        this.isUnlimitedLinks = isUnlimitedLinks;
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

    public static MultiHostHostData createFromMultiHostHost(MultiHostHost source) {
        MultiHostHostData hostData = new MultiHostHostData();
        hostData.setName(source.getName());
        hostData.setDomains(source.getDomains());
        hostData.setIsUnlimitedTraffic(source.isUnlimitedTraffic());
        hostData.setIsUnlimitedLinks(source.isUnlimitedLinks());
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
        MultiHostHost host = new MultiHostHost();
        host.setName(this.getName());
        host.setDomains(this.getDomains());
        host.setLinksLeft(this.getLinksLeft());
        host.setLinksMax(this.getLinksMax());
        host.setTrafficLeft(this.getTrafficLeft());
        host.setTrafficMax(this.getTrafficMax());
        host.setUnlimitedTraffic(this.getIsUnlimitedTraffic());
        host.setUnlimitedLinks(this.getIsUnlimitedLinks());
        host.setTrafficCalculationFactorPercent(this.getTrafficCalculationFactorPercent());
        host.setMaxChunks(this.getMaxChunks());
        host.setMaxDownloads(this.getMaxDownloads());
        host.setResume(this.getResume());
        host.setStatusText(this.getStatusText());
        host.setStatus(this.getStatus());
        // host.setUnavailableStatusText(this.getUnavailableStatusText());
        // host.setUnavailableTimestamp(this.getUnavailableUntilTimestamp());
        return host;
    }
}
