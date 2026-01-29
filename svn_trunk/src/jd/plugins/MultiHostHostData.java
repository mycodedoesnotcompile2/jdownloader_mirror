package jd.plugins;

import java.util.ArrayList;
import java.util.List;

import jd.plugins.MultiHostHost.MultihosterHostStatus;

import org.appwork.storage.Storable;

public class MultiHostHostData implements Storable {
    private Long                  createdTimestamp;
    private List<String>          domains;
    private boolean               enabled;
    private Long                  linksLeft;
    private Long                  linksMax;
    private Integer               maxChunks;
    private Integer               maxDownloads;
    private String                name;
    private Boolean               resume;
    private MultihosterHostStatus status;
    private String                statusText;
    private Short                 trafficCalculationFactorPercent;
    private Long                  trafficLeft;
    private Long                  trafficMax;
    private String                unavailableStatusText;
    private Long                  unavailableUntilTimestamp;
    private Boolean               unlimitedLinks;
    private Boolean               unlimitedTraffic;

    public MultiHostHostData() {
        this.domains = new ArrayList<String>();
    }

    public Long getCreatedTimestamp() {
        return createdTimestamp;
    }

    public void setCreatedTimestamp(Long createdTimestamp) {
        this.createdTimestamp = createdTimestamp;
    }

    public List<String> getDomains() {
        return domains;
    }

    public void setDomains(List<String> domains) {
        this.domains = domains;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public Long getLinksLeft() {
        return linksLeft;
    }

    public void setLinksLeft(Long linksLeft) {
        this.linksLeft = linksLeft;
    }

    public Long getLinksMax() {
        return linksMax;
    }

    public void setLinksMax(Long linksMax) {
        this.linksMax = linksMax;
    }

    public Integer getMaxChunks() {
        return maxChunks;
    }

    public void setMaxChunks(Integer maxChunks) {
        this.maxChunks = maxChunks;
    }

    public Integer getMaxDownloads() {
        return maxDownloads;
    }

    public void setMaxDownloads(Integer maxDownloads) {
        this.maxDownloads = maxDownloads;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Boolean getResume() {
        return resume;
    }

    public void setResume(Boolean resume) {
        this.resume = resume;
    }

    public MultihosterHostStatus getStatus() {
        return status;
    }

    public void setStatus(MultihosterHostStatus status) {
        this.status = status;
    }

    public String getStatusText() {
        return statusText;
    }

    public void setStatusText(String statusText) {
        this.statusText = statusText;
    }

    public Short getTrafficCalculationFactorPercent() {
        return trafficCalculationFactorPercent;
    }

    public void setTrafficCalculationFactorPercent(Short trafficCalculationFactorPercent) {
        this.trafficCalculationFactorPercent = trafficCalculationFactorPercent;
    }

    public Long getTrafficLeft() {
        return trafficLeft;
    }

    public void setTrafficLeft(Long trafficLeft) {
        this.trafficLeft = trafficLeft;
    }

    public Long getTrafficMax() {
        return trafficMax;
    }

    public void setTrafficMax(Long trafficMax) {
        this.trafficMax = trafficMax;
    }

    public String getUnavailableStatusText() {
        return unavailableStatusText;
    }

    public void setUnavailableStatusText(String unavailableStatusText) {
        this.unavailableStatusText = unavailableStatusText;
    }

    public Long getUnavailableUntilTimestamp() {
        return unavailableUntilTimestamp;
    }

    public void setUnavailableUntilTimestamp(Long unavailableUntilTimestamp) {
        this.unavailableUntilTimestamp = unavailableUntilTimestamp;
    }

    public Boolean isUnlimitedLinks() {
        return unlimitedLinks;
    }

    public void setUnlimitedLinks(Boolean unlimitedLinks) {
        this.unlimitedLinks = unlimitedLinks;
    }

    public Boolean isUnlimitedTraffic() {
        return unlimitedTraffic;
    }

    public void setUnlimitedTraffic(Boolean unlimitedTraffic) {
        this.unlimitedTraffic = unlimitedTraffic;
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
        final MultiHostHostData hostData = new MultiHostHostData();
        hostData.setCreatedTimestamp(source.getCreatedTimestamp());
        hostData.setEnabled(source.isEnabled());
        hostData.setName(source.getName());
        hostData.setDomains(source.getDomains());
        hostData.setUnlimitedTraffic(source.isUnlimitedTraffic);
        hostData.setUnlimitedLinks(source.isUnlimitedLinks);
        hostData.setLinksLeft(source.linksLeft);
        hostData.setLinksMax(source.linksMax);
        hostData.setTrafficLeft(source.trafficLeft);
        hostData.setTrafficMax(source.trafficMax);
        hostData.setTrafficCalculationFactorPercent(source.trafficCalculationFactorPercent);
        hostData.setMaxChunks(source.maxChunks);
        hostData.setMaxDownloads(source.maxDownloads);
        hostData.setResume(source.resume);
        hostData.setStatusText(source.statusText);
        hostData.setStatus(source.status);
        hostData.setUnavailableStatusText(source.unavailableStatusText);
        hostData.setUnavailableUntilTimestamp(source.unavailableUntilTimestamp);
        return hostData;
    }

    public MultiHostHost toMultiHostHost() {
        final MultiHostHost mhost = new MultiHostHost();
        final Long createdTimestamp = getCreatedTimestamp();
        if (createdTimestamp != null) {
            mhost.setCreatedTimestamp(createdTimestamp);
        }
        mhost.setEnabled(this.isEnabled());
        mhost.setName(this.getName());
        mhost.setDomains(this.getDomains());
        final Long linksLeft = getLinksLeft();
        if (linksLeft != null) {
            mhost.setLinksLeft(linksLeft);
        }
        final Long linksMax = getLinksMax();
        if (linksMax != null) {
            mhost.setLinksMax(linksMax);
        }
        final Long trafficLeft = getTrafficLeft();
        if (trafficLeft != null) {
            mhost.setTrafficLeft(trafficLeft);
        }
        final Long trafficMax = getTrafficMax();
        if (trafficMax != null) {
            mhost.setTrafficMax(trafficMax);
        }
        final Boolean unlimitedTraffic = isUnlimitedTraffic();
        if (unlimitedTraffic != null) {
            mhost.setUnlimitedTraffic(unlimitedTraffic);
        }
        final Boolean unlimitedLinks = isUnlimitedLinks();
        if (unlimitedLinks != null) {
            mhost.setUnlimitedLinks(unlimitedLinks);
        }
        final Short trafficCalculationFactorPercent = getTrafficCalculationFactorPercent();
        if (trafficCalculationFactorPercent != null) {
            mhost.setTrafficCalculationFactorPercent(trafficCalculationFactorPercent);
        }
        final Integer maxChunks = getMaxChunks();
        if (maxChunks != null) {
            mhost.setMaxChunks(maxChunks);
        }
        final Integer maxDownloads = getMaxDownloads();
        if (maxDownloads != null) {
            mhost.setMaxDownloads(maxDownloads);
        }
        final Boolean resume = getResume();
        if (resume != null) {
            mhost.setResumable(resume);
        }
        mhost.setStatusText(this.getStatusText());
        mhost.setStatus(this.getStatus());
        // host.setUnavailableStatusText(this.getUnavailableStatusText());
        // host.setUnavailableTimestamp(this.getUnavailableUntilTimestamp());
        return mhost;
    }
}