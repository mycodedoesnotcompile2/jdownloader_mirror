package org.jdownloader.settings;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.remoteapi.annotations.AllowNonStorableObjects;
import org.appwork.storage.Storable;
import org.appwork.utils.StringUtils;

import jd.plugins.Account;
import jd.plugins.Account.AccountError;
import jd.plugins.AccountInfo;
import jd.plugins.MultiHostHost;
import jd.plugins.MultiHostHostData;

public class AccountData implements Storable {
    // Variables
    private Map<String, Object>     properties;
    private Map<String, Object>     infoProperties;
    private String                  hoster;
    private String                  password;
    private String                  user;
    private String                  errorType;
    private String                  errorString;
    private String                  statusString;
    private List<String>            hosterHistory         = null;
    private List<MultiHostHostData> infoSupportedhostlist = null;
    private int                     maxSimultanDownloads;
    private long                    createTime;
    private long                    trafficLeft;
    private long                    trafficMax;
    private long                    validUntil;
    private long                    id                    = -1;
    private volatile long           tmpDisabledTimeout    = -1;
    private boolean                 active;
    private boolean                 enabled;
    private boolean                 trafficUnlimited;
    private boolean                 specialtraffic;
    private boolean                 concurrentUsePossible = true;
    private boolean                 trafficRefill         = true;
    private double                  accountBalance;

    // Constructor
    public AccountData(/* Storable */) {
        // required by Storable
    }

    // Getter and Setter methods
    @AllowNonStorableObjects
    public Map<String, Object> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, Object> properties) {
        this.properties = properties;
    }

    public String getHoster() {
        return hoster;
    }

    public void setHoster(String hoster) {
        this.hoster = hoster;
    }

    public int getMaxSimultanDownloads() {
        return maxSimultanDownloads;
    }

    public void setMaxSimultanDownloads(int maxSimultanDownloads) {
        this.maxSimultanDownloads = maxSimultanDownloads;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public long getTmpDisabledTimeout() {
        return tmpDisabledTimeout;
    }

    public void setTmpDisabledTimeout(long tmpDisabledTimeout) {
        this.tmpDisabledTimeout = tmpDisabledTimeout;
    }

    public boolean isTrafficRefill() {
        return trafficRefill;
    }

    public void setTrafficRefill(boolean account_trafficRefill) {
        this.trafficRefill = account_trafficRefill;
    }

    public String getStatusString() {
        return statusString;
    }

    public void setStatusString(String statusString) {
        this.statusString = statusString;
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public boolean isConcurrentUsePossible() {
        return concurrentUsePossible;
    }

    public void setConcurrentUsePossible(boolean concurrentUsePossible) {
        this.concurrentUsePossible = concurrentUsePossible;
    }

    public String getErrorType() {
        return errorType;
    }

    public void setErrorType(String errorType) {
        this.errorType = errorType;
    }

    public String getErrorString() {
        return errorString;
    }

    public void setErrorString(String errorString) {
        this.errorString = errorString;
    }

    @AllowNonStorableObjects
    public Map<String, Object> getInfoProperties() {
        return infoProperties;
    }

    public void setInfoProperties(Map<String, Object> infoProperties) {
        this.infoProperties = infoProperties;
    }

    public long getCreateTime() {
        return createTime;
    }

    public void setCreateTime(long createTime) {
        this.createTime = createTime;
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

    public long getValidUntil() {
        return validUntil;
    }

    public void setValidUntil(long validUntil) {
        this.validUntil = validUntil;
    }

    public boolean isActive() {
        return active;
    }

    public void setActive(boolean active) {
        this.active = active;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public boolean isTrafficUnlimited() {
        return trafficUnlimited;
    }

    public void setTrafficUnlimited(boolean trafficUnlimited) {
        this.trafficUnlimited = trafficUnlimited;
    }

    public boolean isSpecialtraffic() {
        return specialtraffic;
    }

    public void setSpecialtraffic(boolean specialtraffic) {
        this.specialtraffic = specialtraffic;
    }

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    public double getAccountBalance() {
        return accountBalance;
    }

    public void setAccountBalance(double accountBalance) {
        this.accountBalance = accountBalance;
    }

    /**
     * @return the hosterHistory
     */
    public List<String> getHosterHistory() {
        return hosterHistory;
    }

    /**
     * @param hosterHistory
     *            the hosterHistory to set
     */
    public void setHosterHistory(List<String> hosterHistory) {
        this.hosterHistory = hosterHistory;
    }

    public List<MultiHostHostData> getSupportedhostlist() {
        return infoSupportedhostlist;
    }

    public void setSupportedhostlist(List<MultiHostHostData> supportedhostlist) {
        this.infoSupportedhostlist = supportedhostlist;
    }

    // Static Methods
    private static String enumToString(AccountError error) {
        return error == null ? null : error.name();
    }

    // Instance Methods
    public static AccountData create(Account a) {
        AccountData ret = new AccountData();
        ret.properties = a.getProperties();
        ret.id = a.getId().getID();
        ret.errorType = enumToString(a.getError());
        ret.tmpDisabledTimeout = a.getTmpDisabledTimeout();
        ret.errorString = a.getErrorString();
        ret.hosterHistory = a.getHosterHistory();
        final AccountInfo ai = a.getAccountInfo();
        if (ai != null) {
            ret.infoProperties = ai.getProperties();
            if (ret.infoProperties == null) {
                ret.infoProperties = new HashMap<String, Object>();
            }
            ret.trafficRefill = ai.isTrafficRefill();
            ret.createTime = ai.getCreateTime();
            ret.trafficLeft = ai.getTrafficLeft();
            ret.trafficMax = ai.getTrafficMax();
            ret.validUntil = ai.getValidUntil();
            ret.trafficUnlimited = ai.isUnlimitedTraffic();
            ret.specialtraffic = ai.isSpecialTraffic();
            ret.statusString = ai.getStatus();
            ret.accountBalance = ai.getAccountBalance(); // Added this line to store account balance
            ret.infoSupportedhostlist = MultiHostHostData.createFromMultiHostHostList(ai.getMultiHostSupportV2());
        }
        ret.concurrentUsePossible = a.isConcurrentUsePossible();
        ret.enabled = a.isEnabled();
        ret.hoster = a.getHoster();
        ret.maxSimultanDownloads = a.getMaxSimultanDownloads();
        ret.password = a.getPass();
        ret.user = a.getUser();
        return ret;
    }

    public Account toAccount() {
        final Account ret = new Account(user, password);
        ret.setProperties(properties);
        if (infoProperties != null) {
            final AccountInfo ai = new AccountInfo();
            ret.setAccountInfo(ai);
            ai.setProperties(infoProperties);
            ai.setCreateTime(createTime);
            ai.setTrafficLeft(trafficLeft);
            ai.setTrafficMax(trafficMax);
            ai.setValidUntil(validUntil);
            ai.setStatus(statusString);
            ai.setTrafficRefill(trafficRefill);
            ai.setAccountBalance(accountBalance);
            if (trafficUnlimited) {
                ai.setUnlimitedTraffic();
            }
            ai.setSpecialTraffic(specialtraffic);
            if (this.infoSupportedhostlist != null) {
                final List<MultiHostHost> supportedhosts = new ArrayList<MultiHostHost>();
                for (final MultiHostHostData mhostdata : this.infoSupportedhostlist) {
                    supportedhosts.add(mhostdata.toMultiHostHost());
                }
                ai.setMultiHostSupportV2(supportedhosts);
            }
        }
        ret.setHosterHistory(getHosterHistory());
        ret.setId(getId());
        ret.setConcurrentUsePossible(concurrentUsePossible);
        ret.setEnabled(enabled);
        ret.setHoster(hoster);
        ret.setMaxSimultanDownloads(maxSimultanDownloads);
        ret.setPass(password);
        ret.setUser(user);
        if (StringUtils.isNotEmpty(errorType)) {
            try {
                ret.setError(AccountError.valueOf(errorType), getTmpDisabledTimeout(), errorString);
            } catch (Throwable e) {
            }
        }
        ret.setTmpDisabledTimeout(getTmpDisabledTimeout());
        return ret;
    }
}