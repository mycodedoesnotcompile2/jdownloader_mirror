package jd.controlling;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import jd.plugins.Account;
import jd.plugins.Account.AccountType;
import jd.plugins.AccountInfo;
import jd.plugins.PluginForHost;

import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;

/**
 * Filter class to query accounts with specific criteria
 */
public class AccountFilter {
    private List<String>      hosts               = null;
    private Boolean           enabled             = null;
    private Boolean           valid               = null;
    private List<AccountType> accountTypes        = null;
    private Boolean           expired             = null;
    private Boolean           temporarilyDisabled = null;
    private Boolean           multiHost           = null;
    private Long              minimumTrafficLeft  = null;
    private Double            minimumBalance      = null;
    private List<FEATURE>     features            = null;
    private Integer           maxResultsNum       = null; // Default: unlimited results
    private String            username            = null;

    /**
     * Creates a new account filter with no criteria (matches all accounts)
     */
    public AccountFilter() {
        // Default constructor with no criteria
    }

    /**
     * Creates a new account filter for multiple hosts
     *
     * @param hosts
     *            the list of hosts to filter for
     */
    public AccountFilter(List<String> hosts) {
        this(hosts != null ? hosts.toArray(new String[0]) : null);
    }

    /**
     * Creates a new account filter for multiple hosts using varargs
     *
     * @param hosts
     *            variable number of hosts to filter for
     */
    public AccountFilter(String... hosts) {
        setHosts(hosts);
    }

    /**
     * Filter accounts by enabled status
     *
     * @param enabled
     *            true to match only enabled accounts, false to match only disabled accounts
     * @return this filter for chaining
     */
    public AccountFilter setEnabled(Boolean enabled) {
        this.enabled = enabled;
        return this;
    }

    /**
     * Filter accounts by validity status
     *
     * @param valid
     *            true to match only valid accounts, false to match only invalid accounts
     * @return this filter for chaining
     */
    public AccountFilter setValid(Boolean valid) {
        this.valid = valid;
        return this;
    }

    /**
     * Filter accounts by multiple account types using varargs
     *
     * @param accountTypes
     *            variable number of account types to filter for
     * @return this filter for chaining
     */
    public AccountFilter setAccountTypes(AccountType... accountTypes) {
        if (accountTypes != null && accountTypes.length > 0) {
            this.accountTypes = Arrays.asList(accountTypes);
        } else {
            this.accountTypes = null;
        }
        return this;
    }

    public List<AccountType> getAccountTypes() {
        return accountTypes;
    }

    /**
     * Filter accounts by expiration status
     *
     * @param expired
     *            true to match only expired accounts, false to match only non-expired accounts
     * @return this filter for chaining
     */
    public AccountFilter setExpired(Boolean expired) {
        this.expired = expired;
        return this;
    }

    /**
     * Filter accounts by temporary disabled status
     *
     * @param temporarilyDisabled
     *            true to match only temporarily disabled accounts, false to match only non-temporarily disabled accounts
     * @return this filter for chaining
     */
    public AccountFilter setTemporarilyDisabled(Boolean temporarilyDisabled) {
        this.temporarilyDisabled = temporarilyDisabled;
        return this;
    }

    /**
     * Filter accounts by multi-host capability
     *
     * @param multiHost
     *            true to match only multi-host accounts, false to match only single-host accounts
     * @return this filter for chaining
     */
    public AccountFilter setMultiHost(Boolean multiHost) {
        this.multiHost = multiHost;
        return this;
    }

    /**
     * Filter accounts by minimum traffic left
     *
     * @param minimumTrafficLeft
     *            the minimum traffic left in bytes
     * @return this filter for chaining
     */
    public AccountFilter setMinimumTrafficLeft(Long minimumTrafficLeft) {
        this.minimumTrafficLeft = minimumTrafficLeft;
        return this;
    }

    /**
     * Filter accounts by minimum balance
     *
     * @param minimumBalance
     *            the minimum balance
     * @return this filter for chaining
     */
    public AccountFilter setMinimumBalance(Double minimumBalance) {
        this.minimumBalance = minimumBalance;
        return this;
    }

    /**
     * Filter accounts by a single plugin feature
     *
     * @param feature
     *            a feature that the plugin must have
     * @return this filter for chaining
     */
    public AccountFilter setFeature(FEATURE... features) {
        if (features != null && features.length > 0) {
            this.features = Arrays.asList(features);
        } else {
            this.features = null;
        }
        return this;
    }

    /**
     * Set the list of hosts to filter for
     *
     * @param hosts
     *            list of hosts to filter for
     * @return this filter for chaining
     */
    public AccountFilter setHosts(String... hosts) {
        if (hosts != null && hosts.length > 0) {
            this.hosts = Arrays.asList(hosts);
        } else {
            this.hosts = null;
        }
        return this;
    }

    /**
     * Limit the maximum number of results returned by the filter
     *
     * @param maxResultsNum
     *            maximum number of results to return, or null for unlimited results
     * @return this filter for chaining
     */
    public AccountFilter setMaxResultsNum(Integer maxResultsNum) {
        this.maxResultsNum = maxResultsNum;
        return this;
    }

    public Integer getMaxResultsNum() {
        return maxResultsNum;
    }

    /**
     * Get the hosts list to filter for
     *
     * @return the list of hosts
     */
    public List<String> getHosts() {
        return hosts;
    }

    public AccountFilter setUsername(String username) {
        this.username = username;
        return this;
    }

    /**
     * Get the username to filter for
     *
     * @return the username
     */
    public String getUsername() {
        return username;
    }

    protected boolean matchesAccountType(Account account) {
        final List<AccountType> accountTypes = this.accountTypes;
        if (accountTypes != null && !accountTypes.isEmpty()) {
            final AccountType accountType = account.getType();
            if (accountType == null) {
                return false;
            }
            for (AccountType type : accountTypes) {
                if (type != null && type.equals(accountType)) {
                    return true;
                }
            }
            return false;
        }
        return true;
    }

    protected boolean matchesFeatures(Account account) {
        // Check required features
        final List<FEATURE> features = this.features;
        if (features != null && !features.isEmpty()) {
            final PluginForHost plugin = account.getPlugin();
            if (plugin == null) {
                return false;
            }
            for (FEATURE feature : features) {
                if (!plugin.hasFeature(feature)) {
                    return false;
                }
            }
        }
        return true;
    }

    protected boolean matchesHost(Account account) {
        final List<String> hosts = getHosts();
        if (hosts != null && !hosts.isEmpty()) {
            final String accountHost = account.getHosterByPlugin();
            if (accountHost == null) {
                return false;
            }
            for (String host : hosts) {
                if (host != null && host.equalsIgnoreCase(accountHost)) {
                    return true;
                }
            }
            return false;
        }
        return true;
    }

    protected boolean matchesUsername(Account account) {
        final String username = this.username;
        if (username != null) {
            final String accountUsername = account.getUser();
            if (accountUsername == null) {
                return false;
            }
            return accountUsername.equalsIgnoreCase(username);
        }
        return true;
    }

    /**
     * Check if an account matches the filter criteria
     *
     * @param account
     *            the account to check
     * @return true if the account matches all filter criteria, false otherwise
     */
    public boolean matches(Account account) {
        if (account == null || account.getPlugin() == null) {
            return false;
        }
        // Check enabled
        final Boolean enabled = this.enabled;
        if (enabled != null && enabled != account.isEnabled()) {
            return false;
        }
        // Check valid
        final Boolean valid = this.valid;
        if (valid != null && valid != account.isValid()) {
            return false;
        }
        // Check account type
        if (!matchesAccountType(account)) {
            return false;
        }
        // Check multiHost
        final Boolean multiHost = this.multiHost;
        if (multiHost != null && multiHost != account.isMultiHost()) {
            return false;
        }
        // Check hosts list
        if (!matchesHost(account)) {
            return false;
        }
        if (!matchesUsername(account)) {
            return false;
        }
        // Check expired
        final Boolean expired = this.expired;
        if (expired != null) {
            final AccountInfo ai = account.getAccountInfo();
            if (ai != null && expired != ai.isExpired()) {
                return false;
            }
        }
        // Check temporarily disabled
        final Boolean temporarilyDisabled = this.temporarilyDisabled;
        if (temporarilyDisabled != null && temporarilyDisabled != account.isTempDisabled()) {
            return false;
        }
        // Check minimum traffic left
        final Long minimumTrafficLeft = this.minimumTrafficLeft;
        if (minimumTrafficLeft != null) {
            final AccountInfo ai = account.getAccountInfo();
            if (ai != null && !ai.isUnlimitedTraffic() && ai.getTrafficLeft() < minimumTrafficLeft) {
                return false;
            }
        }
        // Check minimum balance
        final Double minimumBalance = this.minimumBalance;
        if (minimumBalance != null) {
            final AccountInfo ai = account.getAccountInfo();
            if (ai != null && ai.getAccountBalance() < minimumBalance) {
                return false;
            }
        }
        if (!matchesFeatures(account)) {
            return false;
        }
        return true;
    }

    /**
     * Helper method to filter a list of accounts based on this filter's criteria
     *
     * @param accounts
     *            the list of accounts to filter
     * @return a new list containing only the accounts that match the filter criteria
     */
    public List<Account> filter(List<Account> accounts) {
        final List<Account> result = new ArrayList<Account>();
        if (accounts == null) {
            return result;
        }
        final Integer maxResultsNum = getMaxResultsNum();
        for (final Account account : accounts) {
            if (!matches(account)) {
                continue;
            }
            result.add(account);
            // Check if we've reached the maximum number of results
            if (maxResultsNum != null && result.size() >= maxResultsNum.intValue()) {
                break;
            }
        }
        return result;
    }
}