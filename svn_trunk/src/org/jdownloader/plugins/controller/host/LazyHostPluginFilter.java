package org.jdownloader.plugins.controller.host;

import java.util.Arrays;
import java.util.List;

import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;

/**
 * Filter class to query plugins with specific criteria
 */
public class LazyHostPluginFilter {
    /**
     * Ready-made filter matching all multihoster plugins ({@link FEATURE#MULTIHOST}). Shared instance; treat it as read-only and do not
     * reconfigure it (create a new {@link LazyHostPluginFilter} instead if other criteria are needed).
     */
    public static final LazyHostPluginFilter ALL_MULTIHOSTERS    = new LazyHostPluginFilter().setFeatures(FEATURE.MULTIHOST).lock();
    /**
     * Ready-made filter matching all captcha solver plugins ({@link FEATURE#CAPTCHA_SOLVER}). Shared instance; treat it as read-only and do
     * not reconfigure it (create a new {@link LazyHostPluginFilter} instead if other criteria are needed).
     */
    public static final LazyHostPluginFilter ALL_CAPTCHA_SOLVERS = new LazyHostPluginFilter().setFeatures(FEATURE.CAPTCHA_SOLVER).lock();
    protected List<String>                   hosts               = null;
    protected Boolean                        premium             = null;
    protected List<FEATURE>                  features            = null;
    protected Integer                        maxResultsNum       = null;

    /**
     * Creates a new plugin filter with no criteria (matches all plugins)
     */
    public LazyHostPluginFilter() {
        // Default constructor with no criteria
    }

    protected LazyHostPluginFilter(LazyHostPluginFilter source) {
        if (source != null) {
            this.hosts = source.hosts;
            this.premium = source.premium;
            this.maxResultsNum = source.maxResultsNum;
            this.features = source.features;
        }
    }

    /**
     * Creates a new plugin filter for multiple hosts
     *
     * @param hosts
     *            the list of hosts to filter for
     */
    public LazyHostPluginFilter(List<String> hosts) {
        this(hosts != null ? hosts.toArray(new String[0]) : null);
    }

    public LazyHostPluginFilter lock() {
        return new LazyHostPluginFilter(this) {
            @Override
            public boolean isLocked() {
                return true;
            }

        };
    }

    protected void checkLockedStatus() throws IllegalStateException {
        if (isLocked()) {
            throw new IllegalStateException("cannot modify locked filter");
        }
    }

    public boolean isLocked() {
        return false;
    }

    /**
     * Creates a new plugin filter for multiple hosts using varargs
     *
     * @param hosts
     *            variable number of hosts to filter for
     */
    public LazyHostPluginFilter(String... hosts) {
        setHosts(hosts);
    }

    /**
     * Filter plugins by premium capability
     *
     * @param premium
     *            true to match only premium-enabled plugins, false to match only non-premium plugins
     * @return this filter for chaining
     */
    public LazyHostPluginFilter setPremium(Boolean premium) {
        checkLockedStatus();
        this.premium = premium;
        return this;
    }

    /**
     * Filter plugins by features
     *
     * @param features
     *            variable number of features that the plugin must have
     * @return this filter for chaining
     */
    public LazyHostPluginFilter setFeatures(FEATURE... features) {
        checkLockedStatus();
        if (features != null && features.length > 0) {
            this.features = Arrays.asList(features);
        } else {
            this.features = null;
        }
        return this;
    }

    public List<FEATURE> getFeatures() {
        return features;
    }

    /**
     * Set the list of hosts to filter for
     *
     * @param hosts
     *            list of hosts to filter for
     * @return this filter for chaining
     */
    public LazyHostPluginFilter setHosts(String... hosts) {
        checkLockedStatus();
        if (hosts != null && hosts.length > 0) {
            this.hosts = Arrays.asList(hosts);
        } else {
            this.hosts = null;
        }
        return this;
    }

    /**
     * Get the hosts list to filter for
     *
     * @return the list of hosts
     */
    public List<String> getHosts() {
        return hosts;
    }

    /**
     * Limit the maximum number of results returned by the filter
     *
     * @param maxResultsNum
     *            maximum number of results to return, or null for unlimited results
     * @return this filter for chaining
     */
    public LazyHostPluginFilter setMaxResultsNum(Integer maxResultsNum) {
        checkLockedStatus();
        this.maxResultsNum = maxResultsNum;
        return this;
    }

    public Integer getMaxResultsNum() {
        return maxResultsNum;
    }

    protected boolean matchesFeatures(LazyHostPlugin plugin) {
        final List<FEATURE> features = this.features;
        if (features != null && !features.isEmpty()) {
            final FEATURE[] pluginFeatures = plugin.getFeatures();
            if (pluginFeatures == null || pluginFeatures.length == 0) {
                return false;
            }
            for (FEATURE requiredFeature : features) {
                boolean found = false;
                for (FEATURE pluginFeature : pluginFeatures) {
                    if (requiredFeature.equals(pluginFeature)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    return false;
                }
            }
        }
        return true;
    }

    protected boolean matchesHost(LazyHostPlugin plugin) {
        final List<String> hosts = getHosts();
        if (hosts != null && !hosts.isEmpty()) {
            final String pluginHost = plugin.getDisplayName();
            if (pluginHost == null) {
                return false;
            }
            for (String host : hosts) {
                if (host != null && host.equalsIgnoreCase(pluginHost)) {
                    return true;
                }
            }
            return false;
        }
        return true;
    }

    /**
     * Check if a plugin matches the filter criteria
     *
     * @param plugin
     *            the plugin to check
     * @return true if the plugin matches all filter criteria, false otherwise
     */
    public boolean matches(LazyHostPlugin plugin) {
        if (plugin == null) {
            return false;
        }
        // Check premium
        final Boolean premium = this.premium;
        if (premium != null && premium.booleanValue() != plugin.isPremium()) {
            return false;
        }
        // Check hosts list
        if (!matchesHost(plugin)) {
            return false;
        }
        // Check features
        if (!matchesFeatures(plugin)) {
            return false;
        }
        return true;
    }
}