package org.jdownloader.plugins.controller.host;

import java.util.Arrays;
import java.util.List;

import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;

/**
 * Filter class to query plugins with specific criteria
 */
public class LazyHostPluginFilter {
    private List<String>  hosts         = null;
    private Boolean       premium       = null;
    private List<FEATURE> features      = null;
    private Integer       maxResultsNum = null;

    /**
     * Creates a new plugin filter with no criteria (matches all plugins)
     */
    public LazyHostPluginFilter() {
        // Default constructor with no criteria
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