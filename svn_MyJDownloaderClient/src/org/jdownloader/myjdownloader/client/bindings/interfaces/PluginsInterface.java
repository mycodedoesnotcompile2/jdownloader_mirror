package org.jdownloader.myjdownloader.client.bindings.interfaces;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.jdownloader.myjdownloader.client.bindings.AdvancedConfigQuery;
import org.jdownloader.myjdownloader.client.bindings.ClientApiNameSpace;
import org.jdownloader.myjdownloader.client.bindings.PluginConfigEntryStorable;
import org.jdownloader.myjdownloader.client.bindings.PluginQuery;
import org.jdownloader.myjdownloader.client.bindings.PluginStorable;

@ClientApiNameSpace(PluginsInterface.NAMESPACE)
public interface PluginsInterface {
    public static final String NAMESPACE = "plugins";

    List<String> getPluginRegex(String URL);

    HashMap<String, ArrayList<String>> getAllPluginRegex();

    List<PluginStorable> list(PluginQuery query);

    boolean setPluginConfigValue(final String interfaceName, final String storageName, String key, final Object newValue);

    List<PluginConfigEntryStorable> query(AdvancedConfigQuery query);
}
