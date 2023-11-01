package org.jdownloader.myjdownloader.client.bindings;

import org.jdownloader.myjdownloader.client.json.AbstractJsonData;

public class PluginQuery extends AbstractJsonData {

    private boolean pattern;
    private boolean version;

    public boolean isPattern() {
        return pattern;
    }

    public void setPattern(boolean pattern) {
        this.pattern = pattern;
    }

    public boolean isVersion() {
        return version;
    }

    public void setVersion(boolean version) {
        this.version = version;
    }

}
