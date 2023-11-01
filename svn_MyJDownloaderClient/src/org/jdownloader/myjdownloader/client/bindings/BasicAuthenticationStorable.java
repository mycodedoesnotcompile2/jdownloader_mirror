package org.jdownloader.myjdownloader.client.bindings;

import java.util.Locale;

import org.jdownloader.myjdownloader.client.json.AbstractJsonData;

public class BasicAuthenticationStorable extends AbstractJsonData {
    public BasicAuthenticationStorable() {
        // empty const. required for Storable interface
    }

    public static enum Type {
        FTP,
        HTTP
    }

    private long created = System.currentTimeMillis();

    public long getCreated() {
        return created;
    }

    public void setCreated(final long created) {
        this.created = created;
    }

    public long getLastValidated() {
        return lastValidated;
    }

    public void setLastValidated(final long lastValidated) {
        this.lastValidated = lastValidated;
    }

    private long    lastValidated;
    private Boolean enabled;

    public Boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(final String username) {
        this.username = username;
    }

    public Type getType() {
        return type;
    }

    public void setType(final Type type) {
        this.type = type;
    }

    public String getHostmask() {
        return hostmask;
    }

    public void setHostmask(final String hostmask) {
        if (hostmask == null) {
            this.hostmask = null;
        } else {
            this.hostmask = hostmask.toLowerCase(Locale.ENGLISH);
        }
    }

    public long getId() {
        return id;
    }

    public void setId(final long id) {
        this.id = id;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(final String password) {
        this.password = password;
    }

    private String username;
    private String password;
    private Type   type;
    private String hostmask;
    private long   id;
}
