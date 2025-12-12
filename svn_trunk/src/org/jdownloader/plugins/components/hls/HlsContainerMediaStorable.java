package org.jdownloader.plugins.components.hls;

import org.appwork.storage.Storable;
import org.appwork.storage.TypeRef;

public class HlsContainerMediaStorable implements Storable {

    public static final TypeRef<HlsContainerStorable> TYPE_REF = new TypeRef<HlsContainerStorable>() {
                                                               };

    protected String                                  type     = null;

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getGroupID() {
        return groupID;
    }

    public void setGroupID(String groupID) {
        this.groupID = groupID;
    }

    public String getLanguage() {
        return language;
    }

    public void setLanguage(String language) {
        this.language = language;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getUri() {
        return uri;
    }

    public void setUri(String uri) {
        this.uri = uri;
    }

    protected String groupID  = null;
    protected String language = null;
    protected String name     = null;
    protected String uri      = null;

    public HlsContainerMediaStorable() {
    }

    public HlsContainerMediaStorable(HlsContainer.MEDIA media) {
        this.setGroupID(media.getGroupID());
        this.setType(media.getType().toString());
        this.setLanguage(media.getLanguage());
        this.setName(media.getName());
        this.setUri(media.getUri());
    }
}
