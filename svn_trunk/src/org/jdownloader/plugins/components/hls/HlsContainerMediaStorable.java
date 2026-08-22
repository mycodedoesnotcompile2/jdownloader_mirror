package org.jdownloader.plugins.components.hls;

import java.io.IOException;
import java.net.URL;
import java.util.List;

import org.appwork.storage.Storable;
import org.appwork.storage.TypeRef;
import org.jdownloader.plugins.components.hls.HlsContainer.MEDIA.TYPE;
import org.jdownloader.plugins.components.hls.HlsContainer.MEDIAInterface;

public class HlsContainerMediaStorable implements Storable, MEDIAInterface {
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

    protected String       groupID  = null;
    protected String       language = null;
    protected String       name     = null;
    protected String       uri      = null;
    protected List<String> characteristics;

    public List<String> getCharacteristics() {
        return characteristics;
    }

    public void setCharacteristics(List<String> characteristics) {
        this.characteristics = characteristics;
    }

    public String getAbsoluteURL() {
        return absoluteURL;
    }

    public void setAbsoluteURL(String absoluteURL) {
        this.absoluteURL = absoluteURL;
    }

    protected String absoluteURL = null;

    public HlsContainerMediaStorable() {
    }

    public boolean hasCharacteristics(final String lookup) {
        final List<String> characteristics = getCharacteristics();
        return lookup != null && characteristics != null && characteristics.contains(lookup);
    }

    public HlsContainerMediaStorable(MEDIAInterface media) throws IOException {
        this.setGroupID(media.getGroupID());
        this.setType(media._getType().toString());
        this.setLanguage(media.getLanguage());
        this.setName(media.getName());
        this.setUri(media.getUri());
        this.setCharacteristics(media.getCharacteristics());
        final URL url = media._getAbsoluteURL();
        this.setAbsoluteURL(url == null ? null : url.toExternalForm());
    }

    @Override
    public TYPE _getType() {
        final String type = getType();
        if (type == null) {
            return null;
        }
        return TYPE.valueOf(type);
    }

    @Override
    public URL _getAbsoluteURL() throws IOException {
        final String url = getAbsoluteURL();
        if (url == null) {
            return null;
        }
        return new URL(url);
    }
}
