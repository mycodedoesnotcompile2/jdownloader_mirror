package org.jdownloader.plugins.components.hls;

import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.Storable;
import org.appwork.storage.TypeRef;

public class HlsContainerStorable implements Storable {

    public static final TypeRef<HlsContainerStorable> TYPE_REF              = new TypeRef<HlsContainerStorable>() {
                                                                            };

    public static final String                        DOWNLOADLINK_PROPERTY = "hlsContainerStorable";

    public int getWidth() {
        return width;
    }

    public void setWidth(int width) {
        this.width = width;
    }

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height;
    }

    public int getFramerate() {
        return framerate;
    }

    public void setFramerate(int framerate) {
        this.framerate = framerate;
    }

    public int getBandwidth() {
        return bandwidth;
    }

    public void setBandwidth(int bandwidth) {
        this.bandwidth = bandwidth;
    }

    public int getAverageBandwidth() {
        return averageBandwidth;
    }

    public void setAverageBandwidth(int averageBandwidth) {
        this.averageBandwidth = averageBandwidth;
    }

    public int getProgramID() {
        return programID;
    }

    public void setProgramID(int programID) {
        this.programID = programID;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    protected int    width            = -1;
    protected int    height           = -1;
    protected int    framerate        = -1;
    protected int    bandwidth        = -1;
    protected int    averageBandwidth = -1;
    protected int    programID        = -1;
    protected String name             = null;
    protected String streamURL        = null;

    public String getStreamURL() {
        return streamURL;
    }

    public void setStreamURL(String streamURL) {
        this.streamURL = streamURL;
    }

    protected String m3u8URL = null;

    public String getM3u8URL() {
        return m3u8URL;
    }

    public void setM3u8URL(String m3u8url) {
        m3u8URL = m3u8url;
    }

    protected String                          codecs     = null;
    protected String                          audioGroup = null;
    protected List<HlsContainerMediaStorable> media      = new ArrayList<HlsContainerMediaStorable>();

    public List<HlsContainerMediaStorable> getMedia() {
        return media;
    }

    public void setMedia(List<HlsContainerMediaStorable> media) {
        this.media = media;
    }

    public String getAudioGroup() {
        return audioGroup;
    }

    public void setAudioGroup(String audioGroup) {
        this.audioGroup = audioGroup;
    }

    public String getCodecs() {
        return codecs;
    }

    public void setCodecs(String codecs) {
        this.codecs = codecs;
    }

    public HlsContainerStorable() {
    }

    public HlsContainerStorable(HlsContainer container) {
        this.setHeight(container.getHeight());
        this.setWidth(container.getWidth());
        this.setFramerate(container.getFramerate());
        this.setBandwidth(container.getBandwidth());
        this.setAverageBandwidth(container.getAverageBandwidth());
        this.setProgramID(container.getProgramID());
        this.setName(container.getName());
        this.setStreamURL(container.getStreamURL());
        this.setCodecs(container.getCodecs());
        this.setAudioGroup(container.getAudioGroupID());
        this.setM3u8URL(container.getM3U8URL());
    }
}
