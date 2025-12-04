package org.jdownloader.downloader.hls;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.appwork.utils.UniqueAlltimeID;

public class HLSContent {

    protected long id = UniqueAlltimeID.next();

    public long getId() {
        return id;
    }

    protected final M3U8Playlist main;

    public M3U8Playlist getMain() {
        return main;
    }

    public List<M3U8Playlist> getAudioTracks() {
        return audioTracks;
    }

    protected final List<M3U8Playlist> audioTracks = new ArrayList<M3U8Playlist>();

    public HLSContent(M3U8Playlist main) {
        this.main = main;
    }

    public void addAudioTrack(M3U8Playlist audioTrack) {
        this.audioTracks.add(audioTrack);
    }

    public List<M3U8Playlist> list() {
        final List<M3U8Playlist> ret = new ArrayList<M3U8Playlist>();
        ret.add(getMain());
        ret.addAll(getAudioTracks());
        return Collections.unmodifiableList(ret);
    }

    public M3U8Playlist get(int index) {
        if (index == 0) {
            return getMain();
        }
        final List<M3U8Playlist> audioTracks = getAudioTracks();
        if (index - 1 < audioTracks.size()) {
            return audioTracks.get(index - 1);
        }
        return null;
    }

    public int size() {
        return 1/* main */+ getAudioTracks().size()/* audioTracks */;
    }
}
