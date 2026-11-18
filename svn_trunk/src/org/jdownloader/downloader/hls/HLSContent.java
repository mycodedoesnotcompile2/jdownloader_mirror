package org.jdownloader.downloader.hls;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.utils.UniqueAlltimeID;
import org.jdownloader.plugins.components.hls.HlsContainer.MEDIA;

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
        final List<M3U8Playlist> ret = new ArrayList<M3U8Playlist>();
        for (Entry<M3U8Playlist, MEDIA> media : mediaPlayLists.entrySet()) {
            if (media.getValue() == null || MEDIA.TYPE.AUDIO.equals(media.getValue()._getType())) {
                ret.add(media.getKey());
            }
        }
        return ret;
    }

    public List<M3U8Playlist> getSubtitles() {
        final List<M3U8Playlist> ret = new ArrayList<M3U8Playlist>();
        for (Entry<M3U8Playlist, MEDIA> media : mediaPlayLists.entrySet()) {
            if (media.getValue() != null && MEDIA.TYPE.SUBTITLES.equals(media.getValue()._getType())) {
                ret.add(media.getKey());
            }
        }
        return ret;
    }

    public List<M3U8Playlist> getMediaPlayLists() {
        return new ArrayList<M3U8Playlist>(mediaPlayLists.keySet());
    }

    protected final Map<M3U8Playlist, MEDIA> mediaPlayLists = new LinkedHashMap<M3U8Playlist, MEDIA>();

    public HLSContent(M3U8Playlist main) {
        this.main = main;
    }

    public boolean add(MEDIA media, M3U8Playlist mediaPlaylist) {
        switch (media._getType()) {
        case AUDIO:
            this.mediaPlayLists.put(mediaPlaylist, media);
            return true;
        case SUBTITLES:
            this.mediaPlayLists.put(mediaPlaylist, media);
            return true;
        default:
            return false;
        }
    }

    public List<M3U8Playlist> list() {
        final List<M3U8Playlist> ret = new ArrayList<M3U8Playlist>();
        ret.add(getMain());
        ret.addAll(getAudioTracks());
        return Collections.unmodifiableList(ret);
    }

    public MEDIA getMediaFor(M3U8Playlist playlist) {
        final MEDIA ret = mediaPlayLists.get(playlist);
        if (ret != null) {
            return ret;
        }
        return null;
    }

    public M3U8Playlist get(final int index) {
        if (index == 0) {
            return getMain();
        } else if (index < 0) {
            return null;
        }
        final List<M3U8Playlist> mediaPlayLists = getMediaPlayLists();
        final int mediaIndex = index - 1;
        if (mediaIndex < mediaPlayLists.size()) {
            return mediaPlayLists.get(mediaIndex);
        }
        return null;
    }

    public int size() {
        return 1/* main */+ getMediaPlayLists().size()/* audioTracks/subTitles */;
    }
}
