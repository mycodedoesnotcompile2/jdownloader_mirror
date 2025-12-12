package org.jdownloader.plugins.components.usenet;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;

import jd.plugins.DownloadLink;
import jd.plugins.download.HashInfo;

import org.appwork.storage.Storable;
import org.appwork.storage.TypeRef;

public class UsenetFile implements Storable {
    private String hash = null;
    private long   size = -1;

    public void _setHashInfo(HashInfo hashInfo) {
        if (hashInfo != null) {
            this.hash = hashInfo.exportAsString();
        } else {
            this.hash = null;
        }
    }

    public HashInfo _getHashInfo() {
        return HashInfo.importFromString(hash);
    }

    public long getSize() {
        return size;
    }

    public void setSize(long size) {
        this.size = size;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getNumSegments() {
        return numSegments;
    }

    public void setNumSegments(int numSegments) {
        this.numSegments = numSegments;
    }

    public ArrayList<UsenetFileSegment> getSegments() {
        return segments;
    }

    public void setSegments(ArrayList<UsenetFileSegment> segments) {
        this.segments = segments;
    }

    private String                       name        = null;
    private int                          numSegments = -1;
    private ArrayList<UsenetFileSegment> segments    = new ArrayList<UsenetFileSegment>();

    public UsenetFile() {
    }

    public static final String  PROPERTY = "useNetFile";
    public static final Charset UTF8     = Charset.forName("UTF-8");

    public static UsenetFile _read(final DownloadLink downloadLink) throws IOException {
        return downloadLink.getCompressedProperty(PROPERTY, new TypeRef<UsenetFile>() {
        });
    }

    public void _write(final DownloadLink downloadLink) throws IOException {
        downloadLink.setCompressedProperty(PROPERTY, this);
    }

    /**
     * @return the hash
     */
    public String getHash() {
        return hash;
    }

    /**
     * @param hash
     *            the hash to set
     */
    public void setHash(String hash) {
        this.hash = hash;
    }
}
