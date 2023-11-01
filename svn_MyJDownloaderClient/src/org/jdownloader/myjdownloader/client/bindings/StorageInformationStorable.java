package org.jdownloader.myjdownloader.client.bindings;

public class StorageInformationStorable {

    private String path  = null;
    private String error = null;

    public String getError() {
        return this.error;
    }

    public void setError(String error) {
        this.error = error;
    }

    public String getPath() {
        return this.path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public long getSize() {
        return this.size;
    }

    public void setSize(long size) {
        if (size < 0) {
            this.size = -1;
        } else {
            this.size = size;
        }
    }

    public long getFree() {
        return this.free;
    }

    public void setFree(long free) {
        if (free < 0) {
            // unlimited, for example a virtual (distributed) filesystem
            this.free = -1;
        } else {
            this.free = free;
        }
    }

    private long size = -1;
    private long free = -1;

}
