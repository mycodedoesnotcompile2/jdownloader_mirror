package org.jdownloader.myjdownloader.client.json;

public class SessionInfoResponse implements RequestIDValidator {
    private long    rid               = -1;
    private long    creationTimeStamp = -1;
    private boolean alive             = false;
    private String  sessionToken      = null;
    private String  deviceId          = null;
    
    public String getDeviceId() {
        return this.deviceId;
    }
    
    public void setDeviceId(String deviceId) {
        this.deviceId = deviceId;
    }
    
    public String getSessionToken() {
        return this.sessionToken;
    }
    
    public void setSessionToken(String sessionToken) {
        this.sessionToken = sessionToken;
    }
    
    public boolean isAlive() {
        return this.alive;
    }
    
    public void setAlive(boolean alive) {
        this.alive = alive;
    }
    
    public long getLastAccessTimeStamp() {
        return this.lastAccessTimeStamp;
    }
    
    public void setLastAccessTimeStamp(long lastAccessTimeStamp) {
        this.lastAccessTimeStamp = lastAccessTimeStamp;
    }
    
    private long lastAccessTimeStamp = -1;
    
    public long getCreationTimeStamp() {
        return this.creationTimeStamp;
    }
    
    public void setCreationTimeStamp(long creationTimeStamp) {
        this.creationTimeStamp = creationTimeStamp;
    }
    
    public String getAppKey() {
        return this.appKey;
    }
    
    public void setAppKey(String appKey) {
        this.appKey = appKey;
    }
    
    private String appKey = null;
    
    public SessionInfoResponse(/* Storable */) {
    }
    
    /**
     * @return the timestamp
     */
    public long getRid() {
        return this.rid;
    }
    
    /**
     * @param timestamp
     *            the timestamp to set
     */
    public void setRid(final long timestamp) {
        this.rid = timestamp;
    }
    
}
