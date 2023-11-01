package org.jdownloader.myjdownloader.client.bindings;

public class SystemInformationStorable {

    private long    heapUsed = -1;
    private String  hardware = null;

    private boolean docker   = false;
    
    public boolean isDocker() {
        return this.docker;
    }
    
    public void setDocker(boolean docker) {
        this.docker = docker;
    }
    
    private boolean snap = false;

    public boolean isSnap() {
        return this.snap;
    }
    
    public void setSnap(boolean snap) {
        this.snap = snap;
    }
    
    public String getHardware() {
        return this.hardware;
    }
    
    public void setHardware(String hardware) {
        this.hardware = hardware;
    }
    
    public long getHeapUsed() {
        return this.heapUsed;
    }

    public void setHeapUsed(long heapUsed) {
        this.heapUsed = heapUsed;
    }

    public long getHeapCommitted() {
        return this.heapCommitted;
    }

    public void setHeapCommitted(long heapCommitted) {
        this.heapCommitted = heapCommitted;
    }

    public long getHeapMax() {
        return this.heapMax;
    }

    public void setHeapMax(long heapMax) {
        this.heapMax = heapMax;
    }

    private long    heapCommitted    = -1;
    private long    heapMax          = -1;

    private boolean os64Bit          = false;
    private boolean arch64Bit        = false;

    private long    startupTimeStamp = -1;

    public long getStartupTimeStamp() {
        return this.startupTimeStamp;
    }

    public void setStartupTimeStamp(long startupTimeStamp) {
        this.startupTimeStamp = startupTimeStamp;
    }

    public boolean isOs64Bit() {
        return this.os64Bit;
    }

    public void setOs64Bit(boolean os64Bit) {
        this.os64Bit = os64Bit;
    }

    public boolean isJvm64Bit() {
        return this.jvm64Bit;
    }

    public void setJvm64Bit(boolean jvm64Bit) {
        this.jvm64Bit = jvm64Bit;
    }

    private boolean jvm64Bit   = false;

    private String  archFamily = null;
    private String  archString = null;

    public String getArchFamily() {
        return this.archFamily;
    }

    public void setArchFamily(String archFamily) {
        this.archFamily = archFamily;
    }

    public String getOperatingSystem() {
        return this.operatingSystem;
    }

    public void setOperatingSystem(String operatingSystem) {
        this.operatingSystem = operatingSystem;
    }

    public String getOsFamily() {
        return this.osFamily;
    }

    public void setOsFamily(String osFamily) {
        this.osFamily = osFamily;
    }

    public boolean isHeadless() {
        return this.isHeadless;
    }

    public void setHeadless(boolean isHeadless) {
        this.isHeadless = isHeadless;
    }

    public String getJavaVersionString() {
        return this.javaVersionString;
    }

    public void setJavaVersionString(String javaVersionString) {
        this.javaVersionString = javaVersionString;
    }

    public long getJavaVersion() {
        return this.javaVersion;
    }

    public void setJavaVersion(long javaVersion) {
        this.javaVersion = javaVersion;
    }

    /**
     * @return the osString
     */
    public String getOsString() {
        return this.osString;
    }

    /**
     * @param osString
     *            the osString to set
     */
    public void setOsString(String osString) {
        this.osString = osString;
    }

    /**
     * @return the archString
     */
    public String getArchString() {
        return this.archString;
    }

    /**
     * @param archString
     *            the archString to set
     */
    public void setArchString(String archString) {
        this.archString = archString;
    }

    /**
     * @return the arch64Bit
     */
    public boolean isArch64Bit() {
        return this.arch64Bit;
    }

    /**
     * @param arch64Bit
     *            the arch64Bit to set
     */
    public void setArch64Bit(boolean arch64Bit) {
        this.arch64Bit = arch64Bit;
    }

    private String  operatingSystem   = null;
    private String  osFamily          = null;
    private String  osString          = null;
    private boolean isHeadless        = false;

    private String  javaVersionString = null;
    private String  javaVendor        = null;
    private String  javaName          = null;

    public String getJavaName() {
        return this.javaName;
    }

    public void setJavaName(String javaName) {
        this.javaName = javaName;
    }

    public String getJavaVendor() {
        return this.javaVendor;
    }

    public void setJavaVendor(String javaVendor) {
        this.javaVendor = javaVendor;
    }

    private long javaVersion = -1;

}
