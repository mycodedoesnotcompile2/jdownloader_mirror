package jd.http;

public interface BlockedTypeInterface {
    public BlockedTypeInterface isBlocked(Browser browser, Request request);

    public String getLabel();

    public BlockLevelType getBlockLevelType();

    public BlockSourceType getBlockSourceType();

    public Boolean prepareBlockDetection(Browser browser, Request request);
}
