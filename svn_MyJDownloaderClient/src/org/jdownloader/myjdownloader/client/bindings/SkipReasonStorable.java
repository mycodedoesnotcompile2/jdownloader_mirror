package org.jdownloader.myjdownloader.client.bindings;

public class SkipReasonStorable {
    public enum Reason {
        CONNECTION_UNAVAILABLE,
        TOO_MANY_RETRIES,
        CAPTCHA,
        MANUAL,
        DISK_FULL,
        NO_ACCOUNT,
        INVALID_DESTINATION,
        FILE_EXISTS,
        UPDATE_RESTART_REQUIRED,
        FFMPEG_MISSING,
        FFPROBE_MISSING;
    }
}