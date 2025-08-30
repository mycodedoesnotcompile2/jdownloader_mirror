package org.jdownloader.controlling.ffmpeg;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import jd.http.Browser;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.Hash;
import org.appwork.utils.IO;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.CrossSystem.OperatingSystem;
import org.jdownloader.controlling.UniqueAlltimeID;
import org.jdownloader.settings.GeneralSettings;

public abstract class FFmpeg extends AbstractFFmpegBinary {
    public FFmpeg(Browser br) {
        super(br);
        final String path = config.getBinaryPath();
        setPath(path);
    }

    public FFmpeg() {
        this(null);
    }

    @Override
    public boolean isCompatible() {
        if (CrossSystem.isWindows() && !CrossSystem.getOS().isMinimum(OperatingSystem.WINDOWS_7)) {
            final String sha256 = Hash.getFileHash(new File(getFullPath()), Hash.HASH_TYPE_SHA256);
            if (StringUtils.equalsIgnoreCase("4d41c2db307db1e6639915aa480b363546ada1990d2fd143680a3673483f3a72", sha256) || StringUtils.equalsIgnoreCase("a26f910d561aa3a6a8e2ce70b5dba79e0639fb93eb77b3c2bf1c915901ecbe3e", sha256)) {
                getLogger().severe("ffmpeg binary(" + getFullPath() + ") requires minimum Windows 7!");
                return false;
            }
        }
        return super.isCompatible();
    }

    public void moveFile(final File dest, final File source) throws IOException {
        if (!source.isFile()) {
            throw new FileNotFoundException("source:" + source.getAbsolutePath());
        } else if (!dest.delete() && dest.exists()) {
            throw new IOException("cannot delete dest:" + dest.getAbsolutePath());
        } else {
            IO.copyFile(source, dest);
        }
    }

    public boolean muxToMkv(FFMpegProgress progress, String out, String videoIn, String audioIn) throws InterruptedException, IOException, FFMpegException {
        return mux(progress, out, videoIn, audioIn, config.getMuxToMkvCommand());
    }

    public boolean muxToMp4(FFMpegProgress progress, String out, String videoIn, String audioIn) throws InterruptedException, IOException, FFMpegException {
        return mux(progress, out, videoIn, audioIn, config.getMuxToMp4Command());
    }

    public boolean generateM4a(FFMpegProgress progress, String out, String audioIn) throws IOException, InterruptedException, FFMpegException {
        return demux(progress, out, audioIn, config.getDash2M4aCommand());
    }

    public boolean generateAac(FFMpegProgress progress, String out, String audioIn) throws InterruptedException, IOException, FFMpegException {
        return demux(progress, out, audioIn, config.getDash2AacCommand());
    }

    public boolean demuxAAC(FFMpegProgress progress, String out, String audioIn) throws InterruptedException, IOException, FFMpegException {
        return demux(progress, out, audioIn, config.getDemux2AacCommand());
    }

    public boolean demuxMp3(FFMpegProgress progress, String out, String audioIn) throws InterruptedException, IOException, FFMpegException {
        return demux(progress, out, audioIn, config.getDemux2Mp3Command());
    }

    private static final Object LOCK = new Object();

    protected void throwFFMpegException(FFMpegException e) throws FFMpegException {
        if (e != null) {
            switch (e.getError()) {
            case DISK_FULL:
            case TOO_OLD:
            case INCOMPATIBLE:
                throw e;
            default:
                break;
            }
        }
    }

    protected boolean demux(FFMpegProgress progress, String out, String audioIn, final String demuxCommand[]) throws InterruptedException, IOException, FFMpegException {
        final LogInterface logger = getLogger();
        synchronized (LOCK) {
            logger.info("Demux:Input=" + audioIn + "|Output=" + out);
            if (StringUtils.equals(out, audioIn)) {
                throw new FFMpegException("demux failed because input file equals output file!");
            }
            final long lastModifiedAudio = new File(audioIn).lastModified();
            final File outFile = new File(out);
            String stdOut = null;
            try {
                stdOut = runCommand(progress, fillCommand(out, null, audioIn, null, demuxCommand));
            } catch (FFMpegException e) {
                throwFFMpegException(e);
                // some systems have problems with special chars to find the in or out file.
                if (FFMpegException.ERROR.PATH_LENGTH.equals(e.getError())) {
                    final File tmpAudioIn = new File(outFile.getParent(), "ffmpeg_audio_in_" + UniqueAlltimeID.create());
                    final File tmpOut = new File(outFile.getParent(), "ffmpeg_out" + UniqueAlltimeID.create());
                    logger.info("Try special char workaround!");
                    logger.info("Replace In:'" + audioIn + "' with '" + tmpAudioIn + "'");
                    logger.info("Replace Out'" + out + "' with '" + tmpOut + "'");
                    boolean okayFlag = false;
                    try {
                        IO.copyFile(new File(audioIn), tmpAudioIn);
                        stdOut = runCommand(progress, fillCommand(tmpOut.getAbsolutePath(), null, tmpAudioIn.getAbsolutePath(), null, demuxCommand));
                        moveFile(tmpOut, outFile);
                        okayFlag = true;
                    } finally {
                        deleteFile(tmpAudioIn);
                        if (!okayFlag) {
                            deleteFile(tmpOut);
                        }
                    }
                } else {
                    throw e;
                }
            }
            if (stdOut != null && outFile.isFile()) {
                try {
                    if (lastModifiedAudio > 0 && JsonConfig.create(GeneralSettings.class).isUseOriginalLastModified()) {
                        outFile.setLastModified(lastModifiedAudio);
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                }
                return true;
            }
            return false;
        }
    }

    protected void deleteFile(File file) {
        if (!file.delete() && file.exists()) {
            file.deleteOnExit();
        }
    }

    public String getVersionString() throws InterruptedException, IOException, FFMpegException {
        final String stdOut = runCommand(null, Arrays.asList(new String[] { getFullPath(), "-version" }));
        final String version = new Regex(stdOut, "ffmpeg version\\s*([^\r\n]+)").getMatch(0);
        return version;
    }

    protected boolean mux(FFMpegProgress progress, String out, String videoIn, String audioIn, final String muxCommand[]) throws InterruptedException, IOException, FFMpegException {
        final LogInterface logger = getLogger();
        synchronized (LOCK) {
            logger.info("Mux:Video=" + videoIn + "|Audio=" + audioIn + "|Output=" + out);
            if (StringUtils.equals(out, videoIn) || StringUtils.equals(out, audioIn)) {
                throw new FFMpegException("demux failed because input file equals output file!");
            }
            final long lastModifiedVideo = new File(videoIn).lastModified();
            final long lastModifiedAudio = new File(audioIn).lastModified();
            final File outFile = new File(out);
            String stdOut = null;
            try {
                stdOut = runCommand(progress, fillCommand(out, videoIn, audioIn, null, muxCommand));
            } catch (FFMpegException e) {
                throwFFMpegException(e);
                // some systems have problems with special chars to find the in or out file.
                if (FFMpegException.ERROR.PATH_LENGTH.equals(e.getError())) {
                    final File tmpAudioIn = new File(outFile.getParent(), "ffmpeg_audio_in_" + UniqueAlltimeID.create());
                    final File tmpVideoIn = new File(outFile.getParent(), "ffmpeg_video_in_" + UniqueAlltimeID.create());
                    final File tmpOut = new File(outFile.getParent(), "ffmpeg_out" + UniqueAlltimeID.create());
                    logger.info("Try special char workaround!");
                    logger.info("Replace In:'" + audioIn + "' with '" + tmpAudioIn + "'");
                    logger.info("Replace In:'" + videoIn + "' with '" + tmpVideoIn + "'");
                    logger.info("Replace Out'" + out + "' with '" + tmpOut + "'");
                    boolean okayFlag = false;
                    try {
                        IO.copyFile(new File(videoIn), tmpVideoIn);
                        IO.copyFile(new File(audioIn), tmpAudioIn);
                        stdOut = runCommand(progress, fillCommand(tmpOut.getAbsolutePath(), tmpVideoIn.getAbsolutePath(), tmpAudioIn.getAbsolutePath(), null, muxCommand));
                        moveFile(tmpOut, outFile);
                        okayFlag = true;
                    } finally {
                        deleteFile(tmpAudioIn);
                        deleteFile(tmpVideoIn);
                        if (!okayFlag) {
                            deleteFile(tmpOut);
                        }
                    }
                } else {
                    throw e;
                }
            }
            if (stdOut != null && outFile.isFile()) {
                try {
                    final long lastModified = Math.max(lastModifiedAudio, lastModifiedVideo);
                    if (lastModified > 0 && JsonConfig.create(GeneralSettings.class).isUseOriginalLastModified()) {
                        outFile.setLastModified(lastModified);
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                }
                return true;
            } else {
                return false;
            }
        }
    }

    public boolean demuxM4a(FFMpegProgress progress, String out, String audioIn) throws InterruptedException, IOException, FFMpegException {
        return demux(progress, out, audioIn, config.getDemux2M4aCommand());
    }

    public List<File> demuxAudio(FFMpegProgress progress, String out, String audioIn) throws IOException, InterruptedException, FFMpegException {
        final LogInterface logger = getLogger();
        synchronized (LOCK) {
            long lastModifiedAudio = new File(audioIn).lastModified();
            ArrayList<File> ret = null;
            ArrayList<String> infoCommand = fillCommand(out, null, audioIn, null, "-i", "%audio");
            try {
                String res = runCommand(null, infoCommand);
                //
            } catch (FFMpegException e) {
                throwFFMpegException(e);
                String[][] audioStreams = new Regex(e.getStdErr(), "Stream \\#0\\:(\\d+)[^\\:]*\\: Audio\\: ([\\w\\d]+)").getMatches();
                int i = 0;
                ret = new ArrayList<File>();
                for (String[] audioStream : audioStreams) {
                    //
                    i++;
                    HashMap<String, String[]> map = new HashMap<String, String[]>();
                    map.put("%map", new String[] { "-map", "0:" + audioStream[0] });
                    audioStream[1] = codecToContainer(audioStream[1]);
                    String tempout = out + "." + i + "." + audioStream[1];
                    String command = null;
                    try {
                        command = runCommand(progress, fillCommand(tempout, null, audioIn, map, config.getDemuxGenericCommand()));
                    } catch (FFMpegException e1) {
                        throwFFMpegException(e1);
                        // some systems have problems with special chars to find the in or out file.
                        if (FFMpegException.ERROR.PATH_LENGTH.equals(e.getError())) {
                            File outFile = new File(tempout);
                            File tmpAudioIn = new File(outFile.getParent(), "ffmpeg_audio_in_" + UniqueAlltimeID.create());
                            File tmpOut = new File(outFile.getParent(), "ffmpeg_out" + UniqueAlltimeID.create());
                            boolean okayFlag = false;
                            try {
                                IO.copyFile(new File(audioIn), tmpAudioIn);
                                command = runCommand(progress, fillCommand(tmpOut.getAbsolutePath(), null, tmpAudioIn.getAbsolutePath(), map, config.getDemuxGenericCommand()));
                                moveFile(tmpOut, outFile);
                                okayFlag = true;
                            } finally {
                                deleteFile(tmpAudioIn);
                                if (!okayFlag) {
                                    deleteFile(tmpOut);
                                }
                            }
                        } else {
                            throw e;
                        }
                    }
                    if (command != null) {
                        if (i > 1) {
                            File f;
                            ret.add(f = new File(tempout));
                            try {
                                if (JsonConfig.create(GeneralSettings.class).isUseOriginalLastModified()) {
                                    f.setLastModified(lastModifiedAudio);
                                }
                            } catch (final Throwable e1) {
                                logger.log(e1);
                            }
                        } else {
                            File f;
                            ret.add(f = new File(out + "." + audioStream[1]));
                            new File(tempout).renameTo(f);
                            try {
                                if (JsonConfig.create(GeneralSettings.class).isUseOriginalLastModified()) {
                                    f.setLastModified(lastModifiedAudio);
                                }
                            } catch (final Throwable e1) {
                                logger.log(e1);
                            }
                        }
                    }
                }
            }
            return ret;
        }
    }

    private String codecToContainer(String codec) {
        if ("aac".equals(codec)) {
            return "m4a";
        }
        return codec;
    }

    public boolean muxToWebm(FFMpegProgress progress, String out, String videoIn, String audioIn) throws InterruptedException, IOException, FFMpegException {
        return mux(progress, out, videoIn, audioIn, config.getMuxToWebmCommand());
    }

    public boolean generateOpusAudio(FFMpegProgress progress, String out, String audioIn) throws IOException, InterruptedException, FFMpegException {
        return demux(progress, out, audioIn, config.getDash2OpusAudioCommand());
    }

    public boolean generateOggAudio(FFMpegProgress progress, String out, String audioIn) throws IOException, InterruptedException, FFMpegException {
        return demux(progress, out, audioIn, config.getDash2OggAudioCommand());
    }

    public boolean generateMkvAudio(FFMpegProgress progress, String out, String audioIn) throws IOException, InterruptedException, FFMpegException {
        return demux(progress, out, audioIn, config.getDash2MkvAudioCommand());
    }
}
