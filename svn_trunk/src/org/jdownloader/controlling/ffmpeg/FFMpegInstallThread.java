package org.jdownloader.controlling.ffmpeg;

import java.io.File;
import java.util.Locale;

import org.appwork.storage.config.JsonConfig;
import org.appwork.uio.CloseReason;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.CrossSystem.OperatingSystem;
import org.jdownloader.logging.LogController;
import org.jdownloader.updatev2.UpdateController;

public class FFMpegInstallThread extends Thread {
    private static enum ExtensionPackages {
        MAC_BIG_SUR_AS("ffmpeg_as_11+") {
            // https://ffmpeg.martin-riedl.de/
            @Override
            protected boolean isSupported() {
                if (MAC_CATALINA.isSupported()) {
                    // MAC_CATALINA is installed, no need to install (while Rosetta 2(to be removed with macOS27) support still exists)
                    // Apple Silicon again
                    final File file = MAC_CATALINA.getBundledBinaryPath(BINARY.FFMPEG);
                    if (isFile(file)) {
                        return false;
                    }
                }
                // MAC_BIG_SUR is first Apple Silicon
                return CrossSystem.getOS().isMinimum(OperatingSystem.MAC_BIG_SUR) && CrossSystem.ARCHFamily.ARM.equals(CrossSystem.getARCHFamily());
            }

            @Override
            protected File getBundledBinaryPath(ExtensionPackages extension, BINARY binary) {
                final String binaryName = binary.name().toLowerCase(Locale.ENGLISH);
                return Application.getResource("tools/mac/" + extensionID + "/" + binaryName);
            }
        },
        MAC_CATALINA("ffmpeg_10.10+") {
            // old extension/folder extensionID kept for compatibility
            // https://ffmpeg.martin-riedl.de/
            @Override
            protected boolean isSupported() {
                // intel package is compatible to apple silicon because of rosetta 2 support
                return CrossSystem.getOS().isMinimum(OperatingSystem.MAC_CATALINA);
            }

            @Override
            protected File getBundledBinaryPath(ExtensionPackages extension, BINARY binary) {
                final String binaryName = binary.name().toLowerCase(Locale.ENGLISH);
                return Application.getResource("tools/mac/" + extensionID + "/" + binaryName);
            }
        },
        MAC_HIGH_SIERRA_MOJAVE("ffmpeg_10.13-14") {
            // https://evermeet.cx/ffmpeg/
            // https://evermeet.cx/ffmpeg/note-2026-03-29
            @Override
            protected boolean isSupported() {
                return CrossSystem.getOS().isMaximum(OperatingSystem.MAC_MOJAVE) && CrossSystem.getOS().isMinimum(OperatingSystem.MAC_HIGH_SIERRA);
            }

            @Override
            protected File getBundledBinaryPath(ExtensionPackages extension, BINARY binary) {
                final String binaryName = binary.name().toLowerCase(Locale.ENGLISH);
                return Application.getResource("tools/mac/" + extensionID + "/" + binaryName);
            }
        },
        MAC_SNOW_LEOPOARD("ffmpeg_10.6+") {
            // no longer updated/maintained
            @Override
            protected boolean isSupported() {
                return CrossSystem.getOS().isMinimum(OperatingSystem.MAC_SNOW_LEOPOARD);
            }

            @Override
            protected File getBundledBinaryPath(ExtensionPackages extension, BINARY binary) {
                final String binaryName = binary.name().toLowerCase(Locale.ENGLISH);
                return Application.getResource("tools/mac/" + extensionID + "/" + binaryName);
            }
        },
        MAC_LEOPOARD("ffmpeg_10.5.x-") {
            // no longer updated/maintained
            @Override
            protected boolean isSupported() {
                return CrossSystem.isMac() && ((!CrossSystem.getOS().isMinimum(OperatingSystem.MAC_SNOW_LEOPOARD) || !CrossSystem.is64BitOperatingSystem()));
            }

            @Override
            protected File getBundledBinaryPath(ExtensionPackages extension, BINARY binary) {
                final String binaryName = binary.name().toLowerCase(Locale.ENGLISH);
                return Application.getResource("tools/mac/" + extensionID + "/" + binaryName);
            }
        },
        LINUX("ffmpeg") {
            @Override
            protected boolean isSupported() {
                return CrossSystem.isLinux() && CrossSystem.ARCHFamily.X86.equals(CrossSystem.getARCHFamily()) && CrossSystem.is64BitOperatingSystem();
            }

            @Override
            protected File getBundledBinaryPath(ExtensionPackages extension, BINARY binary) {
                return GENERIC.getBundledBinaryPath(this, binary);
            }
        },
        WINDOWS_7_to_11("ffmpeg") {
            // https://www.ffmpeg.download/
            // https://www.gyan.dev/ffmpeg/builds/
            // https://github.com/defisym/FFmpeg-Builds-Win32
            @Override
            protected boolean isSupported() {
                final OperatingSystem os = CrossSystem.getOS();
                if (os.isMinimum(OperatingSystem.WINDOWS_10)) {
                    // Windows 10 ,Windows 11
                    // for 32/64/arm64
                    return true;
                } else if (os.isMaximum(OperatingSystem.WINDOWS_8) && os.isMinimum(OperatingSystem.WINDOWS_7) && CrossSystem.is64BitOperatingSystem()) {
                    // Windows 7, Windows 8
                    // for 64bit (required UCRT installed)
                    return true;
                } else {
                    // older Windows (XP, Vista, 7, 8) are no longer supported by newer ffmpeg builds
                    return false;
                }
            }

            @Override
            protected File getBundledBinaryPath(ExtensionPackages extension, BINARY binary) {
                return GENERIC.getBundledBinaryPath(this, binary);
            }
        },
        GENERIC(null) {
            @Override
            protected boolean isSupported() {
                switch (CrossSystem.getOSFamily()) {
                case LINUX:
                    return true;
                case MAC:
                    return true;
                case WINDOWS:
                    return true;
                case BSD:
                    return true;
                default:
                    return false;
                }
            }

            @Override
            protected File getBundledBinaryPath(ExtensionPackages extension, BINARY binary) {
                String binaryName = binary.name().toLowerCase(Locale.ENGLISH);
                final String os;
                switch (CrossSystem.getOSFamily()) {
                case LINUX:
                    os = "linux";
                    break;
                case MAC:
                    os = "mac";
                    break;
                case WINDOWS:
                    os = "Windows";// legacy uppercase W, but doesn't really matter
                    binaryName = binaryName + ".exe";
                    break;
                case BSD:
                    os = "bsd";
                    break;
                default:
                    return null;
                }
                final boolean is64BitOperatingSystem = CrossSystem.is64BitOperatingSystem();
                final String extensionID = (extension != null && extension.extensionID != null) ? extension.extensionID : "ffmpeg";
                switch (CrossSystem.getARCHFamily()) {
                case X86:
                    if (is64BitOperatingSystem) {
                        return Application.getResource("tools/" + os + "/" + extensionID + "/x64/" + binaryName);
                    } else {
                        return Application.getResource("tools/" + os + "/" + extensionID + "/i386/" + binaryName);
                    }
                case ARM:
                    if (is64BitOperatingSystem) {
                        return Application.getResource("tools/" + os + "/" + extensionID + "/arm64/" + binaryName);
                    } else {
                        return Application.getResource("tools/" + os + "/" + extensionID + "/arm/" + binaryName);
                    }
                case PPC:
                    if (is64BitOperatingSystem) {
                        return Application.getResource("tools/" + os + "/" + extensionID + "/ppc64/" + binaryName);
                    } else {
                        return Application.getResource("tools/" + os + "/" + extensionID + "/ppc/" + binaryName);
                    }
                case RISCV:
                    if (is64BitOperatingSystem) {
                        return Application.getResource("tools/" + os + "/" + extensionID + "/riscv64/" + binaryName);
                    } else {
                        return Application.getResource("tools/" + os + "/" + extensionID + "/riscv32/" + binaryName);
                    }
                default:
                    return null;
                }
            }
        };

        protected abstract boolean isSupported();

        protected File getBundledBinaryPath(BINARY binary) {
            return getBundledBinaryPath(this, binary);
        }

        protected abstract File getBundledBinaryPath(ExtensionPackages extension, BINARY binary);

        public static ExtensionPackages getExtension() {
            for (final ExtensionPackages extension : values()) {
                if (extension.isSupported()) {
                    return extension;
                }
            }
            return null;
        }

        protected final String extensionID;

        private ExtensionPackages(final String extensionID) {
            this.extensionID = extensionID;
        }
    }

    /**
     *
     */
    private final FFmpegProvider fFmpegProvider;
    private volatile long        progress = -1;
    private volatile boolean     success  = false;
    private final String         task;

    public static enum BINARY {
        FFMPEG,
        FFPROBE
    }

    public FFMpegInstallThread(FFmpegProvider fFmpegProvider, String task) {
        this.fFmpegProvider = fFmpegProvider;
        this.task = task;
    }

    public boolean isSuccessFul() {
        return success;
    }

    public long getProgress() {
        return progress;
    }

    private static boolean isFile(File file) {
        if (CrossSystem.isMac()) {
            // File.isFile may fail on MacOS
            return file != null && file.exists() && !file.isDirectory();
        } else {
            return file != null && file.isFile();
        }
    }

    public static String getFFmpegExtensionName() {
        final ExtensionPackages extension = ExtensionPackages.getExtension();
        return extension != null ? extension.extensionID : null;
    }

    @Override
    public void run() {
        try {
            final ExtensionPackages extension = ExtensionPackages.getExtension();
            File ffmpeg = extension != null ? extension.getBundledBinaryPath(BINARY.FFMPEG) : null;
            if (!isFile(ffmpeg)) {
                ffmpeg = null;
            }
            File ffprobe = extension != null ? extension.getBundledBinaryPath(BINARY.FFPROBE) : null;
            if (!isFile(ffprobe)) {
                ffprobe = null;
            }
            if (ffmpeg == null || ffprobe == null) {
                if (extension != null) {
                    final LogInterface logger = LogController.CL();
                    if (logger != null) {
                        logger.info("FFMpegInstallThread:" + extension + "|ExtensionID:" + extension.extensionID + "|FFmpeg:" + ffmpeg + "|FFprobe:" + ffprobe);
                    }
                    final String extensionID = extension.extensionID;
                    if (extensionID != null) {
                        final ConfirmDialogInterface res = UIOManager.I().show(ConfirmDialogInterface.class, new FFMpegInstallTypeChooserDialog(task));
                        if (res.getCloseReason() == CloseReason.OK) {
                            UpdateController.getInstance().setGuiVisible(true);
                            try {
                                if (UpdateController.getInstance().isExtensionInstalled(extensionID)) {
                                    UpdateController.getInstance().runExtensionUnInstallation(extensionID);
                                }
                                UpdateController.getInstance().runExtensionInstallation(extensionID);
                            } catch (InterruptedException e) {
                                e.printStackTrace();
                            }
                        }
                    }
                }
            }
            ffmpeg = extension != null ? extension.getBundledBinaryPath(BINARY.FFMPEG) : null;
            if (isFile(ffmpeg)) {
                JsonConfig.create(FFmpegSetup.class).setBinaryPath(ffmpeg.getAbsolutePath());
            } else {
                ffmpeg = null;
            }
            ffprobe = extension != null ? extension.getBundledBinaryPath(BINARY.FFPROBE) : null;
            if (isFile(ffprobe)) {
                JsonConfig.create(FFmpegSetup.class).setBinaryPathProbe(ffprobe.getAbsolutePath());
            } else {
                ffprobe = null;
            }
            success = ffmpeg != null && ffprobe != null;
        } finally {
            if (this.fFmpegProvider != null) {
                synchronized (this.fFmpegProvider) {
                    this.fFmpegProvider.installThread = null;
                }
            }
        }
    }
}