package jd.controlling.downloadcontroller;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.os.ContainerRuntime;
import org.appwork.utils.os.hardware.HardwareType;
import org.appwork.utils.os.hardware.HardwareTypeInterface;
import org.jdownloader.logging.LogController;
import org.jdownloader.settings.GeneralSettings;

public class DiskSpaceManager {
    public static enum DISKSPACERESERVATIONRESULT {
        UNSUPPORTED,
        OK,
        INVALIDDESTINATION,
        FAILED
    }

    private final List<DiskSpaceChecker> reservations = new ArrayList<DiskSpaceChecker>();
    private final GeneralSettings        config;

    public DiskSpaceManager() {
        config = JsonConfig.create(GeneralSettings.class);
    }

    public synchronized DISKSPACERESERVATIONRESULT check(DiskSpaceReservation reservation) {
        return checkAndReserve(reservation, null);
    }

    private DISKSPACERESERVATIONRESULT handle(final DiskSpaceChecker checker, final DISKSPACERESERVATIONRESULT result, final Long requestedDiskSpace, final Long usableSpace) {
        final DiskSpaceReservation reservation = checker.getDiskSpaceReservation();
        final LogInterface logger = reservation.getLogger();
        logger.info("DiskSpaceManager:Result:" + result + "|File:" + reservation.getDestination() + "|Root(s):" + checker.getRoots() + "|Requestor:" + checker.getRequestor() + "|RequestedSpace:" + (requestedDiskSpace != null ? SizeFormatter.formatBytes(requestedDiskSpace.longValue()) : null) + "|UsableSpace:" + (usableSpace != null ? SizeFormatter.formatBytes(usableSpace.longValue()) : null));
        return result;
    }

    /**
     * Returns true if the checker resolved to a valid root: a root that is an existing directory, or that equals the reservation
     * destination itself.
     */
    private boolean isValidRoot(final DiskSpaceChecker checker) {
        final String bestRootMatch = checker.getRoot();
        if (bestRootMatch == null) {
            return false;
        }
        final File rootFile = new File(bestRootMatch);
        return rootFile.isDirectory() || rootFile.equals(checker.getDiskSpaceReservation().getDestination());
    }

    public synchronized DISKSPACERESERVATIONRESULT checkAndReserve(final DiskSpaceReservation reservation, final Object requestor) {
        if (reservation == null) {
            throw new IllegalArgumentException("reservation must not be null!");
        }
        final DiskSpaceChecker checker;
        if (JVMVersion.isMinimum(JVMVersion.JAVA_1_7)) {
            checker = new DiskSpaceChecker17(reservation, requestor);
        } else {
            checker = new DiskSpaceChecker(reservation, requestor);
        }
        if (!config.isFreeSpaceCheckEnabled()) {
            /* Free space check disabled in config. */
            return handle(checker, DISKSPACERESERVATIONRESULT.OK, null, null);
        } else if (reservation.getDestination() == null) {
            return handle(checker, DISKSPACERESERVATIONRESULT.INVALIDDESTINATION, null, null);
        }
        if (!isValidRoot(checker)) {
            return handle(checker, DISKSPACERESERVATIONRESULT.INVALIDDESTINATION, null, null);
        }
        try {
            final HardwareTypeInterface hardwareType = HardwareType.getHardware();
            if (hardwareType != null && !ContainerRuntime.isInsideDocker()) {
                switch (hardwareType.getHardwareType()) {
                case QNAP:
                case SYNOLOGY:
                    if (checker.isSameRoot("/")) {
                        return handle(checker, DISKSPACERESERVATIONRESULT.INVALIDDESTINATION, null, null);
                    }
                    break;
                default:
                    break;
                }
            }
        } catch (final Throwable e) {
            reservation.getLogger().log(e);
        }
        final long forcedFreeSpaceOnDisk = Math.max(0l, config.getForcedFreeSpaceOnDisk() * 1024l * 1024l);
        long requestedDiskSpace = Math.max(0, reservation.getSize()) + forcedFreeSpaceOnDisk;
        for (final DiskSpaceChecker reservedDiskSpace : reservations) {
            if (reservedDiskSpace.isSameRoot(checker)) {
                requestedDiskSpace += Math.max(0, reservedDiskSpace.getSize());
            }
        }
        final long freeDiskSpace = checker.getUsableSpace();
        // freeDiskSpace <0 -> unlimited, for example a virtual (distributed) filesystem
        if (freeDiskSpace >= 0 && freeDiskSpace < requestedDiskSpace) {
            return handle(checker, DISKSPACERESERVATIONRESULT.FAILED, requestedDiskSpace, freeDiskSpace);
        } else {
            if (requestor != null) {
                reservations.add(checker);
            }
            return handle(checker, DISKSPACERESERVATIONRESULT.OK, requestedDiskSpace, freeDiskSpace);
        }
    }

    public synchronized long getReservedDiskSpace(final File path, final Object requestor) {
        final DiskSpaceReservation reservation = new DiskSpaceReservation() {
            @Override
            public Object getOwner() {
                return null;
            }

            @Override
            public LogInterface getLogger() {
                return LogController.CL(true);
            }

            @Override
            public File getDestination() {
                return path;
            }

            @Override
            public long getSize() {
                return 0;
            }
        };
        final DiskSpaceChecker checker;
        if (JVMVersion.isMinimum(JVMVersion.JAVA_1_7)) {
            checker = new DiskSpaceChecker17(reservation, requestor);
        } else {
            checker = new DiskSpaceChecker(reservation, requestor);
        }
        if (!isValidRoot(checker)) {
            return -1;
        }
        long requestedDiskSpace = Math.max(0, reservation.getSize());
        for (final DiskSpaceChecker reservedDiskSpace : reservations) {
            if (reservedDiskSpace.isSameRoot(checker)) {
                requestedDiskSpace += Math.max(0, reservedDiskSpace.getSize());
            }
        }
        return requestedDiskSpace;
    }

    public synchronized boolean free(final DiskSpaceReservation reservation, final Object requestor) {
        final DiskSpaceChecker reservedDiskSpace = getDiskSpaceChecker(reservation);
        return reservedDiskSpace != null && reservedDiskSpace.getRequestor() == requestor && reservations.remove(reservedDiskSpace);
    }

    public synchronized boolean isReservedBy(final DiskSpaceReservation reservation, final Object requestor) {
        final DiskSpaceChecker reservedDiskSpace = getDiskSpaceChecker(reservation);
        return reservedDiskSpace != null && reservedDiskSpace.getRequestor() == requestor;
    }

    public synchronized boolean holdsReservations(final Object requestor) {
        for (final DiskSpaceChecker reservedDiskSpace : reservations) {
            if (reservedDiskSpace.getRequestor() == requestor) {
                return true;
            }
        }
        return false;
    }

    public synchronized void freeAllReservationsBy(final Object requestor) {
        final Iterator<DiskSpaceChecker> it = reservations.iterator();
        while (it.hasNext()) {
            final DiskSpaceChecker reservedDiskSpace = it.next();
            if (reservedDiskSpace != null && reservedDiskSpace.getRequestor() == requestor) {
                it.remove();
            }
        }
    }

    private synchronized DiskSpaceChecker getDiskSpaceChecker(final DiskSpaceReservation reservation) {
        if (reservation == null) {
            return null;
        }
        for (final DiskSpaceChecker reservedDiskSpace : reservations) {
            if (reservedDiskSpace.getDiskSpaceReservation() == reservation) {
                return reservedDiskSpace;
            }
        }
        return null;
    }

    public synchronized boolean isReserved(final DiskSpaceReservation reservation) {
        return getDiskSpaceChecker(reservation) != null;
    }
}
