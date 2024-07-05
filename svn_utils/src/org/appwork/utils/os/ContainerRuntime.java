/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.os;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;

/**
 * @author daniel
 * @date Sep 14, 2020
 *
 */
public class ContainerRuntime {
    public static enum TYPE {
        DOCKER,
        KUBERNETES,
        LXC,
        ECS,
        PODMAN,
        GARDEN,
        WSL,
        WSL2,
        FREEBSD_JAIL
    }

    private final static Object CONTAINER[] = detectContainer();

    public static TYPE getType() {
        if (CONTAINER != null) {
            final Object containerType = CONTAINER[0];
            if (containerType instanceof TYPE) {
                return (TYPE) containerType;
            }
        }
        return null;
    }

    @Deprecated
    public static boolean isInsideDocker() {
        final TYPE containerType = getType();
        if (containerType != null) {
            switch (containerType) {
            case DOCKER:
            case KUBERNETES:
            case ECS:
                return true;
            default:
                return false;
            }
        }
        return false;
    }

    public static boolean isInsideContainer() {
        return CONTAINER != null;
    }

    public static String getID() {
        if (CONTAINER != null) {
            final Object containerID = CONTAINER[1];
            if (containerID == null) {
                return "unknownContainerID";
            } else {
                return String.valueOf(containerID);
            }
        } else {
            return null;
        }
    }

    private static Object[] detectContainer() {
        final Object containerByProc[] = detectContainerByProc();
        if (containerByProc != null) {
            return containerByProc;
        }
        final Object containerByFiles[] = detectContainerByFiles();
        if (containerByFiles != null) {
            return containerByFiles;
        }
        final Object freeBSDJail[] = detectFreeBSDJail();
        if (freeBSDJail != null) {
            return freeBSDJail;
        }
        return null;
    }

    private static Object[] detectFreeBSDJail() {
        if (CrossSystem.OS.FREEBSD.isMinimum(CrossSystem.getOS()) && StringUtils.equals(System.getProperty("user.home"), "/nonexistent")) {
            String hostName = System.getenv("HOST");
            if (StringUtils.isEmpty(hostName)) {
                hostName = System.getenv("HOSTNAME");
            }
            return new Object[] { TYPE.FREEBSD_JAIL, hostName };
        }
        return null;
    }

    private static Object[] detectContainerByFiles() {
        if (CrossSystem.isUnix()) {
            final Map<TYPE, List<String>> containerMap = new HashMap<TYPE, List<String>>();
            containerMap.put(TYPE.DOCKER, Arrays.asList(new String[] { "/run/.dockerenv", "/.dockerenv" }));
            containerMap.put(TYPE.PODMAN, Arrays.asList(new String[] { "/run/.containerenv" }));
            for (final Entry<TYPE, List<String>> container : containerMap.entrySet()) {
                for (final String checkFile : container.getValue()) {
                    if (new File(checkFile).isFile()) {
                        String hostName = System.getenv("HOSTNAME");
                        if (StringUtils.isEmpty(hostName)) {
                            hostName = System.getenv("HOST");
                        }
                        return new Object[] { container.getKey(), hostName };
                    }
                }
            }
        }
        return null;
    }

    private static boolean hasWSLInterop() {
        if (CrossSystem.isUnix()) {
            final File procFile = new File("/proc/sys/fs/binfmt_misc/WSLInterop");
            if (procFile.isFile()) {
                FileInputStream fis = null;
                try {
                    fis = new FileInputStream(procFile);
                    final BufferedReader is = new BufferedReader(new InputStreamReader(fis, "UTF-8"));
                    try {
                        String line = null;
                        while ((line = is.readLine()) != null) {
                            if (line.startsWith("magic 4d5a")) {
                                return true;
                            }
                        }
                    } finally {
                        is.close();
                        fis = null;
                    }
                } catch (final Throwable ignore) {
                    ignore.printStackTrace();
                } finally {
                    if (fis != null) {
                        try {
                            fis.close();
                        } catch (final Throwable ignore) {
                        }
                    }
                }
            }
        }
        return false;
    }

    private static Object[] detectContainerByProc() {
        if (CrossSystem.isUnix()) {
            for (final String procFileString : new String[] { "/proc/1/cgroup", "/proc/self/cgroup", "/proc/version_signature", "/proc/version" }) {
                final File procFile = new File(procFileString);
                if (procFile.isFile()) {
                    FileInputStream fis = null;
                    try {
                        fis = new FileInputStream(procFile);
                        final BufferedReader is = new BufferedReader(new InputStreamReader(fis, "UTF-8"));
                        try {
                            String line = null;
                            while ((line = is.readLine()) != null) {
                                if (procFileString.startsWith("/proc/version")) {
                                    if (line.matches("(?i).*-WSL(1|2)?\\s+.*") && line.matches("(?i).*Microsoft.*")) {
                                        if (line.matches("(?i).*-WSL2\\s+.*")) {
                                            return new Object[] { TYPE.WSL2, System.getenv("WSL_DISTRO_NAME") };
                                        } else {
                                            return new Object[] { TYPE.WSL, System.getenv("WSL_DISTRO_NAME") };
                                        }
                                    }
                                } else if (procFileString.endsWith("/cgroup")) {
                                    // 5:net_cls:/system.slice/docker-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.scope
                                    // 5:net_prio,net_cls:/docker/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                                    final String docker[] = new Regex(line, ":/(docker/|system.slice/docker-)([\\p{XDigit}\\-]{1,64})(?:.scope|$)").getRow(0);
                                    if (docker != null) {
                                        return new Object[] { TYPE.DOCKER, docker[1] };
                                    }
                                    final String ecs[] = new Regex(line, ":/(ecs/|ecs/[^/]+/)([\\p{XDigit}\\-]{1,64})(?:.scope|$)").getRow(0);
                                    if (ecs != null) {
                                        return new Object[] { TYPE.ECS, ecs[1] };
                                    }
                                    // net_cls,net_prio:/kubepods/besteffort/aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                                    final String kubepods[] = new Regex(line, ":/(kube.*?)/.*?([\\p{XDigit}\\-]{1,64})(?:.scope|$)").getRow(0);
                                    if (kubepods != null) {
                                        return new Object[] { TYPE.KUBERNETES, kubepods[1] };
                                    }
                                    final String podman[] = new Regex(line, ":/(podman/|[^/]*/libpod-)([\\p{XDigit}\\-]{1,64})(?:.scope|$)").getRow(0);
                                    if (podman != null) {
                                        return new Object[] { TYPE.PODMAN, podman[1] };
                                    }
                                    // 5:net_cls,net_prio:/lxc/test-name
                                    final String lxc[] = new Regex(line, ":/(lxc(?:-libvirt)?)/([^/+]+)").getRow(0);
                                    if (lxc != null) {
                                        return new Object[] { TYPE.LXC, lxc[1] };
                                    }
                                }
                            }
                        } finally {
                            is.close();
                            fis = null;
                        }
                    } catch (final Throwable ignore) {
                        ignore.printStackTrace();
                    } finally {
                        if (fis != null) {
                            try {
                                fis.close();
                            } catch (final Throwable ignore) {
                            }
                        }
                    }
                }
            }
            if (hasWSLInterop()) {
                if (new File("/run/WSL").isDirectory()) {
                    return new Object[] { TYPE.WSL2, System.getenv("WSL_DISTRO_NAME") };
                } else {
                    return new Object[] { TYPE.WSL, System.getenv("WSL_DISTRO_NAME") };
                }
            }
        }
        return null;
    }
}
