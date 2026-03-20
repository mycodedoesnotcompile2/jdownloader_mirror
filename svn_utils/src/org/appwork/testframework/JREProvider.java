/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
package org.appwork.testframework;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.zip.GZIPInputStream;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.net.DownloadProgress;
import org.appwork.utils.net.httpclient.HttpClient.RequestContext;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.CrossSystem.ARCHFamily;
import org.appwork.utils.os.CrossSystem.OSFamily;
import org.appwork.utils.zip.ZipIOReader;

/**
 * Provides JRE lookup and download for tests. Resolves a requested Java version to a local JRE path by searching cache, downloading from
 * Adoptium/Azul, or prompting for a local path. Used by {@link JREExecuter} for execution; this class does not run code in the JRE.
 * <p>
 * Download sources:
 * <ul>
 * <li>Adoptium (Eclipse Temurin) - Primary source for Java 8, 11, 17, 21+</li>
 * <li>Azul Zulu API - Fallback source with wider version coverage</li>
 * <li>Azul Zulu CDN (cdn.azul.com/zulu/bin/) - Direct downloads with known stable versions for Java 6-25</li>
 * </ul>
 *
 * @author Thomas
 * @date 20.02.2026
 */
public class JREProvider {
    private static final String JRE_CACHE_FOLDER = "tmp/jre_cache";

    /**
     * Enum representing the bitness (32-bit or 64-bit) of a JRE.
     */
    public static enum Bitness {
        /**
         * 32-bit JRE
         */
        BIT_32,
        /**
         * 64-bit JRE
         */
        BIT_64;

        /**
         * Returns the current system's bitness.
         */
        public static Bitness getSystemBitness() {
            return CrossSystem.is64BitOperatingSystem() ? BIT_64 : BIT_32;
        }
    }

    /**
     * Result of JRE lookup containing the java binary path, cache folder, and the actual bitness used.
     */
    public static class JreLookupResult {
        private final File    javaBinary;
        private final Bitness bitness;
        private final File    cacheFolder;

        public JreLookupResult(final File javaBinary, final Bitness bitness, final File cacheFolder) {
            this.javaBinary = javaBinary;
            this.bitness = bitness;
            this.cacheFolder = cacheFolder;
        }

        public File getJavaBinary() {
            return this.javaBinary;
        }

        public Bitness getBitness() {
            return this.bitness;
        }

        public File getCacheFolder() {
            return this.cacheFolder;
        }
    }

    /**
     * Central JRE lookup: cache → download → prompt. Use this when you have version/bitness/detailedVersion (e.g. from
     * {@link JREExecuter.JreOptions}).
     *
     * @param version
     *            Java version to resolve
     * @param osFamily
     *            Target OS
     * @param bitness
     *            Requested bitness (may be null for auto: try system bitness then fallback)
     * @param bitnessExplicit
     *            If true, do not fallback to other bitness
     * @param detailedVersion
     *            Optional exact version string (e.g. "1.6.0_45")
     * @return Resolved JRE (java binary, cache folder, bitness)
     */
    public static JreLookupResult findOrDownload(final JavaVersion version, final OSFamily osFamily, final Bitness bitness, final boolean bitnessExplicit, final String detailedVersion) throws Exception {
        if (version == null || version == JavaVersion.UNKNOWN) {
            throw new IllegalArgumentException("version must not be null or UNKNOWN");
        }
        if (bitnessExplicit && bitness != null) {
            return findOrDownloadExplicitBitness(version, osFamily, bitness, detailedVersion);
        } else {
            return findOrDownloadAutoBitness(version, osFamily, detailedVersion);
        }
    }

    /**
     * Returns the cache folder for JRE installations using current system OS and bitness.
     */
    public static File getJRECacheFolder(final JavaVersion version) {
        return getJRECacheFolder(version, CrossSystem.getOSFamily(), Bitness.getSystemBitness());
    }

    /**
     * Returns the cache folder for JRE installations with specified bitness and current system OS.
     */
    public static File getJRECacheFolder(final JavaVersion version, final Bitness bitness) {
        return getJRECacheFolder(version, CrossSystem.getOSFamily(), bitness);
    }

    /**
     * Returns the cache folder for JRE installations with specified OS and bitness.
     * <p>
     * Path format: tmp/jre_cache/{version}/{os}/{bitness}/
     */
    public static File getJRECacheFolder(final JavaVersion version, final OSFamily osFamily, final Bitness bitness) {
        final int featureVersion = getFeatureVersion(version);
        final String osString = getOSFolderName(osFamily);
        final String bitnessString = bitness == Bitness.BIT_32 ? "x86" : "x64";
        return Application.getResource(JRE_CACHE_FOLDER + "/" + featureVersion + "/" + osString + "/" + bitnessString);
    }
    // --------------- ensureJRE API (convenience; delegates to internal lookup + download) ---------------

    /**
     * Ensures a JRE of the specified version is available for the current system. Downloads from Adoptium/Azul if not present. Uses current
     * system OS and bitness. If 64-bit is not available (e.g. for Java 6/7), falls back to 32-bit.
     *
     * @return The path to the java binary (java.exe on Windows, java on Unix)
     */
    public static File ensureJRE(final JavaVersion version) throws Exception {
        return ensureJREWithBitnessFallback(version, CrossSystem.getOSFamily(), null, Bitness.getSystemBitness());
    }

    /**
     * Ensures a JRE of the specified version and bitness is available for the current system.
     */
    public static File ensureJRE(final JavaVersion version, final Bitness bitness) throws Exception {
        return ensureJRE(version, CrossSystem.getOSFamily(), bitness);
    }

    /**
     * Ensures a JRE of the specified version is available for the given OS. Uses system bitness with 32-bit fallback.
     */
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily) throws Exception {
        return ensureJREWithBitnessFallback(version, osFamily, null, Bitness.getSystemBitness());
    }

    /**
     * Ensures a JRE of the specified version and bitness is available for the given OS.
     */
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily, final Bitness bitness) throws Exception {
        final File targetFolder = getJRECacheFolder(version, osFamily, bitness);
        return ensureJRE(version, osFamily, bitness, targetFolder);
    }

    private static File ensureJREWithBitnessFallback(final JavaVersion version, final OSFamily osFamily, final ARCHFamily archFamily, final Bitness preferredBitness) throws Exception {
        final File targetFolder64 = getJRECacheFolder(version, osFamily, preferredBitness);
        try {
            return ensureJREInternal(version, osFamily, archFamily, preferredBitness, targetFolder64);
        } catch (final IOException e) {
            if (preferredBitness == Bitness.BIT_64) {
                LogV3.info("64-bit JRE not available for Java " + getFeatureVersion(version) + ", trying 32-bit fallback...");
                final File targetFolder32 = getJRECacheFolder(version, osFamily, Bitness.BIT_32);
                return ensureJREInternal(version, osFamily, archFamily, Bitness.BIT_32, targetFolder32);
            }
            throw e;
        }
    }

    /**
     * Ensures a JRE of the specified version and bitness is available in the target folder.
     */
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily, final Bitness bitness, final File targetFolder) throws Exception {
        return ensureJRE(version, osFamily, null, bitness, targetFolder);
    }

    /**
     * @deprecated Use {@link #ensureJRE(JavaVersion, OSFamily, Bitness, File)} instead
     */
    @Deprecated
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily, final ARCHFamily archFamily, final File targetFolder) throws Exception {
        return ensureJRE(version, osFamily, archFamily, Bitness.getSystemBitness(), targetFolder);
    }

    /**
     * Ensures a JRE of the specified version, architecture and bitness is available in the target folder.
     */
    public static File ensureJRE(final JavaVersion version, final OSFamily osFamily, final ARCHFamily archFamily, final Bitness bitness, final File targetFolder) throws Exception {
        return ensureJREInternal(version, osFamily, archFamily, bitness, targetFolder);
    }

    private static File ensureJREInternal(final JavaVersion version, final OSFamily osFamily, final ARCHFamily archFamily, final Bitness bitness, final File targetFolder) throws Exception {
        if (version == null || version == JavaVersion.UNKNOWN) {
            throw new IllegalArgumentException("version must not be null or UNKNOWN");
        }
        if (osFamily == null) {
            throw new IllegalArgumentException("osFamily must not be null");
        }
        if (bitness == null) {
            throw new IllegalArgumentException("bitness must not be null");
        }
        if (targetFolder == null) {
            throw new IllegalArgumentException("targetFolder must not be null");
        }
        final int featureVersion = getFeatureVersion(version);
        final String osString = getAdoptiumOS(osFamily);
        final String archString = getAdoptiumArch(archFamily != null ? archFamily : CrossSystem.getARCHFamily(), bitness);
        final String javaBinaryName = osFamily == OSFamily.WINDOWS ? "java.exe" : "java";
        final File javaBinary = findJavaBinaryInFolder(targetFolder, javaBinaryName);
        if (javaBinary != null && javaBinary.exists()) {
            LogV3.info("JRE already exists at: " + javaBinary.getAbsolutePath());
            return javaBinary;
        }
        targetFolder.mkdirs();
        final File tempArchive = downloadJREFromAvailableSource(featureVersion, osString, archString, targetFolder, osFamily);
        if (!tempArchive.equals(targetFolder)) {
            try {
                LogV3.info("Extracting JRE to: " + targetFolder.getAbsolutePath());
                extractArchive(tempArchive, targetFolder, osFamily);
            } finally {
                tempArchive.delete();
            }
        }
        final File extractedJavaBinary = findJavaBinaryInFolder(targetFolder, javaBinaryName);
        if (extractedJavaBinary == null || !extractedJavaBinary.exists()) {
            throw new IOException("Failed to find java binary after extraction in: " + targetFolder);
        }
        if (osFamily != OSFamily.WINDOWS) {
            extractedJavaBinary.setExecutable(true);
        }
        LogV3.info("JRE installed at: " + extractedJavaBinary.getAbsolutePath());
        return extractedJavaBinary;
    }
    // --------------- findOrDownload helpers ---------------

    private static JreLookupResult findOrDownloadExplicitBitness(final JavaVersion version, final OSFamily osFamily, final Bitness bitness, final String detailedVersion) throws Exception {
        final File cacheFolder = getJRECacheFolder(version, osFamily, bitness);
        final String javaBinaryName = osFamily == OSFamily.WINDOWS ? "java.exe" : "java";
        File javaBinary = findJavaBinaryInCache(cacheFolder, javaBinaryName, detailedVersion);
        if (javaBinary != null) {
            LogV3.info("JRE found in cache: " + javaBinary.getAbsolutePath());
            return new JreLookupResult(javaBinary, bitness, cacheFolder);
        }
        final int featureVersion = getFeatureVersion(version);
        final String os = getAdoptiumOS(osFamily);
        final ARCHFamily archFamily = CrossSystem.getARCHFamily();
        final String arch = getAdoptiumArch(archFamily, bitness);
        try {
            javaBinary = downloadAndExtractJRE(featureVersion, os, arch, cacheFolder, osFamily, javaBinaryName);
            if (javaBinary != null) {
                if (detailedVersion != null) {
                    final String actualVersion = detectJavaVersionString(javaBinary);
                    if (actualVersion != null && !actualVersion.contains(detailedVersion) && !detailedVersion.contains(actualVersion.split("_")[0])) {
                        LogV3.info("Downloaded JRE version " + actualVersion + " does not match requested " + detailedVersion);
                    }
                }
                return new JreLookupResult(javaBinary, bitness, cacheFolder);
            }
        } catch (final Exception e) {
            LogV3.info("Download failed for " + bitness + ": " + e.getMessage());
        }
        javaBinary = promptAndCopyLocalJRE(featureVersion, cacheFolder, osFamily, detailedVersion);
        return new JreLookupResult(javaBinary, bitness, cacheFolder);
    }

    private static JreLookupResult findOrDownloadAutoBitness(final JavaVersion version, final OSFamily osFamily, final String detailedVersion) throws Exception {
        final Bitness systemBitness = Bitness.getSystemBitness();
        final Bitness alternateBitness = systemBitness == Bitness.BIT_64 ? Bitness.BIT_32 : Bitness.BIT_64;
        final String javaBinaryName = osFamily == OSFamily.WINDOWS ? "java.exe" : "java";
        final int featureVersion = getFeatureVersion(version);
        final String os = getAdoptiumOS(osFamily);
        final ARCHFamily archFamily = CrossSystem.getARCHFamily();
        final File cacheFolder64 = getJRECacheFolder(version, osFamily, Bitness.BIT_64);
        File javaBinary = findJavaBinaryInCache(cacheFolder64, javaBinaryName, detailedVersion);
        if (javaBinary != null) {
            LogV3.info("JRE found in cache (64-bit): " + javaBinary.getAbsolutePath());
            return new JreLookupResult(javaBinary, Bitness.BIT_64, cacheFolder64);
        }
        final File cacheFolder32 = getJRECacheFolder(version, osFamily, Bitness.BIT_32);
        javaBinary = findJavaBinaryInCache(cacheFolder32, javaBinaryName, detailedVersion);
        if (javaBinary != null) {
            LogV3.info("JRE found in cache (32-bit): " + javaBinary.getAbsolutePath());
            return new JreLookupResult(javaBinary, Bitness.BIT_32, cacheFolder32);
        }
        final File systemCacheFolder = systemBitness == Bitness.BIT_64 ? cacheFolder64 : cacheFolder32;
        final String systemArch = getAdoptiumArch(archFamily, systemBitness);
        try {
            javaBinary = downloadAndExtractJRE(featureVersion, os, systemArch, systemCacheFolder, osFamily, javaBinaryName);
            if (javaBinary != null) {
                LogV3.info("Downloaded JRE (" + systemBitness + "): " + javaBinary.getAbsolutePath());
                return new JreLookupResult(javaBinary, systemBitness, systemCacheFolder);
            }
        } catch (final Exception e) {
            LogV3.info("Download failed for " + systemBitness + ": " + e.getMessage() + ", trying " + alternateBitness + "...");
        }
        final File alternateCacheFolder = alternateBitness == Bitness.BIT_64 ? cacheFolder64 : cacheFolder32;
        final String alternateArch = getAdoptiumArch(archFamily, alternateBitness);
        try {
            javaBinary = downloadAndExtractJRE(featureVersion, os, alternateArch, alternateCacheFolder, osFamily, javaBinaryName);
            if (javaBinary != null) {
                LogV3.info("Downloaded JRE (" + alternateBitness + "): " + javaBinary.getAbsolutePath());
                return new JreLookupResult(javaBinary, alternateBitness, alternateCacheFolder);
            }
        } catch (final Exception e) {
            LogV3.info("Download failed for " + alternateBitness + ": " + e.getMessage());
        }
        LogV3.info("No JRE download available, prompting for local path...");
        javaBinary = promptAndCopyLocalJRE(featureVersion, systemCacheFolder, osFamily, detailedVersion);
        final Bitness promptedBitness = detectBitnessFromJava(javaBinary);
        return new JreLookupResult(javaBinary, promptedBitness != null ? promptedBitness : systemBitness, systemCacheFolder);
    }

    private static File findJavaBinaryInCache(final File cacheFolder, final String javaBinaryName, final String detailedVersion) {
        if (!cacheFolder.exists() || !cacheFolder.isDirectory()) {
            return null;
        }
        final File javaBinary = findJavaBinaryInFolder(cacheFolder, javaBinaryName);
        if (javaBinary == null || !javaBinary.exists()) {
            return null;
        }
        if (detailedVersion != null && !detailedVersion.isEmpty()) {
            try {
                final String actualVersion = detectJavaVersionString(javaBinary);
                if (actualVersion != null && !actualVersion.contains(detailedVersion) && !detailedVersion.startsWith(actualVersion.split("_")[0].split("-")[0])) {
                    LogV3.info("Cached JRE version " + actualVersion + " does not match requested " + detailedVersion + ", skipping cache");
                    return null;
                }
            } catch (final Exception e) {
                LogV3.warning("Failed to detect cached JRE version: " + e.getMessage());
            }
        }
        return javaBinary;
    }

    private static Bitness detectBitnessFromJava(final File javaBinary) {
        if (javaBinary == null) {
            return null;
        }
        try {
            final ProcessBuilder pb = new ProcessBuilder(javaBinary.getAbsolutePath(), "-XshowSettings:all", "-version");
            pb.redirectErrorStream(true);
            final Process process = pb.start();
            final BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
            String line;
            while ((line = reader.readLine()) != null) {
                final String lower = line.toLowerCase(Locale.ROOT);
                if (lower.contains("sun.arch.data.model") || lower.contains("os.arch")) {
                    if (lower.contains("64")) {
                        reader.close();
                        process.waitFor();
                        return Bitness.BIT_64;
                    } else if (lower.contains("32") || lower.contains("x86") || lower.contains("i386")) {
                        reader.close();
                        process.waitFor();
                        return Bitness.BIT_32;
                    }
                }
            }
            reader.close();
            process.waitFor();
        } catch (final Exception e) {
            LogV3.warning("Failed to detect JRE bitness: " + e.getMessage());
        }
        return null;
    }

    private static String getOSFolderName(final OSFamily osFamily) {
        return osFamily.name().toLowerCase(Locale.ROOT);
    }

    /** Visible for {@link JREExecuter} (execution / compiler version strings). */
    static int getFeatureVersion(final JavaVersion version) {
        final String versionString = version.getBase().getVersionString();
        if (versionString.startsWith("1.")) {
            return Integer.parseInt(versionString.substring(2));
        }
        final int dotIndex = versionString.indexOf('.');
        if (dotIndex > 0) {
            return Integer.parseInt(versionString.substring(0, dotIndex));
        }
        return Integer.parseInt(versionString);
    }

    private static String getAdoptiumOS(final OSFamily osFamily) {
        switch (osFamily) {
        case WINDOWS:
            return "windows";
        case LINUX:
            return "linux";
        case MAC:
            return "mac";
        default:
            throw new IllegalArgumentException("Unsupported OS family for Adoptium: " + osFamily);
        }
    }

    private static String getAdoptiumArch(final ARCHFamily archFamily, final Bitness bitness) {
        switch (archFamily) {
        case X86:
            if (bitness == Bitness.BIT_64) {
                return "x64";
            } else {
                return "x86";
            }
        case ARM:
            if (bitness == Bitness.BIT_64) {
                return "aarch64";
            } else {
                return "arm";
            }
        case PPC:
            if (bitness == Bitness.BIT_64) {
                return "ppc64le";
            } else {
                return "ppc64le";
            }
        default:
            return bitness == Bitness.BIT_64 ? "x64" : "x86";
        }
    }

    private static File downloadAndExtractJRE(final int featureVersion, final String os, final String arch, final File targetFolder, final OSFamily osFamily, final String javaBinaryName) throws Exception {
        final File tempArchive = downloadJREFromAvailableSource(featureVersion, os, arch, targetFolder, osFamily);
        if (tempArchive == null) {
            return null;
        }
        try {
            LogV3.info("Extracting JRE to: " + targetFolder.getAbsolutePath());
            extractArchive(tempArchive, targetFolder, osFamily);
        } finally {
            tempArchive.delete();
        }
        final File extractedJavaBinary = findJavaBinaryInFolder(targetFolder, javaBinaryName);
        if (extractedJavaBinary == null || !extractedJavaBinary.exists()) {
            throw new IOException("Failed to find java binary after extraction in: " + targetFolder);
        }
        if (osFamily != OSFamily.WINDOWS) {
            extractedJavaBinary.setExecutable(true);
        }
        LogV3.info("JRE installed at: " + extractedJavaBinary.getAbsolutePath());
        return extractedJavaBinary;
    }

    private static File downloadJREFromAvailableSource(final int featureVersion, final String os, final String arch, final File targetFolder, final OSFamily osFamily) throws Exception {
        final List<String> urls = new ArrayList<String>();
        urls.add(buildAdoptiumJREUrl(featureVersion, os, arch));
        urls.add(buildAdoptiumJDKUrl(featureVersion, os, arch));
        urls.add(buildAzulZuluUrl(featureVersion, os, arch, osFamily, "jre"));
        urls.add(buildAzulZuluUrl(featureVersion, os, arch, osFamily, "jdk"));
        urls.addAll(buildAzulZuluCDNUrls(featureVersion, os, arch, osFamily, "jre"));
        urls.addAll(buildAzulZuluCDNUrls(featureVersion, os, arch, osFamily, "jdk"));
        if (featureVersion == 27) {
            if ("windows".equals(os) && "x64".equals(arch)) {
                urls.add("https://download.java.net/java/early_access/jdk27/13/GPL/openjdk-27-ea+13_windows-x64_bin.zip");
            } else if ("linux".equals(os) && "x64".equals(arch)) {
                urls.add("https://download.java.net/java/early_access/jdk27/13/GPL/openjdk-27-ea+13_linux-x64_bin.tar.gz");
            } else if ("mac".equals(os) && "x64".equals(arch)) {
                urls.add("https://download.java.net/java/early_access/jdk27/13/GPL/openjdk-27-ea+13_macos-x64_bin.tar.gz");
            } else if ("mac".equals(os) && "aarch64".equals(arch)) {
                urls.add("https://download.java.net/java/early_access/jdk27/13/GPL/openjdk-27-ea+13_macos-aarch64_bin.tar.gz");
            }
        }
        for (final String url : urls) {
            if (url == null) {
                continue;
            }
            LogV3.info("Trying to download JRE from: " + url);
            try {
                final File result = tryDownloadJRE(url, targetFolder, osFamily);
                if (result != null) {
                    LogV3.info("Download successful from: " + url);
                    return result;
                }
            } catch (final Exception e) {
                LogV3.info("Download failed from " + url + ": " + e.getMessage());
            }
        }
        return null;
    }

    private static File promptAndCopyLocalJRE(final int featureVersion, final File targetFolder, final OSFamily osFamily) throws Exception {
        return promptAndCopyLocalJRE(featureVersion, targetFolder, osFamily, null);
    }

    private static File promptAndCopyLocalJRE(final int featureVersion, final File targetFolder, final OSFamily osFamily, final String detailedVersion) throws Exception {
        final String javaBinaryName = osFamily == OSFamily.WINDOWS ? "java.exe" : "java";
        final String versionDisplay = detailedVersion != null ? detailedVersion : String.valueOf(featureVersion);
        System.out.println();
        System.out.println("=======================================================================");
        System.out.println("  No JRE download available for Java " + versionDisplay);
        System.out.println("  Please provide a local JRE/JDK path for Java " + versionDisplay);
        System.out.println("  (Enter path to the JRE root folder, e.g. C:\\Program Files\\Java\\jre1.6.0_45)");
        System.out.println("  Or press Enter to abort.");
        System.out.println("=======================================================================");
        System.out.print("JRE path: ");
        final BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        final String inputPath = reader.readLine();
        if (inputPath == null || inputPath.trim().isEmpty()) {
            throw new IOException("No JRE download source available for Java " + versionDisplay + " and no local path provided");
        }
        final File sourceJreFolder = new File(inputPath.trim());
        if (!sourceJreFolder.exists() || !sourceJreFolder.isDirectory()) {
            throw new IOException("Provided path does not exist or is not a directory: " + sourceJreFolder.getAbsolutePath());
        }
        final File sourceJavaBinary = findJavaBinaryInFolder(sourceJreFolder, javaBinaryName);
        if (sourceJavaBinary == null || !sourceJavaBinary.exists()) {
            final File directBin = new File(sourceJreFolder, "bin/" + javaBinaryName);
            throw new IOException("No java binary found in provided path. Expected at: " + directBin.getAbsolutePath());
        }
        final int detectedFeatureVersion = detectJavaVersion(sourceJavaBinary);
        if (detectedFeatureVersion != featureVersion) {
            throw new IOException("Java version mismatch. Expected Java " + featureVersion + " but found Java " + detectedFeatureVersion + " at: " + sourceJavaBinary.getAbsolutePath());
        }
        if (detailedVersion != null) {
            final String detectedFullVersion = detectJavaVersionString(sourceJavaBinary);
            if (detectedFullVersion != null && !detectedFullVersion.contains(detailedVersion)) {
                LogV3.warning("Detailed version mismatch. Expected " + detailedVersion + " but found " + detectedFullVersion + ". Continuing anyway.");
            }
        }
        LogV3.info("Valid JRE found at: " + sourceJavaBinary.getAbsolutePath());
        LogV3.info("Copying JRE to cache: " + targetFolder.getAbsolutePath());
        final File sourceRootFolder = sourceJavaBinary.getParentFile().getParentFile();
        copyDirectory(sourceRootFolder, new File(targetFolder, sourceRootFolder.getName()));
        LogV3.info("JRE copied successfully");
        return findJavaBinaryInFolder(targetFolder, javaBinaryName);
    }

    private static int detectJavaVersion(final File javaBinary) throws Exception {
        final ProcessBuilder pb = new ProcessBuilder(javaBinary.getAbsolutePath(), "-version");
        pb.redirectErrorStream(true);
        final Process process = pb.start();
        final BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
        String line;
        int version = -1;
        while ((line = reader.readLine()) != null) {
            version = parseVersionFromOutput(line);
            if (version > 0) {
                break;
            }
        }
        process.waitFor();
        reader.close();
        if (version <= 0) {
            throw new IOException("Could not determine Java version from: " + javaBinary.getAbsolutePath());
        }
        return version;
    }

    private static String detectJavaVersionString(final File javaBinary) throws Exception {
        final ProcessBuilder pb = new ProcessBuilder(javaBinary.getAbsolutePath(), "-version");
        pb.redirectErrorStream(true);
        final Process process = pb.start();
        final BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
        String line;
        String versionString = null;
        while ((line = reader.readLine()) != null) {
            final int quoteStart = line.indexOf('"');
            if (quoteStart >= 0) {
                final int quoteEnd = line.indexOf('"', quoteStart + 1);
                if (quoteEnd > quoteStart) {
                    versionString = line.substring(quoteStart + 1, quoteEnd);
                    break;
                }
            }
        }
        process.waitFor();
        reader.close();
        return versionString;
    }

    private static int parseVersionFromOutput(final String line) {
        if (line == null) {
            return -1;
        }
        final String lower = line.toLowerCase(Locale.ROOT);
        if (!lower.contains("version")) {
            return -1;
        }
        final int quoteStart = line.indexOf('"');
        if (quoteStart < 0) {
            return -1;
        }
        final int quoteEnd = line.indexOf('"', quoteStart + 1);
        if (quoteEnd < 0) {
            return -1;
        }
        final String versionStr = line.substring(quoteStart + 1, quoteEnd);
        if (versionStr.startsWith("1.")) {
            final int dotIndex = versionStr.indexOf('.', 2);
            if (dotIndex > 2) {
                try {
                    return Integer.parseInt(versionStr.substring(2, dotIndex));
                } catch (final NumberFormatException e) {
                    return -1;
                }
            }
        } else {
            final int dotIndex = versionStr.indexOf('.');
            if (dotIndex > 0) {
                try {
                    return Integer.parseInt(versionStr.substring(0, dotIndex));
                } catch (final NumberFormatException e) {
                    return -1;
                }
            }
            try {
                return Integer.parseInt(versionStr.split("[^0-9]")[0]);
            } catch (final NumberFormatException e) {
                return -1;
            }
        }
        return -1;
    }

    private static void copyDirectory(final File source, final File target) throws IOException {
        if (source.isDirectory()) {
            target.mkdirs();
            final String[] children = source.list();
            if (children != null) {
                for (final String child : children) {
                    copyDirectory(new File(source, child), new File(target, child));
                }
            }
        } else {
            target.getParentFile().mkdirs();
            IO.copyFile(source, target);
        }
    }

    private static String buildAdoptiumJREUrl(final int featureVersion, final String os, final String arch) {
        return "https://api.adoptium.net/v3/binary/latest/" + featureVersion + "/ga/" + os + "/" + arch + "/jre/hotspot/normal/eclipse";
    }

    private static String buildAdoptiumJDKUrl(final int featureVersion, final String os, final String arch) {
        return "https://api.adoptium.net/v3/binary/latest/" + featureVersion + "/ga/" + os + "/" + arch + "/jdk/hotspot/normal/eclipse";
    }

    private static String buildAzulZuluUrl(final int featureVersion, final String os, final String arch, final OSFamily osFamily, final String packageType) {
        final String zuluOs = getAzulOS(os);
        final String zuluArch = getAzulArch(arch);
        final String archiveType = osFamily == OSFamily.WINDOWS ? "zip" : "tar.gz";
        return "https://api.azul.com/metadata/v1/zulu/packages/?java_version=" + featureVersion + "&os=" + zuluOs + "&arch=" + zuluArch + "&java_package_type=" + packageType + "&archive_type=" + archiveType + "&latest=true&availability_types=CA";
    }

    private static String getAzulOS(final String adoptiumOs) {
        if ("windows".equals(adoptiumOs)) {
            return "windows";
        } else if ("linux".equals(adoptiumOs)) {
            return "linux";
        } else if ("mac".equals(adoptiumOs)) {
            return "macos";
        }
        return adoptiumOs;
    }

    private static String getAzulArch(final String adoptiumArch) {
        if ("x64".equals(adoptiumArch)) {
            return "x64";
        } else if ("x86".equals(adoptiumArch) || "x32".equals(adoptiumArch)) {
            return "i686";
        } else if ("aarch64".equals(adoptiumArch)) {
            return "aarch64";
        }
        return adoptiumArch;
    }

    private static String getAzulCDNArch(final String adoptiumArch) {
        if ("x64".equals(adoptiumArch)) {
            return "x64";
        } else if ("x86".equals(adoptiumArch) || "x32".equals(adoptiumArch)) {
            return "i686";
        } else if ("aarch64".equals(adoptiumArch)) {
            return "aarch64";
        }
        return adoptiumArch;
    }

    private static String getAzulCDNOS(final String adoptiumOs) {
        if ("windows".equals(adoptiumOs)) {
            return "win";
        } else if ("linux".equals(adoptiumOs)) {
            return "linux";
        } else if ("mac".equals(adoptiumOs)) {
            return "macosx";
        }
        return adoptiumOs;
    }

    private static final String[][] ZULU_KNOWN_VERSIONS = {
        // Java 6
        { "6", "6.22.0.3", "6.0.119" },
        // Java 7
        { "7", "7.56.0.11", "7.0.352" },
        // Java 8
        { "8", "8.82.0.21", "8.0.432" },
        // Java 9
        { "9", "9.0.7.1", "9.0.7" },
        // Java 10
        { "10", "10.3.5", "10.0.2" },
        // Java 11
        { "11", "11.74.15", "11.0.24" },
        // Java 12
        { "12", "12.3.11", "12.0.2" },
        // Java 13
        { "13", "13.54.17", "13.0.14" },
        // Java 14
        { "14", "14.29.23", "14.0.2" },
        // Java 15
        { "15", "15.46.17", "15.0.10" },
        // Java 16
        { "16", "16.32.15", "16.0.2" },
        // Java 17
        { "17", "17.54.17", "17.0.13" },
        // Java 18
        { "18", "18.32.13", "18.0.2.1" },
        // Java 19
        { "19", "19.32.13", "19.0.2" },
        // Java 20
        { "20", "20.32.11", "20.0.2" },
        // Java 21
        { "21", "21.38.21", "21.0.5" },
        // Java 22
        { "22", "22.32.15", "22.0.2" },
        // Java 23
        { "23", "23.32.11", "23.0.1" },
        // Java 24
        { "24", "24.28.79", "24" },
        // Java 25
        { "25", "25.32.21", "25.0.2" } };

    private static String[] findKnownZuluVersion(final int featureVersion) {
        for (final String[] entry : ZULU_KNOWN_VERSIONS) {
            if (Integer.parseInt(entry[0]) == featureVersion) {
                return new String[] { entry[1], entry[2] };
            }
        }
        return null;
    }

    private static List<String> buildAzulZuluCDNUrls(final int featureVersion, final String os, final String arch, final OSFamily osFamily, final String packageType) {
        final List<String> urls = new ArrayList<String>();
        final String[] versionInfo = findKnownZuluVersion(featureVersion);
        if (versionInfo == null) {
            return urls;
        }
        final String zuluVersion = versionInfo[0];
        final String javaVersion = versionInfo[1];
        final String cdnOs = getAzulCDNOS(os);
        final String cdnArch = getAzulCDNArch(arch);
        final String extension = osFamily == OSFamily.WINDOWS ? "zip" : "tar.gz";
        final String baseUrl = "https://cdn.azul.com/zulu/bin/zulu" + zuluVersion + "-ca-" + packageType + javaVersion + "-" + cdnOs + "_" + cdnArch + "." + extension;
        urls.add(baseUrl);
        if ("i686".equals(cdnArch)) {
            final String altUrl = "https://cdn.azul.com/zulu/bin/zulu" + zuluVersion + "-ca-" + packageType + javaVersion + "-" + cdnOs + "_i386." + extension;
            urls.add(altUrl);
        }
        return urls;
    }

    private static DownloadProgress createDownloadProgress(final String url) {
        return new DownloadProgress() {
            private long   lastPrintTime    = 0;
            private int    lastPrintPercent = -1;
            private long   totalSize        = -1;
            private String shortUrl         = url.length() > 60 ? "..." + url.substring(url.length() - 57) : url;

            @Override
            public void setTotal(final long total) {
                super.setTotal(total);
                if (total > 0) {
                    this.totalSize = total;
                    LogV3.info("Downloading: " + shortUrl + " (" + SizeFormatter.formatBytes(total) + ")");
                }
            }

            @Override
            public void onBytesLoaded(final byte[] b, final int len) {
                super.onBytesLoaded(b, len);
                final long now = System.currentTimeMillis();
                if (this.totalSize > 0) {
                    final int percent = (int) ((getLoaded() * 100) / this.totalSize);
                    if (percent != this.lastPrintPercent && (now - this.lastPrintTime > 1000 || percent >= 100)) {
                        this.lastPrintTime = now;
                        this.lastPrintPercent = percent;
                        LogV3.info("  Progress: " + Math.min(percent, 100) + "% (" + SizeFormatter.formatBytes(getLoaded()) + " / " + SizeFormatter.formatBytes(this.totalSize) + ")");
                    }
                } else if (now - this.lastPrintTime > 2000) {
                    this.lastPrintTime = now;
                    LogV3.info("  Downloaded: " + SizeFormatter.formatBytes(getLoaded()));
                }
            }
        };
    }

    private static File tryDownloadJRE(final String url, final File targetFolder, final OSFamily osFamily) throws Exception {
        if (url.contains("api.azul.com")) {
            return downloadFromAzulAPI(url, targetFolder, osFamily);
        }
        final String extension = osFamily == OSFamily.WINDOWS ? ".zip" : ".tar.gz";
        final File tempFile = new File(targetFolder, "jre_download" + extension);
        final org.appwork.utils.net.httpclient.HttpClient client = new org.appwork.utils.net.httpclient.HttpClient();
        client.setConnectTimeout(30000);
        client.setReadTimeout(120000);
        tempFile.getParentFile().mkdirs();
        final DownloadProgress progress = createDownloadProgress(url);
        final RequestContext context = RequestContext.get(url).setTarget(tempFile).setDownloadProgress(progress);
        try {
            context.execute();
        } finally {
            System.out.println(context);
        }
        final int responseCode = context.getCode();
        if (responseCode == 404 || responseCode == 403) {
            tempFile.delete();
            return null;
        }
        if (responseCode < 200 || responseCode >= 300) {
            tempFile.delete();
            throw new IOException("Failed to download JRE. HTTP response code: " + responseCode);
        }
        if (tempFile.length() < 1000) {
            final String content = IO.readFileToString(tempFile);
            if (content != null && (content.contains("<!DOCTYPE") || content.contains("<html") || content.contains("Not Found"))) {
                tempFile.delete();
                return null;
            }
        }
        LogV3.info("  Download complete: " + SizeFormatter.formatBytes(tempFile.length()));
        return tempFile;
    }

    private static File downloadFromAzulAPI(final String apiUrl, final File targetFolder, final OSFamily osFamily) throws Exception {
        final org.appwork.utils.net.httpclient.HttpClient client = new org.appwork.utils.net.httpclient.HttpClient();
        client.setConnectTimeout(30000);
        client.setReadTimeout(30000);
        final RequestContext apiContext = RequestContext.get(apiUrl);
        apiContext.execute();
        final int apiResponseCode = apiContext.getCode();
        if (apiResponseCode == 404 || apiResponseCode == 204) {
            return null;
        }
        if (apiResponseCode < 200 || apiResponseCode >= 300) {
            throw new IOException("Azul API request failed. HTTP response code: " + apiResponseCode);
        }
        final String jsonResponse = apiContext.getResponseString();
        if (jsonResponse == null || jsonResponse.trim().isEmpty() || "[]".equals(jsonResponse.trim())) {
            return null;
        }
        final String downloadUrl = extractAzulDownloadUrl(jsonResponse);
        if (downloadUrl == null) {
            return null;
        }
        LogV3.info("Azul download URL: " + downloadUrl);
        final String extension = osFamily == OSFamily.WINDOWS ? ".zip" : ".tar.gz";
        final File tempFile = new File(targetFolder, "jre_download" + extension);
        tempFile.getParentFile().mkdirs();
        final DownloadProgress progress = createDownloadProgress(downloadUrl);
        final RequestContext downloadContext = RequestContext.get(downloadUrl).setTarget(tempFile).setDownloadProgress(progress);
        downloadContext.execute();
        final int downloadResponseCode = downloadContext.getCode();
        if (downloadResponseCode < 200 || downloadResponseCode >= 300) {
            tempFile.delete();
            throw new IOException("Failed to download from Azul. HTTP response code: " + downloadResponseCode);
        }
        LogV3.info("  Download complete: " + SizeFormatter.formatBytes(tempFile.length()));
        return tempFile;
    }

    private static String extractAzulDownloadUrl(final String json) {
        final int downloadUrlIndex = json.indexOf("\"download_url\"");
        if (downloadUrlIndex < 0) {
            return null;
        }
        final int colonIndex = json.indexOf(":", downloadUrlIndex);
        if (colonIndex < 0) {
            return null;
        }
        final int startQuote = json.indexOf("\"", colonIndex);
        if (startQuote < 0) {
            return null;
        }
        final int endQuote = json.indexOf("\"", startQuote + 1);
        if (endQuote < 0) {
            return null;
        }
        return json.substring(startQuote + 1, endQuote);
    }

    private static void extractArchive(final File archive, final File targetFolder, final OSFamily osFamily) throws Exception {
        if (osFamily == OSFamily.WINDOWS) {
            extractZip(archive, targetFolder);
        } else {
            extractTarGz(archive, targetFolder);
        }
    }

    private static void extractZip(final File zipFile, final File targetFolder) throws Exception {
        final ZipIOReader reader = new ZipIOReader(zipFile);
        try {
            reader.extractTo(targetFolder);
        } finally {
            reader.close();
        }
    }

    private static void extractTarGz(final File tarGzFile, final File targetFolder) throws Exception {
        try {
            extractTarGzNative(tarGzFile, targetFolder);
        } catch (final ClassNotFoundException e) {
            extractTarGzCommandLine(tarGzFile, targetFolder);
        }
    }

    private static void extractTarGzNative(final File tarGzFile, final File targetFolder) throws Exception {
        final Class<?> tarInputStreamClass = Class.forName("org.apache.commons.compress.archivers.tar.TarArchiveInputStream");
        final Class<?> tarEntryClass = Class.forName("org.apache.commons.compress.archivers.tar.TarArchiveEntry");
        final InputStream fis = new FileInputStream(tarGzFile);
        try {
            final InputStream gzis = new GZIPInputStream(fis);
            try {
                final Object tarIn = tarInputStreamClass.getConstructor(InputStream.class).newInstance(gzis);
                try {
                    final java.lang.reflect.Method getNextEntry = tarInputStreamClass.getMethod("getNextTarEntry");
                    final java.lang.reflect.Method isDirectory = tarEntryClass.getMethod("isDirectory");
                    final java.lang.reflect.Method getName = tarEntryClass.getMethod("getName");
                    final java.lang.reflect.Method read = tarInputStreamClass.getMethod("read", byte[].class, int.class, int.class);
                    Object entry;
                    while ((entry = getNextEntry.invoke(tarIn)) != null) {
                        final String entryName = (String) getName.invoke(entry);
                        final File outputFile = new File(targetFolder, entryName);
                        if ((Boolean) isDirectory.invoke(entry)) {
                            outputFile.mkdirs();
                        } else {
                            outputFile.getParentFile().mkdirs();
                            final FileOutputStream fos = new FileOutputStream(outputFile);
                            try {
                                final byte[] buffer = new byte[32768];
                                int len;
                                while ((len = (Integer) read.invoke(tarIn, buffer, 0, buffer.length)) != -1) {
                                    fos.write(buffer, 0, len);
                                }
                            } finally {
                                fos.close();
                            }
                        }
                    }
                } finally {
                    if (tarIn instanceof java.io.Closeable) {
                        ((java.io.Closeable) tarIn).close();
                    }
                }
            } finally {
                gzis.close();
            }
        } finally {
            fis.close();
        }
    }

    private static void extractTarGzCommandLine(final File tarGzFile, final File targetFolder) throws Exception {
        final ProcessBuilder pb = new ProcessBuilder("tar", "-xzf", tarGzFile.getAbsolutePath(), "-C", targetFolder.getAbsolutePath());
        pb.redirectErrorStream(true);
        final Process process = pb.start();
        final int exitCode = process.waitFor();
        if (exitCode != 0) {
            throw new IOException("tar extraction failed with exit code: " + exitCode);
        }
    }

    private static File findJavaBinaryInFolder(final File folder, final String javaBinaryName) {
        if (!folder.exists() || !folder.isDirectory()) {
            return null;
        }
        final File directBin = new File(folder, "bin/" + javaBinaryName);
        if (directBin.exists()) {
            return directBin;
        }
        final File[] subFolders = folder.listFiles();
        if (subFolders != null) {
            for (final File subFolder : subFolders) {
                if (subFolder.isDirectory()) {
                    final String name = subFolder.getName().toLowerCase(Locale.ROOT);
                    if (name.startsWith("jdk") || name.startsWith("jre") || name.startsWith("zulu") || name.contains("temurin") || name.contains("adoptium")) {
                        final File binDir = new File(subFolder, "bin/" + javaBinaryName);
                        if (binDir.exists()) {
                            return binDir;
                        }
                        if (CrossSystem.getOSFamily() == OSFamily.MAC) {
                            final File macBin = new File(subFolder, "Contents/Home/bin/" + javaBinaryName);
                            if (macBin.exists()) {
                                return macBin;
                            }
                        }
                    }
                }
            }
        }
        return null;
    }
}
