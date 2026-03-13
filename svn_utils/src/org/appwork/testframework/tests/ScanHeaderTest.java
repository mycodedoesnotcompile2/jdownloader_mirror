/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.testframework.tests;

import java.io.File;
import java.io.IOException;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Date;
import java.util.concurrent.TimeUnit;

import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.AWTest;
import org.appwork.utils.DebugMode;
import org.appwork.utils.Files;
import org.appwork.utils.Files.Handler;
import org.appwork.utils.Hash;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;
import org.appwork.utils.StringUtils;
import org.appwork.utils.ide.IDEUtils;

/**
 * @author thomas
 * @date 18.10.2024
 *
 */
public class ScanHeaderTest extends AWTest {
    private static final int WC_DB_MAX_READ = 5 * 1024 * 1024;
    private static final String SHA1_PREFIX = "$sha1$";
    private static final int SHA1_HEX_LEN = 40;

    /**
     * SHA256 hash of content with all whitespace removed. Used to detect relevant (non-whitespace) changes.
     */
    private static String nonWhitespaceHash(String content) {
        if (content == null) {
            return null;
        }
        String normalized = content.replaceAll("\\s+", "");
        return Hash.getSHA256(normalized);
    }

    /**
     * Find the .svn directory for the given file (walk up until .svn exists).
     */
    private static File findSvnDir(File f) {
        File dir = f.isDirectory() ? f : f.getParentFile();
        while (dir != null) {
            File svn = new File(dir, ".svn");
            if (svn.isDirectory()) {
                return svn;
            }
            dir = dir.getParentFile();
        }
        return null;
    }

    /**
     * Extract SHA1 checksum for a path from SVN 1.7 wc.db by searching for the path and then "$sha1$" + 40 hex in the raw DB.
     * Returns the 40-char hex string or null if not found.
     */
    private static String readChecksumFromWcDb(File wcDb, String localRelpath) throws IOException {
        byte[] pathBytes = localRelpath.getBytes("UTF-8");
        byte[] prefixBytes = SHA1_PREFIX.getBytes("UTF-8");
        byte[] data = IO.readFile(wcDb, WC_DB_MAX_READ);
        if (data == null || data.length == 0) {
            return null;
        }
        int idx = 0;
        while (true) {
            idx = indexOf(data, pathBytes, idx);
            if (idx < 0) {
                break;
            }
            int prefixIdx = indexOf(data, prefixBytes, idx);
            if (prefixIdx >= 0 && prefixIdx < idx + 500 && prefixIdx + prefixBytes.length + SHA1_HEX_LEN <= data.length) {
                StringBuilder hex = new StringBuilder(SHA1_HEX_LEN);
                boolean valid = true;
                for (int i = 0; i < SHA1_HEX_LEN; i++) {
                    char c = (char) data[prefixIdx + prefixBytes.length + i];
                    if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) {
                        hex.append(c);
                    } else {
                        valid = false;
                        break;
                    }
                }
                if (valid && hex.length() == SHA1_HEX_LEN) {
                    return hex.toString();
                }
            }
            idx += pathBytes.length;
        }
        return null;
    }

    private static int indexOf(byte[] data, byte[] needle, int fromIndex) {
        if (fromIndex < 0 || needle.length == 0 || fromIndex + needle.length > data.length) {
            return -1;
        }
        for (int i = fromIndex; i <= data.length - needle.length; i++) {
            boolean match = true;
            for (int j = 0; j < needle.length; j++) {
                if (data[i + j] != needle[j]) {
                    match = false;
                    break;
                }
            }
            if (match) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Get last committed version of the file from .svn/pristine (SVN 1.7+). Reads wc.db to resolve path to checksum, then reads the pristine file.
     *
     * @param f
     *            working copy file
     * @return base file content or null if not available
     */
    private static String getPristineContent(File f) {
        File svnDir = findSvnDir(f);
        if (svnDir == null) {
            return null;
        }
        File wcRoot = svnDir.getParentFile();
        File wcDb = new File(svnDir, "wc.db");
        if (!wcDb.isFile()) {
            return null;
        }
        String localRelpath;
        try {
            localRelpath = Files.getRelativePath(wcRoot, f);
            if (localRelpath == null) {
                return null;
            }
            localRelpath = localRelpath.replace('\\', '/');
        } catch (Exception e) {
            return null;
        }
        String checksum;
        try {
            checksum = readChecksumFromWcDb(wcDb, localRelpath);
        } catch (IOException e) {
            LogV3.finest("Could not read wc.db: " + e.getMessage());
            return null;
        }
        if (checksum == null || checksum.length() != SHA1_HEX_LEN) {
            return null;
        }
        String subdir = checksum.substring(0, 2);
        File pristineFile = new File(new File(svnDir, "pristine"), subdir + "/" + checksum + ".svn-base");
        if (!pristineFile.isFile()) {
            return null;
        }
        try {
            return IO.readFileToString(pristineFile);
        } catch (IOException e) {
            LogV3.finest("Could not read pristine file: " + e.getMessage());
            return null;
        }
    }

    public static void main(String[] args) {
        run();
    }

    @Override
    public boolean isSkipOnUnchangedDependencies() {
        return false;
    }

    @Override
    public void runTest() throws Exception {
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            return;
        }
        final File appWorkUtilsRoot = new File(IDEUtils.getWorkSpace(), "AppWorkUtils");
        // Verify SVN pristine lookup once with a known file
        final File testFile = new File(appWorkUtilsRoot, "src/org/appwork/utils/Hash.java");
        if (testFile.isFile()) {
            String pristine = getPristineContent(testFile);
            if (pristine == null || pristine.length() < 100) {
                throw new Exception("SVN pristine lookup failed for " + testFile.getPath() + ": got " + (pristine == null ? "null" : "too short"));
            }
            if (!pristine.contains("package org.appwork")) {
                throw new Exception("SVN pristine content unexpected for " + testFile.getPath());
            }
        }
        final ArrayList<File> modified = new ArrayList<File>();
        Files.walkThroughStructure(new Handler<Exception>() {
            @Override
            public void intro(File f) throws Exception {
            }

            @Override
            public void onFile(File f) throws Exception {
                BasicFileAttributes attrs = java.nio.file.Files.readAttributes(f.toPath(), BasicFileAttributes.class);
                // Creation time as epoch milliseconds
                long timestamp = attrs.creationTime().toMillis();
                if (f.lastModified() - timestamp < TimeUnit.DAYS.toMillis(3)) {
                    return;
                }
                if (f.isFile() && f.getName().endsWith(".java")) {
                    if (f.getName().equals((ScanHeaderTest.class.getSimpleName() + ".java"))) {
                        return;
                    }
                    if (f.lastModified() > System.currentTimeMillis() - 7 * 24 * 60 * 60 * 1000l) {
                        String java = IO.readFileToString(f);
                        int year = new Date().getYear() + 1900;
                        String newJava = java.replaceAll("Copyright \\(c\\) 2009-20\\d\\d,", "Copyright (c) 2009-" + year + ",");
                        newJava = newJava.replace("Schwabacher Straße 117", "Spalter Strasse 58");
                        newJava = newJava.replace("Schwabacher StraÃŸe 117", "Spalter Strasse 58");// files with broken encoding
                        newJava = newJava.replace("90763 Fürth", "91183 Abenberg");
                        newJava = newJava.replace("90763 FÃ¼rth", "91183 Abenberg");// files with broken encoding
                        if (!newJava.contains("e-mail@appwork.org")) {
                            newJava = newJava.replace("91183 Abenberg", "91183 Abenberg\r\n *         e-mail@appwork.org");
                        }
                        if (!newJava.equals(java)) {
                            String currentNonWsHash = nonWhitespaceHash(newJava);
                            String pristineContent = getPristineContent(f);
                            String previousNonWsHash = pristineContent != null ? nonWhitespaceHash(pristineContent) : null;
                            boolean relevantChange = previousNonWsHash == null || !previousNonWsHash.equals(currentNonWsHash);
                            if (relevantChange) {
                                IO.secureWrite(f, newJava, SYNC.META_AND_DATA);
                                modified.add(f);
                            }
                        }
                    }
                }
            }

            @Override
            public void outro(File f) throws Exception {
            }
        }, appWorkUtilsRoot);
        if (modified.size() > 0) {
            LogV3.info("This tests finds and corrects AppWorkUtils headers in modified files. You do not have to change anything - simply re-run the test");
            throw new Exception("Found and fixed incorrect (Abenberg)header: \r\n" + StringUtils.join(modified, "\r\n"));
        }
    }
}
