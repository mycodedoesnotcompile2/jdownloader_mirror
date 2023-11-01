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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.ftpserver.test;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;

import org.appwork.utils.Files;
import org.appwork.utils.net.ftpserver.FTPUser;
import org.appwork.utils.net.ftpserver.FtpConnectionHandler;
import org.appwork.utils.net.ftpserver.FtpConnectionState;
import org.appwork.utils.net.ftpserver.FtpException;
import org.appwork.utils.net.ftpserver.FtpFile;
import org.appwork.utils.net.ftpserver.FtpFileNotExistException;
import org.appwork.utils.net.ftpserver.FtpServer;

/**
 * @author daniel
 * 
 */
public class FTPTest {
    protected static final File ROOT = new File("/home/daniel/test");

    public static void main(final String[] args) throws IOException {
        final FtpConnectionHandler<FtpTestFile> handler = new FtpConnectionHandler<FtpTestFile>() {
            /**
             * @return
             */
            @Override
            public FtpConnectionState createNewConnectionState() {
                final FtpConnectionState state = new FtpConnectionState();
                state.setCurrentDir("/");
                return state;
            }

            @Override
            public java.util.List<FtpTestFile> getFileList(final FtpConnectionState connectionState, final String item) throws UnsupportedEncodingException, IOException, FtpFileNotExistException {
                if (item == null || item.length() == 0 || item.startsWith("-") || item.startsWith("*")) {
                    return list(new File(FTPTest.ROOT, connectionState.getCurrentDir()));
                } else {
                    final File file = new File(item);
                    return list(file);
                }
            }

            @Override
            public FTPUser getUser(final String user) {
                if ("test".equals(user)) {
                    return new FTPUser("test", "test");
                } else {
                    return null;
                }
            }

            @Override
            public String getWelcomeMessage(final FtpConnectionState connectionState) {
                return "Hallo " + connectionState;
            }

            /**
             * @param file
             * @return
             * @throws FtpFileNotExistException
             */
            protected java.util.List<FtpTestFile> list(final File file) throws FtpFileNotExistException {
                final java.util.List<FtpTestFile> ret = new ArrayList<FtpTestFile>();
                if (!file.exists()) {
                    throw new FtpFileNotExistException();
                }
                if (file.isDirectory()) {
                    final File[] list = file.listFiles();
                    if (list != null) {
                        for (final File item : list) {
                            ret.add(new FtpTestFile(item.getName(), item.length(), item.isDirectory(), item.lastModified()));
                        }
                    } else {
                        throw new FtpFileNotExistException();
                    }
                } else {
                    ret.add(new FtpTestFile(file.getName(), file.length(), file.isDirectory(), file.lastModified()));
                }
                return ret;
            }

            @Override
            public void onDirectoryUp(final FtpConnectionState connectionState) throws FtpFileNotExistException {
                if (connectionState.getCurrentDir().equals("/")) {
                    throw new FtpFileNotExistException();
                }
                final File newcur = new File(FTPTest.ROOT, connectionState.getCurrentDir()).getParentFile();
                if (newcur != null) {
                    connectionState.setCurrentDir(Files.getRelativePath(FTPTest.ROOT, newcur));
                    return;
                }
                throw new FtpFileNotExistException();
            }

            @Override
            public String onLoginFailedMessage(final FtpConnectionState con) {
                return "NÄ " + con.getUser();
            }

            @Override
            public String onLoginSuccessRequest(final FtpConnectionState con) {
                return "OK " + con.getUser();
            }

            @Override
            public String onLogoutRequest(final FtpConnectionState connectionState) {
                return "Bye";
            }

            @Override
            public void setCurrentDirectory(final FtpConnectionState connectionState, final String cwd) throws FtpFileNotExistException {
                File newcur = null;
                if (cwd.startsWith("/")) {
                    newcur = new File(FTPTest.ROOT, cwd);
                } else {
                    newcur = new File(new File(FTPTest.ROOT, connectionState.getCurrentDir()), cwd);
                }
                final String rel = Files.getRelativePath(FTPTest.ROOT, newcur);
                if (rel == null) {
                    throw new FtpFileNotExistException();
                }
                if (newcur.exists() && newcur.isDirectory()) {
                    connectionState.setCurrentDir(rel);
                    return;
                }
                throw new FtpFileNotExistException();
            }

            @Override
            public long onRETR(OutputStream outputStream, FtpConnectionState connectionState, String param) throws IOException, FtpFileNotExistException {
                File newcur = null;
                if (param.startsWith("/")) {
                    newcur = new File(FTPTest.ROOT, param);
                } else {
                    newcur = new File(new File(FTPTest.ROOT, connectionState.getCurrentDir()), param);
                }
                final String rel = Files.getRelativePath(FTPTest.ROOT, newcur);
                if (rel == null) {
                    throw new FtpFileNotExistException();
                }
                if (newcur.exists() && newcur.isFile()) {
                    FileInputStream fis = null;
                    try {
                        try {
                            fis = new FileInputStream(newcur);
                        } catch (FileNotFoundException e) {
                            throw new FtpFileNotExistException();
                        }
                        byte[] temp = new byte[8192];
                        int read = 0;
                        int written = 0;
                        while ((read = fis.read(temp)) >= 0) {
                            if (read > 0) {
                                written = written + read;
                                outputStream.write(temp, 0, read);
                            }
                        }
                        return written;
                    } finally {
                        try {
                            fis.close();
                        } catch (final Throwable e) {
                        }
                    }
                }
                throw new FtpFileNotExistException();
            }

            @Override
            public void makeDirectory(FtpConnectionState connectionState, String cwd) throws FtpFileNotExistException {
                File newcur = null;
                if (cwd.startsWith("/")) {
                    newcur = new File(FTPTest.ROOT, cwd);
                } else {
                    newcur = new File(new File(FTPTest.ROOT, connectionState.getCurrentDir()), cwd);
                }
                final String rel = Files.getRelativePath(FTPTest.ROOT, newcur);
                if (rel == null) {
                    throw new FtpFileNotExistException();
                }
                if (!newcur.exists()) {
                    newcur.mkdir();
                    return;
                }
                throw new FtpFileNotExistException();
            }

            @Override
            public long onSTOR(InputStream inputStream, FtpConnectionState connectionState, boolean append, String param) throws FtpFileNotExistException, IOException {
                File newcur = null;
                if (param.startsWith("/")) {
                    newcur = new File(FTPTest.ROOT, param);
                } else {
                    newcur = new File(new File(FTPTest.ROOT, connectionState.getCurrentDir()), param);
                }
                final String rel = Files.getRelativePath(FTPTest.ROOT, newcur);
                if (rel == null) {
                    throw new FtpFileNotExistException();
                }
                if (!newcur.isDirectory()) {
                    RandomAccessFile fos = null;
                    try {
                        try {
                            fos = new RandomAccessFile(newcur, "rw");
                            if (append) {
                                /* append at end of file */
                                fos.seek(newcur.length());
                            }
                        } catch (FileNotFoundException e) {
                            throw new FtpFileNotExistException();
                        }
                        byte[] temp = new byte[8192];
                        int read = 0;
                        int written = 0;
                        while ((read = inputStream.read(temp)) >= 0) {
                            if (read > 0) {
                                written = written + read;
                                fos.write(temp, 0, read);
                            }
                        }
                        return written;
                    } finally {
                        try {
                            fos.close();
                        } catch (final Throwable e) {
                        }
                    }
                }
                throw new FtpFileNotExistException();
            }

            @Override
            public long getSize(FtpConnectionState connectionState, String cwd) throws FtpFileNotExistException {
                File newcur = null;
                if (cwd.startsWith("/")) {
                    newcur = new File(FTPTest.ROOT, cwd);
                } else {
                    newcur = new File(new File(FTPTest.ROOT, connectionState.getCurrentDir()), cwd);
                }
                final String rel = Files.getRelativePath(FTPTest.ROOT, newcur);
                if (rel == null) {
                    throw new FtpFileNotExistException();
                }
                if (newcur.exists() && newcur.isFile()) {
                    return newcur.length();
                }
                throw new FtpFileNotExistException();
            }

            @Override
            public void removeDirectory(FtpConnectionState connectionState, String cwd) throws FtpException {
                File newcur = null;
                if (cwd.startsWith("/")) {
                    newcur = new File(FTPTest.ROOT, cwd);
                } else {
                    newcur = new File(new File(FTPTest.ROOT, connectionState.getCurrentDir()), cwd);
                }
                final String rel = Files.getRelativePath(FTPTest.ROOT, newcur);
                if (rel == null) {
                    throw new FtpFileNotExistException();
                }
                if (newcur.exists()) {
                    if (newcur.isFile() || !newcur.delete()) {
                        throw new FtpException(550, "Could not delete " + cwd);
                    }
                    return;
                }
                throw new FtpFileNotExistException();
            }

            @Override
            public void removeFile(FtpConnectionState connectionState, String cwd) throws FtpException {
                File newcur = null;
                if (cwd.startsWith("/")) {
                    newcur = new File(FTPTest.ROOT, cwd);
                } else {
                    newcur = new File(new File(FTPTest.ROOT, connectionState.getCurrentDir()), cwd);
                }
                final String rel = Files.getRelativePath(FTPTest.ROOT, newcur);
                if (rel == null) {
                    throw new FtpFileNotExistException();
                }
                if (newcur.exists()) {
                    if (newcur.isDirectory() || !newcur.delete()) {
                        throw new FtpException(550, "Could not delete " + cwd);
                    }
                    return;
                }
                throw new FtpFileNotExistException();
            }

            @Override
            public void renameFile(FtpConnectionState connectionState, String cwd) throws FtpFileNotExistException {
                File newcur = null;
                if (cwd.startsWith("/")) {
                    newcur = new File(FTPTest.ROOT, cwd);
                } else {
                    newcur = new File(new File(FTPTest.ROOT, connectionState.getCurrentDir()), cwd);
                }
                final String rel = Files.getRelativePath(FTPTest.ROOT, newcur);
                if (rel == null) {
                    throw new FtpFileNotExistException();
                }
                FtpFile rnfrom = connectionState.getRenameFile();
                if (rnfrom != null) {
                    File oldFile = ((FtpTestFile) rnfrom).getFile();
                    if (oldFile.renameTo(newcur)) {/* do the rename */
                        return;
                    }
                } else {
                    if (!newcur.exists()) {
                        throw new FtpFileNotExistException();
                    }
                    /* set rename old file */
                    FtpTestFile ff = new FtpTestFile(newcur.getName(), newcur.length(), newcur.isDirectory(), newcur.lastModified());
                    ff.setFile(newcur);
                    connectionState.setRenameFile(ff);
                    return;
                }
            }
        };
        final FtpServer server = new FtpServer(handler, 8080);
        server.start();
    }
}
