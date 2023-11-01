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
package org.appwork.utils.net.ftpserver;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

/**
 * @author daniel
 * 
 */
public abstract class FtpConnectionHandler<E extends FtpFile> {

    /**
     * @return
     */
    public FtpConnectionState createNewConnectionState() {

        return new FtpConnectionState();
    }

    public String formatFileList(final java.util.List<? extends FtpFile> list) {
        final String DEL = " ";
        final StringBuilder sb = new StringBuilder();
        final Calendar cal = Calendar.getInstance();
        cal.setTimeInMillis(System.currentTimeMillis());
        cal.set(Calendar.DAY_OF_MONTH, 1);
        cal.set(Calendar.MONTH, 0);
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MINUTE, 0);
        final SimpleDateFormat dfCur = new SimpleDateFormat("MMM dd HH:mm", Locale.ENGLISH);
        final SimpleDateFormat dfHist = new SimpleDateFormat("MMM dd yyyy", Locale.ENGLISH);
        for (final FtpFile f : list) {
            // directory or not
            sb.append(f.isDirectory() ? "d" : "-");
            // rights
            sb.append("rwxrwxrwx");
            sb.append(DEL);
            sb.append("0");
            sb.append(DEL);
            // group
            sb.append(f.getGroup());
            sb.append(DEL);
            // user
            sb.append(f.getOwner());
            sb.append(DEL);
            sb.append(f.getSize());
            sb.append(DEL);
            if (f.getLastModified() > cal.getTimeInMillis()) {
                sb.append(dfCur.format(new Date(f.getLastModified())));
            } else {
                sb.append(dfHist.format(new Date(f.getLastModified())));
            }
            sb.append(DEL);
            sb.append(f.getName());
            sb.append("\r\n");
        }
        return sb.toString();
    }

    /**
     * @param string
     * @return TODO
     * @throws IOException
     * @throws UnsupportedEncodingException
     */
    public abstract java.util.List<E> getFileList(FtpConnectionState connectionState, String string) throws UnsupportedEncodingException, IOException, FtpException;

    /**
     * @param connectionState
     * @param buildParameter
     * @return
     */
    public abstract long getSize(FtpConnectionState connectionState, String buildParameter) throws FtpException;

    public abstract FTPUser getUser(final String user);

    public abstract String getWelcomeMessage(FtpConnectionState ftpConnection);

    /**
     * @param connectionState
     * @param buildParameter
     */
    public abstract void makeDirectory(FtpConnectionState connectionState, String buildParameter) throws FtpException;

    /**
     * @param dir
     * @param connectionState
     * @return
     */
    public abstract void onDirectoryUp(FtpConnectionState connectionState) throws FtpException;

    public abstract String onLoginFailedMessage(FtpConnectionState ftpConnection) throws FtpException;

    public abstract String onLoginSuccessRequest(FtpConnectionState connectionState) throws FtpException;

    public abstract String onLogoutRequest(FtpConnectionState connectionState) throws FtpException;

    /**
     * @param connectionState
     * @param position
     */
    public void onREST(final FtpConnectionState connectionState, final long position) throws FtpException {
        throw new FtpCommandNotImplementedException();
    }

    /**
     * @param outputStream
     * @param connectionState
     * @param param
     */
    public abstract long onRETR(OutputStream outputStream, FtpConnectionState connectionState, String param) throws IOException, FtpException;

    /**
     * @param inputStream
     * @param connectionState
     * @param buildParameter
     * @return
     * @throws FtpFileNotExistException
     * @throws IOException
     */
    public abstract long onSTOR(InputStream inputStream, FtpConnectionState connectionState, boolean append, String buildParameter) throws FtpException, IOException;

    /**
     * @param connectionState
     * @param buildParameter
     * @throws FtpFileNotExistException
     * @throws FtpException
     */
    public abstract void removeDirectory(FtpConnectionState connectionState, String buildParameter) throws FtpException;

    /**
     * @param connectionState
     * @param buildParameter
     */
    public abstract void removeFile(FtpConnectionState connectionState, String buildParameter) throws FtpException;

    /**
     * @param connectionState
     * @param buildParameter
     */
    /*
     * this function gets called twice:
     * 
     * 1.) by RNFR, which should set the renameFile in connectionState if
     * renaming is possible
     * 
     * 2.) by RNTO, which should do the actual renaming
     */
    public abstract void renameFile(FtpConnectionState connectionState, String buildParameter) throws FtpException;

    /**
     * @param connectionState
     * @param cwd
     * @return
     */
    public abstract void setCurrentDirectory(FtpConnectionState connectionState, String cwd) throws FtpException;

}
