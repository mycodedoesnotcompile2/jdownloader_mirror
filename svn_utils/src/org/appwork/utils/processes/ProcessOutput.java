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
package org.appwork.utils.processes;

import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.Exceptions;

/**
 * @author Thomas
 *
 */
public class ProcessOutput {
    private final int                   exitCode;
    private final ByteArrayOutputStream stdOutData;

    public ByteArrayOutputStream getStdOutData() {
        return stdOutData;
    }

    public ByteArrayOutputStream getErrOutData() {
        return errOutData;
    }

    private final ByteArrayOutputStream errOutData;
    private final String                codePage;

    /**
     * @param codePage
     * @param waitFor
     * @param byteArray
     * @param byteArray2
     * @throws InterruptedException
     */
    public ProcessOutput(int exitCode, ByteArrayOutputStream stdOut, ByteArrayOutputStream errOut, String codePage) {
        this.exitCode = exitCode;
        this.stdOutData = stdOut;
        this.errOutData = errOut;
        this.codePage = codePage;
    }

    @Override
    public String toString() {
        return "Exitcode: " + getExitCode() + " STD:" + getStdOutString() + " ERR:" + getErrOutString();
    }

    public String getStdOutString(String charset) throws UnsupportedEncodingException {
        return getStdOutData().toString(charset);
    }

    public String getErrOutString(String charset) throws UnsupportedEncodingException {
        return getErrOutData().toString(charset);
    }

    public int getExitCode() {
        return exitCode;
    }

    public String getStdOutString() {
        try {
            return getStdOutString(codePage);
        } catch (UnsupportedEncodingException e) {
            try {
                return getStdOutString("UTF-8");
            } catch (UnsupportedEncodingException e2) {
                throw Exceptions.addSuppressed(new WTFException(e2), e);
            }
        }
    }

    /**
     * @return
     * @throws InterruptedException
     */
    public String getErrOutString() {
        try {
            return getErrOutString(codePage);
        } catch (UnsupportedEncodingException e) {
            try {
                return getErrOutString("UTF-8");
            } catch (UnsupportedEncodingException e2) {
                throw Exceptions.addSuppressed(new WTFException(e2), e);
            }
        }
    }
}
