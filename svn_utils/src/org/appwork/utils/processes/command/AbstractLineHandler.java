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
package org.appwork.utils.processes.command;

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;

import org.appwork.utils.net.LineParsingInputStream;
import org.appwork.utils.net.LineParsingOutputStream;
import org.appwork.utils.net.LineParsingOutputStream.NEWLINE;
import org.appwork.utils.net.NullOutputStream;
import org.appwork.utils.processes.LineHandler;

/**
 * @author Thomas
 * @date 08.11.2018
 *
 */
public abstract class AbstractLineHandler implements LineHandler, OutputHandler {

    /**
     * @author thomas
     * @date 09.11.2020
     *
     */
    private final class LineParser extends LineParsingInputStream {
        /**
         *
         */
        private final InputStream inputStream;

        /**
         * @param is
         * @param charset
         * @param inputStream
         */
        private LineParser(InputStream is, Charset charset, InputStream inputStream) {
            super(is, charset);
            this.inputStream = inputStream;
        }

        @Override
        protected void onNextLine(NEWLINE newLine, long line, StringBuilder sb, int startIndex, int endIndex) {
            AbstractLineHandler.this.handleLine(sb.substring(startIndex, endIndex), inputStream);
        }

        protected LineParsingOutputStream createOutputStream(Charset charset, int bufferSize) {

            return new LineParsingOutputStream(charset, bufferSize) {
                @Override
                protected void onNextLine(NEWLINE newLine, long line, StringBuilder sb, int startIndex, int endIndex) {
                    LineParser.this.onNextLine(newLine, line, sb, startIndex, endIndex);
                }

                /*
                 * (non-Javadoc)
                 *
                 * @see org.appwork.utils.net.LineParsingOutputStream#split(java.lang.StringBuilder)
                 */
                @Override
                protected int split(StringBuilder sb) {                    
                    return AbstractLineHandler.this.splitLine(sb);
                }
            };
        }
    }

    public AbstractLineHandler() {
    }

    protected int getLineReaderBufferSize() {
        return 8 * 1024;
    }

    protected boolean killProcessInFinally(ProcessStreamReader streamReader, Exception e) {
        // old inner LineReaderThread class did not kill Process
        return false;
    }

    protected boolean ignoreOutputStreamException(ProcessStreamReader streamReader, IOException e) {
        // old inner LineReaderThread had no outputstream
        return true;
    }

    @Override
    public void onExitCode(int exitCode) {
    }

    protected LineParsingInputStream createLineParsingInputStream(final InputStream inputStream, Charset charset) throws UnsupportedEncodingException {
        return new LineParser(inputStream, charset, inputStream);
    }

    /**
     * @param sb
     * @return
     */
    protected int splitLine(StringBuilder sb) {        
        return 0;
    }

    @Override
    public AsyncInputStreamHandler createAsyncStreamHandler(ProcessInputStream inputStream, Charset charset) throws UnsupportedEncodingException, InterruptedException {
        final LineParsingInputStream is = createLineParsingInputStream(inputStream, charset);
        final Process process = inputStream.getProcess();
        return new ProcessStreamReader("Line-Reader-Std:" + process, process, is, new NullOutputStream()) {

            @Override
            protected int getReadBufferSize() {
                return getLineReaderBufferSize();
            }

            @Override
            protected boolean ignoreOutputStreamException(IOException e) {
                return AbstractLineHandler.this.ignoreOutputStreamException(this, e);
            }

            @Override
            protected boolean killProcessInFinally(Exception e) {
                return AbstractLineHandler.this.killProcessInFinally(this, e);
            }
        };
    }

    @Override
    public AsyncInputStreamHandler createAsyncStreamHandler(ProcessErrorStream inputStream, Charset charset) throws UnsupportedEncodingException, InterruptedException {
        final LineParsingInputStream is = createLineParsingInputStream(inputStream, charset);
        final Process process = inputStream.getProcess();
        return new ProcessStreamReader("Line-Reader-Error:" + process, process, is, new NullOutputStream()) {

            @Override
            protected int getReadBufferSize() {
                return getLineReaderBufferSize();
            }

            @Override
            protected boolean ignoreOutputStreamException(IOException e) {
                return AbstractLineHandler.this.ignoreOutputStreamException(this, e);
            }

            @Override
            protected boolean killProcessInFinally(Exception e) {
                return AbstractLineHandler.this.killProcessInFinally(this, e);
            }
        };
    }
}
