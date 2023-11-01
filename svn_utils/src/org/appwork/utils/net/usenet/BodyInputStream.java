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
package org.appwork.utils.net.usenet;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;

/**
 * @author daniel
 * @date 20.09.2018
 *
 */
public class BodyInputStream extends InputStream {
    private final InputStream inputStream;

    protected BodyInputStream(SimpleUseNet client) throws IOException {
        this(client.getInputStream());
    }

    protected BodyInputStream(InputStream inputStream) throws IOException {
        this.inputStream = inputStream;
    }

    private int singleDotLineState = -1;

    /**
     * RFC: https://tools.ietf.org/html/rfc3977#section-3.1.1
     *
     * @return
     * @throws IOException
     */
    @Override
    public synchronized int read() throws IOException {
        if (singleDotLineState == 5) {
            return -1;
        } else {
            final int ret = inputStream.read();
            if (ret == -1) {
                throw new EOFException("singleDotLineState:" + singleDotLineState);
            } else {
                switch (singleDotLineState) {
                case -1:
                    // current:
                    // special handling in case the current line is already the last single dot line
                    if (ret == '.') {
                        // next: 0d0a2e
                        singleDotLineState = 3;
                    } else {
                        singleDotLineState = 0;
                    }
                    break;
                case 0:
                    // current:
                    if (ret == 13) {
                        // next: 0d
                        singleDotLineState++;
                    } else {
                        singleDotLineState = 0;
                    }
                    break;
                case 1:
                    // current: 0d
                    if (ret == 10) {
                        // next: 0d0a
                        singleDotLineState++;
                    } else {
                        singleDotLineState = 0;
                    }
                    break;
                case 2:
                    // current: 0d0a
                    if (ret == 13) {
                        // next: 0d0a0d -> 0d
                        singleDotLineState = 1;
                    } else if (ret == '.') {
                        // next: 0d0a2e
                        singleDotLineState++;
                    } else {
                        singleDotLineState = 0;
                    }
                    break;
                case 3:
                    // current: 0d0a2e
                    if (ret == 13) {
                        // next: 0d0a2e0d
                        singleDotLineState++;
                    } else {
                        singleDotLineState = 0;
                    }
                    break;
                case 4:
                    // current: 0d0a2e0d
                    if (ret == 10) {
                        // next: 0d0a2e0d0a
                        singleDotLineState++;
                    } else {
                        singleDotLineState = 0;
                    }
                    break;
                default:
                    break;
                }
                return ret;
            }
        }
    }

    @Override
    public int available() throws IOException {
        if (singleDotLineState == 5) {
            return 0;
        } else {
            return inputStream.available();
        }
    }
}
