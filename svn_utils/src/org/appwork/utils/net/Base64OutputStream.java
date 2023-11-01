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
package org.appwork.utils.net;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * @author daniel
 * 
 */
public class Base64OutputStream extends FilterOutputStream {
    private static final char[] BASE64         = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toCharArray();
    private final int           base64buffer[] = new int[3];
    private final byte          writebuffer[]  = new byte[4];
    private int                 index          = 0;
    private static final byte   PADDING        = (byte) '=';
    private boolean             endFlush       = false;

    /**
     * @param out
     */
    public Base64OutputStream(final OutputStream out) {
        super(out);
    }

    @Override
    public void close() throws IOException {
        this.flush(true);
        super.close();
    }

    @Override
    public void flush() throws IOException {
        if (this.index == 0 || !this.endFlush) {
            this.out.flush();
            return;
        }
        if (this.endFlush == true) {
            /* a Base64 Stream can only be padded once at the end! */
            this.writebuffer[2] = Base64OutputStream.PADDING;
            this.writebuffer[3] = Base64OutputStream.PADDING;
            switch (this.index) {
            case 1:
                this.writebuffer[0] = (byte) Base64OutputStream.BASE64[(this.base64buffer[0] & 0xFC) >> 2];
                this.writebuffer[1] = (byte) Base64OutputStream.BASE64[(this.base64buffer[0] & 0x03) << 4];
                this.out.write(this.writebuffer);
                break;
            case 2:
                this.writebuffer[0] = (byte) Base64OutputStream.BASE64[(this.base64buffer[0] & 0xFC) >> 2];
                this.writebuffer[1] = (byte) Base64OutputStream.BASE64[(this.base64buffer[0] & 0x03) << 4 | (this.base64buffer[1] & 0xF0) >> 4];
                this.writebuffer[2] = (byte) Base64OutputStream.BASE64[(this.base64buffer[1] & 0x0F) << 2];
                this.out.write(this.writebuffer);
                break;
            }
        }
        this.index = 0;
        this.out.flush();
    }

    public void flush(final boolean padding) throws IOException {
        if (padding) {
            this.endFlush = true;
        }
        this.flush();
    }

    @Override
    public void write(final int b) throws IOException {
        /* put byte into base64Buffer */
        this.base64buffer[this.index++] = b;
        if (this.index == 3) {
            this.index = 0;
            /* first 6 bits, &0xFC returns bit 7-2 and >>2 shifts down */
            this.writebuffer[0] = (byte) Base64OutputStream.BASE64[(this.base64buffer[0] & 0xFC) >> 2];
            /*
             * second 6 bits, &0x03 returns bit 1-0 and <<4 shifts up, &0xF0
             * returns bit 7-4 and >>4 shifts down
             */
            this.writebuffer[1] = (byte) Base64OutputStream.BASE64[(this.base64buffer[0] & 0x03) << 4 | (this.base64buffer[1] & 0xF0) >> 4];
            /*
             * third 6 bits, &0x0F returns bit 3-0 and <<2 shifts up, &0xC0
             * returns bit 7-6 and >>6 shifts down
             */
            this.writebuffer[2] = (byte) Base64OutputStream.BASE64[(this.base64buffer[1] & 0x0F) << 2 | (this.base64buffer[2] & 0xC0) >> 6];
            /* last 6 bits, 0x3F returns bit 5-0 */
            this.writebuffer[3] = (byte) Base64OutputStream.BASE64[this.base64buffer[2] & 0x3F];
            this.out.write(this.writebuffer);
        }
    }
}
