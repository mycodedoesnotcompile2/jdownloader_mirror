/**
 * ====================================================================================================================================================
 * "My JDownloader Client" License
 * The "My JDownloader Client" will be called [The Product] from now on.
 * ====================================================================================================================================================
 * Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 * Schwabacher Straße 117
 * 90763 Fürth
 * Germany
 * === Preamble ===
 * This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 * The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 * These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 * <p>
 * === 3rd Party Licences ===
 * Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 * to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 * <p>
 * === Definition: Commercial Usage ===
 * If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 * If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 * If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 * Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 * If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 * "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 * <p>
 * If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ====================================================================================================================================================
 */
package org.jdownloader.myjdownloader.client.exceptions;

import org.jdownloader.myjdownloader.client.json.ErrorResponse.Source;

public class MyJDownloaderException extends Exception {

    private Source source = Source.UNKNOWN;

    public Source getSource() {
        return this.source;
    }

    public MyJDownloaderException(final String string) {
        super(string);
    }

    public MyJDownloaderException() {
    }

    public MyJDownloaderException(final Throwable e) {
        super(e);
    }

    public MyJDownloaderException(final Throwable e, Source source) {
        super(e);
        this.source = source;
    }

    public MyJDownloaderException(final String string, final Exception e) {
        super(string, e);
    }

    public static MyJDownloaderException get(final Exception e) {
        if (e instanceof MyJDownloaderException) {
            return (MyJDownloaderException) e;
        }
        return new MyJDownloaderException(e);
    }

    public static MyJDownloaderException get(final Exception e, String message) {
        if (e instanceof MyJDownloaderException) {
            return (MyJDownloaderException) e;
        }
        return new MyJDownloaderException(message, e);
    }

    @Override
    public String toString() {
        return super.toString() + "(SRC: " + this.getSource() + ")";
    }

    public void setSource(final Source src) {
        if (src == null) {
            this.source = Source.UNKNOWN;
        } else {
            this.source = src;
        }
    }
}
