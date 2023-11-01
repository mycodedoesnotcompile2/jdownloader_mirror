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
package org.appwork.utils;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import javax.swing.TransferHandler.TransferSupport;

import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.parser.HTMLParser;

public class ClipboardUtils {

    public final static DataFlavor fileListFlavor = DataFlavor.javaFileListFlavor;
    public final static DataFlavor stringFlavor   = DataFlavor.stringFlavor;
    private final static byte[]    tmpByteArray   = new byte[0];
    public final static DataFlavor arrayListFlavor;
    public final static DataFlavor uriListFlavor;

    static {
        DataFlavor tmp;
        try {
            tmp = new DataFlavor("text/uri-list; class=java.lang.String");
        } catch (final Throwable e) {
            tmp = null;
        }
        uriListFlavor = tmp;
        try {
            tmp = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType + ";class=java.util.ArrayList");
        } catch (final Throwable e) {
            tmp = null;
        }
        arrayListFlavor = tmp;
    }

    @SuppressWarnings("unchecked")
    public static java.util.List<File> getFiles(final Transferable info) {
        final java.util.List<File> files = new ArrayList<File>();
        String inString = null;
        if (info != null) {
            StringTokenizer izer;
            try {
                if (info.isDataFlavorSupported(ClipboardUtils.fileListFlavor)) {
                    final List<File> list = (List<File>) info.getTransferData(ClipboardUtils.fileListFlavor);
                    for (final File f : list) {
                        if (f.isAbsolute() && f.exists()) {
                            files.add(f);
                        }
                    }
                } else if (ClipboardUtils.uriListFlavor != null && info.isDataFlavorSupported(ClipboardUtils.uriListFlavor)) {
                    inString = (String) info.getTransferData(ClipboardUtils.uriListFlavor);
                    izer = new StringTokenizer(inString, "\r\n");
                    while (izer.hasMoreTokens()) {
                        final String token = izer.nextToken().trim();
                        try {
                            final URI fi = new URI(token);
                            final File f = new File(fi.getPath());
                            if (f.isAbsolute() && f.exists()) {
                                files.add(f);
                            }
                        } catch (final Throwable e) {
                        }
                    }
                }
            } catch (final Exception e) {
                org.appwork.loggingv3.LogV3.warning(inString);
                org.appwork.loggingv3.LogV3.warning(e.getMessage());
            }
        }
        return files;
    }

    @SuppressWarnings("unchecked")
    public static java.util.List<File> getFiles(final TransferSupport info) {
        final java.util.List<File> files = new ArrayList<File>();
        String inString = null;
        if (info != null) {
            StringTokenizer izer;
            try {
                if (info.isDataFlavorSupported(ClipboardUtils.fileListFlavor)) {
                    final List<File> list = (List<File>) info.getTransferable().getTransferData(ClipboardUtils.fileListFlavor);
                    for (final File f : list) {
                        if (f.isAbsolute() && f.exists()) {
                            files.add(f);
                        }
                    }
                } else if (ClipboardUtils.uriListFlavor != null && info.isDataFlavorSupported(ClipboardUtils.uriListFlavor)) {
                    inString = (String) info.getTransferable().getTransferData(ClipboardUtils.uriListFlavor);
                    izer = new StringTokenizer(inString, "\r\n");
                    while (izer.hasMoreTokens()) {
                        final String token = izer.nextToken().trim();
                        try {
                            final URI fi = new URI(token);
                            final File f = new File(fi);
                            if (f.isAbsolute() && f.exists()) {
                                files.add(f);
                            }
                        } catch (final Throwable e) {
                        }
                    }
                }
            } catch (final Exception e) {
                org.appwork.loggingv3.LogV3.warning(inString);
                org.appwork.loggingv3.LogV3.warning(e.getMessage());
            }
        }
        return files;
    }

    public static java.util.List<String> getLinks(final Transferable trans) {
        final java.util.List<String> links = new ArrayList<String>();
        String content = null;
        DataFlavor htmlFlavor = null;
        /*
         * workaround for https://bugzilla.mozilla.org/show_bug.cgi?id=385421
         */
        try {
            for (final DataFlavor flav : trans.getTransferDataFlavors()) {
                if (flav.getMimeType().contains("html") && flav.getRepresentationClass().isInstance(ClipboardUtils.tmpByteArray)) {
                    if (htmlFlavor != null) {
                        htmlFlavor = flav;
                    }
                    final String charSet = new Regex(flav.toString(), "charset=(.*?)]").getMatch(0);
                    if (charSet != null && charSet.equalsIgnoreCase("UTF-8")) {
                        /* we found utf-8 encoding, so lets use that */
                        htmlFlavor = flav;
                        break;
                    }
                }
            }

            if (htmlFlavor != null) {
                final String charSet = new Regex(htmlFlavor.toString(), "charset=(.*?)]").getMatch(0);
                byte[] html = (byte[]) trans.getTransferData(htmlFlavor);
                if (CrossSystem.isUnix()) {
                    /*
                     * workaround for https://bugzilla.mozilla.org/show_bug.cgi?id=385421if
                     */
                    final int htmlLength = html.length;
                    final byte[] html2 = new byte[htmlLength];

                    int o = 0;
                    for (int i = 6; i < htmlLength - 1; i++) {
                        if (html[i] != 0) {
                            html2[o++] = html[i];
                        }
                    }
                    html = html2;
                    content = new String(html, "UTF-8");
                } else {
                    if (charSet != null) {
                        content = new String(html, charSet);
                    } else {
                        content = new String(html);
                    }
                }
            } else {
                /* try stringFlavor */
                if (trans.isDataFlavorSupported(ClipboardUtils.stringFlavor)) {
                    content = (String) trans.getTransferData(ClipboardUtils.stringFlavor);
                }
            }
            if (content != null) {
                links.addAll(HTMLParser.findUrls(content));
            }
        } catch (final Exception e) {
            org.appwork.loggingv3.LogV3.info(e.getMessage());
        }
        return links;
    }

    public static java.util.List<String> getLinks(final TransferSupport trans) {
        return ClipboardUtils.getLinks(trans.getTransferable());
    }

    public static boolean hasSupport(final DataFlavor flavor) {
        if (flavor != null) {
            if (ClipboardUtils.uriListFlavor != null && flavor.isMimeTypeEqual(ClipboardUtils.uriListFlavor)) {
                return true;
            }
            if (ClipboardUtils.fileListFlavor != null && flavor.isMimeTypeEqual(ClipboardUtils.fileListFlavor)) {
                return true;
            }
            if (ClipboardUtils.arrayListFlavor != null && flavor.isMimeTypeEqual(ClipboardUtils.arrayListFlavor)) {
                return true;
            }
        }
        return false;
    }

    public static boolean hasSupport(final TransferSupport info) {
        if (info != null) {
            for (final DataFlavor flavor : info.getDataFlavors()) {
                if (ClipboardUtils.hasSupport(flavor)) {
                    return true;
                }
            }
        }
        return false;
    }

    private ClipboardUtils() {
    }

    /**
     * put text content to the clipboard
     *
     * @param text
     */
    public static void setTextContent(String text) {
        StringSelection selection = new StringSelection(text);
        Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
        clipboard.setContents(selection, selection);
    }

    /**
     * get text from clipBOARD
     *
     * @return
     */
    public static String getTextContent() {
        String data = null;

        try {
            data = (String) Toolkit.getDefaultToolkit().getSystemClipboard().getData(DataFlavor.stringFlavor);
        } catch (UnsupportedFlavorException e) {

        } catch (IOException e) {

        }

        return data;
    }
}
