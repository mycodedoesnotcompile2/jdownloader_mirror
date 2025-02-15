/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.utils.images;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Locale;

import org.appwork.exceptions.WTFException;
import org.appwork.resources.MultiResolutionImageHelper;
import org.appwork.utils.Application;
import org.appwork.utils.IO;

import com.sun.jna.Memory;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.GDI32;
import com.sun.jna.platform.win32.Shell32;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinDef.HICON;
import com.sun.jna.platform.win32.WinGDI;

/**
 * @author thomas
 * @date Feb 3, 2025
 *
 */
public class JNAImageHelper {
    /**
     * @param resource
     * @return
     * @throws IOException
     */
    public static Image getImageFromExe(URL resource) throws IOException {
        File target = null;
        boolean deleteExe = false;
        try {
            if (!"file".equals(resource.getProtocol().toLowerCase(Locale.ROOT))) {
                target = Application.getTempUniqueResource(".tmpexe");
                IO.writeToFile(target, IO.readURL(resource));
                deleteExe = true;
            } else {
                try {
                    target = new File(resource.toURI());
                } catch (URISyntaxException e) {
                    throw new WTFException(e);
                }
            }
            if (!target.isFile()) {
                return null;
            }
            final int iconIndex = 0;
            int totalIcons = Shell32.INSTANCE.ExtractIconEx(target.getAbsolutePath(), -1, null, null, 0);
            if (totalIcons == 0) {
                return null;
            }
            // Arrays to hold the icons (large and small)
            final HICON[] largeIcons = new HICON[totalIcons];
            final HICON[] smallIcons = new HICON[totalIcons];
            // Extract icons using ExtractIconEx
            final int numExtracted = Shell32.INSTANCE.ExtractIconEx(target.getAbsolutePath(), iconIndex, largeIcons, smallIcons, totalIcons);
            if (numExtracted == 0) {
                return null;
            }
            final ArrayList<Image> images = new ArrayList<Image>();
            for (HICON i : largeIcons) {
                if (i == null) {
                    continue;
                }
                try {
                    final Image largeImage = toImage(i);
                    if (largeImage != null) {
                        images.add(largeImage);
                    }
                } finally {
                    User32.INSTANCE.DestroyIcon(i);
                }
            }
            for (HICON i : smallIcons) {
                if (i == null) {
                    continue;
                }
                try {
                    final Image largeImage = toImage(i);
                    if (largeImage != null) {
                        images.add(largeImage);
                    }
                } finally {
                    User32.INSTANCE.DestroyIcon(i);
                }
            }
            MultiResolutionImageHelper.sortImagesBySize(images);
            if (images.size() == 0) {
                return null;
            } else if (images.size() > 1 && MultiResolutionImageHelper.isSupported()) {
                // use biggest image as base
                return MultiResolutionImageHelper.create(images.get(images.size() - 1), images);
            } else {
                return images.get(images.size() - 1);
            }
        } finally {
            if (deleteExe && target != null) {
                target.delete();
                target.deleteOnExit();
            }
        }
    }

    public static BufferedImage toImage(final WinDef.HICON hicon) {
        WinDef.HBITMAP bitmapHandle = null;
        final User32 user32 = User32.INSTANCE;
        final GDI32 gdi32 = GDI32.INSTANCE;
        try {
            final WinGDI.ICONINFO info = new WinGDI.ICONINFO();
            if (!user32.GetIconInfo(hicon, info)) {
                return null;
            }
            info.read();
            bitmapHandle = (info.hbmColor != null) ? info.hbmColor : info.hbmMask;
            final WinGDI.BITMAP bitmap = new WinGDI.BITMAP();
            if (gdi32.GetObject(bitmapHandle, bitmap.size(), bitmap.getPointer()) > 0) {
                bitmap.read();
                final int width = bitmap.bmWidth.intValue();
                final int height = bitmap.bmHeight.intValue();
                final WinDef.HDC deviceContext = user32.GetDC(null);
                final WinGDI.BITMAPINFO bitmapInfo = new WinGDI.BITMAPINFO();
                bitmapInfo.bmiHeader.biSize = bitmapInfo.bmiHeader.size();
                if (gdi32.GetDIBits(deviceContext, bitmapHandle, 0, 0, Pointer.NULL, bitmapInfo, WinGDI.DIB_RGB_COLORS) == 0) {
                    throw new IllegalArgumentException("GetDIBits should not return 0");
                }
                bitmapInfo.read();
                final Memory pixels = new Memory(bitmapInfo.bmiHeader.biSizeImage);
                bitmapInfo.bmiHeader.biCompression = WinGDI.BI_RGB;
                bitmapInfo.bmiHeader.biHeight = -height;
                if (gdi32.GetDIBits(deviceContext, bitmapHandle, 0, bitmapInfo.bmiHeader.biHeight, pixels, bitmapInfo, WinGDI.DIB_RGB_COLORS) == 0) {
                    throw new IllegalArgumentException("GetDIBits should not return 0");
                }
                final int[] colorArray = pixels.getIntArray(0, width * height);
                final BufferedImage image = IconIO.createEmptyImage(width, height);
                image.setRGB(0, 0, width, height, colorArray, 0, width);
                return image;
            }
        } finally {
            gdi32.DeleteObject(hicon);
            if (bitmapHandle != null) {
                gdi32.DeleteObject(bitmapHandle);
            }
        }
        return null;
    }
}
