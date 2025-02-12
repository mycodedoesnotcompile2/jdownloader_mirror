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

/**
 * @author thomas
 * @date Feb 4, 2025
 *
 */
import java.awt.image.BufferedImage;

public class ImageCropper {
    /**
     * Crops the given image by removing transparent borders.
     *
     * @param img
     *            The source image to crop.
     * @return A new BufferedImage without transparent borders.
     */
    public BufferedImage cropTransparentBorder(BufferedImage img) {
        int left = findLeftBoundary(img);
        int right = findRightBoundary(img);
        int top = findTopBoundary(img);
        int bottom = findBottomBoundary(img);
        // If no non-transparent pixel is found, return the original image.
        if (left < 0 || right < 0 || top < 0 || bottom < 0) {
            return img;
        }
        int newWidth = right - left + 1;
        int newHeight = bottom - top + 1;
        return img.getSubimage(left, top, newWidth, newHeight);
    }

    /**
     * Finds the left boundary of non-transparent pixels.
     *
     * @param img
     *            The source image.
     * @return The x-coordinate of the left boundary.
     */
    private int findLeftBoundary(BufferedImage img) {
        int width = img.getWidth();
        int height = img.getHeight();
        int midY = height / 2;
        int candidate = -1;
        // Heuristic: scan the middle row from left to right.
        for (int x = 0; x < width; x++) {
            if (!isTransparent(img.getRGB(x, midY))) {
                candidate = x;
                break;
            }
        }
        if (candidate == -1) {
            return -1; // No non-transparent pixel found.
        }
        // Check if the column before the candidate is entirely transparent.
        if (candidate > 0) {
            boolean colBeforeTransparent = true;
            for (int y = 0; y < height; y++) {
                if (!isTransparent(img.getRGB(candidate - 1, y))) {
                    colBeforeTransparent = false;
                    break;
                }
            }
            if (colBeforeTransparent) {
                return candidate;
            } else {
                // Fallback: search all columns from the left up to the candidate.
                for (int x = 0; x < candidate; x++) {
                    for (int y = 0; y < height; y++) {
                        if (!isTransparent(img.getRGB(x, y))) {
                            return x;
                        }
                    }
                }
            }
        }
        return candidate;
    }

    /**
     * Finds the right boundary of non-transparent pixels.
     *
     * @param img
     *            The source image.
     * @return The x-coordinate of the right boundary.
     */
    private int findRightBoundary(BufferedImage img) {
        int width = img.getWidth();
        int height = img.getHeight();
        int midY = height / 2;
        int candidate = -1;
        // Heuristic: scan the middle row from right to left.
        for (int x = width - 1; x >= 0; x--) {
            if (!isTransparent(img.getRGB(x, midY))) {
                candidate = x;
                break;
            }
        }
        if (candidate == -1) {
            return -1; // No non-transparent pixel found.
        }
        // Check if the column after the candidate is entirely transparent.
        if (candidate < width - 1) {
            boolean colAfterTransparent = true;
            for (int y = 0; y < height; y++) {
                if (!isTransparent(img.getRGB(candidate + 1, y))) {
                    colAfterTransparent = false;
                    break;
                }
            }
            if (colAfterTransparent) {
                return candidate;
            } else {
                // Fallback: search all columns from the right up to the candidate.
                for (int x = width - 1; x > candidate; x--) {
                    for (int y = 0; y < height; y++) {
                        if (!isTransparent(img.getRGB(x, y))) {
                            return x;
                        }
                    }
                }
            }
        }
        return candidate;
    }

    /**
     * Finds the top boundary of non-transparent pixels.
     *
     * @param img
     *            The source image.
     * @return The y-coordinate of the top boundary.
     */
    private int findTopBoundary(BufferedImage img) {
        int width = img.getWidth();
        int height = img.getHeight();
        int midX = width / 2;
        int candidate = -1;
        // Heuristic: scan the middle column from top to bottom.
        for (int y = 0; y < height; y++) {
            if (!isTransparent(img.getRGB(midX, y))) {
                candidate = y;
                break;
            }
        }
        if (candidate == -1) {
            return -1; // No non-transparent pixel found.
        }
        // Check if the row before the candidate is entirely transparent.
        if (candidate > 0) {
            boolean rowBeforeTransparent = true;
            for (int x = 0; x < width; x++) {
                if (!isTransparent(img.getRGB(x, candidate - 1))) {
                    rowBeforeTransparent = false;
                    break;
                }
            }
            if (rowBeforeTransparent) {
                return candidate;
            } else {
                // Fallback: search all rows from the top up to the candidate.
                for (int y = 0; y < candidate; y++) {
                    for (int x = 0; x < width; x++) {
                        if (!isTransparent(img.getRGB(x, y))) {
                            return y;
                        }
                    }
                }
            }
        }
        return candidate;
    }

    /**
     * Finds the bottom boundary of non-transparent pixels.
     *
     * @param img
     *            The source image.
     * @return The y-coordinate of the bottom boundary.
     */
    private int findBottomBoundary(BufferedImage img) {
        int width = img.getWidth();
        int height = img.getHeight();
        int midX = width / 2;
        int candidate = -1;
        // Heuristic: scan the middle column from bottom to top.
        for (int y = height - 1; y >= 0; y--) {
            if (!isTransparent(img.getRGB(midX, y))) {
                candidate = y;
                break;
            }
        }
        if (candidate == -1) {
            return -1; // No non-transparent pixel found.
        }
        // Check if the row after the candidate is entirely transparent.
        if (candidate < height - 1) {
            boolean rowAfterTransparent = true;
            for (int x = 0; x < width; x++) {
                if (!isTransparent(img.getRGB(x, candidate + 1))) {
                    rowAfterTransparent = false;
                    break;
                }
            }
            if (rowAfterTransparent) {
                return candidate;
            } else {
                // Fallback: search all rows from the bottom up to the candidate.
                for (int y = height - 1; y > candidate; y--) {
                    for (int x = 0; x < width; x++) {
                        if (!isTransparent(img.getRGB(x, y))) {
                            return y;
                        }
                    }
                }
            }
        }
        return candidate;
    }

    /**
     * Checks whether an ARGB pixel is fully transparent.
     *
     * @param argb
     *            The pixel value (in the format 0xAARRGGBB).
     * @return true if the alpha value is 0, false otherwise.
     */
    private boolean isTransparent(int argb) {
        return ((argb >> 24) & 0xff) == 0;
    }
}
