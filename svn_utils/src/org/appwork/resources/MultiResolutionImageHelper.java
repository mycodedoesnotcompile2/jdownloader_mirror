/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.resources;

import java.awt.Image;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.AWTestValidateClassReference;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.reflection.Clazz;

/**
 * @author thomas
 * @date Feb 3, 2025
 *
 */
public class MultiResolutionImageHelper {
    @AWTestValidateClassReference
    public static String                CLAZZ     = "java.awt.image.BaseMultiResolutionImage";
    @AWTestValidateClassReference
    public static String                INTERFACE = "java.awt.image.MultiResolutionImage";
    private final static Constructor<?> CONSTRUCTOR;
    private final static Class<?>       CLS;
    private final static Class<?>       INTFS;
    private final static Method         METHOD_GETRESOLUTION_VARIANTS;
    private final static Method         METHOD_GETRESOLUTION_VARIANT;
    private final static boolean        SUPPORTED;
    static {
        boolean isSupported = false;
        Class<?> cls = null;
        Class<?> intfs = null;
        Constructor<?> cons = null;
        Method variants = null;
        Method variant = null;
        if (JavaVersion.getVersion().isMinimum(JavaVersion.JVM_9_0)) {
            try {
                cls = Class.forName(CLAZZ);
                intfs = Class.forName(INTERFACE);
                cons = cls.getConstructor(int.class, Image[].class);
                variants = cls.getMethod("getResolutionVariants");
                variant = cls.getMethod("getResolutionVariant", double.class, double.class);
                isSupported = true;
            } catch (NoSuchMethodException e) {
                LogV3.log(e);
            } catch (SecurityException e) {
                LogV3.log(e);
            } catch (IllegalArgumentException e) {
                LogV3.log(e);
            } catch (ClassNotFoundException e) {
                LogV3.log(e);
            }
        }
        SUPPORTED = isSupported;
        CLS = cls;
        INTFS = intfs;
        CONSTRUCTOR = cons;
        METHOD_GETRESOLUTION_VARIANTS = variants;
        METHOD_GETRESOLUTION_VARIANT = variant;
    }

    /**
     * @param variants
     */
    public static void sortImagesBySize(List<Image> variants) {
        Collections.sort(variants, new Comparator<Image>() {
            @Override
            public int compare(Image o1, Image o2) {
                return CompareUtils.compareLong((long) o1.getWidth(null) * o1.getHeight(null), (long) o2.getWidth(null) * o2.getHeight(null));
            }
        });
    }

    /**
     * @param i
     * @param baseImageHighDPIFinal
     * @param baseImageTargetFinal
     * @return
     */
    public static Image create(int i, Image... images) {
        Image base = images[i];
        List<Image> list = Arrays.asList(images);
        try {
            sortImagesBySize(list);
            return (Image) CONSTRUCTOR.newInstance(list.indexOf(base), list.toArray(new Image[0]));
        } catch (SecurityException e) {
            LogV3.log(e);
            throw new WTFException(e);
        } catch (InstantiationException e) {
            LogV3.log(e);
            throw new WTFException(e);
        } catch (IllegalAccessException e) {
            LogV3.log(e);
            throw new WTFException(e);
        } catch (IllegalArgumentException e) {
            LogV3.log(e);
            throw new WTFException(e);
        } catch (InvocationTargetException e) {
            LogV3.log(e);
            throw new WTFException(e);
        }
    }

    /**
     * @param img
     * @return
     */
    public static boolean isInstanceOf(Image img) {
        return Clazz.isInstanceof(img.getClass(), INTFS);
    }

    /**
     * @param input
     * @return
     */
    public static List<Image> getResolutionVariants(Image input) {
        try {
            return (List<Image>) METHOD_GETRESOLUTION_VARIANTS.invoke(input, new Object[0]);
        } catch (IllegalAccessException e) {
            LogV3.log(e);
            throw new WTFException();
        } catch (IllegalArgumentException e) {
            LogV3.log(e);
            throw new WTFException();
        } catch (InvocationTargetException e) {
            LogV3.log(e);
            throw new WTFException();
        }
    }

    /**
     * @return
     */
    public static boolean isSupported() {
        return SUPPORTED;
    }

    /**
     * @param images
     * @return
     */
    public static Image create(List<Image> images) {
        // smallest one as base image
        sortImagesBySize(images);
        return create(0, images.toArray(new Image[0]));
    }

    /**
     * @param base
     * @param images
     * @return
     */
    public static Image create(Image base, List<Image> images) {
        try {
            List<Image> list = new ArrayList<Image>(images);
            if (!list.contains(base)) {
                list.add(base);
            }
            sortImagesBySize(list);
            return (Image) CONSTRUCTOR.newInstance(list.indexOf(base), list.toArray(new Image[0]));
        } catch (SecurityException e) {
            LogV3.log(e);
            throw new WTFException(e);
        } catch (InstantiationException e) {
            LogV3.log(e);
            throw new WTFException(e);
        } catch (IllegalAccessException e) {
            LogV3.log(e);
            throw new WTFException(e);
        } catch (IllegalArgumentException e) {
            LogV3.log(e);
            throw new WTFException(e);
        } catch (InvocationTargetException e) {
            LogV3.log(e);
            throw new WTFException(e);
        }
    }
}
