package org.jdownloader.crosssystem.macos;

import java.awt.Image;
import java.lang.reflect.InvocationTargetException;

import org.appwork.loggingv3.LogV3;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.ReflectionUtils;
import org.jdownloader.logging.LogController;

public class MacOSApplicationAdapter {
    public static void setDockIcon(final Image icon) {
        try {
            taskBarJava9: if (JavaVersion.getVersion().isMinimum(JavaVersion.JVM_9_0)) {
                try {
                    final String className = "java.awt.Taskbar";
                    final boolean isTaskbarSupported = ReflectionUtils.invoke(className, "isTaskbarSupported", null, boolean.class);
                    if (!isTaskbarSupported) {
                        break taskBarJava9;
                    }
                    final Object taskBar = ReflectionUtils.invoke(className, "getTaskbar", null, Class.forName(className));
                    icon_image: {
                        final Object FEATURE_ICON_IMAGE = Enum.valueOf((Class<Enum>) Class.forName("java.awt.Taskbar$Feature"), "ICON_IMAGE");
                        final boolean isIconImageSupported = ReflectionUtils.invoke(className, "isSupported", taskBar, boolean.class, FEATURE_ICON_IMAGE);
                        if (!isIconImageSupported) {
                            break icon_image;
                        }
                        try {
                            ReflectionUtils.invoke(taskBar.getClass(), "setIconImage", taskBar, void.class, icon);
                        } catch (InvocationTargetException e) {
                            throw e.getTargetException();
                        }
                        return;
                    }
                } catch (Throwable e) {
                    LogV3.log(e);
                }
            }
            com.apple.eawt.Application.getApplication().setDockIconImage(icon);
        } catch (final Throwable e) {
            LogController.CL().log(e);
        }
    }
}
