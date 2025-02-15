package org.jdownloader.crosssystem.macos;

import java.awt.Image;
import java.lang.reflect.InvocationTargetException;

import org.appwork.utils.JVMVersion;
import org.appwork.utils.ReflectionUtils;
import org.jdownloader.logging.LogController;

public class MacOSApplicationAdapter {
    public static void setDockIcon(final Image icon) {
        try {
            if (JVMVersion.isMinimum(JVMVersion.JAVA_9)) {
                final String className = "java.awt.Taskbar";
                final boolean isTaskbarSupported = ReflectionUtils.invoke(className, "isTaskbarSupported", null, boolean.class);
                if (isTaskbarSupported) {
                    // update to check Taskbar.isSupported(Taskbar.Feature.ICON_IMAGE)
                    final Object taskBar = ReflectionUtils.invoke(className, "getTaskbar", null, Class.forName(className));
                    try {
                        ReflectionUtils.invoke(taskBar.getClass(), "setIconImage", taskBar, void.class, icon);
                    } catch (InvocationTargetException e) {
                        throw e.getTargetException();
                    }
                    return;
                }
            }
            com.apple.eawt.Application.getApplication().setDockIconImage(icon);
        } catch (final UnsupportedOperationException ignore) {
            LogController.CL().log(ignore);
        } catch (final Throwable e) {
            LogController.CL().log(e);
        }
    }
}
