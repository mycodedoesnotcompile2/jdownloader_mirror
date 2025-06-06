package org.jdownloader.crosssystem.macos;

import java.awt.Desktop;
import java.io.File;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.List;

import javax.swing.JFrame;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.Application;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.JavaVersion;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.logging2.extmanager.LoggerFactory;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.windowmanager.WindowManager;
import org.appwork.utils.swing.windowmanager.WindowManager.FrameState;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;
import org.jdownloader.updatev2.RestartController;
import org.jdownloader.updatev2.SmartRlyExitRequest;

import jd.SecondLevelLaunch;
import jd.controlling.TaskQueue;
import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcollector.LinkOrigin;
import jd.gui.swing.dialog.AboutDialog;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.views.settings.ConfigurationView;

public class AWTMacOSApplicationAdapter {
    public static void main(String[] args) {
        Application.setApplication(".test");
        enableMacSpecial();
    }

    public static void enableMacSpecial() {
        final Desktop desktop = Desktop.getDesktop();
        if (desktop != null && JVMVersion.getVersion().isMinimum(JavaVersion.JVM_9_0)) {
            final AWTMacOSApplicationAdapter adapter = new AWTMacOSApplicationAdapter();
            adapter.setAboutHandler​(desktop);
            adapter.setPreferencesHandler​(desktop);
            adapter.setQuitHandler(desktop);
            adapter.setOpenFileHandler(desktop);
            adapter.setOpenURIHandler​(desktop);
            adapter.addAppReopenedListener(desktop);
            MacOSDockAdapter.init();
        }
    }

    private void setQuitHandler(Desktop desktop) {
        if (desktop != null) {
            try {
                final Method setQuitHandler​ = getMethod(desktop.getClass(), "setQuitHandler");
                final Object quitHandler = java.lang.reflect.Proxy.newProxyInstance(desktop.getClass().getClassLoader(), new Class[] { setQuitHandler​.getParameterTypes()[0] }, new InvocationHandler() {
                    @Override
                    public Object invoke(Object proxy, final Method method, final Object[] args) throws Throwable {
                        System.out.println(method.getName());
                        final Thread thread = new Thread("java.awt.desktop.QuitHandler") {
                            public void run() {
                                System.out.println(method.getName());
                                RestartController.getInstance().exitAsynch(new SmartRlyExitRequest() {
                                    @Override
                                    public void onShutdown() {
                                        new Thread("QuitResponse:performQuit") {
                                            public void run() {
                                                /*
                                                 * own thread because else it will block, performQuit calls exit again
                                                 */
                                                try {
                                                    final Object quitResponse = args[1];
                                                    final Method performQuit = quitResponse.getClass().getMethod("performQuit​", new Class[0]);
                                                    performQuit.invoke(quitResponse, new Object[0]);
                                                } catch (Throwable e) {
                                                    e.printStackTrace();
                                                }
                                            };
                                        }.start();
                                    }

                                    @Override
                                    public void onShutdownVeto() {
                                        new Thread("QuitResponse:cancelQuit") {
                                            public void run() {
                                                /*
                                                 * own thread because else it will block, performQuit calls exit again
                                                 */
                                                try {
                                                    final Object quitResponse = args[1];
                                                    final Method cancelQuit​ = quitResponse.getClass().getMethod("cancelQuit​​", new Class[0]);
                                                    cancelQuit​.invoke(quitResponse, new Object[0]);
                                                } catch (Throwable e) {
                                                    e.printStackTrace();
                                                }
                                            };
                                        }.start();
                                    }
                                });
                            };
                        };
                        thread.setDaemon(true);
                        thread.start();
                        return null;
                    }
                });
                try {
                    setQuitHandler​.invoke(desktop, quitHandler);
                } catch (InvocationTargetException e) {
                    throw e.getTargetException();
                }
            } catch (final UnsupportedOperationException ignore) {
                LoggerFactory.getDefaultLogger().log(ignore);
            } catch (final Throwable e) {
                LoggerFactory.getDefaultLogger().log(e);
            }
        }
    }

    private void setOpenURIHandler​(Desktop desktop) {
        if (desktop != null) {
            try {
                final Method setOpenURIHandler​ = getMethod(desktop.getClass(), "setOpenURIHandler");
                final Object openURIHandler = java.lang.reflect.Proxy.newProxyInstance(desktop.getClass().getClassLoader(), new Class[] { setOpenURIHandler​.getParameterTypes()[0] }, new InvocationHandler() {
                    @Override
                    public Object invoke(Object proxy, final Method method, final Object[] args) throws Throwable {
                        TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
                            @Override
                            protected Void run() throws RuntimeException {
                                try {
                                    appReOpened();
                                    final Object openURIEvent​ = args[0];
                                    final Method getURI​ = openURIEvent​.getClass().getMethod("getURI​", new Class[0]);
                                    final URI uri = (URI) getURI​.invoke(openURIEvent​, new Object[0]);
                                    final String links = uri.toString();
                                    SecondLevelLaunch.GUI_COMPLETE.executeWhenReached(new Runnable() {
                                        @Override
                                        public void run() {
                                            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().info("Distribute links: " + links);
                                            LinkCollector.getInstance().addCrawlerJob(new LinkCollectingJob(LinkOrigin.MAC_DOCK.getLinkOriginDetails(), links));
                                        }
                                    });
                                } catch (final Throwable e) {
                                    LoggerFactory.getDefaultLogger().log(e);
                                }
                                return null;
                            }
                        });
                        return null;
                    }
                });
                try {
                    setOpenURIHandler​.invoke(desktop, openURIHandler);
                } catch (InvocationTargetException e) {
                    throw e.getTargetException();
                }
            } catch (final UnsupportedOperationException ignore) {
                LoggerFactory.getDefaultLogger().log(ignore);
            } catch (final Throwable e) {
                LoggerFactory.getDefaultLogger().log(e);
            }
        }
    }

    private final Method getMethod(final Class clz, final String name) throws NoSuchMethodException {
        if (clz != null && name != null) {
            for (final Method method : clz.getMethods()) {
                if (name.equals(method.getName())) {
                    return method;
                }
            }
        }
        throw new NoSuchMethodException(name);
    }

    private void setOpenFileHandler(Desktop desktop) {
        if (desktop != null) {
            try {
                final Method setOpenFileHandler​ = getMethod(desktop.getClass(), "setOpenFileHandler");
                final Object openFileHandler = java.lang.reflect.Proxy.newProxyInstance(desktop.getClass().getClassLoader(), new Class[] { setOpenFileHandler​.getParameterTypes()[0] }, new InvocationHandler() {
                    @Override
                    public Object invoke(Object proxy, final Method method, final Object[] args) throws Throwable {
                        TaskQueue.getQueue().enqueue(new QueueAction<Void, RuntimeException>() {
                            @Override
                            protected Void run() throws RuntimeException {
                                try {
                                    appReOpened();
                                    final Object openFilesEvent = args[0];
                                    Method getFiles = openFilesEvent.getClass().getMethod("getFiles", new Class[0]);
                                    final List<File> files = (List<File>) getFiles.invoke(openFilesEvent, new Object[0]);
                                    org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().info("Handle open files from Dock " + files.toString());
                                    final StringBuilder sb = new StringBuilder();
                                    for (final File file : files) {
                                        if (sb.length() > 0) {
                                            sb.append("\r\n");
                                        }
                                        sb.append(file.toURI().toString());
                                    }
                                    final String links = sb.toString();
                                    SecondLevelLaunch.GUI_COMPLETE.executeWhenReached(new Runnable() {
                                        @Override
                                        public void run() {
                                            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().info("Distribute links: " + links);
                                            LinkCollector.getInstance().addCrawlerJob(new LinkCollectingJob(LinkOrigin.MAC_DOCK.getLinkOriginDetails(), links));
                                        }
                                    });
                                } catch (final Throwable e) {
                                    LoggerFactory.getDefaultLogger().log(e);
                                }
                                return null;
                            }
                        });
                        return null;
                    }
                });
                try {
                    setOpenFileHandler​.invoke(desktop, openFileHandler);
                } catch (InvocationTargetException e) {
                    throw e.getTargetException();
                }
            } catch (final UnsupportedOperationException ignore) {
                LoggerFactory.getDefaultLogger().log(ignore);
            } catch (final Throwable e) {
                LoggerFactory.getDefaultLogger().log(e);
            }
        }
    }

    private void setPreferencesHandler​(Desktop desktop) {
        if (desktop != null) {
            try {
                final Method setPreferencesHandler = getMethod(desktop.getClass(), "setPreferencesHandler");
                final Object preferencesHandler = java.lang.reflect.Proxy.newProxyInstance(desktop.getClass().getClassLoader(), new Class[] { setPreferencesHandler.getParameterTypes()[0] }, new InvocationHandler() {
                    @Override
                    public Object invoke(Object proxy, final Method method, final Object[] args) throws Throwable {
                        System.out.println(method.getName());
                        TaskQueue.getQueue().addAsynch(new QueueAction<Void, RuntimeException>() {
                            @Override
                            protected boolean allowAsync() {
                                return true;
                            }

                            @Override
                            protected Void run() throws RuntimeException {
                                new EDTRunner() {
                                    @Override
                                    protected void runInEDT() {
                                        try {
                                            appReOpened();
                                            JsonConfig.create(GraphicalUserInterfaceSettings.class).setConfigViewVisible(true);
                                            JDGui.getInstance().setContent(ConfigurationView.getInstance(), true);
                                        } catch (Throwable e) {
                                            LoggerFactory.getDefaultLogger().log(e);
                                        }
                                    }
                                };
                                return null;
                            }
                        });
                        return null;
                    }
                });
                try {
                    setPreferencesHandler.invoke(desktop, preferencesHandler);
                } catch (InvocationTargetException e) {
                    throw e.getTargetException();
                }
            } catch (final UnsupportedOperationException ignore) {
                LoggerFactory.getDefaultLogger().log(ignore);
            } catch (final Throwable e) {
                LoggerFactory.getDefaultLogger().log(e);
            }
        }
    }

    private void appReOpened() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                final JDGui swingGui = JDGui.getInstance();
                if (swingGui == null || swingGui.getMainFrame() == null) {
                    return;
                }
                final JFrame mainFrame = swingGui.getMainFrame();
                if (!mainFrame.isVisible()) {
                    WindowManager.getInstance().setVisible(mainFrame, true, FrameState.OS_DEFAULT);
                }
            }
        };
    }

    private void addAppReopenedListener(Desktop desktop) {
        if (desktop != null) {
            try {
                final Method addAppEventListener = getMethod(desktop.getClass(), "addAppEventListener");
                final Object appEventListener = java.lang.reflect.Proxy.newProxyInstance(desktop.getClass().getClassLoader(), new Class[] { Class.forName("java.awt.desktop.AppReopenedListener") }, new InvocationHandler() {
                    @Override
                    public Object invoke(Object proxy, final Method method, final Object[] args) throws Throwable {
                        if ("appReopened".equals(method.getName())) {
                            appReOpened();
                        }
                        return null;
                    }
                });
                try {
                    addAppEventListener.invoke(desktop, appEventListener);
                } catch (InvocationTargetException e) {
                    throw e.getTargetException();
                }
            } catch (final UnsupportedOperationException ignore) {
                LoggerFactory.getDefaultLogger().log(ignore);
            } catch (final Throwable e) {
                LoggerFactory.getDefaultLogger().log(e);
            }
        }
    }

    private void setAboutHandler​(Desktop desktop) {
        if (desktop != null) {
            try {
                final Method setAboutHandler​ = getMethod(desktop.getClass(), "setAboutHandler");
                final Object aboutHandler = java.lang.reflect.Proxy.newProxyInstance(desktop.getClass().getClassLoader(), new Class[] { setAboutHandler​.getParameterTypes()[0] }, new InvocationHandler() {
                    @Override
                    public Object invoke(Object proxy, final Method method, final Object[] args) throws Throwable {
                        AboutDialog.showNonBlocking();
                        return null;
                    }
                });
                try {
                    setAboutHandler​.invoke(desktop, aboutHandler);
                } catch (InvocationTargetException e) {
                    throw e.getTargetException();
                }
            } catch (final UnsupportedOperationException ignore) {
                LoggerFactory.getDefaultLogger().log(ignore);
            } catch (final Throwable e) {
                LoggerFactory.getDefaultLogger().log(e);
            }
        }
    }
}
