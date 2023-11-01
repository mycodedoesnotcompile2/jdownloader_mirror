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

import java.lang.reflect.Field;
import java.net.URL;
import java.net.URLStreamHandler;
import java.net.URLStreamHandlerFactory;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * @author daniel
 *
 */
public class JarHandlerWorkaroundOracle {
    private static final AtomicBoolean INIT = new AtomicBoolean(false);

    // add custom jarHandler for http://bugs.java.com/view_bug.do?bug_id=6390779
    public static void init() {
        if (INIT.compareAndSet(false, true)) {
            try {
                final Class<java.net.URLStreamHandler> oracleWorkaroundJarHandler = (Class<URLStreamHandler>) Class.forName("org.appwork.utils.OracleWorkaroundJarHandler");
                {
                    URL.setURLStreamHandlerFactory(new URLStreamHandlerFactory() {
                        @Override
                        public URLStreamHandler createURLStreamHandler(String protocol) {
                            try {
                                if ("jar".equals(protocol)) {
                                    return oracleWorkaroundJarHandler.newInstance();
                                }
                            } catch (Throwable e) {
                                e.printStackTrace();
                            }
                            return null;
                        }
                    });
                    System.out.println("JarHandlerWorkaroundOracle:setURLStreamHandlerFactory");
                }
                {
                    try {
                        final Field field = ReflectionUtils.getField("sun.misc.Launcher", "factory", null, URLStreamHandlerFactory.class);
                        final URLStreamHandlerFactory originalFactory = (URLStreamHandlerFactory) field.get(null);
                        final URLStreamHandlerFactory workAroundFactory = new URLStreamHandlerFactory() {
                            @Override
                            public URLStreamHandler createURLStreamHandler(final String protocol) {
                                try {
                                    if ("jar".equals(protocol)) {
                                        return oracleWorkaroundJarHandler.newInstance();
                                    }
                                } catch (final Throwable e) {
                                    e.printStackTrace();
                                }
                                return originalFactory.createURLStreamHandler(protocol);
                            }
                        };
                        field.set(null, workAroundFactory);
                        System.out.println("JarHandlerWorkaroundOracle:replaceLauncherFactory");
                    } catch (final NoClassDefFoundError ignore) {
                    }
                }
                {
                    Object urlClassPath = null;
                    final ClassLoader cl = JarHandlerWorkaroundOracle.class.getClassLoader();
                    try {
                        final Field ucp = cl.getClass().getDeclaredField("ucp");
                        ucp.setAccessible(true);
                        urlClassPath = ucp.get(cl);
                    } catch (final NoSuchFieldException e) {
                        try {
                            final Field ucp = cl.getClass().getSuperclass().getDeclaredField("ucp");
                            ucp.setAccessible(true);
                            urlClassPath = ucp.get(cl);
                        } catch (final NoSuchFieldException e2) {
                            try {
                                final Field bcp = ReflectionUtils.getField("sun.misc.Launcher", "bcp", null, null);
                                urlClassPath = bcp.get(null);
                            } catch (final NoSuchFieldException e3) {
                            }
                        }
                    }
                    if (urlClassPath != null) {
                        System.out.println("JarHandlerWorkaroundOracle:replaceURLClassPath");
                        final Field jarHandler = urlClassPath.getClass().getDeclaredField("jarHandler");
                        jarHandler.setAccessible(true);
                        jarHandler.set(urlClassPath, oracleWorkaroundJarHandler.newInstance());
                        System.out.println("JarHandlerWorkaroundOracle:replacejarHandler");
                        final Field loadersField = urlClassPath.getClass().getDeclaredField("loaders");
                        loadersField.setAccessible(true);
                        final List<Object> loaders = (List<Object>) loadersField.get(urlClassPath);
                        System.out.println("JarHandlerWorkaroundOracle:replaceLoaders:" + loaders.size());
                        for (int index = 0; index < loaders.size(); index++) {
                            try {
                                final Object loader = loaders.get(index);
                                if (loader.getClass().getName().endsWith("JarLoader")) {
                                    Field handlerField = loader.getClass().getDeclaredField("handler");
                                    handlerField.setAccessible(true);
                                    handlerField.set(loader, oracleWorkaroundJarHandler.newInstance());
                                    System.out.println("JarHandlerWorkaroundOracle:replaceLoader:" + index + ":handler");
                                    final Field baseField = loader.getClass().getSuperclass().getDeclaredField("base");
                                    baseField.setAccessible(true);
                                    final URL base = (URL) baseField.get(loader);
                                    handlerField = base.getClass().getDeclaredField("handler");
                                    handlerField.setAccessible(true);
                                    handlerField.set(base, oracleWorkaroundJarHandler.newInstance());
                                    System.out.println("JarHandlerWorkaroundOracle:replaceLoader:" + index + ":handler:" + base);
                                }
                            } catch (final Throwable ignore) {
                                ignore.printStackTrace();
                            }
                        }
                    }
                }
            } catch (final Throwable e) {
                e.printStackTrace();
            }
        }
    }
}
