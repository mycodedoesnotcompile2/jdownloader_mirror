package org.appwork.utils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.security.ProtectionDomain;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.concurrent.atomic.AtomicReference;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;

public class InMemoryJarClassLoader extends URLClassLoader {
    protected final byte[]          jarFileBytes;
    protected final HashSet<String> entries = new HashSet<String>();

    public InMemoryJarClassLoader(byte[] jarFileBytes) throws IOException {
        super(new URL[0], InMemoryJarClassLoader.class.getClassLoader());
        this.jarFileBytes = jarFileBytes;
        final JarInputStream is = new JarInputStream(new ByteArrayInputStream(this.jarFileBytes));
        try {
            JarEntry entry = null;
            while ((entry = is.getNextJarEntry()) != null) {
                this.entries.add(entry.getName());
            }
        } finally {
            is.close();
        }
    }

    private InputStream getInputStream(final String entryName) throws IOException {
        if (this.entries.contains(entryName)) {
            final JarInputStream is = new JarInputStream(new ByteArrayInputStream(this.jarFileBytes));
            boolean closeFlag = true;
            try {
                JarEntry entry = null;
                while ((entry = is.getNextJarEntry()) != null) {
                    if (entry.getName().equals(entryName)) {
                        closeFlag = false;
                        return is;
                    }
                }
                return null;
            } finally {
                if (closeFlag) {
                    is.close();
                }
            }
        } else {
            return null;
        }
    }

    protected String getClassRessourceName(final String className) {
        return className.replace(".", "/") + ".class";
    }

    @Override
    public Class<?> findClass(final String className) throws ClassNotFoundException {
        final Class<?> clazz = this.findLoadedClass(className);
        if (clazz != null) {
            return clazz;
        } else {
            final String jarClassName = getClassRessourceName(className);
            if (!this.entries.contains(jarClassName)) {
                try {
                    return super.findClass(className);
                } catch (ClassNotFoundException e) {
                    throw e;
                }
            } else {
                try {
                    final InputStream is = this.getInputStream(jarClassName);
                    if (is != null) {
                        try {
                            final ByteArrayOutputStream bos = new ByteArrayOutputStream();
                            final byte[] buffer = new byte[32767];
                            int read = 0;
                            while ((read = is.read(buffer)) != -1) {
                                if (read > 0) {
                                    bos.write(buffer, 0, read);
                                }
                            }
                            byte[] classByte = bos.toByteArray();
                            this.definePackage(className);
                            return this.defineClass(className, classByte, 0, classByte.length, (ProtectionDomain) null);
                        } finally {
                            try {
                                is.close();
                            } catch (final Throwable e) {
                            }
                        }
                    }
                    throw new ClassNotFoundException("could not find:" + className);
                } catch (IOException e) {
                    throw new ClassNotFoundException("could not find:" + className, e);
                }
            }
        }
    }

    @Override
    public InputStream getResourceAsStream(String name) {
        if (this.entries.contains(name)) {
            try {
                return this.getInputStream(name);
            } catch (IOException e) {
                return null;
            }
        } else {
            return super.getResourceAsStream(name);
        }
    }

    private void definePackage(String className) {
        final int i = className.lastIndexOf('.');
        if (i != -1) {
            String pkgname = className.substring(0, i);
            // Check if package already loaded.
            if (this.getPackage(pkgname) == null) {
                this.definePackage(pkgname, null, null, null, null, null, null, null);
            }
        }
    }

    @Override
    public Enumeration<URL> findResources(String name) throws IOException {
        if (!this.entries.contains(name)) {
            return super.findResources(name);
        } else {
            final URL url = this.findResource(name);
            final AtomicReference<URL> elem = new AtomicReference<URL>(url);
            return new Enumeration<URL>() {
                public boolean hasMoreElements() {
                    return elem.get() != null;
                }

                public URL nextElement() {
                    return elem.getAndSet(null);
                }
            };
        }
    }

    @Override
    public URL findResource(final String name) {
        if (!this.entries.contains(name)) {
            return super.findResource(name);
        } else {
            try {
                final JarInputStream is = new JarInputStream(new ByteArrayInputStream(this.jarFileBytes));
                try {
                    JarEntry entry = null;
                    while ((entry = is.getNextJarEntry()) != null) {
                        if (entry.getName().equals(name)) {
                            return new URL(null, "jarInMemory:/" + name, new URLStreamHandler() {
                                @Override
                                protected URLConnection openConnection(URL u) throws IOException {
                                    return new URLConnection(u) {
                                        @Override
                                        public void connect() throws IOException {
                                        }

                                        @Override
                                        public InputStream getInputStream() throws IOException {
                                            return InMemoryJarClassLoader.this.getInputStream(name);
                                        }
                                    };
                                }
                            });
                        }
                    }
                    return null;
                } finally {
                    try {
                        is.close();
                    } catch (final Throwable e) {
                    }
                }
            } catch (IOException e) {
                return null;
            }
        }
    }

    @Override
    protected Class<?> loadClass(String className, boolean resolve) throws ClassNotFoundException {
        final String jarClassName = getClassRessourceName(className);
        if (!this.entries.contains(jarClassName)) {
            return super.loadClass(className, resolve);
        } else {
            Object lock = this;
            try {
                lock = this.getClassLoadingLock(className);
            } catch (final Throwable e) {
            }
            synchronized (lock) {
                Class<?> clazz = this.findLoadedClass(className);
                if (clazz == null) {
                    clazz = this.findClass(className);
                }
                if (resolve) {
                    this.resolveClass(clazz);
                }
                return clazz;
            }
        }
    }
}
