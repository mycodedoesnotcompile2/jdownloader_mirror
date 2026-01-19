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
package org.appwork.testframework;

import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.appwork.utils.Hash;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * Performance-optimized ClassCollector with extensive caching. Scans class dependency trees and caches results to avoid redundant scanning.
 *
 * @author thomas
 * @date Jan 18, 2026
 */
public class ClassCollector2 {
    /**
     * Cached class data containing hash and references
     */
    private static class ClassData {
        final String      hash;
        final Set<String> references;

        ClassData(String hash, Set<String> references) {
            this.hash = hash;
            this.references = Collections.unmodifiableSet(references);
        }
    }

    /**
     * Global cache: className -> ClassData (hash + references) Thread-safe for concurrent access
     */
    private static final Map<String, ClassData> classCache  = new ConcurrentHashMap<String, ClassData>();

    /**
     * Cache statistics for monitoring performance
     */
    private static volatile long                cacheHits   = 0;
    private static volatile long                cacheMisses = 0;

    /**
     * Main method: Collects all referenced classes and their hashes as fast as possible.
     *
     * @param className
     *            the starting class name
     * @param collectAll
     *            if true, recursively collect all transitive dependencies
     * @return Map of className -> hash for all collected classes
     * @throws IOException
     *             if class reading fails
     */
    public Map<String, String> getClasses(final String className, final boolean collectAll) throws IOException {
        final Map<String, String> result = new LinkedHashMap<String, String>();
        final Set<String> toProcess = new HashSet<String>();
        final Set<String> processed = new HashSet<String>();

        // Start with initial class
        toProcess.add(className);

        while (!toProcess.isEmpty()) {
            // Get next class to process
            final String clazz = toProcess.iterator().next();
            toProcess.remove(clazz);

            // Skip if already processed or should be skipped
            if (processed.contains(clazz) || skip(clazz)) {
                continue;
            }
            processed.add(clazz);

            // Try to get from cache first
            ClassData cachedData = classCache.get(clazz);
            if (cachedData != null) {
                // Cache hit - use cached data
                cacheHits++;
                result.put(clazz, cachedData.hash);

                // Add references to processing queue if collecting all
                if (collectAll) {
                    for (String ref : cachedData.references) {
                        if (!processed.contains(ref) && !skip(ref)) {
                            toProcess.add(ref);
                        }
                    }
                }
            } else {
                // Cache miss - scan the class
                cacheMisses++;
                try {
                    ClassReader reader = new ClassReader(clazz);
                    String hash = Hash.getBytesHash(reader.b, Hash.HASH_TYPE_SHA256);
                    result.put(clazz, hash);

                    if (collectAll) {
                        // Collect references
                        CachedClassReferenceCollector collector = new CachedClassReferenceCollector();
                        reader.accept(collector, ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES);
                        Set<String> references = collector.getReferencedClasses();

                        // Store in cache
                        classCache.put(clazz, new ClassData(hash, new HashSet<String>(references)));

                        // Add references to processing queue
                        for (String ref : references) {
                            if (!processed.contains(ref) && !skip(ref)) {
                                toProcess.add(ref);
                            }
                        }
                    } else {
                        // Even if not collecting all, cache the direct references for future use
                        CachedClassReferenceCollector collector = new CachedClassReferenceCollector();
                        reader.accept(collector, ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES);
                        classCache.put(clazz, new ClassData(hash, new HashSet<String>(collector.getReferencedClasses())));
                    }
                } catch (IOException e) {
                    // Class not found or not readable - skip it
                    continue;
                }
            }
        }

        return result;
    }

    /**
     * Determines if a class should be skipped (system classes, third-party libs, etc.)
     */
    protected boolean skip(final String clazz) {
        if (clazz.startsWith("com.sun.")) {
            return true;
        } else if (clazz.startsWith("org.bouncycastle.")) {
            return true;
        } else if (clazz.startsWith("de.javasoft.")) {
            return true;
        } else if (clazz.startsWith("org.mozilla.")) {
            return true;
        } else if (clazz.startsWith("sun.")) {
            return true;
        } else if (clazz.startsWith("java.") || clazz.startsWith("javax.") || clazz.startsWith("[")) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Clears the global class cache. Useful for testing or when classes have been recompiled.
     */
    public static void clearCache() {
        classCache.clear();
        cacheHits = 0;
        cacheMisses = 0;
    }

    /**
     * Returns cache statistics
     */
    public static String getCacheStats() {
        long total = cacheHits + cacheMisses;
        double hitRate = total > 0 ? (cacheHits * 100.0 / total) : 0;
        return String.format("Cache: %d entries, Hits: %d, Misses: %d, Hit Rate: %.2f%%", classCache.size(), cacheHits, cacheMisses, hitRate);
    }

    /**
     * Returns the number of cached classes
     */
    public static int getCacheSize() {
        return classCache.size();
    }

    /**
     * Optimized ClassVisitor that collects class references efficiently
     */
    protected static class CachedClassReferenceCollector extends ClassVisitor {
        private final Set<String> referenced = new HashSet<String>();

        public CachedClassReferenceCollector() {
            super(Opcodes.ASM9);
        }

        public Set<String> getReferencedClasses() {
            return referenced;
        }

        private void addType(Type type) {
            if (type.getSort() == Type.OBJECT) {
                referenced.add(type.getClassName());
            } else if (type.getSort() == Type.ARRAY) {
                addType(type.getElementType());
            }
        }

        @Override
        public void visit(int version, int access, String name, String signature, String superName, String[] interfaces) {
            if (superName != null) {
                referenced.add(superName.replace('/', '.'));
            }
            if (interfaces != null) {
                for (String itf : interfaces) {
                    referenced.add(itf.replace('/', '.'));
                }
            }
        }

        @Override
        public FieldVisitor visitField(int access, String name, String desc, String signature, Object value) {
            addType(Type.getType(desc));
            // No need to call super - we don't need FieldVisitor
            return null;
        }

        @Override
        public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
            Type methodType = Type.getMethodType(desc);
            addType(methodType.getReturnType());
            for (Type arg : methodType.getArgumentTypes()) {
                addType(arg);
            }
            if (exceptions != null) {
                for (String ex : exceptions) {
                    referenced.add(ex.replace('/', '.'));
                }
            }
            return new MethodVisitor(Opcodes.ASM9) {
                @Override
                public void visitTypeInsn(int opcode, String type) {
                    referenced.add(type.replace('/', '.'));
                }

                @Override
                public void visitMethodInsn(int opcode, String owner, String name, String desc, boolean itf) {
                    referenced.add(owner.replace('/', '.'));
                    Type mt = Type.getMethodType(desc);
                    addType(mt.getReturnType());
                    for (Type t : mt.getArgumentTypes()) {
                        addType(t);
                    }
                }

                @Override
                public void visitLdcInsn(Object value) {
                    if (value instanceof Type) {
                        addType((Type) value);
                    }
                }

                @Override
                public void visitFieldInsn(int opcode, String owner, String name, String desc) {
                    referenced.add(owner.replace('/', '.'));
                    addType(Type.getType(desc));
                }

                @Override
                public void visitInvokeDynamicInsn(String name, String desc, org.objectweb.asm.Handle bsm, Object... bsmArgs) {
                    Type methodType = Type.getMethodType(desc);
                    addType(methodType.getReturnType());
                    for (Type arg : methodType.getArgumentTypes()) {
                        addType(arg);
                    }
                    // Also scan bootstrap method arguments for types
                    for (Object arg : bsmArgs) {
                        if (arg instanceof Type) {
                            addType((Type) arg);
                        } else if (arg instanceof org.objectweb.asm.Handle) {
                            org.objectweb.asm.Handle handle = (org.objectweb.asm.Handle) arg;
                            referenced.add(handle.getOwner().replace('/', '.'));
                        }
                    }
                }

                @Override
                public void visitTryCatchBlock(org.objectweb.asm.Label start, org.objectweb.asm.Label end, org.objectweb.asm.Label handler, String type) {
                    if (type != null) {
                        referenced.add(type.replace('/', '.'));
                    }
                }

                @Override
                public void visitMultiANewArrayInsn(String desc, int dims) {
                    addType(Type.getType(desc));
                }
            };
        }

        @Override
        public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
            addType(Type.getType(desc));
            // Return null to skip annotation details for performance
            return null;
        }
    }
}
