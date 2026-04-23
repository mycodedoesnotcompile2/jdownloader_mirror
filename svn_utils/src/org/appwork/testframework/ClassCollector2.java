/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
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
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

import org.appwork.utils.Hash;
import org.appwork.utils.StringUtils;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.TypePath;
import org.objectweb.asm.signature.SignatureReader;
import org.objectweb.asm.signature.SignatureVisitor;

/**
 * Performance-optimized ClassCollector with extensive caching. Scans class dependency trees and caches results to avoid redundant scanning.
 *
 * @author thomas
 * @date Jan 18, 2026
 */
public class ClassCollector2 {
    private static final String DEBUG_DEPENDENCY_PROPERTY      = "build.debug.dependency";
    private static final String DEBUG_DEPENDENCY_PROPERTY_ALT  = "build.debug.dependency.target";
    private static final String DEBUG_DEPENDENCY_DEFAULTS      = "org.appwork.updateproviderng.exchange.api2.clientInterfaces.storables.ActiveSessionEntry";
    private final boolean skipThirdPartyLibraries;

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
    private static final AtomicLong             cacheHits   = new AtomicLong(0);
    private static final AtomicLong             cacheMisses = new AtomicLong(0);

    public ClassCollector2() {
        this(true);
    }

    public ClassCollector2(final boolean skipThirdPartyLibraries) {
        this.skipThirdPartyLibraries = skipThirdPartyLibraries;
    }

    private static Set<String> getDebugDependencyTargets() {
        return java.util.Collections.emptySet();
    }

    private static String firstNotEmpty(final String first, final String second) {
        if (StringUtils.isNotEmpty(first)) {
            return first;
        }
        return second;
    }

    private static boolean matchesDebugTarget(final String className, final String target) {
        return matchesDebugTargetStrict(className, target);
    }

    private static boolean matchesDebugTargetStrict(final String className, final String target) {
        if (StringUtils.isEmpty(className) || StringUtils.isEmpty(target)) {
            return false;
        }
        if (className.equals(target)) {
            return true;
        }
        if (target.indexOf('.') < 0) {
            return className.endsWith("." + target) || className.endsWith("$" + target);
        }
        if (target.endsWith(".*")) {
            final String prefix = target.substring(0, target.length() - 2);
            return className.equals(prefix) || className.startsWith(prefix + ".");
        }
        return false;
    }

    private static String findMatchingTarget(final Set<String> refs, final Set<String> targets) {
        if (refs == null || refs.isEmpty() || targets == null || targets.isEmpty()) {
            return null;
        }
        for (final String ref : refs) {
            for (final String target : targets) {
                if (matchesDebugTargetStrict(ref, target)) {
                    return ref;
                }
            }
        }
        return null;
    }

    private static void logDebugReferences(final String phase, final String owner, final Set<String> references) {
        return;
    }

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
        long steps = 0;

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
            steps++;

            // Try to get from cache first
            ClassData cachedData = classCache.get(clazz);
            if (cachedData != null) {
                // Cache hit - use cached data
                cacheHits.incrementAndGet();
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
                cacheMisses.incrementAndGet();
                try {
                    ClassReader reader = new ClassReader(clazz);
                    String hash = Hash.getBytesHash(reader.b, Hash.HASH_TYPE_SHA256);
                    result.put(clazz, hash);

                    if (collectAll) {
                        // Collect references
                        CachedClassReferenceCollector collector = new CachedClassReferenceCollector();
                        reader.accept(collector, ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES);
                        Set<String> references = collector.getReferencedClasses();
                        final Set<String> debugTargets = getDebugDependencyTargets();
                        final String debugHit = findMatchingTarget(references, debugTargets);
                        if (debugHit != null) {
                            org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG scan owner=" + clazz + " hit=" + debugHit + ", refs=" + references.size());
                        }

                        // Store in cache
                        classCache.put(clazz, new ClassData(hash, new HashSet<String>(references)));
                        logDebugReferences("collectAll", clazz, references);

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
     * Returns the direct references of a class without recursively traversing its dependency tree.
     *
     * @param className
     *            the class to inspect
     * @return direct class references
     * @throws IOException
     *             if class reading fails
     */
    public Set<String> getReferencedClasses(final String className) throws IOException {
        if (className == null || className.length() == 0) {
            return Collections.emptySet();
        }
        ClassData cachedData = classCache.get(className);
        if (cachedData != null) {
            cacheHits.incrementAndGet();
            return cachedData.references;
        }
        cacheMisses.incrementAndGet();
        final ClassReader reader = new ClassReader(className);
        final String hash = Hash.getBytesHash(reader.b, Hash.HASH_TYPE_SHA256);
        final CachedClassReferenceCollector collector = new CachedClassReferenceCollector();
        reader.accept(collector, ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES);
        final Set<String> references = collector.getReferencedClasses();
        logDebugReferences("direct", className, references);
        final String debugHit = findMatchingTarget(references, getDebugDependencyTargets());
        if (debugHit != null) {
            org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG direct owner=" + className + " hit=" + debugHit + ", refs=" + references.size());
        }
        final ClassData data = new ClassData(hash, new HashSet<String>(references));
        classCache.put(className, data);
        return data.references;
    }

    /**
     * Determines if a class should be skipped (system classes, third-party libs, etc.)
     */
    protected boolean skip(final String clazz) {
        if (!skipThirdPartyLibraries) {
            if (clazz.startsWith("java.") || clazz.startsWith("jdk.") || clazz.startsWith("sun.") || (clazz.startsWith("com.sun.") && !clazz.startsWith("com.sun.jna.")) || clazz.startsWith("javax.xml.") || clazz.startsWith("org.w3c.") || clazz.startsWith("org.xml.") || clazz.startsWith("[")) {
                return true;
            }
            return false;
        }
        if (clazz.startsWith("com.sun.") && !clazz.startsWith("com.sun.jna.")) {
            return true;
        } else if (clazz.startsWith("com.install4j.")) {
            return true;
        } else if (clazz.startsWith("de.javasoft.")) {
            return true;
        } else if (clazz.startsWith("org.mozilla.")) {
            return true;
        } else if (clazz.startsWith("javafx.")) {
            return true;
        } else if (clazz.startsWith("sun.")) {
            return true;
        } else if (clazz.startsWith("java.")) {
            return true;
        } else if (clazz.startsWith("javax.mail.") || clazz.startsWith("javax.activation.")) {
            return false;
        } else if (clazz.startsWith("javax.") || clazz.startsWith("[")) {
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
        cacheHits.set(0);
        cacheMisses.set(0);
    }

    /**
     * Returns cache statistics
     */
    public static String getCacheStats() {
        final long hits = cacheHits.get();
        final long misses = cacheMisses.get();
        final long total = hits + misses;
        final double hitRate = total > 0 ? (hits * 100.0 / total) : 0;
        return String.format("Cache: %d entries, Hits: %d, Misses: %d, Hit Rate: %.2f%%", classCache.size(), hits, misses, hitRate);
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
        private boolean           debugOwner;
        private String            debugOwnerName;

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

        private void addSignature(final String signature, final boolean typeOnly) {
            if (StringUtils.isEmpty(signature)) {
                return;
            }
            try {
                final SignatureReader reader = new SignatureReader(signature);
                if (typeOnly) {
                    reader.acceptType(new ReferenceSignatureVisitor());
                } else {
                    reader.accept(new ReferenceSignatureVisitor());
                }
            } catch (final Throwable ignore) {
                // Keep the bytecode scan resilient; signatures are only an enrichment layer.
            }
        }

        private class ReferenceSignatureVisitor extends SignatureVisitor {
            private String currentClassName;

            ReferenceSignatureVisitor() {
                super(Opcodes.ASM9);
            }

            @Override
            public void visitClassType(final String name) {
                currentClassName = name.replace('/', '.');
                referenced.add(currentClassName);
            }

            @Override
            public void visitInnerClassType(final String name) {
                if (StringUtils.isEmpty(currentClassName)) {
                    currentClassName = name.replace('/', '.');
                } else {
                    currentClassName = currentClassName + "$" + name;
                }
                referenced.add(currentClassName);
            }

            @Override
            public SignatureVisitor visitArrayType() {
                return this;
            }

            @Override
            public void visitTypeArgument() {
            }

            @Override
            public SignatureVisitor visitTypeArgument(final char wildcard) {
                return this;
            }

            @Override
            public void visitEnd() {
                currentClassName = null;
            }
        }

        @Override
        public void visit(int version, int access, String name, String signature, String superName, String[] interfaces) {
            debugOwnerName = name != null ? name.replace('/', '.') : null;
            debugOwner = isDebugTargetClass(debugOwnerName);
            if (debugOwner) {
                org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target owner=" + debugOwnerName + ", version=" + version + ", access=" + access + ", signature=" + signature + ", super=" + (superName == null ? "-" : superName.replace('/', '.')) + ", interfaces=" + Arrays.toString(interfaces));
            }
            addSignature(signature, false);
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
            final String fieldName = name;
            if (debugOwner) {
                org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target field owner=" + debugOwnerName + ", field=" + fieldName + ", desc=" + desc + ", signature=" + signature + ", value=" + value);
            }
            addType(Type.getType(desc));
            addSignature(signature, true);
            return new FieldVisitor(Opcodes.ASM9) {
                @Override
                public AnnotationVisitor visitAnnotation(String annotationDesc, boolean visible) {
                    if (debugOwner) {
                        org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target field-annotation owner=" + debugOwnerName + ", field=" + fieldName + ", annotation=" + annotationDesc + ", visible=" + visible);
                    }
                    addType(Type.getType(annotationDesc));
                    return createAnnotationVisitor();
                }

                @Override
                public AnnotationVisitor visitTypeAnnotation(int typeRef, TypePath typePath, String annotationDesc, boolean visible) {
                    if (debugOwner) {
                        org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target field-type-annotation owner=" + debugOwnerName + ", field=" + fieldName + ", annotation=" + annotationDesc + ", visible=" + visible);
                    }
                    addType(Type.getType(annotationDesc));
                    return createAnnotationVisitor();
                }
            };
        }

        @Override
        public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
            final String methodName = name;
            if (debugOwner) {
                org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target method owner=" + debugOwnerName + ", method=" + methodName + ", desc=" + desc + ", signature=" + signature + ", exceptions=" + Arrays.toString(exceptions));
            }
            Type methodType = Type.getMethodType(desc);
            addType(methodType.getReturnType());
            for (Type arg : methodType.getArgumentTypes()) {
                addType(arg);
            }
            addSignature(signature, false);
            if (exceptions != null) {
                for (String ex : exceptions) {
                    referenced.add(ex.replace('/', '.'));
                }
            }
            return new MethodVisitor(Opcodes.ASM9) {
                @Override
                public void visitTypeInsn(int opcode, String type) {
                    if (debugOwner) {
                        org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target type-insn owner=" + debugOwnerName + ", method=" + methodName + ", type=" + type + ", opcode=" + opcode);
                    }
                    referenced.add(type.replace('/', '.'));
                }

                @Override
                public void visitMethodInsn(int opcode, String owner, String name, String desc, boolean itf) {
                    if (debugOwner) {
                        org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target method-insn owner=" + debugOwnerName + ", method=" + methodName + ", callOwner=" + owner + ", callDesc=" + desc + ", opcode=" + opcode + ", itf=" + itf);
                    }
                    referenced.add(owner.replace('/', '.'));
                    Type mt = Type.getMethodType(desc);
                    addType(mt.getReturnType());
                    for (Type t : mt.getArgumentTypes()) {
                        addType(t);
                    }
                }

                @Override
                public void visitLdcInsn(Object value) {
                    if (debugOwner) {
                        org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target ldc owner=" + debugOwnerName + ", method=" + methodName + ", value=" + value);
                    }
                    if (value instanceof Type) {
                        addType((Type) value);
                    }
                }

                @Override
                public void visitFieldInsn(int opcode, String owner, String name, String desc) {
                    if (debugOwner) {
                        org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target field-insn owner=" + debugOwnerName + ", method=" + methodName + ", fieldOwner=" + owner + ", fieldDesc=" + desc + ", opcode=" + opcode);
                    }
                    referenced.add(owner.replace('/', '.'));
                    addType(Type.getType(desc));
                }

                @Override
                public AnnotationVisitor visitAnnotation(String annotationDesc, boolean visible) {
                    if (debugOwner) {
                        org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target method-annotation owner=" + debugOwnerName + ", method=" + methodName + ", annotation=" + annotationDesc + ", visible=" + visible);
                    }
                    addType(Type.getType(annotationDesc));
                    return createAnnotationVisitor();
                }

                @Override
                public AnnotationVisitor visitTypeAnnotation(int typeRef, TypePath typePath, String annotationDesc, boolean visible) {
                    if (debugOwner) {
                        org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target method-type-annotation owner=" + debugOwnerName + ", method=" + methodName + ", annotation=" + annotationDesc + ", visible=" + visible);
                    }
                    addType(Type.getType(annotationDesc));
                    return createAnnotationVisitor();
                }

                @Override
                public AnnotationVisitor visitParameterAnnotation(int parameter, String annotationDesc, boolean visible) {
                    if (debugOwner) {
                        org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target parameter-annotation owner=" + debugOwnerName + ", method=" + methodName + ", parameter=" + parameter + ", annotation=" + annotationDesc + ", visible=" + visible);
                    }
                    addType(Type.getType(annotationDesc));
                    return createAnnotationVisitor();
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
                    if (debugOwner) {
                        org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target try-catch owner=" + debugOwnerName + ", method=" + methodName + ", type=" + type);
                    }
                    if (type != null) {
                        referenced.add(type.replace('/', '.'));
                    }
                }

                @Override
                public void visitMultiANewArrayInsn(String desc, int dims) {
                    if (debugOwner) {
                        org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target multi-anew-array owner=" + debugOwnerName + ", method=" + methodName + ", desc=" + desc + ", dims=" + dims);
                    }
                    addType(Type.getType(desc));
                }
            };
        }

        @Override
        public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
            if (debugOwner) {
                org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target class-annotation owner=" + debugOwnerName + ", annotation=" + desc + ", visible=" + visible);
            }
            addType(Type.getType(desc));
            return createAnnotationVisitor();
        }

        private static boolean isDebugTargetClass(final String className) {
            return false;
    }

        private AnnotationVisitor createAnnotationVisitor() {
            return new AnnotationVisitor(Opcodes.ASM9) {
                @Override
                public void visit(String name, Object value) {
                    if (debugOwner) {
                        org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target annotation-value owner=" + debugOwnerName + ", name=" + name + ", value=" + value);
                    }
                    if (value instanceof Type) {
                        addType((Type) value);
                    }
                }

                @Override
                public void visitEnum(String name, String desc, String value) {
                    if (debugOwner) {
                        org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target annotation-enum owner=" + debugOwnerName + ", name=" + name + ", desc=" + desc + ", value=" + value);
                    }
                    addType(Type.getType(desc));
                }

                @Override
                public AnnotationVisitor visitAnnotation(String name, String desc) {
                    if (debugOwner) {
                        org.appwork.loggingv3.LogV3.info("ClassCollector2: DEBUG target nested-annotation owner=" + debugOwnerName + ", name=" + name + ", desc=" + desc);
                    }
                    addType(Type.getType(desc));
                    return this;
                }

                @Override
                public AnnotationVisitor visitArray(String name) {
                    return this;
                }
            };
        }
    }
}
