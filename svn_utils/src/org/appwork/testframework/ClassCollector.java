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
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.appwork.utils.Hash;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * @author daniel
 * @date Dec 18, 2025
 *
 */
public class ClassCollector {
    public Map<String, String> getClasses(final String className, final boolean collectAll) throws IOException {
        ClassReader reader = new ClassReader(className);
        ClassReferenceCollector collector = new ClassReferenceCollector();
        reader.accept(collector, ClassReader.SKIP_DEBUG);
        final Set<String> classes = new HashSet<String>();
        classes.addAll(collector.referenced);
        final Map<String, String> inspected = new LinkedHashMap<String, String>();
        loop: while (true) {
            for (final String clazz : classes) {
                if (skip(clazz)) {
                    continue;
                }
                if (!inspected.containsKey(clazz)) {
                    reader = new ClassReader(clazz);
                    inspected.put(clazz, Hash.getBytesHash(reader.b, Hash.HASH_TYPE_SHA256));
                    if (collectAll) {
                        collector = new ClassReferenceCollector();
                        reader.accept(collector, ClassReader.SKIP_DEBUG);
                        for (String referenced : collector.referenced) {
                            if (skip(referenced)) {
                                continue;
                            }
                            classes.add(referenced);
                        }
                    }
                    continue loop;
                }
            }
            break;
        }
        return inspected;
    }

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

    protected class ClassReferenceCollector extends ClassVisitor {
        private final Set<String> referenced = new HashSet<String>();

        public ClassReferenceCollector() {
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
            return super.visitField(access, name, desc, signature, value);
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
            };
        }
    }
}
