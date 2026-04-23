/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file.
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
package org.appwork.build;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Declares optional build hints for classpath resources and/or additional types that must be included when assembling a reduced source tree
 * or JAR.
 * <p>
 * <strong>Resource paths ({@link #paths()})</strong>: each path is relative to the declaring top-level type's package directory (same folder as the
 * {@code .java} / {@code .class}). If an entry starts with {@code '/'}, it is treated as relative to the project <strong>source tree
 * root</strong> (the leading slash is stripped).
 * <p>
 * <strong>Extra types ({@link #types()})</strong>: build tooling (for example {@code org.appwork.ide.build.BuildScriptGenerator})
 * scans {@code .java} sources for these markers and treats the referenced types like bytecode dependencies, so their sources (and transitive
 * references) are pulled in even when static dependency analysis misses them. You may place the annotation on any supported element; the
 * generator matches it anywhere in the file text.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE, ElementType.FIELD, ElementType.METHOD, ElementType.PARAMETER, ElementType.CONSTRUCTOR, ElementType.LOCAL_VARIABLE, ElementType.ANNOTATION_TYPE })
public @interface RequiresResource {
    /**
     * Classpath resource paths: each path is relative to the declaring top-level type's package directory, unless prefixed with {@code '/'}
     * (then relative to the project source root; leading slash stripped).
     */
    String[] paths() default {};

    /**
     * Additional types to include in reduced builds (same effect as an extra bytecode dependency root per type).
     */
    Class<?>[] types() default {};
}
