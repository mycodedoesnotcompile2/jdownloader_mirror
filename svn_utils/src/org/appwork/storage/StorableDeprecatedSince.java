
package org.appwork.storage;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author thomas
 *
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.METHOD, ElementType.FIELD, ElementType.TYPE, ElementType.PARAMETER })
// @StorableDeprecatedSince(value = "2022-03-23T00:00+0200", message = "Use 'repoName' instead.")
public @interface StorableDeprecatedSince {
    String value();

    FailLevel level() default FailLevel.WARNING;

    String message() default "";

}
