
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
@Target({ ElementType.METHOD, ElementType.FIELD })
/**
 * -1 matches to NOW
 *
 * @author thomas
 * @date 29.06.2022
 *
 */
public @interface StorableValidateTimestampRelative {
    long min() default Long.MIN_VALUE;

    FailLevel level() default FailLevel.ERROR;

    long max() default Long.MAX_VALUE;

    String message() default "";

}
