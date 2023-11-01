
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
/**
 * -1 matches to NOW
 *
 * @author thomas
 * @date 29.06.2022
 *
 */
public @interface StorableValidateTimestamp {
    long min() default 0;

    FailLevel level() default FailLevel.ERROR;

    long max() default Long.MAX_VALUE;

    String message() default "";

}
