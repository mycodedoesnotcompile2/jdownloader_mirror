package org.appwork.remoteapi.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.METHOD, ElementType.TYPE })
@HiddenForHelpDocs
public @interface APITag {
    Class<? extends APITagDefinition>[] value();
}
