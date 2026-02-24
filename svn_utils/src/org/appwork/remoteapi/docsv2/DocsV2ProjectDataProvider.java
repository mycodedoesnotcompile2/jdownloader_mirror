package org.appwork.remoteapi.docsv2;

import java.lang.annotation.Annotation;
import java.lang.reflect.AnnotatedElement;
import java.util.List;

import org.appwork.remoteapi.docsv2.model.DocsV2Definition.AuthDoc;

public interface DocsV2ProjectDataProvider {
    List<AuthDoc> extractAuthInfo(String methodName, Annotation[] annotations);

    String toWikiUrl(String wikiRef);

    List<String> inferWikiLinks(AnnotatedElement element);
}
