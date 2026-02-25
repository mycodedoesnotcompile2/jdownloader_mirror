package org.appwork.remoteapi.docsv2;

import java.lang.reflect.AnnotatedElement;
import java.util.List;

public interface DocsV2ProjectDataProvider {
    String toWikiUrl(String wikiRef);

    List<String> inferWikiLinks(AnnotatedElement element);
}
