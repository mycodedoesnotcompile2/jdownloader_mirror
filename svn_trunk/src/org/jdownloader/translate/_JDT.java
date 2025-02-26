package org.jdownloader.translate;

import org.appwork.txtresource.TranslationFactory;

public class _JDT {
    public static String getLanguage() {
        return TranslationFactory.getDesiredLanguage();
    }

    public static final JdownloaderTranslation T = TranslationFactory.create(JdownloaderTranslation.class);

    /** Dummy function for missing TODO translation strings. */
    public static final String FIX_ME(String text) {
        return text;
    }
}
