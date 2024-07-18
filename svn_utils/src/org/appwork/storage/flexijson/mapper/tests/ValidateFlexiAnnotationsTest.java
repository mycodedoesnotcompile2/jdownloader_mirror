/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
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
package org.appwork.storage.flexijson.mapper.tests;

import java.lang.annotation.Annotation;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;

import org.appwork.remoteapi.annotations.ApiDocExample;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.Storable;
import org.appwork.storage.StorableAvailableSince;
import org.appwork.storage.StorableDateFormat;
import org.appwork.storage.StorableDeprecatedSince;
import org.appwork.storage.StorableExample;
import org.appwork.storage.TypeRef;
import org.appwork.storage.flexijson.FlexiJSONParser;
import org.appwork.storage.flexijson.FlexiParserException;
import org.appwork.storage.flexijson.FlexiUtils;
import org.appwork.storage.flexijson.ParsingError;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiEnumFallback;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.testframework.AWTest;
import org.appwork.utils.ClassPathScanner;
import org.appwork.utils.Exceptions;

/**
 * @author thomas
 * @date 18.10.2022
 *
 */
public class ValidateFlexiAnnotationsTest extends AWTest {
    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        new ClassPathScanner<Exception>() {
            @Override
            public void handle(Class<?> cls) throws Exception {
                if (Storable.class.isAssignableFrom(cls) && Storable.class != cls) {
                    validateClass(cls);
                }
            }
        }.run();
    }

    private void validateClass(Class<?> cls) throws Exception {
        ClassCache cc = ClassCache.getClassCache(cls);
        extracted(cc, StorableExample.class);
        extracted(cc, ApiDocExample.class);
        extracted(cc, StorableDeprecatedSince.class);
        extracted(cc, StorableAvailableSince.class);
        extracted(cc, Deprecated.class);
        extracted(cc, FlexiEnumFallback.class);
    }

    public <TT extends Annotation> void extracted(ClassCache cc, Class<TT> an) throws Exception {
        for (TT a : cc.getAnnotations(null, an)) {
            try {
                validate(a, cc, null);
            } catch (Exception e) {
                throw Exceptions.addSuppressed(e, new Exception(a + "\r\nContext: Class: " + cc.getCachedClass() + " " + an));
            }
        }
        for (String key : cc.getKeys()) {
            for (TT a : cc.getAnnotations(key, an)) {
                try {
                    validate(a, cc, key);
                } catch (Exception e) {
                    throw Exceptions.addSuppressed(e, new Exception(a + "\r\nContext: Method " + cc.getCachedClass() + " - " + key + " " + an + " \r\n" + cc.getGetter(key) + "\r\n" + cc.getSetter(key)));
                }
            }
        }
    }

    /**
     * @param a
     * @param key
     * @param cc
     * @throws Exception
     */
    private void validate(Annotation a, ClassCache cc, String key) throws Exception {
        if (a instanceof StorableExample) {
            validateAnnotation(((StorableExample) a));
        } else if (a instanceof ApiDocExample) {
            validateAnnotation(((ApiDocExample) a));
        } else if (a instanceof StorableDateFormat) {
            validateAnnotation(((StorableDateFormat) a));
        } else if (a instanceof StorableAvailableSince) {
            validateAnnotation(((StorableAvailableSince) a));
        } else if (a instanceof StorableDeprecatedSince) {
            validateAnnotation(((StorableDeprecatedSince) a));
            if (cc.getAnnotations(key, Deprecated.class).size() == 0) {
                throw new Exception("Excepcted a @Deprecated here as well");
            }
        } else if (a instanceof Deprecated) {
            if (cc.getAnnotations(key, StorableDeprecatedSince.class).size() == 0) {
                throw new Exception("Excepcted a @StorableDeprecatedSince here: " + cc + "." + key);
            }
        }
    }

    /**
     * @param storableDeprecatedSince
     * @throws Exception
     */
    private void validateAnnotation(StorableDeprecatedSince a) throws Exception {
        Date date = FlexiUtils.jsonToObject(FlexiUtils.serializeMinimized(a.value()), FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, new SimpleTypeRef<Date>(Date.class));
        assertNotNull(date);
        assertTrue(date.getTime() > FlexiUtils.jsonToObject("\"2010-06-01T00:00+0200\"", FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, new SimpleTypeRef<Date>(Date.class)).getTime());
        assertTrue(date.getTime() < System.currentTimeMillis());
    }

    /**
     * @param storableAvailableSince
     * @throws Exception
     */
    private void validateAnnotation(StorableAvailableSince a) throws Exception {
        Date date = FlexiUtils.jsonToObject(FlexiUtils.serializeMinimized(a.value()), FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, new SimpleTypeRef<Date>(Date.class));
        assertNotNull(date);
        assertTrue(date.getTime() > FlexiUtils.jsonToObject("\"2010-06-01T00:00+0200\"", FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES, new SimpleTypeRef<Date>(Date.class)).getTime());
        assertTrue(date.getTime() < System.currentTimeMillis());
    }

    /**
     * @param storableDateFormat
     */
    private void validateAnnotation(StorableDateFormat a) {
        new SimpleDateFormat(a.value());
    }

    private void validateAnnotation(ApiDocExample a) throws FlexiParserException, FlexiMapperException {
        final FlexiJSONParser parser = new FlexiJSONParser(a.value()) {
            @Override
            public boolean isParseInlineCommentEnabled() {
                return true;
            }

            @Override
            public boolean isParseLineCommentEnabled() {
                return true;
            }
        };
        parser.setDebug(new StringBuilder());
        final HashSet<ParsingError> ignore = new HashSet<ParsingError>(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES);
        // ignore.add(ParsingError.ERROR_STRING_VALUE_WITHOUT_QUOTES);
        parser.setIgnoreIssues(ignore);
        Object example = new FlexiJSonMapper().jsonToObject(parser.parse(), TypeRef.OBJECT);
    }

    /**
     * @param storableExample
     * @throws FlexiMapperException
     * @throws FlexiParserException
     */
    private void validateAnnotation(StorableExample a) throws FlexiParserException, FlexiMapperException {
        final FlexiJSONParser parser = new FlexiJSONParser(a.value()) {
            @Override
            public boolean isParseInlineCommentEnabled() {
                return true;
            }

            @Override
            public boolean isParseLineCommentEnabled() {
                return true;
            }
        };
        parser.setDebug(new StringBuilder());
        final HashSet<ParsingError> ignore = new HashSet<ParsingError>(FlexiJSONParser.IGNORE_LIST_ENSURE_CORRECT_VALUES);
        // ignore.add(ParsingError.ERROR_STRING_VALUE_WITHOUT_QUOTES);
        parser.setIgnoreIssues(ignore);
        Object example = new FlexiJSonMapper().jsonToObject(parser.parse(), TypeRef.OBJECT);
    }

    public static void main(String[] args) {
        run();
    }
}
