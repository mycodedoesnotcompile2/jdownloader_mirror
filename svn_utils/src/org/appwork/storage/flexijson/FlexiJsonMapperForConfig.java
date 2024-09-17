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
package org.appwork.storage.flexijson;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Comparator;

import org.appwork.storage.flexijson.mapper.DefaultObjectToJsonContext;
import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperException;
import org.appwork.storage.flexijson.mapper.FlexiMapperTags;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 16.12.2022
 *
 */
public class FlexiJsonMapperForConfig extends FlexiJSonMapper {
    {
        setIgnoreDefaultValuesEnabled(false);
    }

    @Override
    protected boolean isAnnotationCommentsEnabled() {
        return true;
    }

    @Override
    public boolean isTagDefaultValuesEnabled(final Object obj, final CompiledType clazz, final Getter g) {
        return true;
    }

    @Override
    protected KeyValueElement methodOrFieldAnnotationsToComments(final Getter g, final DefaultObjectToJsonContext context, final KeyValueElement create, final Object defaultValue, final boolean addDefaultValueAnnotation) throws FlexiMapperException {
        final KeyValueElement ret = super.methodOrFieldAnnotationsToComments(g, context, create, defaultValue, addDefaultValueAnnotation);
        if (ret.getCommentsBeforeKey() != null) {
            Collections.sort(ret.getCommentsBeforeKey(), new Comparator<FlexiCommentJsonNode>() {
                @Override
                public int compare(FlexiCommentJsonNode o1, FlexiCommentJsonNode o2) {
                    if (!(o1 instanceof FlexiComment) && !(o1 instanceof FlexiComment)) {
                        return 0;
                    }
                    if (!(o1 instanceof FlexiComment)) {
                        return -1;
                    }
                    if (!(o2 instanceof FlexiComment)) {
                        return 1;
                    }
                    for (FlexiMapperTags tag : new FlexiMapperTags[] { FlexiMapperTags.DOCS, FlexiMapperTags.AUTH, FlexiMapperTags.TYPE, FlexiMapperTags.OPTIONS, FlexiMapperTags.EXAMPLE, FlexiMapperTags.DEFAULT_VALUE, FlexiMapperTags.SEE }) {
                        if (((FlexiComment) o1).hasTag(tag) && !((FlexiComment) o2).hasTag(tag)) {
                            return -1;
                        } else if (((FlexiComment) o2).hasTag(tag) && !((FlexiComment) o1).hasTag(tag)) {
                            return 1;
                        }
                    }
                    for (FlexiMapperTags tag : FlexiMapperTags.values()) {
                        if (((FlexiComment) o1).hasTag(tag) && !((FlexiComment) o2).hasTag(tag)) {
                            return -1;
                        } else if (((FlexiComment) o2).hasTag(tag) && !((FlexiComment) o1).hasTag(tag)) {
                            return 1;
                        }
                    }
                    return 0;
                }
            });
            ret.getCommentsBeforeKey().merge(this, true);
        }
        return ret;
    }

    @Override
    protected boolean isEnumOptionsCommentsEnabled(final CompiledType enumClass) {
        return true;
    }

    @Override
    protected boolean isAddDefaultValueCommentEnabled(final Object obj, final CompiledType clazz, final Getter g) {
        final Type type = g.getMethod().getGenericReturnType();
        if (Clazz.isPrimitive(type)) {
            return true;
        }
        if (Clazz.isString(type)) {
            return true;
        }
        if (Clazz.isPrimitiveWrapper(type)) {
            return true;
        }
        CompiledType ct = CompiledType.create(type, clazz);
        final CompiledType componentType = ct.getComponentType();
        if (componentType != null) {
            if (componentType.isPrimitive()) {
                return true;
            }
            if (componentType.isString()) {
                return true;
            }
        }
        return false;
    }

    @Override
    protected boolean isTypeCommentsEnabled() {
        return true;
    }
}