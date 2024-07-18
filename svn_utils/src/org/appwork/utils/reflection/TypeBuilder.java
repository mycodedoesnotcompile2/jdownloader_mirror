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
package org.appwork.utils.reflection;

import java.lang.reflect.Array;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author thomas
 * @date 11.06.2024
 *
 */
public class TypeBuilder {
    private static final Map<String, Class> REPLACE = new HashMap<String, Class>();
    static {
        REPLACE.put("byte", byte.class);
        REPLACE.put("short", short.class);
        REPLACE.put("int", int.class);
        REPLACE.put("long", long.class);
        REPLACE.put("char", char.class);
        REPLACE.put("float", float.class);
        REPLACE.put("double", double.class);
        REPLACE.put("boolean", boolean.class);
        REPLACE.put("void", void.class);
    }

    /**
     * @param string
     * @return
     * @throws ClassNotFoundException
     * @throws TypeParserException
     */
    public Type parse(String string) throws TypeParserException {
        GenericTypeImpl gn = new GenericTypeImpl(Object.class);
        char[] chars = string.toCharArray();
        int i = getGenericType(gn, chars, 0);
        if (i != chars.length) {
            throw new TypeParserException("Unexpected End of TypeDefinition");
        }
        return gn.getActualTypeArguments()[0];
    }

    public static class GenericTypeImpl implements ParameterizedType {
        private Class      raw;
        private List<Type> actualTypeArguments = new ArrayList<Type>();

        /**
         * @param class1
         */
        public GenericTypeImpl(Class class1) {
            raw = class1;
        }

        /**
         * @see java.lang.reflect.ParameterizedType#getActualTypeArguments()
         */
        @Override
        public Type[] getActualTypeArguments() {
            return actualTypeArguments.toArray(new Type[0]);
        }

        /**
         * @see java.lang.reflect.ParameterizedType#getRawType()
         */
        @Override
        public Type getRawType() {
            return raw;
        }

        /**
         * @see java.lang.reflect.ParameterizedType#getOwnerType()
         */
        @Override
        public Type getOwnerType() {
            // is called from class.equals
            // DebugMode.debugger();
            return null;
        }

        /**
         * @param class1
         */
        public void add(Type class1) {
            actualTypeArguments.add(class1);
        }
    }

    /**
     * @param chars
     * @return
     * @throws ClassNotFoundException
     * @throws TypeParserException
     */
    private int getGenericType(GenericTypeImpl major, char[] chars, int start) throws TypeParserException {
        StringBuilder sb = new StringBuilder();
        int i;
        for (i = start; i < chars.length; i++) {
            char c = chars[i];
            if (c == '<') {
                GenericTypeImpl genType = new GenericTypeImpl(getClass(sb.toString()));
                sb.setLength(0);
                i = getGenericType(genType, chars, i + 1);
                if (i >= chars.length) {
                    throw new TypeParserException("Unexpected End of Type Definition. Expected , or >");
                }
                c = chars[i];
                while (c == ',') {
                    i = getGenericType(genType, chars, i + 1);
                    c = chars[i];
                }
                if (c == '>') {
                    i++;
                }
                major.add(genType);
                validate(genType);
                return i;
            } else if (c == '>') {
                major.add(getClass(sb.toString()));
                return i;
            } else if (Character.isWhitespace(c)) {
                // ignore whitespace
                continue;
            } else if (c == ',') {
                major.add(getClass(sb.toString()));
                return i;
            } else {
                sb.append(c);
            }
        }
        major.add(getClass(sb.toString()));
        return i;
    }

    /**
     * @param genType
     * @throws TypeParserException
     */
    private void validate(GenericTypeImpl genType) throws TypeParserException {
        if (genType.actualTypeArguments.size() != genType.raw.getTypeParameters().length) {
            throw new TypeParserException("Invalid generic definition for " + genType.raw);
        }
    }

    /**
     * @param string
     * @return
     * @throws TypeParserException
     * @throws ClassNotFoundException
     */
    private Class getClass(String string) throws TypeParserException {
        if (string.length() == 0) {
            throw new TypeParserException("Unexpected End of Type. Expected a Classname");
        }
        if (string.endsWith("[]")) {
            Class base = getClass(string.substring(0, string.length() - 2));
            Class ret = Array.newInstance(base, 0).getClass();
            return ret;
        } else {
            Class ret = REPLACE.get(string);
            if (ret != null) {
                return ret;
            }
            try {
                return Class.forName(string);
            } catch (ClassNotFoundException e) {
                if (isJavaLangAutoExtensionAllowed() && !string.contains(".")) {
                    try {
                        return getClass("java.lang." + string);
                    } catch (TypeParserException e1) {
                    }
                    ;
                }
                if (isJavaUtilAutoExtensionAllowed() && !string.contains(".")) {
                    try {
                        return getClass("java.util." + string);
                    } catch (TypeParserException e1) {
                    }
                    ;
                }
                throw new TypeParserException(e);
            }
        }
    }

    private boolean javaUtilAutoExtensionAllowed = true;

    public boolean isJavaUtilAutoExtensionAllowed() {
        return javaUtilAutoExtensionAllowed;
    }

    public void setJavaUtilAutoExtensionAllowed(boolean javaUtilAutoExtensionAllowed) {
        this.javaUtilAutoExtensionAllowed = javaUtilAutoExtensionAllowed;
    }

    private boolean javaLangAutoExtensionAllowed = true;

    public boolean isJavaLangAutoExtensionAllowed() {
        return javaLangAutoExtensionAllowed;
    }

    public void setJavaLangAutoExtensionAllowed(boolean javaLangAutoExtensionAllowed) {
        this.javaLangAutoExtensionAllowed = javaLangAutoExtensionAllowed;
    }
}
