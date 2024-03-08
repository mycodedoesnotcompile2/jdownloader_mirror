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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.storage.simplejson;

import org.appwork.remoteapi.annotations.AllowNonStorableObjects;
import org.appwork.storage.Storable;
import org.appwork.utils.reflection.Clazz;

/**
 * @author thomas
 *
 */
public class JSonValue implements JSonNode, Storable {
    public final static JSonValue BOOLEAN_TRUE  = new JSonValue(true) {
                                                    public void setValue(Object value) {
                                                        throw new IllegalStateException();
                                                    };

                                                    public void setType(org.appwork.storage.simplejson.ValueType type) {
                                                        throw new IllegalStateException();
                                                    };
                                                };
    public final static JSonValue BOOLEAN_FALSE = new JSonValue(false) {
                                                    public void setValue(Object value) {
                                                        throw new IllegalStateException();
                                                    };

                                                    public void setType(org.appwork.storage.simplejson.ValueType type) {
                                                        throw new IllegalStateException();
                                                    };
                                                };
    public final static JSonValue BOOLEAN_NULL  = new JSonValue((Boolean) null) {
                                                    public void setValue(Object value) {
                                                        throw new IllegalStateException();
                                                    };

                                                    public void setType(org.appwork.storage.simplejson.ValueType type) {
                                                        throw new IllegalStateException();
                                                    };
                                                };
    public final static JSonValue NULL          = new JSonValue((String) null) {
                                                    public void setValue(Object value) {
                                                        throw new IllegalStateException();
                                                    };

                                                    public void setType(org.appwork.storage.simplejson.ValueType type) {
                                                        throw new IllegalStateException();
                                                    };
                                                };
    protected Object              value;

    public void setValue(final Object value) {
        this.value = value;
    }

    public void setType(final ValueType type) {
        this.type = type;
    }

    protected ValueType type;

    public JSonValue(/* Storable */) {
        // undefined
        value = null;
    }

    /**
     * @param b
     */
    public JSonValue(final Boolean value) {
        this.value = value;
        type = ValueType.BOOLEAN;
    }

    public JSonValue(final Number value) {
        this.value = value;
        if (Clazz.isFloatingPointNumber(value.getClass())) {
            type = ValueType.DOUBLE;
        } else {
            type = ValueType.LONG;
        }
    }

    /**
     * @param object
     */
    public JSonValue(final String value) {
        this(value, false);
    }

    /**
     * @param value2
     * @param b
     */
    public JSonValue(final String value, final boolean reference) {
        this.value = value;
        if (value == null) {
            type = ValueType.NULL;
        } else if (reference) {
            type = ValueType.REFERENCE;
        } else {
            type = ValueType.STRING;
        }
    }

    public ValueType getType() {
        return type;
    }

    @AllowNonStorableObjects
    public Object getValue() {
        return value;
    }

    @Override
    public String toString() {
        switch (getType()) {
        case BOOLEAN:
        case DOUBLE:
        case LONG:
            return getValue().toString();
        case STRING:
            return "\"" + JSonUtils.escape(getValue().toString()) + "\"";
        case NULL:
            return "null";
        case UNDEFINED:
            return "undefined";
        case REFERENCE:
            return "reference:" + getValue().toString();
        default:
            return null;
        }
    }

    @Override
    public String toPrettyString() {
        return toString();
    }
}
