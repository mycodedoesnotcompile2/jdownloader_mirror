package org.appwork.moncompare;

import org.appwork.moncompare.list.ListAccessorInterface;
import org.appwork.moncompare.object.MapAccessorInterface;

public interface TypeHandler {
    /**
     * Not handled, return value;
     *
     * @param value
     * @return
     */
    Object unwrapType(Object value);

    /**
     * Not handled return null;
     *
     * @param expressions
     * @return
     */
    ListAccessorInterface getListAccessor(Object expressions);

    /**
     * Not handled: return null;
     *
     * @param a
     * @param b
     * @return
     */
    Boolean equals(Object a, Object b);

    /**
     * Not handled: return null
     *
     * @param a
     * @param b
     * @return
     */
    Integer compare(Object a, Object b);

    /**
     * @param expression
     * @return
     */
    MapAccessorInterface getMapAccessor(Object expression);

    /**
     * @return
     */
    Object newAutoCreateMap();

    /**
     * @return
     */
    Object newAutoCreateArray();
}
