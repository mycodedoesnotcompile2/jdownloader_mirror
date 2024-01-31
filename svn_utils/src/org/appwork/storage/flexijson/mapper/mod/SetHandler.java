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
package org.appwork.storage.flexijson.mapper.mod;

import java.util.Map.Entry;

import org.appwork.exceptions.WTFException;
import org.appwork.moncompare.Condition;
import org.appwork.moncompare.Condition.OpHandler;
import org.appwork.moncompare.ConditionException;
import org.appwork.moncompare.Scope;
import org.appwork.moncompare.list.ListAccessorInterface;
import org.appwork.moncompare.object.MapAccessorInterface;
import org.appwork.storage.flexijson.InvalidPathException;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.StringUtils;

/**
 * @author thomas
 * @date 22.11.2023
 *
 */
public class SetHandler implements OpHandler {
    @Override
    public Object opEval(Condition<?> container, Object query, Scope scope) throws ConditionException {
        boolean anyThingChanged = false;
        MapAccessorInterface queryMap = container.getMapWrapper(query);
        try {
            boolean onlyIfUnset = false;
            Condition<Object> options = container.getOptions(Condition.class);
            if (options != null) {
                for (Entry<String, Object> es : options.entrySet()) {
                    if (StringUtils.equalsIgnoreCase(es.getKey(), "ifUnset")) {
                        onlyIfUnset = Boolean.TRUE.equals(es.getValue()) || (es.getValue() instanceof Number && !CompareUtils.equalsNumber((Number) es.getValue(), 0));
                    }
                }
            }
            if (queryMap == null) {
                // {a.b:{§set:1}} --> a.b:{§set:{§this:1}} --> a:{§set:{b:1}}
                queryMap = container.getMapWrapper(new Condition(Condition.$$THIS, query));
            }
            {
                // MapAccessor map = container.getMapWrapper(parent);
                // if (map != null) {
                for (Entry<String, Object> es : queryMap) {
                    if (Condition.$OPTIONS.equalsIgnoreCase(es.getKey())) {
                        continue;
                    }
                    Scope newScope = scope.copy();
                    Scope resolved = container.resolveKeyPath(newScope, JSPath.fromPathString(es.getKey()));
                    // if (resolved.getLast() == null) {
                    // element does not exist yet
                    Object parent = null;
                    Object key = null;
                    int lowestKeyNotFound = -1;
                    for (int i = resolved.getPath().size() - 1; i >= 0; i--) {
                        Object pe = resolved.getPath().get(i);
                        if (pe instanceof String && ((String) pe).startsWith("§")) {
                            continue;
                        }
                        key = pe;
                        while (i >= 0) {
                            if (resolved.getScope().get(i) == Condition.KEY_DOES_NOT_EXIST) {
                                lowestKeyNotFound = i;
                            }
                            if (parent == null) {
                                parent = resolved.getScope().get(i);
                            }
                            i--;
                        }
                        break;
                    }
                    if (parent == Condition.KEY_DOES_NOT_EXIST) {
                        // Could not resolve - no autoCreate
                        throw new ConditionException("Element not found: " + JSPath.fromPathElements(resolved.getPath().getElements().subList(0, lowestKeyNotFound)));
                    }
                    // Scope parent = resolved;
                    // Special handling for §§this
                    // if (key instanceof String && Condition.$$THIS.equalsIgnoreCase((String) key)) {
                    // key = parent.getPath().getLast();
                    // parent = parent.getParent();
                    // }
                    MapAccessorInterface map = container.getMapWrapper(parent);
                    ListAccessorInterface list;
                    if (map != null) {
                        if (onlyIfUnset && map.get(String.valueOf(key)) != Condition.KEY_DOES_NOT_EXIST) {
                            continue;
                        }
                        Object old = map.put(String.valueOf(key), es.getValue());
                        if (!container.equalsDeep(container, old, es.getValue(), resolved)) {
                            anyThingChanged = true;
                            if (container._isDebug()) {
                                container.log(resolved.getPath(), "Set value: " + map.get(String.valueOf(key)));
                            }
                        }
                    } else if ((list = container.getListWrapper(parent)) != null) {
                        int indexToSet = JSPath.toArrayIndex(key);
                        if (onlyIfUnset && list.size() > indexToSet && list.get(indexToSet) != Condition.KEY_DOES_NOT_EXIST) {
                            continue;
                        }
                        if (container._isAutoCreateMissingNodes()) {
                            for (int i = list.size(); i < indexToSet; i++) {
                                list.add(null);
                                anyThingChanged = true;
                            }
                        }
                        if (list.size() <= indexToSet) {
                            anyThingChanged = true;
                        } else {
                            if (!CompareUtils.equals(list.get(indexToSet), es.getValue())) {
                                anyThingChanged = true;
                            }
                        }
                        if (list.size() == indexToSet) {
                            list.add(es.getValue());
                        } else if (list.size() > indexToSet) {
                            list.set(indexToSet, es.getValue());
                        } else {
                            throw new ConditionException("Index Missmatch");
                        }
                    } else {
                        throw new WTFException("Not Supported");
                    }
                    // } else {
                    //
                    //
                    // throw new WTFException("Not Supported");
                    // }
                    // map.put(es.getKey(), es.getValue());
                    // System.out.println(123);
                }
                // }
            }
        } catch (InvalidPathException e) {
            throw new ConditionException(e);
        }
        // TODO: eventl. ist boolean als return besser
        return anyThingChanged;
        // re-resolve - object structure might has changed
        // das geht nicht für die §any conditzions - eventl. mit Filterroot arbeiten
        // return container.resolveKeyPath(new Scope(scope.getFirst()), scope.getPath().getParent()).getLast();
    }

    @Override
    public boolean isFilterRoot() {
        return false;
    }
}