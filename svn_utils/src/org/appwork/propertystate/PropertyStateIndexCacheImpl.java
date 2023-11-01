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
package org.appwork.propertystate;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * @author thomas
 * @date 01.09.2022
 *
 */
public class PropertyStateIndexCacheImpl implements PropertyStateIndexCache {

    private ArrayList<PropertyState> indexList;

    /**
     *
     */
    public PropertyStateIndexCacheImpl() {
        indexList = new ArrayList<PropertyState>();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.propertystate.PropertyStateIndexCache#toMap(org.appwork.propertystate.PropertyState)
     */
    @Override
    public Map<String, Object> toMap(PropertyState state) {

        int nextFreeIndex = -1;
        for (int i = 0; i < indexList.size(); i++) {
            PropertyState other = indexList.get(i);

            if (other == state) {
                HashMap<String, Object> ret = new HashMap<String, Object>();
                ret.put(Manager.INDEX_PROPERTY, i);
                return ret;
            } else if (other != null) {
                continue;
            } else if (nextFreeIndex < 0) {
                nextFreeIndex = i;
            }
        }

        if (nextFreeIndex >= 0) {
            indexList.set(nextFreeIndex, state);
            HashMap<String, Object> ret = new HashMap<String, Object>();
            ret.putAll(state.toMap());
            ret.put(Manager.INDEX_PROPERTY, nextFreeIndex);
            return ret;
        } else {
            indexList.add(state);
            HashMap<String, Object> ret = new HashMap<String, Object>();
            ret.putAll(state.toMap());
            ret.put(Manager.INDEX_PROPERTY, indexList.size() - 1);
            return ret;
        }

    }

    @Override
    public PropertyState getState(int index) {
        if (indexList != null && indexList.size() > index) {
            return indexList.get(index);
        } else {
            return null;
        }

    }

    @Override
    public void put(int index, PropertyState found) {
        while (indexList.size() < index) {
            indexList.add(null);
        }
        if (indexList.size() == index) {
            indexList.add(found);
        } else {
            indexList.set(index, found);
        }

    }

}
