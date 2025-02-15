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
package org.appwork.storage.flexijson.utils;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import org.appwork.storage.flexijson.FlexiJSonArray;
import org.appwork.storage.flexijson.FlexiJSonNode;
import org.appwork.storage.flexijson.FlexiJSonObject;
import org.appwork.storage.flexijson.FlexiJSonValue;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.storage.flexijson.KeyValueElement;
import org.appwork.storage.simplejson.mapper.Property;
import org.appwork.utils.reflection.CompiledType;

/**
 * @author thomas
 * @date 23.12.2022
 *
 */
public abstract class FlexiWalker {
    public static final FlexiJSonNode SKIP = new FlexiJSonValue();
    private FlexiJSonNode             root;
    private CompiledType              rootType;

    /**
     * @param node
     * @param type
     */
    public FlexiWalker(FlexiJSonNode node, CompiledType type) {
        this.root = node;
        this.rootType = type;
    }

    public FlexiJSonNode run() {
        JSPath path = new JSPath();
        FlexiJSonNode newNode = build(root, rootType, path);
        if (!runOn(root, rootType, path, newNode)) {
            root = null;
        }
        return newNode;
    }

    public FlexiJSonNode getRoot() {
        return root;
    }

    /**
     * @param root2
     * @param rootType2
     * @param path
     * @return
     */
    protected FlexiJSonNode build(FlexiJSonNode sourceNode, CompiledType type, JSPath path) {
        return SKIP;
    }

    /**
     * @param type
     * @param jsPath
     * @param root2
     */
    private boolean runOn(FlexiJSonNode node, CompiledType type, JSPath path, FlexiJSonNode buildNode) {
        if (!onNode(node, type, path)) {
            return false;
        }
        FlexiJSonNode newNode = null;
        if (node instanceof FlexiJSonArray) {
            FlexiJSonArray array = (FlexiJSonArray) node;
            int i = 0;
            type = type == null ? null : type.getComponentTypeFor(Collection.class, Arrays.class);
            for (Iterator<FlexiJSonNode> it = array.iterator(); it.hasNext();) {
                FlexiJSonNode sub = it.next();
                JSPath newPath = path.derive(i++);
                if (buildNode != null && buildNode != SKIP) {
                    newNode = build(sub, type, newPath);
                    if (newNode != SKIP) {
                        ((FlexiJSonArray) buildNode).add(newNode);
                    }
                }
                if (!runOn(sub, type, newPath, newNode)) {
                    it.remove();
                }
            }
        } else if (node instanceof FlexiJSonObject) {
            FlexiJSonObject object = (FlexiJSonObject) node;
            for (Iterator<KeyValueElement> it = object.getElements().iterator(); it.hasNext();) {
                KeyValueElement e = it.next();
                CompiledType sub = null;
                if (type != null) {
                    if (type.isMap()) {
                        sub = type.getComponentTypeFor(Map.class);
                    } else {
                        Property prop = type.getClassCache().getProperty(e.getKey());
                        if (prop != null) {
                            sub = prop.type;
                        }
                    }
                }
                JSPath newPath = path.derive(e.getKey());
                if (newNode != null) {
                    newNode = build(e.getValue(), sub, newPath);
                    if (buildNode != null && buildNode != SKIP) {
                        ((FlexiJSonObject) buildNode).add(new KeyValueElement((FlexiJSonObject) buildNode, newPath, e.getKey(), newNode));
                    }
                }
                if (!runOn(e.getValue(), sub, newPath, newNode)) {
                    it.remove();
                }
            }
        }
        return true;
    }

    /**
     * @param node
     * @param newNode
     * @param type
     * @param path
     * @return
     */
    private FlexiJSonNode build(FlexiJSonNode node, FlexiJSonNode newNode, CompiledType type, JSPath path) {        
        return null;
    }

    /**
     * @param node
     * @param type
     * @param path
     */
    abstract public boolean onNode(FlexiJSonNode node, CompiledType type, JSPath path);
}
