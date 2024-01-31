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
package org.appwork.moncompare;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import org.appwork.serializer.Deser;
import org.appwork.serializer.SC;
import org.appwork.storage.Storable;
import org.appwork.storage.flexijson.JSPath;
import org.appwork.storage.flexijson.mapper.interfacestorage.FlexiStorableInterface;

/**
 * @author thomas
 * @date 27.07.2023
 *
 */
public class Scope {
    private final LinkedList<Object> scope;

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        String ret = "<ROOT>";
        for (int i = 0; i < path.size(); i++) {
            ret += "\r\n";
            if (scope.get(i + 1) == Condition.KEY_DOES_NOT_EXIST) {
                ret += path.get(i) + " = " + "KEY_DOES_NOT_EXIST";
            } else {
                Object seria = scope.get(i + 1);
                if (seria == null || seria instanceof Storable || seria instanceof FlexiStorableInterface) {
                    ret += path.get(i) + " = " + Deser.toString(scope.get(i + 1), SC.LOG_SINGLELINE);
                } else {
                    ret += path.get(i) + " = " + String.valueOf(scope.get(i + 1));
                }
            }
        }
        return ret;
    }

    public List<Object> getScope() {
        return scope;
    }

    public JSPath getPath() {
        return path;
    }

    private JSPath path;

    /**
     * @param ret
     * @param realPath
     */
    public Scope(List<Object> scope, JSPath path) {
        this(new LinkedList<Object>(scope), path);
    }

    private Scope(LinkedList<Object> scope, JSPath path) {
        this.scope = scope;
        this.path = path;
    }

    /**
     * @param matcher
     */
    public Scope(Object matcher) {
        this(new LinkedList<Object>(Arrays.asList(matcher)), new JSPath());
    }

    /**
     * @return
     */
    public Object getLast() {
        return scope.getLast();
    }

    /**
     * @return
     */
    public Object getFirst() {
        return scope.getFirst();
    }

    /**
     * @return
     */
    public Scope copy() {
        return new Scope(new LinkedList<Object>(scope), path);
    }

    /**
     * @param value
     * @param keyOrg
     */
    public void add(Object value, Object key) {
        // DebugMode.breakIf(value == null);
        scope.add(value);
        path = path.derive(key);
    }

    /**
     * @return
     */
    public Scope getParent() {
        Scope ret = copy();
        Object last = ret.scope.removeLast();
        ret.path = ret.path.getParent();
        while (ret.scope.size() > 0 && ret.getLast() == last) {
            // find real parent.
            // b.a.$$THIS.$$THIS.$$THIS.$parent should not resolve to $$this(a) but to b.
            // DebugMode.debugger();
            ret.scope.removeLast();
            ret.path = ret.path.getParent();
        }
        if (ret.scope.size() == 0) {
            return null;
        }
        return ret;
    }

    /**
     *
     */
    public void removeLast() {
        scope.removeLast();
        path = path.getParent();
    }

    /**
     * @param keyDoesNotExist
     */
    public void replaceLast(Object newValue) {
        // path stays unchanged
        scope.removeLast();
        scope.add(newValue);
    }

    /**
     * @param test
     */
    public void replaceAll(Scope test) {
        scope.clear();
        scope.addAll(test.scope);
        this.path = test.path;
    }

    /**
     * @param indexMissing
     * @param autoCreated
     */
    public void set(int index, Object object) {
        scope.set(index, object);
    }

    /**
     * @param i
     * @param i2
     */
    public void trim(int from, int to) {
        List<Object> sub = scope.subList(from, to);
        scope.clear();
        scope.addAll(sub);
        path = JSPath.fromPathElements(path.getElements().subList(from, to).toArray(new Object[0]));
    }
}
