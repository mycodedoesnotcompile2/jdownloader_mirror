/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2024, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.builddecision;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.Storable;

/**
 * @author thomas
 * @date Nov 28, 2024
 *
 */
public class BuildDecisionData implements Storable {
    public static class Option implements Storable {
        private String[] dependsOn;

        /**
         *
         */
        public Option() {
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "Launcher Regex: " + tag + " Required Imports: " + Arrays.toString(imports) + " DependsOn: " + Arrays.toString(dependsOn);
        }

        /**
         * @param tag2
         * @param string
         * @param depends
         */
        public Option(String tag2, String string, String depends) {
            this.tag = tag2;
            this.imports = string == null ? null : string.split(";");
            this.dependsOn = depends == null ? null : depends.split(";");
        }

        public String[] getDependsOn() {
            return dependsOn;
        }

        public void setDependsOn(String[] dependsOn) {
            this.dependsOn = dependsOn;
        }

        private String tag;

        public String getTag() {
            return tag;
        }

        public void setTag(String tag) {
            this.tag = tag;
        }

        public String[] getImports() {
            return imports;
        }

        public void setImports(String[] imports) {
            this.imports = imports;
        }

        private String[] imports;
    }

    private String msg;

    /**
     *
     */
    public BuildDecisionData() {
    }

    /**
     * @param anno
     * @param project
     * @param type
     */
    public BuildDecisionData(BuildDecisionRequired anno, Class<?> type) {
        String[] imports = anno.imports();
        if (anno.imports().length == 0 || (anno.imports().length == 1 && anno.imports()[0].equals(""))) {
            imports = null;
        }
        if (imports != null && imports.length != anno.tags().length) {
            throw new WTFException("Bad annotation. ids and anno.imports");
        }
        int i = 0;
        String[] dependsOn = anno.dependsOn();
        if (dependsOn.length == 0 || (dependsOn.length == 1 && dependsOn[0].equals(""))) {
            dependsOn = null;
        }
        if (dependsOn != null && dependsOn.length != anno.tags().length) {
            throw new WTFException("Bad annotation. tags and anno.dependsOn");
        }
        this.msg = anno.msg();
        options = new ArrayList<Option>();
        for (String tag : anno.tags()) {
            options.add(new Option(tag, imports == null ? null : imports[i], dependsOn == null ? null : dependsOn[i]));
            i++;
        }
        this.cls = type.getName();
    }

    public String getMsg() {
        return msg;
    }

    public void setMsg(String msg) {
        this.msg = msg;
    }

    private List<Option> options;

    public List<Option> getOptions() {
        return options;
    }

    public void setOptions(List<Option> options) {
        this.options = options;
    }

    public String getCls() {
        return cls;
    }

    public void setCls(String cls) {
        this.cls = cls;
    }

    private String cls;
}
