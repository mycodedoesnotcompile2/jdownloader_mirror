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
package org.appwork.utils.parser;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.appwork.utils.IO;

public class SourceParser extends Object {

    private static final Pattern PATTERN_REMOVE_COMENTS1 = Pattern.compile("\\/\\*.*?\\*\\/", Pattern.DOTALL);

    private static final Pattern PATTERN_REMOVE_COMENTS2 = Pattern.compile("//.*");

    private HashMap<File, String[]> map;

    private final File sourceFolder;

    private FilenameFilter filter;

    public SourceParser(final File file) throws IOException {
        this.map = new HashMap<File, String[]>();
        this.sourceFolder = file;
        this.filter = null;
    }

    public HashMap<File, String> findOccurancesOf(final Field f) {
        final HashMap<File, String> found = new HashMap<File, String>();

        for (Entry<File, String[]> next : this.map.entrySet()) {
            for (String statement : next.getValue()) {
                if (statement.contains(f.getName())) {
                    if (statement.contains("//") || statement.contains("/*")) {
                        // TODO: Old assignment made no sense
                        // statement = statement;
                    }
                    found.put(next.getKey(), statement);
                }
            }
        }
        return found;
    }

    public File getSource() {
        return this.sourceFolder;
    }

    private void getSourceFiles(final File file) throws IOException {
        for (final File f : file.listFiles(new FilenameFilter() {

            
            public boolean accept(final File dir, final String name) {
                return (SourceParser.this.filter == null || SourceParser.this.filter.accept(dir, name)) && (name.endsWith(".java") || new File(dir, name).isDirectory());
            }

        })) {
            if (f.isDirectory()) {
                this.getSourceFiles(f);
            } else {
                String statement = IO.readFileToString(f);
                statement = SourceParser.PATTERN_REMOVE_COMENTS1.matcher(statement).replaceAll("/*comment*/");
                statement = SourceParser.PATTERN_REMOVE_COMENTS2.matcher(statement).replaceAll("//comment");
                this.map.put(f, statement.split("[\\{\\}\\;]"));
            }
        }
    }

    public void scan() throws IOException {
        this.map = new HashMap<File, String[]>();
        this.getSourceFiles(this.sourceFolder);
    }

    public void setFilter(final FilenameFilter filenameFilter) {
        this.filter = filenameFilter;
    }

}
