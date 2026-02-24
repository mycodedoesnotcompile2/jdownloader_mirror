/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
package org.appwork.utils.net.httpserver;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Represents a CORS origin rule with optional path restrictions.
 *
 * <p>
 * An OriginRule consists of:
 * </p>
 * <ul>
 * <li><b>origin:</b> A Pattern that matches the origin (e.g., "https://example.com")</li>
 * <li><b>paths:</b> An optional Set of Patterns that restrict which paths are allowed (null means all paths allowed)</li>
 * </ul>
 *
 * <p>
 * Examples:
 * </p>
 * <ul>
 * <li>OriginRule with only origin: Allows all paths for matching origin</li>
 * <li>OriginRule with origin and paths: Only allows specified paths for matching origin</li>
 * </ul>
 *
 * @author AppWork
 */
public class OriginRule {
    private final Pattern       origin;
    private final List<Pattern> paths;

    /**
     * Creates an OriginRule with only an origin pattern (all paths allowed).
     *
     * @param originPattern
     *            The origin pattern (e.g., Pattern.compile("https://example\\.com"))
     */
    public OriginRule(final Pattern originPattern) {
        this(originPattern, (List<Pattern>) null);
    }

    /**
     * Creates an OriginRule with origin and path patterns.
     *
     * @param originPattern
     *            The origin pattern (e.g., Pattern.compile("https://example\\.com"))
     * @param pathPatterns
     *            Set of path patterns (null means all paths allowed)
     */
    public OriginRule(final Pattern originPattern, final List<Pattern> pathPatterns) {
        if (originPattern == null) {
            throw new IllegalArgumentException("Origin pattern cannot be null");
        }
        this.origin = originPattern;
        this.paths = pathPatterns == null ? null : Collections.unmodifiableList(new ArrayList<Pattern>(pathPatterns));
    }

    public OriginRule(final Pattern originPattern, final Pattern... pathPatterns) {
        if (originPattern == null) {
            throw new IllegalArgumentException("Origin pattern cannot be null");
        }
        this.origin = originPattern;
        this.paths = pathPatterns == null ? null : Collections.unmodifiableList(Arrays.asList(pathPatterns));
    }

    /**
     * Creates an OriginRule from origin string (compiled to Pattern internally).
     *
     * @param originString
     *            The origin string (e.g., "https://example.com" or ".*" for all origins)
     */
    public OriginRule(final String originString) {
        this(originString, (List<String>) null);
    }

    /**
     * Creates an OriginRule from origin string and path strings (compiled to Patterns internally).
     *
     * @param originString
     *            The origin string (e.g., "https://example.com")
     * @param pathStrings
     *            List of path strings (null or empty means all paths allowed)
     */
    public OriginRule(final String originString, final List<String> pathStrings) {
        this(Pattern.compile(Pattern.quote(originString), Pattern.CASE_INSENSITIVE), toPattern(pathStrings));
    }

    /**
     * @param pathStrings
     * @return
     */
    private static List<Pattern> toPattern(List<String> pathStrings) {
        ArrayList<Pattern> ret = new ArrayList<Pattern>();
        if (pathStrings != null) {
            for (String p : pathStrings) {
                ret.add(Pattern.compile(Pattern.quote(p), Pattern.CASE_INSENSITIVE));
            }
        }
        return ret;
    }

    /**
     * Creates an OriginRule from origin string and path strings (varargs version).
     *
     * @param originString
     *            The origin string (e.g., "https://example.com")
     * @param pathStrings
     *            Path strings (empty means all paths allowed)
     */
    public OriginRule(final String originString, final String... pathStrings) {
        this(originString, pathStrings != null && pathStrings.length > 0 ? Arrays.asList(pathStrings) : null);
    }

    /**
     * @param string
     * @param compile
     */
    public OriginRule(String originString, Pattern... paths) {
        this(Pattern.compile(Pattern.quote(originString), Pattern.CASE_INSENSITIVE), paths);
    }

    /**
     * Gets the origin pattern.
     *
     * @return The origin pattern (never null)
     */
    public Pattern getOrigin() {
        return this.origin;
    }

    /**
     * Gets the path patterns.
     *
     * @return The path patterns (null means all paths allowed)
     */
    public List<Pattern> getPaths() {
        return paths;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("OriginRule{origin=").append(this.origin.pattern());
        if (this.paths != null && !this.paths.isEmpty()) {
            sb.append(", paths=[");
            boolean first = true;
            for (final Pattern path : this.paths) {
                if (!first) {
                    sb.append(", ");
                }
                sb.append(path.pattern());
                first = false;
            }
            sb.append("]");
        } else {
            sb.append(", paths=all");
        }
        sb.append("}");
        return sb.toString();
    }
}
