package jd.parser.html;

import java.util.regex.Pattern;

import jd.http.Browser;
import jd.http.Request;
import jd.parser.Regex;

public class HTMLSearch {
    /**
     * Returns first value of given list of possible html meta tags. </br> Example for such a tag: <meta property="<tag>"
     * content="<Content we want>" />
     */
    @Deprecated
    public static String searchMetaTag(final Browser br, final String... properties) {
        return HTMLSearch.searchMetaTagProperty(br.getRequest(), properties);
    }

    public static String searchMetaTagProperty(final Request request, final String... properties) {
        return HTMLSearch.searchMetaTagProperty(request.getHtmlCode(), properties);
    }

    public static String searchMetaTagName(final Request request, final String... properties) {
        return HTMLSearch.searchMetaTagName(request.getHtmlCode(), properties);
    }

    public static String searchMetaTagProperty(final String source, final String... properties) {
        for (final String property : properties) {
            String result = new Regex(source, "<meta[^>]*property\\s*=\\s*(\"|')" + Pattern.quote(property) + "\\1[^>]*content\\s*=\\s*\\1([^\">]+)\\1[^>]*/?>").getMatch(1);
            if (result == null) {
                result = new Regex(source, "<meta[^>]*content\\s*=\\s*(\"|')([^\">]+)\\1[^>]*property\\s*=\\s*\\1" + Pattern.quote(property) + "\\1[^>]*/?>").getMatch(1);
            }
            if (result != null) {
                return result;
            }
        }
        return null;
    }

    public static String searchMetaTagName(final String source, final String... names) {
        for (final String name : names) {
            String result = new Regex(source, "<meta[^>]*name\\s*=\\s*(\"|')" + Pattern.quote(name) + "\\1[^>]*content\\s*=\\s*\\1([^\">]+)\\1[^>]*/?>").getMatch(1);
            if (result == null) {
                result = new Regex(source, "<meta[^>]*content\\s*=\\s*(\"|')([^\">]+)\\1[^>]*name\\s*=\\s*\\1" + Pattern.quote(name) + "\\1[^>]*/?>").getMatch(1);
            }
            if (result != null) {
                return result;
            }
        }
        return null;
    }
}
