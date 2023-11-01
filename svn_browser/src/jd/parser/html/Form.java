//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.parser.html;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jd.http.requests.FormData;
import jd.nutils.encoding.HTMLEntities;
import jd.parser.Regex;
import jd.parser.html.InputField.ElementType;
import jd.utils.EditDistance;

import org.appwork.utils.StringUtils;
import org.appwork.utils.net.URLHelper;

public class Form {
    public enum MethodType {
        GET,
        POST
    }

    /**
     * Ein Array mit allen Forms dessen Inhalt dem matcher entspricht. Achtung der Matcher bezieht sich nicht auf die Properties einer Form
     * sondern auf den Text der zwischen der Form steht. DafÃ¼r gibt es die formProperties
     */
    public static Form[] getForms(final Object requestInfo) {
        final LinkedList<Form> forms = new LinkedList<Form>();
        // opening and closing within opening tag | opening and closing with traditional tags | opened ended tag (no closing)
        final Pattern pattern = Pattern.compile("<\\s*form\\s+([^>]*)/\\s*>|<\\s*form(?:>|\\s+[^>]*>)(.*?)<\\s*/\\s*form\\s*>|<\\s*form(?:>|\\s+[^>]*>)(.+)", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);
        final String requestHtml = requestInfo.toString().replaceAll("(?s)<!--.*?-->", "");
        final Matcher formmatcher = pattern.matcher(requestHtml);
        while (formmatcher.find()) {
            final String total = formmatcher.group(0);
            // System.out.println(inForm);
            final Form form = new Form(total);
            forms.add(form);
        }
        return forms.toArray(new Form[forms.size()]);
    }

    /**
     * Action der Form entspricht auch oft einer URL
     */
    private String                        action;
    private final List<InputField>        inputfields            = new ArrayList<InputField>();
    private String                        htmlcode               = null;
    private MethodType                    method                 = MethodType.GET;
    /* default encoding for http forms */
    private String                        encoding               = "application/x-www-form-urlencoded";
    private InputField                    preferredSubmit;
    private final HashMap<String, String> keyValueMap            = new HashMap<String, String>();
    /**
     * https://html.spec.whatwg.org/multipage/syntax.html#attributes-2
     *
     * Attributes have a name and a value. Attribute names must consist of one or more characters other than controls, U+0020 SPACE, U+0022
     * ("), U+0027 ('), U+003E (>), U+002F (/), U+003D (=)
     */
    public final static String            ATTRIBUTE_NAME_PATTERN = "[^\t\n\f />\"'=\\p{Cntrl}]+";

    public Form() {
    }

    public Form(final String html) {
        this();
        this.parse(html);
    }

    public void addInputField(final InputField nv) {
        this.addInputFieldAt(nv, -1);
    }

    public void addInputFieldAt(final InputField nv, final int index) {
        if (nv != null) {
            final List<InputField> inputFields = this.getInputFields();
            if (index < 0 || index > inputFields.size()) {
                inputFields.add(nv);
            } else {
                inputFields.add(index, nv);
            }
        }
    }

    /**
     * Gibt zurück ob der gesuchte needle String im html Text bgefunden wurde
     *
     * @param needle
     * @return
     */
    public boolean containsHTML(final String needle) {
        return new Regex(this.getHtmlCode(), needle).matches();
    }

    public boolean equalsIgnoreCase(final Form f) {
        return this.toString().equalsIgnoreCase(f.toString());
    }

    public String getAction() {
        return this.action;
    }

    public String getAction(final URL base) {
        final String formAction = this.getAction();
        if ("".equals(formAction) && base != null) {
            // https://html.spec.whatwg.org/multipage/forms.html#form-submission-algorithm
            // https://www.w3.org/Bugs/Public/show_bug.cgi?id=14215#c1
            // https://tools.ietf.org/html/rfc3986#section-5.1
            return base.toString();
        }
        final boolean baseIsHTTPs = base != null && "https".equalsIgnoreCase(base.getProtocol());
        final boolean actionIsHTTPs = formAction != null && formAction.startsWith("https://");
        final boolean actionIsHTTP = formAction != null && formAction.startsWith("http://");
        final String ret;
        if (base != null && StringUtils.isNotEmpty(this.action)) {
            ret = URLHelper.parseLocation(base, this.action);
        } else if (StringUtils.isNotEmpty(this.action) && this.action.matches("^https?://.+")) {
            ret = this.action.replaceAll(" ", "%20");
        } else if (base != null && StringUtils.isEmpty(this.action)) {
            ret = base.toString();
        } else {
            ret = null;
        }
        if (ret != null && baseIsHTTPs) {
            if (actionIsHTTPs || actionIsHTTP == false) {
                /* only keep https when action does use https or not specified, but do NOT change formAction(http) to https */
                return ret.replaceFirst("http://", "https://");
            }
        }
        try {
            return URLHelper.fixPathTraversal(new URL(ret)).toString();
        } catch (MalformedURLException e) {
            return ret;
        }
    }

    /**
     * Gibt den variablennamen der am besten zu varname passt zurÃ¼ck.
     *
     * @param varname
     * @return
     */
    public String getBestVariable(final String varname) {
        String best = null;
        int bestDist = Integer.MAX_VALUE;
        for (final InputField ipf : this.getInputFields()) {
            final int dist = EditDistance.getLevenshteinDistance(varname, ipf.getKey());
            if (dist < bestDist) {
                best = ipf.getKey();
                bestDist = dist;
            }
        }
        return best;
    }

    public String getEncoding() {
        return this.encoding;
    }

    public String getHtmlCode() {
        return this.htmlcode;
    }

    /**
     * Gets the first InputField with this key (case insensitive matching).
     *
     * REMEMBER. There can be more than one file with this key
     *
     * @param key
     * @return
     */
    @Deprecated
    public InputField getInputField(final String key) {
        return this.getInputFieldByName(key);
    }

    /**
     * Gets the first InputField with this name (case insensitive matching).
     *
     * REMEMBER. There can be more than one file with this name
     *
     * @param name
     * @return
     */
    public InputField getInputFieldByName(final String name) {
        final List<InputField> ret = this.getInputFields("(?i)" + Pattern.quote(name), null);
        if (ret.size() > 0) {
            return ret.get(0);
        } else {
            return null;
        }
    }

    /**
     * @return InputField with key matching given regular expression.
     * @param regex
     *            RegEx to match against.
     */
    @Deprecated
    public InputField getInputFieldByNameRegex(final String keyPattern) {
        final List<InputField> ret = this.getInputFields(keyPattern, null);
        if (ret.size() > 0) {
            return ret.get(0);
        } else {
            return null;
        }
    }

    @Deprecated
    public InputField getInputFieldByType(final String type) {
        for (final InputField ipf : this.getInputFields()) {
            if (StringUtils.equalsIgnoreCase(ipf.getElementType().type(), type)) {
                return ipf;
            }
        }
        return null;
    }

    public java.util.List<InputField> getInputFields() {
        return this.inputfields;
    }

    public List<InputField> getInputFields(final String keyPattern, final String valuePattern, final ElementType... types) {
        final java.util.List<InputField> ret = new ArrayList<InputField>(this.getInputFields());
        if (types != null && types.length > 0 && ret.size() > 0) {
            final Set<ElementType> typesSet = new HashSet<ElementType>(Arrays.asList(types));
            final Iterator<InputField> it = ret.iterator();
            while (it.hasNext()) {
                final InputField next = it.next();
                if (!typesSet.contains(next.getElementType())) {
                    it.remove();
                }
            }
        }
        if (keyPattern != null && ret.size() > 0) {
            final Pattern pattern = Pattern.compile(keyPattern);
            final Iterator<InputField> it = ret.iterator();
            while (it.hasNext()) {
                final InputField next = it.next();
                final String key = StringUtils.valueOrEmpty(next.getKey());
                if (!pattern.matcher(key).matches()) {
                    it.remove();
                }
            }
        }
        if (valuePattern != null && ret.size() > 0) {
            final Pattern pattern = Pattern.compile(valuePattern);
            final Iterator<InputField> it = ret.iterator();
            while (it.hasNext()) {
                final InputField next = it.next();
                final String value = StringUtils.valueOrEmpty(next.getValue());
                if (!pattern.matcher(value).matches()) {
                    it.remove();
                }
            }
        }
        return ret;
    }

    @Deprecated
    public java.util.List<InputField> getInputFieldsByType(final String type) {
        final java.util.List<InputField> ret = new ArrayList<InputField>();
        for (final InputField ipf : this.getInputFields()) {
            if (org.appwork.utils.Regex.matches(ipf.getElementType().type(), type)) {
                ret.add(ipf);
            }
        }
        return ret;
    }

    public MethodType getMethod() {
        return this.method;
    }

    public InputField getPreferredSubmit() {
        final InputField preferredSubmit = this.preferredSubmit;
        if (preferredSubmit != null && !preferredSubmit.isDisabled() && this.getInputFields().contains(preferredSubmit)) {
            return preferredSubmit;
        } else {
            /*
             * we do not send InputFields with key== null || value == null, so we search for first submit InputField with key !=null &&
             * value != null
             */
            InputField bestSubmit = null;
            for (final InputField inputField : this.getInputFields()) {
                if (inputField.isSubmitType() && !inputField.isDisabled()) {
                    if (bestSubmit == null) {
                        bestSubmit = inputField;
                    } else if (bestSubmit.getKey() == null && inputField.getKey() != null) {
                        bestSubmit = inputField;
                    } else if (bestSubmit.getValue() == null && inputField.getValue() != null) {
                        bestSubmit = inputField;
                    }
                }
            }
            return bestSubmit;
        }
    }

    @Deprecated
    public String getPropertyString() {
        final StringBuilder stbuffer = new StringBuilder();
        for (final InputField ipf : this.getInputFields()) {
            /* nameless key-value are not being sent, see firefox */
            if (ipf.getKey() == null || ipf.isDisabled()) {
                continue;
            } else {
                if (stbuffer.length() > 0) {
                    stbuffer.append("&");
                }
                stbuffer.append(ipf.getKey());
                stbuffer.append("=");
                stbuffer.append(ipf.getValue());
            }
        }
        return stbuffer.toString();
    }

    /**
     * Gibt ein RegexObject bezülich des Form htmltextes zurück
     *
     * @param compile
     * @return
     */
    public Regex getRegex(final Pattern compile) {
        return new Regex(this.getHtmlCode(), compile);
    }

    /**
     * Gibt ein RegexObject bezülich des Form htmltextes zurück
     *
     * @param compile
     * @return
     */
    public Regex getRegex(final String string) {
        return new Regex(this.getHtmlCode(), string);
    }

    /**
     * https://www.w3schools.com/tags/tryit.asp?filename=tryhtml5_button_form
     *
     * @return
     */
    public List<FormData> getFormData() {
        final InputField preferredSubmit = this.getPreferredSubmit();
        final List<FormData> ret = new ArrayList<FormData>();
        for (final InputField entry : this.getInputFields()) {
            final String key = entry.getKey();
            if (preferredSubmit != null && entry.isSubmitType() && preferredSubmit != entry) {
                continue;
            } else if (entry.isDisabled()) {
                continue;
            } else if (key == null) {
                // 2022-11-16: major change! in the past fields with value==null has been skipped
                continue;
            } else if (entry.isType(InputField.InputType.IMAGE)) {
                final String xKey = key + ".x";
                if (this.getInputField(xKey) == null) {
                    final String xValue = entry.getProperty("x", null);
                    if (xValue != null) {
                        ret.add(new FormData(xKey, xValue));
                    }
                }
                final String yKey = key + ".y";
                if (this.getInputField(yKey) == null) {
                    final String yValue = entry.getProperty("y", null);
                    if (yValue != null) {
                        ret.add(new FormData(yKey, yValue));
                    }
                }
            } else if (entry.isType(InputField.InputType.FILE)) {
                final File postFile = entry.getFileToPost();
                if (postFile != null) {
                    ret.add(new FormData(key, postFile.getName(), postFile));
                }
            } else {
                ret.add(new FormData(key, entry.getValue()));
            }
        }
        return ret;
    }

    public String getStringProperty(final String property) {
        return this.keyValueMap.get(property);
    }

    public HashMap<String, String> getVarsMap() {
        final HashMap<String, String> ret = new HashMap<String, String>();
        for (final InputField ipf : this.getInputFields()) {
            /* nameless key-value are not being sent, see FireFox */
            if (ipf.getKey() == null || ipf.isDisabled()) {
                continue;
            } else {
                ret.put(ipf.getKey(), ipf.getValue());
            }
        }
        return ret;
    }

    public boolean hasInputFieldByName(final String name) {
        return this.getInputFieldByName(name) != null;
    }

    private void parse(final String html) {
        this.htmlcode = html;
        // form.baseRequest = requestInfo;
        String header = new Regex(html, "<\\s*form(.*?)>").getMatch(0);
        final List<String[]> headerEntries = new ArrayList<String[]>();
        while (true) {
            // first parse/remove all x='y' and x="y"
            final String[][] quotedAttributes = new Regex(header, "((" + Form.ATTRIBUTE_NAME_PATTERN + ")\\s*=\\s*('|\")(.*?)\\3)").getMatches();
            // then parse/remove remaining x=y
            final String[][] unQuotedAttributes = new Regex(header, "((" + Form.ATTRIBUTE_NAME_PATTERN + ")\\s*=\\s*(.*?)( |$))").getMatches();
            if (quotedAttributes != null && quotedAttributes.length > 0) {
                for (final String[] quotedAttribute : quotedAttributes) {
                    headerEntries.add(new String[] { quotedAttribute[1], quotedAttribute[3] });
                    header = header.replaceFirst(Pattern.quote(quotedAttribute[0]), " ");
                }
            } else if (unQuotedAttributes != null && unQuotedAttributes.length > 0) {
                for (final String[] unQuotedAttribute : unQuotedAttributes) {
                    headerEntries.add(new String[] { unQuotedAttribute[1], unQuotedAttribute[2] });
                    header = header.replaceFirst(Pattern.quote(unQuotedAttribute[0]), " ");
                }
            } else {
                break;
            }
        }
        this.parseHeader(headerEntries.toArray(new String[0][]));
        this.inputfields.addAll(Form.parseInputFields(html));
    }

    private void parseHeader(final String[][] headerEntries) {
        if (headerEntries != null) {
            for (final String[] entry : headerEntries) {
                final String key;
                final String value;
                if (entry.length == 3) {
                    key = entry[0];
                    value = entry[2];
                } else {
                    key = entry[0];
                    value = entry[1];
                }
                final String lowvalue = value.toLowerCase(Locale.ENGLISH);
                if (key.equalsIgnoreCase("action")) {
                    this.setAction(HTMLEntities.unhtmlentities(value));
                } else if (key.equalsIgnoreCase("enctype")) {
                    this.setEncoding(value);
                } else if (key.equalsIgnoreCase("method")) {
                    if (lowvalue.matches(".*post.*")) {
                        this.setMethod(MethodType.POST);
                    } else if (lowvalue.matches(".*get.*")) {
                        this.setMethod(MethodType.GET);
                    } else {
                        /* fallback */
                        this.setMethod(MethodType.POST);
                    }
                } else {
                    this.setProperty(key, value);
                }
            }
        }
    }

    public static final ArrayList<InputField> parseInputFields(final String htmlCode) {
        final ArrayList<InputField> inputfields = new ArrayList<InputField>();
        String escapedHtmlCode = htmlCode;
        // remove comments
        escapedHtmlCode = escapedHtmlCode.replaceAll("(?s)<!--.*?-->", "");
        // remove scripts, speed up parser
        escapedHtmlCode = escapedHtmlCode.replaceAll("(?s)<script[^>]*>.*?</script>", "");
        final List<String> values = new ArrayList<String>();
        final long random = System.nanoTime();
        for (final Pattern valuePattern : new Pattern[] { Pattern.compile("(?s)(?<!\\\\)\"(((?!VALUE-" + random + "-).)*?)(?<!\\\\)\""), Pattern.compile("(?s)(?<!\\\\)'(((?!VALUE-" + random + "-).)*?)(?<!\\\\)'") }) {
            final StringBuffer sb = new StringBuffer();
            final Matcher matcher = valuePattern.matcher(escapedHtmlCode);
            while (matcher.find()) {
                final String value = matcher.group(1);
                if (value != null) {
                    int index = values.indexOf(value);
                    if (index == -1) {
                        index = values.size();
                        values.add(value);
                    }
                    matcher.appendReplacement(sb, "VALUE-" + random + "-" + index + " ");
                }
            }
            if (sb.length() > 0) {
                matcher.appendTail(sb);
                escapedHtmlCode = sb.toString();
            }
        }
        Matcher matcher = Pattern.compile("(?s)(<\\s*(input|textarea|button).*?>)", Pattern.CASE_INSENSITIVE).matcher(escapedHtmlCode);
        while (matcher.find()) {
            final InputField nv = InputField.parse(matcher.group(1), random, values);
            if (nv != null) {
                inputfields.add(nv);
            }
        }
        matcher = Pattern.compile("(?s)(<\\s*select.*?</select>)", Pattern.CASE_INSENSITIVE).matcher(escapedHtmlCode);
        while (matcher.find()) {
            final InputField nv = InputField.parse(matcher.group(1), random, values);
            if (nv != null) {
                inputfields.add(nv);
            }
        }
        return inputfields;
    }

    /**
     * Changes the value of the first filed entry with the key, with new key&value. if no field exists, a new one is created.
     *
     * @param key
     * @param value
     */
    public void put(final String key, final String value) {
        final InputField ipf = this.getInputField(key);
        if (ipf != null) {
            ipf.setValue(value);
        } else {
            this.addInputField(new InputField(key, value));
        }
    }

    /**
     * Removes the first InputField with this key. REMEMBER. There can be more than one file with this key
     *
     * @param key
     * @return
     */
    public void remove(final String key) {
        /*
         * InputField extends HashMap which overrides hashCode, thats why we use iterator here
         */
        final Iterator<InputField> it = this.getInputFields().iterator();
        while (it.hasNext()) {
            final InputField ipf = it.next();
            if (ipf.getKey() == null && key == null) {
                it.remove();
                return;
            }
            if (ipf.getKey() != null && ipf.getKey().equalsIgnoreCase(key)) {
                it.remove();
                return;
            }
        }
    }

    public void setAction(final String action) {
        this.action = action;
    }

    public void setEncoding(final String encoding) {
        this.encoding = encoding;
    }

    public void setMethod(final MethodType method) {
        this.method = method;
    }

    /**
     * Tell the form which submit field to use
     *
     * @param preferredSubmit
     */
    public void setPreferredSubmit(final String preferredSubmit) {
        this.preferredSubmit = null;
        for (final InputField ipf : this.getInputFields()) {
            if (ipf.isSubmitType() && StringUtils.equalsIgnoreCase(ipf.getValue(), preferredSubmit)) {
                if (this.setPreferredSubmit(ipf)) {
                    return;
                }
            }
        }
        // org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().warning("No exact match for submit found! Trying to find
        // best match now!");
        for (final InputField ipf : this.getInputFields()) {
            if (ipf.isSubmitType() && StringUtils.containsIgnoreCase(ipf.getValue(), preferredSubmit)) {
                if (this.setPreferredSubmit(ipf)) {
                    return;
                }
            }
        }
        throw new IllegalArgumentException("No such Submitfield: " + preferredSubmit);
    }

    public boolean setPreferredSubmit(final InputField preferredSubmit) {
        if (preferredSubmit == null || preferredSubmit.isSubmitType() && !preferredSubmit.isDisabled() && this.getInputFields().contains(preferredSubmit)) {
            this.preferredSubmit = preferredSubmit;
            return true;
        } else {
            return true;
        }
    }

    public void setProperty(final String key, final String value) {
        this.keyValueMap.put(key, value);
    }

    @Override
    public String toString() {
        final StringBuilder ret = new StringBuilder();
        ret.append("Action: ");
        ret.append(this.getAction());
        ret.append('\n');
        if (this.method == MethodType.POST) {
            ret.append("Method: POST\n");
        } else if (this.method == MethodType.GET) {
            ret.append("Method: GET\n");
        }
        final InputField preferred = this.getPreferredSubmit();
        for (final InputField ipf : this.getInputFields()) {
            if (preferred == ipf) {
                ret.append("PreferredSubmit:");
            }
            ret.append(ipf.toString());
            ret.append('\n');
        }
        ret.append(this.keyValueMap.toString());
        ret.append('\n');
        ret.append("FormData:");
        ret.append(this.getFormData());
        return ret.toString();
    }

    public boolean removeInputField(InputField f) {
        return f != null && this.getInputFields().remove(f);
    }
}
