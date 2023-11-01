//    jDownloader - Downloadmanager
//    Copyright (C) 2013  JD-Team support@jdownloader.org
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
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import jd.parser.Regex;

import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;

public class InputField {
    public static enum Element {
        BUTTON,
        INPUTFIELD,
        SELECT,
        TEXTAREA,
        UNKNOWN;
        private static ElementType parse(final String element, final String type) {
            ElementType ret = ButtonType.parse(element, type);
            if (ret == null) {
                ret = InputType.parse(element, type);
            }
            if (ret == null) {
                ret = SelectType.parse(element, type);
            }
            if (ret == null) {
                ret = TextAreaType.parse(element, type);
            }
            if (ret == null) {
                ret = new UnknownType(element, type);
            }
            return ret;
        }
    }

    public static class UnknownType implements ElementType {
        private final String element;
        private final String type;

        public UnknownType(final String element, final String type) {
            this.element = element;
            this.type = type;
        }

        @Override
        public String type() {
            return this.type;
        }

        @Override
        public Element getElementType() {
            return Element.UNKNOWN;
        }

        @Override
        public String element() {
            return this.element;
        }

        @Override
        public String toString() {
            return this.getElementType() + ":" + this.element() + "=" + this.type;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            } else if (obj == this) {
                return true;
            } else if (obj instanceof ElementType) {
                ElementType o = (ElementType) obj;
                return this.getElementType().equals(o.getElementType()) && StringUtils.equalsIgnoreCase(this.type(), o.type()) && StringUtils.equalsIgnoreCase(this.element(), o.element());
            } else {
                return false;
            }
        }
    }

    public static interface ElementType {
        public Element getElementType();

        public String type();

        public String element();
    }

    public static InputField parse(String data) {
        return InputField.parse(data, -1, null);
    }

    /**
     * https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
     *
     * @author daniel
     *
     */
    public static enum ButtonType implements ElementType {
        SUBMIT,
        RESET,
        BUTTON;
        private static ButtonType parse(final String element, final String type) {
            if (StringUtils.equalsIgnoreCase(element, "button")) {
                final String typeString = StringUtils.toUpperCaseOrNull(type, Locale.ENGLISH);
                try {
                    /**
                     * The button submits the form data to the server. This is the default if the attribute is not specified for buttons
                     * associated with a <form>, or if the attribute is an empty or invalid value.
                     */
                    return typeString != null ? ButtonType.valueOf(typeString) : ButtonType.SUBMIT;
                } catch (IllegalArgumentException ignore) {
                    return ButtonType.SUBMIT;
                }
            } else {
                return null;
            }
        }

        @Override
        public String toString() {
            return this.getElementType() + ":" + this.type();
        }

        @Override
        public Element getElementType() {
            return Element.BUTTON;
        }

        @Override
        public String type() {
            return this.name();
        }

        @Override
        public String element() {
            return this.getElementType().name();
        }
    }

    public static enum TextAreaType implements ElementType {
        TEXTAREA;
        private static TextAreaType parse(final String element, final String type) {
            if (StringUtils.equalsIgnoreCase(element, "textarea")) {
                return TextAreaType.TEXTAREA;
            } else {
                return null;
            }
        }

        @Override
        public String toString() {
            return this.getElementType() + ":" + this.type();
        }

        @Override
        public Element getElementType() {
            return Element.TEXTAREA;
        }

        @Override
        public String type() {
            return this.name();
        }

        @Override
        public String element() {
            return this.getElementType().name();
        }
    }

    public static enum SelectType implements ElementType {
        SELECT;
        private static SelectType parse(final String element, final String type) {
            if (StringUtils.equalsIgnoreCase(element, "select")) {
                return SelectType.SELECT;
            } else {
                return null;
            }
        }

        @Override
        public String toString() {
            return this.getElementType() + ":" + this.type();
        }

        @Override
        public Element getElementType() {
            return Element.SELECT;
        }

        @Override
        public String type() {
            return this.name();
        }

        @Override
        public String element() {
            return this.getElementType().name();
        }
    }

    public static enum InputType implements ElementType {
        BUTTON,
        CHECKBOX,
        COLOR,
        DATE,
        EMAIL,
        FILE,
        HIDDEN,
        IMAGE,
        MONTH,
        NUMBER,
        PASSWORD,
        RADIO,
        RANGE,
        RESET,
        SEARCH,
        SUBMIT,
        TEL,
        TEXT,
        TIME,
        URL,
        WEEK;
        private static InputType parse(final String element, final String type) {
            if (StringUtils.equalsIgnoreCase(element, "input")) {
                final String typeString = StringUtils.toUpperCaseOrNull(type, Locale.ENGLISH);
                try {
                    return typeString != null ? InputType.valueOf(typeString) : null;
                } catch (IllegalArgumentException ignore) {
                    return null;
                }
            } else {
                return null;
            }
        }

        @Override
        public String toString() {
            return this.getElementType() + ":" + this.type();
        }

        @Override
        public Element getElementType() {
            return Element.INPUTFIELD;
        }

        @Override
        public String type() {
            return this.name();
        }

        @Override
        public String element() {
            return this.getElementType().name();
        }
    }

    protected static InputField parse(String data, final long random, List<String> values) {
        // lets make all quotation marks within 'data' the same. As it's hard to make consistent regex 'matches' when quote marks are not
        // the same, without using lazy regex!.
        final String element = new Regex(data, "\\s*<\\s*(button|input|select|textarea)").getMatch(0);
        if (element == null && DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            // for example textarea|select
            new Exception("update element parser/support:" + data).printStackTrace();
        }
        ArrayList<String> cleanupRegex = new ArrayList<String>();
        cleanupRegex.add("(" + Form.ATTRIBUTE_NAME_PATTERN + "\\s*=\\s*\"[^\"]*\")");
        cleanupRegex.add("(" + Form.ATTRIBUTE_NAME_PATTERN + "\\s*=\\s*'[^']*')");
        for (String reg : cleanupRegex) {
            String results[] = new Regex(data, reg).getColumn(0);
            if (results != null) {
                String quote = new Regex(reg, "(\"|')").getMatch(0);
                for (String result : results) {
                    String cleanedResult = result.replaceFirst(quote, "\\\"").replaceFirst(quote + "$", "\\\"");
                    data = data.replace(result, cleanedResult);
                }
            }
        }
        // no longer have to worry about 'data' with miss matched quotation marks!
        // Note: input form correction for 'checked' and 'disabled' fields.
        // 'disabled' can be for any input field type. Can not be changed! Value shouldn't been submitted with form, .:. null value.
        // 'checked' states current value, can can be re-sent with current request. .:. null value.
        // when 'checked' not present value shouldn't be sent/set within forms input field.
        boolean cbr = false;
        boolean checked = false;
        boolean disabled = false;
        final ArrayList<String> matches = new ArrayList<String>();
        matches.add("\\s?+type\\s?+=\\s?+\"?(checkbox|radio)?\"");
        matches.add("\\s+(checked)\\s?+");
        matches.add("\\s+(disabled)\\s?+");
        for (final String reg : matches) {
            final String result = new Regex(data, reg).getMatch(0);
            if (result != null) {
                if (result.matches("(?i)disabled")) {
                    disabled = true;
                } else if (result.matches("(?i)checked")) {
                    checked = true;
                } else if (result.matches("(?i)checkbox|radio")) {
                    cbr = true;
                }
            }
        }
        final ArrayList<String> input = new ArrayList<String>();
        // end of a " or ' (we corrected above so they are all ") is end of value of key, space before next key name isn't required.
        input.add("[\"']{0,1}\\s*(\\w+)\\s*=\\s*\"(.*?)\"");
        // for key and value without use of " or ', the delimiter needs to be: whitespace, end of inputfield >, and NOT ' or " since they
        // shouldn't be present. Rhetorically should not contain empty value
        // need to exclude values found in URLS, as inputfields!. Also do not overwrite a set entry with secondary regex findings. - raztoki
        // 20150128
        input.add("(?!(?:https?://)?[^\\s]+[/\\w\\-\\.\\?&=]+)[\"']{0,1}\\s*(\\w+)\\s*=\\s*([^\\s\"'>]+)");
        String type = null;
        String key = null;
        String value = null;
        final LinkedHashMap<String, String> properties = new LinkedHashMap<String, String>();
        int selectValues = 0;
        final String valueReplacement = "VALUE-" + random + "-";
        for (String reg : input) {
            String[][] results = new Regex(data, reg).getMatches();
            for (final String[] match : results) {
                if (values != null && match[1].matches("^" + valueReplacement + "\\d+$")) {
                    final int index = Integer.parseInt(match[1].substring(valueReplacement.length()));
                    if (index >= 0 && index < values.size()) {
                        match[1] = values.get(index);
                    }
                }
                if (match[0].equalsIgnoreCase("type") && type == null) {
                    type = match[1];
                } else if (match[0].equalsIgnoreCase("name") && key == null) {
                    key = InputField.formEncoding(match[1]);
                } else if (match[0].equalsIgnoreCase("value") && value == null) {
                    value = InputField.formEncoding(match[1]);
                    properties.put("<INPUTFIELD:TYPEVALUE>", value);
                    if (cbr) {
                        if (checked) {
                            // ret.put("<INPUTFIELD:CHECKED>", "true");
                        } else {
                            properties.put("<INPUTFIELD:CHECKED>", "false");
                            value = InputField.formEncoding(null);
                        }
                    }
                } else if (match[0].equalsIgnoreCase("value") && value != null && "select".equalsIgnoreCase(element)) {
                    if (selectValues == 0) {
                        properties.put("<SELECT:VALUE:0>", value);
                        selectValues++;
                    }
                    properties.put("<SELECT:VALUE:" + selectValues++ + ">", InputField.formEncoding(match[1]));
                } else {
                    properties.put(InputField.formEncoding(match[0]), InputField.formEncoding(match[1]));
                }
            }
        }
        if (disabled) {
            properties.put("<INPUTFIELD:DISABLED>", "true");
        }
        final InputField ret = new InputField(key, value, type, element);
        if (properties.size() > 0) {
            ret.setProperties(properties);
        }
        return ret;
    }

    public boolean isDisabled() {
        final Boolean disabledFlag = this.disabledFlag;
        if (disabledFlag != null) {
            return disabledFlag;
        }
        final Map<String, String> properties = this.properties;
        return properties != null && "true".equals(properties.get("<INPUTFIELD:DISABLED>"));
    }

    public void setDisabled(boolean disabledFlag) {
        this.disabledFlag = disabledFlag;
        if (!disabledFlag && this.properties != null) {
            this.putProperty("<INPUTFIELD:DISABLED>", "false");
        } else if (disabledFlag) {
            this.putProperty("<INPUTFIELD:DISABLED>", "true");
        }
    }

    /**
     * always URLEncode form values parsed from html
     *
     * @param str
     * @return
     */
    private static String formEncoding(final String str) {
        if (str == null) {
            return null;
        } else {
            try {
                return URLEncoder.encode(str, "UTF-8");
            } catch (UnsupportedEncodingException e) {
                return str;
            }
        }
    }

    private String              key;
    private String              value        = null;
    private Map<String, String> properties   = null;
    private final ElementType   elementType;
    private Boolean             disabledFlag = null;

    private void setProperties(Map<String, String> properties) {
        this.properties = properties;
    }

    public InputField(final String key, final String value) {
        this(key, value, InputField.InputType.HIDDEN);
    }

    public InputField(final String key, final String value, ElementType type) {
        this.elementType = type;
        this.key = key;
        if (this.isSubmitType() && value == null) {
            this.value = "";
        } else {
            this.value = value;
        }
        if (this.isType(InputField.InputType.FILE)) {
            this.setDisabled(true);
        }
    }

    protected InputField(final String key, final String value, final String type, final String element) {
        this(key, value, Element.parse(element, type));
    }

    public File getFileToPost() {
        if (!this.isType(InputType.FILE)) {
            throw new IllegalStateException("No file post field:" + this.getElementType());
        } else if (this.value == null) {
            return null;
        } else {
            return new File(this.value);
        }
    }

    public String getKey() {
        return this.key;
    }

    /**
     * so you can rename inputfield without having to delete and re-add inputfield/put and lose properties etc.
     *
     * @since JD2
     * @author raztoki
     * @param key
     */
    public void setKey(final String key) {
        // inputfields require non null value
        if (key != null) {
            this.key = key;
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        } else if (obj != null && obj instanceof InputField) {
            final InputField o = (InputField) obj;
            return StringUtils.equals(this.getKey(), o.getKey()) && StringUtils.equals(this.getValue(), o.getValue()) && this.getElementType().equals(o.getElementType());
        } else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return this.getClass().hashCode();
    }

    public String getProperty(final String key, final String defValue) {
        final String ret;
        final Map<String, String> properties = this.properties;
        if (properties != null) {
            ret = properties.get(key);
        } else {
            ret = null;
        }
        return ret != null ? ret : defValue;
    }

    public ElementType getElementType() {
        return this.elementType;
    }

    public boolean isType(ElementType type) {
        return type != null && type.equals(this.getElementType());
    }

    public boolean isSubmitType() {
        return InputType.SUBMIT.equals(this.getElementType()) || ButtonType.SUBMIT.equals(this.getElementType());
    }

    public String getValue() {
        return this.value;
    }

    public void setFileToPost(final File file) {
        if (!this.isType(InputType.FILE)) {
            throw new IllegalStateException("No file post field:" + this.getElementType());
        } else if (file == null) {
            this.value = null;
        } else {
            this.value = file.getAbsolutePath();
        }
    }

    @Override
    public String toString() {
        final Map<String, String> properties = this.properties;
        if (properties != null) {
            return "Field: " + this.getKey() + "(" + this.getElementType() + ")" + " = " + this.getValue() + " [" + properties.toString() + "]";
        } else {
            return "Field: " + this.getKey() + "(" + this.getElementType() + ")" + " = " + this.getValue() + " []";
        }
    }

    public void setValue(String value2) {
        if (value2 != null) {
            this.value = value2.trim();
        } else {
            this.value = null;
        }
    }

    public boolean containsProperty(String key) {
        final Map<String, String> properties = this.properties;
        return properties != null && properties.containsKey(key);
    }

    /**
     * returns true when Properties of a InputField contains the provided Key and Value
     *
     * @author raztoki
     * @param key
     * @param value
     * @return
     */
    public boolean containsPropertyKeyValue(final String key, final String value) {
        final Map<String, String> properties = this.properties;
        if (properties != null) {
            if (this.containsProperty(key) && value.equals(properties.get(key))) {
                return true;
            }
        }
        return false;
    }

    public void putProperty(String key, String value) {
        if (this.properties == null) {
            this.properties = new LinkedHashMap<String, String>();
        }
        this.properties.put(key, value);
    }
}