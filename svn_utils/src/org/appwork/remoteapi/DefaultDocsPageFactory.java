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
package org.appwork.remoteapi;

import java.io.IOException;
import java.lang.annotation.Annotation;
import java.lang.ref.SoftReference;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.remoteapi.annotations.APIParameterNames;
import org.appwork.remoteapi.annotations.ApiDoc;
import org.appwork.remoteapi.annotations.ApiMethodName;
import org.appwork.remoteapi.annotations.ApiSessionRequired;
import org.appwork.remoteapi.annotations.HiddenForHelpDocs;
import org.appwork.remoteapi.exceptions.BasicRemoteAPIException;
import org.appwork.storage.Storable;
import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.storage.simplejson.mapper.ClassCache;
import org.appwork.storage.simplejson.mapper.Getter;
import org.appwork.storage.simplejson.mapper.Setter;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.IO;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.net.HTTPHeader;

public class DefaultDocsPageFactory extends InterfaceHandler<Object> {
    /**
     *
     */
    public static final String       AUTHENTICATION = "Authentication";
    final protected RemoteAPI        api;
    private Method                   help;
    private SoftReference<byte[]>    cachedBytes;
    private HashMap<Object, Integer> reservedAnchorIds;
    private HashSet<Object>          dupeCheck;

    public DefaultDocsPageFactory(RemoteAPI api) throws SecurityException, NoSuchMethodException {
        super();
        this.api = api;
    }

    /**
     * @param returnType
     * @return
     */
    protected boolean isAddObjectToHelp(Type returnType) {
        if (Storable.class.isAssignableFrom((Class<?>) returnType)) {
            return true;
        }
        return false;
    }

    @Override
    public Object invoke(Method method, Object[] parameters) throws IllegalArgumentException, IllegalAccessException, InvocationTargetException {
        return help.invoke(this, parameters);
    }

    @Override
    public Method getMethod(String methodName, int length) {
        return help;
    }

    @Override
    public void parse() throws ParseException {
    }

    /**
     * @param helpMethod
     * @throws SecurityException
     * @throws NoSuchMethodException
     */
    public void link(HelpMethod helpMethod) throws NoSuchMethodException, SecurityException {
        registerExtraMethod("help", help = getClass().getMethod("help", new Class[] { RemoteAPIRequest.class, RemoteAPIResponse.class }));
    }

    public void help(RemoteAPIRequest request, RemoteAPIResponse response) {
        try {
            byte[] bytes = null;
            bytes = cachedBytes != null ? this.cachedBytes.get() : null;
            if (bytes == null || true) {
                String txt = build();
                bytes = txt.getBytes("UTF-8");
                this.cachedBytes = new SoftReference<byte[]>(bytes);
            }
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "text/html"));
            ContentSecurityHeader ret = new ContentSecurityHeader();
            ret.addDefaultSrc("'self'");
            ret.addScriptSrc("'unsafe-inline'");
            ret.addStyleSrc("'unsafe-inline'");
            ret.addImgSrc("data:");
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_SECURITY_POLICY, ret.toHeaderString()));
            response.setResponseCode(ResponseCode.SUCCESS_OK);
            response.sendBytes(request, bytes);
        } catch (IOException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param requiredTypes
     *            TODO
     * @param returnType
     * @param html
     *            TODO
     * @param addExtends
     *            TODO
     * @return
     */
    private String typeToString(HashSet<Type> requiredTypes, final Type returnType, boolean html, boolean addExtends) {
        Type currentType = returnType;
        if (currentType instanceof Class && ((Class) currentType).isArray()) {
            currentType = ((Class) currentType).getComponentType();
        }
        if (currentType instanceof Class) {
            final Class cls = (Class) currentType;
            if (BasicRemoteAPIException.class.isAssignableFrom((Class) currentType)) {
                try {
                    BasicRemoteAPIException inst = null;
                    try {
                        Constructor c = cls.getDeclaredConstructor(new Class[] {});
                        c.setAccessible(true);
                        inst = (BasicRemoteAPIException) c.newInstance(new Object[] {});
                    } catch (NoSuchMethodException e) {
                        Constructor c = cls.getDeclaredConstructor(new Class[] { String.class });
                        c.setAccessible(true);
                        inst = (BasicRemoteAPIException) c.newInstance(new Object[] { "" });
                    }
                    if (inst.getData() != null) {
                        return "ResponseCode " + inst.getCode().getCode() + " (" + inst.getCode().getDescription() + ")" + "; Type:" + inst.getType() + "; Description: " + inst.getData();
                    } else {
                        return "ResponseCode " + inst.getCode().getCode() + " (" + inst.getCode().getDescription() + ")" + "; Type:" + inst.getType() + "";
                    }
                } catch (Throwable e) {
                    System.out.println("Unsupported Exception type: " + returnType + "|" + currentType);
                }
            }
            String ret = modifyName(((Class) returnType).getSimpleName());
            if (addExtends) {
                Type genSuper = ((Class) returnType).getGenericSuperclass();
                if (genSuper instanceof ParameterizedType) {
                    if (genSuper.toString().startsWith("org.appwork.")) {
                        ret += " extends " + typeToString(requiredTypes, genSuper, html, addExtends);
                    }
                }
            }
            if (requiredTypes != null && requiredTypes.contains(currentType)) {
                Integer aid = getReservedID(currentType);
                if (html) {
                    if (aid == null) {
                        if (((Class) currentType).isEnum()) {
                            return htmlEncode("Enum:" + ret);
                        } else {
                            return htmlEncode("Object:" + ret);
                        }
                    } else {
                        return "<a href='#tag_" + aid + "'>" + htmlEncode(ret) + "</a>";
                    }
                } else {
                    if (((Class) currentType).isEnum()) {
                        return "Enum:" + ret;
                    } else {
                        return "Object:" + ret;
                    }
                }
            }
            return ret;
        } else if (returnType instanceof ParameterizedType) {
            Type raw = ((ParameterizedType) returnType).getRawType();
            String ret = typeToString(requiredTypes, raw, html, addExtends);
            ret += html ? htmlEncode("<") : "<";
            boolean first = true;
            for (Type subtype : ((ParameterizedType) returnType).getActualTypeArguments()) {
                if (!first) {
                    ret += ", ";
                }
                first = false;
                ret += typeToString(requiredTypes, subtype, html, addExtends);
            }
            ret += html ? htmlEncode(">") : ">";
            return ret;
        } else if (returnType instanceof TypeVariable) {
            return ((TypeVariable) returnType).getName();
        }
        return String.valueOf(returnType);
    }

    /**
     * @param simpleName
     * @return
     */
    private String modifyName(String simpleName) {
        // if (true) {
        // return simpleName;
        // }
        simpleName = simpleName.replaceAll("StorableV?\\d*$", "");
        simpleName = simpleName.replaceAll("API$", "");
        if ("HashMap".equals(simpleName)) {
            return "Map";
        } else if ("HashSet".equals(simpleName)) {
            return "UnorderedList";
        } else if ("LinkedHashSet".equals(simpleName)) {
            return "List";
        } else if ("LinkedHashMap".equals(simpleName)) {
            return "Map";
        } else if ("ArrayList".equals(simpleName)) {
            return "List";
        } else if ("LinkedList".equals(simpleName)) {
            return "List";
        } else if ("boolean".equals(simpleName)) {
            return "boolean";
        } else if ("Boolean".equals(simpleName)) {
            return "boolean|null";
        } else {
            return simpleName;
        }
    }

    public class HTMLStringBuilder {
        private StringBuilder sb;

        /*
         * (non-Javadoc)
         *
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {            
            return sb.toString();
        }

        /**
         *
         */
        public HTMLStringBuilder() {
            this.sb = new StringBuilder();
        }

        /**
         * @param string
         * @return
         */
        public HTMLStringBuilder append(String string) {
            sb.append(string);
            sb.append("\r\n");
            return this;
        }

        public HTMLStringBuilder comment(String string) {
            sb.append("\r\n");
            sb.append("<!--" + string + "-->");
            sb.append("\r\n");
            return this;
        }

        /**
         * @param string
         * @return
         */
        public HTMLStringBuilder appendRaw(String string) {
            sb.append(string);
            return this;
        }
    }

    public static String htmlEncode(String s) {
        StringBuffer out = new StringBuffer();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == ' ') {
                out.append("&nbsp;");
                continue;
            }
            if (c > 127 || c == '"' || c == '<' || c == '>') {
                out.append("&#" + (int) c + ";");
            } else {
                out.append(c);
            }
        }
        return out.toString();
    }

    public String build() {
        final HTMLStringBuilder content = new HTMLStringBuilder();
        HTMLStringBuilder nav = new HTMLStringBuilder();
        Template template = readHTMLTemplate();
        addTitle(template);
        ArrayList<InterfaceHandler<RemoteAPIInterface>> handlerList = createInterfaceHandlerList();
        Collections.sort(handlerList, new Comparator<InterfaceHandler<RemoteAPIInterface>>() {
            @Override
            public int compare(InterfaceHandler<RemoteAPIInterface> o1, InterfaceHandler<RemoteAPIInterface> o2) {
                return o1.getNamespace().toLowerCase(Locale.ENGLISH).compareTo(o2.getNamespace().toLowerCase(Locale.ENGLISH));
            }
        });
        nav.append("<h1 class='sidebarheader'>");
        nav.append("<a style='padding: 0;' href='#methods'>Methods</a>");
        nav.append("</h1>");
        nav.append("<ul>");
        reservedAnchorIds = new HashMap<Object, Integer>();
        dupeCheck = new HashSet<Object>();
        HashSet<Type> requiredTypes = new HashSet<Type>();
        for (InterfaceHandler<RemoteAPIInterface> handler : handlerList) {
            HashSet<Method> methodSet = new HashSet<Method>(handler.getMethodsMap().values());
            if (methodSet.size() == 0) {
                continue;
            }
            ArrayList<Method> methods = new ArrayList<Method>();
            for (Method m : methodSet) {
                if (m.getAnnotation(HiddenForHelpDocs.class) != null) {
                    continue;
                }
                methods.add(m);
            }
            if (methods.size() == 0 || methods.get(0).getDeclaringClass().getAnnotation(HiddenForHelpDocs.class) != null) {
                continue;
            }
            if (!isInterfaceVisible(methods.get(0).getDeclaringClass())) {
                continue;
            }
            Collections.sort(methods, new Comparator<Method>() {
                @Override
                public int compare(Method o1, Method o2) {
                    return o1.getName().toLowerCase(Locale.ENGLISH).compareTo(o2.getName().toLowerCase(Locale.ENGLISH));
                }
            });
            nav.append("<li class='namespace'>");
            addMenuEntry(content, nav, 1, "Namespace /" + handler.getNamespace(), null, htmlEncode(methods.get(0).getDeclaringClass().getName()));
            nav.append("<ul  class='typelist'>");
            // nav.append("<li>");
            // addMenuEntry(content, nav, 2, "Methods");
            // nav.append("<ul class='nav-methodname has-sub'>");
            collectRequiredTypes(requiredTypes);
            for (final Method m : methods) {
                String name = m.getName();
                final ApiMethodName methodname = m.getAnnotation(ApiMethodName.class);
                if (methodname != null) {
                    name = methodname.value();
                }
                // sb.append("\r\n Description: ");
                // final HashMap<Type, Integer> map = new HashMap<Type, Integer>();
                String call = StringUtils.isEmpty(handler.getNamespace()) ? "/" + name : "/" + handler.getNamespace() + "/" + name;
                String header = "";
                int count = 0;
                Type returnType = m.getGenericReturnType();
                requiredTypes.addAll(getTypes(returnType));
                for (Type rt : requiredTypes) {
                    putReservedID(rt);
                }
                final APIParameterNames anno = m.getAnnotation(APIParameterNames.class);
                final String[] parameterNames = anno == null ? null : anno.value();
                for (int i = 0; i < m.getGenericParameterTypes().length; i++) {
                    if (m.getParameterTypes()[i] == RemoteAPIRequest.class || m.getParameterTypes()[i] == RemoteAPIResponse.class) {
                        continue;
                    }
                    if (count > 0) {
                        call += "&";
                        header += ", ";
                    } else {
                        call += "?";
                        header += "[";
                    }
                    count++;
                    final Class<?> paramClass = m.getParameterTypes()[i];
                    requiredTypes.addAll(getTypes(paramClass));
                    for (Type rt : requiredTypes) {
                        putReservedID(rt);
                    }
                    String paramName = typeToString(requiredTypes, paramClass, false, false);
                    if (parameterNames != null) {
                        if (i < parameterNames.length) {
                            call += new Regex(parameterNames[i], "^(\\S+)").getMatch(0);
                            header += parameterNames[i];
                        } else {
                            System.out.println("FIXME: Invalid APIParameterNames Annotation for Method:" + m.getName() + "|Class:" + m.getDeclaringClass().getName());
                            call += paramName;
                            header += paramName;
                        }
                    } else {
                        call += paramName;
                        header += paramName;
                    }
                }
                if (header.contains("[")) {
                    header += "]";
                }
                nav.append("<li class='type-method'>");
                for (Type rt : requiredTypes) {
                    putReservedID(rt);
                }
                int id = this.count.incrementAndGet();
                int i = 3;
                content.append("<a class='anchor' id='tag_" + id + "'></a>" + "<h3 class='main-type-method'>" + "<span  style=''>" + htmlEncode(name) + "</span><span  style='position: absolute;right: 20px;'>Parameter: " + count + "</span>" + "</h" + i + ">");
                nav.append("<div class='menu-h" + i + "'><a href=\"#tag_" + id + "\"><span class='menu-content-h" + i + " tooltip' >" + "<span  style=''>" + htmlEncode(name) + "</span><span class='tooltiptext'>" + htmlEncode("Parameter: " + count + " " + htmlEncode(header)) + "</span>" + "</span></a></div>");
                nav.append("</li>");
                content.append("<ul class='keyvalue'>");
                if (m.getAnnotation(Deprecated.class) != null) {
                    content.append("<li><p class='deprecated'>DEPRECATED Method. This method will be removed soon. DO NOT USE IT!</p></li>");
                }
                if (m.getAnnotation(ApiSessionRequired.class) != null || m.getDeclaringClass().getAnnotation(ApiSessionRequired.class) != null) {
                    addKeyValueEntry(content, AUTHENTICATION, "required");
                }
                HashMap<String, HashSet<String>> map = new HashMap<String, HashSet<String>>();
                for (Annotation a : m.getAnnotations()) {
                    try {
                        Method docKey = a.getClass().getDeclaredMethod("docKey", new Class[] {});
                        if (docKey != null) {
                            String docKeyValue = String.valueOf(docKey.invoke(a, new Object[] {}));
                            if (StringUtils.isNotEmpty(docKeyValue)) {
                                String docValueValue = "";
                                Method docValue = a.getClass().getDeclaredMethod("docValue", new Class[] {});
                                if (docKey != null) {
                                    docValueValue = String.valueOf(docValue.invoke(a, new Object[] {}));
                                }
                                if (StringUtils.isEmpty(docValueValue)) {
                                    docValueValue = "";
                                }
                                HashSet<String> lst = map.get(docKeyValue);
                                if (lst == null) {
                                    map.put(docKeyValue, lst = new HashSet<String>());
                                }
                                lst.add(docValueValue);
                            }
                        }
                    } catch (Throwable e) {
                        // e.printStackTrace();
                    }
                }
                ArrayList<String> keys = new ArrayList<String>(map.keySet());
                Collections.sort(keys);
                for (String key : keys) {
                    ArrayList<String> lst = new ArrayList<String>(map.get(key));
                    for (int ii = 0; ii < lst.size(); ii++) {
                        if (lst.size() > 0 && StringUtils.isEmpty(lst.get(ii))) {
                            continue;
                        }
                        if (ii == 0) {
                            addKeyValueEntry(content, key, lst.get(ii));
                        } else {
                            addKeyValueEntry(content, "", lst.get(ii));
                        }
                    }
                }
                count = 0;
                for (i = 0; i < m.getGenericParameterTypes().length; i++) {
                    if (m.getParameterTypes()[i] == RemoteAPIRequest.class || m.getParameterTypes()[i] == RemoteAPIResponse.class) {
                        continue;
                    }
                    count++;
                    final Class<?> paramClass = m.getParameterTypes()[i];
                    // Integer num = map.get(paramClass);
                    // if (num == null) {
                    // map.put(paramClass, 0);
                    // num = 0;
                    // }
                    // num++;
                    String paramNameHTML = typeToString(requiredTypes, paramClass, true, false);
                    addParameter(content, count, i, parameterNames, paramNameHTML);
                    // map.put(paramClass, num);
                }
                String doc = getDocByMethod(m);
                if (StringUtils.isNotEmpty(doc)) {
                    addKeyValueEntry(content, "Description", doc);
                }
                addKeyValueEntry(content, "Call", call);
                String returnStr = typeToString(requiredTypes, returnType, true, false);
                if (!"void".equalsIgnoreCase(returnStr)) {
                    content.append("<li class='keyvalueentry'>");
                    content.append("<span class='key'>" + htmlEncode("Return type") + "</span>").append("<span class='value'>" + returnStr + "</span>");
                    content.append("</li>");
                }
                appendPossibleErrors(requiredTypes, content, m);
                content.append("</ul>");
            }
            // // close cat 3 methodslist
            // nav.append("</ul>");
            // nav.append("</li>");
            // close cat 2 methods header
            nav.comment("End of Methods");
            nav.append("</ul>");
            nav.append("</li>");
        } // close cat 1 handler
        nav.append("</ul>");
        if (requiredTypes.size() > 0) {
            nav.append("<h1 class='sidebarheader'>");
            nav.append("<a style='padding: 0;' href='#objects'>Structures, Objects &amp; Enums</a>");
            nav.append("</h1>");
            nav.append("<ul>");
            ArrayList<Type> lst = new ArrayList<Type>(requiredTypes);
            Collections.sort(lst, new Comparator<Type>() {
                @Override
                public int compare(Type o1, Type o2) {
                    int ret = CompareUtils.compareBoolean(o2 instanceof Class && ((Class) o2).isEnum(), o1 instanceof Class && ((Class) o1).isEnum());
                    if (ret != 0) {
                        return ret;
                    }
                    return typeToString(null, o1, false, false).compareTo(typeToString(null, o2, false, false));
                }
            });
            boolean header = false;
            for (Type enumClass : lst) {
                if (enumClass instanceof Class && ((Class) enumClass).isEnum()) {
                    if (!dupeCheck.add(enumClass)) {
                        continue;
                    }
                    if (!header) {
                        nav.comment("Start of enums");
                        nav.append("<li class='namespace'>");
                        addMenuEntry(content, nav, 1, "Enums &amp; Constants", null, null);
                        nav.append("<ul  class='typelist'>");
                        header = true;
                    }
                    nav.append("<li class='type-enum'>");
                    addMenuEntry(content, nav, 3, typeToString(null, enumClass, false, false), getReservedID(enumClass), htmlEncode(enumClass.toString()));
                    nav.append("</li>");
                    final Class<? extends Enum<?>> num = (Class<? extends Enum<?>>) enumClass;
                    int i = 0;
                    content.append("<ul class='enums'>");
                    for (Enum<?> c : num.getEnumConstants()) {
                        String docs = "";
                        Field fi;
                        boolean deprecated = false;
                        try {
                            fi = num.getField(c.name());
                            if (fi.getAnnotation(HiddenForHelpDocs.class) != null) {
                                continue;
                            }
                            if (fi.getAnnotation(Deprecated.class) != null) {
                                deprecated = true;
                            }
                            String doc = getDocByField(fi);
                            if (StringUtils.isNotEmpty(doc)) {
                                docs = doc;
                                appendDocs(content, doc);
                            }
                        } catch (NoSuchFieldException e) {
                        } catch (Throwable e) {
                            e.printStackTrace();
                        }
                        if (LabelInterface.class.isAssignableFrom(num)) {
                            if (docs.length() > 0) {
                                docs += "\r\n";
                            }
                            docs += ((LabelInterface) c).getLabel();
                            appendDocs(content, ((LabelInterface) c).getLabel());
                        }
                        content.append("<li>");
                        if (StringUtils.isNotEmpty(docs)) {
                            content.append("<span class='tooltip'>" + c.name() + "<span class='tooltiptext'>" + htmlEncode(docs) + "</span></span>");
                        } else {
                            if (deprecated) {
                                content.append(c.name() + " <span class='deprecated'>DEPRECATED! This field will be removed soon. DO NOT USE IT!</span>");
                            } else {
                                content.append(c.name());
                            }
                        }
                        content.append("</li>");
                    }
                    content.append("</ul>");
                    continue;
                }
            }
            if (header) {
                // end enums list
                nav.comment("End of enums");
                nav.append("</ul>");
                // nav.append("</li>");
            }
            header = false;
            for (Type enumClass : lst) {
                if (enumClass instanceof Class && !((Class) enumClass).isEnum()) {
                    if (!dupeCheck.add(enumClass)) {
                        continue;
                    }
                    if (!header) {
                        nav.comment("Start of objects");
                        nav.append("<li class='namespace'>");
                        addMenuEntry(content, nav, 1, "Structures &amp; Objects", null, null);
                        nav.append("<ul  class='typelist'>");
                        header = true;
                    }
                    nav.append("<li class='type-object'>");
                    Integer aid = getReservedID(enumClass);
                    int i = 3;
                    int id = aid != null ? aid : count.incrementAndGet();
                    content.append("<div class='header" + i + "'><a class='anchor' id='tag_" + id + "'>" + "</a>" + "<h" + i + " class='tooltip'>" + htmlEncode(typeToString(null, enumClass, false, true)) + "<span class='tooltiptext'>" + htmlEncode(enumClass.toString()) + "</span>" + "</h" + i + "></div>");
                    nav.append("<div class='menu-h" + i + "'><a href=\"#tag_" + id + "\">" + htmlEncode(typeToString(null, enumClass, false, false)) + "</a></div>");
                    nav.append("</li>");
                    ClassCache cc;
                    try {
                        cc = ClassCache.getClassCache((enumClass));
                        content.append("<pre><code class=\"javascript\">");
                        try {
                            Method exampleMethod = ((Class) enumClass).getMethod("createHelpText", new Class[] {});
                            exampleMethod.setAccessible(true);
                            String ex = (String) exampleMethod.invoke(null, new Object[] {});
                            appendCodeComments(content, ex);
                        } catch (NoSuchMethodException e) {
                        } catch (Throwable e1) {                            
                            e1.printStackTrace();
                        }
                        content.appendRaw("          my" + typeToString(null, enumClass, false, false) + " = ");
                        content.appendRaw("\r\n                  {");
                        HashSet<String> keySet = new HashSet<String>();
                        int widestKey = 0;

                        for (String sg : cc.getKeys()) {
                            keySet.add(sg);
                            widestKey = Math.max(sg.length(), widestKey);
                        }
                        ArrayList<String> keyList = new ArrayList<String>(keySet);
                        Collections.sort(keyList);
                        boolean first = true;
                        for (String key : keyList) {
                            if (!first) {
                                content.appendRaw(",");
                            }
                            first = false;
                            Type type = null;
                            String desc = "";
                            Getter g = cc.getGetter(key);
                            Setter s = cc.getSetter(key);
                            if (g != null) {
                                type = g.getMethod().getGenericReturnType();
                                String d = getDocByMethod(g.getMethod());
                                if (d != null) {
                                    desc = d;
                                }
                            }
                            if (s != null) {
                                type = s.getMethod().getGenericParameterTypes()[0];
                                String d = getDocByMethod(s.getMethod());
                                if (d != null) {
                                    if (desc.length() > 0) {
                                        desc += "\r\n";
                                    }
                                    desc += d;
                                }
                            }
                            appendCodeComments(content, desc);
                            if (StringUtils.isNotEmpty(desc)) {
                                content.appendRaw("\r\n                    <span class='tooltip'>" + StringUtils.fillPost("\"" + key + "\"", " ", widestKey + 2) + " = (" + typeToString(requiredTypes, type, true, false) + ")<span class='tooltiptext'>" + desc + "</span></span>");
                            } else {
                                content.appendRaw("\r\n                    " + StringUtils.fillPost("\"" + key + "\"", " ", widestKey + 2) + " = (" + typeToString(requiredTypes, type, true, false) + ")");
                            }
                        }
                        content.appendRaw("\r\n                  }");
                        content.append("</code></pre>");
                        continue;
                    } catch (Throwable e1) {                        
                        e1.printStackTrace();
                    }
                }
            }
            if (header) {
                // ends objects list
                nav.comment("End of enums");
                nav.append("</ul>");
                // nav.append("</li>");
            }
            nav.append("</ul>");
        }
        addNavigation(nav, template);
        addContent(content, template);
        return template.build();
    }

    protected ArrayList<InterfaceHandler<RemoteAPIInterface>> createInterfaceHandlerList() {
        return new ArrayList<InterfaceHandler<RemoteAPIInterface>>(api.getHandlerMap().values());
    }

    /**
     * @param type
     * @param method
     * @param enumClass
     * @return
     */
    private Type resolveActualType(TypeVariable type, Method method, Type enumClass) {
        String name = type.getName();
        Type genSuper = ((Class) enumClass).getGenericSuperclass();
        Type sClass = (((Class) enumClass).getSuperclass()).getGenericSuperclass();
        // Type[] genInt = sClass.getGenericInterfaces();
        return null;
    }

    /**
     * @param fi
     * @return
     */
    protected String getDocByField(Field fi) {
        ApiDoc docAnno = fi.getAnnotation(ApiDoc.class);
        if (docAnno != null) {
            return docAnno.value();
        }
        return null;
    }

    /**
     * @param m
     * @return
     */
    protected String getDocByMethod(Method m) {
        final ApiDoc an = m.getAnnotation(ApiDoc.class);
        if (an != null) {
            return an.value();
        }
        return null;
    }

    /**
     * @param declaringClass
     * @return
     */
    protected boolean isInterfaceVisible(Class<?> iClass) {        
        return true;
    }

    protected void appendCodeComments(final HTMLStringBuilder content, String desc) {
        if (StringUtils.isNotEmpty(desc)) {
            content.appendRaw("\r\n    /**");
            for (String ss : desc.split("[\r\n]+")) {
                content.appendRaw("\r\n     * " + ss);
            }
            content.appendRaw("\r\n     */\r\n");
        }
    }

    /**
     * @param requiredTypes
     */
    protected void collectRequiredTypes(HashSet<Type> requiredTypes) {        
    }

    protected void appendPossibleErrors(HashSet<Type> requiredTypes, final HTMLStringBuilder content, final Method m) {
        Type[] exs = m.getGenericExceptionTypes();
        if (exs != null && exs.length > 0) {
            boolean f = true;
            for (Type e : exs) {
                f = appendPossibleError(requiredTypes, content, f, e);
            }
        }
    }

    protected boolean appendPossibleError(HashSet<Type> requiredTypes, final HTMLStringBuilder sb, boolean f, Type e) {
        String key = "Possible Error(s)";
        if (!f) {
            key = "&nbsp;";
        } else {
            f = false;
        }
        sb.append("<li class='keyvalueentry'>");
        sb.append("<span class='key'>" + htmlEncode(key) + "</span>").append("<span class='value'>" + typeToString(requiredTypes, e, true, false) + "</span>");
        sb.append("</li>");
        return f;
    }

    protected void addContent(final HTMLStringBuilder content, Template template) {
        template.put("content", content.toString());
    }

    protected void addNavigation(HTMLStringBuilder nav, Template template) {
        template.put("navigation", nav.toString());
    }

    protected void addTitle(Template template) {
        template.put("title", "API Documentation");
    }

    protected void putReservedID(Type rt) {
        if (reservedAnchorIds.get(rt) == null) {
            reservedAnchorIds.put(rt, this.count.incrementAndGet());
            System.out.println("PUT reserved: " + rt + " - " + reservedAnchorIds.get(rt));
        }
    }

    protected Integer getReservedID(Type enumClass) {
        System.out.println("Get reserved: " + enumClass + " - " + reservedAnchorIds.get(enumClass));
        return reservedAnchorIds.get(enumClass);
    }

    /**
     * @param content
     * @param nav
     * @param i
     * @param string
     */
    private AtomicInteger count = new AtomicInteger(0);

    private void addMenuEntry(HTMLStringBuilder content, HTMLStringBuilder nav, int i, String string, Integer aid, String tooltipHTML) {
        int id = aid != null ? aid : count.incrementAndGet();
        if (StringUtils.isNotEmpty(tooltipHTML)) {
            content.append("<div class='header" + i + "'><a class='anchor' id='tag_" + id + "'>" + "</a>" + "<h" + i + " class='tooltip'>" + htmlEncode(string) + "<span class='tooltiptext'>" + tooltipHTML + "</span>" + "</h" + i + "></div>");
        } else {
            content.append("<div class='header" + i + "'><a class='anchor' id='tag_" + id + "'>" + "</a>" + "<h" + i + " class=''>" + htmlEncode(string) + "</h" + i + "></div>");
        }
        nav.append("<div class='menu-h" + i + "'><a href=\"#tag_" + id + "\">" + htmlEncode(string) + "</a></div>");
    }

    /**
     * @param sb
     * @param value
     */
    protected void appendDocs(HTMLStringBuilder sb, String value) {
        if (StringUtils.isNotEmpty(value)) {
            sb.append("\r\n<span class='comment'>" + value.replaceAll("[\r\n]{1,2}", "</br>") + "</span>");
        }
    }

    /**
     * @param sb
     * @param count
     * @param i
     * @param parameterNames
     * @param paramName
     */
    private void addParameter(HTMLStringBuilder sb, int count, int i, String[] parameterNames, String paramNameHTML) {
        String key = null;
        String value = null;
        if (count == 1) {
            key = "Parameter";
        } else {
            key = "&nbsp;";
        }
        if (parameterNames != null) {
            if (i < parameterNames.length) {
                value = htmlEncode(count + " - " + parameterNames[i] + " (") + paramNameHTML + htmlEncode(")");
            } else {
                value = htmlEncode(count + " - ") + paramNameHTML;
            }
        } else {
            value = htmlEncode(count + " - ") + paramNameHTML;
        }
        sb.append("<li class='keyvalueentry'>");
        sb.append("<span class='key'>" + htmlEncode(key) + "</span>").append("<span class='value'>" + value + "</span>");
        sb.append("</li>");
    }

    /**
     * @param sb
     * @param string
     * @param string2
     */
    private void addKeyValueEntry(HTMLStringBuilder sb, String key, String value) {
        sb.append("<li class='keyvalueentry'>");
        sb.append("<span class='key'>" + htmlEncode(key) + "</span>").append("<span class='value'>" + htmlEncode(value) + "</span>");
        sb.append("</li>");
    }

    /**
     * @return
     */
    protected Template readHTMLTemplate() {
        try {
            String ret = IO.readURLToString(InterfaceHandler.class.getResource("html/docs.html"));
            Template t = new Template(ret);
            t.put("style", "<style>\r\n" + readCSSStyle() + "\r\n</style>");
            t.put("highlight.js", IO.readURLToString(InterfaceHandler.class.getResource("html/highlight.js")));
            t.put("highlight.js.css", IO.readURLToString(InterfaceHandler.class.getResource("html/highlight.js.css")));
            return t;
        } catch (IOException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @return
     */
    protected String readCSSStyle() {
        try {
            return IO.readURLToString(InterfaceHandler.class.getResource("html/style.css"));
        } catch (IOException e) {
            throw new WTFException(e);
        }
    }

    /**
     * @param paramClass
     * @return
     */
    private List<Type> getTypes(Type returnType) {
        return getTypes(returnType, new HashSet<Type>());
    }

    private List<Type> getTypes(Type returnType, HashSet<Type> dupe) {
        final ArrayList<Type> ret = new ArrayList<Type>();
        if (!dupe.add(returnType)) {
            return ret;
        } else if (returnType instanceof Class) {
            if (((Class) returnType).isEnum()) {
                ret.add(returnType);
                return ret;
            } else if (((Class) returnType).isArray()) {
                final Class componentType = ((Class) returnType).getComponentType();
                ret.addAll(getTypes(componentType, dupe));
                return ret;
            } else if (!isAddObjectToHelp(returnType)) {
                return ret;
            }
            ret.add(returnType);
            ClassCache cc;
            try {
                cc = ClassCache.getClassCache(returnType);
                for (Getter g : cc.getAllGetter()) {
                    ret.addAll(getTypes(g.getMethod().getGenericReturnType(), dupe));
                }
                for (Setter g : cc.getAllSetter()) {
                    ret.addAll(getTypes(g.getMethod().getGenericParameterTypes()[0], dupe));
                }
            } catch (Throwable e) {
                LogV3.log(e);
            }
        } else if (returnType instanceof ParameterizedType) {
            final Type raw = ((ParameterizedType) returnType).getRawType();
            ret.addAll(getTypes(raw, dupe));
            for (Type subtype : ((ParameterizedType) returnType).getActualTypeArguments()) {
                ret.addAll(getTypes(subtype, dupe));
            }
        }
        return ret;
    }
}