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
package org.appwork.remoteapi;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.zip.Deflater;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.GZIPOutputStream;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.net.protocol.http.HTTPConstants;
import org.appwork.net.protocol.http.HTTPConstants.ResponseCode;
import org.appwork.remoteapi.annotations.AllowResponseAccess;
import org.appwork.remoteapi.annotations.ApiAuthLevel;
import org.appwork.remoteapi.annotations.ApiNamespace;
import org.appwork.remoteapi.annotations.ApiSessionRequired;
import org.appwork.remoteapi.exceptions.ApiCommandNotAvailable;
import org.appwork.remoteapi.exceptions.AuthException;
import org.appwork.remoteapi.exceptions.BadParameterException;
import org.appwork.remoteapi.exceptions.BasicRemoteAPIException;
import org.appwork.remoteapi.exceptions.InternalApiException;
import org.appwork.remoteapi.exceptions.RemoteAPIException;
import org.appwork.remoteapi.responsewrapper.DataObject;
import org.appwork.storage.JSonMapperException;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.Regex;
import org.appwork.utils.net.ChunkedOutputStream;
import org.appwork.utils.net.CountingOutputStream;
import org.appwork.utils.net.HTTPHeader;
import org.appwork.utils.net.NullOutputStream;
import org.appwork.utils.net.httpserver.HttpConnection.HttpConnectionType;
import org.appwork.utils.net.httpserver.handler.HttpRequestHandler;
import org.appwork.utils.net.httpserver.requests.GetRequest;
import org.appwork.utils.net.httpserver.requests.HTTPBridge;
import org.appwork.utils.net.httpserver.requests.HttpRequest;
import org.appwork.utils.net.httpserver.requests.KeyValuePair;
import org.appwork.utils.net.httpserver.requests.PostRequest;
import org.appwork.utils.net.httpserver.responses.HttpResponse;
import org.appwork.utils.reflection.Clazz;

/**
 * @author daniel
 *
 */
public class RemoteAPI implements HttpRequestHandler {
    public static class RemoteAPIMethod {
        protected final InterfaceHandler<?> interfaceHandler;
        protected final String              methodName;
        protected final String              nameSpace;

        public RemoteAPIMethod(final String nameSpace, final InterfaceHandler<?> interfaceHandler, final String methodName) {
            this.nameSpace = nameSpace;
            this.interfaceHandler = interfaceHandler;
            this.methodName = methodName;
        }

        public final InterfaceHandler<?> getInterfaceHandler() {
            return this.interfaceHandler;
        }

        public final String getMethodName() {
            return this.methodName;
        }

        public final String getNameSpace() {
            return this.nameSpace;
        }
    }

    final static Pattern INTF = Pattern.compile("/((.+)/)?(.+)$");

    /**
     * @param stringValue
     * @param type
     * @return
     */
    public static Object convert(final String stringValue, final Type type) {
        final boolean isQuotedStringValue = stringValue.startsWith("\"") && stringValue.endsWith("\"");
        if ((type == String.class || Clazz.isEnum(type)) && !isQuotedStringValue) {
            /* fast for String and Enum */
            if ("null".equals(stringValue)) {
                return null;
            } else {
                final String jsonStringValue = JSonStorage.serializeToJson(stringValue);
                return convert(jsonStringValue, type);
            }
        }
        try {
            @SuppressWarnings({ "unchecked", "rawtypes" })
            final Object restoredValue = JSonStorage.restoreFromString(stringValue, new TypeRef(type) {
            });
            if (Clazz.isPrimitive(type)) {
                return ReflectionUtils.cast(restoredValue, type);
            } else {
                return restoredValue;
            }
        } catch (JSonMapperException e) {
            /* slower retry due to JSonMapperException */
            if (!isQuotedStringValue) {
                if (Clazz.isEnum(type)) {
                    final String jsonStringValue = JSonStorage.serializeToJson(stringValue);
                    return convert(jsonStringValue, type);
                } else if (type == String.class) {
                    final String jsonStringValue = JSonStorage.serializeToJson(stringValue);
                    return convert(jsonStringValue, type);
                } else if (type == Object.class) {
                    final String jsonStringValue = JSonStorage.serializeToJson(stringValue);
                    return convert(jsonStringValue, type);
                }
            }
            throw e;
        }
    }

    protected HTTPBridge getHTTPBridge(final RemoteAPIRequest request, final RemoteAPIResponse response) {
        if (request != null) {
            final HttpRequest httpRequest = request.getHttpRequest();
            if (httpRequest != null) {
                return httpRequest.getBridge();
            }
        }
        return null;
    }

    @Deprecated
    public static OutputStream getOutputStream(final RemoteAPIResponse response, final RemoteAPIRequest request, final boolean gzip, final boolean wrapJQuery) throws IOException {
        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_REQUEST_CACHE_CONTROL, "no-store, no-cache"));
        response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_TYPE, "application/json"));
        final HTTPBridge bridge = response.getRemoteAPI().getHTTPBridge(request, response);
        final boolean chunked;
        if (bridge == null || bridge.canHandleChunkedEncoding(request.getHttpRequest(), response.getHttpResponse())) {
            chunked = true;
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING, HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED));
        } else {
            chunked = false;
        }
        if (gzip) {
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_ENCODING, "gzip"));
        }
        response.setResponseCode(ResponseCode.SUCCESS_OK);
        final OutputStream os;
        if (chunked) {
            os = new ChunkedOutputStream(response.getOutputStream(true));
        } else {
            os = response.getOutputStream(true);
        }
        final OutputStream uos;
        final GZIPOutputStream out;
        if (gzip) {
            uos = out = new GZIPOutputStream(os);
        } else {
            out = null;
            uos = os;
        }
        return new OutputStream() {
            boolean wrapperHeader = wrapJQuery && request != null && request.getJqueryCallback() != null;
            boolean wrapperEnd    = wrapJQuery && request != null && request.getJqueryCallback() != null;

            @Override
            public void close() throws IOException {
                this.wrapperEnd();
                if (out != null) {
                    out.finish();
                    out.flush();
                }
                uos.close();
            }

            @Override
            public void flush() throws IOException {
                uos.flush();
            }

            private void wrapperEnd() throws UnsupportedEncodingException, IOException {
                if (this.wrapperEnd) {
                    uos.write(")".getBytes("UTF-8"));
                    this.wrapperEnd = false;
                }
            }

            private void wrapperHeader() throws UnsupportedEncodingException, IOException {
                if (this.wrapperHeader) {
                    uos.write(request.getJqueryCallback().getBytes("UTF-8"));
                    uos.write("(".getBytes("UTF-8"));
                    this.wrapperHeader = false;
                }
            }

            @Override
            public void write(final byte[] b) throws IOException {
                this.wrapperHeader();
                uos.write(b);
            }

            @Override
            public void write(final int b) throws IOException {
                this.wrapperHeader();
                uos.write(b);
            }
        };
    }

    public static boolean gzip(final HttpRequest request) {
        final HTTPHeader acceptEncoding = request.getRequestHeaders().get(HTTPConstants.HEADER_REQUEST_ACCEPT_ENCODING);
        if (acceptEncoding != null) {
            final String value = acceptEncoding.getValue();
            if (value != null && value.contains("gzip")) {
                return true;
            }
        }
        return false;
    }

    public static boolean deflate(final HttpRequest request) {
        final HTTPHeader acceptEncoding = request.getRequestHeaders().get(HTTPConstants.HEADER_REQUEST_ACCEPT_ENCODING);
        if (acceptEncoding != null) {
            final String value = acceptEncoding.getValue();
            if (value != null && value.contains("deflate")) {
                return true;
            }
        }
        return false;
    }

    public static boolean deflate(final RemoteAPIRequest request) {
        return deflate(request.getHttpRequest());
    }

    public static boolean gzip(final RemoteAPIRequest request) {
        return gzip(request.getHttpRequest());
    }

    /* hashmap that holds all registered interfaces and their paths */
    private HashMap<String, InterfaceHandler<RemoteAPIInterface>> interfaces = new HashMap<String, InterfaceHandler<RemoteAPIInterface>>();
    private DefaultDocsPageFactory                                helpBuilder;

    public RemoteAPI() {
    }

    @SuppressWarnings("unchecked")
    protected void _handleRemoteAPICall(final RemoteAPIRequest request, final RemoteAPIResponse response) throws BasicRemoteAPIException {
        Object responseData = null;
        final Method method = request.getMethod();
        try {
            if (method == null) {
                //
                System.out.println("No API Method Found: " + request.getRequestedURL() + " Parameter: " + request.getParameters().length);
                throw new ApiCommandNotAvailable(request.getRequestedURL());
            }
            this.authenticate(method, request, response);
            final Object[] parameters = new Object[method.getParameterTypes().length];
            boolean methodHasReturnTypeAndAResponseParameter = false;
            boolean methodHasResponseParameter = false;
            int count = 0;
            for (int i = 0; i < parameters.length; i++) {
                if (RemoteAPIRequest.class.isAssignableFrom(method.getParameterTypes()[i])) {
                    parameters[i] = request;
                } else if (RemoteAPIResponse.class.isAssignableFrom(method.getParameterTypes()[i])) {
                    methodHasResponseParameter = true;
                    if (method.getAnnotation(AllowResponseAccess.class) != null) {
                        methodHasReturnTypeAndAResponseParameter = true;
                    }
                    parameters[i] = response;
                } else {
                    try {
                        final String json = request.getParameters()[count];
                        final Type type = method.getGenericParameterTypes()[i];
                        parameters[i] = jsonToParameterObject(json, type);
                    } catch (final BasicRemoteAPIException e) {
                        throw e;
                    } catch (final Throwable e) {
                        throw new BadParameterException(e, request.getParameters()[count]);
                    }
                    count++;
                }
            }
            try {
                responseData = request.getIface().invoke(method, parameters);
            } catch (final InvocationTargetException e) {
                throw e.getTargetException();
            }
            if (methodHasResponseParameter && !methodHasReturnTypeAndAResponseParameter) {
                /*
                 * TODO: check for unhandled response, be aware of async responses!
                 */
                return;
            }
            this.writeStringResponse(responseData, method, request, response);
        } catch (BasicRemoteAPIException e) {
            // set request and response if it has not set yet
            if (e.getRequest() == null) {
                e.setRequest(request);
            }
            if (e.getResponse() == null) {
                e.setResponse(response);
            }
            e = this.preProcessBasicRemoteAPIException(request, response, e);
            if (e != null) {
                throw e;
            }
        } catch (final Throwable e) {
            e.printStackTrace();
            final InternalApiException internal = new InternalApiException(e);
            internal.setRequest(request);
            internal.setResponse(response);
            final BasicRemoteAPIException ret = this.preProcessBasicRemoteAPIException(request, response, internal);
            if (ret != null) {
                throw ret;
            }
        }
    }

    protected Object jsonToParameterObject(String json, Type type) throws RemoteAPIException {
        return RemoteAPI.convert(json, type);
    }

    /**
     * @param method
     * @param request
     * @param response
     * @throws BasicRemoteAPIException
     */
    protected void authenticate(final Method method, final RemoteAPIRequest request, final RemoteAPIResponse response) throws BasicRemoteAPIException {
        if (request.getIface().getSignatureHandler() != null && request.getIface().isSignatureRequired(method)) {
            /* maybe this request is handled by rawMethodHandler */
            final Object[] parameters = new Object[] { request, response };
            Object responseData;
            try {
                try {
                    responseData = request.getIface().invoke(request.getIface().getSignatureHandler(), parameters);
                    if (!Boolean.TRUE.equals(responseData)) {
                        throw new AuthException();
                    }
                } catch (final InvocationTargetException e) {
                    throw e.getTargetException();
                }
            } catch (final BasicRemoteAPIException e) {
                throw e;
            } catch (final Throwable e) {
                throw new InternalApiException(e);
            }
        }
    }

    public static class ParsedParameters {
        public final java.util.List<String> parameters;

        public ParsedParameters(List<String> parameters, String jqueryCallback) {
            super();
            this.parameters = Collections.unmodifiableList(parameters);
            this.jqueryCallback = jqueryCallback;
        }

        public final String jqueryCallback;
    }

    public RemoteAPIRequest createRemoteAPIRequestObject(final HttpRequest request) throws BasicRemoteAPIException {
        Object preData = prepareRawRequest(request);
        this.validateRequest(request);
        final RemoteAPIMethod remoteAPIMethod = this.getRemoteAPIMethod(request);
        if (remoteAPIMethod == null) {
            return null;
        }
        ParsedParameters parsedParameters = parseParameters(request);
        // if (jqueryCallback != null) {
        // System.out.println("found jquery callback: " + jqueryCallback);
        // }
        RemoteAPIRequest ret;
        try {
            ret = this.createRemoteAPIRequestObject(request, preData, remoteAPIMethod.getMethodName(), remoteAPIMethod.getInterfaceHandler(), parsedParameters);
        } catch (final IOException e) {
            throw new BasicRemoteAPIException(e);
        }
        this.validateRequest(ret);
        return ret;
    }

    /**
     * @param request
     * @return
     * @throws BasicRemoteAPIException
     */
    public ParsedParameters parseParameters(HttpRequest request) throws BasicRemoteAPIException {
        final java.util.List<String> parameters = new ArrayList<String>();
        String jqueryCallback = null;
        /* convert GET parameters to methodParameters */
        for (final KeyValuePair param : request.getRequestedURLParameters()) {
            if (param.key != null) {
                /* key=value(parameter) */
                if ("callback".equalsIgnoreCase(param.key)) {
                    /* filter jquery callback */
                    jqueryCallback = param.value;
                    continue;
                } else if ("signature".equalsIgnoreCase(param.key)) {
                    /* filter url signature */
                    continue;
                } else if ("rid".equalsIgnoreCase(param.key)) {
                    continue;
                }
            }
            parameters.add(param.value);
        }
        if (request instanceof PostRequest) {
            try {
                final List<KeyValuePair> ret = ((PostRequest) request).getPostParameter();
                if (ret != null) {
                    /* add POST parameters to methodParameters */
                    for (final KeyValuePair param : ret) {
                        if (param.key != null) {
                            /* key=value(parameter) */
                            if ("callback".equalsIgnoreCase(param.key)) {
                                /* filter jquery callback */
                                jqueryCallback = param.value;
                                continue;
                            }
                        }
                        /* key(parameter) */
                        parameters.add(param.value);
                    }
                }
            } catch (final Throwable e) {
                if (e instanceof BasicRemoteAPIException) {
                    throw (BasicRemoteAPIException) e;
                }
                if (e.getCause() instanceof BasicRemoteAPIException) {
                    throw (BasicRemoteAPIException) e.getCause();
                }
                throw new RuntimeException(e);
            }
        }
        return new ParsedParameters(parameters, jqueryCallback);
    }

    /**
     * @param request
     * @return
     */
    protected Object prepareRawRequest(HttpRequest request) {        
        return null;
    }

    public RemoteAPIRequest createRemoteAPIRequestObject(final HttpRequest request, Object extractedData, final String method, final InterfaceHandler<?> interfaceHandler, ParsedParameters parsedParameters) throws IOException, BasicRemoteAPIException {
        return new RemoteAPIRequest(interfaceHandler, method, parsedParameters.parameters.toArray(new String[] {}), request, parsedParameters.jqueryCallback);
    }

    protected RemoteAPIResponse createRemoteAPIResponseObject(final RemoteAPIRequest request, final HttpResponse response) throws IOException {
        return new RemoteAPIResponse(response, this);
    }

    public RemoteAPIMethod getRemoteAPIMethod(final HttpRequest request) throws BasicRemoteAPIException {
        final String path = getRequestPathWithoutNamespace(request);
        String[] intf = new Regex(path, RemoteAPI.INTF).getRow(0);
        if (intf == null || intf.length != 3) {
            if ("/".equals(path)) {
                // special handling. method "" in root
                intf = new String[] { "", "", "" };
            } else {
                return null;
            }
        }
        /* intf=unimportant,namespace,method */
        if (intf[2] != null && intf[2].endsWith("/")) {
            /* special handling for commands without name */
            /**
             * Explanation: this is for special handling of <br>
             * http://localhost/test --> this is method test in root <br>
             * http://localhost/test/ --> this is method without name in namespace test
             */
            intf[1] = intf[2].substring(0, intf[2].length() - 1);
            intf[2] = "";
        }
        if (intf[1] == null) {
            intf[1] = "";
        }
        String namespace = intf[1];
        String methodName = intf[2];
        if (isHelpRequest(methodName)) {
            try {
                return new HelpMethod(getHelpBuilder());
            } catch (Throwable e) {
                throw new WTFException(e);
            }
        }
        final InterfaceHandler<RemoteAPIInterface> ret = getInterfaceByNamespace(namespace);
        if (ret != null) {
            return new RemoteAPIMethod(namespace, ret, methodName);
        } else {
            return null;
        }
    }

    protected String getRequestPathWithoutNamespace(final HttpRequest request) {
        final String requestPath = request.getRequestedPath();
        final int index = requestPath.lastIndexOf("/", requestPath.lastIndexOf("/") - 1);
        if (index > 0) {
            for (final String nameSpace : interfaces.keySet()) {
                if (requestPath.startsWith("/" + nameSpace + "/")) {
                    // keep original requestPath because of existing interface with matching namespace
                    return requestPath;
                }
            }
            return requestPath.substring(index);
        } else {
            return requestPath;
        }
    }

    protected InterfaceHandler<RemoteAPIInterface> getInterfaceByNamespace(String namespace) {
        final InterfaceHandler<RemoteAPIInterface> ret = this.interfaces.get(namespace);
        return ret;
    }

    public ArrayList<Class<? extends RemoteAPIInterface>> listInterfaces() {
        final Set<Class<? extends RemoteAPIInterface>> ret = new HashSet<Class<? extends RemoteAPIInterface>>();
        for (Entry<String, InterfaceHandler<RemoteAPIInterface>> es : interfaces.entrySet()) {
            for (Class<RemoteAPIInterface> iface : es.getValue().getInterfaceClasses()) {
                ret.add(iface);
            }
        }
        return new ArrayList<Class<? extends RemoteAPIInterface>>(ret);
    }

    /**
     * @return
     * @throws NoSuchMethodException
     */
    public synchronized DefaultDocsPageFactory getHelpBuilder() {
        try {
            if (helpBuilder == null) {
                helpBuilder = createHelpBuilder();
            }
            return helpBuilder;
        } catch (NoSuchMethodException e) {
            throw new WTFException(e);
        }
    }

    protected DefaultDocsPageFactory createHelpBuilder() throws NoSuchMethodException {
        final DefaultDocsPageFactory helpBuilder = new DefaultDocsPageFactory(this);
        return helpBuilder;
    }

    protected boolean isHelpRequest(String methodName) {
        return "help".equalsIgnoreCase(methodName);
    }

    /**
     * @param responseData
     * @param method
     * @return
     */
    protected Object handleVoidMethods(Object responseData, final Method method) {
        if (Clazz.isVoid(method.getReturnType())) {
            // void return
            responseData = "";
        }
        return responseData;
    }

    /**
     * override this if you want to authorize usage of methods
     *
     * @param request
     * @return
     */
    protected boolean isAllowed(final RemoteAPIRequest request, final RemoteAPIResponse response) {
        return true;
    }

    /**
     * @param request
     * @param text
     * @return
     */
    protected String jQueryWrap(final RemoteAPIRequest request, String text) {
        if (request.getJqueryCallback() != null) {
            /* wrap response into a valid jquery callback response format */
            final StringBuilder sb = new StringBuilder();
            sb.append(request.getJqueryCallback());
            sb.append("(");
            sb.append(text);
            sb.append(");");
            text = sb.toString();
        }
        return text;
    }

    /**
     * @param interfaceHandler
     * @param string
     * @return
     */
    public boolean onGetRequest(final GetRequest request, final HttpResponse response) throws BasicRemoteAPIException {
        final RemoteAPIRequest apiRequest = this.createRemoteAPIRequestObject(request);
        if (apiRequest == null) {
            return this.onUnknownRequest(request, response);
        }
        try {
            this._handleRemoteAPICall(apiRequest, this.createRemoteAPIResponseObject(apiRequest, response));
        } catch (final IOException e) {
            throw new BasicRemoteAPIException(e);
        }
        return true;
    }

    public boolean onPostRequest(final PostRequest request, final HttpResponse response) throws BasicRemoteAPIException {
        final RemoteAPIRequest apiRequest = this.createRemoteAPIRequestObject(request);
        if (apiRequest == null) {
            return this.onUnknownRequest(request, response);
        }
        try {
            this._handleRemoteAPICall(apiRequest, this.createRemoteAPIResponseObject(apiRequest, response));
        } catch (final IOException e) {
            throw new BasicRemoteAPIException(e);
        }
        return true;
    }

    protected boolean onUnknownRequest(final HttpRequest request, final HttpResponse response) {
        return false;
    }

    protected BasicRemoteAPIException preProcessBasicRemoteAPIException(final RemoteAPIRequest request, final RemoteAPIResponse response, final BasicRemoteAPIException e) {
        return e;
    }

    protected HashMap<String, InterfaceHandler<RemoteAPIInterface>> getHandlerMap() {
        return new HashMap<String, InterfaceHandler<RemoteAPIInterface>>(this.interfaces);
    }

    @SuppressWarnings("unchecked")
    public void register(final RemoteAPIInterface x) throws ParseException {
        final HashSet<Class<?>> interfaces = new HashSet<Class<?>>();
        synchronized (this) {
            final HashMap<String, InterfaceHandler<RemoteAPIInterface>> linterfaces = new HashMap<String, InterfaceHandler<RemoteAPIInterface>>(this.interfaces);
            Class<?> clazz = x.getClass();
            while (clazz != null) {
                main: for (final Class<?> c : clazz.getInterfaces()) {
                    if (RemoteAPIInterface.class.isAssignableFrom(c)) {
                        for (final Class<?> e : interfaces) {
                            /* avoid multiple adding of same interfaces */
                            if (c.isAssignableFrom(e)) {
                                continue main;
                            }
                        }
                        interfaces.add(c);
                        String namespace = c.getName();
                        final ApiNamespace a = c.getAnnotation(ApiNamespace.class);
                        if (a != null) {
                            namespace = a.value();
                        }
                        try {
                            try {
                                Method method = x.getClass().getMethod("getAPINamespace", new Class[] { Class.class });
                                if (method != null) {
                                    namespace = (String) method.invoke(x, new Object[] { c });
                                }
                            } catch (NoSuchMethodException e) {
                                // ok
                            }
                            int defaultAuthLevel = 0;
                            final ApiAuthLevel b = c.getAnnotation(ApiAuthLevel.class);
                            if (b != null) {
                                defaultAuthLevel = b.value();
                            }
                            // if (this.interfaces.containsKey(namespace)) { throw
                            // new IllegalStateException("Interface " + c.getName()
                            // + " with namespace " + namespace +
                            // " already has been registered by " +
                            // this.interfaces.get(namespace)); }
                            // System.out.println("Register: " + c.getName() +
                            // "->" + namespace);
                            // try {
                            // System.out.println("Register: " + c.getName() +
                            // "->" + namespace);
                            LogV3.info("Try to register API namespace /" + namespace + " = " + c);
                            InterfaceHandler<RemoteAPIInterface> handler = linterfaces.get(namespace);
                            if (handler == null) {
                                handler = createHandler(x, c, defaultAuthLevel);
                                handler.setSessionRequired(c.getAnnotation(ApiSessionRequired.class) != null);
                                linterfaces.put(namespace, handler);
                            } else {
                                if (handler.isSessionRequired() != (c.getAnnotation(ApiSessionRequired.class) != null)) {
                                    throw new WTFException("Session API Interface Mismatch");
                                }
                                handler.add((Class<RemoteAPIInterface>) c, x, defaultAuthLevel);
                            }
                        } catch (IllegalAccessException e) {
                            throw new ParseException(e);
                        } catch (IllegalArgumentException e) {
                            throw new ParseException(e);
                        } catch (InvocationTargetException e) {
                            throw new ParseException(e);
                        } catch (final SecurityException e) {
                            throw new ParseException(e);
                        } catch (final NoSuchMethodException e) {
                            throw new ParseException(e);
                        }
                    }
                }
                clazz = clazz.getSuperclass();
                this.interfaces = linterfaces;
            }
        }
    }

    protected InterfaceHandler<RemoteAPIInterface> createHandler(final RemoteAPIInterface x, final Class<?> c, int defaultAuthLevel) throws ParseException, NoSuchMethodException {
        return InterfaceHandler.create((Class<RemoteAPIInterface>) c, x, defaultAuthLevel);
    }

    /**
     * @param request
     * @param response
     * @param text
     * @param chunked
     *            TODO
     * @throws UnsupportedEncodingException
     * @throws IOException
     */
    public void sendText(final RemoteAPIRequest request, final RemoteAPIResponse response, String text) throws UnsupportedEncodingException, IOException {
        text = this.jQueryWrap(request, text);
        final byte[] bytes = text.getBytes("UTF-8");
        response.setResponseCode(ResponseCode.SUCCESS_OK);
        response.sendBytes(request, bytes);
    }

    /**
     * @param responseData
     * @param responseData2
     * @return
     */
    public String toString(final RemoteAPIRequest request, final RemoteAPIResponse response, final Object responseData) {
        return JSonStorage.serializeToJson(new DataObject(responseData));
    }

    public void unregister(final RemoteAPIInterface x) {
        final HashSet<Class<?>> interfaces = new HashSet<Class<?>>();
        synchronized (this) {
            final HashMap<String, InterfaceHandler<RemoteAPIInterface>> linterfaces = new HashMap<String, InterfaceHandler<RemoteAPIInterface>>(this.interfaces);
            Class<?> clazz = x.getClass();
            while (clazz != null) {
                main: for (final Class<?> c : clazz.getInterfaces()) {
                    if (RemoteAPIInterface.class.isAssignableFrom(c)) {
                        for (final Class<?> e : interfaces) {
                            /* avoid multiple removing of same interfaces */
                            if (c.isAssignableFrom(e)) {
                                continue main;
                            }
                        }
                        interfaces.add(c);
                        String namespace = c.getName();
                        final ApiNamespace a = c.getAnnotation(ApiNamespace.class);
                        if (a != null) {
                            namespace = a.value();
                        }
                        linterfaces.remove(namespace);
                    }
                }
                clazz = clazz.getSuperclass();
            }
            this.interfaces = linterfaces;
        }
    }

    /**
     * @param request
     */
    protected void validateRequest(final HttpRequest request) throws BasicRemoteAPIException {        
    }

    /**
     * @param ret
     */
    protected void validateRequest(final RemoteAPIRequest ret) throws BasicRemoteAPIException {        
    }

    public void writeStringResponse(Object responseData, final Method method, final RemoteAPIRequest request, final RemoteAPIResponse response) throws BasicRemoteAPIException {
        try {
            String text = null;
            if (method != null) {
                responseData = this.handleVoidMethods(responseData, method);
            }
            if (method != null && method.getAnnotation(ResponseWrapper.class) != null) {
                text = ((AbstractResponseWrapper<Object>) method.getAnnotation(ResponseWrapper.class).value().newInstance()).toString(responseData);
            } else {
                text = this.toString(request, response, responseData);
            }
            this.sendText(request, response, text);
        } catch (final Throwable e) {
            final InternalApiException internal = new InternalApiException(e);
            internal.setRequest(request);
            internal.setResponse(response);
            throw internal;
        }
    }

    /**
     * @param request
     * @param response
     * @param bytes
     * @throws InternalApiException
     * @throws IOException
     */
    public static void sendBytesCompressed(HttpRequest request, HttpResponse response, final InputStream is) throws IOException {
        final boolean gzip = RemoteAPI.gzip(request);
        final boolean deflate = RemoteAPI.deflate(request) && Application.getJavaVersion() >= Application.JAVA16;
        final boolean isHeadRequest = HttpConnectionType.HEAD.equals(request.getHttpConnectionType());
        if (deflate) {
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_ENCODING, "deflate"));
        } else if (gzip) {
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_ENCODING, "gzip"));
        }
        OutputStream os;
        final HTTPBridge bridge = request != null ? request.getBridge() : null;
        if (bridge == null || bridge.canHandleChunkedEncoding(request, response)) {
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING, HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED));
            os = new ChunkedOutputStream(response.getOutputStream(true));
        } else {
            os = response.getOutputStream(true);
        }
        if (!isHeadRequest) {
            if (!isHeadRequest) {
                try {
                    if (deflate) {
                        os = new DeflaterOutputStream(os, new Deflater(9, true), false);
                    } else if (gzip) {
                        os = new GZIPOutputStream(os);
                    }
                    IO.readStreamToOutputStream(-1, is, os, false);
                } finally {
                    os.close();
                }
            }
        }
    }

    /**
     * @param request
     * @param response
     * @param bytes
     * @throws InternalApiException
     * @throws IOException
     */
    public static void sendBytesCompressed(HttpRequest request, HttpResponse response, byte[] bytes) throws IOException {
        final boolean gzip = RemoteAPI.gzip(request);
        final boolean deflate = RemoteAPI.deflate(request) && Application.getJavaVersion() >= Application.JAVA16;
        final boolean isHeadRequest = HttpConnectionType.HEAD.equals(request.getHttpConnectionType());
        long contentLength = bytes.length;
        if ((gzip == false && deflate == false) || contentLength == 0) {
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, String.valueOf(bytes.length)));
            final OutputStream os = response.getOutputStream(true);
            if (!isHeadRequest) {
                os.write(bytes);
            }
        } else {
            boolean inMemoryCompressed = false;
            if (contentLength < 1 * 1024 * 1024 && (gzip || deflate)) {
                // compress small in memory
                final CountingOutputStream cos;
                final ByteArrayOutputStream bos;
                if (isHeadRequest) {
                    bos = null;
                    cos = new CountingOutputStream(new NullOutputStream());
                } else {
                    bos = new ByteArrayOutputStream();
                    cos = new CountingOutputStream(bos);
                }
                if (deflate) {
                    final DeflaterOutputStream out = new DeflaterOutputStream(cos, new Deflater(9, true), true);
                    out.write(bytes);
                    out.close();
                } else if (gzip) {
                    final GZIPOutputStream out = new GZIPOutputStream(cos);
                    out.write(bytes);
                    out.close();
                }
                contentLength = cos.transferedBytes();
                inMemoryCompressed = true;
                if (bos != null) {
                    bytes = bos.toByteArray();
                    if (contentLength != bytes.length) {
                        throw new IOException("in memory compression failed:" + contentLength + "!=" + bytes.length);
                    }
                }
            }
            response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_LENGTH, String.valueOf(contentLength)));
            if (deflate) {
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_ENCODING, "deflate"));
            } else if (gzip) {
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_CONTENT_ENCODING, "gzip"));
            }
            OutputStream os;
            final HTTPBridge bridge = request != null ? request.getBridge() : null;
            if (bridge == null || bridge.canHandleChunkedEncoding(request, response)) {
                response.getResponseHeaders().add(new HTTPHeader(HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING, HTTPConstants.HEADER_RESPONSE_TRANSFER_ENCODING_CHUNKED));
                os = new ChunkedOutputStream(response.getOutputStream(true));
            } else {
                os = response.getOutputStream(true);
            }
            if (!isHeadRequest) {
                try {
                    if (inMemoryCompressed == false) {
                        if (deflate) {
                            os = new DeflaterOutputStream(os, new Deflater(9, true), false);
                        } else if (gzip) {
                            os = new GZIPOutputStream(os);
                        }
                    }
                    os.write(bytes);
                } finally {
                    os.close();
                }
            }
        }
    }
}
