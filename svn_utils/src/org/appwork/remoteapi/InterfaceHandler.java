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

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.regex.Pattern;

import org.appwork.remoteapi.annotations.AllowResponseAccess;
import org.appwork.remoteapi.annotations.ApiAuthLevel;
import org.appwork.remoteapi.annotations.ApiDoNotExpose;
import org.appwork.remoteapi.annotations.ApiDoc;
import org.appwork.remoteapi.annotations.ApiHiddenMethod;
import org.appwork.remoteapi.annotations.ApiMethodName;
import org.appwork.remoteapi.annotations.ApiNamespace;
import org.appwork.remoteapi.annotations.ApiRawMethod;
import org.appwork.remoteapi.annotations.ApiSessionRequired;
import org.appwork.remoteapi.annotations.ApiSignatureRequired;
import org.appwork.storage.JSonStorage;
import org.appwork.utils.Application;
import org.appwork.utils.StringUtils;

/**
 * @author thomas
 *
 */
public class InterfaceHandler<T> {
    private static final String REGEX_CLS = "[a-zA-Z0-9\\[\\]_\\.]+(:?<[\\.a-zA-Z0-9\\[\\]_\\?\\,\\s<>]*>)?[\\[\\]]*";

    /**
     * @param c
     * @param x
     * @return
     * @throws ParseException
     * @throws NoSuchMethodException
     * @throws SecurityException
     */
    public static <T extends RemoteAPIInterface> InterfaceHandler<T> create(final Class<T> c, final RemoteAPIInterface x, final int defaultAuthLevel) throws ParseException, SecurityException, NoSuchMethodException {
        final InterfaceHandler<T> ret = new InterfaceHandler<T>(c, x, defaultAuthLevel);
        ret.parse();
        return ret;
    }

    private final RemoteAPIInterface impl;

    public RemoteAPIInterface getImpl() {
        return impl;
    }

    private final java.util.List<Class<T>> interfaceClasses;
    private final HashMap<Method, Integer> parameterCountMap;
    private final HashMap<Method, Integer> methodsAuthLevel;
    private final HashMap<String, Method>  methods;
    private final HashSet<Method>          signatureRequiredMethods;
    private Method                         signatureHandler = null;
    private final int                      defaultAuthLevel;
    private boolean                        sessionRequired  = false;
    final private String                   namespace;

    public String getNamespace() {
        return namespace;
    }

    /**
     * @param <T>
     * @param c
     * @param x
     * @throws NoSuchMethodException
     * @throws SecurityException
     */
    public InterfaceHandler(final Class<T> c, final RemoteAPIInterface x, final int defaultAuthLevel) throws SecurityException, NoSuchMethodException {
        this.interfaceClasses = new ArrayList<Class<T>>();
        this.interfaceClasses.add(c);
        ApiNamespace an = c.getAnnotation(ApiNamespace.class);
        this.namespace = an == null ? c.getName() : an.value();
        this.impl = x;
        this.defaultAuthLevel = defaultAuthLevel;
        this.methodsAuthLevel = new HashMap<Method, Integer>();
        this.parameterCountMap = new HashMap<Method, Integer>();
        this.methods = new HashMap<String, Method>();
        this.signatureRequiredMethods = new HashSet<Method>();
        if (x instanceof InterfaceHandlerSetter) {
            ((InterfaceHandlerSetter) x).setInterfaceHandler(this);
        }
    }

    /**
     *
     */
    protected InterfaceHandler() {
        impl = null;
        defaultAuthLevel = -1;
        namespace = null;
        this.methodsAuthLevel = new HashMap<Method, Integer>();
        this.methods = new HashMap<String, Method>();
        this.interfaceClasses = new ArrayList<Class<T>>();
        this.signatureRequiredMethods = new HashSet<Method>();
        this.parameterCountMap = new HashMap<Method, Integer>();
    }

    /**
     * @param c
     * @param defaultAuthLevel2
     * @param process
     * @throws ParseException
     */
    public void add(final Class<T> c, final RemoteAPIInterface process, final int defaultAuthLevel) throws ParseException {
        if (this.sessionRequired != (c.getAnnotation(ApiSessionRequired.class) != null)) {
            throw new ParseException("Check SessionRequired for " + this);
        }
        if (defaultAuthLevel != this.getDefaultAuthLevel()) {
            throw new ParseException("Check Authlevel " + c + " " + this);
        }
        if (process != this.impl) {
            throw new ParseException(process + "!=" + this.impl);
        }
        try {
            this.interfaceClasses.add(c);
            this.parse();
        } catch (final ParseException e) {
            this.interfaceClasses.remove(c);
            this.parse();
            throw e;
        }
    }

    public java.util.List<Class<T>> getInterfaceClasses() {
        return new ArrayList<Class<T>>(interfaceClasses);
    }

    public int getAuthLevel(final Method m) {
        final Integer auth = this.methodsAuthLevel.get(m);
        if (auth != null) {
            return auth;
        }
        return this.defaultAuthLevel;
    }

    public int getDefaultAuthLevel() {
        return this.defaultAuthLevel;
    }

    /**
     * @param length
     * @param methodName
     * @return
     */
    public Method getMethod(final String methodName, final int length) {
        final String methodID = methodName + length;
        final Method ret = this.methods.get(methodID);
        if (ret != null) {
            return ret;
        }
        return this.methods.get(methodName);
    }

    public boolean hasMethodName(final String methodName) {
        if (StringUtils.isNotEmpty(methodName)) {
            for (final String method : methods.keySet()) {
                if (method.equals(methodName) || method.matches("^" + Pattern.quote(methodName) + "\\d+$")) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * @param method
     * @return
     */
    public int getParameterCount(final Method method) {
        final Integer ret = this.parameterCountMap.get(method);
        if (ret != null) {
            return ret;
        }
        return -1;
    }

    public Method getSignatureHandler() {
        return this.signatureHandler;
    }

    private String helpJSON(final RemoteAPIRequest request, final RemoteAPIResponse response) throws UnsupportedEncodingException, IOException {
        final List<RemoteAPIMethodDefinition> methodDefinitions = new ArrayList<RemoteAPIMethodDefinition>();
        for (final Method m : this.methods.values()) {
            final RemoteAPIMethodDefinition mDef = new RemoteAPIMethodDefinition();
            mDef.setMethodName(m.getName());
            final ApiDoc an = m.getAnnotation(ApiDoc.class);
            if (an != null) {
                mDef.setDescription(an.value());
            }
            final List<String> parameters = new ArrayList<String>();
            for (int i = 0; i < m.getGenericParameterTypes().length; i++) {
                if (m.getParameterTypes()[i] == RemoteAPIRequest.class || m.getParameterTypes()[i] == RemoteAPIResponse.class) {
                    continue;
                }
                parameters.add(m.getParameterTypes()[i].getSimpleName());
            }
            mDef.setParameters(parameters);
            methodDefinitions.add(mDef);
        }
        return JSonStorage.serializeToJson(methodDefinitions);
    }

    /**
     * @param method
     * @param parameters
     * @return
     * @throws InvocationTargetException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     */
    public Object invoke(final Method method, final Object[] parameters) throws IllegalArgumentException, IllegalAccessException, InvocationTargetException {
        if (method.getDeclaringClass() == InterfaceHandler.class) {
            return method.invoke(this, parameters);
        } else {
            return method.invoke(this.impl, parameters);
        }
    }

    /**
     * @return the sessionRequired
     */
    public boolean isSessionRequired() {
        return this.sessionRequired;
    }

    public boolean isSignatureRequired(final Method m) {
        return this.signatureRequiredMethods.contains(m);
    }

    /**
     * @throws ParseException
     *
     */
    public void parse() throws ParseException {
        this.methods.clear();
        this.parameterCountMap.clear();
        this.methodsAuthLevel.clear();
        this.signatureHandler = null;
        Class<T> signatureHandlerNeededClass = null;
        for (final Class<T> interfaceClass : this.interfaceClasses) {

            for (final Method m : interfaceClass.getMethods()) {
                if (m.getAnnotation(ApiHiddenMethod.class) != null) {
                    continue;
                }
                if (m.getAnnotation(ApiDoNotExpose.class) != null) {
                    continue;
                }

                int paramCounter = 0;
                for (final Class<?> c : m.getParameterTypes()) {
                    if (c != RemoteAPIRequest.class && c != RemoteAPIResponse.class) {
                        paramCounter++;
                    }
                }
                String name = m.getName();
                if ("handleRemoteAPISignature".equals(name) && paramCounter == 0) {
                    this.signatureHandler = m;
                    continue;
                }
                final ApiMethodName methodname = m.getAnnotation(ApiMethodName.class);
                if (methodname != null) {
                    name = methodname.value();
                }
                if (this.methods.put(name + paramCounter, m) != null) {
                    throw new ParseException(interfaceClass + " already contains method: \r\n" + name + "\r\n");
                }
                if (m.getAnnotation(ApiRawMethod.class) != null) {
                    final Method existing = methods.get(name);
                    // prefer method with less(more generic) parameters, so best method would be
                    //
                    // xy method(RemoteAPIRequest,RemoteAPIResponse) signature
                    // see example for Cnl2APIFlash.add
                    if (existing == null) {
                        this.methods.put(name, m);
                    } else {
                        int existingParamCounter = 0;
                        boolean hasRemoteAPIRequest = false;
                        for (final Class<?> c : existing.getParameterTypes()) {
                            if (c == RemoteAPIRequest.class) {
                                hasRemoteAPIRequest = true;
                            }
                            if (c != RemoteAPIRequest.class && c != RemoteAPIResponse.class) {
                                existingParamCounter++;
                            }
                        }
                        if (hasRemoteAPIRequest && paramCounter < existingParamCounter) {
                            this.methods.put(name, m);
                        }
                    }
                }
                this.parameterCountMap.put(m, paramCounter);
                final ApiAuthLevel auth = m.getAnnotation(ApiAuthLevel.class);
                if (auth != null) {
                    this.methodsAuthLevel.put(m, auth.value());
                }
                final ApiSignatureRequired signature = m.getAnnotation(ApiSignatureRequired.class);
                if (signature != null) {
                    signatureHandlerNeededClass = interfaceClass;
                    this.signatureRequiredMethods.add(m);
                }
            }

        }
        if (signatureHandlerNeededClass != null && this.signatureHandler == null) {
            throw new ParseException(signatureHandlerNeededClass + " Contains methods that need validated Signatures but no Validator provided");
        }
    }

    /**
     * @param string
     * @param hELP2
     */
    public void registerExtraMethod(String string, Method m) {
        this.methods.put("help", m);
        this.parameterCountMap.put(m, 0);
        this.methodsAuthLevel.put(m, 0);
    }

    /**
     * @param sessionRequired
     *            the sessionRequired to set
     * @throws ParseException
     */
    protected void setSessionRequired(final boolean sessionRequired) throws ParseException {
        this.sessionRequired = sessionRequired;
    }

    /**
     * @param m
     * @throws ParseException
     */
    private void validateMethod(final Method m) throws ParseException {

        if ("help".equalsIgnoreCase(m.getName())) {
            throw new ParseException(m + " is reserved for internal usage");
        }
        boolean responseIsParamater = false;

        for (final Type t : m.getGenericParameterTypes()) {
            if (RemoteAPIRequest.class == t) {
                continue;
            } else if (RemoteAPIResponse.class == t) {
                if (m.getAnnotation(AllowResponseAccess.class) == null) {
                    responseIsParamater = true;
                }
                continue;
            }
        }
        if (responseIsParamater) {
            if (m.getGenericReturnType() != void.class && m.getGenericReturnType() != Void.class) {
                if (!RemoteAPISignatureHandler.class.isAssignableFrom(m.getDeclaringClass())) {
                    throw new ParseException("Response in Parameters. " + m + " must return void, and has to handle the response itself");
                }
            }
        } else {
            if (m.getGenericReturnType() == void.class || m.getGenericReturnType() == Void.class) {
                // void is ok.
                return;
            }

        }
        return;
    }

    protected File getSourceFile(final Class cls) {
        File srcFile;
        URL url2 = Application.getRessourceURL("");
        try {
            File bin = new File(url2.toURI());
            File ws = bin.getParentFile().getParentFile();
            for (File project : ws.listFiles()) {
                if (project.isDirectory()) {
                    File file = new File(project, "src/" + cls.getName().replace(".", "/") + ".java");
                    if (file.exists()) {
                        return file;
                    }
                }
            }
        } catch (Throwable e) {            
            e.printStackTrace();
        }
        return null;
    }

    /**
     * @return
     */
    public HashMap<String, Method> getMethodsMap() {
        synchronized (this) {
            return new HashMap<String, Method>(methods);
        }
    }
}
