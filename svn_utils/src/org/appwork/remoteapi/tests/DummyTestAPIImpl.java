/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
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
package org.appwork.remoteapi.tests;

import org.appwork.remoteapi.exceptions.BadRequestException;

/**
 * Dummy API Implementation for tests
 *
 * @author test
 */
public class DummyTestAPIImpl implements DummyTestAPI {

    public static String RECEIVED_TEST;

    @Override
    public String echo(final org.appwork.remoteapi.RemoteAPIRequest request, final String message) throws BadRequestException {
        // Verify that only GET method is allowed for echo
        if (request == null || request.getHttpRequest() == null) {
            throw new BadRequestException("Request is required");
        }
        final org.appwork.utils.net.httpserver.requests.HttpRequest httpRequest = request.getHttpRequest();
        // Check if it's a GetRequest instance
        // warning: this includes all GET-Like requests as well.
        if (!(httpRequest instanceof org.appwork.utils.net.httpserver.requests.AbstractGetRequest)) {
            throw new BadRequestException("Request class " + httpRequest.getClass().getSimpleName() + " is not allowed for echo. Only GetRequest is allowed.");
        }

        final String requestClass = httpRequest.getClass().getSimpleName();
        if (message == null) {
            return "Echo: null [Request: " + requestClass + "]";
        }
        return "Echo: " + message + " [Request: " + requestClass + "]";
    }

    @Override
    public String postData(final org.appwork.remoteapi.RemoteAPIRequest request, final String data) throws BadRequestException {
        // Verify that only POST method is allowed for postData
        if (request == null || request.getHttpRequest() == null) {
            throw new BadRequestException("Request is required");
        }
        final org.appwork.utils.net.httpserver.requests.HttpRequest httpRequest = request.getHttpRequest();
        // Check if it's a PostRequest instance
        // warning - this includes all post like requests as well
        if (!(httpRequest instanceof org.appwork.utils.net.httpserver.requests.AbstractPostRequest)) {
            throw new BadRequestException("Request class " + httpRequest.getClass().getSimpleName() + " is not allowed for postData. Only PostRequest is allowed.");
        }

        final String requestClass = httpRequest.getClass().getSimpleName();
        if (data == null) {
            return "Received: null [Request: " + requestClass + "]";
        }
        return "Received: " + data.length() + " bytes [Request: " + requestClass + "]";
    }

    @Override
    public String multiParam(final String param1, final String param2) {
        return "param1=" + (param1 != null ? param1 : "null") + ", param2=" + (param2 != null ? param2 : "null");
    }

    @Override
    public String browserTest(final String testId) {
        if (testId == null) {
            return "null";
        }
        RECEIVED_TEST = testId;
        return "BrowserTest:" + testId;
    }

    @Override
    public String longRunningOperation(final int sleepMillis) {
        try {
            Thread.sleep(sleepMillis);
            return "Completed after " + sleepMillis + "ms";
        } catch (final InterruptedException e) {
            Thread.currentThread().interrupt();
            return "Interrupted after sleep";
        }
    }

    @Override
    public String longRunningOperationThrowsInterrupt(final int sleepMillis) throws InterruptedException {
        Thread.sleep(sleepMillis);
        return "Completed after " + sleepMillis + "ms";
    }

    @Override
    public String withRequestAndResponse(final org.appwork.remoteapi.RemoteAPIRequest request, final org.appwork.remoteapi.RemoteAPIResponse response, final String message) {
        if (request == null || response == null) {
            return "Error: Request or Response is null";
        }
        final String requestMethod = request.getHttpRequest() != null && request.getHttpRequest().getRequestMethod() != null ? request.getHttpRequest().getRequestMethod().name() : "UNKNOWN";
        return "Request: " + requestMethod + ", Message: " + (message != null ? message : "null") + ", Response available: true";
    }

    @Override
    public String withInteger(final int value) {
        return "Integer value: " + value;
    }

    @Override
    public String withBoolean(final boolean value) {
        return "Boolean value: " + value;
    }

    @Override
    public String throwsException(final boolean shouldThrow) throws BadRequestException {
        if (shouldThrow) {
            throw new BadRequestException("Exception thrown as requested");
        }
        return "No exception thrown";
    }

    @Override
    public void voidMethod(final String message) {
        // Just log or store the message - void methods don't return anything
        RECEIVED_TEST = message != null ? message : "null";
    }

    @Override
    public String multiTypedParams(final String str, final int num, final boolean flag) {
        return "String: " + (str != null ? str : "null") + ", Int: " + num + ", Boolean: " + flag;
    }
}
