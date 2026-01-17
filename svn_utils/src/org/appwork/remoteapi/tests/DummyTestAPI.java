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

import org.appwork.remoteapi.RemoteAPIInterface;
import org.appwork.remoteapi.RemoteAPIRequest;
import org.appwork.remoteapi.annotations.AllowResponseAccess;
import org.appwork.remoteapi.annotations.ApiNamespace;
import org.appwork.remoteapi.exceptions.BadRequestException;

/**
 * Dummy API Interface for tests
 *
 * @author test
 */
@ApiNamespace("test")
public interface DummyTestAPI extends RemoteAPIInterface {

    /**
     * Simple test method - only accepts GET requests
     *
     * @param request
     *            The RemoteAPI request (optional, automatically injected)
     * @param message
     *            Test message
     * @return Echo of the message with HTTP method
     * @throws BadRequestException
     *             if the request method is not GET
     */
    public String echo(RemoteAPIRequest request, String message) throws BadRequestException;

    /**
     * Method that accepts POST data - only accepts POST requests
     *
     * @param request
     *            The RemoteAPI request (optional, automatically injected)
     * @param data
     *            POST data
     * @return Confirmation with HTTP method
     * @throws BadRequestException
     *             if the request method is not POST
     */
    public String postData(RemoteAPIRequest request, String data) throws BadRequestException;

    /**
     * Method with multiple parameters
     *
     * @param param1
     *            Parameter 1
     * @param param2
     *            Parameter 2
     * @return Result
     */
    public String multiParam(String param1, String param2);

    /**
     * Method for browser test - returns a unique test ID
     *
     * @param testId
     *            Test ID to verify
     * @return Confirmation message
     */
    public String browserTest(String testId);

    /**
     * Method that performs a long-running operation (for timeout testing)
     *
     * @param sleepMillis
     *            How long to sleep in milliseconds
     * @return Confirmation message
     */
    public String longRunningOperation(int sleepMillis);

    /**
     * Method that performs a long-running operation and throws InterruptedException (for timeout testing - verifies that timeout monitor
     * works even when exception is thrown)
     *
     * @param sleepMillis
     *            How long to sleep in milliseconds
     * @return Confirmation message
     * @throws InterruptedException
     *             if the sleep is interrupted
     */
    public String longRunningOperationThrowsInterrupt(int sleepMillis) throws InterruptedException;

    /**
     * Method that accepts both Request and Response parameters
     *
     * @param request
     *            The RemoteAPI request (automatically injected)
     * @param response
     *            The RemoteAPI response (automatically injected)
     * @param message
     *            Test message
     * @return Confirmation with request info
     */
    @AllowResponseAccess
    public String withRequestAndResponse(RemoteAPIRequest request, org.appwork.remoteapi.RemoteAPIResponse response, String message);

    /**
     * Method with integer parameter
     *
     * @param value
     *            Integer value
     * @return Result as string
     */
    public String withInteger(int value);

    /**
     * Method with boolean parameter
     *
     * @param value
     *            Boolean value
     * @return Result as string
     */
    public String withBoolean(boolean value);

    /**
     * Method that throws BadRequestException
     *
     * @param shouldThrow
     *            If true, throws exception
     * @return Success message
     * @throws BadRequestException
     *             if shouldThrow is true
     */
    public String throwsException(boolean shouldThrow) throws BadRequestException;

    /**
     * Method that returns void
     *
     * @param message
     *            Message to log
     */
    public void voidMethod(String message);

    /**
     * Method with multiple typed parameters
     *
     * @param str
     *            String parameter
     * @param num
     *            Integer parameter
     * @param flag
     *            Boolean parameter
     * @return Combined result
     */
    public String multiTypedParams(String str, int num, boolean flag);
}
