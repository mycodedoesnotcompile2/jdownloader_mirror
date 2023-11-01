/**
 * 
 * ====================================================================================================================================================
 *         "My JDownloader Client" License
 *         The "My JDownloader Client" will be called [The Product] from now on.
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
package org.jdownloader.myjdownloader.client.eventsender;


/**
 * @author thomas Callerclass,Parameterclass,Eventclass
 */
public class SimpleEvent<C, P, T> extends DefaultEvent {

    private P[]     parameters;
    private final T type;
    private final C callerImpl;

    /**
     * @param caller
     * @param newState
     * @param currentState
     * @param id
     */
    public SimpleEvent(final C caller, final T type, final P... parameters) {
        super(caller);
        this.callerImpl = caller;
        this.type = type;
        this.parameters = parameters;

    }

    @Override
    public C getCaller() {
        return this.callerImpl;
    }

    public P getParameter() {
        if (this.parameters.length == 0) { return null; }
        return this.parameters[0];
    }

    public P getParameter(final int i) {
        if (i < 0 || this.parameters.length == 0 || i > this.parameters.length - 1) { return null; }
        return this.parameters[i];
    }

    /**
     * @return the parameters
     */
    public P[] getParameters() {
        return this.parameters;
    }

    /**
     * @return the type
     */
    public T getType() {
        return this.type;
    }

    /**
     * @param parameters
     *            the parameters to set
     */
    public void setParameters(final P[] parameters) {
        this.parameters = parameters;
    }

    @Override
    public String toString() {
        return (this.getCaller() == null ? "null" : this.getCaller().getClass().getSimpleName()) + "'s " + this.getClass().getSimpleName() + " | " + this.type + " (" + this.parameters + ")";
    }

}
