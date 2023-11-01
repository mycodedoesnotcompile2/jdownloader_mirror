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
package org.appwork.utils.logging;

import java.io.InvalidObjectException;
import java.io.ObjectStreamException;
import java.io.PrintStream;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author daniel, inspired by
 *         http://blogs.sun.com/nickstephen/entry/java_redirecting_system_out_and
 * 
 */
public final class StdOutErr2Logging extends Level {

    private static final PrintStream stdout           = System.out;
    private static final PrintStream stderr           = System.err;
    /**
     * 
     */
    private static final long        serialVersionUID = -8193079562712405474L;

    /**
     * Level for STDOUT activity.
     */
    public static final Level        STDOUT           = new StdOutErr2Logging("STDOUT", Level.INFO.intValue() + 53);

    /**
     * Level for STDERR activity
     */
    public static final Level        STDERR           = new StdOutErr2Logging("STDERR", Level.INFO.intValue() + 54);

    public static PrintStream getStdErr() {
        return StdOutErr2Logging.stderr;
    }

    public static PrintStream getStdOut() {
        return StdOutErr2Logging.stdout;
    }

    public static void redirectStdErr2Logger(final Logger logger) {
        System.setErr(new PrintStream(new LoggingOutputStream(logger, StdOutErr2Logging.STDERR), true));
    }

    public static void redirectStdOut2Logger(final Logger logger) {
        System.setOut(new PrintStream(new LoggingOutputStream(logger, StdOutErr2Logging.STDOUT), true));
    }

    private StdOutErr2Logging(final String name, final int value) {
        super(name, value);
    }

    protected Object readResolve() throws ObjectStreamException {
        if (this.intValue() == StdOutErr2Logging.STDOUT.intValue()) { return StdOutErr2Logging.STDOUT; }
        if (this.intValue() == StdOutErr2Logging.STDERR.intValue()) { return StdOutErr2Logging.STDERR; }
        throw new InvalidObjectException("Unknown instance :" + this);
    }
}
