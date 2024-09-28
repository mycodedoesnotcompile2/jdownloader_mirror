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
package org.appwork.console;

import java.io.IOException;

import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.Exceptions;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;

public class ConsoleDialog implements ConsoleDialogInterface {
    private final AbstractConsole console;
    private boolean               stdBefore;
    private boolean               errBefore;
    private final String          title;

    public ConsoleDialog(final AbstractConsole console, String string) {
        if (console == null) {
            throw new IllegalStateException("No Console Available!");
        }
        this.console = console;
        this.title = string;
    }

    public ConsoleDialog(String string) {
        this(AbstractConsole.newInstance(), string);
    }

    public void start() {
        this.stdBefore = false;
        this.errBefore = false;
        try {
            this.stdBefore = Application.STD_OUT.setBufferEnabled(true);
            this.errBefore = Application.ERR_OUT.setBufferEnabled(true);
        } catch (IOException e) {
            e.printStackTrace();
            // cannot happen for parameter=true;
        }
        this.console.println("|---------------------------Headless Information-------------------------------");
        this.console.println("|\t" + this.title);
    }

    public void end() {
        this.console.println("|------------------------------------------------------------------------------");
        try {
            Application.STD_OUT.setBufferEnabled(this.stdBefore);
            Application.STD_OUT.flush();
        } catch (Throwable e) {
            e.printStackTrace();
        }
        try {
            Application.ERR_OUT.setBufferEnabled(this.errBefore);
            Application.ERR_OUT.flush();
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }

    public void println(String string) {
        this.console.println("|\t" + string);
    }

    public void waitYesOrNo(int flags, String yes, String no) throws DialogCanceledException, DialogClosedException {
        if ((flags & UIOManager.BUTTONS_HIDE_OK) != 0 || (flags & UIOManager.BUTTONS_HIDE_CANCEL) != 0) {
            this.waitToContinue((flags & UIOManager.BUTTONS_HIDE_OK) != 0 ? yes : no);
            if ((flags & UIOManager.BUTTONS_HIDE_OK) != 0) {
            } else if ((flags & UIOManager.BUTTONS_HIDE_CANCEL) != 0) {
                throw new DialogCanceledException(Dialog.RETURN_CANCEL);
            } else {
                throw new DialogClosedException(Dialog.RETURN_CLOSED);
            }
        } else {
            while (true) {
                this.println("Enter y -> " + yes);
                this.println("Enter n -> " + no);
                String c;
                c = this.console.readLine();
                if (c.trim().equalsIgnoreCase("y")) {
                    return;
                } else if (c.trim().equalsIgnoreCase("n")) {
                    throw new DialogCanceledException(Dialog.RETURN_CANCEL);
                }
            }
        }
    }

    public void printLines(String stackTrace) {
        for (String l : stackTrace.split("[\r\n]+")) {
            this.println(l);
        }
    }

    public void waitToContinue() {
        this.waitToContinue("continue");
    }

    public void waitToContinue(String string) {
        if (string == null) {
            string = "continue";
        }
        this.println("Press Enter to " + string);
        this.console.readLine();
    }

    public void print(String string) {
        this.console.print(string);
    }

    /**
     * @param string
     * @return
     */
    public String ask(String string) {
        this.println(string);
        this.print("|>\t");
        return this.console.readLine();
    }

    public String askHidden(String string) {
        this.println(string);
        this.print("|>\t");
        return this.console.readPassword();
    }

    public void flush() {
        this.console.flush();
    }

    /**
     * @param string
     * @param string2
     * @param e
     * @return
     */
    public static boolean showExceptionDialog(String title, String message, Throwable e) {
        if (!Application.isHeadless()) {
            return false;
        }
        synchronized (AbstractConsole.LOCK) {
            ConsoleDialog cd = new ConsoleDialog("Exception occured");
            cd.start();
            try {
                cd.println(title);
                cd.printLines(message);
                cd.printLines(Exceptions.getStackTrace(e));
                cd.waitToContinue();
                return true;
            } finally {
                cd.end();
            }
        }
    }
}
