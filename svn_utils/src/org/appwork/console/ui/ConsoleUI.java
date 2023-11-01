package org.appwork.console.ui;

import java.io.IOException;

import org.appwork.console.AbstractConsole;
import org.appwork.txtresource.TranslationFactory;
import org.appwork.utils.Application;

/**
 * Simple Console UI. Keep it free from any GUI imports and coupling!
 *
 * @author thomas
 * @date 04.04.2022
 *
 */
public class ConsoleUI {
    private AbstractConsole             console;
    private boolean                     stdBefore;
    private boolean                     errBefore;
    public static final ConsoleUILocale T = TranslationFactory.create(ConsoleUILocale.class);

    /**
     *
     */
    public ConsoleUI() {
        this(AbstractConsole.newInstance());
    }

    /**
     * @param newInstance
     */
    public ConsoleUI(AbstractConsole console) {
        this.console = console;
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

    }

    public void end() {

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
        this.console.println(string);
    }

    public boolean waitYesOrNo(String yes, String no) {
        return waitYesOrNo(T.pattern_yes(), T.pattern_no(), T.lit_yes_or_y(), T.lit_no_or_n(), yes, no);
    }

    public boolean waitYesOrNo(String pattern_yes, String pattern_no, String input_yes, String input_no, String yes, String no) {

        while (true) {

            this.printLines(T.lit_please_confirm(input_yes, input_no, yes, no));
            String c;
            c = this.console.readLine();
            if (c.trim().matches(pattern_yes)) {
                return true;
            } else if (c.trim().matches(pattern_no)) {
                return false;
            }
        }

    }

    public void printLines(String stackTrace) {
        for (String l : stackTrace.split("[\r\n]+")) {
            this.println(l);
        }
    }

    public void waitForEnter() {

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
        this.print(">\t");
        return this.console.readLine();
    }

    public String askHidden(String string) {
        this.println(string);
        this.print(">\t");
        return this.console.readPassword();
    }

    /**
     * There might be no console -e,g. if we start the application from java - no console linked
     *
     * @return
     */
    public boolean hasConsole() {

        return console != null;
    }

}
