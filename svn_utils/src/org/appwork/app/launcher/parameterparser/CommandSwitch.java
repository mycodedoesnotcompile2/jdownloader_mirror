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
package org.appwork.app.launcher.parameterparser;

import java.util.ArrayList;

import org.appwork.utils.StringUtils;

/**
 * This event contains information about a startup parameter kombination like -switch p1 p2 ...
 *
 * @author $Author: unknown$
 *
 *
 */
public class CommandSwitch {
    /**
     * the parameters that follow the {@link #switchCommand} without leading -
     */
    private String[]     parameters;
    /**
     * command. given at startup with --command or -command
     */
    private final String caseInsensitiveSwitchCommand;
    private final String caseSensitiveSwitchCommand;

    /**
     * @param switchCommand
     * @param caseSensitiveSwitch
     *            *
     * @param array
     */
    public CommandSwitch(final String switchCommand, final String[] array) {
        this.caseInsensitiveSwitchCommand = StringUtils.toLowerCaseOrNull(switchCommand);
        this.caseSensitiveSwitchCommand = switchCommand;
        parameters = array;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("-");
        sb.append(getCaseInsensitiveSwitchCommand());
        for (final String p : getParameters()) {
            if (sb.length() > 0) {
                sb.append(" ");
            }
            sb.append("\"");
            sb.append(p);
            sb.append("\"");
        }
        return sb.toString();
    }

    /**
     * @return the {@link CommandSwitch#parameters}
     * @see CommandSwitch#parameters
     */
    public String[] getParameters() {
        return parameters;
    }

    @Deprecated
    public String getSwitchCommand() {
        return getCaseInsensitiveSwitchCommand();
    }

    public String getCaseInsensitiveSwitchCommand() {
        return caseInsensitiveSwitchCommand;
    }

    public String getCaseSensitiveSwitchCommand() {
        return caseSensitiveSwitchCommand;
    }

    public boolean hasAnyParameter(final String... strings) {
        for (String search : strings) {
            for (String param : getParameters()) {
                if (StringUtils.equals(search, param)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * @param string
     * @return
     */
    public boolean hasParameter(final String string) {
        for (final String p : getParameters()) {
            if (p.equals(string)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @param string
     * @return
     */
    public boolean hasAnyParameterIgnoreCase(String... strings) {
        for (String search : strings) {
            for (String param : getParameters()) {
                if (StringUtils.equalsIgnoreCase(search, param)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Case Sensitive! remove all paramaters that match any of the values in the searches list
     *
     * @param string
     * @param string2
     * @param string3
     * @return
     */
    public boolean removeAll(String... searches) {
        boolean ret = false;
        ArrayList<String> newList = new ArrayList<String>();
        String[] params = getParameters();
        paramLoop: for (String next : params) {
            for (String search : searches) {
                if (StringUtils.equals(next, search)) {
                    ret = true;
                    continue paramLoop;
                }
            }
            newList.add(next);
        }
        if (ret) {
            parameters = newList.toArray(new String[0]);
        }
        return ret;
    }

    /**
     * Case INSensitive! remove all paramaters that match any of the values in the searches list
     */
    public boolean removeAllIgnoreCase(String... searches) {
        boolean ret = false;
        ArrayList<String> newList = new ArrayList<String>();
        String[] params = getParameters();
        paramLoop: for (String next : params) {
            for (String search : searches) {
                if (StringUtils.equalsIgnoreCase(next, search)) {
                    ret = true;
                    continue paramLoop;
                }
            }
            newList.add(next);
        }
        if (ret) {
            parameters = newList.toArray(new String[0]);
        }
        return ret;
    }
}
