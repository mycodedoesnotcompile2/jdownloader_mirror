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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import org.appwork.utils.DebugMode;
import org.appwork.utils.IO;
import org.appwork.utils.StringUtils;
import org.appwork.utils.parser.ShellParser;

/**
 * This class is used to parse and evaluate Startparameters
 *
 * @author $Author: unknown $
 *
 */
public class ParameterParser {
    /**
     * Stores the Applications startParameters
     */
    private String[]                       rawArguments;
    private HashMap<String, CommandSwitch> map;
    private ArrayList<CommandSwitch>       list;

    public ParameterParser(final String[] args) {
        rawArguments = args;
    }

    /**
     * @param string
     * @return
     */
    public CommandSwitch getCommandSwitch(final String string) {
        return getMap().get(StringUtils.toLowerCaseOrNull(string));
    }

    /**
     * @return
     */
    public String[] getRawArguments() {
        return rawArguments;
    }

    @Deprecated
    /**
     *
     * @deprecated Seems like a dirty hack and should get replaced
     */
    public void setRawArguments(final String[] rawArguments) {
        this.rawArguments = rawArguments;
    }

    public boolean hasAnyCommandSwitch(final String... string) {
        for (String s : string) {
            if (hasCommandSwitch(s)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @param string
     * @return
     */
    public boolean hasCommandSwitch(final String string) {
        return getMap().containsKey(StringUtils.toLowerCaseOrNull(string));
    }

    public HashMap<String, CommandSwitch> getMap() {
        return map;
    }

    /**
     * parses the command row. and fires {@link CommandSwitch} for each switch command
     *
     * @param commandFilePath
     *
     * @return
     */
    public ParameterParser parse() {
        map = new HashMap<String, CommandSwitch>();
        list = new ArrayList<CommandSwitch>();
        this.parse(getRawArguments());
        return this;
    }

    public ParameterParser parse(File file) throws IOException {
        parse();
        if (file != null && file.isFile()) {
            this.parse(ShellParser.splitCommandString(IO.readFileToString(file).replaceAll("[\r\n]", " ")).toArray(new String[] {}));
        }
        return this;
    }

    /**
     * @param startArguments2
     */
    private void parse(final String[] startArguments) {
        String switchCommand = null;
        final ArrayList<String> params = new ArrayList<String>();
        for (String var : startArguments) {
            if (var.startsWith("-")) {
                while (var.length() > 0 && var.startsWith("-")) {
                    var = var.substring(1);
                }
                if (switchCommand != null || params.size() > 0) {
                    final CommandSwitch commandSwitch = new CommandSwitch(switchCommand, params.toArray(new String[] {}));
                    map.put(commandSwitch.getCaseInsensitiveSwitchCommand(), commandSwitch);
                    list.add(commandSwitch);
                }
                switchCommand = var;
                params.clear();
            } else {
                params.add(var);
            }
        }
        if (switchCommand != null || params.size() > 0) {
            final CommandSwitch commandSwitch = new CommandSwitch(switchCommand, params.toArray(new String[] {}));
            map.put(commandSwitch.getCaseInsensitiveSwitchCommand(), commandSwitch);
            list.add(commandSwitch);
        }
    }

    public ArrayList<CommandSwitch> getList() {
        return list;
    }

    /**
     * @param string
     * @param i
     * @param j
     * @return
     */
    public boolean hasCommandSwitch(String string, int minParameters, int maxParameters) {
        final CommandSwitch s = getCommandSwitch(string);
        return s != null && s.getParameters().length >= minParameters && s.getParameters().length <= maxParameters;
    }

    /**
     * @param string
     * @param i
     * @return
     */
    public String getParameter(String key, int index, String def) {
        try {
            return getMandatoryParameter(key, index);
        } catch (ParameterMissingException e) {
            return def;
        }
    }

    /**
     * @param string
     * @param i
     * @return
     * @throws ParameterMissingException
     */
    public String getMandatoryParameter(String key, int index) throws ParameterMissingException {
        if (index < 0) {
            throw new ParameterMissingException(key, index);
        }
        final CommandSwitch s = getCommandSwitch(key);
        if (s == null) {
            throw new ParameterMissingException(key, index);
        } else if (s.getParameters().length <= index) {
            throw new ParameterMissingException(key, index);
        } else {
            return s.getParameters()[index];
        }
    }

    public boolean hasParameter(String key, int index) {
        final CommandSwitch s = getCommandSwitch(key);
        if (s == null) {
            return false;
        } else if (s.getParameters().length <= index) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * @param string
     * @param string2
     * @return
     * @throws AmbigiousArgumentsException
     */
    public CommandSwitch getCommandByKeyOrSynonyms(String... synonyms) throws AmbigiousArgumentsException {
        CommandSwitch ret = null;
        for (String key : synonyms) {
            CommandSwitch cmd = getCommandSwitch(key);
            if (cmd != null) {
                if (ret != null) {
                    throw new AmbigiousArgumentsException("These switches are pseudonymes. Use them only once: " + Arrays.asList(synonyms));
                } else {
                    ret = cmd;
                }
            }
        }
        return ret;
    }

    /**
     * @param commandSwitch
     */
    public void add(CommandSwitch commandSwitch) {
        if (commandSwitch == null) {
            return;
        }
        // Do we use this method? if yes, think about sync
        DebugMode.debugger();
        this.list.add(commandSwitch);
        this.map.put(commandSwitch.getCaseInsensitiveSwitchCommand(), commandSwitch);
    }
}
