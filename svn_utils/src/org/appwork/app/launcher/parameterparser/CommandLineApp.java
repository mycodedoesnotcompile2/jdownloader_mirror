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

import org.appwork.utils.locale._AWU;

/**
 * @author $Author: unknown$
 */
public abstract class CommandLineApp {
    /**
     * Switchcommands (-command)
     */
    private final String[]            commands;
    /**
     * Description of this commandline App. Is printed to stdout on help command
     */
    private String                    description;
    /**
     * Parameters required for this command. String[]{name,description}
     */
    private final java.util.List<String[]> parameters;

    public CommandLineApp(final String... commands) {
        this.commands = commands;
        parameters = new ArrayList<String[]>();
    }

    /**
     * adds a new Parameter to the app
     * 
     * @param name
     *            of the parameter
     * @param description
     *            of the parameter
     */
    public void addParameter(final String name, final String description) {
        parameters.add(new String[] { name, description });

    }

    /**
     * Executes the Apps Features
     * 
     * @param event
     */
    abstract public void execute(CommandSwitch event);

    /**
     * @return the {@link CommandLineApp#commands}
     * @see CommandLineApp#commands
     */
    public String[] getCommands() {
        return commands;
    }

    /**
     * @return the {@link CommandLineApp#description}
     * @see CommandLineApp#description
     */
    public String getDescription() {
        return description;
    }

    /**
     * @return the {@link CommandLineApp#parameters}
     * @see CommandLineApp#parameters
     */
    public java.util.List<String[]> getParameters() {
        return parameters;
    }

    public void onEmptyCommand(final CommandSwitch event) {
    }

    /**
     * @param description
     *            the {@link CommandLineApp#description} to set
     * @see CommandLineApp#description
     */
    public void setDescription(final String description) {
        this.description = description;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append(_AWU.T.COMMANDLINEAPP_COMMAND());
        for (int i = 0; i < commands.length; i++) {
            sb.append(commands[i]);
            if (i < commands.length - 1) {
                sb.append(" / ");
            }
        }
        sb.append(" | ").append(getDescription());
        sb.append("\r\n");
        for (final String[] parameter : parameters) {
            sb.append("    ").append(parameter[0]).append(" | ").append(parameter[1]).append("\r\n");
        }
        sb.append("\r\n");
        return sb.toString();
    }

}
