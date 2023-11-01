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
package org.appwork.uio;

import org.appwork.utils.swing.dialog.Dialog;

public class UIOManager {

    private static volatile UserIOHandlerInterface USERIO = org.appwork.utils.Application.isHeadless() ? new HeadlessDialogHandler() : new BasicDialogHandler();

    public static void setUserIO(final UserIOHandlerInterface io) {
        USERIO = io;
    }

    public static UserIOHandlerInterface I() {
        return USERIO;
    }

    /**
     * Use this flag to show display of the Timer
     */

    public static final int LOGIC_COUNTDOWN                      = 1 << 2;
    /**
     * Don't show again is only valid for this session, but is not saved for further sessions
     */

    public static final int LOGIC_DONT_SHOW_AGAIN_DELETE_ON_EXIT = 1 << 11;
    /**
     * Hide the cancel Button
     */

    public static final int BUTTONS_HIDE_CANCEL                  = 1 << 4;
    /**
     * Hide the OK button
     */

    public static final int BUTTONS_HIDE_OK                      = 1 << 3;
    /**
     * Often, the {@link Dialog#STYLE_SHOW_DO_NOT_DISPLAY_AGAIN} option does not make sense for the cancel option. Use this flag if the
     * option should be ignored if the user selects Cancel
     */

    public static final int LOGIC_DONT_SHOW_AGAIN_IGNORES_CANCEL = 1 << 9;
    /**
     * Often, the {@link Dialog#STYLE_SHOW_DO_NOT_DISPLAY_AGAIN} option does not make sense for the ok option. Use this flag if the option
     * should be ignored if the user selects OK
     */

    public static final int LOGIC_DONT_SHOW_AGAIN_IGNORES_OK     = 1 << 10;
    public static final int STYLE_SHOW_DO_NOT_DISPLAY_AGAIN      = Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN;

}
