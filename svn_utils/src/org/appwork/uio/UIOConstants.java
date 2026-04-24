/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.uio;

import org.appwork.utils.swing.dialog.InputDialog;

/**
 * @author daniel
 * @date Apr 14, 2026
 *
 */
public final class UIOConstants {
    public static final class STYLE {
        /**
         * Do Not use an Icon. By default dialogs have an Icon
         */
        public static final int STYLE_HIDE_ICON                 = 1 << 8;
        /**
         * Some dialogs are able to render HTML. Use this switch to enable html
         */
        public static final int STYLE_HTML                      = 1 << 7;
        /**
         * Some dialogs are able to layout themselves in a large mode. E:g. to display a huge text.
         */
        public static final int STYLE_LARGE                     = 1 << 6;
        /**
         * Displays a Checkbox with "do not show this again" text. If the user selects this box, the UserInteraktion class will remember the
         * answer and will not disturb the user with the same question (same title)
         */
        public static final int STYLE_SHOW_DO_NOT_DISPLAY_AGAIN = 1 << 5;
        /**
         * Inputdialogs will use passwordfields instead of textfields
         */
        public static final int STYLE_PASSWORD                  = 1 << 9;
        /**
         * {@link InputDialog}: do not write the default input value to debug logs (use for passwords and other secrets).
         */
        public static final int STYLE_SENSITIVE_INPUT           = 1 << 13;
    }

    public static final class LOGIC {
        /**
         * Use this flag to show display of the Timer
         */
        public static final int LOGIC_COUNTDOWN                      = 1 << 2;
        /**
         * Don't show again is only valid for this session, but is not saved for further sessions
         */
        public static final int LOGIC_DONT_SHOW_AGAIN_DELETE_ON_EXIT = 1 << 11;
        /**
         * Often, the {@link UIOConstants.STYLE#STYLE_SHOW_DO_NOT_DISPLAY_AGAIN} option does not make sense for the cancel option. Use this
         * flag if the option should be ignored if the user selects Cancel
         */
        public static final int LOGIC_DONT_SHOW_AGAIN_IGNORES_CANCEL = 1 << 9;
        /**
         * Often, the {@link UIOConstants.STYLE#STYLE_SHOW_DO_NOT_DISPLAY_AGAIN} option does not make sense for the ok option. Use this flag
         * if the option should be ignored if the user selects OK
         */
        public static final int LOGIC_DONT_SHOW_AGAIN_IGNORES_OK     = 1 << 10;
        /**
         * If set, the dialog does not handle do not show again - just shows the ui and sets the flag if the user clicked it. this can be
         * used to handle the result externaly
         */
        public static final int LOGIC_DONT_SHOW_AGAIN_GUI_ONLY       = 1 << 12;
    }

    public static final class RETURN {
        /**
         * if the user pressed cancel, the return mask will contain this mask
         */
        public static final int RETURN_CANCEL               = 1 << 2;
        /**
         * if user closed the window
         */
        public static final int RETURN_CLOSED               = 1 << 6;
        public static final int RETURN_INTERRUPT            = 1 << 8;
        public static final int RETURN_EXCEPTION            = 1 << 9;
        /**
         * this return flag can be set in two situations:<br>
         * a) The user selected the {@link UIOConstants.STYLE#STYLE_SHOW_DO_NOT_DISPLAY_AGAIN} Option<br>
         * b) The dialog has been skipped because the DO NOT SHOW AGAIN flag has been set previously<br>
         * <br>
         * Check {@link #RETURN_SKIPPED_BY_DONT_SHOW} to know if the dialog has been visible or autoskipped
         */
        public static final int RETURN_DONT_SHOW_AGAIN      = 1 << 3;
        /**
         * If the user pressed OK, the return mask will contain this flag
         */
        public static final int RETURN_OK                   = 1 << 1;
        /**
         * If the dialog has been skipped due to previously selected {@link UIOConstants.STYLE#STYLE_SHOW_DO_NOT_DISPLAY_AGAIN} Option, this
         * return flag is set.
         *
         * @see #RETURN_DONT_SHOW_AGAIN
         */
        public static final int RETURN_SKIPPED_BY_DONT_SHOW = 1 << 4;
        /**
         * If the Timeout ({@link UIOConstants.LOGIC#LOGIC_COUNTDOWN}) has run out, the return mask contains this flag
         */
        public static final int RETURN_TIMEOUT              = 1 << 5;
        /**
         * If the dialog has been skiped/closed by ESC key
         */
        public static final int RETURN_ESC                  = 1 << 7;
    }

    public static final class BUTTONS {
        /**
         * Hide the cancel Button
         */
        public static final int BUTTONS_HIDE_CANCEL = 1 << 4;
        /**
         * Hide the OK button
         */
        public static final int BUTTONS_HIDE_OK     = 1 << 3;
    }
}
