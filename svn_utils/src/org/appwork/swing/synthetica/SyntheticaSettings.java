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
package org.appwork.swing.synthetica;

import org.appwork.storage.config.ConfigInterface;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultStringValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.RequiresRestart;

/**
 * @author Thomas
 *
 */
public interface SyntheticaSettings extends ConfigInterface {
    @RequiresRestart("an application Restart is Required")
    @AboutConfig
    void setLanguage(String lng);

    String getLanguage();

    @AboutConfig
    @DescriptionForConfigEntry("Font to be used. Default value is default. For foreign chars use e.g. Dialog")
    @DefaultStringValue("default")
    @RequiresRestart("an application Restart is Required")
    String getFontName();

    @AboutConfig
    @DescriptionForConfigEntry("Font scale factor in percent. Default value is 100 which means no font scaling.")
    @DefaultIntValue(100)
    @RequiresRestart("an application Restart is Required")
    int getFontScaleFactor();

    @AboutConfig
    @DescriptionForConfigEntry("Disable animation and all animation threads. Optional value. Default value is true.")
    @DefaultBooleanValue(true)
    @RequiresRestart("an application Restart is Required")
    boolean isAnimationEnabled();

    @AboutConfig
    @DescriptionForConfigEntry("Enable/disable support for system DPI settings. Default value is true.")
    @RequiresRestart("an application Restart is Required")
    boolean isFontRespectsSystemDPI();

    @AboutConfig
    @DescriptionForConfigEntry("Enable/disable window opacity on Java 6u10 and above. A value of 'false' disables window opacity which means that the window corner background which is visible for non-rectangular windows disappear. Furthermore the shadow for popupMenus makes use of real translucent window. Some themes like SyntheticaSimple2D support translucent titlePanes if opacity is disabled. The property is ignored on JRE's below 6u10. Note: It is recommended to activate this feature only if your graphics hardware acceleration is supported by the JVM - a value of 'false' can affect application performance. Default value is false which means the translucency feature is enabled")
    @DefaultBooleanValue(true)
    @RequiresRestart("an application Restart is Required")
    boolean isWindowOpaque();

    void setAnimationEnabled(boolean b);

    void setWindowOpaque(boolean b);

    void setFontName(String name);

    void setFontRespectsSystemDPI(boolean b);

    void setFontScaleFactor(int b);

    @AboutConfig
    @DescriptionForConfigEntry("Paint all labels/text with or without antialias. Default value is false.")
    @DefaultBooleanValue(false)
    @RequiresRestart("an application Restart is Required")
    boolean isTextAntiAliasEnabled();

    void setTextAntiAliasEnabled(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("(JD09 style)Paint the window title bar.")
    @DefaultBooleanValue(false)
    @RequiresRestart("an application Restart is Required")
    boolean isWindowDecorationEnabled();

    void setWindowDecorationEnabled(boolean b);
}
