//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//     along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.gui.swing.laf;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import javax.swing.AbstractAction;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.KeyStroke;
import javax.swing.LookAndFeel;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.text.JTextComponent;
import javax.swing.text.Keymap;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.swing.components.tooltips.ExtTooltip;
import org.appwork.swing.synthetica.SyntheticaHelper;
import org.appwork.uio.UIOManager;
import org.appwork.utils.Application;
import org.appwork.utils.Files;
import org.appwork.utils.IO;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.logging2.extmanager.LoggerFactory;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.LAFManagerInterface;
import org.appwork.utils.swing.windowmanager.WindowManager;
import org.appwork.utils.swing.windowmanager.WindowsWindowManager;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.logging.LogController;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;
import org.jdownloader.settings.staticreferences.CFG_GUI;
import org.jdownloader.updatev2.UpdateController;
import org.jdownloader.updatev2.gui.LAFOptions;
import org.jdownloader.updatev2.gui.LookAndFeelType;

import jd.SecondLevelLaunch;

public class LookAndFeelController implements LAFManagerInterface {
    private static final LookAndFeelController INSTANCE = new LookAndFeelController();

    /**
     * get the only existing instance of LookAndFeelController. This is a singleton
     *
     * @return
     */
    public static LookAndFeelController getInstance() {
        return LookAndFeelController.INSTANCE;
    }

    private GraphicalUserInterfaceSettings config;
    private LogSource                      logger;

    /**
     * Create a new instance of LookAndFeelController. This is a singleton class. Access the only existing instance by using
     * {@link #getInstance()}.
     */
    private LookAndFeelController() {
        config = JsonConfig.create(GraphicalUserInterfaceSettings.class);
        logger = LogController.getInstance().getLogger(getClass().getName());
        SecondLevelLaunch.UPDATE_HANDLER_SET.executeWhenReached(new Runnable() {
            @Override
            public void run() {
                CFG_GUI.LOOK_AND_FEEL_THEME.getEventSender().addListener(new GenericConfigEventListener<Enum>() {
                    @Override
                    public void onConfigValueModified(KeyHandler<Enum> keyHandler, Enum newValue) {
                        handleThemesInstallation();
                    }

                    @Override
                    public void onConfigValidatorError(KeyHandler<Enum> keyHandler, Enum invalidValue, ValidationException validateException) {
                    }
                });
                handleThemesInstallation();
            }
        });
    }

    protected void handleThemesInstallation() {
        if (UpdateController.getInstance().getHandler() == null) {
            return;
        }
        LookAndFeelType lafTheme = CFG_GUI.CFG.getLookAndFeelTheme();
        if (lafTheme == null) {
            lafTheme = LookAndFeelType.DEFAULT;
            CFG_GUI.CFG.setLookAndFeelTheme(lafTheme);
        }
        if (LookAndFeelType.DEFAULT.equals(lafTheme) || lafTheme.getExtensionID() == null) {
            return;
        } else if (!lafTheme.isSupported()) {
            return;
        } else if (UpdateController.getInstance().isExtensionInstalled(lafTheme.getExtensionID()) && lafTheme.isAvailable()) {
            return;
        } else if (UIOManager.I().showConfirmDialog(0, _GUI.T.LookAndFeelController_handleThemesInstallation_title_(), _GUI.T.LookAndFeelController_handleThemesInstallation_message_(lafTheme.name()), new AbstractIcon(IconKey.ICON_UPDATERICON0, 64), null, null)) {
            final LookAndFeelType finalLafTheme = lafTheme;
            new Thread("Install Extension") {
                public void run() {
                    try {
                        UpdateController.getInstance().setGuiVisible(true);
                        if (UpdateController.getInstance().isExtensionInstalled(finalLafTheme.getExtensionID())) {
                            // reinstall laf as isAvailable did return false
                            UpdateController.getInstance().runExtensionsFullUpdate(new ArrayList<String>(Arrays.asList(finalLafTheme.getExtensionID())));
                        } else {
                            UpdateController.getInstance().runExtensionInstallation(finalLafTheme.getExtensionID());
                        }
                        while (true) {
                            Thread.sleep(500);
                            if (!UpdateController.getInstance().isRunning()) {
                                break;
                            }
                            UpdateController.getInstance().waitForUpdate();
                        }
                    } catch (Exception e) {
                        org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
                    }
                }
            }.start();
        } else {
            CFG_GUI.CFG.setLookAndFeelTheme(LookAndFeelType.DEFAULT);
        }
    }

    /**
     * Config parameter to store the users laf selection
     */
    public static final String DEFAULT_PREFIX = "LAF_CFG";
    private static boolean     uiInitated     = false;

    private void initLookAndFeel(final LookAndFeel laf) {
        if (ReflectionUtils.isInstanceOf("de.javasoft.plaf.synthetica.SyntheticaLookAndFeel", laf)) {
            ExtTooltip.setForgroundColor(LAFOptions.getInstance().getColorForTooltipForeground());
            UIManager.put(/* ExtTooltip.APPWORK_TOOLTIP_FOREGROUND */"Appwork.Tooltip.Foreground", LAFOptions.getInstance().getColorForTooltipForeground());
        } else if (ReflectionUtils.isInstanceOf("com.formdev.flatlaf.FlatLaf", laf)) {
            // required to activate interpolation on icons
            // AWUTheme.setFACTORY(new FlatLafIconFactory());
            UIManager.put("ScrollBar.width", 14);
            // https://www.formdev.com/flatlaf/window-decorations
            // https://www.formdev.com/flatlaf/system-properties/#flatlaf.useWindowDecorations
            if (LAFOptions.getInstance().getCfg().isWindowDecorationEnabled()) {
                System.setProperty("flatlaf.useWindowDecorations", "true");
                JFrame.setDefaultLookAndFeelDecorated(true);
                JDialog.setDefaultLookAndFeelDecorated(true);
            } else {
                System.setProperty("flatlaf.useWindowDecorations", "false");
                JFrame.setDefaultLookAndFeelDecorated(false);
                JDialog.setDefaultLookAndFeelDecorated(false);
            }
        }
    }

    private void addJarsToClasspath(List<File> jars) throws IOException {
        if (jars == null || jars.size() == 0) {
            return;
        }
        final class ClassPathChecker {
            public boolean isJarInClasspath(String jarName) {
                try {
                    final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
                    final Enumeration<URL> urls = classLoader.getResources("META-INF/");
                    while (urls.hasMoreElements()) {
                        final URL url = urls.nextElement();
                        if (url.toString().contains(jarName)) {
                            return true;
                        }
                    }
                } catch (Throwable e) {
                    e.printStackTrace();
                }
                return false;
            }
        }
        final ClassPathChecker cpc = new ClassPathChecker();
        // copy the libs to a tmp dir. we do not want to block them
        for (final File jar : jars) {
            if (!jar.isFile()) {
                continue;
            } else if (cpc.isJarInClasspath(jar.getName())) {
                continue;
            }
            logger.info("Add to classpath: " + jar);
            final String rel = Files.getRelativePath(Application.getTemp().getParentFile(), jar);
            // we use the timestamp here. this might result in problems on linux and mac systems
            final File tmp = new File(Application.getTempResource("synthlibs"), rel + ".ts" + jar.lastModified());
            if (!tmp.exists()) {
                try {
                    // cleanup old versions
                    if (tmp.getParentFile().exists()) {
                        for (final File f : tmp.getParentFile().listFiles(new FileFilter() {
                            @Override
                            public boolean accept(final File pathname) {
                                return pathname.getName().startsWith(jar.getName() + ".ts");
                            }
                        })) {
                            f.delete();
                        }
                    }
                } catch (final Exception e) {
                    logger.log(e);
                }
                tmp.getParentFile().mkdirs();
                IO.copyFile(jar, tmp);
            }
            Application.addUrlToClassPath(tmp.toURI().toURL(), LookAndFeelController.class.getClassLoader());
        }
    }

    /**
     * setups the correct Look and Feel
     */
    public synchronized void setUIManager() {
        if (uiInitated) {
            return;
        } else {
            uiInitated = true;
        }
        if (Application.isHeadless()) {
            return;
        }
        initWindowManager();
        installGlobalCaretSelectionFix();
        long t = System.currentTimeMillis();
        try {
            // de.javasoft.plaf.synthetica.SyntheticaLookAndFeel.setLookAndFeel("de.javasoft.plaf.synthetica.SyntheticaStandardLookAndFeel");
            // if (true) return;
            String laf = null;
            try {
                final String customLookAndFeel = config.getCustomLookAndFeelClass();
                if (StringUtils.isNotEmpty(customLookAndFeel)) {
                    try {
                        // copy the libs to a tmp dir. we do not want to block them
                        final File[] lafFiles = Application.getResource("libs/laf/").listFiles();
                        if (lafFiles != null) {
                            try {
                                final ArrayList<File> jars = new ArrayList<File>();
                                for (final File file : lafFiles) {
                                    final String name = file.getName();
                                    if (file.isFile() && name.endsWith(".jar") && !name.startsWith("synthetica")) {
                                        jars.add(file);
                                    }
                                }
                                addJarsToClasspath(jars);
                            } catch (Throwable e) {
                                logger.log(e);
                            }
                        }
                        Class.forName(customLookAndFeel);
                        laf = customLookAndFeel;
                    } catch (Throwable e) {
                        logger.log(e);
                    }
                }
                if (laf == null) {
                    final LookAndFeelType theme = config.getLookAndFeelTheme();
                    if (theme == null || !theme.isAvailable()) {
                        laf = LookAndFeelType.DEFAULT.getClazz();
                    } else {
                        laf = theme.getClazz();
                    }
                }
            } catch (Throwable e) {
                logger.log(e);
                laf = LookAndFeelType.DEFAULT.getClazz();
            } finally {
                org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().info("Use Look & Feel: " + laf);
            }
            if (laf.contains("Synthetica") || laf.equals(LookAndFeelType.DEFAULT.getClazz()) || laf.equals(LookAndFeelType.JD_PLAIN.getClazz())) {
                //
                String liz = null;
                try {
                    URL url = Application.getRessourceURL("cfg/synthetica-license.key");
                    if (url != null) {
                        liz = IO.readURLToString(url);
                    }
                    if (liz == null && !Application.isJared(LookAndFeelController.class)) {
                        // enable the synthetica dev license for people working on our offical repo at svn.jdownloader.org
                        // for all other mirror repos: please do not use our license
                        url = Application.getRessourceURL("");
                        final File bin = new File(url.toURI());
                        final File db = new File(bin.getParent(), ".svn/wc.db");
                        if (db.isFile()) {
                            String str = IO.readFileToString(db);
                            if (str.contains("svn://svn.jdownloader.org/jdownloader") || str.contains("SQLite format")) {
                                str = null;
                                if (Application.getResource("JDownloader.jar").exists()) {
                                    JarFile jf = null;
                                    try {
                                        jf = new JarFile(Application.getResource("JDownloader.jar"));
                                        JarEntry je = jf.getJarEntry("cfg/synthetica-license.key");
                                        liz = IO.readInputStreamToString(jf.getInputStream(je));
                                    } finally {
                                        if (jf != null) {
                                            jf.close();
                                        }
                                    }
                                }
                            }
                        } else {
                            String str = IO.readFileToString(new File(bin.getParent(), ".svn/entries"));
                            if (str != null && str.contains("svn://svn.jdownloader.org/jdownloader/trunk")) {
                                str = null;
                                if (Application.getResource("JDownloader.jar").isFile()) {
                                    JarFile jf = null;
                                    try {
                                        jf = new JarFile(Application.getResource("JDownloader.jar"));
                                        JarEntry je = jf.getJarEntry("cfg/synthetica-license.key");
                                        liz = IO.readInputStreamToString(jf.getInputStream(je));
                                    } finally {
                                        if (jf != null) {
                                            jf.close();
                                        }
                                    }
                                }
                            }
                        }
                    }
                } catch (Exception e) {
                }
                LAFOptions.init(laf);
                if (Application.isHeadless()) {
                    SyntheticaHelper.setLicense(liz);
                } else {
                    new SyntheticaHelper(LAFOptions.getInstance().getCfg()).load(laf, liz);
                }
                initLookAndFeel(UIManager.getLookAndFeel());
            } else {
                /* init for all other laf */
                UIManager.setLookAndFeel(laf);
                LAFOptions.init(laf);
                initLookAndFeel(UIManager.getLookAndFeel());
            }
        } catch (Throwable e) {
            LogV3.log(e);
            try {
                final LookAndFeel currentLaf = UIManager.getLookAndFeel();
                // this may happen if the updater launcher already has set the look and feel.
                if (currentLaf != null && !(currentLaf instanceof MetalLookAndFeel)) {
                    LogV3.info("Don't set System look and feel " + currentLaf + " is already set");
                    return;
                }
                final String systemLookAndFeelClassName = UIManager.getSystemLookAndFeelClassName();
                UIManager.setLookAndFeel(systemLookAndFeelClassName);
                LAFOptions.init(systemLookAndFeelClassName);
                initLookAndFeel(UIManager.getLookAndFeel());
            } catch (ClassNotFoundException e1) {
                e1.printStackTrace();
            } catch (InstantiationException e1) {
                e1.printStackTrace();
            } catch (IllegalAccessException e1) {
                e1.printStackTrace();
            } catch (UnsupportedLookAndFeelException e1) {
                e1.printStackTrace();
            }
        } finally {
            try {
                final String theme = LAFOptions.getInstance().getCfg().getIconSetID();
                org.jdownloader.images.NewTheme.getInstance().setTheme(theme);
                if (!StringUtils.equals("standard", theme) && StringUtils.isNotEmpty(theme) && !StringUtils.startsWithCaseInsensitive(theme, "my-")) {
                    SecondLevelLaunch.UPDATE_HANDLER_SET.executeWhenReached(new Runnable() {
                        @Override
                        public void run() {
                            if (Application.isJared(null)) {
                                SecondLevelLaunch.INIT_COMPLETE.executeWhenReached(new Runnable() {
                                    @Override
                                    public void run() {
                                        final String extensionID = "iconset-" + theme;
                                        if (!UpdateController.getInstance().isExtensionInstalled(extensionID)) {
                                            try {
                                                UpdateController.getInstance().setGuiVisible(true);
                                                UpdateController.getInstance().runExtensionInstallation(extensionID);
                                            } catch (InterruptedException e) {
                                                e.printStackTrace();
                                            }
                                        }
                                    }
                                });
                            }
                        }
                    });
                }
            } catch (Throwable e) {
                LoggerFactory.getDefaultLogger().log(e);
            }
            org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().info("LAF init: " + (System.currentTimeMillis() - t));
        }
    }

    private void initWindowManager() {
        final WindowManager wm = WindowManager.getInstance();
        if (wm instanceof WindowsWindowManager && CrossSystem.isWindows()) {
            final WindowsWindowManager wwm = (WindowsWindowManager) wm;
            wwm.setAltWorkaroundEnabled(CFG_GUI.CFG.isWindowsWindowManagerAltKeyWorkaroundEnabled());
            wwm.setAltWorkaroundKeys(CFG_GUI.CFG.getWindowsWindowManagerAltKeyCombi());
            try {
                CFG_GUI.CFG.setWindowsWindowManagerForegroundLockTimeout(WindowsWindowManager.readForegroundLockTimeout());
            } catch (Exception e) {
                CFG_GUI.CFG.setWindowsWindowManagerForegroundLockTimeout(-1);
                logger.log(e);
            }
            CFG_GUI.WINDOWS_WINDOW_MANAGER_FOREGROUND_LOCK_TIMEOUT.getEventSender().addListener(new GenericConfigEventListener<Integer>() {
                @Override
                public void onConfigValidatorError(KeyHandler<Integer> keyHandler, Integer invalidValue, ValidationException validateException) {
                }

                @Override
                public void onConfigValueModified(KeyHandler<Integer> keyHandler, Integer newValue) {
                    try {
                        if (newValue >= 0 && newValue != WindowsWindowManager.readForegroundLockTimeout()) {
                            WindowsWindowManager.writeForegroundLockTimeout(newValue);
                            Dialog.getInstance().showMessageDialog(_GUI.T.LookAndFeelController_onConfigValueModified_reboot_required());
                        }
                    } catch (Exception e) {
                        logger.log(e);
                        Dialog.getInstance().showExceptionDialog(_GUI.T.lit_error_occured(), e.getMessage(), e);
                    }
                }
            });
            CFG_GUI.WINDOWS_WINDOW_MANAGER_ALT_KEY_WORKAROUND_ENABLED.getEventSender().addListener(new GenericConfigEventListener<Boolean>() {
                @Override
                public void onConfigValidatorError(KeyHandler<Boolean> keyHandler, Boolean invalidValue, ValidationException validateException) {
                }

                @Override
                public void onConfigValueModified(KeyHandler<Boolean> keyHandler, Boolean newValue) {
                    wwm.setAltWorkaroundEnabled(Boolean.TRUE.equals(newValue));
                }
            });
            CFG_GUI.WINDOWS_WINDOW_MANAGER_ALT_KEY_COMBI.getEventSender().addListener(new GenericConfigEventListener<int[]>() {
                @Override
                public void onConfigValueModified(KeyHandler<int[]> keyHandler, int[] newValue) {
                    wwm.setAltWorkaroundKeys(CFG_GUI.CFG.getWindowsWindowManagerAltKeyCombi());
                }

                @Override
                public void onConfigValidatorError(KeyHandler<int[]> keyHandler, int[] invalidValue, ValidationException validateException) {
                }
            });
        }
    }

    /**
     * By default, Swing moves the caret one character past the end/start of the current selection when the right/left arrow key is pressed,
     * instead of just collapsing the selection to its edge (like the Windows Explorer/most other applications do). This is installed on the
     * shared default {@link Keymap} so it applies to every {@link JTextComponent} (JTextField, JTextArea, ...) in the application that does
     * not install its own custom keymap/keybinding for these keys.
     */
    private void installGlobalCaretSelectionFix() {
        final Keymap keymap = JTextComponent.getKeymap(JTextComponent.DEFAULT_KEYMAP);
        if (keymap == null) {
            return;
        }
        installCaretForwardSelectionFix(keymap);
        installCaretBackwardSelectionFix(keymap);
    }

    /**
     * Fixes the right arrow key: if there is an active selection, the caret is placed at the end of the selection.
     *
     * Standard Swing behavior (without this fix): the caret is moved one character to the right of its current position (which, right after
     * a select(0, x) call, is already the end of the selection), effectively landing one character past the end of the selection instead of
     * exactly at its end. For a renamed file this means the caret ends up right behind the "." of the extension instead of right in front
     * of it. This is a long-standing Swing quirk - modern operating systems (Windows Explorer, macOS Finder, ...) and most other native
     * applications simply collapse the caret to the selection's edge on the first arrow key press instead.
     */
    private void installCaretForwardSelectionFix(final Keymap keymap) {
        final KeyStroke keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT, 0);
        keymap.addActionForKeyStroke(keyStroke, new AbstractAction() {
            private static final long serialVersionUID = 1L;

            @Override
            public void actionPerformed(ActionEvent e) {
                final Object source = e.getSource();
                if (!(source instanceof JTextComponent)) {
                    return;
                }
                final JTextComponent textComponent = (JTextComponent) source;
                if (textComponent.getSelectionStart() != textComponent.getSelectionEnd()) {
                    textComponent.setCaretPosition(textComponent.getSelectionEnd());
                    return;
                }
                /*
                 * No selection: there is no standard action to delegate to here. Swing only loads its default keymap's standard key
                 * bindings lazily on first use, and at LAF setup time (when this action is installed) none exist yet to grab a
                 * reference to. So we set the caret position ourselves, one character to the right.
                 */
                final int pos = textComponent.getCaretPosition();
                if (pos < textComponent.getDocument().getLength()) {
                    textComponent.setCaretPosition(pos + 1);
                }
            }
        });
    }

    /**
     * Fixes the left arrow key: if there is an active selection, the caret is placed at the start of the selection.
     *
     * Standard Swing behavior (without this fix): the caret is moved one character to the left of its current position (which, right after
     * a select(0, x) call, is the end of the selection, not the start), effectively landing one character inside the selection instead of
     * exactly at its start. This is a long-standing Swing quirk - modern operating systems (Windows Explorer, macOS Finder, ...) and most
     * other native applications simply collapse the caret to the selection's edge on the first arrow key press instead.
     */
    private void installCaretBackwardSelectionFix(final Keymap keymap) {
        final KeyStroke keyStroke = KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, 0);
        keymap.addActionForKeyStroke(keyStroke, new AbstractAction() {
            private static final long serialVersionUID = 1L;

            @Override
            public void actionPerformed(ActionEvent e) {
                final Object source = e.getSource();
                if (!(source instanceof JTextComponent)) {
                    return;
                }
                final JTextComponent textComponent = (JTextComponent) source;
                if (textComponent.getSelectionStart() != textComponent.getSelectionEnd()) {
                    textComponent.setCaretPosition(textComponent.getSelectionStart());
                    return;
                }
                /* no selection: see installCaretForwardSelectionFix for why we set the caret position ourselves here */
                final int pos = textComponent.getCaretPosition();
                if (pos > 0) {
                    textComponent.setCaretPosition(pos - 1);
                }
            }
        });
    }

    @Override
    public void init() {
        setUIManager();
    }
}
