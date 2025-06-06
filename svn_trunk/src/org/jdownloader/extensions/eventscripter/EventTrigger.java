package org.jdownloader.extensions.eventscripter;

import java.awt.event.ActionEvent;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.swing.JLabel;

import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.reconnect.Reconnecter.ReconnectResult;
import jd.controlling.reconnect.pluginsinc.liveheader.LiveHeaderReconnect;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.views.settings.components.SettingsButton;
import jd.gui.swing.jdgui.views.settings.components.Spinner;

import org.appwork.storage.config.annotations.LabelInterface;
import org.appwork.utils.Application;
import org.appwork.utils.reflection.Clazz;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.actions.AppAction;
import org.jdownloader.extensions.eventscripter.sandboxobjects.ArchiveSandbox;
import org.jdownloader.extensions.eventscripter.sandboxobjects.CrawledLinkSandbox;
import org.jdownloader.extensions.eventscripter.sandboxobjects.CrawlerJobSandbox;
import org.jdownloader.extensions.eventscripter.sandboxobjects.DownloadLinkSandBox;
import org.jdownloader.extensions.eventscripter.sandboxobjects.DownloadlistSelectionSandbox;
import org.jdownloader.extensions.eventscripter.sandboxobjects.EventSandbox;
import org.jdownloader.extensions.eventscripter.sandboxobjects.FilePackageSandBox;
import org.jdownloader.extensions.eventscripter.sandboxobjects.HTTPProxySandbox;
import org.jdownloader.extensions.eventscripter.sandboxobjects.LinkgrabberSelectionSandbox;
import org.jdownloader.extensions.eventscripter.sandboxobjects.PackagizerLinkSandbox;
import org.jdownloader.gui.jdtrayicon.MenuManagerTrayIcon;
import org.jdownloader.gui.mainmenu.MenuManagerMainmenu;
import org.jdownloader.gui.toolbar.MenuManagerMainToolbar;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.ArraySet;
import org.jdownloader.gui.views.downloads.MenuManagerDownloadTabBottomBar;
import org.jdownloader.gui.views.downloads.contextmenumanager.MenuManagerDownloadTableContext;
import org.jdownloader.gui.views.linkgrabber.bottombar.MenuManagerLinkgrabberTabBottombar;
import org.jdownloader.gui.views.linkgrabber.contextmenu.MenuManagerLinkgrabberTableContext;

public enum EventTrigger implements LabelInterface {
    ON_DOWNLOAD_CONTROLLER_START {
        @Override
        public String getLabel() {
            return T.T.ON_DOWNLOAD_CONTROLLER_START();
        }

        @Override
        public boolean isSynchronousSupported() {
            return true;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> ret = new HashMap<String, Object>();
            ret.put("link", new DownloadLinkSandBox());
            ret.put("package", new FilePackageSandBox());
            ret.put("proxy", new HTTPProxySandbox());
            return ret;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    ON_DOWNLOAD_CONTROLLER_STOPPED {
        @Override
        public String getLabel() {
            return T.T.ON_DOWNLOAD_CONTROLLER_STOPPED();
        }

        @Override
        public boolean isSynchronousSupported() {
            return true;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        public HashMap<String, Object> getTestProperties() {
            return ON_DOWNLOAD_CONTROLLER_START.getTestProperties();
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    ON_PACKAGE_FINISHED {
        @Override
        public String getLabel() {
            return T.T.ON_PACKAGE_FINISHED();
        }

        @Override
        public boolean isSynchronousSupported() {
            return true;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> ret = new HashMap<String, Object>();
            ret.put("link", new DownloadLinkSandBox());
            ret.put("package", new FilePackageSandBox());
            return ret;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    ON_GENERIC_EXTRACTION {
        @Override
        public String getLabel() {
            return T.T.ON_GENERIC_EXTRACTION();
        }

        @Override
        public boolean isSynchronousSupported() {
            return true;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> ret = new HashMap<String, Object>();
            ret.put("archive", new ArchiveSandbox());
            ret.put("event", org.jdownloader.extensions.extraction.ExtractionEvent.Type.PASSWORD_FOUND.name());
            return ret;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    ON_ARCHIVE_EXTRACTED {
        @Override
        public String getLabel() {
            return T.T.ON_ARCHIVE_EXTRACTED();
        }

        @Override
        public boolean isSynchronousSupported() {
            return true;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> ret = new HashMap<String, Object>();
            ret.put("archive", new ArchiveSandbox());
            return ret;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    ON_JDOWNLOADER_STARTED {
        @Override
        public boolean isSynchronousSupported() {
            return false;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        @Override
        public String getLabel() {
            return T.T.ON_JDOWNLOADER_STARTED();
        }

        public HashMap<String, Object> getTestProperties() {
            return NONE.getTestProperties();
        }

        public String getAPIDescription() {
            return NONE.getAPIDescription();
        }
    },
    NONE {
        @Override
        public boolean isSynchronousSupported() {
            return false;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        @Override
        public String getLabel() {
            return T.T.NONE();
        }

        @Override
        public HashMap<String, Object> getTestProperties() {
            return new HashMap<String, Object>();
        }
    },
    ON_OUTGOING_REMOTE_API_EVENT {
        @Override
        public boolean isSynchronousSupported() {
            return false;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        @Override
        public String getLabel() {
            return T.T.ON_OUTGOING_REMOTE_API_EVENT();
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> props = new HashMap<String, Object>();
            props.put("event", new EventSandbox());
            return props;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    ON_NEW_FILE {
        @Override
        public boolean isSynchronousSupported() {
            return false;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        @Override
        public String getLabel() {
            return T.T.ON_NEW_FILE();
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> props = new HashMap<String, Object>();
            props.put("files", new String[] { Application.getResource("license.txt").getAbsolutePath() });
            props.put("caller", DownloadController.class.getName());
            return props;
        }

        public String getAPIDescription() {
            final StringBuilder sb = new StringBuilder();
            sb.append(T.T.properties_for_eventtrigger(getLabel())).append("\r\n");
            sb.append("var myStringArray=files;").append("\r\n");
            sb.append("var myString=caller; /*Who created the files*/").append("\r\n");
            return sb.toString();
        }
    },
    ON_NEW_CRAWLER_JOB {
        @Override
        public String getLabel() {
            return T.T.ON_NEW_CRAWLER_JOB();
        }

        @Override
        public boolean isDefaultSynchronous() {
            return true;
        }

        @Override
        public boolean isSynchronousSupported() {
            return true;
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> props = new HashMap<String, Object>();
            props.put("job", new CrawlerJobSandbox());
            return props;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    ON_NEW_LINK {
        @Override
        public String getLabel() {
            return T.T.ON_NEW_LINK();
        }

        @Override
        public boolean isSynchronousSupported() {
            return true;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return true;
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> props = new HashMap<String, Object>();
            props.put("link", new PackagizerLinkSandbox());
            props.put("crawledLink", new CrawledLinkSandbox());
            return props;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    ON_PACKAGIZER {
        @Override
        public String getLabel() {
            return T.T.ON_PACKAGIZER();
        }

        @Override
        public boolean isSynchronousSupported() {
            return true;
        }

        public boolean isDefaultSynchronous() {
            return true;
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> props = new HashMap<String, Object>();
            props.put("linkcheckDone", true);
            props.put("state", "BEFORE");
            props.put("link", new PackagizerLinkSandbox());
            return props;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    ON_DOWNLOADS_PAUSE {
        @Override
        public boolean isSynchronousSupported() {
            return false;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        @Override
        public String getLabel() {
            return T.T.ON_DOWNLOADS_PAUSE();
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> props = new HashMap<String, Object>();
            return props;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    ON_DOWNLOADS_RUNNING {
        @Override
        public boolean isSynchronousSupported() {
            return false;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        @Override
        public String getLabel() {
            return T.T.ON_DOWNLOADS_RUNNING();
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> props = new HashMap<String, Object>();
            return props;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    ON_DOWNLOADS_STOPPED {
        @Override
        public boolean isSynchronousSupported() {
            return false;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        @Override
        public String getLabel() {
            return T.T.ON_DOWNLOADS_STOPPED();
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> props = new HashMap<String, Object>();
            return props;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    RECONNECT_BEFORE {
        @Override
        public String getLabel() {
            return T.T.RECONNECT_BEFORE();
        }

        @Override
        public boolean isSynchronousSupported() {
            return true;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return true;
        }

        public HashMap<String, Object> getTestProperties() {
            HashMap<String, Object> props = new HashMap<String, Object>();
            props.put("method", LiveHeaderReconnect.class.getSimpleName());
            return props;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    RECONNECT_AFTER {
        @Override
        public String getLabel() {
            return T.T.RECONNECT_AFTER();
        }

        @Override
        public boolean isSynchronousSupported() {
            return true;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return true;
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> props = new HashMap<String, Object>();
            props.put("result", ReconnectResult.SUCCESSFUL.name());
            props.put("method", LiveHeaderReconnect.class.getSimpleName());
            return props;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    CAPTCHA_CHALLENGE_BEFORE {
        @Override
        public String getLabel() {
            return T.T.CAPTCHA_CHALLENGE_BEFORE();
        }

        @Override
        public boolean isSynchronousSupported() {
            return true;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return true;
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> props = new HashMap<String, Object>();
            props.put("hasPendingJobs", Boolean.FALSE);
            props.put("getCaptchaName", "");
            props.put("getCaptchaHost", "jdownloader.org");
            props.put("isAccountCheck", Boolean.FALSE);
            props.put("link", new DownloadLinkSandBox());
            return props;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    CAPTCHA_CHALLENGE_AFTER {
        @Override
        public String getLabel() {
            return T.T.CAPTCHA_CHALLENGE_AFTER();
        }

        @Override
        public boolean isSynchronousSupported() {
            return true;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return true;
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> props = CAPTCHA_CHALLENGE_BEFORE.getTestProperties();
            props.put("solver", new String[] { "jac.JACSolver", "dialog.DialogBasicCaptchaSolver", "9kw.Captcha9kwSolver" });
            props.put("solved", true);
            props.put("result", "dummy");
            return props;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    INTERVAL {
        @Override
        public String getLabel() {
            return T.T.INTERVAL();
        }

        @Override
        public TriggerSetupPanel createSettingsPanel(final ScriptEntry entry, final Map<String, Object> testEventTriggerSettings, JavaScriptEditorDialog editorDialog) {
            final TriggerSetupPanel ret = super.createSettingsPanel(entry, testEventTriggerSettings, editorDialog);
            final Spinner spinner = new Spinner(1000, Integer.MAX_VALUE);
            try {
                spinner.setValue(((Number) entry.getEventTriggerSettings().get("interval")).intValue());
            } catch (Throwable e) {
                spinner.setValue(1000);
            }
            ret.executeOnSave(new Runnable() {
                public void run() {
                    entry.getEventTriggerSettings().put("interval", ((Number) spinner.getValue()).intValue());
                };
            });
            ret.executeOnTestRun(new Runnable() {
                public void run() {
                    testEventTriggerSettings.put("interval", ((Number) spinner.getValue()).intValue());
                };
            });
            ret.addPair(T.T.interval_settings(), null, spinner);
            return ret;
        }

        @Override
        public boolean isSynchronousSupported() {
            return true;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        public HashMap<String, Object> getTestProperties() {
            final HashMap<String, Object> props = new HashMap<String, Object>();
            props.put("interval", 1000);
            return props;
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    TOOLBAR_BUTTON {
        @Override
        public boolean isSynchronousSupported() {
            return false;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        @Override
        public String getLabel() {
            return T.T.TOOLBAR_BUTTON();
        }

        @Override
        public TriggerSetupPanel createSettingsPanel(ScriptEntry entry, final Map<String, Object> testEventTriggerSettings, JavaScriptEditorDialog editorDialog) {
            final TriggerSetupPanel ret = super.createSettingsPanel(entry, testEventTriggerSettings, editorDialog);
            ret.add(new JLabel(T.T.TOOLBAR_BUTTON_explain()), "spanx");
            SettingsButton toolbarManager = new SettingsButton(new AppAction() {
                {
                    setName(_GUI.T.gui_config_menumanager_toolbar());
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            MenuManagerMainToolbar.getInstance().openGui();
                        }
                    };
                }
            });
            ret.addPair("", null, toolbarManager);
            return ret;
        }

        public HashMap<String, Object> getTestProperties() {
            return getContextTriggerProperties(true, true);
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    MAIN_MENU_BUTTON {
        @Override
        public boolean isSynchronousSupported() {
            return false;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        @Override
        public String getLabel() {
            return T.T.MAIN_MENU_BUTTON();
        }

        @Override
        public TriggerSetupPanel createSettingsPanel(ScriptEntry entry, final Map<String, Object> testEventTriggerSettings, JavaScriptEditorDialog editorDialog) {
            final TriggerSetupPanel ret = super.createSettingsPanel(entry, testEventTriggerSettings, editorDialog);
            ret.add(new JLabel(T.T.MAIN_MENU_BUTTON_explain()), "spanx");
            ret.addPair("", null, new SettingsButton(new AppAction() {
                {
                    setName(_GUI.T.gui_config_menumanager_mainmenu());
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            MenuManagerMainmenu.getInstance().openGui();
                        }
                    };
                }
            }));
            return ret;
        }

        public HashMap<String, Object> getTestProperties() {
            return getContextTriggerProperties(true, true);
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    DOWNLOAD_TABLE_CONTEXT_MENU_BUTTON {
        @Override
        public boolean isSynchronousSupported() {
            return false;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        @Override
        public String getLabel() {
            return T.T.DOWNLOAD_TABLE_CONTEXT_MENU_BUTTON();
        }

        @Override
        public TriggerSetupPanel createSettingsPanel(ScriptEntry entry, final Map<String, Object> testEventTriggerSettings, JavaScriptEditorDialog editorDialog) {
            final TriggerSetupPanel ret = super.createSettingsPanel(entry, testEventTriggerSettings, editorDialog);
            ret.add(new JLabel(T.T.DOWNLOAD_TABLE_CONTEXT_MENU_BUTTON_explain()), "spanx");
            ret.addPair("", null, new SettingsButton(new AppAction() {
                {
                    setName(_GUI.T.gui_config_menumanager_downloadlist());
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            JDGui.getInstance().requestPanel(JDGui.Panels.DOWNLOADLIST);
                            MenuManagerDownloadTableContext.getInstance().openGui();
                        }
                    };
                }
            }));
            return ret;
        }

        public HashMap<String, Object> getTestProperties() {
            return getContextTriggerProperties(true, false);
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    LINKGRABBER_TABLE_CONTEXT_MENU_BUTTON {
        @Override
        public boolean isSynchronousSupported() {
            return false;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        @Override
        public String getLabel() {
            return T.T.LINKGRABBER_TABLE_CONTEXT_MENU_BUTTON();
        }

        @Override
        public TriggerSetupPanel createSettingsPanel(ScriptEntry entry, final Map<String, Object> testEventTriggerSettings, JavaScriptEditorDialog editorDialog) {
            final TriggerSetupPanel ret = super.createSettingsPanel(entry, testEventTriggerSettings, editorDialog);
            ret.add(new JLabel(T.T.LINKGRABBER_TABLE_CONTEXT_MENU_BUTTON_explain()), "spanx");
            ret.addPair("", null, new SettingsButton(new AppAction() {
                {
                    setName(_GUI.T.gui_config_menumanager_linkgrabber());
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            JDGui.getInstance().requestPanel(JDGui.Panels.LINKGRABBER);
                            MenuManagerLinkgrabberTableContext.getInstance().openGui();
                        }
                    };
                }
            }));
            return ret;
        }

        public HashMap<String, Object> getTestProperties() {
            return getContextTriggerProperties(false, true);
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    DOWNLOAD_TABLE_BOTTOM_BAR_BUTTON {
        @Override
        public boolean isSynchronousSupported() {
            return false;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        @Override
        public String getLabel() {
            return T.T.DOWNLOAD_TABLE_BOTTOM_BAR_BUTTON();
        }

        @Override
        public TriggerSetupPanel createSettingsPanel(ScriptEntry entry, final Map<String, Object> testEventTriggerSettings, JavaScriptEditorDialog editorDialog) {
            final TriggerSetupPanel ret = super.createSettingsPanel(entry, testEventTriggerSettings, editorDialog);
            ret.add(new JLabel(T.T.DOWNLOAD_TABLE_BOTTOM_BAR_BUTTON_explain()), "spanx");
            ret.addPair("", null, new SettingsButton(new AppAction() {
                {
                    setName(_GUI.T.gui_config_menumanager_downloadBottom());
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            MenuManagerDownloadTabBottomBar.getInstance().openGui();
                        }
                    };
                }
            }));
            return ret;
        }

        public HashMap<String, Object> getTestProperties() {
            return getContextTriggerProperties(true, false);
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    LINKGRABBER_BOTTOM_BAR_BUTTON {
        @Override
        public boolean isSynchronousSupported() {
            return false;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        @Override
        public String getLabel() {
            return T.T.LINKGRABBER_BOTTOM_BAR_BUTTON();
        }

        @Override
        public TriggerSetupPanel createSettingsPanel(ScriptEntry entry, final Map<String, Object> testEventTriggerSettings, JavaScriptEditorDialog editorDialog) {
            final TriggerSetupPanel ret = super.createSettingsPanel(entry, testEventTriggerSettings, editorDialog);
            ret.add(new JLabel(T.T.LINKGRABBER_BOTTOM_BAR_BUTTON_explain()), "spanx");
            ret.addPair("", null, new SettingsButton(new AppAction() {
                {
                    setName(_GUI.T.gui_config_menumanager_linkgrabberBottom());
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            MenuManagerLinkgrabberTabBottombar.getInstance().openGui();
                        }
                    };
                }
            }));
            return ret;
        }

        public HashMap<String, Object> getTestProperties() {
            return getContextTriggerProperties(false, true);
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    },
    TRAY_BUTTON {
        @Override
        public boolean isSynchronousSupported() {
            return false;
        }

        @Override
        public boolean isDefaultSynchronous() {
            return false;
        }

        @Override
        public String getLabel() {
            return T.T.TRAY_BUTTON();
        }

        @Override
        public TriggerSetupPanel createSettingsPanel(ScriptEntry entry, final Map<String, Object> testEventTriggerSettings, JavaScriptEditorDialog editorDialog) {
            final TriggerSetupPanel ret = super.createSettingsPanel(entry, testEventTriggerSettings, editorDialog);
            ret.add(new JLabel(T.T.TRAY_BUTTON_explain()), "spanx");
            ret.addPair("", null, new SettingsButton(new AppAction() {
                {
                    setName(_GUI.T.gui_config_menumanager_traymenu());
                }

                @Override
                public void actionPerformed(ActionEvent e) {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            MenuManagerTrayIcon.getInstance().openGui();
                        }
                    };
                }
            }));
            return ret;
        }

        public HashMap<String, Object> getTestProperties() {
            return getContextTriggerProperties(true, true);
        }

        public String getAPIDescription() {
            return defaultAPIDescription(this);
        }
    };
    public String getAPIDescription() {
        return T.T.none_trigger();
    }

    protected static String defaultAPIDescription(EventTrigger eventTrigger) {
        final StringBuilder sb = new StringBuilder();
        sb.append(T.T.properties_for_eventtrigger(eventTrigger.getLabel())).append("\r\n");
        for (final Entry<String, Object> es : eventTrigger.getTestProperties().entrySet()) {
            sb.append("var ").append(Utils.toMy(Utils.cleanUpClass(es.getValue().getClass().getSimpleName()))).append(" = ").append(es.getKey()).append(";").append("\r\n");
        }
        return sb.toString();
    }

    public static HashMap<String, Object> getContextTriggerProperties(boolean includeDownloadList, boolean includeLinkgrabberList) {
        final HashMap<String, Object> props = new HashMap<String, Object>();
        props.put("name", "MyMenuButton");
        props.put("icon", "myIconKey");
        props.put("shortCutString", "myShortcut");
        props.put("menu", "TriggerName");
        if (includeDownloadList) {
            props.put("dlSelection", new DownloadlistSelectionSandbox());
        }
        if (includeLinkgrabberList) {
            props.put("lgSelection", new LinkgrabberSelectionSandbox());
        }
        return props;
    }

    protected static void collectClasses(Class<? extends Object> cl, ArraySet<Class<?>> clazzes) {
        if (Clazz.isString(cl)) {
            return;
        }
        if (Clazz.isPrimitive(cl)) {
            return;
        }
        for (Method m : cl.getDeclaredMethods()) {
            if (m.getReturnType() == Object.class || !Modifier.isPublic(m.getModifiers()) || Clazz.isPrimitive(m.getReturnType()) || Clazz.isPrimitiveWrapper(m.getReturnType()) || Clazz.isString(m.getReturnType())) {
                continue;
            }
            if (clazzes.add(m.getReturnType())) {
                collectClasses(m.getReturnType(), clazzes);
            }
            for (Class<?> cl2 : m.getParameterTypes()) {
                if (cl2 == Object.class || Clazz.isPrimitive(cl2) || Clazz.isPrimitiveWrapper(cl2) || Clazz.isString(cl2)) {
                    continue;
                }
                if (clazzes.add(cl2)) {
                    collectClasses(cl2, clazzes);
                }
            }
        }
    }

    public abstract HashMap<String, Object> getTestProperties();

    public ArraySet<Class<?>> getAPIClasses() {
        ArraySet<Class<?>> clazzes = new ArraySet<Class<?>>();
        for (Entry<String, Object> es : getTestProperties().entrySet()) {
            clazzes.add(es.getValue().getClass());
            collectClasses(es.getValue().getClass(), clazzes);
        }
        return clazzes;
    }

    public abstract boolean isSynchronousSupported();

    public abstract boolean isDefaultSynchronous();

    public void setSynchronous(Map<String, Object> settings, boolean isSynchronous) {
        if (settings != null) {
            if (isSynchronousSupported()) {
                settings.put("isSynchronous", isSynchronous);
            } else {
                settings.remove("isSynchronous");
            }
        }
    }

    public boolean isSynchronous(final Map<String, Object> settings) {
        if (!isSynchronousSupported()) {
            return false;
        } else {
            if (settings != null) {
                final Object isSynchronous = settings.get("isSynchronous");
                if (isSynchronous instanceof Boolean) {
                    return ((Boolean) isSynchronous).booleanValue();
                } else if (isSynchronous instanceof String && ("true".equalsIgnoreCase((String) isSynchronous) || "false".equalsIgnoreCase((String) isSynchronous))) {
                    return Boolean.parseBoolean((String) isSynchronous);
                }
            }
            return isDefaultSynchronous();
        }
    }

    public TriggerSetupPanel createSettingsPanel(final ScriptEntry entry, final Map<String, Object> testEventTriggerSettings, final JavaScriptEditorDialog editorDialog) {
        return editorDialog.createDefaultTriggerSetupPanel(entry, testEventTriggerSettings);
    }
}
