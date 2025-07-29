package org.jdownloader.extensions.eventscripter;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.Box;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;

import jd.gui.swing.jdgui.views.settings.components.Checkbox;
import jsyntaxpane.DefaultSyntaxKit;
import jsyntaxpane.syntaxkits.JavaSyntaxKit;

import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtButton;
import org.appwork.utils.DebugMode;
import org.appwork.utils.IO;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTHelper;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.dimensor.RememberLastDialogDimension;
import org.appwork.utils.swing.dialog.locator.RememberAbsoluteDialogLocator;
import org.jdownloader.actions.AppAction;
import org.jdownloader.extensions.eventscripter.sandboxobjects.ScriptEnvironment;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.logging.LogController;
import org.jdownloader.scripting.JSRhinoPermissionRestricter;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Script;
import org.mozilla.javascript.ScriptableObject;
import org.mozilla.javascript.tools.shell.Global;

public class JavaScriptEditorDialog extends AbstractDialog<Object> {
    private static final String                   CLEANUP                  = "[^\\w\\d\\(\\)\\+\\-\\[\\]\\;\\,/\\\\]";
    private final ScriptEntry                     entry;
    private final Map<String, Object>             testEventTriggerSettings = new HashMap<String, Object>();
    private JEditorPane                           editor;
    private org.appwork.scheduler.DelayedRunnable delayer;
    private JToolBar                              toolbar;
    private Global                                scope;
    private MigPanel                              p;
    private JScrollPane                           apiScrollbar;
    private JScrollPane                           scrollpane;
    private EventScripterExtension                extension;
    private TriggerSetupPanel                     settingsPanel;

    public JavaScriptEditorDialog(EventScripterExtension extension, ScriptEntry entry) {
        super(Dialog.STYLE_HIDE_ICON, T.T.script_editor_title(entry.getName()), null, _GUI.T.lit_save(), null);
        this.entry = entry;
        this.extension = extension;
        setLocator(new RememberAbsoluteDialogLocator(getClass().getSimpleName()));
        setDimensor(new RememberLastDialogDimension(getClass().getSimpleName()));
    }

    @Override
    protected ScriptEntry createReturnValue() {
        return null;
    }

    @Override
    protected int getPreferredHeight() {
        return 300;
    }

    @Override
    protected int getPreferredWidth() {
        return 1024;
    }

    public TriggerSetupPanel createDefaultTriggerSetupPanel(final ScriptEntry entry, final Map<String, Object> testEventTriggerSettings) {
        final TriggerSetupPanel settingsPanel = new TriggerSetupPanel(0);
        if (entry.getEventTrigger().isSynchronousSupported()) {
            final Map<String, Object> settingsMap = entry.getEventTriggerSettings();
            final Checkbox synchronousCheckBox = new Checkbox(entry.getEventTrigger().isSynchronous(settingsMap));
            settingsPanel.addDescriptionPlain(T.T.synchronous_desc());
            settingsPanel.addPair(T.T.synchronous(), null, synchronousCheckBox);
            settingsPanel.executeOnSave(new Runnable() {
                public void run() {
                    entry.getEventTrigger().setSynchronous(entry.getEventTriggerSettings(), synchronousCheckBox.isSelected());
                };
            });
            settingsPanel.executeOnTestRun(new Runnable() {
                public void run() {
                    entry.getEventTrigger().setSynchronous(testEventTriggerSettings, synchronousCheckBox.isSelected());
                };
            });
        }
        return settingsPanel;
    }

    @Override
    public JComponent layoutDialogContent() {
        // dummy. @see #relayout
        p = new MigPanel("ins 0,wrap 1", "[grow,fill]", "[][][grow,fill][grow,fill]");
        toolbar = new JToolBar();
        toolbar.setRollover(true);
        toolbar.setFloatable(false);
        toolbar.setPreferredSize(new Dimension(-1, 22));
        p.add(toolbar);
        settingsPanel = entry.getEventTrigger().createSettingsPanel(entry, testEventTriggerSettings, this);
        final JEditorPane defaults = new JEditorPane();
        final JavaSyntaxKit javaSyntaxKit = new JavaSyntaxKit();
        try {
            // set UI default font to JavaSyntaxKit
            final Font defaultFont = ReflectionUtils.getFieldValue(DefaultSyntaxKit.class, "DEFAULT_FONT", javaSyntaxKit, Font.class);
            ReflectionUtils.setField(javaSyntaxKit, "DEFAULT_FONT", new JLabel().getFont().deriveFont(defaultFont.getStyle(), defaultFont.getSize()));
        } catch (Exception e) {
            LogController.CL().log(e);
            DebugMode.debugger(e);
        }
        defaults.setEditorKit(javaSyntaxKit);
        // defaults.setFocusable(false);
        p.add(apiScrollbar = new JScrollPane(defaults) {
            @Override
            public Dimension getPreferredSize() {
                Dimension ret = defaults.getPreferredSize();
                ret.width += 10;
                ret.height += 10;
                return super.getPreferredSize();
            }
        });
        defaults.setEditable(false);
        defaults.setContentType("text/javascript; charset=UTF-8");
        defaults.setText(ScriptEnvironment.getAPIDescription(entry.getEventTrigger().getAPIClasses()) + "\r\n" + entry.getEventTrigger().getAPIDescription());
        editor = new JEditorPane();
        editor.setEditorKit(javaSyntaxKit);
        p.add(scrollpane = new JScrollPane(editor) {
            @Override
            public Dimension getPreferredSize() {
                Dimension ret = editor.getPreferredSize();
                ret.width += 10;
                ret.height += 10;
                ret.height = Math.max(ret.height, 200);
                return super.getPreferredSize();
            }
        });
        delayer = new org.appwork.scheduler.DelayedRunnable(1000, 5000) {
            @Override
            public void delayedrun() {
                updateHighlighter();
            }
        };
        editor.setContentType("text/javascript; charset=UTF-8");
        String txt = entry.getScript();
        if (StringUtils.isEmpty(txt)) {
            txt = T.T.emptyScript();
        }
        editor.setText(txt);
        editor.setCaretPosition(0);
        // toolbar
        toolbar.add(new ExtButton(new AppAction() {
            {
                // setIconKey(IconKey.ICON_TEXT);
                setSelected(CFG_EVENT_CALLER.CFG.isAPIPanelVisible());
                setName(T.T.editor_showhelp());
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                CFG_EVENT_CALLER.CFG.setAPIPanelVisible(!CFG_EVENT_CALLER.CFG.isAPIPanelVisible());
                relayout();
            }
        }));
        toolbar.add(new ExtButton(new AppAction() {
            {
                setName(T.T.editor_autoformat());
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                delayer.resetAndStart();
            }
        }));
        toolbar.add(new ExtButton(new AppAction() {
            {
                setName(T.T.editor_testcompile());
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                extension.runTestCompile(entry.getEventTrigger(), editor.getText());
            }
        }));
        toolbar.add(new ExtButton(new AppAction() {
            {
                setName(T.T.editor_testrun());
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                testEventTriggerSettings.clear();
                if (settingsPanel != null) {
                    settingsPanel.testRun();
                }
                final ScriptEntry testEntry = new ScriptEntry();
                testEntry.setEnabled(true);
                testEntry.setEventTrigger(entry.getEventTrigger());
                testEntry.setEventTriggerSettings(testEventTriggerSettings);
                testEntry.setName(entry.getName());
                testEntry.setScript(getScript());
                final Map<String, Object> testProperties = entry.getEventTrigger().getTestProperties();
                testProperties.putAll(testEventTriggerSettings);
                // we have to remove isSynchronous because method with same name does exist
                // testProperties.remove("isSynchronous");
                extension.runScript(testEntry, testProperties, true);
            }
        }));
        relayout();
        return p;
    }

    protected void relayout() {
        p.removeAll();
        if (CFG_EVENT_CALLER.CFG.isAPIPanelVisible()) {
            p.setLayout("ins 0,wrap 1", "[grow,fill]", "[][][][grow,fill]");
        } else {
            p.setLayout("ins 0,wrap 1", "[grow,fill]", "[][][grow,fill]");
        }
        p.add(toolbar);
        if (settingsPanel != null) {
            p.add(settingsPanel);
        } else {
            p.add(Box.createGlue());
        }
        if (CFG_EVENT_CALLER.CFG.isAPIPanelVisible()) {
            p.add(apiScrollbar, "height 200:n:n");
        }
        p.add(scrollpane, "height 200:n:n");
        p.revalidate();
        p.repaint();
    }

    protected void updateHighlighter() {
        final AtomicInteger caretPosition = new AtomicInteger();
        try {
            String text = new EDTHelper<String>() {
                @Override
                public String edtRun() {
                    editor.setEditable(false);
                    caretPosition.set(editor.getCaretPosition());
                    return editor.getText();
                }
            }.getReturnValue();
            String before = text.substring(0, caretPosition.get()).replaceAll(CLEANUP, "");
            final String formatedText = format(text);
            if (!formatedText.equals(text)) {
                final int max = Math.min(caretPosition.get(), formatedText.length());
                for (int i = 0; i < max; i++) {
                    final String sb = formatedText.substring(0, i).replaceAll(CLEANUP, "");
                    if (sb.length() == before.length()) {
                        final int caret = i;
                        new EDTRunner() {
                            @Override
                            protected void runInEDT() {
                                editor.setText(formatedText);
                                editor.setCaretPosition(caret);
                            }
                        }.waitForEDT();
                        return;
                    }
                }
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        editor.setText(formatedText);
                    }
                }.waitForEDT();
            }
        } finally {
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    editor.setEditable(true);
                }
            }.waitForEDT();
        }
    }

    private synchronized String format(String script) {
        try {
            final Context cx = Context.enter();
            cx.setOptimizationLevel(9);
            cx.setLanguageVersion(Context.VERSION_1_5);
            if (scope == null) {
                scope = new Global();
                scope.init(cx);
                String lib;
                lib = IO.readURLToString(ScriptEntry.class.getResource("js_beautifier.js"));
                Script compiledLibrary = JSRhinoPermissionRestricter.compileTrustedString(cx, scope, lib, "", 1, null);
                JSRhinoPermissionRestricter.evaluateTrustedString(cx, scope, "global=this;", "", 1, null);
                compiledLibrary.exec(cx, scope);
            }

            ScriptableObject.putProperty(scope, "text", script);
            String formated = (String) JSRhinoPermissionRestricter.evaluateTrustedString(cx, scope, "js_beautify(text, {   });", "", 1, null);
            return formated;
            // ProcessBuilderFactory.runCommand(commandline);
        } catch (Throwable e) {
            LogController.CL().log(e);
        } finally {
            Context.exit();
        }
        return script;
    }

    @Override
    protected boolean isResizable() {
        return true;
    }

    public void pack() {
        this.getDialog().pack();
    }

    public String getScript() {
        return new EDTHelper<String>() {
            @Override
            public String edtRun() {
                return editor.getText();
            }
        }.getReturnValue();
    }

    public Map<String, Object> getEventTriggerSetup() {
        if (settingsPanel != null) {
            settingsPanel.save();
            return entry.getEventTriggerSettings();
        } else {
            return null;
        }
    }
}
