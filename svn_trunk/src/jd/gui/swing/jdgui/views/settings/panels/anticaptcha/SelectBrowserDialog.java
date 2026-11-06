package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.SwingUtilities;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileSystemView;

import org.appwork.swing.MigPanel;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.appwork.utils.swing.dialog.ExtFileChooserDialog;
import org.appwork.utils.swing.dialog.FileChooserSelectionMode;
import org.appwork.utils.swing.dialog.FileChooserType;
import org.appwork.utils.swing.dialog.dimensor.RememberLastDialogDimension;
import org.appwork.utils.swing.dialog.locator.RememberAbsoluteDialogLocator;
import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.browser.ExternalBrowserManager;
import org.jdownloader.controlling.browser.ExternalBrowserManager.InstalledBrowser;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.updatev2.gui.LAFOptions;

import jd.gui.swing.jdgui.BasicJDTable;
import net.miginfocom.swing.MigLayout;

/**
 * Lets the user choose the browser used by the browser captcha solver. The first entry is always the "OS Default" dummy entry (path ==
 * null). Installed browsers are searched in a background thread while the dialog is already open. The dialog returns the selected entry.
 */
public class SelectBrowserDialog extends AbstractDialog<InstalledBrowser> {
    private final InstalledBrowser    osDefault;
    /*
     * Dummy entry shown as the last row while the user has not picked a custom browser: Clicking it opens the file chooser. It is removed
     * as soon as the user has picked a browser that way.
     */
    private final InstalledBrowser    browsePlaceholder;
    private boolean                   browsePlaceholderVisible = true;
    /* The browser that was chosen when the dialog was opened. Everything else is a change which can be saved. */
    private final InstalledBrowser    initialBrowser;
    private final String[]            currentCommandline;
    private final BrowserTableModel   model;
    private final BasicJDTable<InstalledBrowser> table;
    private JPanel                    searchingPanel;
    /* The browser shown in bold. It is applied when the dialog is closed with "Save". */
    private InstalledBrowser          chosenBrowser;
    /* The configured browser if its executable does not exist, else null. Shown in red. */
    private InstalledBrowser          missingBrowser;
    /* System icons of the browser executables, filled in a background thread. */
    private final Map<InstalledBrowser, Icon> icons = new ConcurrentHashMap<InstalledBrowser, Icon>();
    /* Generic browser (globe) icon. */
    private final Icon                fallbackIcon = new AbstractIcon(IconKey.ICON_BROWSE, 16);
    /* Icon of the "browse" row. */
    private final Icon                addIcon      = new AbstractIcon(IconKey.ICON_ADD, 16);

    /**
     * @param currentCommandline
     *            the currently configured browser commandline, may be null. Used to preselect the matching entry.
     */
    public SelectBrowserDialog(final String[] currentCommandline) {
        super(Dialog.STYLE_HIDE_ICON, _GUI.T.SelectBrowserDialog_title(), null, _GUI.T.SelectBrowserDialog_save(), _GUI.T.SelectBrowserDialog_cancel());
        this.currentCommandline = currentCommandline;
        setLocator(new RememberAbsoluteDialogLocator(getClass().getSimpleName()));
        setDimensor(new RememberLastDialogDimension(getClass().getSimpleName()));
        osDefault = new InstalledBrowser(_GUI.T.SelectBrowserDialog_osDefault(), null);
        browsePlaceholder = new InstalledBrowser(_GUI.T.SelectBrowserDialog_browseCustom(), null);
        chosenBrowser = osDefault;
        model = new BrowserTableModel();
        final List<InstalledBrowser> initial = new ArrayList<InstalledBrowser>();
        final String configuredPath = getConfiguredPath();
        if (configuredPath != null) {
            /* The configured browser is always listed (and chosen), even if the auto detection does not find it. */
            final InstalledBrowser configured = new InstalledBrowser(ExternalBrowserManager.getInstance().getLazyBrowserName(configuredPath), configuredPath);
            initial.add(configured);
            chosenBrowser = configured;
            /* Checked only once here, not on every repaint. */
            if (!new File(configuredPath).exists()) {
                missingBrowser = configured;
            }
        }
        initialBrowser = chosenBrowser;
        model.setBrowsers(initial);
        table = new BasicJDTable<InstalledBrowser>(model) {
            /* ESC is the default "clear selection" shortcut of the table, which would swallow it before it reaches the dialog. */
            @Override
            protected boolean isClearSelectionTrigger(final KeyStroke ks) {
                return false;
            }
        };
        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        table.setShowHorizontalLineBelowLastEntry(false);
        table.setShowHorizontalLines(true);
        /* A single click chooses the browser (bold). It is only applied when the dialog is closed with "Save". */
        table.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(final ListSelectionEvent e) {
                if (e.getValueIsAdjusting()) {
                    return;
                }
                final int row = table.getSelectedRow();
                /* Rebuilding the model clears the selection (row -1), which must not change the choice. */
                if (row >= 0) {
                    final InstalledBrowser selected = model.getObjectbyRow(row);
                    if (selected == browsePlaceholder) {
                        /* Not a browser: Keep the current choice and open the file chooser (after the selection event is finished). */
                        selectObject(chosenBrowser);
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                browseForCustomBrowser();
                            }
                        });
                        return;
                    }
                    chosenBrowser = selected;
                    updateSaveButton();
                    table.repaint();
                }
            }
        });
    }

    /** True if the user has chosen a different browser than the one that was chosen when the dialog was opened. */
    private boolean hasChanges() {
        return chosenBrowser != initialBrowser;
    }

    /** The Save button is only enabled as long as there is something to save. */
    private void updateSaveButton() {
        final JButton save = getOkButton();
        if (save != null) {
            save.setEnabled(hasChanges());
        }
    }

    /** ESC only closes the dialog as long as the user has not changed anything, so changes are never lost by accident. */
    @Override
    protected void registerEscape() {
        super.registerEscape();
        final JComponent target = getFocusButton();
        if (target == null) {
            return;
        }
        final Action original = target.getActionMap().get("ESCAPE");
        if (original == null) {
            return;
        }
        target.getActionMap().put("ESCAPE", new AbstractAction() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                if (hasChanges()) {
                    return;
                }
                original.actionPerformed(e);
            }
        });
    }

    /**
     * Puts the info button at the very left of the bottom bar, while all other buttons stay on the right (like before).
     */
    @Override
    protected MigPanel createBottomPanel() {
        final MigPanel bottom = new MigPanel("ins 0", "[]20[]20[grow,fill][]", "[]");
        bottom.add(new JButton(createInfoAction()));
        return bottom;
    }

    /** Icon-only button which opens the knowledgebase article about defining a custom browser for the browser captcha solver. */
    private AbstractAction createInfoAction() {
        return new AppAction() {
            {
                setIconKey(IconKey.ICON_INFO);
                setTooltipText(_GUI.T.SelectBrowserDialog_info_tooltip());
            }

            @Override
            public void actionPerformed(final ActionEvent e) {
                CrossSystem.openURL("https://support.jdownloader.org/knowledgebase/article/define-custom-browser-solver-captcha-browser");
            }
        };
    }

    @Override
    public JComponent layoutDialogContent() {
        final JPanel panel = new JPanel(new MigLayout("ins 0,wrap 1", "[grow,fill]", "[grow,fill][]"));
        panel.add(new JScrollPane(table), "hmin 150");
        searchingPanel = new JPanel(new MigLayout("ins 0", "[][grow,fill]", "[]"));
        final JProgressBar bar = new JProgressBar();
        bar.setIndeterminate(true);
        searchingPanel.add(new JLabel(_GUI.T.SelectBrowserDialog_searching()));
        searchingPanel.add(bar);
        panel.add(searchingPanel);
        /* Same ESC handling as the AboutDialog: register ESC on the content panel. */
        registerEscape(panel);
        selectObject(chosenBrowser);
        loadIconsAsync(model.getBrowsers());
        startSearch();
        /* Nothing is changed yet -> nothing to save. */
        updateSaveButton();
        return panel;
    }

    /**
     * Loads the system icons of the given browser executables in a background thread (this accesses the file system / shell) and repaints
     * the table afterwards.
     */
    private void loadIconsAsync(final List<InstalledBrowser> browsers) {
        final List<InstalledBrowser> todo = new ArrayList<InstalledBrowser>();
        for (final InstalledBrowser browser : browsers) {
            if (browser.getPath() != null && !icons.containsKey(browser)) {
                todo.add(browser);
            }
        }
        if (todo.isEmpty()) {
            return;
        }
        final Thread thread = new Thread("SelectBrowserDialog:LoadIcons") {
            @Override
            public void run() {
                for (final InstalledBrowser browser : todo) {
                    final File file = new File(browser.getPath());
                    if (file.exists()) {
                        final Icon icon = FileSystemView.getFileSystemView().getSystemIcon(file);
                        if (icon != null) {
                            icons.put(browser, icon);
                        }
                    }
                }
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        table.repaint();
                    }
                };
            }
        };
        thread.setDaemon(true);
        thread.start();
    }

    private void startSearch() {
        final Thread thread = new Thread("SelectBrowserDialog:SearchBrowsers") {
            @Override
            public void run() {
                final List<InstalledBrowser> found = new ArrayList<InstalledBrowser>();
                try {
                    found.addAll(ExternalBrowserManager.getInstance().getInstalledBrowsers());
                } finally {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            final List<InstalledBrowser> list = new ArrayList<InstalledBrowser>(model.getBrowsers());
                            /* Keep entries the user added via "Browse..." while the search was running. */
                            for (final InstalledBrowser browser : found) {
                                if (indexOfPath(list, browser.getPath()) == -1) {
                                    list.add(browser);
                                }
                            }
                            model.setBrowsers(list);
                            loadIconsAsync(list);
                            searchingPanel.setVisible(false);
                            /* The configured browser is part of the list from the start, so the chosen browser stays valid. */
                            selectObject(chosenBrowser);
                            table.repaint();
                        }
                    };
                }
            }
        };
        thread.setDaemon(true);
        thread.start();
    }

    private String getConfiguredPath() {
        if (currentCommandline != null) {
            for (final String arg : currentCommandline) {
                if (arg != null && arg.trim().length() > 0) {
                    return arg.trim();
                }
            }
        }
        return null;
    }

    /**
     * @return index of the entry with the given path, or -1. The "OS Default" entry (path == null) is never matched.
     */
    private int indexOfPath(final List<InstalledBrowser> list, final String path) {
        if (path == null) {
            return -1;
        }
        for (int i = 0; i < list.size(); i++) {
            final String other = list.get(i).getPath();
            if (other != null && isSamePath(other, path)) {
                return i;
            }
        }
        return -1;
    }

    private boolean isSamePath(final String a, final String b) {
        String pa;
        String pb;
        try {
            pa = new File(a).getCanonicalPath();
            pb = new File(b).getCanonicalPath();
        } catch (final IOException e) {
            pa = a;
            pb = b;
        }
        if (CrossSystem.isWindows()) {
            return pa.toLowerCase(Locale.ENGLISH).equals(pb.toLowerCase(Locale.ENGLISH));
        }
        return pa.equals(pb);
    }

    /** Selects the row of the given entry (the row index depends on the current sorting). */
    private void selectObject(final InstalledBrowser browser) {
        final int row = model.getRowforObject(browser);
        if (row >= 0 && row < model.getRowCount()) {
            table.getSelectionModel().setSelectionInterval(row, row);
        }
    }

    /**
     * Lets the user pick the executable of a browser which is not in the list. The picked browser becomes the chosen one and replaces the
     * "browse" row of the table.
     */
    private void browseForCustomBrowser() {
        final ExtFileChooserDialog d = new ExtFileChooserDialog(0, _GUI.T.SelectBrowserDialog_browse_title(), null, null);
        /* Limit the selectable files to executables: ".exe" on windows, files with the executable flag elsewhere. */
        d.setFileFilter(new FileFilter() {
            @Override
            public boolean accept(final File f) {
                if (f.isDirectory()) {
                    return true;
                }
                if (CrossSystem.isWindows()) {
                    return f.getName().toLowerCase(Locale.ENGLISH).endsWith(".exe");
                }
                return f.canExecute();
            }

            @Override
            public String getDescription() {
                return _GUI.T.SelectBrowserDialog_filter_executables();
            }
        });
        d.setAcceptAllFileFilterUsed(false);
        d.setFileSelectionMode(FileChooserSelectionMode.FILES_ONLY);
        d.setMultiSelection(false);
        d.setStorageID("SelectBrowserDialog");
        d.setType(FileChooserType.OPEN_DIALOG);
        try {
            Dialog.getInstance().showDialog(d);
        } catch (DialogClosedException e1) {
            return;
        } catch (DialogCanceledException e1) {
            return;
        }
        final File file = d.getSelectedFile();
        if (file == null || !file.isFile()) {
            return;
        }
        final List<InstalledBrowser> list = new ArrayList<InstalledBrowser>(model.getBrowsers());
        int index = indexOfPath(list, file.getAbsolutePath());
        if (index == -1) {
            list.add(new InstalledBrowser(ExternalBrowserManager.getInstance().getLazyBrowserName(file.getAbsolutePath()), file.getAbsolutePath()));
            index = list.size() - 1;
        }
        /* The picked browser replaces the "browse" row. */
        browsePlaceholderVisible = false;
        model.setBrowsers(list);
        loadIconsAsync(list);
        /* A manually picked browser becomes the chosen one. */
        chosenBrowser = list.get(index);
        selectObject(chosenBrowser);
        updateSaveButton();
        table.repaint();
    }

    /**
     * @return the browser shown in bold, i.e. the one that is applied when the dialog is closed with "Save".
     */
    @Override
    protected InstalledBrowser createReturnValue() {
        return chosenBrowser;
    }

    @Override
    protected int getPreferredWidth() {
        return 700;
    }

    @Override
    protected boolean isResizable() {
        return true;
    }

    /**
     * Text column which renders the currently chosen (default) browser in bold.
     */
    private abstract class BoldColumn extends ExtTextColumn<InstalledBrowser> {
        public BoldColumn(final String name) {
            super(name);
            defaultForeground = rendererField.getForeground();
            /* Sort by the displayed text, but keep the "OS Default" entry on top in both sort directions. */
            setRowSorter(new ExtDefaultRowSorter<InstalledBrowser>() {
                @Override
                public int compare(final InstalledBrowser o1, final InstalledBrowser o2) {
                    if (o1 == o2) {
                        return 0;
                    } else if (o1 == osDefault || o2 == browsePlaceholder) {
                        return -1;
                    } else if (o2 == osDefault || o1 == browsePlaceholder) {
                        return 1;
                    }
                    final String s1 = BoldColumn.this.getStringValue(o1);
                    final String s2 = BoldColumn.this.getStringValue(o2);
                    if (ExtColumn.SORT_ASC.equals(getSortOrderIdentifier())) {
                        return s1.compareToIgnoreCase(s2);
                    } else {
                        return s2.compareToIgnoreCase(s1);
                    }
                }
            });
        }

        @Override
        public void configureRendererComponent(final InstalledBrowser value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
            /* Set the font before super so that the text clipping uses the final font. */
            final Font font = rendererField.getFont();
            final int fontStyle;
            if (value == chosenBrowser) {
                fontStyle = Font.BOLD;
            } else if (value == browsePlaceholder) {
                /* The "browse" row is an action and not a browser. */
                fontStyle = Font.ITALIC;
            } else {
                fontStyle = Font.PLAIN;
            }
            rendererField.setFont(font.deriveFont(fontStyle));
            super.configureRendererComponent(value, isSelected, hasFocus, row, column);
            /* The renderer is reused for all rows, so the foreground has to be set (or reset) for every row. */
            rendererField.setForeground(value == missingBrowser ? LAFOptions.getInstance().getColorForErrorForeground() : defaultForeground);
        }

        private Color defaultForeground;
    }

    /**
     * Table model. All columns are sortable, the "OS Default" entry always stays on top.
     */
    private class BrowserTableModel extends ExtTableModel<InstalledBrowser> {
        private final List<InstalledBrowser> browsers = new ArrayList<InstalledBrowser>();

        public BrowserTableModel() {
            super("SelectBrowserDialogTableModel");
        }

        /** @return the browsers without the "OS Default" dummy entry */
        public List<InstalledBrowser> getBrowsers() {
            return new ArrayList<InstalledBrowser>(browsers);
        }

        public void setBrowsers(final List<InstalledBrowser> list) {
            browsers.clear();
            browsers.addAll(list);
            final List<InstalledBrowser> all = new ArrayList<InstalledBrowser>();
            all.add(osDefault);
            all.addAll(browsers);
            if (browsePlaceholderVisible) {
                all.add(browsePlaceholder);
            }
            _fireTableStructureChanged(all, true);
        }

        @Override
        protected void initColumns() {
            addColumn(new BoldColumn(_GUI.T.lit_name()) {
                /* Icon of the executable. A generic browser icon is used for "OS Default" and until/unless the real icon is available. */
                @Override
                protected Icon getIcon(final InstalledBrowser value) {
                    if (value == browsePlaceholder) {
                        return addIcon;
                    }
                    final Icon icon = icons.get(value);
                    return icon != null ? icon : fallbackIcon;
                }

                @Override
                public int getDefaultWidth() {
                    /* Wide enough for the text of the "browse" row. */
                    return 330;
                }

                @Override
                public String getStringValue(final InstalledBrowser value) {
                    return value.getName();
                }
            });
            addColumn(new BoldColumn(_GUI.T.SelectBrowserDialog_column_path()) {
                @Override
                public int getDefaultWidth() {
                    return 400;
                }

                @Override
                public String getStringValue(final InstalledBrowser value) {
                    return value.getPath() == null ? "" : value.getPath();
                }

                /* Double click on the path opens the folder of the browser, unless there is no path ("OS Default") or it is known to be broken. */
                @Override
                public boolean onDoubleClick(final MouseEvent e, final InstalledBrowser value) {
                    if (value.getPath() == null || value == missingBrowser) {
                        return false;
                    }
                    final File folder = new File(value.getPath()).getParentFile();
                    if (folder == null || !folder.isDirectory()) {
                        return false;
                    }
                    CrossSystem.openFile(folder);
                    return true;
                }
            });
        }
    }
}
