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
package org.appwork.swing.components.pathchooser;

import java.awt.KeyboardFocusManager;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.filechooser.FileFilter;

import net.miginfocom.swing.MigLayout;

import org.appwork.storage.JSonStorage;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtButton;
import org.appwork.swing.components.ExtTextField;
import org.appwork.swing.components.searchcombo.SearchComboBox;
import org.appwork.utils.StringUtils;
import org.appwork.utils.locale._AWU;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.appwork.utils.swing.dialog.ExtFileChooserDialog;
import org.appwork.utils.swing.dialog.FileChooserSelectionMode;
import org.appwork.utils.swing.dialog.FileChooserType;

public class PathChooser extends MigPanel {
    private class BrowseAction extends AbstractAction {
        /**
         *
         */
        private static final long serialVersionUID = -4350861121298607806L;

        BrowseAction() {
            this.putValue(Action.NAME, PathChooser.this.getBrowseLabel());
        }

        /*
         * (non-Javadoc)
         *
         * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent )
         */
        @Override
        public void actionPerformed(final ActionEvent e) {
            final File file = PathChooser.this.doFileChooser();
            if (file == null) {
                return;
            }
            PathChooser.this.setFile(file);
        }
    }

    /**
     *
     */
    private static final long        serialVersionUID = -3651657642011425583L;
    protected ExtTextField           txt;
    protected ExtButton              bt;
    private String                   id;
    protected SearchComboBox<String> destination;

    public PathChooser(final String id) {
        this(id, false);
    }

    public PathChooser(final String id, final boolean useQuickLIst) {
        super("ins 0", "[fill,grow][]", "[fill]");
        this.id = id;
        this.setOpaque(false);
        this.txt = new ExtTextField() {
            /**
             *
             */
            private static final long serialVersionUID = 3243788323043431841L;

            /*
             * (non-Javadoc)
             *
             * @see org.appwork.swing.components.ExtTextField#getText()
             */
            @Override
            public String getText() {
                String ret = super.getText();
                if (ret.equals(helpText) && getForeground() == helpColor) {
                    ret = "";
                }
                return ret;
            }

            @Override
            public JPopupMenu getPopupMenu(MouseEvent event, final AbstractAction cutAction, final AbstractAction copyAction, final AbstractAction pasteAction, final AbstractAction deleteAction, final AbstractAction selectAction) {
                final JPopupMenu self = PathChooser.this.getPopupMenu(PathChooser.this.txt, cutAction, copyAction, pasteAction, deleteAction, selectAction);
                if (self == null) {
                    return super.getPopupMenu(event, cutAction, copyAction, pasteAction, deleteAction, selectAction);
                }
                return self;
            }

            /*
             * (non-Javadoc)
             *
             * @see org.appwork.swing.components.ExtTextField#onChanged()
             */
            @Override
            public void onChanged() {
                PathChooser.this.onChanged(PathChooser.this.txt);
            }
        };
        this.txt.setHelpText(this.getHelpText());
        this.bt = new ExtButton(new BrowseAction());
        if (useQuickLIst) {
            this.txt.setHelperEnabled(false);
            this.destination = new SearchComboBox<String>() {
                @Override
                public JTextField createTextField() {
                    return PathChooser.this.txt;
                }

                /*
                 * (non-Javadoc)
                 *
                 * @see org.appwork.swing.components.searchcombo.SearchComboBox#getProtoType(java.util.List)
                 */
                @Override
                public String getProtoType(List<String> model) {
                    return PathChooser.this.getProtoType(this, model);
                }

                @Override
                protected Icon getIconForValue(final String value) {
                    return null;
                }

                @Override
                protected String getTextForValue(final String value) {
                    return value;
                }

                @Override
                public boolean isAutoCompletionEnabled() {
                    return false;
                }

                @Override
                public void onChanged() {
                    PathChooser.this.onChanged(PathChooser.this.txt);
                }

                @Override
                protected void sortFound(String search, final List<String> found) {
                    Collections.sort(found, new Comparator<String>() {
                        @Override
                        public int compare(final String o1, final String o2) {
                            return o1.compareTo(o2);
                        }
                    });
                }
            };
            // this code makes enter leave the dialog.
            this.destination.getTextField().getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, KeyEvent.SHIFT_DOWN_MASK, true), "auto");
            this.destination.getTextField().getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0, true), "auto");
            this.destination.getTextField().setFocusTraversalKeysEnabled(false);
            this.destination.getTextField().getActionMap().put("auto", new AbstractAction() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    if (!PathChooser.this.auto(PathChooser.this.txt)) {
                        final KeyboardFocusManager manager = KeyboardFocusManager.getCurrentKeyboardFocusManager();
                        if ((e.getModifiers() & ActionEvent.SHIFT_MASK) == ActionEvent.SHIFT_MASK) {
                            manager.focusPreviousComponent();
                        } else {
                            manager.focusNextComponent();
                        }
                    }
                }
            });
            this.destination.setUnkownTextInputAllowed(true);
            this.destination.setBadColor(null);
            this.destination.setSelectedItem(null);
            this.add(this.destination);
        } else {
            this.txt.setHelperEnabled(true);
            this.add(this.txt);
        }
        this.add(this.bt);
        final String preSelection = JSonStorage.getStorage(Dialog.FILECHOOSER).get(Dialog.LASTSELECTION + id, this.getDefaultPreSelection());
        if (preSelection != null) {
            this.setFile(new File(preSelection));
        }
    }

    protected String getProtoType(SearchComboBox<String> comboBox, List<String> model) {
        if (comboBox.usePrototype() && model != null && model.size() > 0) {
            return model.get(0);
        } else {
            return null;
        }
    }

    @Override
    public synchronized void addMouseListener(final MouseListener l) {
        this.txt.addMouseListener(l);
        this.bt.addMouseListener(l);
        super.addMouseListener(l);
    }

    protected boolean auto(final JTextField oldTextField) {
        final String txt = oldTextField.getText();
        final int selstart = oldTextField.getSelectionStart();
        final int selend = oldTextField.getSelectionEnd();
        if (selend != txt.length()) {
            return false;
        }
        final String sel = txt.substring(selstart, selend);
        final String bef = txt.substring(0, selstart);
        final String name = bef.endsWith("/") || bef.endsWith("\\") ? "" : new File(bef).getName();
        final String findName = txt.endsWith("/") || txt.endsWith("\\") ? "" : new File(txt).getName();
        boolean found = sel.length() == 0;
        File root = new File(bef);
        while (root != null && !root.exists()) {
            if (root.getParentFile() == root) {
                return false;
            }
            root = root.getParentFile();
        }
        if (root == null) {
            return false;
        }
        for (final File f : root.listFiles()) {
            if (f.isFile()) {
                continue;
            }
            if (f.isHidden()) {
                continue;
            }
            if (this.equals(f.getName(), findName)) {
                found = true;
                continue;
            }
            if (found && this.startsWith(f.getName(), name)) {
                oldTextField.setText(f.getAbsolutePath());
                oldTextField.setSelectionStart(selstart);
                oldTextField.setSelectionEnd(oldTextField.getText().length());
                return true;
            }
        }
        oldTextField.setText(bef);
        return false;
    }

    /**
     * @return
     */
    public File doFileChooser() {
        final ExtFileChooserDialog d = new ExtFileChooserDialog(0, this.getDialogTitle(), null, null);
        d.setStorageID(this.getID());
        d.setFileSelectionMode(this.getSelectionMode());
        d.setFileFilter(this.getFileFilter());
        d.setType(this.getType());
        d.setMultiSelection(false);
        d.setPreSelection(this.getFile());
        try {
            Dialog.I().showDialog(d);
            return d.getSelectedFile();
        } catch (final DialogNoAnswerException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * @param name
     * @param findName
     * @return
     */
    private boolean equals(final String name, final String findName) {
        if (CrossSystem.isWindows()) {
            return name.equalsIgnoreCase(findName);
        }
        return name.equals(findName);
    }

    /**
     * @param file2
     * @return
     */
    protected String fileToText(final File file2) {
        return file2.getAbsolutePath();
    }

    /**
     * @return
     */
    public String getBrowseLabel() {
        return _AWU.T.pathchooser_browselabel();
    }

    /**
     * removes the button from the component to place it externaly
     *
     * @return
     */
    public JButton getButton() {
        this.remove(this.bt);
        this.setLayout(new MigLayout("ins 0", "[grow,fill]", "[grow,fill]"));
        return this.bt;
    }

    /**
     * @return
     */
    protected String getDefaultPreSelection() {
        return null;
    }

    public SearchComboBox<String> getDestination() {
        return this.destination;
    }

    /**
     * @return
     */
    public String getDialogTitle() {
        return _AWU.T.pathchooser_dialog_title();
    }

    /**
     * @return
     */
    public File getFile() {
        if (StringUtils.isEmpty(this.txt.getText())) {
            return null;
        }
        return this.textToFile(this.txt.getText());
    }

    /**
     * @return
     */
    public FileFilter getFileFilter() {
        return null;
    }

    /**
     * @return
     */
    protected String getHelpText() {
        return _AWU.T.pathchooser_helptext();
    }

    /**
     * @return
     */
    public String getID() {
        return this.id;
    }

    /**
     * @return
     */
    public String getPath() {
        return new File(this.txt.getText()).getAbsolutePath();
    }

    public JPopupMenu getPopupMenu(final ExtTextField txt, final AbstractAction cutAction, final AbstractAction copyAction, final AbstractAction pasteAction, final AbstractAction deleteAction, final AbstractAction selectAction) {
        return null;
    }

    /**
     * @return
     */
    public FileChooserSelectionMode getSelectionMode() {
        return FileChooserSelectionMode.DIRECTORIES_ONLY;
    }

    public ExtTextField getTxt() {
        return this.txt;
    }

    /**
     * @return
     */
    public FileChooserType getType() {
        return FileChooserType.SAVE_DIALOG;
    }

    /**
     * @param txt2
     */
    protected void onChanged(final ExtTextField txt2) {
    }

    @Override
    public synchronized void removeMouseListener(final MouseListener l) {
        this.txt.removeMouseListener(l);
        this.bt.removeMouseListener(l);
        super.removeMouseListener(l);
    }

    @Override
    public void setEnabled(final boolean b) {
        this.txt.setEnabled(b);
        this.bt.setEnabled(b);
        if (this.destination != null) {
            this.destination.setEnabled(b);
        }
    }

    public void setFile(final File file) {
        final String text = this.fileToText(file);
        if (this.destination != null) {
            this.destination.setText(text);
        } else {
            this.txt.setText(text);
        }
    }

    /**
     * @param packagizerFilterRuleDialog_layoutDialogContent_dest_help
     */
    public void setHelpText(final String helpText) {
        this.txt.setHelpText(helpText);
        if (this.destination != null) {
            this.destination.setHelpText(helpText);
        }
    }

    /**
     * @param downloadDestination
     */
    public void setPath(final String downloadDestination) {
        if (this.destination != null) {
            this.destination.setText(downloadDestination);
        } else {
            this.txt.setText(downloadDestination);
        }
    }

    public void setQuickSelectionList(final List<String> quickSelectionList) {
        this.destination.setList(quickSelectionList);
    }

    /**
     * @param name
     * @param name2
     * @return
     */
    private boolean startsWith(final String name, final String name2) {
        if (CrossSystem.isWindows()) {//
            return name.toLowerCase(Locale.ENGLISH).startsWith(name2.toLowerCase(Locale.ENGLISH));
        }
        return name.startsWith(name2);
    }

    /**
     * @param text
     * @return
     */
    protected File textToFile(final String text) {
        return new File(text);
    }
}
