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
package org.appwork.swing.components.searchcombo;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Rectangle;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicInteger;

import javax.accessibility.Accessible;
import javax.swing.ComboBoxEditor;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.CaretEvent;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.plaf.basic.BasicComboBoxUI;
import javax.swing.plaf.basic.ComboPopup;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;

import org.appwork.swing.MigPanel;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTHelper;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;

/**
 * this component extends a normal combobox and implements a editable filter/autocompletion feature. <b> make sure that you model is
 * sorted</b>
 *
 * @author thomas
 *
 * @param <T>
 */
public abstract class SearchComboBox<T> extends JComboBox {
    class Editor implements ComboBoxEditor, FocusListener, DocumentListener {
        private final JTextField       tf;
        private final MigPanel         panel;
        private final JLabel           icon;
        private T                      value;
        private volatile AtomicInteger valueSetter = new AtomicInteger(0);
        private final SearchComboBox   searchComboBox;

        public Editor(final SearchComboBox searchComboBox) {
            this.searchComboBox = searchComboBox;
            this.tf = SearchComboBox.this.createTextField();
            this.tf.getDocument().addDocumentListener(this);
            this.tf.addFocusListener(new FocusListener() {
                @Override
                public void focusGained(final FocusEvent e) {
                    // SearchComboBox.this.getEditor().getEditorComponent().requestFocus();
                }

                @Override
                public void focusLost(final FocusEvent e) {
                    SearchComboBox.this.hidePopup();
                    // to replaint the focus hilight
                    SearchComboBox.this.repaint();
                }
            });
            this.icon = new JLabel();
            // editor panel
            this.panel = new MigPanel("ins 0", "[][grow,fill]", "[grow,fill]") {
                /**
                 *
                 */
                private static final long serialVersionUID = 3558783171884965102L;

                @Override
                public void requestFocus() {
                    Editor.this.tf.requestFocus();
                }

                /*
                 * (non-Javadoc)
                 *
                 * @see javax.swing.JComponent#setEnabled(boolean)
                 */
                @Override
                public void setEnabled(boolean enabled) {
                    super.setEnabled(enabled);
                    tf.setEnabled(enabled);
                }
            };
            this.tf.addFocusListener(this);
            this.panel.add(this.icon);
            this.panel.setOpaque(true);
            this.panel.setBackground(this.tf.getBackground());
            this.tf.setBackground(null);
            SwingUtils.setOpaque(this.tf, false);
            this.panel.add(this.tf);
            SwingUtils.setOpaque(this.panel, false);
            // this.panel.setBorder(this.tf.getBorder());
            this.tf.setBorder(null);
            panel.setEnabled(searchComboBox.isEnabled());
        }

        /*
         * (non-Javadoc)
         *
         * @see javax.swing.ComboBoxEditor#addActionListener(java.awt.event. ActionListener)
         */
        @Override
        public void addActionListener(final ActionListener l) {
            this.tf.addActionListener(l);
        }

        private void auto() {
            if (!SearchComboBox.this.isAutoCompletionEnabled()) {
                return;
            }
            if (this.valueSetter.get() > 0) {
                return;
            }
            // scheduler executes at least 50 ms after this submit.
            // this.sheduler.run();
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    if (Editor.this.valueSetter.get() > 0) {
                        return;
                    }
                    Editor.this.autoComplete(true);
                }
            });
        }

        /**
         * finds all possible matches of the entered text and sets the selected object
         */
        protected boolean autoComplete(final boolean showPopup) {
            if (!SearchComboBox.this.isAutoCompletionEnabled()) {
                return false;
            }
            String rawtxt = Editor.this.tf.getText();
            if (StringUtils.isEmpty(rawtxt)) {
                /* every string will begin with "" so we return here */
                return false;
            }
            final String finalRaw = rawtxt;
            if (this.searchComboBox.isSearchCaseSensitive() == false) {
                rawtxt = rawtxt.toLowerCase(Locale.ENGLISH);
            }
            final String txt = rawtxt;
            final T lValue = this.value;
            if (lValue != null && SearchComboBox.this.getTextForValue(lValue).equals(txt)) {
                return true;
            }
            final java.util.List<T> found = new ArrayList<T>();
            final java.util.List<T> all = new ArrayList<T>();
            final ComboBoxModel model = SearchComboBox.this.getModel();
            this.searchComboBox.searchAutoComplete(model, txt, found, all);
            SearchComboBox.this.sortFound(txt, found);
            System.out.println("Found: " + found);
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    setListSearchResults(found, all);
                    final int pos = Editor.this.tf.getCaretPosition();
                    if (found.size() == 0) {
                        SearchComboBox.this.hidePopup();
                        if (SearchComboBox.this.getSelectedIndex() != -1) {
                            SearchComboBox.this.setSelectedIndex(-1);
                            Editor.this.safeSet(finalRaw);
                            Editor.this.tf.setCaretPosition(Math.min(pos, finalRaw.length()));
                        }
                        // javax.swing.plaf.synth.SynthComboPopup
                    } else {
                        Editor.this.tf.setForeground(SearchComboBox.this.getForeground());
                        if (SearchComboBox.this.isSetTextFieldOnlyForSingleMatch() && found.size() > 1) {
                            // SearchComboBox.this.setSelectedItem(found.get(0));
                        } else {
                            Editor.this.setItem(found.get(0));
                            SearchComboBox.this.setSelectedItem(found.get(0));
                            Editor.this.setItem(found.get(0));
                            setSelectionAfterAutoComplete(tf, txt, pos, found);
                        }
                        // Show popup, and scroll to correct position
                        if (found.size() > 1 && showPopup) {
                            // limit popup rows
                            SearchComboBox.this.setMaximumRowCount(Math.min(SearchComboBox.this.getActualMaximumRowCount(), found.size()));
                            SearchComboBox.this.setPopupVisible(true);
                            // Scroll popup list, so that found[0] is the first
                            // entry. This is a bit "dirty", so we put it in a
                            // try catch...just to avoid EDT Exceptions
                            try {
                                final Object popup = SearchComboBox.this.getUI().getAccessibleChild(SearchComboBox.this, 0);
                                if (popup instanceof Container) {
                                    final Component scrollPane = ((Container) popup).getComponent(0);
                                    if (popup instanceof ComboPopup) {
                                        final JList jlist = ((ComboPopup) popup).getList();
                                        final int selectedIndex = data.indexOf(found.get(0));
                                        if (scrollPane instanceof JScrollPane) {
                                            new EDTHelper<Void>() {
                                                @Override
                                                public Void edtRun() {
                                                    final Rectangle cellBounds = jlist.getCellBounds(selectedIndex, selectedIndex + found.size() - 1);
                                                    System.out.println("Scroll to " + cellBounds);
                                                    if (cellBounds != null) {
                                                        jlist.scrollRectToVisible(cellBounds);
                                                    }
                                                    return null;
                                                }
                                            }.start(true);
                                        }
                                    }
                                }
                            } catch (final Throwable e) {
                                org.appwork.loggingv3.LogV3.log(e);
                            }
                        } else {
                            SearchComboBox.this.hidePopup();
                        }
                    }
                    SearchComboBox.this.updateColorByContent();
                }
            };
            return found.size() > 0;
        }

        public void caretUpdate(final CaretEvent arg0) {
        }

        /*
         * (non-Javadoc)
         *
         * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event .DocumentEvent)
         */
        @Override
        public void changedUpdate(final DocumentEvent e) {
            this.auto();
            SearchComboBox.this.onChanged();
        }

        public void focusGained(final FocusEvent arg0) {
            if (this.tf.getText().equals(SearchComboBox.this.helptext)) {
                this.safeSet("");
                SearchComboBox.this.updateColorByContent();
            } else {
                Editor.this.tf.selectAll();
            }
        }

        public void focusLost(final FocusEvent arg0) {
            if (!SearchComboBox.this.isUnkownTextInputAllowed() && !Editor.this.autoComplete(false)) {
                // reset text after modifications to a valid value
                final String ret = SearchComboBox.this.getTextForValue(Editor.this.value);
                this.safeSet(ret);
            } else {
                SearchComboBox.this.updateHelpText();
            }
            SearchComboBox.this.updateColorByContent();
        }

        /*
         * (non-Javadoc)
         *
         * @see javax.swing.ComboBoxEditor#getEditorComponent()
         */
        @Override
        public Component getEditorComponent() {
            return this.panel;
        }

        /*
         * (non-Javadoc)
         *
         * @see javax.swing.ComboBoxEditor#getItem()
         */
        @Override
        public Object getItem() {
            return this.value;
        }

        public JTextField getTf() {
            return this.tf;
        }

        /*
         * (non-Javadoc)
         *
         * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event .DocumentEvent)
         */
        @Override
        public void insertUpdate(final DocumentEvent e) {
            this.auto();
            SearchComboBox.this.onChanged();
        }

        /*
         * (non-Javadoc)
         *
         * @see javax.swing.ComboBoxEditor#removeActionListener(java.awt.event. ActionListener)
         */
        @Override
        public void removeActionListener(final ActionListener l) {
            this.tf.removeActionListener(l);
        }

        /*
         * (non-Javadoc)
         *
         * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event .DocumentEvent)
         */
        @Override
        public void removeUpdate(final DocumentEvent e) {
            if (this.tf.getSelectionEnd() - this.tf.getSelectionStart() == 0) {
                this.auto();
            }
            SearchComboBox.this.onChanged();
        }

        /**
         * use this to set a Text to tf, we don't want action listeners to react on this
         */
        protected void safeSet(final String txt) {
            if (byPassTxtUpdate) {
                return;
            }
            this.valueSetter.incrementAndGet();
            try {
                Editor.this.tf.setText(txt);
            } finally {
                this.valueSetter.decrementAndGet();
            }
        }

        /*
         * (non-Javadoc)
         *
         * @see javax.swing.ComboBoxEditor#selectAll()
         */
        @Override
        public void selectAll() {
            this.tf.selectAll();
        }

        /*
         * (non-Javadoc)
         *
         * @see javax.swing.ComboBoxEditor#setItem(java.lang.Object)
         */
        @SuppressWarnings("unchecked")
        @Override
        public void setItem(final Object anObject) {
            // if (this.value == anObject) { return; }
            this.valueSetter.incrementAndGet();
            try {
                this.safeSet(SearchComboBox.this.getTextForValue((T) anObject));
                this.icon.setIcon(SearchComboBox.this.getIconForValue((T) anObject));
                this.value = (T) anObject;
                SearchComboBox.this.updateHelpText();
                SearchComboBox.this.updateColorByContent();
            } finally {
                this.valueSetter.decrementAndGet();
            }
        }
    }

    /**
     * @param model
     * @param txt
     * @param found
     * @param all
     */
    protected void searchAutoComplete(ComboBoxModel model, String txt, List<T> found, List<T> all) {
        if (!isSearchCaseSensitive()) {
            txt = txt.toLowerCase(Locale.ROOT);
        }
        for (int i = 0; i < model.getSize(); i++) {
            final T element = (T) SearchComboBox.this.getModel().getElementAt(i);
            all.add(element);
            final String text = SearchComboBox.this.getTextForValue(element);
            if (matches(text, txt)) {
                found.add(element);
            }
        }
    }

    /**
     * @return
     */
    protected boolean isSetTextFieldOnlyForSingleMatch() {
        return false;
    }

    /**
     * @return
     */
    protected boolean matches(String element, String matches) {
        return element != null && matches != null && (element.startsWith(matches) || this.isSearchCaseSensitive() == false && element.toLowerCase(Locale.ENGLISH).startsWith(matches));
    }

    private int               actualMaximumRowCount = 8;
    public boolean            autoCompletionEnabled = true;
    /**
     *
     */
    private static final long serialVersionUID      = 6475635443708682554L;
    private final ColorState  helpColorSet          = new ColorState(Color.LIGHT_GRAY);
    private final ColorState  badColorSet           = new ColorState(Color.RED);
    private final ColorState  normalColorSet        = new ColorState(Color.BLACK);
    {
        this.normalColorSet.setForeground(this.getForeground());
        final Color disabled = (Color) UIManager.get("TextField.disabledForeground");
        if (disabled != null) {
            this.helpColorSet.setForeground(disabled);
        }
    }
    private String      helptext;
    private boolean     unkownTextInputAllowed = false;
    protected ImageIcon badgeIcon;
    private ColorState  currentColorSet;
    private List<T>     data;
    private boolean     byPassTxtUpdate        = false;

    /**
     * @param plugins
     */
    public SearchComboBox() {
        this((List<T>) null);
    }

    public SearchComboBox(final T... elements) {
        this(Arrays.asList(elements));
    }

    public SearchComboBox(final List<T> plugins) {
        super((ComboBoxModel) null);
        this.data = plugins;
        this.setEditor(new Editor(this));
        if (plugins != null) {
            this.setList(plugins, true);
        }
        this.addFocusListener(new FocusListener() {
            @Override
            public void focusGained(final FocusEvent e) {
                SearchComboBox.this.getEditor().getEditorComponent().requestFocus();
            }

            @Override
            public void focusLost(final FocusEvent e) {
                SearchComboBox.this.hidePopup();
            }
        });
        this.setEditable(true);
        // we extends the existing renderer. this avoids LAF incompatibilities
        final ListCellRenderer org = this.getRenderer();
        this.addPopupMenuListener(new PopupMenuListener() {
            @Override
            public void popupMenuCanceled(final PopupMenuEvent e) {
            }

            @Override
            public void popupMenuWillBecomeInvisible(final PopupMenuEvent e) {
            }

            @Override
            public void popupMenuWillBecomeVisible(final PopupMenuEvent e) {
                /* limit max row to 8 */
                SearchComboBox.this.setMaximumRowCount(SearchComboBox.this.getActualMaximumRowCount());
            }
        });
        this.setRenderer(new ListCellRenderer() {
            @SuppressWarnings("unchecked")
            public Component getListCellRendererComponent(final JList list, final Object value, final int index, final boolean isSelected, final boolean cellHasFocus) {
                try {
                    final JLabel ret = (JLabel) org.getListCellRendererComponent(list, SearchComboBox.this.getTextForValue((T) value), index, isSelected, cellHasFocus);
                    ret.setIcon(SearchComboBox.this.getIconForValue((T) value));
                    // ret.setOpaque(false);
                    return ret;
                } catch (final Throwable e) {
                    // org might not be a JLabel (depending on the LAF)
                    // fallback here
                    return org.getListCellRendererComponent(list, SearchComboBox.this.getTextForValue((T) value), index, isSelected, cellHasFocus);
                }
            }
        });
        this.setColorState(this.normalColorSet);
    }

    /**
     * @return
     */
    public JTextField createTextField() {
        return new JTextField() {
            /**
             *
             */
            private static final long serialVersionUID = 8594276945732071594L;

            @Override
            public void setText(final String t) {
                super.setText(t);
            }

            private boolean key = false;

            @Override
            public void replaceSelection(String content) {
                if (key) {
                    // type on keyboard
                    super.replaceSelection(content);
                } else {
                    // paste externaly. like contextmenu
                    setText(content);
                }
            }

            /*
             * (non-Javadoc)
             *
             * @see javax.swing.JTextField#fireActionPerformed()
             */
            @Override
            protected void fireActionPerformed() {
                if (SearchComboBox.this.isPopupVisible()) {
                    // enter while combo is open - use selected index
                    Object selected = SearchComboBox.this.getSelectedItem();
                    setText(SearchComboBox.this.getTextForValue(SearchComboBox.this.getSelectedItem()));
                }
                super.fireActionPerformed();
            }

            @Override
            protected boolean processKeyBinding(KeyStroke ks, KeyEvent e, int condition, boolean pressed) {
                // forward events
                // this will cause to trigger a pressed event on enter. this
                // will the trigger the default action of dialogs - for example
                key = true;
                try {
                    return super.processKeyBinding(ks, e, condition, pressed);
                } finally {
                    key = false;
                }
            }
        };
    }

    public int getActualMaximumRowCount() {
        return this.actualMaximumRowCount;
    }

    /**
     * @return
     */
    public String getEditorText() {
        return this.getTextField().getText();
    }

    /**
     * @param value
     * @return
     */
    abstract protected Icon getIconForValue(T value);

    @Override
    public T getSelectedItem() {
        return (T) super.getSelectedItem();
    }

    public String getText() {
        String ret = this.getTextField().getText();
        if (ret.equals(this.helptext)) {
            ret = "";
        }
        return ret;
    }

    @SuppressWarnings("unchecked")
    public JTextField getTextField() {
        if (this.getEditor() == null) {
            return null;
        }
        return ((Editor) this.getEditor()).getTf();
    }

    /**
     * @param value
     * @return
     */
    abstract protected String getTextForValue(T value);

    @Override
    public boolean hasFocus() {
        if (super.hasFocus()) {
            return true;
        } else {
            final ComboBoxEditor lEditor = getEditor();
            if (lEditor instanceof SearchComboBox.Editor) {
                return ((SearchComboBox.Editor) lEditor).getTf().hasFocus();
            }
        }
        return false;
    }

    public boolean isAutoCompletionEnabled() {
        return this.autoCompletionEnabled;
    }

    public boolean isHelpTextVisible() {
        return this.helptext != null && this.helptext.equals(this.getText());
    }

    protected boolean isSearchCaseSensitive() {
        return false;
    }

    /**
     * if unknown values are allowed, the component will not try to find a valid entry on fopcus lost
     *
     * @return
     */
    public boolean isUnkownTextInputAllowed() {
        return this.unkownTextInputAllowed;
    }

    /**
     *
     */
    public void onChanged() {
    }

    public void setActualMaximumRowCount(final int count) {
        this.actualMaximumRowCount = count;
    }

    public void setAutoCompletionEnabled(final boolean autoCompletionEnabled) {
        this.autoCompletionEnabled = autoCompletionEnabled;
    }

    /**
     * @param object
     */
    public void setBadColor(final Color color) {
        this.badColorSet.setForeground(color);
        final JTextField tf = this.getTextField();
        if (tf != null) {
            tf.setForeground(this.currentColorSet.getForeground());
        }
    }

    private void setColorState(final ColorState cs) {
        this.currentColorSet = cs;
        final JTextField tf = this.getTextField();
        if (tf != null) {
            tf.setForeground(this.currentColorSet.getForeground());
        }
    }

    @Override
    public void setForeground(final Color fg) {
        super.setForeground(fg);
        this.setNormalColor(fg);
    }

    /**
     * @param addLinksDialog_layoutDialogContent_packagename_help
     */
    public void setHelpText(final String helptext) {
        this.helptext = helptext;
        this.updateHelpText();
    }

    public T getProtoType(List<T> model) {
        if (this.usePrototype() && model != null && model.size() > 0) {
            return model.get(0);
        } else {
            return null;
        }
    }

    public void setList(final List<T> listModel) {
        setList(listModel, true);
    }

    /**
     * Sets the Model for this combobox
     *
     * @param listModel
     * @param autoSelectFirstModelElement
     *            TODO
     */
    public void setList(final List<T> listModel, boolean autoSelectFirstModelElement) {
        this.data = listModel;
        final Object prototype = getProtoType(listModel);
        if (this.usePrototype()) {
            this.setPrototypeDisplayValue(prototype);
        }
        // without byPassTxtUpdate setModel would trigger setItem and this would overwrite the textfield
        if (!autoSelectFirstModelElement) {
            byPassTxtUpdate = true;
        }
        try {
            super.setModel(new DefaultComboBoxModel(listModel.toArray(new Object[] {})));
        } finally {
            // else a set selected item after setList might not throw events, because setModel alrady set the selected item to the first
            if (!autoSelectFirstModelElement) {
                setSelectedItem(null);
                byPassTxtUpdate = false;
            }
        }
        try {
            if (prototype != null) {
                /*
                 * http://stackoverflow.com/questions/5896282/how-to-prevent- jcombobox
                 * -from-becoming-unresponsive-when-using-a-custom-listcell
                 */
                final Accessible a = this.getUI().getAccessibleChild(this, 0);
                if (a instanceof javax.swing.plaf.basic.ComboPopup) {
                    final JList popupList = ((javax.swing.plaf.basic.ComboPopup) a).getList();
                    // route the comboBox' prototype to the list
                    // should happen in BasicComboxBoxUI
                    popupList.setPrototypeCellValue(prototype);
                }
            }
        } catch (final Throwable e) {
        }
        try {
            final BasicComboBoxUI udi = (BasicComboBoxUI) this.getUI();
            JComponent arrowButton = null;
            try {
                final Field field = BasicComboBoxUI.class.getDeclaredField("arrowButton");
                final BasicComboBoxUI bla = null;
                if (field != null) {
                    field.setAccessible(true);
                    arrowButton = (JComponent) field.get(udi);
                }
            } catch (final Throwable e) {
            }
            if (listModel.size() > 0) {
                udi.unconfigureArrowButton();
                udi.configureArrowButton();
                if (arrowButton != null) {
                    arrowButton.setEnabled(true);
                }
            } else {
                udi.unconfigureArrowButton();
                if (arrowButton != null) {
                    arrowButton.setEnabled(false);
                }
            }
        } catch (final Throwable e) {
            e.printStackTrace();
            // for lafs not extending BasicComboBoxUI it is possible to open a
            // empty popup
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.JComboBox#setEnabled(boolean)
     */
    @Override
    public void setEnabled(boolean b) {
        getEditor().getEditorComponent().setEnabled(b);
        super.setEnabled(b);
    }

    /**
     * @deprecated Use {@link #setActualMaximumRowCount(int)} instead
     */
    @Override
    @Deprecated
    public void setMaximumRowCount(final int count) {
        super.setMaximumRowCount(count);
    }

    /**
     * Do not use this method. For Type Safty, please use {@link #setList(java.util.List, boolean)} instead
     *
     * @deprecated use {@link #setList(java.util.List, boolean)}
     */
    @Override
    @Deprecated
    public void setModel(final ComboBoxModel aModel) {
        if (aModel == null) {
            super.setModel(new DefaultComboBoxModel());
            return;
        }
        throw new RuntimeException("Use setList()");
    }

    /**
     * @param fg
     */
    public void setNormalColor(final Color fg) {
        final JTextField tf = this.getTextField();
        if (tf != null) {
            tf.setForeground(this.currentColorSet.getForeground());
        }
    }

    @Override
    public void setRenderer(final ListCellRenderer aRenderer) {
        super.setRenderer(aRenderer);
    }

    /**
     * @param defaultDownloadFolder
     */
    public void setText(final String text) {
        this.getTextField().setText(text);
        this.updateHelpText();
    }

    @Override
    public void setToolTipText(final String text) {
        super.setToolTipText(text);
        final JTextField tf = this.getTextField();
        if (tf != null) {
            tf.setToolTipText(text);
        }
    }

    /**
     * if unknown values are allowed, the component will not try to find a valid entry on fopcus lost
     *
     * @param allowUnknownValuesEnabled
     */
    public void setUnkownTextInputAllowed(final boolean allowUnknownValuesEnabled) {
        this.unkownTextInputAllowed = allowUnknownValuesEnabled;
    }

    /**
     * @param search
     *            TODO
     * @param found
     */
    protected void sortFound(String search, final List<T> found) {
    }

    protected void setListSearchResults(final List<T> found, final List<T> all) {
    }

    /**
     * @param txt
     * @return
     */
    protected boolean textMatchesEntry(String txt) {
        if (txt == null) {
            return false;
        }
        txt = txt.toLowerCase(Locale.ENGLISH);
        String text;
        for (int i = 0; i < SearchComboBox.this.getModel().getSize(); i++) {
            text = SearchComboBox.this.getTextForValue((T) SearchComboBox.this.getModel().getElementAt(i));
            if (text != null && text.toLowerCase(Locale.ENGLISH).startsWith(txt)) {
                return true;
            }
        }
        return false;
    }

    /**
     *
     */
    public void updateColorByContent() {
        final String txt = this.getTextField().getText();
        if (this.helptext != null && this.helptext.equals(txt)) {
            this.setColorState(this.helpColorSet);
        } else {
            if (this.textMatchesEntry(txt)) {
                this.setColorState(this.normalColorSet);
            } else {
                this.setColorState(this.badColorSet);
            }
        }
    }

    /**
     *
     */
    private void updateHelpText() {
        if (this.getEditor() == null || this.helptext == null) {
            return;
        }
        Document doc = this.getTextField().getDocument();
        String txt;
        try {
            txt = doc.getText(0, doc.getLength());
        } catch (BadLocationException e) {
            txt = "";
        }
        final boolean hasHelpText = txt.equals(this.helptext);
        if (StringUtils.isEmpty(txt) || hasHelpText) {
            if (hasHelpText == false) {
                this.setText(this.helptext);
            }
            this.updateColorByContent();
        }
    }

    public boolean usePrototype() {
        return true;
    }

    public void setSelectionAfterAutoComplete(JTextField tf, final String txt, final int pos, List<T> found) {
        tf.setCaretPosition(pos);
        tf.select(txt.length(), tf.getText().length());
    }
}
