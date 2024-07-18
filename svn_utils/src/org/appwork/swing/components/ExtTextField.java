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
package org.appwork.swing.components;

import java.awt.Color;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.AbstractAction;
import javax.swing.JPopupMenu;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.UIManager;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.Highlighter;

import org.appwork.app.gui.copycutpaste.ContextMenuAdapter;
import org.appwork.storage.config.swing.ValueProvider;
import org.appwork.storage.config.swing.ValueProviderListener;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;

public class ExtTextField extends JTextField implements CaretListener, FocusListener, DocumentListener, ContextMenuAdapter, TextComponentInterface, ValueProviderListener<String> {
    /**
     *
     */
    private static final long serialVersionUID = -3625278218179478516L;
    protected Color           defaultColor;
    protected Color           helpColor;
    {
        this.addCaretListener(this);
        this.addFocusListener(this);
        this.defaultColor = this.getForeground();
        this.helpColor = (Color) UIManager.get("TextField.disabledForeground");
        if (this.helpColor == null) {
            this.helpColor = Color.LIGHT_GRAY;
        }
        this.getDocument().addDocumentListener(this);
        this.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void removeUpdate(DocumentEvent e) {
                applyTextHighlighter(e);
            }

            @Override
            public void insertUpdate(DocumentEvent e) {
                applyTextHighlighter(e);
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                applyTextHighlighter(e);
            }
        });
    }
    protected String                                         helpText             = null;
    private boolean                                          setting;
    private boolean                                          clearHelpTextOnFocus = true;
    private boolean                                          helperEnabled        = true;
    private String                                           textSetBySetText;
    protected final CopyOnWriteArrayList<ExtTextHighlighter> highlighters         = new CopyOnWriteArrayList<ExtTextHighlighter>();
    private final ValueProvider<String>                      provider;
    private final AtomicInteger                              onChange             = new AtomicInteger(0);

    /**
     *
     */
    public ExtTextField() {
        this(null);
    }

    /**
     * @param propertyHandlerProviderBridge
     */
    public ExtTextField(ValueProvider<String> provider) {
        this.provider = provider;
        if (provider != null) {
            provider.register(this, true);
            setText(provider.get());
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.config.swing.ValueProviderListener#onValueModified(org.appwork.storage.config.swing.ValueProvider,
     * java.lang.Object)
     */
    @Override
    public void onValueModified(ValueProvider<String> owner, final String newValue) {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                if (onChange.get() == 0) {
                    setText(newValue);
                }
            }
        };
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.storage.config.swing.ValueProviderListener#onValueValidationError(org.appwork.storage.config.swing.ValueProvider,
     * java.lang.Object, java.lang.Exception)
     */
    @Override
    public void onValueValidationError(ValueProvider<String> owner, String invalidValue, Exception exception) {
    }

    public void caretUpdate(final CaretEvent arg0) {
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event. DocumentEvent)
     */
    @Override
    public void changedUpdate(final DocumentEvent e) {
        if (!this.isHelperEnabled()) {
            this.onChanged();
            return;
        }
        if (!this.setting) {
            this.onChanged();
        }
    }

    public void focusGained(final FocusEvent arg0) {
        if (!this.isHelperEnabled()) {
            return;
        }
        if (super.getText().equals(this.helpText)) {
            if (this.isClearHelpTextOnFocus()) {
                super.setText("");
            } else {
                selectAll();
            }
        }
        this.setForeground(this.defaultColor);
    }

    public void focusLost(final FocusEvent arg0) {
        if (!this.isHelperEnabled()) {
            return;
        }
        if (this.getDocument().getLength() == 0 || super.getText().equals(this.helpText)) {
            this.setText(this.helpText);
            this.setForeground(this.helpColor);
        }
    }

    /**
     * @return
     */
    public Color getDefaultColor() {
        return this.defaultColor;
    }

    public Color getHelpColor() {
        return this.helpColor;
    }

    public String getHelpText() {
        return this.helpText;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.app.gui.copycutpaste.ContextMenuAdapter#getPopupMenu(org. appwork.app.gui.copycutpaste.CutAction,
     * org.appwork.app.gui.copycutpaste.CopyAction, org.appwork.app.gui.copycutpaste.PasteAction,
     * org.appwork.app.gui.copycutpaste.DeleteAction, org.appwork.app.gui.copycutpaste.SelectAction)
     */
    @Override
    public JPopupMenu getPopupMenu(MouseEvent event, final AbstractAction cutAction, final AbstractAction copyAction, final AbstractAction pasteAction, final AbstractAction deleteAction, final AbstractAction selectAction) {
        final JPopupMenu menu = new JPopupMenu();
        menu.add(cutAction);
        menu.add(copyAction);
        menu.add(pasteAction);
        menu.add(deleteAction);
        menu.add(selectAction);
        return menu;
    }

    @Override
    public void replaceSelection(final String content) {
        if (this.isHelperEnabled() && super.getText().equals(this.helpText) && StringUtils.isNotEmpty(content)) {
            super.setText("");
        }
        super.replaceSelection(content);
        this.setForeground(this.defaultColor);
    }

    @Override
    public String getText() {
        String ret = super.getText();
        if (!this.isHelperEnabled()) {
            return ret;
        }
        if (ret.equals(this.helpText) && this.getForeground() == this.helpColor) {
            ret = "";
        }
        return ret;
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event. DocumentEvent)
     */
    @Override
    public void insertUpdate(final DocumentEvent e) {
        if (!this.isHelperEnabled()) {
            this.onChanged();
            return;
        }
        if (!this.setting) {
            this.onChanged();
        }
    }

    public boolean isClearHelpTextOnFocus() {
        return this.clearHelpTextOnFocus;
    }

    public boolean isHelperEnabled() {
        return this.helperEnabled;
    }

    protected void applyTextHighlighter(DocumentEvent e) {
        refreshTextHighlighter();
    }

    public void refreshTextHighlighter() {
        final Highlighter highlighter = getHighlighter();
        highlighter.removeAllHighlights();
        for (final ExtTextHighlighter textHighlighter : getTextHighlighter()) {
            textHighlighter.highlight(highlighter, getText());
        }
    }

    public List<ExtTextHighlighter> getTextHighlighter() {
        return highlighters;
    }

    public boolean addTextHighlighter(ExtTextHighlighter highlighter) {
        return highlighter != null && highlighters.addIfAbsent(highlighter);
    }

    public boolean removeTextHighlighter(ExtTextHighlighter highlighter) {
        return highlighter != null && highlighters.remove(highlighter);
    }

    /**
     *
     */
    public void onChanged() {
        if (provider != null) {
            onChange.incrementAndGet();
            try {
                provider.set(getText());
            } finally {
                onChange.decrementAndGet();
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event. DocumentEvent)
     */
    @Override
    public void removeUpdate(final DocumentEvent e) {
        if (!this.isHelperEnabled()) {
            this.onChanged();
            return;
        }
        if (!this.setting) {
            this.onChanged();
        }
    }

    /**
     * @param b
     */
    public void setClearHelpTextOnFocus(final boolean b) {
        this.clearHelpTextOnFocus = b;
    }

    public void setHelpColor(final Color helpColor) {
        this.helpColor = helpColor;
    }

    public void setHelperEnabled(final boolean helperEnabled) {
        this.helperEnabled = helperEnabled;
    }

    /**
     * @param addLinksDialog_layoutDialogContent_input_help
     */
    public void setHelpText(final String helpText) {
        final String old = this.helpText;
        this.helpText = helpText;
        if (this.getText().length() == 0 || this.getText().equals(old)) {
            this.setText(this.helpText);
            this.setToolTipText(this.helpText);
            this.setForeground(this.helpColor);
        }
    }

    public ExtTextField setLabelMode(final boolean b) {
        this.setEditable(!b);
        this.setFocusable(!b);
        this.setBorder(b ? null : new JTextArea().getBorder());
        SwingUtils.setOpaque(this, !b);
        return this;
    }

    public ExtTextField text(String text) {
        setText(text);
        return this;
    }

    @Override
    public void setText(String t) {
        if (StringUtils.equals(t, getText())) {
            return;
        }
        if (!this.isHelperEnabled()) {
            if (StringUtils.equals(t == null ? "" : t, getText())) {
                return;
            }
            textSetBySetText = t;
            super.setText(t);
            return;
        }
        if (!this.setting) {
            this.setting = true;
            try {
                if (!this.hasFocus() && this.helpText != null && (t == null || t.length() == 0)) {
                    t = this.helpText;
                }
                if (StringUtils.equals(t == null ? "" : t, super.getText())) {
                    return;
                }
                textSetBySetText = t;
                super.setText(t);
                if (this.helpText != null) {
                    if (this.helpText.equals(t)) {
                        this.setForeground(this.helpColor);
                    } else {
                        this.setForeground(this.defaultColor);
                    }
                }
            } finally {
                this.setting = false;
                this.onChanged();
            }
        }
    }

    public String getTextSetBySetText() {
        return textSetBySetText;
    }

    /**
     * can be used to check if the text got changed for example by typing
     *
     * @return
     */
    public boolean isUnchangedSetText() {
        return StringUtils.equals(textSetBySetText, getText());
    }

    /**
     * @param b
     * @return
     */
    public ExtTextField focusable(boolean b) {
        setFocusable(b);
        return this;
    }
}
