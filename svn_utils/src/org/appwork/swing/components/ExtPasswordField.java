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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.JPasswordField;
import javax.swing.KeyStroke;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.Highlighter;

import org.appwork.swing.MigPanel;
import org.appwork.utils.StringUtils;

public class ExtPasswordField extends MigPanel implements FocusListener, DocumentListener, TextComponentInterface, ActionListener {
    /**
     * @author Thomas
     *
     */
    public final class CustomTextField extends ExtTextField {
        /*
         * (non-Javadoc)
         *
         * @see org.appwork.swing.components.ExtTextField#replaceSelection(java.lang.String)
         */
        @Override
        public void replaceSelection(String content) {
            ExtPasswordField.this.setText(content);
        }
    }

    /**
     * @author Thomas
     *
     */
    public final class CustomPasswordField extends JPasswordField {
        /*
         * (non-Javadoc)
         *
         * @see javax.swing.text.JTextComponent#replaceSelection(java.lang.String)
         */
        private boolean key = false;

        @Override
        public void replaceSelection(String content) {
            if (key) {
                // type on keyboard
                super.replaceSelection(content);
            } else {
                // paste externaly. like contextmenu
                ExtPasswordField.this.setText(content);
            }
        }

        @Override
        protected boolean processKeyBinding(KeyStroke ks, KeyEvent e, int condition, boolean pressed) {
            // forward events
            // this will cause to trigger a pressed event on enter. this
            // will the trigger the default action of dialogs - for example
            ExtPasswordField.this.dispatchEvent(e);
            if (e.isConsumed()) {
                return true;
            } else {
                key = true;
                try {
                    return super.processKeyBinding(ks, e, condition, pressed);
                } finally {
                    key = false;
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.JComponent#setBackground(java.awt.Color)
     */
    @Override
    public void setBackground(Color bg) {
        super.setBackground(bg);
        if (renderer != null) {
            renderer.setBackground(bg);
        }
        if (editor != null) {
            editor.setBackground(bg);
        }
    }

    /**
     *
     */
    private static final long                                serialVersionUID = 9035297840443317147L;
    public static String                                     MASK             = "••••••••••";
    protected final ExtTextField                             renderer;
    protected final JPasswordField                           editor;
    private boolean                                          rendererMode;
    private char[]                                           password         = new char[] {};
    private String                                           mask             = null;
    private final AtomicInteger                              modifier         = new AtomicInteger(0);
    protected final CopyOnWriteArrayList<ExtTextHighlighter> highlighters     = new CopyOnWriteArrayList<ExtTextHighlighter>();

    /**
     * @param constraints
     * @param columns
     * @param rows
     */
    public ExtPasswordField() {
        super("ins 0", "[grow,fill]", "[grow,fill]");
        this.renderer = new CustomTextField();
        this.editor = new CustomPasswordField();
        this.renderer.addFocusListener(this);
        this.editor.addFocusListener(this);
        this.add(this.renderer, "hidemode 3");
        this.add(this.editor, "hidemode 3");
        this.editor.setText("");
        // this.renderer.setBackground(Color.RED);
        this.renderer.setText("");
        this.editor.getDocument().addDocumentListener(this);
        this.editor.addActionListener(this);
        this.renderer.setHelpText("");
        this.setRendererMode(true);
        editor.getDocument().addDocumentListener(new DocumentListener() {
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

    /**
     * @see java.awt.Component#addMouseMotionListener(java.awt.event.MouseMotionListener)
     */
    @Override
    public synchronized void addMouseMotionListener(MouseMotionListener l) {
        super.addMouseMotionListener(l);
        editor.addMouseMotionListener(l);
        renderer.addMouseMotionListener(l);
    }

    /**
     * @see java.awt.Component#removeMouseMotionListener(java.awt.event.MouseMotionListener)
     */
    @Override
    public synchronized void removeMouseMotionListener(MouseMotionListener l) {
        super.removeMouseMotionListener(l);
        editor.removeMouseMotionListener(l);
        renderer.removeMouseMotionListener(l);
    }

    @Override
    protected boolean processKeyBinding(KeyStroke ks, KeyEvent e, int condition, boolean pressed) {
        return super.processKeyBinding(ks, e, condition, pressed);
    }

    @Override
    public synchronized void addKeyListener(final KeyListener l) {
        this.renderer.addKeyListener(l);
        this.editor.addKeyListener(l);
    }

    protected void applyTextHighlighter(DocumentEvent e) {
        final Highlighter highlighter = editor.getHighlighter();
        highlighter.removeAllHighlights();
        final CharSequence pw = new CharSequence() {
            final char[] pw = editor.getPassword();

            @Override
            public CharSequence subSequence(int start, int end) {
                return null;
            }

            @Override
            public int length() {
                return pw.length;
            }

            @Override
            public char charAt(int index) {
                return pw[index];
            }
        };
        for (final ExtTextHighlighter textHighlighter : getTextHighlighter()) {
            textHighlighter.highlight(highlighter, pw);
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

    @Override
    public synchronized void addMouseListener(final MouseListener l) {
        this.renderer.addMouseListener(l);
        this.editor.addMouseListener(l);
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event. DocumentEvent)
     */
    @Override
    public void changedUpdate(final DocumentEvent e) {
        if (this.modifier.get() == 0) {
            if (!Arrays.equals(this.editor.getPassword(), this.getMask().toCharArray())) {
                this.onChanged();
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.event.FocusListener#focusGained(java.awt.event.FocusEvent)
     */
    @Override
    public void focusGained(final FocusEvent e) {
        if (e.getSource() == this.renderer) {
            this.setRendererMode(false);
            this.editor.requestFocus();
        } else {
            // http://svn.jdownloader.org/issues/49542
            if (this.password.length == 0) {
                this.setEditorText("");
            }
            this.editor.selectAll();
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.event.FocusListener#focusLost(java.awt.event.FocusEvent)
     */
    @Override
    public void focusLost(final FocusEvent e) {
        if (e.getSource() == this.editor || e == null) {
            final char[] pass = this.editor.getPassword();
            final char[] mask = this.getMask().toCharArray();
            if (!Arrays.equals(pass, mask)) {
                this.password = pass;
            }
            this.setRendererMode(true);
            this.setHelpText(this.getHelpText());
        }
    }

    public javax.swing.text.Document getDocument() {
        return this.editor.getDocument();
    }

    public Color getHelpColor() {
        return this.renderer.getHelpColor();
    }

    public String getHelpText() {
        return this.renderer.getHelpText();
    }

    /**
     * @return
     */
    protected String getMask() {
        return this.mask != null ? this.mask : ExtPasswordField.MASK;
    }

    public char[] getPassword() {
        if (this.editor.isVisible()) {
            final char[] pass = this.editor.getPassword();
            final char[] mask = this.getMask().toCharArray();
            if (!Arrays.equals(pass, mask)) {
                this.password = pass;
            }
        }
        return this.password;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.TextComponentInterface#getText()
     */
    @Override
    public String getText() {
        final char[] pass = this.getPassword();
        return pass == null ? null : new String(pass);
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event. DocumentEvent)
     */
    @Override
    public void insertUpdate(final DocumentEvent e) {
        if (this.modifier.get() == 0) {
            final char[] pass = this.editor.getPassword();
            final char[] mask = this.getMask().toCharArray();
            if (!Arrays.equals(pass, mask)) {
                this.onChanged();
                if (pass.length > 0) {
                    this.renderer.setText(this.getMask());
                } else {
                    this.renderer.setText("");
                }
            }
        }
    }

    public void onChanged() {
    }

    @Override
    public synchronized void removeKeyListener(final KeyListener l) {
        this.renderer.removeKeyListener(l);
        this.editor.removeKeyListener(l);
    }

    @Override
    public synchronized void removeMouseListener(final MouseListener l) {
        this.renderer.removeMouseListener(l);
        this.editor.removeMouseListener(l);
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event. DocumentEvent)
     */
    @Override
    public void removeUpdate(final DocumentEvent e) {
        if (this.modifier.get() == 0) {
            if (!Arrays.equals(this.editor.getPassword(), this.getMask().toCharArray())) {
                this.onChanged();
            }
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.TextComponentInterface#selectAll()
     */
    @Override
    public void selectAll() {
        this.editor.selectAll();
    }

    private void setEditorText(final String text) {
        this.modifier.incrementAndGet();
        try {
            this.editor.setText(text);
        } finally {
            this.modifier.decrementAndGet();
        }
    }

    @Override
    public void setEnabled(final boolean b) {
        this.editor.setEnabled(b);
        this.renderer.setEnabled(b);
        super.setEnabled(b);
    }

    public void setHelpColor(final Color helpColor) {
        this.renderer.setHelpColor(helpColor);
    }

    /**
     * @param addLinksDialog_layoutDialogContent_input_help
     */
    public void setHelpText(final String helpText) {
        this.renderer.setHelpText(helpText);
        final char[] password = getPassword();
        if (this.getHelpText() != null && (password == null || password.length == 0 || this.getMask().equals(new String(password)))) {
            this.renderer.setText(this.getHelpText());
            this.renderer.setForeground(this.getHelpColor());
        } else {
            this.renderer.setText(this.getMask());
            this.renderer.setForeground(this.renderer.getDefaultColor());
        }
        this.setRendererMode(this.rendererMode);
    }

    public void setMask(final String mask) {
        this.mask = mask;
    }

    public boolean requestFocusInWindow() {
        return renderer.requestFocusInWindow();
    }

    public void setPassword(final char[] password) {
        this.password = password;
        setEditorText(new String(password));
        this.setHelpText(this.getHelpText());
        this.onChanged();
    }

    /**
     * @param b
     */
    private void setRendererMode(boolean b) {
        this.rendererMode = b;
        b &= this.getHelpText() != null;
        this.renderer.setVisible(b);
        this.editor.setVisible(!b);
        this.revalidate();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.TextComponentInterface#setText(java.lang .String)
     */
    @Override
    public void setText(final String text) {
        this.setPassword(StringUtils.isEmpty(text) ? new char[0] : text.toCharArray());
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        this.onChanged();
    }
}
