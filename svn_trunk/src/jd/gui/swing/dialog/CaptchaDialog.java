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
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.

package jd.gui.swing.dialog;

import java.awt.Image;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;

import net.miginfocom.swing.MigLayout;

import org.appwork.swing.components.ExtTextField;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.DomainInfo;
import org.jdownloader.captcha.v2.challenge.stringcaptcha.BasicCaptchaChallenge;
import org.jdownloader.gui.translate._GUI;

/**
 * This Dialog is used to display a Inputdialog for the captchas
 */
public class CaptchaDialog extends AbstractImageCaptchaDialog<String> implements ActionListener {

    private ExtTextField textField;
    private ExtTextField lengthField;
    private String       suggest;

    public CaptchaDialog(BasicCaptchaChallenge captchaChallenge, final int flag, DialogType type, final DomainInfo DomainInfo, final Image image, final String explain) {
        this(captchaChallenge, flag, type, DomainInfo, new Image[] { image }, explain);
    }

    public CaptchaDialog(BasicCaptchaChallenge captchaChallenge, int flag, DialogType type, DomainInfo domainInfo, Image[] images, String explain) {
        super(captchaChallenge, flag | Dialog.STYLE_HIDE_ICON, _GUI.T.gui_captchaWindow_askForInput(domainInfo.getTld()), type, domainInfo, explain, images);

    }

    @Override
    protected void packed() {

        textField.addFocusListener(new FocusListener() {

            public void focusLost(FocusEvent e) {
                cancel();
            }

            public void focusGained(FocusEvent e) {

            }
        });
        getDialog().addWindowFocusListener(new WindowFocusListener() {

            @Override
            public void windowLostFocus(WindowEvent windowevent) {
            }

            @Override
            public void windowGainedFocus(WindowEvent windowevent) {

                textField.requestFocusInWindow();
            }
        });

    }

    @Override
    protected JComponent createInputComponent() {

        this.textField = new ExtTextField() {

            /**
             *
             */
            private static final long serialVersionUID = 1L;

            @Override
            public void onChanged() {
                cancel();
            }

        };

        textField.setClearHelpTextOnFocus(false);
        textField.addFocusListener(new FocusListener() {

            @Override
            public void focusLost(FocusEvent e) {
                // System.out.println(e);
            }

            @Override
            public void focusGained(FocusEvent e) {
                // System.out.println(e);
                textField.selectAll();
            }
        });
        /*
         * The user cannot enter more characters than the maximum length of the challenge. Typing is rejected, pasted text is shortened.
         * Both play the error sound, like the dialog for too long filenames.
         */
        ((AbstractDocument) textField.getDocument()).setDocumentFilter(new DocumentFilter() {
            @Override
            public void insertString(final FilterBypass fb, final int offset, final String string, final AttributeSet attr) throws BadLocationException {
                replace(fb, offset, 0, string, attr);
            }

            @Override
            public void replace(final FilterBypass fb, final int offset, final int length, final String text, final AttributeSet attrs) throws BadLocationException {
                final String inserted = text == null ? "" : text;
                final int max = ((BasicCaptchaChallenge) challenge).getMaxLength();
                if (max <= 0) {
                    super.replace(fb, offset, length, inserted, attrs);
                    return;
                }
                final int allowed = max - (fb.getDocument().getLength() - length);
                if (inserted.length() <= allowed) {
                    super.replace(fb, offset, length, inserted, attrs);
                    return;
                }
                CrossSystem.playErrorSound();
                if (allowed > 0) {
                    super.replace(fb, offset, length, inserted.substring(0, allowed), attrs);
                }
            }
        });
        /* The send button is only enabled if the answer is long enough, see BasicCaptchaChallenge#getMinLength. */
        textField.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                updateOkButtonEnabled();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                updateOkButtonEnabled();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                updateOkButtonEnabled();
            }
        });
        /* The challenge description is shown above the image, see AbstractImageCaptchaDialog#addBeforeImage. */
        if (suggest != null) {
            textField.setText(suggest);
            // this.textField.requestFocusInWindow();
            // this.textField.selectAll();
        }
        final BasicCaptchaChallenge basicChallenge = (BasicCaptchaChallenge) challenge;
        if (basicChallenge.getMinLength() > 1 || basicChallenge.getMaxLength() > 0) {
            /* Length info below the image as plain text, like the click info of the click captcha. */
            lengthField = new ExtTextField();
            lengthField.setEditable(false);
            updateOkButtonEnabled();
            final JPanel panel = new JPanel(new MigLayout("ins 0,wrap 1", "[grow,fill]", "[]3[]"));
            panel.setOpaque(false);
            panel.add(lengthField);
            panel.add(textField);
            return panel;
        }
        updateOkButtonEnabled();
        return textField;
    }

    /**
     * Enables the send button only if the entered text has the required length of the challenge (min. and max. length). This also blocks the enter key because it
     * triggers the default button.
     */
    private void updateOkButtonEnabled() {
        final ExtTextField textField = this.textField;
        final JButton okButton = this.okButton;
        if (textField == null || okButton == null || !(challenge instanceof BasicCaptchaChallenge)) {
            return;
        }
        final BasicCaptchaChallenge basicChallenge = (BasicCaptchaChallenge) challenge;
        okButton.setEnabled(basicChallenge.isValidLength(textField.getText()));
        updateLengthInfo();
    }

    /** Shows the number of entered characters and how many are required. */
    private void updateLengthInfo() {
        final ExtTextField lengthField = this.lengthField;
        if (lengthField == null || textField == null) {
            return;
        }
        final BasicCaptchaChallenge basicChallenge = (BasicCaptchaChallenge) challenge;
        final int length = textField.getText().trim().length();
        final int min = basicChallenge.getMinLength();
        final int max = basicChallenge.getMaxLength();
        final String info;
        if (basicChallenge.hasFixedLength()) {
            /* Exact length required */
            info = _GUI.T.CaptchaDialog_length_exact(length, max);
        } else if (max > 0 && min > 1) {
            info = _GUI.T.CaptchaDialog_length_range(length, max, min);
        } else if (max > 0) {
            info = _GUI.T.CaptchaDialog_length_max(length, max);
        } else {
            info = _GUI.T.CaptchaDialog_length_min(length, min);
        }
        lengthField.setText(info);
    }

    protected void initFocus(final JComponent focus) {

    }

    @Override
    protected String createReturnValue() {
        final ExtTextField textField = this.textField;
        if (textField == null) {
            return null;
        }
        return textField.getText();
    }

    public void suggest(String value) {
        suggest = value;
        if (textField != null && StringUtils.isEmpty(textField.getText())) {
            boolean hasFocus = textField.hasFocus();
            textField.setText(value);
            if (hasFocus) {
                textField.selectAll();
            }
        }
    }

}