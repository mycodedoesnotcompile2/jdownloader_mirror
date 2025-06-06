package org.jdownloader.gui.notify;

import java.awt.Component;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.JTextComponent;

import net.miginfocom.swing.MigLayout;

import org.appwork.swing.components.ExtTextArea;
import org.appwork.swing.components.ExtTextPane;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.SwingUtils;

public class BasicContentPanel extends AbstractBubbleContentPanel {
    public BasicContentPanel(String text, Icon icon) {
        super(icon);
        setLayout(new MigLayout("ins 0,wrap 2", "[][grow,fill]", "[]"));
        add(new JLabel(icon), "aligny top");
        add(getMessage(text), "aligny center");
        SwingUtils.setOpaque(this, false);
    }

    protected void hyperlinkUpdate(final HyperlinkEvent e) {
        if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
            CrossSystem.openURL(e.getURL());
        }
    }

    private Component getMessage(String text) {
        final JTextComponent ret;
        if (text.matches("^<html>.+")) {
            final ExtTextPane textPane = new ExtTextPane();
            textPane.setContentType("text/html");
            textPane.addHyperlinkListener(new HyperlinkListener() {
                public void hyperlinkUpdate(final HyperlinkEvent e) {
                    BasicContentPanel.this.hyperlinkUpdate(e);
                }
            });
            textPane.setEditable(false);
            textPane.setFocusable(false);
            /* Allow line wrap */
            textPane.setEditorKit(new javax.swing.text.StyledEditorKit());
            ret = textPane;
        } else {
            final ExtTextArea textArea = new ExtTextArea();
            textArea.setLabelMode(true);
            textArea.setLineWrap(true);
            textArea.setWrapStyleWord(true);
            ret = textArea;
        }
        SwingUtils.setOpaque(ret, false);
        ret.setText(text);
        return ret;
    }

    @Override
    public void updateLayout() {
    }
}
