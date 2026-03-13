/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 * ====================================================================================================================================================
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.executer;

import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

import org.appwork.loggingv3.simple.LogRecord2;
import org.appwork.loggingv3.simple.sink.AbstractSink;

/**
 * Log sink that appends all log records to a JTextArea (e.g. the AdminHelperProcess debug window). Updates the text area on the EDT.
 */
public class LogToDebugWindowSink extends AbstractSink {
    private final JTextArea textArea;

    public LogToDebugWindowSink(JTextArea textArea) {
        this.textArea = textArea;
    }

    @Override
    public void publish(final LogRecord2 record) {
        if (textArea == null) {
            return;
        }
        final String line = format(record);
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                if (textArea != null) {
                    textArea.append(line);
                    if (!line.endsWith("\n")) {
                        textArea.append("\n");
                    }
                    textArea.setCaretPosition(textArea.getDocument().getLength());
                }
            }
        });
    }
}
