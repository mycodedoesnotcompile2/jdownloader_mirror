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
package org.appwork.utils.logging2.sendlogs;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.SwingConstants;

import org.appwork.swing.MigPanel;
import org.appwork.utils.swing.dialog.AbstractDialog;

public class SendLogDialog extends AbstractDialog<Object> {

    protected final List<LogFolder> folders;
    protected LogModel              model = null;

    public SendLogDialog(final List<LogFolder> folders) {
        super(0, T.T.SendLogDialog_SendLogDialog_title_(), null, T.T.LogAction_actionPerformed_upload_(), null);
        this.folders = folders;
    }

    @Override
    protected Object createReturnValue() {
        return null;
    }

    @Override
    protected int getPreferredHeight() {
        if (folders == null || folders.size() == 0) {
            return 100;
        } else {
            return 600;
        }
    }

    protected boolean isResizable() {
        return folders != null && folders.size() > 0;
    }

    @Override
    public JComponent layoutDialogContent() {
        final MigPanel panel = new MigPanel("ins 0,wrap 1", "[grow,fill]", "[][grow,fill]");
        if (folders == null || folders.size() == 0) {
            final JLabel noLogs = new JLabel(T.T.SendLogDialog_layoutDialogContent_nologs_());
            noLogs.setForeground(Color.RED);
            noLogs.setHorizontalAlignment(SwingConstants.CENTER);
            panel.add(noLogs);
        } else {
            final JLabel lbl = new JLabel(T.T.SendLogDialog_layoutDialogContent_desc_());
            panel.add(lbl);
            model = new LogModel(folders);
            final LogTable table = new LogTable(model);
            panel.add(new JScrollPane(table));
        }
        return panel;
    }

    public List<LogFolder> getSelectedFolders() {
        final List<LogFolder> ret = new ArrayList<LogFolder>();
        if (model != null) {
            for (final LogFolder logFolder : model.getTableData()) {
                if (logFolder.isSelected()) {
                    ret.add(logFolder);
                }
            }
        }
        return ret;
    }
}
