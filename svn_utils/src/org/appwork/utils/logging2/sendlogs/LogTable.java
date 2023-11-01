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
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;

import javax.swing.JLabel;
import javax.swing.JPopupMenu;
import javax.swing.ListSelectionModel;

import org.appwork.swing.action.BasicAction;
import org.appwork.swing.exttable.AlternateHighlighter;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtComponentRowHighlighter;
import org.appwork.swing.exttable.ExtTable;
import org.appwork.utils.ColorUtils;

public class LogTable extends ExtTable<LogFolder> {

    public LogTable(final LogModel model) {
        super(model);
        setShowVerticalLines(true);
        setShowGrid(true);
        setShowHorizontalLines(true);
        setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

        final Color b2 = getForeground();
        final Color f2 = getBackground();
        getModel().addExtComponentRowHighlighter(new ExtComponentRowHighlighter<LogFolder>(f2, b2, null) {

            @Override
            public boolean accept(final ExtColumn<LogFolder> column, final LogFolder value, final boolean selected, final boolean focus, final int row) {
                return selected;
            }

        });
        getModel().addExtComponentRowHighlighter(new ExtComponentRowHighlighter<LogFolder>(null, Color.ORANGE, null) {

            @Override
            public boolean accept(final ExtColumn<LogFolder> column, final LogFolder value, final boolean selected, final boolean focus, final int row) {
                return value.isCurrent();
            }

        });
        addRowHighlighter(new AlternateHighlighter(null, ColorUtils.getAlphaInstance(new JLabel().getForeground(), 6)));
        setIntercellSpacing(new Dimension(0, 0));
    }

    @Override
    protected JPopupMenu onContextMenu(final JPopupMenu popup, final LogFolder contextObject, final java.util.List<LogFolder> selection, final ExtColumn<LogFolder> column, final MouseEvent mouseEvent) {
        popup.add(new BasicAction() {
            {
                setName(T.T.LogTable_onContextMenu_enable_());
            }

            @Override
            public void actionPerformed(final ActionEvent e) {
                for (final LogFolder f : selection) {
                    f.setSelected(true);
                }
                repaint();
            }
        });
        popup.add(new BasicAction() {
            {
                setName(T.T.LogTable_onContextMenu_disable_());
            }

            @Override
            public void actionPerformed(final ActionEvent e) {
                for (final LogFolder f : selection) {
                    f.setSelected(false);
                }
                repaint();
            }
        });
        return popup;
    }

}
