package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.List;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.text.DefaultCaret;

import org.appwork.swing.MigPanel;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.utils.MinimumSelectionObserver;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.jdownloader.actions.AppAction;
import org.jdownloader.captcha.v2.CaptchaSolverLimitRule;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.components.AbstractAddAction;
import org.jdownloader.plugins.components.config.CaptchaSolverPluginConfig;

import jd.gui.swing.jdgui.BasicJDTable;

/**
 * The custom limits of one external captcha solver: a hint, the table of {@link CaptchaSolverLimitRule}s and an Add/Remove button bar.
 * The table is always shown in full (no scrollbar) and the mouse wheel is forwarded to the surrounding scroll pane, like the other tables
 * of the solver overview; whenever the number of rows changes, the given callback is run so the surrounding layout can adapt.
 */
public class CaptchaSolverLimitsPanel extends MigPanel {
    private static final long                          serialVersionUID = 1L;
    private final CaptchaSolverLimitsTableModel        model;
    private final BasicJDTable<CaptchaSolverLimitRule> table;
    private final JScrollPane                          scrollPane;
    /** Shown instead of the (empty) table's rows while there is no rule: the "Restore example rules" button in the first row. */
    private final MigPanel                             restorePanel;
    private boolean                                    showingRestorePanel = false;

    /**
     * @param onSizeChanged
     *            run after the table's height changed (rule added/removed), so the parent can revalidate its layout
     */
    public CaptchaSolverLimitsPanel(final CaptchaSolverPluginConfig cfg, final Runnable onSizeChanged) {
        super("ins 0, wrap 1", "[grow,fill]", "[][][]");
        SwingUtils.setOpaque(this, false);
        model = new CaptchaSolverLimitsTableModel(cfg);
        table = new BasicJDTable<CaptchaSolverLimitRule>(model) {
            private static final long serialVersionUID = 1L;

            @Override
            protected JPopupMenu onContextMenu(final JPopupMenu popup, final CaptchaSolverLimitRule contextObject, final List<CaptchaSolverLimitRule> selection, final ExtColumn<CaptchaSolverLimitRule> column, final MouseEvent mouseEvent) {
                popup.add(new JMenuItem(new AddAction()));
                popup.add(new JMenuItem(new RemoveAction(selection)));
                return popup;
            }

            @Override
            protected boolean onShortcutDelete(final List<CaptchaSolverLimitRule> selectedObjects, final KeyEvent evt, final boolean direct) {
                new RemoveAction(selectedObjects).actionPerformed(null);
                return true;
            }
        };
        table.getTableHeader().setReorderingAllowed(false);
        table.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        final JTextArea hint = new JTextArea();
        SwingUtils.setOpaque(hint, false);
        hint.setEditable(false);
        hint.setLineWrap(true);
        hint.setWrapStyleWord(true);
        hint.setFocusable(false);
        /*
         * setText moves the caret to the end of the text, and the caret then scrolls the surrounding scroll pane so it becomes visible,
         * which made the whole settings page jump to this hint whenever a solver with custom limits got selected. The hint is read-only,
         * so its caret must never move.
         */
        ((DefaultCaret) hint.getCaret()).setUpdatePolicy(DefaultCaret.NEVER_UPDATE);
        hint.setText(_GUI.T.CaptchaSolverLimits_hint());
        add(hint, "growx, wmin 10, gapbottom 5");
        scrollPane = new JScrollPane(table);
        /* Never scroll: the table is always shown at full size (all rows), see updateTableSize(). */
        scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        SolverOrderContainer.passMouseWheelToParent(scrollPane);
        add(scrollPane, "growx");
        restorePanel = new MigPanel("ins 2 4 2 4", "[]", "[]");
        restorePanel.setOpaque(true);
        restorePanel.setBackground(table.getBackground());
        restorePanel.add(new JButton(new AppAction() {
            private static final long serialVersionUID = 1L;
            {
                setName(_GUI.T.CaptchaSolverLimits_restoreExamples());
            }

            @Override
            public void actionPerformed(final ActionEvent e) {
                model.restoreExampleRules();
            }
        }));
        final MigPanel buttonBar = new MigPanel("ins 0", "[][][grow,fill]", "[]");
        final JButton addButton = new JButton(new AddAction());
        final RemoveAction removeAction = new RemoveAction(null);
        final JButton removeButton = new JButton(removeAction);
        buttonBar.add(addButton, "height 26!,sg 1");
        buttonBar.add(removeButton, "height 26!,sg 1");
        buttonBar.add(Box.createHorizontalGlue());
        add(buttonBar, "growx");
        /* Remove is only enabled while at least one rule is selected. */
        table.getSelectionModel().addListSelectionListener(new MinimumSelectionObserver(table, removeAction, 1));
        removeAction.setEnabled(false);
        updateEmptyState();
        updateTableSize();
        model.addTableModelListener(new TableModelListener() {
            @Override
            public void tableChanged(final TableModelEvent e) {
                /*
                 * Swing notifies the listeners of a model in reverse order of registration, so this listener runs before the table itself
                 * has processed the change and its preferred size would still be the old one. Therefore everything is done afterwards.
                 */
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        updateEmptyState();
                        updateTableSize();
                        if (onSizeChanged != null) {
                            onSizeChanged.run();
                        }
                    }
                });
            }
        });
    }

    /**
     * While there is no rule (e.g. the user deleted all of them), the table's rows are replaced by a first row holding the "Restore
     * example rules" button; the header stays visible above it. As soon as a rule exists, the table is shown again.
     */
    private void updateEmptyState() {
        final boolean empty = model.getRowCount() == 0;
        if (empty == showingRestorePanel) {
            return;
        }
        showingRestorePanel = empty;
        if (empty) {
            scrollPane.setViewportView(restorePanel);
            /* Taking the table out of the viewport also removed its header from the scroll pane, so put it back above the button. */
            scrollPane.setColumnHeaderView(table.getTableHeader());
        } else {
            scrollPane.setViewportView(table);
        }
    }

    /**
     * Sizes the scroll pane to fit the table header plus all rows (or the restore button row while empty), so everything is shown without
     * a scrollbar.
     */
    private void updateTableSize() {
        final int height;
        if (showingRestorePanel) {
            final int headerHeight = table.getTableHeader() != null ? table.getTableHeader().getPreferredSize().height : 0;
            height = headerHeight + Math.max(table.getRowHeight(), restorePanel.getPreferredSize().height) + 10;
        } else {
            height = SolverOrderContainer.tableFullHeight(table);
        }
        final Dimension size = new Dimension(table.getPreferredSize().width, height);
        scrollPane.setPreferredSize(size);
        scrollPane.setMinimumSize(size);
    }

    private class AddAction extends AbstractAddAction {
        private static final long serialVersionUID = 1L;

        public AddAction() {
            super();
            setName(_GUI.T.CaptchaSolverLimits_add());
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            final CaptchaSolverLimitRule rule = model.addRule();
            /*
             * Land in the new rule's "Name" cell with its text selected. The table may just have been put back into view (it is replaced
             * by the restore button while empty) and is sized/laid out later, and a layout change would cancel the editing right away.
             * Therefore editing starts only after the pending resize and the following layout pass, hence the two nested invokeLater.
             */
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    SwingUtilities.invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            model.startEditingName(rule);
                        }
                    });
                }
            });
        }
    }

    /** Removes the given rules, or the table's current selection if none are given (button bar instance). */
    private class RemoveAction extends AppAction {
        private static final long                serialVersionUID = 1L;
        private final List<CaptchaSolverLimitRule> selected;

        public RemoveAction(final List<CaptchaSolverLimitRule> selected) {
            this.selected = selected;
            setName(_GUI.T.CaptchaRules_remove_button());
            setIconKey(IconKey.ICON_REMOVE);
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            final List<CaptchaSolverLimitRule> remove = selected != null ? selected : model.getSelectedObjects();
            if (remove == null || remove.isEmpty()) {
                return;
            }
            try {
                Dialog.getInstance().showConfirmDialog(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN, _GUI.T.literall_are_you_sure(), _GUI.T.CaptchaRules_remove_confirm(), null, null, null);
            } catch (final DialogClosedException ex) {
                return;
            } catch (final DialogCanceledException ex) {
                return;
            }
            model.removeRules(remove);
        }
    }
}
