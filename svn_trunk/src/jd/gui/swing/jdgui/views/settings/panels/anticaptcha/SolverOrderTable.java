package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.appwork.swing.exttable.ExtColumn;
import org.jdownloader.api.captcha.CaptchaAPIManualRemoteSolverService;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.captcha.v2.solver.service.BrowserSolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.components.captchasolver.PluginForCaptchaSolverSolverService;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.gui.swing.jdgui.views.settings.components.ToggleEnabledSelectionAction;

public class SolverOrderTable extends BasicJDTable<SolverService> {
    public interface SelectionListener {
        void onSolverSelected(SolverService solver);
    }

    private SelectionListener selectionListener;

    public void setSelectionListener(SelectionListener selectionListener) {
        this.selectionListener = selectionListener;
    }

    public SolverOrderTable() {
        super(new SolverOrderTableModel());
        setShowHorizontalLineBelowLastEntry(false);
        setShowHorizontalLines(true);
        /* Focusable (click on a row): Without focus the table never receives keyboard shortcuts like Ctrl+A (select all). */
        setFocusable(true);
        getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(ListSelectionEvent e) {
                if (e.getValueIsAdjusting()) {
                    return;
                }
                /* The model clears and restores the selection while reloading its rows: not a user action, ignore it. */
                if (((SolverOrderTableModel) getModel()).isReloading()) {
                    return;
                }
                fireSelectionChanged();
            }
        });
    }

    /** Informs the selection listener about the currently selected solver (null = nothing selected). */
    public void fireSelectionChanged() {
        if (selectionListener != null) {
            final int row = getSelectedRow();
            final SolverService solver = row >= 0 ? getModel().getObjectbyRow(row) : null;
            selectionListener.onSolverSelected(solver);
        }
    }

    @Override
    protected boolean onDoubleClick(MouseEvent e, SolverService obj) {
        if (obj instanceof BrowserSolverService) {
            showSelectBrowserDialog((BrowserSolverService) obj);
            return true;
        } else if (obj instanceof CaptchaAPIManualRemoteSolverService) {
            /* MyJDownloader remote solver: Opens the MyJDownloader configuration (same as "Configure" in the context menu). */
            ((CaptchaAPIManualRemoteSolverService) obj).openConfiguration();
            return true;
        }
        /* Config is shown inline below the table now; no separate properties dialog. */
        return false;
    }

    /** Lets the user choose the browser of the browser solver and stores the choice. */
    public void showSelectBrowserDialog(final BrowserSolverService browserSolver) {
        if (browserSolver.showSelectBrowserDialog()) {
            /* Status text and ready state are computed live, so a redraw of the solver's row is enough. */
            final int row = getModel().getRowforObject(browserSolver);
            if (row >= 0) {
                getModel().fireTableRowsUpdated(row, row);
            }
        }
    }

    /**
     * Context menu actions, depending on the type of the right-clicked solver (all independent of the solver's ready state):
     * <ul>
     * <li>MyJDownloader remote solver -> "Configure" (opens the My.JDownloader tab).</li>
     * <li>External (plugin based) solver -> "Open account manager" (and selects the solver's first account, if it has one) and, if the
     * solver has a buy page, "Buy credits".</li>
     * <li>Browser solver -> "Select browser" (choose the browser used to open captcha pages, or the OS default).</li>
     * </ul>
     * All solver types additionally get an enable/disable action for the whole selection.
     */
    @Override
    protected JPopupMenu onContextMenu(final JPopupMenu popup, final SolverService contextObject, final List<SolverService> selection, final ExtColumn<SolverService> column, final MouseEvent mouseEvent) {
        if (contextObject instanceof CaptchaAPIManualRemoteSolverService) {
            final CaptchaAPIManualRemoteSolverService myjd = (CaptchaAPIManualRemoteSolverService) contextObject;
            /* MyJDownloader logo icon in front of the text (reuses the solver's own icon). */
            final JMenuItem configure = new JMenuItem(_GUI.T.CaptchaSolverService_status_configure(), myjd.getIcon(18));
            configure.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    myjd.openConfiguration();
                }
            });
            popup.add(configure);
        } else if (contextObject instanceof PluginForCaptchaSolverSolverService) {
            final PluginForCaptchaSolverSolverService external = (PluginForCaptchaSolverSolverService) contextObject;
            /*
             * Account manager icon in front of the text (same icon the Account Manager settings panel uses). Only offered if the solver
             * has an account the account manager could show.
             */
            if (external.hasAccount()) {
                final JMenuItem openAccountManager = new JMenuItem(_GUI.T.SolverOrderTable_context_openAccountManager(), new AbstractIcon(IconKey.ICON_PREMIUM, 18));
                openAccountManager.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent e) {
                        external.openAccountManagerSelectingFirstMatchingAccount();
                    }
                });
                popup.add(openAccountManager);
            }
            if (external.getBuyURL() != null) {
                /* Paid solver: same "Buy" icon and affiliate/redirect link logic as the account manager. */
                final JMenuItem buyCredits = new JMenuItem(_GUI.T.SolverOrderTable_context_buyCredits(), new AbstractIcon(IconKey.ICON_BUY, 18));
                buyCredits.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent e) {
                        external.openBuyPage("captchasolver/solvertable/context");
                    }
                });
                popup.add(buyCredits);
            }
        } else if (contextObject instanceof BrowserSolverService) {
            final BrowserSolverService browserSolver = (BrowserSolverService) contextObject;
            final JMenuItem selectBrowser = new JMenuItem(_GUI.T.SolverOrderTable_context_selectBrowser(), browserSolver.getIcon(18));
            selectBrowser.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    showSelectBrowserDialog(browserSolver);
                }
            });
            popup.add(selectBrowser);
        }
        boolean externalSelected = false;
        if (selection != null) {
            for (final SolverService selected : selection) {
                if (selected instanceof PluginForCaptchaSolverSolverService) {
                    externalSelected = true;
                    break;
                }
            }
        }
        if (externalSelected) {
            /* Shown if at least one external solver is selected, it always refers to the solver that was right clicked (only if external). */
            final JMenuItem addAccount = new JMenuItem(_GUI.T.lit_add_account(), new AbstractIcon(IconKey.ICON_ADD, 18));
            addAccount.setEnabled(contextObject instanceof PluginForCaptchaSolverSolverService);
            addAccount.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    ((PluginForCaptchaSolverSolverService) contextObject).addAccount();
                }
            });
            popup.add(addAccount);
        }
        if (selection != null && !selection.isEmpty()) {
            /* Enable/disable all selected solvers at once, same behavior as in the download list. */
            popup.add(new ToggleEnabledSelectionAction<SolverService>(contextObject, selection) {
                private static final long serialVersionUID = 1L;

                @Override
                protected boolean isEntryEnabled(final SolverService entry) {
                    return entry.getConfigV3().isEnabled();
                }

                @Override
                protected void setEntryEnabled(final SolverService entry, final boolean enabled) {
                    entry.getConfigV3().setEnabled(enabled);
                }

                @Override
                protected void onFinished() {
                    SolverOrderTable.this.repaint();
                }
            });
        }
        if (contextObject != null && CaptchaConfigPanel.isDebugTabShown()) {
            /*
             * Only shown if the "Test & Debug" tab exists, and only for solvers that can be tested there (ready and enabled). All selected
             * solvers are preselected there (only the clicked one if it is not part of the selection).
             */
            final List<SolverService> testable = new ArrayList<SolverService>();
            final List<SolverService> targets = selection != null && selection.contains(contextObject) ? selection : Collections.singletonList(contextObject);
            for (final SolverService target : targets) {
                if (CaptchaTestPanel.isSelectable(target)) {
                    testable.add(target);
                }
            }
            final JMenuItem testAndDebug = new JMenuItem("Test & Debug", new AbstractIcon(IconKey.ICON_BUG, 18));
            testAndDebug.setEnabled(!testable.isEmpty());
            testAndDebug.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    CaptchaConfigPanel.showTestAndDebug(testable, null);
                }
            });
            popup.add(testAndDebug);
        }
        if (popup.getComponentCount() == 0) {
            /* No context menu actions at all. */
            return null;
        }
        return popup;
    }

    @Override
    protected boolean isDisplayFullsizeAlways() {
        return true;
    }
}
