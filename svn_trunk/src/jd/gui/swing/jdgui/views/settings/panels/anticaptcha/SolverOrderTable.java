package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.List;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.jdownloader.api.captcha.CaptchaAPIManualRemoteSolverService;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.captcha.v2.solver.service.BrowserSolverService;
import org.jdownloader.controlling.browser.ExternalBrowserManager.InstalledBrowser;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.components.captchasolver.PluginForCaptchaSolverSolverService;

import jd.gui.swing.jdgui.BasicJDTable;

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
        setFocusable(false);
        getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(ListSelectionEvent e) {
                if (e.getValueIsAdjusting()) {
                    return;
                }
                if (selectionListener != null) {
                    int row = getSelectedRow();
                    SolverService solver = row >= 0 ? getModel().getObjectbyRow(row) : null;
                    selectionListener.onSolverSelected(solver);
                }
            }
        });
    }

    @Override
    protected boolean onDoubleClick(MouseEvent e, SolverService obj) {
        if (obj instanceof BrowserSolverService) {
            showSelectBrowserDialog((BrowserSolverService) obj);
            return true;
        }
        /* Config is shown inline below the table now; no separate properties dialog. */
        return false;
    }

    /** Lets the user choose the browser of the browser solver and stores the choice. */
    public void showSelectBrowserDialog(final BrowserSolverService browserSolver) {
        final SelectBrowserDialog dialog = new SelectBrowserDialog(browserSolver.getConfig().getBrowserCommandline());
        try {
            final InstalledBrowser selected = Dialog.getInstance().showDialog(dialog);
            if (selected == null) {
                return;
            }
            if (selected.getPath() == null) {
                /* "OS Default" */
                browserSolver.getConfig().setBrowserCommandline(null);
            } else {
                browserSolver.getConfig().setBrowserCommandline(new String[] { selected.getPath(), "%s" });
            }
            /* Check the (possibly new) browser path again on next access. */
            browserSolver.resetBrowserPathCache();
            /* Status text and ready state are computed live, so a redraw of the solver's row is enough. */
            final int row = getModel().getRowforObject(browserSolver);
            if (row >= 0) {
                getModel().fireTableRowsUpdated(row, row);
            }
        } catch (DialogClosedException e1) {
        } catch (DialogCanceledException e1) {
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
     * All other solver types (local JAC, dialog) get no context menu actions.
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
            return popup;
        } else if (contextObject instanceof PluginForCaptchaSolverSolverService) {
            final PluginForCaptchaSolverSolverService external = (PluginForCaptchaSolverSolverService) contextObject;
            /* Account manager icon in front of the text (same icon the Account Manager settings panel uses). */
            final JMenuItem openAccountManager = new JMenuItem(_GUI.T.SolverOrderTable_context_openAccountManager(), new AbstractIcon(IconKey.ICON_PREMIUM, 18));
            openAccountManager.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    external.openAccountManagerSelectingFirstMatchingAccount();
                }
            });
            popup.add(openAccountManager);
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
            return popup;
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
            return popup;
        }
        /* No context menu actions for any other solver type. */
        return null;
    }

    @Override
    public Dimension getPreferredScrollableViewportSize() {
        Dimension dim = super.getPreferredScrollableViewportSize();
        // here we return the pref height
        dim.height = getPreferredSize().height;
        return dim;
    }
}
