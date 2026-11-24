package jd.gui.swing.jdgui.views.settings.panels.accountmanager;

import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;

import org.appwork.swing.components.searchcombo.SearchComboBox;
import org.appwork.uio.UIOManager;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.images.Interpolation;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.dialog.ComboBoxDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.jdownloader.DomainInfo;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.sponsor.AdDisclosureHelper;
import org.jdownloader.gui.sponsor.Sponsor;
import org.jdownloader.gui.sponsor.SponsorUtils;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;
import org.jdownloader.plugins.controller.host.HostPluginController;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;
import org.jdownloader.plugins.controller.host.LazyHostPluginFilter;
import org.jdownloader.premium.BuyAndAddPremiumAccount;
import org.jdownloader.premium.BuyAndAddPremiumDialogInterface;

import jd.controlling.TaskQueue;
import jd.plugins.PluginForHost;

public class BuyAction extends AbstractAction {
    /**
     *
     */
    private static final long   serialVersionUID = 1L;
    private PremiumAccountTable table            = null;
    private boolean             context          = false;
    /*
     * The hoster that is preselected (null: the hoster of the selected account or the default) and where the link is opened from (null: the
     * account manager).
     */
    private LazyHostPlugin      preselected      = null;
    private String              source           = null;

    public BuyAction(PremiumAccountTable table) {
        this.table = table;
        this.putValue(NAME, _GUI.T.settings_accountmanager_buy());
        this.putValue(AbstractAction.SMALL_ICON, new AbstractIcon(IconKey.ICON_BUY, 20));
    }

    /** True if the given plugin is a captcha solver (its accounts are captcha credits). */
    private boolean isCaptchaSolver(final LazyHostPlugin plugin) {
        return plugin != null && plugin.hasFeature(FEATURE.CAPTCHA_SOLVER);
    }

    /**
     * The Buy action of the context menu of the table: Like the Buy button below the table (the hoster of the selected account is
     * preselected), but the link is marked as opened from the context menu and it is named Buy credits if the selected account is the
     * account of a captcha solver.
     */
    public BuyAction(PremiumAccountTable table, boolean context) {
        this.table = table;
        this.context = context;
        final List<AccountEntry> selection = table.getModel().getSelectedObjects();
        final PluginForHost selectedPlugin = selection != null && selection.size() > 0 ? selection.get(0).getAccount().getPlugin() : null;
        if (selectedPlugin != null && isCaptchaSolver(selectedPlugin.getLazyP())) {
            this.putValue(NAME, _GUI.T.SolverOrderTable_context_buyCredits());
        } else {
            this.putValue(NAME, _GUI.T.settings_accountmanager_buy());
        }
        this.putValue(AbstractAction.SMALL_ICON, new AbstractIcon(IconKey.ICON_BUY, 16));
    }

    public BuyAction() {
        this((LazyHostPlugin) null);
    }

    public BuyAction(LazyHostPlugin hoster) {
        this(hoster, null);
    }

    /**
     * The dialog of the Buy button with the given hoster preselected.
     *
     * @param source
     *            Identifies where the link was opened from (part of the redirect link), null for the account manager
     */
    public BuyAction(LazyHostPlugin hoster, String source) {
        this.preselected = hoster;
        this.source = source;
        if (isCaptchaSolver(hoster)) {
            this.putValue(NAME, _GUI.T.SolverOrderTable_context_buyCredits());
        } else {
            this.putValue(NAME, _GUI.T.settings_accountmanager_buy());
        }
        this.putValue(AbstractAction.SMALL_ICON, new AbstractIcon(IconKey.ICON_BUY, 16));
    }

    public static String getPreselectedHoster() {
        final Sponsor sp = SponsorUtils.getSponsor();
        final String host = sp.getPreSelectedInAddAccountDialog();
        if (host != null) {
            return host;
        } else {
            return "rapidgator.net";
        }
    }

    public void actionPerformed(ActionEvent e) {
        final List<AccountEntry> selection;
        if (table != null) {
            selection = table.getModel().getSelectedObjects();
        } else {
            selection = null;
        }
        TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
            @Override
            protected Void run() throws RuntimeException {
                LazyHostPlugin plg = preselected;
                plg: if (plg == null && selection != null && selection.size() > 0) {
                    /* If an entry is selected in the table, preselect its hoster. */
                    final PluginForHost plugin = selection.get(0).getAccount().getPlugin();
                    if (plugin != null) {
                        plg = plugin.getLazyP();
                        break plg;
                    }
                }
                if (plg == null) {
                    /* No selection (or selected hoster not found) -> fall back to the default preselection. */
                    plg = HostPluginController.getInstance().get(getPreselectedHoster());
                }
                /*
                 * If the preselected hoster is a captcha solver (the dialog was opened for a solver, or an account of a solver is selected
                 * in the account manager), the dialog is about captcha credits and only lists captcha solvers.
                 */
                final boolean solversOnly = isCaptchaSolver(plg);
                final LazyHostPluginFilter filter = new LazyHostPluginFilter().setPremium(Boolean.TRUE);
                if (solversOnly) {
                    filter.setFeatures(FEATURE.CAPTCHA_SOLVER);
                }
                final List<LazyHostPlugin> plugins = HostPluginController.getInstance().list(filter);
                final LazyHostPlugin[] options = plugins.toArray(new LazyHostPlugin[plugins.size()]);
                final LazyHostPlugin defaultSelection = plg;
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        try {
                            final ComboBoxDialog d = new ComboBoxDialog(0, solversOnly ? _GUI.T.buyaction_captcha_title() : _GUI.T.buyaction_title(), solversOnly ? _GUI.T.buyaction_captcha_message() : _GUI.T.buyaction_message(), options, 0, new AbstractIcon(IconKey.ICON_BUY, 32), _GUI.T.buyaction_title_buy_account(), null, null) {
                                private SearchComboBox<LazyHostPlugin> combo;

                                @Override
                                protected void packed() {
                                    super.packed();
                                    combo.requestFocus();
                                }

                                @Override
                                protected void addButtons(JPanel buttonBar) {
                                    /* The ad disclosure is intentionally always shown here. */
                                    final JLabel adDisclosureText = AdDisclosureHelper.createAdDisclosureLabel(null, SwingConstants.BOTTOM);
                                    buttonBar.add(adDisclosureText, "aligny bottom, gapbottom 2", 0);
                                };

                                @Override
                                protected void initFocus(final javax.swing.JComponent focus) {
                                    // focus the search field instead of the default OK button. initFocus runs on
                                    // windowGainedFocus (after the dialog is visible), so requestFocusInWindow sticks here.
                                    if (combo != null) {
                                        combo.getTextField().requestFocusInWindow();
                                    } else {
                                        super.initFocus(focus);
                                    }
                                }

                                protected String getIconConstraints() {
                                    return "gapright 10,gaptop 2,width 32!,height 32!,alignx center, aligny center";
                                }

                                @Override
                                protected JComboBox getComboBox(final Object[] options2) {
                                    combo = new SearchComboBox<LazyHostPlugin>(plugins) {
                                        /**
                                         *
                                         */
                                        private static final long serialVersionUID = -7421876925835937449L;

                                        @Override
                                        protected Icon getIconForValue(LazyHostPlugin value) {
                                            if (value == null) {
                                                return null;
                                            } else {
                                                return value.getDomainInfo().getFavIcon(false);
                                            }
                                        }

                                        @Override
                                        protected String getTextForValue(LazyHostPlugin value) {
                                            if (value == null) {
                                                return null;
                                            } else {
                                                return value.getDisplayName();
                                            }
                                        }
                                    };
                                    final ComboBoxDialog _this = this;
                                    combo.addActionListener(new ActionListener() {
                                        public void actionPerformed(ActionEvent e) {
                                            Object item = combo.getSelectedItem();
                                            if (item == null) {
                                                _this.setIcon(null);
                                                return;
                                            }
                                            DomainInfo domainInfo = ((LazyHostPlugin) item).getDomainInfo();
                                            String tld = domainInfo.getTld();
                                            Image ic = null;
                                            if (NewTheme.I().hasIcon("fav/big." + tld)) {
                                                ic = NewTheme.I().getImage("fav/big." + tld, -1);
                                            }
                                            if (ic == null && NewTheme.I().hasIcon("fav/" + tld)) {
                                                ic = NewTheme.I().getImage("fav/" + tld, -1);
                                            }
                                            if (ic != null) {
                                                _this.setIcon(new ImageIcon(IconIO.getScaledInstance(ic, Math.min(ic.getWidth(null), 32), Math.min(ic.getHeight(null), 32), Interpolation.BILINEAR, true)));
                                                return;
                                            } else {
                                                _this.setIcon(domainInfo.getFavIcon(false));
                                            }
                                        }
                                    });
                                    combo.setSelectedItem(defaultSelection);
                                    return combo;
                                }
                            };
                            Dialog.getInstance().showDialog(d);
                            if (d.getReturnValue() < 0) {
                                return;
                            }
                            final LazyHostPlugin lazyHostPlugin = options[d.getReturnValue()];
                            if (lazyHostPlugin != null) {
                                /*
                                 * The afflink is not opened here: The BuyAndAddPremiumAccount dialog has its own button for it (next to the
                                 * ad disclosure), opening it here as well would open the link twice.
                                 */
                                final BuyAndAddPremiumAccount dia = new BuyAndAddPremiumAccount(lazyHostPlugin.getDomainInfo(), source != null ? source : "accountmanager" + (table == null || context ? "/context" : "/table"));
                                UIOManager.I().show(BuyAndAddPremiumDialogInterface.class, dia);
                                dia.throwCloseExceptions();
                            }
                        } catch (DialogNoAnswerException e1) {
                            e1.printStackTrace();
                        }
                    }
                };
                return null;
            }
        });
    }
}
