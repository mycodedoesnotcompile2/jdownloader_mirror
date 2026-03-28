package jd.gui.swing.jdgui.views.settings.panels.accountmanager;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.text.DecimalFormat;
import java.text.FieldPosition;
import java.util.ArrayList;
import java.util.Currency;
import java.util.Date;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;

import jd.SecondLevelLaunch;
import jd.controlling.AccountController;
import jd.controlling.AccountControllerEvent;
import jd.controlling.AccountControllerListener;
import jd.controlling.accountchecker.AccountChecker;
import jd.controlling.accountchecker.AccountCheckerEventListener;
import jd.gui.swing.jdgui.interfaces.SwitchPanelEvent;
import jd.gui.swing.jdgui.interfaces.SwitchPanelListener;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.AccountTrafficView;
import jd.plugins.PluginForHost;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.storage.config.JsonConfig;
import org.appwork.swing.components.ExtMergedIcon;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableHeaderRenderer;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtComponentColumn;
import org.appwork.swing.exttable.columns.ExtCurrencyColumn;
import org.appwork.swing.exttable.columns.ExtDateColumn;
import org.appwork.swing.exttable.columns.ExtPasswordEditorColumn;
import org.appwork.swing.exttable.columns.ExtProgressColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.DomainInfo;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.components.ColumnButton;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.controller.LazyPlugin.FEATURE;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

public class PremiumAccountTableModel extends ExtTableModel<AccountEntry> implements AccountCheckerEventListener {
    protected class TrafficColumn extends ExtProgressColumn<AccountEntry> {
        private static final long   serialVersionUID = -8376056840172682617L;
        private final DecimalFormat formatter;
        private final SIZEUNIT      maxSizeUnit;

        public TrafficColumn(PremiumAccountTableModel tableModel, String title) {
            super(title);
            maxSizeUnit = JsonConfig.create(GraphicalUserInterfaceSettings.class).getMaxSizeUnit();
            this.formatter = new DecimalFormat() {
                final StringBuffer        sb               = new StringBuffer();
                /**
                 *
                 */
                private static final long serialVersionUID = 1L;

                @Override
                public StringBuffer format(final double number, final StringBuffer result, final FieldPosition pos) {
                    sb.setLength(0);
                    return super.format(number, sb, pos);
                }
            };

            replaceSorter(this);

        }

        @Override
        public boolean isEnabled(AccountEntry obj) {
            return obj.getAccount().isEnabled();
        }

        @Override
        public boolean isSortable(AccountEntry obj) {
            return PremiumAccountTableModel.this.isSortable();
        }

        @Override
        public int getMinWidth() {
            return 120;
        }

        protected boolean isIndeterminated(final AccountEntry value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
            if (PremiumAccountTableModel.this.checkRunning) {
                return AccountChecker.getInstance().contains(value.getAccount());
            }
            if (value.getAccount().isValid() && value.getAccount().isEnabled() && value.getAccount().isTempDisabled()) {
                return true;
            }
            return false;
        }

        @Override
        protected String getString(AccountEntry ac, long current, long total) {
            final Account acc = ac.getAccount();
            final PluginForHost plg = acc.getPlugin();
            if (!acc.isValid()) {
                return "";
            }
            final long tmpDisabledTimeout = acc.getTmpDisabledTimeout();
            final long timeout = tmpDisabledTimeout - System.currentTimeMillis();
            if (acc.isEnabled() && acc.isTempDisabled() && timeout > 0) {
                return _GUI.T.premiumaccounttablemodel_column_trafficleft_tempdisabled(TimeFormatter.formatMilliSeconds(timeout, 0));
            }
            if (plg != null && plg.hasFeature(FEATURE.CAPTCHA_SOLVER)) {
                /* Captcha solver accounts have no download traffic */
                String balanceStr = "0";
                final AccountInfo ai = acc.getAccountInfo();
                if (ai != null) {
                    balanceStr = ai.getAccountBalanceFormatted();
                }
                return "Balance: " + balanceStr;
            }
            final AccountTrafficView accountTrafficView = acc.getAccountTrafficView();
            if (accountTrafficView == null) {
                return "";
            }
            if (accountTrafficView.isUnlimitedTraffic()) {
                return _GUI.T.premiumaccounttablemodel_column_trafficleft_unlimited();
            }
            synchronized (formatter) {
                return _GUI.T.premiumaccounttablemodel_column_trafficleft_left_(SIZEUNIT.formatValue(maxSizeUnit, formatter, accountTrafficView.getTrafficLeft()), SIZEUNIT.formatValue(maxSizeUnit, formatter, accountTrafficView.getTrafficMax()));
            }
        }

        @Override
        protected long getMax(AccountEntry ac) {
            final Account acc = ac.getAccount();
            final PluginForHost plg = acc.getPlugin();
            if (!acc.isValid()) {
                return 0;
            }
            if (plg != null && plg.hasFeature(FEATURE.CAPTCHA_SOLVER)) {
                /* Captcha solver accounts have no download traffic */
                return Long.MAX_VALUE;
            }
            final AccountTrafficView accountTrafficView = acc.getAccountTrafficView();
            if (accountTrafficView == null) {
                return 0;
            }
            if (accountTrafficView.isUnlimitedTraffic()) {
                return Long.MAX_VALUE;
            }
            return accountTrafficView.getTrafficMax();
        }

        @Override
        protected long getValue(AccountEntry ac) {
            final Account acc = ac.getAccount();
            final PluginForHost plg = acc.getPlugin();
            if (!acc.isValid()) {
                return 0;
            }
            if (plg != null && plg.hasFeature(FEATURE.CAPTCHA_SOLVER)) {
                /* Captcha solver accounts have no download traffic */
                return Long.MAX_VALUE;
            }
            final AccountTrafficView accountTrafficView = acc.getAccountTrafficView();
            if (accountTrafficView == null) {
                return 0;
            }
            if (accountTrafficView.isUnlimitedTraffic()) {
                return Long.MAX_VALUE;
            }
            return accountTrafficView.getTrafficLeft();
        }
    }

    protected class BalanceColumn extends ExtCurrencyColumn<AccountEntry> {
        private static final long serialVersionUID = 1L;

        public BalanceColumn(final PremiumAccountTableModel tableModel, final String title) {
            super(title, tableModel);
            replaceSorter(this);
        }

        @Override
        protected Currency getCurrency(final AccountEntry value) {
            final AccountInfo ai = value.getAccount().getAccountInfo();
            if (ai != null) {
                return ai.getCurrency();
            }
            return null;
        }

        @Override
        protected long getValue(final Object o) {
            final AccountEntry entry = (AccountEntry) o;
            final AccountInfo ai = entry.getAccount().getAccountInfo();
            if (ai == null) {
                return 0L;
            }
            return Math.round(ai.getAccountBalance() * 100);
        }

        @Override
        public boolean isEnabled(final AccountEntry obj) {
            return obj.getAccount().isEnabled();
        }

        @Override
        public boolean isSortable(final AccountEntry obj) {
            return PremiumAccountTableModel.this.isSortable();
        }

        @Override
        public void configureEditorComponent(AccountEntry value, boolean isSelected, int row, int column) {
        }
    }

    protected class ExpireColumn extends ExtDateColumn<AccountEntry> {
        private static final long serialVersionUID = 5067606909520874358L;

        public ExpireColumn(PremiumAccountTableModel model, String string) {
            super(string);
            replaceSorter(this);
        }

        @Override
        public boolean isSortable(AccountEntry obj) {
            return PremiumAccountTableModel.this.isSortable();
        }

        @Override
        public boolean isEnabled(AccountEntry obj) {
            return obj.getAccount().isEnabled();
        }

        @Override
        public boolean isAutoWidthEnabled() {
            return true;
        }

        @Override
        protected boolean isDefaultResizable() {
            return true;
        }

        @Override
        public int getDefaultWidth() {
            return 120;
        }

        @Override
        public int getMinWidth() {
            return 50;
        }

        @Override
        protected String getDateFormatString() {
            return Account.getExpireDateFormatString(this);
        }

        @Override
        protected Date getDate(AccountEntry o2, Date date) {
            final AccountInfo ai = o2.getAccount().getAccountInfo();
            if (ai == null) {
                return null;
            }
            if (ai.getValidUntil() <= 0) {
                return null;
            } else {
                return new Date(ai.getValidUntil());
            }
        }
    }

    private static final long serialVersionUID       = 3120481189794897020L;
    private AccountListPanel  accountManagerSettings = null;
    private DelayedRunnable   delayedFill;
    private volatile boolean  checkRunning           = false;
    private DelayedRunnable   delayedUpdate;

    public PremiumAccountTableModel(final AccountListPanel accountListPanel) {
        super("PremiumAccountTableModel2");
        this.accountManagerSettings = accountListPanel;
        ScheduledExecutorService scheduler = DelayedRunnable.getNewScheduledExecutorService();
        delayedFill = new DelayedRunnable(scheduler, 250l) {
            @Override
            public String getID() {
                return "PremiumAccountTableFill";
            }

            @Override
            public void delayedrun() {
                System.out.println("Refill");
                _refill();
            }
        };
        delayedUpdate = new DelayedRunnable(scheduler, 250l) {
            @Override
            public String getID() {
                return "PremiumAccountTableUpdate";
            }

            @Override
            public void delayedrun() {
                _update();
            }
        };
        initListeners(accountListPanel);
    }

    protected void initListeners(final AccountListPanel accountListPanel) {
        SecondLevelLaunch.ACCOUNTLIST_LOADED.executeWhenReached(new Runnable() {
            @Override
            public void run() {
                AccountController.getInstance().getEventSender().addListener(new AccountControllerListener() {
                    public void onAccountControllerEvent(AccountControllerEvent event) {
                        if (accountListPanel.isShown()) {
                            switch (event.getType()) {
                            case ACCOUNT_CHECKED:
                            case ACCOUNT_PROPERTY_UPDATE:
                                /* just repaint */
                                delayedUpdate.run();
                                break;
                            default:
                                /* structure changed */
                                delayedFill.run();
                            }
                        }
                    }
                });
                AccountChecker.getInstance().getEventSender().addListener(PremiumAccountTableModel.this);
                accountManagerSettings.getBroadcaster().addListener(new SwitchPanelListener() {
                    @Override
                    public void onPanelEvent(SwitchPanelEvent event) {
                        if (event.getEventID() == SwitchPanelEvent.ON_SHOW) {
                            _refill();
                        }
                    }
                });
                if (AccountChecker.getInstance().isRunning()) {
                    onCheckStarted();
                }
                _refill();
            }
        });
    }

    public void fill() {
        delayedFill.run();
    }

    @Override
    protected void initColumns() {
        addEnabledColumn();
        addColumnHoster();
        addStatusColumn();
        addUsernameColumn();
        addPasswordColumn();
        addExpireColumn();
        addTrafficColumn();
        addBalanceColumn();
        addColumnSettingsButton();
    }

    protected void addTrafficColumn() {
        this.addColumn(new TrafficColumn(this, _GUI.T.premiumaccounttablemodel_column_trafficleft()));
    }

    protected void addBalanceColumn() {
        this.addColumn(new BalanceColumn(this, _GUI.T.premiumaccounttablemodel_column_balance()) {
            @Override
            public boolean isDefaultVisible() {
                /* Default invisible since balance is not important for most use cases. */
                return false;
            }
        });
    }

    protected void addExpireColumn() {
        this.addColumn(new ExpireColumn(this, _GUI.T.premiumaccounttablemodel_column_expiredate()));
    }

    protected void addUsernameColumn() {
        this.addColumn(new ExtTextColumn<AccountEntry>(_GUI.T.premiumaccounttablemodel_column_user()) {
            {
                replaceSorter(this);
            }
            private static final long serialVersionUID = -8070328156326837828L;

            @Override
            public boolean isSortable(AccountEntry obj) {
                return PremiumAccountTableModel.this.isSortable();
            }

            @Override
            public boolean isHidable() {
                return false;
            }

            @Override
            public boolean isAutoWidthEnabled() {
                return true;
            }

            @Override
            protected boolean isDefaultResizable() {
                return true;
            }

            @Override
            public int getDefaultWidth() {
                return getMinWidth() + 10;
            }

            @Override
            public int getMinWidth() {
                return 70;
            }

            @Override
            public boolean isEnabled(AccountEntry obj) {
                return obj.getAccount().isEnabled() || isEditable(obj);
            }

            @Override
            public boolean isEditable(AccountEntry obj) {
                /* prevent username value from been edited in presentation mode */
                if (CFG_GUI.CFG.isPresentationModeEnabled()) {
                    return false;
                }
                final Account acc = obj.getAccount();
                final PluginForHost plg = acc.getPlugin();
                if (plg != null && plg.hasFeature(FEATURE.API_KEY_LOGIN)) {
                    /*
                     * Login happens via API key -> There is either no username available or username shall not be edited by user since it
                     * is set during account-check.
                     */
                    return false;
                } else {
                    return PremiumAccountTableModel.this.isEditable();
                }
            }

            @Override
            protected void setStringValue(String value, AccountEntry object) {
                object.getAccount().setUser(value);
            }

            @Override
            public String getStringValue(AccountEntry value) {
                if (CFG_GUI.CFG.isPresentationModeEnabled()) {
                    return CFG_GUI.CFG.getPresentationModeText();
                }
                return value.getAccount().getUser();
            }
        });
    }

    protected void addStatusColumn() {
        this.addColumn(new ExtTextColumn<AccountEntry>(_GUI.T.premiumaccounttablemodel_column_status()) {
            private static final long serialVersionUID = -3693931358975303164L;
            {
                replaceSorter(this);
            }

            @Override
            public boolean isEnabled(AccountEntry obj) {
                return obj.getAccount().isEnabled();
            }

            @Override
            public boolean isSortable(AccountEntry obj) {
                return PremiumAccountTableModel.this.isSortable();
            }

            @Override
            public boolean isHidable() {
                return true;
            }

            @Override
            protected Icon getIcon(AccountEntry value) {
                return accountToStatusIcon(value.getAccount());
            }

            @Override
            public int getDefaultWidth() {
                return 160;
            }

            @Override
            public int getMinWidth() {
                return 24;
            }

            // @Override
            // protected String getTooltipText(AccountEntry obj) {
            // return obj.getAccount().getHoster();
            // }
            @Override
            public String getStringValue(AccountEntry value) {
                return accountToStatusString(value.getAccount());
            }
        });
    }

    protected void addPasswordColumn() {
        this.addColumn(new ExtPasswordEditorColumn<AccountEntry>(_GUI.T.premiumaccounttablemodel_column_password()) {
            {
                replaceSorter(this);
            }
            private static final long serialVersionUID = 3180414754658474808L;

            @Override
            public boolean isHidable() {
                return true;
            }

            @Override
            public boolean isSortable(AccountEntry obj) {
                return PremiumAccountTableModel.this.isSortable();
            }

            @Override
            public int getMaxWidth() {
                return 140;
            }

            @Override
            public int getDefaultWidth() {
                return 110;
            }

            @Override
            public int getMinWidth() {
                return 70;
            }

            @Override
            protected String getPlainStringValue(AccountEntry value) {
                return value.getAccount().getPass();
            }

            @Override
            protected void setStringValue(String value, AccountEntry object) {
                object.getAccount().setPass(value);
            }
        });
    }

    protected boolean isEditable() {
        return true;
    }

    protected void addEnabledColumn() {
        this.addColumn(new ExtCheckColumn<AccountEntry>(_GUI.T.premiumaccounttablemodel_column_enabled()) {
            private static final long serialVersionUID = 1515656228974789237L;

            public ExtTableHeaderRenderer getHeaderRenderer(final JTableHeader jTableHeader) {
                final ExtTableHeaderRenderer ret = new ExtTableHeaderRenderer(this, jTableHeader) {
                    private final Icon        ok               = NewTheme.I().getIcon(IconKey.ICON_OK, 14);
                    private static final long serialVersionUID = 3224931991570756349L;

                    @Override
                    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
                        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                        setIcon(ok);
                        setHorizontalAlignment(CENTER);
                        setText(null);
                        return this;
                    }
                };
                return ret;
            }

            @Override
            public int getMaxWidth() {
                return 30;
            }

            @Override
            protected boolean getBooleanValue(AccountEntry value) {
                return value.getAccount().isEnabled();
            }

            @Override
            public boolean isSortable(AccountEntry obj) {
                return PremiumAccountTableModel.this.isSortable();
            }

            @Override
            public boolean isEditable(AccountEntry obj) {
                return true;
            }

            @Override
            protected void setBooleanValue(boolean enabled, final AccountEntry object) {
                object.getAccount().setEnabled(enabled);
                if (enabled && object.getAccount().isMultiHost()) {
                    RefreshAction.displayMultihosterDetailOverviewHelpDialog();
                }
            }
        });
    }

    protected void addColumnSettingsButton() {
        this.addColumn(new ExtComponentColumn<AccountEntry>(_GUI.T.lit_settings()) {
            private ColumnButton button;
            private ColumnButton rbutton;
            private AccountEntry editing;
            {
                button = new ColumnButton(new AbstractIcon(IconKey.ICON_SETTINGS, 16));
                rbutton = new ColumnButton(new AbstractIcon(IconKey.ICON_SETTINGS, 16));
                rbutton.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        if (editing != null) {
                            editing.showConfiguration();
                        }
                    }
                });
            }

            public ExtTableHeaderRenderer getHeaderRenderer(final JTableHeader jTableHeader) {
                final ExtTableHeaderRenderer ret = new ExtTableHeaderRenderer(this, jTableHeader) {
                    @Override
                    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
                        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                        if (isSettingsColumnHeaderIconVisible()) {
                            setIcon(new AbstractIcon(IconKey.ICON_SETTINGS, 16));
                        }
                        setHorizontalAlignment(CENTER);
                        setText(null);
                        return this;
                    }
                };
                return ret;
            }

            @Override
            public boolean isAutoWidthEnabled() {
                return true;
            }

            @Override
            protected boolean isDefaultResizable() {
                return true;
            }

            @Override
            public boolean isEnabled(AccountEntry obj) {
                return true;
            }

            @Override
            public boolean isHidable() {
                return false;
            }

            @Override
            public boolean isEditable(AccountEntry obj) {
                return super.isEditable(obj);
            }

            @Override
            public boolean onSingleClick(MouseEvent e, AccountEntry obj) {
                return super.onSingleClick(e, obj);
            }

            @Override
            protected JComponent getInternalEditorComponent(AccountEntry value, boolean isSelected, int row, int column) {
                return rbutton;
            }

            @Override
            protected JComponent getInternalRendererComponent(AccountEntry value, boolean isSelected, boolean hasFocus, int row, int column) {
                return button;
            }

            @Override
            public void configureEditorComponent(AccountEntry value, boolean isSelected, int row, int column) {
                editing = value;
                // rbutton.setEnabled(isEnabled(value));
            }

            @Override
            public void configureRendererComponent(AccountEntry value, boolean isSelected, boolean hasFocus, int row, int column) {
                // button.setEnabled(isEnabled(value));
                ;
            }

            @Override
            public void resetEditor() {
                // rbutton.setBackground(null);
                // rbutton.setOpaque(false);
            }

            @Override
            public void resetRenderer() {
                // button.setBackground(null);
                // button.setOpaque(false);
            }
        });
    }

    protected boolean isSettingsColumnHeaderIconVisible() {
        return true;
    }

    protected void addColumnHoster() {
        this.addColumn(new ExtTextColumn<AccountEntry>(_GUI.T.premiumaccounttablemodel_column_hoster()) {
            {
                replaceSorter(this);
            }
            private static final long serialVersionUID = -3693931358975303164L;

            @Override
            public boolean isEnabled(AccountEntry obj) {
                return obj.getAccount().isEnabled();
            }

            @Override
            public boolean isSortable(AccountEntry obj) {
                return PremiumAccountTableModel.this.isSortable();
            }

            @Override
            protected Icon getIcon(AccountEntry value) {
                final PluginForHost plugin = value.getAccount().getPlugin();
                final DomainInfo domainInfo;
                if (plugin != null) {
                    domainInfo = DomainInfo.getInstance(plugin.getHost(null, value.getAccount(), false));
                } else {
                    domainInfo = DomainInfo.getInstance(value.getAccount().getHoster());
                }
                if (domainInfo != null) {
                    return domainInfo.getFavIcon();
                } else {
                    return null;
                }
            }

            @Override
            public int getDefaultWidth() {
                return 120;
            }

            @Override
            public int getMinWidth() {
                return 30;
            }

            @Override
            protected String getTooltipText(AccountEntry obj) {
                return obj.getAccount().getHoster();
            }

            @Override
            public void configureRendererComponent(AccountEntry value, boolean isSelected, boolean hasFocus, int row, int column) {
                this.rendererIcon.setIcon(this.getIcon(value));
                String str = null;
                if (getWidth() > 60) {
                    str = this.getStringValue(value);
                }
                if (str == null) {
                    str = "";
                }
                if (this.getTableColumn() != null) {
                    this.rendererField.setText(org.appwork.sunwrapper.sun.swing.SwingUtilities2Wrapper.clipStringIfNecessary(this.rendererField, this.rendererField.getFontMetrics(this.rendererField.getFont()), str, this.getTableColumn().getWidth() - this.rendererIcon.getPreferredSize().width - 5));
                } else {
                    this.rendererField.setText(str);
                }
            }

            @Override
            public String getStringValue(AccountEntry value) {
                return value.getAccount().getHosterByPlugin();
            }
        });
    }

    protected boolean isSortable() {
        return true;
    }

    public static void replaceSorter(ExtColumn<AccountEntry> column) {
        if (column != null) {
            final ExtDefaultRowSorter<AccountEntry> oldSorter = column.getRowSorter();
            column.setRowSorter(new ExtDefaultRowSorter<AccountEntry>() {
                public int compare(boolean x, boolean y) {
                    return (x == y) ? 0 : (x ? -1 : 1);
                }

                @Override
                public int compare(final AccountEntry o1, final AccountEntry o2) {
                    final boolean b1 = o1.getAccount().isEnabled();
                    final boolean b2 = o2.getAccount().isEnabled();
                    if (b1 == b2) {
                        if (getSortOrderIdentifier() != ExtColumn.SORT_ASC) {
                            return oldSorter.compare(o1, o2);
                        } else {
                            return -oldSorter.compare(o1, o2);
                        }
                    }
                    return compare(b1, b2);
                }
            });
        }
    }

    public void onCheckStarted() {
        checkRunning = true;
    }

    public void onCheckStopped() {
        checkRunning = false;
        _update();
    }

    protected void _update() {
        if (accountManagerSettings.isShown()) {
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    PremiumAccountTableModel.this.getTable().repaint();
                }
            };
        }
    }

    protected void _refill() {
        if (!accountManagerSettings.isShown()) {
            return;
        }
        final List<AccountEntry> newtableData = new ArrayList<AccountEntry>(this.getRowCount());
        final List<Account> accs = AccountController.getInstance().list(null);
        if (accs != null) {
            for (Account acc : accs) {
                if (acc.getPlugin() == null) {
                    continue;
                }
                newtableData.add(new AccountEntry(acc));
            }
        }
        _fireTableStructureChanged(newtableData, true);
    }

    public static String accountToStatusString(Account value) {
        if (value.isChecking()) {
            return _GUI.T.PremiumAccountTableModel_refresh();
        }
        final String errString = value.getErrorString();
        if (value.getError() == null) {
            AccountInfo ai = value.getAccountInfo();
            String ret = ai == null ? null : ai.getStatus();
            if (StringUtils.isEmpty(ret)) {
                if (value.isTempDisabled()) {
                    ret = errString;
                    if (StringUtils.isEmpty(ret)) {
                        ret = _GUI.T.PremiumAccountTableModel_getStringValue_temp_disabled();
                    }
                } else {
                    ret = _GUI.T.PremiumAccountTableModel_getStringValue_account_ok_();
                }
            } else {
                if (value.isTempDisabled()) {
                    if (StringUtils.isNotEmpty(errString)) {
                        return errString;
                    }
                    ret = _GUI.T.PremiumAccountTableModel_getStringValue_temp_disabled2(ret);
                } else {
                    ret = _GUI.T.PremiumAccountTableModel_getStringValue_account_ok_2(ret);
                }
            }
            return ret;
        }
        if (StringUtils.isNotEmpty(errString)) {
            return errString;
        }
        switch (value.getError()) {
        case TEMP_DISABLED:
            return _GUI.T.PremiumAccountTableModel_getStringValue_temp_disabled();
        case EXPIRED:
            return _GUI.T.PremiumAccountTableModel_getStringValue_status_expired();
        case INVALID:
            return _GUI.T.PremiumAccountTableModel_getStringValue_status_invalid();
        case PLUGIN_ERROR:
            return _GUI.T.PremiumAccountTableModel_getStringValue_status_plugin_error();
        default:
            return _GUI.T.PremiumAccountTableModel_getStringValue_status_unknown_error();
        }
    }

    static final Icon REFRESH = new AbstractIcon(IconKey.ICON_REFRESH, 16);
    static final Icon OKAY    = new AbstractIcon(IconKey.ICON_OK, 16);
    static final Icon WAIT    = new AbstractIcon(IconKey.ICON_WAIT, 16);
    static final Icon ERROR   = new AbstractIcon(IconKey.ICON_ERROR, 16);
    static final Icon EXPIRED = new ExtMergedIcon(new AbstractIcon(IconKey.ICON_ERROR, 18)).add(new AbstractIcon(IconKey.ICON_WAIT, 12), 6, 6);

    public static Icon accountToStatusIcon(Account value) {
        if (value.isChecking()) {
            return REFRESH;
        }
        if (value.getError() == null) {
            if (value.isTempDisabled()) {
                return WAIT;
            }
            return OKAY;
        }
        switch (value.getError()) {
        case EXPIRED:
            return EXPIRED;
        case INVALID:
            return ERROR;
        case PLUGIN_ERROR:
            return ERROR;
        case TEMP_DISABLED:
            return WAIT;
        }
        return OKAY;
    }
}
