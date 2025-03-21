package jd.plugins;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JSeparator;
import javax.swing.SwingUtilities;

import org.appwork.storage.config.ConfigInterface;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.BooleanKeyHandler;
import org.appwork.storage.config.handler.EnumKeyHandler;
import org.appwork.storage.config.handler.IntegerKeyHandler;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.storage.config.handler.LongKeyHandler;
import org.appwork.storage.config.handler.ObjectKeyHandler;
import org.appwork.storage.config.handler.StringKeyHandler;
import org.appwork.storage.config.swing.models.ConfigIntSpinnerModel;
import org.appwork.storage.config.swing.models.ConfigLongSpinnerModel;
import org.appwork.swing.components.ExtCheckBox;
import org.appwork.uio.UIOManager;
import org.appwork.utils.ColorUtils;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.DebugMode;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.DefaultEvent;
import org.appwork.utils.event.DontThrowFromCurrentThreadEventSuppressor;
import org.appwork.utils.event.EventSuppressor;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.logging2.extmanager.LoggerFactory;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.jdownloader.DomainInfo;
import org.jdownloader.extensions.Header;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.settings.AbstractConfigPanel;
import org.jdownloader.gui.settings.Pair;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.logging.LogController;
import org.jdownloader.plugins.config.AccountConfigInterface;
import org.jdownloader.plugins.config.AccountJsonConfig;
import org.jdownloader.plugins.config.CustomUI;
import org.jdownloader.plugins.config.Group;
import org.jdownloader.plugins.config.Order;
import org.jdownloader.plugins.config.PluginConfigInterface;
import org.jdownloader.plugins.config.PluginConfigPanelEventSenderEvent;
import org.jdownloader.plugins.config.PluginConfigPanelEventSenderEventSender;
import org.jdownloader.plugins.config.PluginConfigPanelEventSenderListener;
import org.jdownloader.plugins.config.PluginJsonConfig;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.premium.BuyAndAddPremiumAccount;
import org.jdownloader.premium.BuyAndAddPremiumDialogInterface;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;
import org.jdownloader.updatev2.gui.LAFOptions;

import jd.controlling.AccountController;
import jd.controlling.AccountControllerEvent;
import jd.controlling.AccountControllerListener;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.views.settings.ConfigurationView;
import jd.gui.swing.jdgui.views.settings.components.Checkbox;
import jd.gui.swing.jdgui.views.settings.components.ComboBox;
import jd.gui.swing.jdgui.views.settings.components.Label;
import jd.gui.swing.jdgui.views.settings.components.MultiComboBox;
import jd.gui.swing.jdgui.views.settings.components.SettingsComponent;
import jd.gui.swing.jdgui.views.settings.components.Spinner;
import jd.gui.swing.jdgui.views.settings.components.TextInput;
import jd.gui.swing.jdgui.views.settings.components.TextPane;
import jd.gui.swing.jdgui.views.settings.panels.accountmanager.AccountEntry;
import jd.gui.swing.jdgui.views.settings.panels.accountmanager.AccountManagerSettings;
import jd.gui.swing.jdgui.views.settings.panels.accountmanager.PremiumAccountTableModel;
import jd.nutils.Formatter;
import net.miginfocom.swing.MigLayout;

public abstract class PluginConfigPanelNG extends AbstractConfigPanel implements AccountControllerListener {
    private List<Group> groups = new ArrayList<Group>();
    private boolean     seperatorRequired;

    public void addGroup(Group g) {
        this.groups.add(g);
    }

    public void addGroup(String title, String regex) {
        addGroup(new Group(title, regex, null));
    }

    private static final String                     ACCOUNT  = "ACCOUNT";
    private boolean                                 updateAccounts;
    private HashSet<Component>                      accountSettingsComponents;
    private Plugin                                  plugin;
    private Object                                  translation;
    private String                                  gapleft  = "0";
    private String                                  gapright = "";
    private int                                     headerHight;
    private boolean                                 added;
    private PluginConfigPanelEventSenderEventSender eventSender;
    private HashMap<JLabel, Account>                accountMap;

    public synchronized PluginConfigPanelEventSenderEventSender getEventSender() {
        if (eventSender == null) {
            eventSender = new PluginConfigPanelEventSenderEventSender();
        }
        return eventSender;
    }

    public PluginConfigPanelNG() {
        super(0);
    }

    @Override
    public void onAccountControllerEvent(final AccountControllerEvent event) {
        if (plugin == null || !StringUtils.equals(plugin.getClass().getSimpleName(), event.getAccount().getPlugin().getClass().getSimpleName())) {
            return;
        }
        switch (event.getType()) {
        case ACCOUNT_CHECKED:
        case ACCOUNT_PROPERTY_UPDATE:
        case ACCOUNT_UP_OR_DOWNGRADE:
        case ADDED:
        case REMOVED:
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    updateAccountSettings(event.getAccount().getPlugin());
                    final Container parent = getParent();
                    if (parent != null && parent instanceof JComponent) {
                        ((JComponent) parent).revalidate();
                    }
                }
            };
            break;
        default:
            break;
        }
    }

    @Override
    public String getRightGap() {
        return gapright;
    }

    public String getLeftGap() {
        return gapleft;
    }

    @Override
    public Header addHeader(String name, Icon icon) {
        final Header header;
        add(header = new Header(name, icon), "gapleft " + getLeftGap() + ",spanx,newline,growx,pushx" + getRightGap());
        header.setLayout(new MigLayout("ins 0", "[35!,align left]5[]10[grow,fill]"));
        return header;
    }

    public JLabel addStartDescription(String description) {
        if (!description.toLowerCase().startsWith("<html>")) {
            description = "<html>" + description.replace("\r\n", "<br>").replace("\r", "<br>").replace("\n", "<br>") + "</html>";
        }
        final JLabel txt = new JLabel();
        SwingUtils.setOpaque(txt, false);
        // txt.setEnabled(false);
        txt.setText(description);
        add(txt, "gaptop 0,spanx,growx,pushx,gapleft 0,gapbottom 5,wmin 10");
        addSeperator();
        return txt;
    }

    public JSeparator addSeperator() {
        if (getComponent(getComponentCount() - 1) instanceof JSeparator) {
            return null;
        }
        final JSeparator sep;
        add(sep = new JSeparator(), "gapleft " + getLeftGap() + ",spanx,growx,pushx,gapbottom 5" + getRightGap());
        return sep;
    }

    @Override
    public JLabel addDescription(String description) {
        final JLabel txt = addDescriptionPlain(description);
        addSeperator();
        return txt;
    }

    public JLabel addDescriptionPlain(String description) {
        if (StringUtils.isEmpty(description)) {
            return null;
        }
        if (!description.toLowerCase().startsWith("<html>")) {
            description = "<html>" + description.replace("\r\n", "<br>").replace("\r", "<br>").replace("\n", "<br>") + "</html>";
        }
        final JLabel txt = new JLabel();
        SwingUtils.setOpaque(txt, false);
        txt.setEnabled(false);
        // txt.setEnabled(false);
        txt.setText(description);
        add(txt, "gaptop 0,spanx,growx,pushx,gapleft " + getLeftGap() + ",gapbottom 5,wmin 10" + getRightGap());
        return txt;
    }

    public void reset() {
        for (ConfigInterface cfg : interfaces) {
            for (KeyHandler m : cfg._getStorageHandler().getKeyHandler()) {
                m.setValue(m.getDefaultValue());
            }
        }
        getEventSender().fireEvent(new PluginConfigPanelEventSenderEvent() {
            @Override
            public void callListener(PluginConfigPanelEventSenderListener listener) {
                listener.onConfigPanelReset(plugin, PluginConfigPanelNG.this, interfaces);
            }
        });
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                updateContents();
            }
        };
    }

    @Override
    public Icon getIcon() {
        return null;
    }

    @Override
    public String getTitle() {
        return null;
    }

    public void initLayout(Plugin protoType) {
        this.plugin = protoType;
        if (accountSettingsComponents != null) {
            updateAccountSettings(protoType);
            return;
        }
        initPluginSettings(protoType);
        updateAccountSettings(protoType);
    }

    private void updateAccountSettings(Plugin protoType) {
        if (accountSettingsComponents != null) {
            for (Component c : accountSettingsComponents) {
                remove(c);
            }
        }
        accountSettingsComponents = new HashSet<Component>();
        updateAccounts = true;
        try {
            initAccountSettings(protoType, AccountController.getInstance().list(protoType.getHost()));
        } finally {
            updateAccounts = false;
        }
    }

    @Override
    protected void addImpl(Component comp, Object constraints, int index) {
        if (updateAccounts) {
            accountSettingsComponents.add(comp);
        }
        super.addImpl(comp, constraints, index);
    }

    @Override
    public void paint(Graphics g) {
        // Paint Border around and account entry. I do not want to use a wrapper panel, because this would fuck up the layout.
        int start = -1;
        int end = -1;
        Account acc = null;
        for (Component c : getComponents()) {
            if (c.getName() == ACCOUNT) {
                acc = accountMap.get(c);
                if (start >= 0) {
                    g.fillRect(0, start, getWidth(), headerHight);
                }
                if (acc != null) {
                    if (!acc.isValid()) {
                        g.setColor(LAFOptions.getInstance().getColorForTableAccountErrorRowBackground());
                    } else if (acc.isTempDisabled()) {
                        g.setColor(LAFOptions.getInstance().getColorForTableAccountTempErrorRowBackground());
                    } else {
                        g.setColor(LAFOptions.getInstance().getColorForPanelHeaderBackground());
                    }
                    if (!acc.isEnabled()) {
                        g.setColor(Color.LIGHT_GRAY);
                    }
                } else {
                    g.setColor(LAFOptions.getInstance().getColorForTableAccountTempErrorRowBackground());
                }
                start = c.getLocation().y - 2;
                end = c.getLocation().y + c.getHeight();
            } else {
                end = c.getLocation().y + c.getHeight();
            }
        }
        if (start >= 0) {
            g.fillRect(0, start, getWidth(), headerHight);
        }
        super.paint(g);
        g.setColor(LAFOptions.getInstance().getColorForPanelBorders());
        start = -1;
        end = -1;
        for (Component c : getComponents()) {
            if (c.getName() == ACCOUNT) {
                if (start >= 0) {
                    if (acc != null && !acc.isEnabled()) {
                        g.setColor(ColorUtils.getAlphaInstance(Color.LIGHT_GRAY, 50));
                        g.fillRect(0, start, getWidth() - 1, end - start - 1);
                        g.setColor(LAFOptions.getInstance().getColorForPanelBorders());
                    }
                    g.drawLine(0, start + headerHight, getWidth(), start + headerHight);
                    g.drawRect(0, start, getWidth() - 1, end - start - 1);
                }
                acc = accountMap.get(c);
                start = c.getLocation().y - 2;
                end = c.getLocation().y + c.getHeight();
            } else {
                end = c.getLocation().y + c.getHeight();
            }
        }
        if (start >= 0) {
            if (acc != null && !acc.isEnabled()) {
                g.setColor(ColorUtils.getAlphaInstance(Color.LIGHT_GRAY, 50));
                g.fillRect(0, start, getWidth() - 1, end - start - 1);
                g.setColor(LAFOptions.getInstance().getColorForPanelBorders());
            }
            g.drawLine(0, start + headerHight, getWidth(), start + headerHight);
            g.drawRect(0, start, getWidth() - 1, end - start - 1);
        }
    }

    protected String getTrafficString(final Account acc) {
        final AccountTrafficView trafficView = acc.getAccountTrafficView();
        if (!acc.isValid()) {
            return null;
        } else if (acc.isEnabled() && acc.isTempDisabled() && ((acc.getTmpDisabledTimeout() - System.currentTimeMillis()) > 0)) {
            return null;
        } else if (trafficView == null) {
            return "";
        } else {
            if (trafficView.isUnlimitedTraffic()) {
                return _GUI.T.premiumaccounttablemodel_column_trafficleft_unlimited();
            } else {
                return _GUI.T.premiumaccounttablemodel_column_trafficleft_left_(Formatter.formatReadable(trafficView.getTrafficLeft()), Formatter.formatReadable(trafficView.getTrafficMax()));
            }
        }
    }

    protected void initAccountSettings(final Plugin plugin, ArrayList<Account> accounts) {
        String gapbefore = gapleft;
        String gaprightBefore = getRightGap();
        if (accounts != null) {
            Collections.sort(accounts, new Comparator<Account>() {
                public int compare(boolean x, boolean y) {
                    return (x == y) ? 0 : (x ? 1 : -1);
                }

                @Override
                public int compare(Account o1, Account o2) {
                    final boolean e1 = o1.isEnabled();
                    final boolean e2 = o2.isEnabled();
                    int ret = compare(e2, e1);
                    if (ret == 0) {
                        boolean error1 = o1.getError() == null;
                        boolean error2 = o2.getError() == null;
                        ret = compare(error2, error1);
                    }
                    return ret;
                }
            });
        }
        try {
            if (plugin instanceof PluginForHost) {
                final PluginForHost plgh = ((PluginForHost) plugin);
                if (plgh.isPremiumEnabled()) {
                    AccountController.getInstance().getEventSender().removeListener(this);
                    AccountController.getInstance().getEventSender().addListener(this, true);
                    addHeader(_GUI.T.lit_your_accounts(plugin.getHost()), new AbstractIcon(IconKey.ICON_PREMIUM, 18));
                    addDescriptionPlain(_GUI.T.account_settings_description());
                    added = accounts != null && accounts.size() > 0;
                    if (!added) {
                        addDescriptionPlain(_GUI.T.description_accountmanager_button());
                    }
                    JButton bt = new JButton(ConfigurationView.getInstance().getSubPanel(AccountManagerSettings.class).getTitle());
                    bt.addActionListener(new ActionListener() {
                        @Override
                        public void actionPerformed(ActionEvent e) {
                            switchToAccountManager(null);
                            if (!added) {
                                SwingUtilities.invokeLater(new Runnable() {
                                    @Override
                                    public void run() {
                                        try {
                                            final BuyAndAddPremiumAccount dia;
                                            UIOManager.I().show(BuyAndAddPremiumDialogInterface.class, dia = new BuyAndAddPremiumAccount(DomainInfo.getInstance(plgh.getHost()), "pluginsettings"));
                                            dia.throwCloseExceptions();
                                        } catch (DialogClosedException e1) {
                                            e1.printStackTrace();
                                        } catch (DialogCanceledException e1) {
                                            e1.printStackTrace();
                                        }
                                    }
                                });
                            }
                        }
                    });
                    add(bt, "spanx,pushx,growx");
                    gapleft = "5";
                    gapright = ",gapright 5";
                    headerHight = 0;
                    accountMap = new HashMap<JLabel, Account>();
                    if (accounts != null) {
                        for (final Account acc : accounts) {
                            // addHeader(acc.getUser(), (Icon) null);
                            TextPane status;
                            JLabel accountHeader = new JLabel(_GUI.T.plugin_account_header(acc.getUser()));
                            SwingUtils.toBold(accountHeader);
                            add(accountHeader, "gaptop 2,gapbottom 2, gapleft " + gapleft + "" + getRightGap());
                            headerHight = Math.max(headerHight, accountHeader.getPreferredSize().height + 4);
                            accountHeader.setName(ACCOUNT);
                            accountMap.put(accountHeader, acc);
                            final ExtCheckBox enabledBox = new ExtCheckBox();
                            enabledBox.setSelected(acc.isEnabled());
                            enabledBox.addActionListener(new ActionListener() {
                                @Override
                                public void actionPerformed(ActionEvent e) {
                                    acc.setEnabled(enabledBox.isSelected());
                                    repaint();
                                }
                            });
                            enabledBox.setText(acc.isEnabled() ? _GUI.T.lit_enabled() : _GUI.T.lit_disabled());
                            accountHeader.setEnabled(acc.isEnabled());
                            add(enabledBox, "spanx,gapright " + gapleft + ", alignx right" + getRightGap());
                            addPair(_GUI.T.lit_status(), null, new Label(PremiumAccountTableModel.accountToStatusString(acc), PremiumAccountTableModel.accountToStatusIcon(acc)));
                            // addPair(_GUI.T.lit_type(), null, new Label(acc.getType().getLabel()));
                            final String traffic = getTrafficString(acc);
                            if (StringUtils.isNotEmpty(traffic)) {
                                addPair(_GUI.T.lit_download_traffic(), null, new Label(traffic));
                            }
                            final AccountInfo ai = acc.getAccountInfo();
                            if (ai != null) {
                                String expire = getExpireDateString(acc, ai);
                                if (StringUtils.isNotEmpty(expire)) {
                                    addPair(_GUI.T.premiumaccounttablemodel_column_expiredate(), null, new Label(expire));
                                }
                                long addedTs = acc.getLongProperty("added", -1);
                                if (addedTs > 0) {
                                    addPair(_GUI.T.lit_added(), null, new Label(formatDate(new Date(addedTs))));
                                }
                                if (ai.getUsedSpace() != -1) {
                                    addPair(_GUI.T.lit_used_space(), null, new Label(Formatter.formatReadable(ai.getUsedSpace())));
                                }
                                if (ai.getPremiumPoints() != -1) {
                                    addPair(_GUI.T.lit_premium_points(), null, new Label(ai.getPremiumPoints() + ""));
                                }
                            }
                            final Class<? extends AccountConfigInterface> accountConfig = plgh.getAccountConfigInterface(acc);
                            if (accountConfig != null) {
                                initAccountConfig(plgh, acc, accountConfig);
                            }
                            plgh.extendAccountSettingsPanel(acc, this);
                            if (acc.isMultiHost() && acc.getPlugin().getLazyP().hasFeature(LazyPlugin.FEATURE.MULTIHOST)) {
                                initMultiHosterInfo(plgh, acc);
                            }
                        }
                    }
                }
            }
        } finally {
            gapleft = gapbefore;
            gapright = gaprightBefore;
        }
    }

    private String getExpireDateString(Account acc, AccountInfo ai) {
        final long validUntil = ai.getValidUntil();
        if (validUntil <= 0) {
            return null;
        }
        final Date date = new Date(validUntil);
        final long left = validUntil - System.currentTimeMillis();
        if (left <= 0) {
            return formatDate(date) + " (" + _GUI.T.PremiumAccountTableModel_getStringValue_status_expired() + ")";
        } else {
            return formatDate(date) + " (" + TimeFormatter.formatMilliSeconds(left, TimeFormatter.HIDE_SECONDS) + ")";
        }
    }

    protected String formatDate(final Date date) {
        final String dateformat = Account.getExpireDateFormatString(this);
        return new SimpleDateFormat(dateformat).format(date);
    }

    protected void initAccountConfig(PluginForHost plugin, Account acc, Class<? extends AccountConfigInterface> accountConfig) {
        Header header = addHeader(_GUI.T.account_settings_header(), new AbstractIcon(IconKey.ICON_SETTINGS, 16));
        build(AccountJsonConfig.get(plugin, acc));
        if (getComponents()[getComponentCount() - 1] == header) {
            remove(header);
        } else {
            add(Box.createGlue(), "gapbottom 5,pushx,growx,spanx" + getRightGap());
        }
    }

    protected void initMultiHosterInfo(final PluginForHost plugin, final Account acc) {
        String gapbefore = getLeftGap();
        String gapbeforeright = getRightGap();
        try {
            gapleft = "5";
            gapright = ",gapright 5";
            addHeader(_GUI.T.multihoster_account_settings_header(), new AbstractIcon(IconKey.ICON_LIST, 18));
            addDescriptionPlain(_GUI.T.multihoster_account_settings_description());
            plugin.extendMultiHostAccountSettingsPanel(acc, this);
        } finally {
            gapleft = gapbefore;
            gapright = gapbeforeright;
        }
    }

    protected void initPluginSettings(Plugin plugin) {
        final Class<? extends PluginConfigInterface> inf = plugin.getConfigInterface();
        if (inf == null) {
            return;
        }
        final PluginConfigInterface config;
        if (plugin instanceof PluginForHost) {
            config = PluginJsonConfig.get(((PluginForHost) plugin).getLazyP(), inf);
        } else if (plugin instanceof PluginForDecrypt) {
            config = PluginJsonConfig.get(((PluginForDecrypt) plugin).getLazyC(), inf);
        } else {
            config = null;
        }
        if (config != null) {
            build(config);
        }
    }

    public Rectangle getAccountRectangle(Account account) {
        return null;
    }

    protected void switchToAccountManager(AccountEntry obj) {
        JsonConfig.create(GraphicalUserInterfaceSettings.class).setConfigViewVisible(true);
        JDGui.getInstance().setContent(ConfigurationView.getInstance(), true);
        ConfigurationView.getInstance().setSelectedSubPanel(AccountManagerSettings.class);
        ConfigurationView.getInstance().getSubPanel(AccountManagerSettings.class).getAccountManager().setTab(0);
        if (obj != null) {
            ConfigurationView.getInstance().getSubPanel(AccountManagerSettings.class).getAccountManager().selectAccount(obj.getAccount());
        }
    }

    protected Map<String, Boolean> newMapInstance(KeyHandler m) throws InstantiationException, IllegalAccessException {
        Class raw = ReflectionUtils.getRaw(m.getTypeRef().getType());
        if (raw.isInterface()) {
            raw = HashMap.class;
        }
        final Map<String, Boolean> value = (Map<String, Boolean>) raw.newInstance();
        value.clear();
        return value;
    }

    public void addHandler(final ConfigInterface cfg, final KeyHandler m) {
        final DescriptionForConfigEntry anno = (DescriptionForConfigEntry) m.getAnnotation(DescriptionForConfigEntry.class);
        String label = getTranslation(m, "_label");
        if (label == null) {
            label = m.getReadableName();
        }
        String description = getTranslation(m, "_description");
        if (description == null && anno != null) {
            description = anno.value();
        }
        Pair<?> pair = null;
        try {
            if (m instanceof BooleanKeyHandler) {
                pair = addPair(label, null, new Checkbox((BooleanKeyHandler) m));
                return;
            } else if (m instanceof StringKeyHandler) {
                pair = addPair(label, null, new TextInput((StringKeyHandler) m));
                return;
            } else if (m instanceof IntegerKeyHandler) {
                pair = addPair(label, null, new Spinner(new ConfigIntSpinnerModel((IntegerKeyHandler) m)));
                return;
            } else if (m instanceof LongKeyHandler) {
                pair = addPair(label, null, new Spinner(new ConfigLongSpinnerModel((LongKeyHandler) m)));
                return;
            } else if (m instanceof EnumKeyHandler) {
                pair = addPair(label, null, null, new ComboBox<Enum>(m, ((EnumKeyHandler) m).values(), null));
                return;
            } else if (m instanceof ObjectKeyHandler) {
                final Class<?> raw = org.appwork.utils.ReflectionUtils.getRaw(m.getTypeRef().getType());
                if (Map.class.isAssignableFrom(raw)) {
                    final Type[] types = ((ParameterizedType) m.getTypeRef().getType()).getActualTypeArguments();
                    if (types[0] == String.class && types[1] == Boolean.class) {
                        try {
                            Map<String, Boolean> value = (Map<String, Boolean>) m.getValue();
                            if (value == null) {
                                value = newMapInstance(m);
                            }
                            final Map<String, Boolean> finalValue = value;
                            final MultiComboBox<String> comp = new MultiComboBox<String>(new ArrayList<String>(value.keySet())) {
                                private final GenericConfigEventListener<Map<String, Boolean>> listener = new GenericConfigEventListener<Map<String, Boolean>>() {
                                    @Override
                                    public void onConfigValidatorError(KeyHandler<Map<String, Boolean>> keyHandler, Map<String, Boolean> invalidValue, ValidationException validateException) {
                                    }

                                    @Override
                                    public void onConfigValueModified(KeyHandler<Map<String, Boolean>> keyHandler, Map<String, Boolean> newValue) {
                                        updateModel(newValue);
                                    }
                                };
                                {
                                    m.getEventSender().addListener(listener, true);
                                    updateModel(finalValue);
                                }

                                @Override
                                protected String getLabel(String sc) {
                                    return super.getLabel(sc);
                                }

                                @Override
                                protected String getLabel(List<String> list) {
                                    return "[" + list.size() + "/" + getValues().size() + "] " + super.getLabel(list);
                                }

                                protected void updateModel(final Map<String, Boolean> value) {
                                    final ArrayList<String> selected = new ArrayList<String>();
                                    for (Entry<String, Boolean> es : value.entrySet()) {
                                        if (es.getValue() == Boolean.TRUE) {
                                            selected.add(es.getKey());
                                        }
                                    }
                                    setSelectedItems(selected);
                                }

                                @Override
                                public void onChanged() {
                                    super.onChanged();
                                    final Map<String, Boolean> value = (Map<String, Boolean>) m.getValue();
                                    final DontThrowFromCurrentThreadEventSuppressor<DefaultEvent> added = new DontThrowFromCurrentThreadEventSuppressor<DefaultEvent>();
                                    m.getEventSender().addEventSuppressor(added);
                                    try {
                                        final Map<String, Boolean> ret = newMapInstance(m);
                                        ret.putAll(value);
                                        for (String key : ret.keySet()) {
                                            ret.put(key, isItemSelected(key));
                                        }
                                        m.setValue(ret);
                                    } catch (InstantiationException e) {
                                        LogController.CL().log(e);
                                    } catch (IllegalAccessException e) {
                                        LogController.CL().log(e);
                                    } finally {
                                        m.getEventSender().removeEventSuppressor(added);
                                    }
                                }
                            };
                            pair = addPair(label, null, null, comp);
                        } catch (InstantiationException e) {
                            LogController.CL().log(e);
                        } catch (IllegalAccessException e) {
                            LogController.CL().log(e);
                        }
                        return;
                    }
                } else if (Set.class.isAssignableFrom(raw)) {
                    final Type[] types = ((ParameterizedType) m.getTypeRef().getType()).getActualTypeArguments();
                    if (types[0] instanceof Class && ((Class) types[0]).isEnum()) {
                        try {
                            final MultiComboBox<Object> comp = new MultiComboBox<Object>(((Class) types[0]).getEnumConstants()) {
                                private final GenericConfigEventListener<Set<Enum>> listener = new GenericConfigEventListener<Set<Enum>>() {
                                    @Override
                                    public void onConfigValidatorError(KeyHandler<Set<Enum>> keyHandler, Set<Enum> invalidValue, ValidationException validateException) {
                                    }

                                    @Override
                                    public void onConfigValueModified(KeyHandler<Set<Enum>> keyHandler, Set<Enum> newValue) {
                                        updateModel(newValue);
                                    }
                                };
                                {
                                    Set<Enum> value = (Set<Enum>) m.getValue();
                                    if (value == null) {
                                        value = newSetInstance(m);
                                        for (Object e : ((Class) types[0]).getEnumConstants()) {
                                            value.add((Enum) e);
                                        }
                                    }
                                    m.getEventSender().addListener(listener, true);
                                    updateModel(value);
                                }

                                @Override
                                protected String getLabel(List<Object> list) {
                                    return "[" + list.size() + "/" + getValues().size() + "] " + super.getLabel(list);
                                }

                                protected Set<Enum> newSetInstance(KeyHandler m) throws InstantiationException, IllegalAccessException {
                                    Class raw = ReflectionUtils.getRaw(m.getTypeRef().getType());
                                    if (raw.isInterface()) {
                                        raw = HashSet.class;
                                    }
                                    final Set<Enum> value = (Set<Enum>) raw.newInstance();
                                    return value;
                                }

                                protected void updateModel(final Set<Enum> newValue) {
                                    if (newValue == null) {
                                        setSelectedItems(((Class) types[0]).getEnumConstants());
                                    } else {
                                        setSelectedItems((Object[]) newValue.toArray(new Enum[0]));
                                    }
                                }

                                @Override
                                public void onChanged() {
                                    super.onChanged();
                                    final EventSuppressor added = new DontThrowFromCurrentThreadEventSuppressor<DefaultEvent>();
                                    m.getEventSender().addEventSuppressor(added);
                                    try {
                                        final Set<Enum> set = newSetInstance(m);
                                        for (Object e : getSelectedItems()) {
                                            set.add((Enum) e);
                                        }
                                        m.setValue(set);
                                    } catch (InstantiationException e1) {
                                        LogController.CL().log(e1);
                                    } catch (IllegalAccessException e1) {
                                        LogController.CL().log(e1);
                                    } finally {
                                        m.getEventSender().removeEventSuppressor(added);
                                    }
                                }
                            };
                            pair = addPair(label, null, null, comp);
                        } catch (InstantiationException e) {
                            LogController.CL().log(e);
                        } catch (IllegalAccessException e) {
                            LogController.CL().log(e);
                        }
                        return;
                    }
                }
            }
        } finally {
            if (description != null && !StringUtils.equalsIgnoreCase(description, label)) {
                addDescriptionPlain(description);
            }
            if (pair != null) {
                final SettingsComponent comp = pair.getComponent();
                if (comp != null) {
                    comp.setToolTipText(description);
                }
            }
        }
        if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            UIOManager.I().showException("Unsupported Type: " + m, new Exception());
        }
    }

    private String getTranslation(KeyHandler m, String ext) {
        if (translation == null) {
            return null;
        }
        String caseSensitiveGetter = m.getGetMethod().getName();
        if (caseSensitiveGetter.startsWith("is")) {
            caseSensitiveGetter = "get" + caseSensitiveGetter.substring(2);
        }
        try {
            Method method;
            method = translation.getClass().getMethod(caseSensitiveGetter + ext, new Class[] {});
            method.setAccessible(true);
            return (String) method.invoke(translation, new Object[] {});
        } catch (NoSuchMethodException e) {
            if ("_label".equals(ext)) {
                System.out.println("Missing Translation: " + translation.getClass().getName() + "." + caseSensitiveGetter + ext + "(){return ....}");
            }
        } catch (Throwable e) {
            LoggerFactory.getDefaultLogger().log(e);
        }
        return null;
    }

    private final CopyOnWriteArraySet<ConfigInterface> interfaces = new CopyOnWriteArraySet<ConfigInterface>();

    protected boolean showKeyHandler(KeyHandler<?> keyHandler) {
        return keyHandler != null;
    }

    public void build(ConfigInterface cfg) {
        interfaces.add(cfg);
        ArrayList<Group> interfaceGroups = new ArrayList<Group>();
        interfaceGroups.addAll(groups);
        Field groupField;
        try {
            groupField = cfg._getStorageHandler().getConfigInterface().getField("GROUPS");
            groupField.setAccessible(true);
            for (Group p : (Group[]) groupField.get(null)) {
                interfaceGroups.add(p.createClone());
            }
        } catch (NoSuchFieldException e) {
            // ok
        } catch (Throwable e) {
            LoggerFactory.getDefaultLogger().log(e);
            UIOManager.I().showException("Bad Plugin Config Interface. Contact Support.", e);
        }
        try {
            Field field = cfg._getStorageHandler().getConfigInterface().getField("TRANSLATION");
            field.setAccessible(true);
            translation = field.get(null);
        } catch (NoSuchFieldException e) {
            // ok
        } catch (Throwable e) {
            LoggerFactory.getDefaultLogger().log(e);
            UIOManager.I().showException("Bad Plugin Config Interface. Contact Support.", e);
        }
        Group defGroup = null;
        ArrayList<Group> finalGroups = new ArrayList<Group>();
        HashSet<Group> groupsAdded = new HashSet<Group>();
        List<KeyHandler<?>> list = cfg._getStorageHandler().getKeyHandler();
        Collections.sort(list, new Comparator<KeyHandler<?>>() {
            @Override
            public int compare(KeyHandler<?> o1, KeyHandler<?> o2) {
                Order orderAn1 = o1.getAnnotation(Order.class);
                Order orderAn2 = o2.getAnnotation(Order.class);
                int order1 = orderAn1 == null ? Integer.MAX_VALUE : orderAn1.value();
                int order2 = orderAn2 == null ? Integer.MAX_VALUE : orderAn2.value();
                int ret = CompareUtils.compareInt(order1, order2);
                if (ret == 0) {
                    return o1.getKey().compareToIgnoreCase(o2.getKey());
                } else {
                    return ret;
                }
            }
        });
        parent: for (KeyHandler<?> m : list) {
            if (!showKeyHandler(m)) {
                continue;
            }
            for (Group g : interfaceGroups) {
                if (g.matches(m)) {
                    g.add(m);
                    if (groupsAdded.add(g)) {
                        finalGroups.add(g);
                    }
                    defGroup = null;
                    continue parent;
                }
            }
            if (defGroup == null) {
                defGroup = new Group();
                finalGroups.add(defGroup);
            }
            defGroup.add(m);
        }
        seperatorRequired = false;
        for (Group p : finalGroups) {
            if (StringUtils.isNotEmpty(p.getTitle())) {
                addHeader(p.getTitle(), new AbstractIcon(p.getIconKey(), 20));
                seperatorRequired = false;
            } else if (seperatorRequired) {
                addSeperator();
                seperatorRequired = false;
            }
            for (KeyHandler h : p.handler) {
                if (useCustomUI(h)) {
                    continue;
                }
                addHandler(cfg, h);
                if (p.getTitle() != null) {
                    seperatorRequired = true;
                }
            }
        }
    }

    protected boolean useCustomUI(KeyHandler<?> h) {
        return h.getAnnotation(CustomUI.class) != null;
    }

    public void addStringPair(String key, Object value) {
        if (value == null) {
            return;
        }
        addPair(key, null, new Label(String.valueOf(value)));
    }
}
