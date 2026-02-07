package jd.plugins;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.Icon;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.Timer;
import javax.swing.table.JTableHeader;

import org.appwork.storage.config.JsonConfig;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableHeaderRenderer;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtFileSizeColumn;
import org.appwork.swing.exttable.columns.ExtLongColumn;
import org.appwork.swing.exttable.columns.ExtProgressColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.formatter.TimeFormatter;
import org.jdownloader.DomainInfo;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.controller.host.HostPluginController;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;
import org.jdownloader.settings.GraphicalUserInterfaceSettings;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SIZEUNIT;

import jd.controlling.AccountController;
import jd.gui.swing.jdgui.BasicJDTable;
import jd.plugins.MultiHostHost.MultihosterHostStatus;
import net.miginfocom.swing.MigLayout;

/**
 * Builds the multi-host account settings panel UI.
 *
 * Separates panel building logic from PluginForHost to keep that class smaller.
 */
public class MultiHostAccountSettingsPanelBuilder {
    private final Account             account;
    private final AccountInfo         accountInfo;
    private final List<MultiHostHost> hosts;

    /**
     * Creates a new panel builder for a multi-host account.
     *
     * @param account
     *            The account to build the panel for
     */
    public MultiHostAccountSettingsPanelBuilder(final Account account) {
        if (account == null) {
            throw new IllegalArgumentException("Account is missing");
        }
        final AccountInfo ai = account.getAccountInfo();
        if (ai == null) {
            throw new IllegalArgumentException("Account without AccountInfo");
        }
        final List<MultiHostHost> hostList = ai.getMultiHostSupportV2();
        this.account = account;
        this.accountInfo = ai;
        this.hosts = hostList;
    }

    /**
     * Builds and adds the panel to the given container
     *
     * @param panel
     *            The panel to add the table to
     */
    public void build(final PluginConfigPanelNG panel) {
        if (accountInfo == null || hosts == null) {
            return;
        }
        final ExtTableModel<MultiHostHost> tableModel = createTableModel();
        // Add highlighter if needed
        boolean needsHighlighter = false;
        for (final MultiHostHost mhost : hosts) {
            if ((!mhost.isUnlimitedLinks() && mhost.getLinksLeft() <= 0) || (!mhost.isUnlimitedTraffic() && mhost.getTrafficLeft() <= 0) || (mhost.getUnavailableTimeMillis() > 0)) {
                needsHighlighter = true;
                break;
            }
        }
        if (needsHighlighter) {
            tableModel.addExtComponentRowHighlighter(new org.appwork.swing.exttable.ExtComponentRowHighlighter<MultiHostHost>(null, Color.YELLOW, null) {
                @Override
                public boolean accept(ExtColumn<MultiHostHost> column, MultiHostHost mhost, boolean selected, boolean focus, int row) {
                    if (!mhost.isUnlimitedLinks() && mhost.getLinksLeft() <= 0) {
                        return true;
                    } else if (!mhost.isUnlimitedTraffic() && mhost.getTrafficLeft() <= 0) {
                        return true;
                    } else if (mhost.getUnavailableTimeMillis() > 0) {
                        return true;
                    } else {
                        return false;
                    }
                }
            });
        }
        tableModel._fireTableStructureChanged(hosts, false);
        final BasicJDTable<MultiHostHost> table = new BasicJDTable<MultiHostHost>(tableModel);
        table.setPreferredScrollableViewportSize(new Dimension(table.getPreferredSize().width, table.getRowHeight() * table.getRowCount()));
        table.setSearchEnabled(true);
        table.addMouseWheelListener(new MouseWheelListener() {
            @Override
            public void mouseWheelMoved(MouseWheelEvent e) {
                /**
                 * Forward event to upper panel so that the scrolling happens in it and not in our table which is always full-size and has
                 * no vertical scrollbar.
                 */
                panel.dispatchEvent(e);
            }
        });
        final JScrollPane scrollPane = new JScrollPane(table);
        panel.add(scrollPane);
    }

    /**
     * Creates the table model with all columns
     */
    private ExtTableModel<MultiHostHost> createTableModel() {
        /* Determine default visibility states for some columns */
        boolean shouldShowLinkLimitColumns = false;
        boolean shouldShowLinksMaxColumn = false;
        boolean shouldShowTrafficLeftColumns = false;
        boolean shouldShowTrafficMaxInfo = false;
        boolean shouldShowTrafficCalculationColumn = false;
        boolean shouldShowUnavailableForColumn = false;
        boolean containsItemsWithCustomStatusText = false;
        for (final MultiHostHost mhost : hosts) {
            if (!shouldShowLinkLimitColumns && !mhost.isUnlimitedLinks()) {
                shouldShowLinkLimitColumns = true;
                if (!shouldShowLinksMaxColumn && mhost.getLinksMax() > 0) {
                    shouldShowLinksMaxColumn = true;
                }
            }
            if (!shouldShowTrafficLeftColumns && !mhost.isUnlimitedTraffic()) {
                shouldShowTrafficLeftColumns = true;
                if (!shouldShowTrafficMaxInfo && mhost.getTrafficMax() > 0) {
                    shouldShowTrafficMaxInfo = true;
                }
            }
            if (!shouldShowTrafficCalculationColumn && mhost.getTrafficCalculationFactorPercent() != 100) {
                shouldShowTrafficCalculationColumn = true;
            }
            if (!shouldShowUnavailableForColumn && mhost.getUnavailableTimeMillis() > 0) {
                shouldShowUnavailableForColumn = true;
            }
            if (!containsItemsWithCustomStatusText && mhost.getStatusText() != null) {
                containsItemsWithCustomStatusText = true;
            }
            if (shouldShowTrafficLeftColumns && shouldShowLinkLimitColumns && shouldShowLinkLimitColumns && shouldShowTrafficMaxInfo && shouldShowTrafficCalculationColumn && shouldShowUnavailableForColumn) {
                /* All default disabled columns shall be displayed -> Early abort this loop. */
                break;
            }
        }
        final boolean shouldShowLinkLimitColumns_final = shouldShowLinkLimitColumns;
        final boolean shouldShowLinksMaxColumn_final = shouldShowLinksMaxColumn;
        final boolean shouldShowTrafficLeftColumns_final = shouldShowTrafficLeftColumns;
        final boolean shouldShowTrafficMaxInfo_final = shouldShowTrafficMaxInfo;
        final boolean shouldShowTrafficCalculationColumn_final = shouldShowTrafficCalculationColumn;
        final boolean shouldShowUnavailableForColumn_final = shouldShowUnavailableForColumn;
        final boolean shouldShowInternalStatusColumn_final = containsItemsWithCustomStatusText;
        final Icon icon_error = NewTheme.I().getIcon(IconKey.ICON_ERROR, 16);
        final Icon icon_okay = NewTheme.I().getIcon(IconKey.ICON_OK, 16);
        final Icon icon_warning = NewTheme.I().getIcon(IconKey.ICON_WARNING, 16);
        final Icon icon_wait = NewTheme.I().getIcon(IconKey.ICON_WAIT, 16);
        final Icon icon_filler = NewTheme.I().getIcon(IconKey.ICON_BEER, 16);
        final DecimalFormat formatter = new DecimalFormat();
        return new ExtTableModel<MultiHostHost>("MultiHostHostTable_" + account.getHoster()) {
            @Override
            protected void initColumns() {
                addColumn(createEnabledColumn());
                addColumn(createDomainColumn(icon_filler));
                addColumn(createStatusColumn(icon_error, icon_okay, icon_warning, icon_wait));
                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && shouldShowInternalStatusColumn_final) {
                    addColumn(createInternalStatusColumn());
                }
                if (shouldShowUnavailableForColumn_final) {
                    addColumn(createUnavailableForColumn());
                }
                if (shouldShowLinkLimitColumns_final) {
                    addColumn(createLinksProgressColumn());
                    addColumn(createLinksLeftColumn(formatter));
                }
                if (shouldShowLinksMaxColumn_final) {
                    addColumn(createLinksMaxColumn(formatter));
                }
                if (shouldShowTrafficLeftColumns_final) {
                    addColumn(createTrafficProgressColumn(formatter));
                    addColumn(createTrafficLeftColumn(formatter));
                }
                if (shouldShowTrafficMaxInfo_final) {
                    addColumn(createTrafficMaxColumn(formatter));
                }
                if (shouldShowTrafficCalculationColumn_final) {
                    addColumn(createTrafficCalculationFactorColumn());
                }
                addColumn(createCreatedColumn());
            }
        };
    }

    private ExtCheckColumn<MultiHostHost> createEnabledColumn() {
        return new ExtCheckColumn<MultiHostHost>(_GUI.T.premiumaccounttablemodel_column_enabled()) {
            @Override
            public ExtTableHeaderRenderer getHeaderRenderer(final JTableHeader jTableHeader) {
                final ExtTableHeaderRenderer ret = new ExtTableHeaderRenderer(this, jTableHeader) {
                    private final Icon ok = NewTheme.I().getIcon(IconKey.ICON_OK, 14);

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
            protected boolean getBooleanValue(final MultiHostHost mhost) {
                return mhost.isEnabled();
            }

            @Override
            public boolean isEditable(MultiHostHost mhost) {
                return true;
            }

            @Override
            protected void setBooleanValue(final boolean enabled, final MultiHostHost mhost) {
                mhost.setEnabled(enabled);
                // fireTableStructureChanged();
                this.getModel().fireTableStructureChanged();
            }
        };
    }

    private ExtTextColumn<MultiHostHost> createDomainColumn(final Icon icon_filler) {
        return new ExtTextColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_domain()) {
            @Override
            public String getStringValue(final MultiHostHost mhost) {
                return mhost.getDomain();
            }

            @Override
            public Icon getIcon(MultiHostHost mhost) {
                final DomainInfo di = mhost.getDomainInfo();
                if (di != null) {
                    return di.getFavIcon(false);
                } else {
                    return icon_filler;
                }
            }

            @Override
            public boolean onDoubleClick(MouseEvent e, MultiHostHost mhost) {
                if (mhost.getStatus() == MultihosterHostStatus.DEACTIVATED_JDOWNLOADER_UNSUPPORTED) {
                    /* Host is not supported by JD -> It doesn't make sense to even try to open an affiliate link. */
                    return false;
                }
                final LazyHostPlugin lazyHostPlugin = HostPluginController.getInstance().get(mhost.getDomain());
                if (lazyHostPlugin != null && lazyHostPlugin.isPremium()) {
                    AccountController.openAfflink(lazyHostPlugin, null, "MultiHostSupportedHostsDetailTable");
                }
                return false;
            }

            @Override
            protected String getTooltipText(final MultiHostHost mhost) {
                /* Display comma separated list of all known domains of this host as tooltip. */
                final StringBuilder sb = new StringBuilder();
                for (final String domain : mhost.getDomains()) {
                    if (sb.length() > 0) {
                        sb.append(", ");
                    }
                    sb.append(domain);
                }
                return sb.toString();
            }

            @Override
            public boolean isEnabled(final MultiHostHost mhost) {
                return mhost.isEnabled();
            }
        };
    }

    private ExtTextColumn<MultiHostHost> createStatusColumn(final Icon icon_error, final Icon icon_okay, final Icon icon_warning, final Icon icon_wait) {
        return new ExtTextColumn<MultiHostHost>("Status") {
            @Override
            public String getStringValue(final MultiHostHost mhost) {
                final String text = mhost.getStatusText();
                if (mhost.getUnavailableTimeMillis() > 0) {
                    return mhost.getUnavailableStatusText();
                } else if (!mhost.isUnlimitedLinks() && mhost.getLinksLeft() <= 0) {
                    return _GUI.T.account_error_no_links_left();
                } else if (!mhost.isUnlimitedTraffic() && mhost.getTrafficLeft() <= 0) {
                    return _GUI.T.account_error_no_traffic_left();
                } else if (text != null) {
                    return text;
                } else {
                    return mhost.getStatus().getLabel();
                }
            }

            @Override
            protected String getTooltipText(final MultiHostHost mhost) {
                if (mhost.getUnavailableTimeMillis() > 0) {
                    return "Host temporarily unavailable because of too many wrong download attempts";
                } else {
                    return mhost.getStatus().getLabel();
                }
            }

            private final Color defaultColor;
            {
                renderer.setLayout(new MigLayout("ins 0", "[grow,fill][]", "[grow,fill]"));
                defaultColor = rendererField.getForeground();
            }

            @Override
            public void configureRendererComponent(MultiHostHost value, boolean isSelected, boolean hasFocus, int row, int column) {
                super.configureRendererComponent(value, isSelected, hasFocus, row, column);
                final MultihosterHostStatus status = value.getStatus();
                if (status != MultihosterHostStatus.WORKING && status != MultihosterHostStatus.WORKING_UNSTABLE) {
                    rendererField.setForeground(Color.RED);
                } else {
                    rendererField.setForeground(defaultColor);
                }
            }

            @Override
            public Icon getIcon(MultiHostHost mhost) {
                final MultihosterHostStatus status = mhost.getStatus();
                if (mhost.getUnavailableTimeMillis() > 0) {
                    return icon_wait;
                } else if (!mhost.isUnlimitedLinks() && mhost.getLinksLeft() <= 0) {
                    return icon_wait;
                } else if (!mhost.isUnlimitedTraffic() && mhost.getTrafficLeft() <= 0) {
                    return icon_wait;
                } else if (status == MultihosterHostStatus.WORKING) {
                    return icon_okay;
                } else if (status == MultihosterHostStatus.WORKING_UNSTABLE) {
                    return icon_warning;
                } else {
                    return icon_error;
                }
            }

            @Override
            public boolean isEnabled(final MultiHostHost mhost) {
                return mhost.isEnabled();
            }
        };
    }

    private ExtTextColumn<MultiHostHost> createInternalStatusColumn() {
        return new ExtTextColumn<MultiHostHost>("Internal status") {
            @Override
            public String getStringValue(final MultiHostHost mhost) {
                return mhost.getStatus().getLabel();
            }

            private final Color defaultColor;
            {
                renderer.setLayout(new MigLayout("ins 0", "[grow,fill][]", "[grow,fill]"));
                defaultColor = rendererField.getForeground();
            }

            @Override
            public void configureRendererComponent(MultiHostHost value, boolean isSelected, boolean hasFocus, int row, int column) {
                super.configureRendererComponent(value, isSelected, hasFocus, row, column);
                final MultihosterHostStatus status = value.getStatus();
                if (status != MultihosterHostStatus.WORKING && status != MultihosterHostStatus.WORKING_UNSTABLE) {
                    rendererField.setForeground(Color.RED);
                } else {
                    rendererField.setForeground(defaultColor);
                }
            }

            @Override
            public boolean isEnabled(final MultiHostHost mhost) {
                return mhost.isEnabled();
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }
        };
    }

    private ExtLongColumn<MultiHostHost> createUnavailableForColumn() {
        return new ExtLongColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_unavailable_for()) {
            @Override
            protected long getLong(MultiHostHost mhost) {
                return mhost.getUnavailableTimeMillis();
            }

            private final AtomicReference<Timer> timerReference = new AtomicReference<Timer>();

            @Override
            public boolean isVisible(boolean savedValue) {
                final boolean ret = super.isVisible(savedValue);
                final ExtLongColumn<MultiHostHost> thisColumn = this;
                if (ret && timerReference.get() == null) {
                    final Timer countdownTimer = new Timer(1000, new ActionListener() {
                        @Override
                        public void actionPerformed(ActionEvent e) {
                            final ExtTableModel<MultiHostHost> model = thisColumn.getModel();
                            if (model == null) {
                                return;
                            }
                            if (!model.isColumnVisible(thisColumn) || timerReference.get() != e.getSource()) {
                                ((Timer) e.getSource()).stop();
                                return;
                            }
                            model.fireTableDataChanged();
                        }
                    }) {
                        private static final long serialVersionUID = -8818019184160268747L;

                        @Override
                        public void stop() {
                            super.stop();
                            timerReference.compareAndSet(this, null);
                        }
                    };
                    countdownTimer.start();
                    timerReference.set(countdownTimer);
                }
                return ret;
            }

            @Override
            protected String getLongFormatted(MultiHostHost mhost) {
                final long unavailableFor = getLong(mhost);
                if (unavailableFor > 0) {
                    return TimeFormatter.formatMilliSeconds(unavailableFor, 0);
                } else {
                    return "---";
                }
            }

            @Override
            public boolean isEnabled(final MultiHostHost mhost) {
                return mhost.isEnabled();
            }
        };
    }

    private ExtProgressColumn<MultiHostHost> createLinksProgressColumn() {
        return new ExtProgressColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_links_left_max()) {
            @Override
            public int getMinWidth() {
                return 50;
            }

            @Override
            protected boolean isIndeterminated(final MultiHostHost value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                return false;
            }

            @Override
            protected String getString(MultiHostHost mhost, long current, long total) {
                if (mhost.isUnlimitedLinks()) {
                    return _GUI.T.lit_unlimited();
                } else {
                    return mhost.getLinksLeft() + "/" + this.getMax(mhost);
                }
            }

            @Override
            protected long getMax(MultiHostHost mhost) {
                if (mhost.isUnlimitedLinks()) {
                    return Long.MAX_VALUE;
                } else if (mhost.getLinksMax() > 0) {
                    return mhost.getLinksMax();
                } else {
                    /* No max value given -> Return links left value as max */
                    return mhost.getLinksLeft();
                }
            }

            @Override
            protected long getValue(MultiHostHost mhost) {
                if (mhost.isUnlimitedLinks()) {
                    return Long.MAX_VALUE;
                } else {
                    return mhost.getLinksLeft();
                }
            }

            @Override
            public boolean isEnabled(final MultiHostHost mhost) {
                return mhost.isEnabled();
            }
        };
    }

    private ExtLongColumn<MultiHostHost> createLinksLeftColumn(final DecimalFormat formatter) {
        return new ExtLongColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_links_left()) {
            @Override
            protected long getLong(MultiHostHost mhost) {
                return mhost.getLinksLeft();
            }

            @Override
            protected String getLongFormatted(MultiHostHost mhost) {
                if (mhost.isUnlimitedLinks()) {
                    return _GUI.T.lit_unlimited();
                } else {
                    return StringUtils.toString(formatter, getLong(mhost));
                }
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }

            @Override
            public boolean isEnabled(final MultiHostHost mhost) {
                return mhost.isEnabled();
            }
        };
    }

    private ExtLongColumn<MultiHostHost> createLinksMaxColumn(final DecimalFormat formatter) {
        return new ExtLongColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_links_max()) {
            @Override
            protected long getLong(MultiHostHost mhost) {
                if (mhost.getLinksMax() > 0) {
                    return mhost.getLinksMax();
                } else {
                    return mhost.getLinksLeft();
                }
            }

            @Override
            protected String getLongFormatted(MultiHostHost mhost) {
                if (mhost.isUnlimitedLinks()) {
                    return _GUI.T.lit_unlimited();
                } else {
                    return StringUtils.toString(formatter, getLong(mhost));
                }
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }

            @Override
            public boolean isEnabled(final MultiHostHost mhost) {
                return mhost.isEnabled();
            }
        };
    }

    private ExtProgressColumn<MultiHostHost> createTrafficProgressColumn(final DecimalFormat formatter) {
        return new ExtProgressColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_traffic_left_max()) {
            {
                setRowSorter(new ExtDefaultRowSorter<MultiHostHost>() {
                    @Override
                    public int compare(final MultiHostHost o1, final MultiHostHost o2) {
                        final long v1 = getValue(o1);
                        final long v2 = getValue(o2);
                        if (v1 == v2) {
                            return 0;
                        }
                        if (this.getSortOrderIdentifier() != ExtColumn.SORT_ASC) {
                            return v1 > v2 ? -1 : 1;
                        } else {
                            return v2 > v1 ? -1 : 1;
                        }
                    }
                });
            }
            private final SIZEUNIT maxSizeUnit = JsonConfig.create(GraphicalUserInterfaceSettings.class).getMaxSizeUnit();

            @Override
            public int getMinWidth() {
                return 140;
            }

            @Override
            protected boolean isIndeterminated(final MultiHostHost value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                return false;
            }

            @Override
            protected String getString(MultiHostHost mhost, long current, long total) {
                if (mhost.isUnlimitedTraffic()) {
                    return _GUI.T.premiumaccounttablemodel_column_trafficleft_unlimited();
                } else {
                    /* Max traffic is given -> Display both traffic left and max */
                    return _GUI.T.premiumaccounttablemodel_column_trafficleft_left_(getSizeString(Math.max(0, mhost.getTrafficLeft())), getSizeString(getMax(mhost)));
                }
            }

            private final String getSizeString(final long fileSize) {
                return maxSizeUnit.formatValue(maxSizeUnit, formatter, fileSize);
            }

            @Override
            protected long getMax(MultiHostHost mhost) {
                if (mhost.isUnlimitedTraffic()) {
                    return Long.MAX_VALUE;
                } else if (mhost.getTrafficMax() > 0) {
                    return mhost.getTrafficMax();
                } else {
                    return mhost.getTrafficLeft();
                }
            }

            @Override
            protected long getValue(MultiHostHost mhost) {
                if (mhost.isUnlimitedTraffic()) {
                    return Long.MAX_VALUE;
                } else {
                    return Math.max(0, mhost.getTrafficLeft());
                }
            }

            @Override
            public boolean isEnabled(final MultiHostHost mhost) {
                return mhost.isEnabled();
            }
        };
    }

    private ExtFileSizeColumn<MultiHostHost> createTrafficLeftColumn(final DecimalFormat formatter) {
        return new ExtFileSizeColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_traffic_left()) {
            private final SIZEUNIT maxSizeUnit = JsonConfig.create(GraphicalUserInterfaceSettings.class).getMaxSizeUnit();

            @Override
            public String getStringValue(MultiHostHost mhost) {
                if (mhost.isUnlimitedTraffic()) {
                    return _GUI.T.lit_unlimited();
                } else {
                    return getSizeString(Math.max(0, mhost.getTrafficLeft()));
                }
            }

            @Override
            protected final String getSizeString(final long fileSize) {
                return maxSizeUnit.formatValue(maxSizeUnit, formatter, fileSize);
            }

            @Override
            protected long getBytes(MultiHostHost mhost) {
                /* Do not allow for negative numbers since table progress would display this as unlimited. */
                return Math.max(0, mhost.getTrafficLeft());
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }

            @Override
            public boolean isEnabled(final MultiHostHost mhost) {
                return mhost.isEnabled();
            }
        };
    }

    private ExtFileSizeColumn<MultiHostHost> createTrafficMaxColumn(final DecimalFormat formatter) {
        return new ExtFileSizeColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_traffic_max()) {
            private final SIZEUNIT maxSizeUnit = JsonConfig.create(GraphicalUserInterfaceSettings.class).getMaxSizeUnit();

            @Override
            public String getStringValue(MultiHostHost mhost) {
                if (mhost.isUnlimitedTraffic()) {
                    return _GUI.T.lit_unlimited();
                } else {
                    return getSizeString(mhost.getTrafficMax());
                }
            }

            @Override
            protected final String getSizeString(final long fileSize) {
                return maxSizeUnit.formatValue(maxSizeUnit, formatter, fileSize);
            }

            @Override
            protected long getBytes(MultiHostHost val) {
                if (val.getTrafficMax() > 0) {
                    return val.getTrafficMax();
                } else {
                    /* Fallback */
                    return val.getTrafficLeft();
                }
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }

            @Override
            public boolean isEnabled(final MultiHostHost mhost) {
                return mhost.isEnabled();
            }
        };
    }

    private ExtLongColumn<MultiHostHost> createTrafficCalculationFactorColumn() {
        return new ExtLongColumn<MultiHostHost>(_GUI.T.multihost_detailed_host_info_table_column_traffic_calculation_factor_percent()) {
            @Override
            protected long getLong(MultiHostHost mhost) {
                return mhost.getTrafficCalculationFactorPercent();
            }

            @Override
            protected String getLongFormatted(MultiHostHost mhost) {
                return getLong(mhost) + "%";
            }

            @Override
            public boolean isEnabled(final MultiHostHost mhost) {
                return mhost.isEnabled();
            }
        };
    }

    private ExtTextColumn<MultiHostHost> createCreatedColumn() {
        return new ExtTextColumn<MultiHostHost>("Last updated timestamp") {
            @Override
            public String getStringValue(final MultiHostHost mhost) {
                return new ElapsedTimeFormatter().setUseNaturalLanguage(true).formatTimestamp(mhost.getUpdateTimestamp());
            }

            @Override
            protected String getTooltipText(final MultiHostHost mhost) {
                final long timestamp = mhost.getUpdateTimestamp();
                if (timestamp <= 0) {
                    return null;
                }
                final String formattedDate = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date(timestamp));
                return formattedDate;
            }

            @Override
            public boolean isEnabled(final MultiHostHost mhost) {
                return mhost.isEnabled();
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }
        };
    }
}