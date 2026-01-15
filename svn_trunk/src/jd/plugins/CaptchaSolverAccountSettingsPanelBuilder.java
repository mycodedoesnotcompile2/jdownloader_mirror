package jd.plugins;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.MouseEvent;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.swing.Icon;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;

import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableHeaderRenderer;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.DebugMode;
import org.jdownloader.DomainInfo;
import org.jdownloader.captcha.v2.CaptchaHistoryEntry;
import org.jdownloader.captcha.v2.CaptchaHistoryManager;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

/**
 * Builds the captcha solver account settings panel UI. Takes same parameters as extendMultiHostAccountSettingsPanel and creates the table.
 *
 * Separates panel building logic from PluginForHost to keep that class smaller.
 */
public class CaptchaSolverAccountSettingsPanelBuilder {
    private final Account           account;
    private final List<CaptchaType> captchaTypes;
    private final boolean           shouldShowDomainColumn;
    private final boolean           shouldShowDemoUrlColumn;
    private final boolean           shouldShowJDownloaderSupportedColumn;
    private final boolean           shouldShowAccountSupportedColumn;
    private final boolean           shouldShowUsedServicesColumn;

    /**
     * Creates a new panel builder for a captcha solver account. Same parameters as extendMultiHostAccountSettingsPanel.
     *
     * @param account
     *            The account to build the panel for
     */
    public CaptchaSolverAccountSettingsPanelBuilder(final Account account) {
        final AccountInfo ai = account.getAccountInfo();
        final List<CaptchaType> ctypes = new ArrayList<CaptchaType>();
        for (final CAPTCHA_TYPE ctype_static : CAPTCHA_TYPE.values()) {
            final CaptchaType ctype = new CaptchaType(ctype_static);
            ctype.setAccountInfo(ai);
            ctypes.add(ctype);
        }
        this.account = account;
        this.captchaTypes = ctypes;
        // Determine which columns should be visible
        boolean showDomain = false;
        boolean showDemoUrl = false;
        boolean showJDownloaderSupported = false;
        boolean shouldShowUsedServicesColumn = false;
        for (final CaptchaType ctype : captchaTypes) {
            final CAPTCHA_TYPE typeStatic = ctype.getCAPTCHA_TYPE_STATIC();
            final List<CaptchaHistoryEntry> entries = CaptchaHistoryManager.getInstance().getEntriesByCaptchaType(typeStatic);
            if (!showDomain && typeStatic.getDomain() != null) {
                showDomain = true;
            }
            if (!showDemoUrl && typeStatic.getDemoUrl() != null) {
                showDemoUrl = true;
            }
            if (!showJDownloaderSupported && !typeStatic.isJDownloaderSupported()) {
                showJDownloaderSupported = true;
            }
            if (!shouldShowUsedServicesColumn && entries != null && !entries.isEmpty()) {
                shouldShowUsedServicesColumn = true;
            }
            if (showDomain && showDemoUrl && showJDownloaderSupported && shouldShowUsedServicesColumn) {
                /* Early-abort loop in case all default disabled columns shall be enabled. */
                break;
            }
        }
        this.shouldShowDomainColumn = showDomain;
        this.shouldShowDemoUrlColumn = showDemoUrl;
        this.shouldShowJDownloaderSupportedColumn = showJDownloaderSupported;
        this.shouldShowAccountSupportedColumn = this.account != null;
        this.shouldShowUsedServicesColumn = shouldShowUsedServicesColumn;
    }

    /**
     * Builds and adds the panel to the given container
     *
     * @param panel
     *            The panel to add the table to
     */
    public void build(final PluginConfigPanelNG panel) {
        final ExtTableModel<CaptchaType> tableModel = createTableModel();
        tableModel._fireTableStructureChanged(captchaTypes, false);
        final BasicJDTable<CaptchaType> table = new BasicJDTable<CaptchaType>(tableModel);
        table.setPreferredScrollableViewportSize(new Dimension(table.getPreferredSize().width, table.getRowHeight() * table.getRowCount()));
        table.setSearchEnabled(true);
        final JScrollPane scrollPane = new JScrollPane(table);
        panel.add(scrollPane);
    }

    /**
     * Creates the table model with all columns
     */
    private ExtTableModel<CaptchaType> createTableModel() {
        return new ExtTableModel<CaptchaType>("CaptchaTypeTable") {
            @Override
            protected void initColumns() {
                // Enabled column
                addColumn(createEnabledColumn());
                // Name column
                addColumn(createNameColumn());
                // Supported by this account column
                if (shouldShowAccountSupportedColumn) {
                    addColumn(createSupportedColumn());
                }
                // Domain column (debug only)
                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && shouldShowDomainColumn) {
                    addColumn(createDomainColumn());
                }
                // Description column
                addColumn(createDescriptionColumn());
                // Last Used column
                addColumn(createLastUsedColumn());
                // Used for services column
                if (shouldShowUsedServicesColumn) {
                    addColumn(createUsedForServicesColumn());
                }
                // Demo URL column
                if (shouldShowDemoUrlColumn) {
                    addColumn(createDemoUrlColumn());
                }
                // JDownloader Support column (debug only)
                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE && shouldShowJDownloaderSupportedColumn) {
                    addColumn(createJDownloaderSupportedColumn());
                }
            }

            @Override
            public void init(final String id) {
                super.init(id);
                // Sort by "Last Used" (descending - newest first) with secondary sort by "Name" (alphabetical)
                // The secondary sort is handled in the RowSorter of createLastUsedColumn()
                ExtColumn<CaptchaType> lastUsedColumn = null;
                for (final ExtColumn<CaptchaType> column : getColumns()) {
                    if ("Last Used".equals(column.getName())) {
                        lastUsedColumn = column;
                        break;
                    }
                }
                // Set primary sort by Last Used (descending - newest first)
                if (lastUsedColumn != null) {
                    this.sort(CaptchaSolverAccountSettingsPanelBuilder.this.captchaTypes, lastUsedColumn);
                }
            }
        };
    }

    /**
     * Creates the "Enabled" checkbox column
     */
    private ExtCheckColumn<CaptchaType> createEnabledColumn() {
        return new ExtCheckColumn<CaptchaType>(_GUI.T.premiumaccounttablemodel_column_enabled()) {
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
            protected boolean getBooleanValue(final CaptchaType captchaType) {
                return captchaType.isEnabled();
            }

            @Override
            public boolean isEditable(CaptchaType captchaType) {
                return captchaType.getAccount() != null && captchaType.getCAPTCHA_TYPE_STATIC().isJDownloaderSupported();
            }

            @Override
            protected void setBooleanValue(final boolean enabled, final CaptchaType captchaType) {
                captchaType.setEnabled(enabled);
                getModel().fireTableDataChanged();
            }
        };
    }

    /**
     * Creates the "Name" column with domain favicon
     */
    private ExtTextColumn<CaptchaType> createNameColumn() {
        return new ExtTextColumn<CaptchaType>("Name") {
            {
                setRowSorter(new ExtDefaultRowSorter<CaptchaType>() {
                    @Override
                    public int compare(final CaptchaType o1, final CaptchaType o2) {
                        final String v1 = o1.getCAPTCHA_TYPE_STATIC().getDisplayName();
                        final String v2 = o2.getCAPTCHA_TYPE_STATIC().getDisplayName();
                        if (v1 == null && v2 == null) {
                            return 0;
                        }
                        if (v1 == null) {
                            return 1;
                        }
                        if (v2 == null) {
                            return -1;
                        }
                        final int compareResult = v1.compareTo(v2);
                        if (this.getSortOrderIdentifier() != ExtColumn.SORT_ASC) {
                            return compareResult * -1;
                        } else {
                            return compareResult;
                        }
                    }
                });
            }

            @Override
            public String getStringValue(final CaptchaType captchaType) {
                return captchaType.getCAPTCHA_TYPE_STATIC().getDisplayName();
            }

            @Override
            public Icon getIcon(CaptchaType captchaType) {
                final String domain = captchaType.getCAPTCHA_TYPE_STATIC().getDomain();
                if (domain != null) {
                    return DomainInfo.getInstance(domain).getFavIcon();
                } else {
                    return NewTheme.I().getIcon(IconKey.ICON_BEER, 16);
                }
            }
        };
    }

    /**
     * Creates the "Domain" column (debug only)
     */
    private ExtTextColumn<CaptchaType> createDomainColumn() {
        return new ExtTextColumn<CaptchaType>(_GUI.T.multihost_detailed_host_info_table_column_domain()) {
            @Override
            public String getStringValue(final CaptchaType captchaType) {
                final String domain = captchaType.getCAPTCHA_TYPE_STATIC().getDomain();
                return domain != null ? domain : "";
            }

            @Override
            protected String getTooltipText(final CaptchaType captchaType) {
                final String domain = captchaType.getCAPTCHA_TYPE_STATIC().getDomain();
                return domain != null ? domain : null;
            }
        };
    }

    /**
     * Creates the "Description" column
     */
    private ExtTextColumn<CaptchaType> createDescriptionColumn() {
        return new ExtTextColumn<CaptchaType>("Description") {
            @Override
            public String getStringValue(final CaptchaType captchaType) {
                final String description = captchaType.getCAPTCHA_TYPE_STATIC().getDescription();
                return description != null ? description : "";
            }

            @Override
            protected String getTooltipText(final CaptchaType captchaType) {
                final String description = captchaType.getCAPTCHA_TYPE_STATIC().getDescription();
                return description != null ? description : null;
            }
        };
    }

    /**
     * Creates the "Supported by this account" column
     */
    private ExtTextColumn<CaptchaType> createSupportedColumn() {
        final Icon icon_okay = NewTheme.I().getIcon(IconKey.ICON_OK, 16);
        final Icon icon_error = NewTheme.I().getIcon(IconKey.ICON_ERROR, 16);
        return new ExtTextColumn<CaptchaType>("Supported by this account") {
            @Override
            public String getStringValue(final CaptchaType captchaType) {
                return captchaType.isSupported() ? "Yes" : "No";
            }

            @Override
            public Icon getIcon(CaptchaType captchaType) {
                return captchaType.isSupported() ? icon_okay : icon_error;
            }

            @Override
            protected String getTooltipText(final CaptchaType captchaType) {
                if (captchaType.isSupported()) {
                    return "This captcha type is supported by this account";
                } else {
                    return "This captcha type is NOT supported by this account";
                }
            }
        };
    }

    /**
     * Creates the "Last Used" column using CaptchaHistoryManager
     */
    private ExtTextColumn<CaptchaType> createLastUsedColumn() {
        return new ExtTextColumn<CaptchaType>("Last Used") {
            {
                setRowSorter(new ExtDefaultRowSorter<CaptchaType>() {
                    @Override
                    public int compare(final CaptchaType o1, final CaptchaType o2) {
                        final long v1 = getTimestamp(o1);
                        final long v2 = getTimestamp(o2);
                        if (v1 == v2) {
                            // Secondary sort: by Name (alphabetical)
                            final String name1 = o1.getCAPTCHA_TYPE_STATIC().getDisplayName();
                            final String name2 = o2.getCAPTCHA_TYPE_STATIC().getDisplayName();
                            if (name1 == null && name2 == null) {
                                return 0;
                            }
                            if (name1 == null) {
                                return 1;
                            }
                            if (name2 == null) {
                                return -1;
                            }
                            return name1.compareTo(name2);
                        }
                        if (this.getSortOrderIdentifier() != ExtColumn.SORT_ASC) {
                            // Descending: newest first
                            if (v1 == 0 && v2 > 0) {
                                return 1; // "never" goes to end
                            }
                            if (v2 == 0 && v1 > 0) {
                                return -1;
                            }
                            return v1 > v2 ? -1 : 1;
                        } else {
                            // Ascending: oldest first
                            if (v1 == 0 && v2 > 0) {
                                return -1;
                            }
                            if (v2 == 0 && v1 > 0) {
                                return 1;
                            }
                            return v2 > v1 ? -1 : 1;
                        }
                    }

                    private long getTimestamp(CaptchaType ct) {
                        final CaptchaHistoryEntry entry = CaptchaHistoryManager.getInstance().getLastUsedTimestampByCaptchaType(ct.getCAPTCHA_TYPE_STATIC());
                        return entry != null ? entry.getTimestamp() : 0;
                    }
                });
            }

            @Override
            public String getStringValue(final CaptchaType captchaType) {
                final CAPTCHA_TYPE typeStatic = captchaType.getCAPTCHA_TYPE_STATIC();
                final CaptchaHistoryEntry lastEntry = CaptchaHistoryManager.getInstance().getLastUsedTimestampByCaptchaType(typeStatic);
                if (lastEntry == null) {
                    return "never";
                }
                final long currentTime = System.currentTimeMillis();
                final long elapsedMillis = currentTime - lastEntry.getTimestamp();
                return formatElapsedTime(elapsedMillis);
            }

            @Override
            protected String getTooltipText(final CaptchaType captchaType) {
                final CAPTCHA_TYPE typeStatic = captchaType.getCAPTCHA_TYPE_STATIC();
                final CaptchaHistoryEntry lastEntry = CaptchaHistoryManager.getInstance().getLastUsedTimestampByCaptchaType(typeStatic);
                if (lastEntry == null) {
                    return "This captcha type has never been used";
                }
                final String formattedDate = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date(lastEntry.getTimestamp()));
                return "Last used at: " + formattedDate + " via " + lastEntry.getDomain();
            }
        };
    }

    /**
     * Creates the "Demo URL" column
     */
    private ExtTextColumn<CaptchaType> createUsedForServicesColumn() {
        return new ExtTextColumn<CaptchaType>("Used for services") {
            @Override
            public String getStringValue(final CaptchaType captchaType) {
                final CAPTCHA_TYPE typeStatic = captchaType.getCAPTCHA_TYPE_STATIC();
                final List<CaptchaHistoryEntry> entries = CaptchaHistoryManager.getInstance().getEntriesByCaptchaType(typeStatic);
                if (entries == null || entries.isEmpty()) {
                    return "none";
                }
                // Collect unique domains in order of last usage (entries are already sorted)
                final List<String> domains = new ArrayList<String>();
                for (final CaptchaHistoryEntry entry : entries) {
                    final String domain = entry.getDomain();
                    if (!domains.contains(domain)) {
                        domains.add(domain);
                    }
                }
                if (domains.isEmpty()) {
                    return "none";
                }
                // Join domains with comma separator
                final StringBuilder sb = new StringBuilder();
                for (final String domain : domains) {
                    if (sb.length() > 0) {
                        sb.append(", ");
                    }
                    sb.append(domain);
                }
                return sb.toString();
            }

            @Override
            protected String getTooltipText(final CaptchaType captchaType) {
                final CAPTCHA_TYPE typeStatic = captchaType.getCAPTCHA_TYPE_STATIC();
                final List<CaptchaHistoryEntry> entries = CaptchaHistoryManager.getInstance().getEntriesByCaptchaType(typeStatic);
                if (entries == null || entries.isEmpty()) {
                    return "This captcha type has never been used";
                }
                // Collect unique domains in order of last usage (entries are already sorted)
                final List<String> domains = new ArrayList<String>();
                for (final CaptchaHistoryEntry entry : entries) {
                    final String domain = entry.getDomain();
                    if (domain != null && !domain.isEmpty() && !domains.contains(domain)) {
                        domains.add(domain);
                    }
                }
                if (domains.isEmpty()) {
                    return "This captcha type has never been used on any service";
                }
                final StringBuilder sb = new StringBuilder("Used on: ");
                for (int i = 0; i < domains.size(); i++) {
                    if (i > 0) {
                        sb.append(", ");
                    }
                    sb.append(domains.get(i));
                }
                return sb.toString();
            }
        };
    }

    /**
     * Creates the "Demo URL" column
     */
    private ExtTextColumn<CaptchaType> createDemoUrlColumn() {
        return new ExtTextColumn<CaptchaType>("Demo URL") {
            @Override
            public String getStringValue(final CaptchaType captchaType) {
                final String demoUrl = captchaType.getCAPTCHA_TYPE_STATIC().getDemoUrl();
                return demoUrl != null ? demoUrl : "";
            }

            @Override
            protected String getTooltipText(final CaptchaType captchaType) {
                final String demoUrl = captchaType.getCAPTCHA_TYPE_STATIC().getDemoUrl();
                return demoUrl != null ? demoUrl : null;
            }

            @Override
            public boolean onDoubleClick(MouseEvent e, CaptchaType captchaType) {
                final String demoUrl = captchaType.getCAPTCHA_TYPE_STATIC().getDemoUrl();
                if (demoUrl != null && !demoUrl.isEmpty()) {
                    // TODO: URL öffnen
                }
                return false;
            }
        };
    }

    /**
     * Creates the "JDownloader Support" column (debug only) with icons and text
     */
    private ExtTextColumn<CaptchaType> createJDownloaderSupportedColumn() {
        final Icon icon_okay = NewTheme.I().getIcon(IconKey.ICON_OK, 16);
        final Icon icon_error = NewTheme.I().getIcon(IconKey.ICON_ERROR, 16);
        return new ExtTextColumn<CaptchaType>("JDownloader Support") {
            @Override
            public String getStringValue(final CaptchaType captchaType) {
                return captchaType.getCAPTCHA_TYPE_STATIC().isJDownloaderSupported() ? "Yes" : "No";
            }

            @Override
            public Icon getIcon(CaptchaType captchaType) {
                return captchaType.getCAPTCHA_TYPE_STATIC().isJDownloaderSupported() ? icon_okay : icon_error;
            }

            @Override
            protected String getTooltipText(final CaptchaType captchaType) {
                if (captchaType.getCAPTCHA_TYPE_STATIC().isJDownloaderSupported()) {
                    return "This captcha type is supported by JDownloader";
                } else {
                    return "This captcha type is NOT supported by JDownloader";
                }
            }
        };
    }

    /**
     * Formats elapsed time in milliseconds to a human-readable string. Examples: "1 minute ago", "2 hours ago", "3 days ago", "never"
     *
     * @param elapsedMillis
     *            The elapsed time in milliseconds
     * @return A formatted string representing the elapsed time
     */
    private String formatElapsedTime(final long elapsedMillis) {
        if (elapsedMillis < 0) {
            return "never";
        }
        final long seconds = elapsedMillis / 1000;
        final long minutes = seconds / 60;
        final long hours = minutes / 60;
        final long days = hours / 24;
        final long weeks = days / 7;
        final long months = days / 30;
        final long years = days / 365;
        if (seconds < 60) {
            return seconds == 1 ? "1 second ago" : seconds + " seconds ago";
        } else if (minutes < 60) {
            return minutes == 1 ? "1 minute ago" : minutes + " minutes ago";
        } else if (hours < 24) {
            return hours == 1 ? "1 hour ago" : hours + " hours ago";
        } else if (days < 7) {
            return days == 1 ? "1 day ago" : days + " days ago";
        } else if (weeks < 4) {
            return weeks == 1 ? "1 week ago" : weeks + " weeks ago";
        } else if (months < 12) {
            return months == 1 ? "1 month ago" : months + " months ago";
        } else {
            return years == 1 ? "1 year ago" : years + " years ago";
        }
    }
}