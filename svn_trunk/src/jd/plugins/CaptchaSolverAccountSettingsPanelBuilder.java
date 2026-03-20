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
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.captcha.v2.CaptchaHistoryEntry;
import org.jdownloader.captcha.v2.CaptchaHistoryManager;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public class CaptchaSolverAccountSettingsPanelBuilder {
    private final CaptchaTypeAccessor accessor;
    private final List<CAPTCHA_TYPE>  captchaTypes;
    private final boolean             shouldShowDemoUrlColumn;
    private final boolean             shouldShowJDownloaderSupportedColumn;
    private final boolean             shouldShowUsedServicesColumn;
    private int                       numberofNonJDSupportedCaptchaTypes = 0;

    public CaptchaSolverAccountSettingsPanelBuilder(final CaptchaTypeAccessor accessor) {
        if (accessor == null) {
            throw new IllegalArgumentException("accessor must not be null");
        }
        this.accessor = accessor;
        final List<CAPTCHA_TYPE> ctypes = CaptchaType.getProcessableCaptchaTypes();
        this.captchaTypes = ctypes;
        // Determine which columns should be visible by default
        boolean showDemoUrl = false;
        boolean showJDownloaderSupported = false;
        boolean showUsedServicesColumn = false;
        for (final CAPTCHA_TYPE ctype : captchaTypes) {
            final List<CaptchaHistoryEntry> entries = CaptchaHistoryManager.getInstance().getEntriesByCaptchaType(ctype);
            if (!showDemoUrl && ctype.getDemoUrl() != null) {
                showDemoUrl = true;
            }
            if (!showJDownloaderSupported && !ctype.isJDownloaderSupported()) {
                showJDownloaderSupported = true;
            }
            if (!showUsedServicesColumn && entries != null && !entries.isEmpty()) {
                showUsedServicesColumn = true;
            }
            if (!ctype.isJDownloaderSupported()) {
                numberofNonJDSupportedCaptchaTypes += 1;
            }
        }
        this.shouldShowDemoUrlColumn = showDemoUrl;
        this.shouldShowJDownloaderSupportedColumn = showJDownloaderSupported;
        this.shouldShowUsedServicesColumn = showUsedServicesColumn;
    }

    public List<CAPTCHA_TYPE> getCaptchaTypes() {
        return captchaTypes;
    }

    public void build(final PluginConfigPanelNG panel) {
        final BasicJDTable<CAPTCHA_TYPE> table = this.getCaptchaTypesTable();
        final JScrollPane scrollPane = new JScrollPane(table);
        panel.add(scrollPane);
    }

    public final BasicJDTable<CAPTCHA_TYPE> getCaptchaTypesTable() {
        final ExtTableModel<CAPTCHA_TYPE> tableModel = createTableModel();
        tableModel._fireTableStructureChanged(captchaTypes, false);
        final BasicJDTable<CAPTCHA_TYPE> table = new BasicJDTable<CAPTCHA_TYPE>(tableModel);
        table.setPreferredScrollableViewportSize(new Dimension(table.getPreferredSize().width, table.getRowHeight() * table.getRowCount()));
        table.setSearchEnabled(true);
        return table;
    }

    public ExtTableModel<CAPTCHA_TYPE> createTableModel() {
        return new ExtTableModel<CAPTCHA_TYPE>("CaptchaTypeTable") {
            @Override
            protected void initColumns() {
                addColumn(createEnabledColumn());
                addColumn(createNameColumn());
                addColumn(createSupportedColumn());
                if (numberofNonJDSupportedCaptchaTypes > 0) {
                    addColumn(createSupportedByJDownloaderColumn());
                }
                addColumn(createDomainColumn());
                addColumn(createDescriptionColumn());
                addColumn(createLastUsedColumn());
                addColumn(createUsedForServicesColumn());
                addColumn(createDemoUrlColumn());
            }

            @Override
            public void init(final String id) {
                super.init(id);
                ExtColumn<CAPTCHA_TYPE> lastUsedColumn = null;
                for (final ExtColumn<CAPTCHA_TYPE> column : getColumns()) {
                    if ("Last Used by you".equals(column.getName())) {
                        lastUsedColumn = column;
                        break;
                    }
                }
                if (lastUsedColumn != null) {
                    this.sort(CaptchaSolverAccountSettingsPanelBuilder.this.captchaTypes, lastUsedColumn);
                }
            }
        };
    }

    private ExtCheckColumn<CAPTCHA_TYPE> createEnabledColumn() {
        return new ExtCheckColumn<CAPTCHA_TYPE>(_GUI.T.premiumaccounttablemodel_column_enabled()) {
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
            protected boolean getBooleanValue(final CAPTCHA_TYPE ctype) {
                return accessor.isEnabled(ctype);
            }

            @Override
            public boolean isEditable(final CAPTCHA_TYPE ctype) {
                return ctype.isJDownloaderSupported();
            }

            @Override
            protected void setBooleanValue(final boolean enabled, final CAPTCHA_TYPE ctype) {
                accessor.setEnabled(ctype, enabled);
                getModel().fireTableDataChanged();
            }
        };
    }

    private ExtTextColumn<CAPTCHA_TYPE> createNameColumn() {
        return new ExtTextColumn<CAPTCHA_TYPE>("Name") {
            {
                setRowSorter(new ExtDefaultRowSorter<CAPTCHA_TYPE>() {
                    @Override
                    public int compare(final CAPTCHA_TYPE o1, final CAPTCHA_TYPE o2) {
                        final String v1 = o1.getDisplayName();
                        final String v2 = o2.getDisplayName();
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
            public String getStringValue(final CAPTCHA_TYPE ctype) {
                return ctype.getDisplayName();
            }

            @Override
            public Icon getIcon(final CAPTCHA_TYPE ctype) {
                return ctype.getIcon();
            }
        };
    }

    private ExtTextColumn<CAPTCHA_TYPE> createDomainColumn() {
        return new ExtTextColumn<CAPTCHA_TYPE>(_GUI.T.multihost_detailed_host_info_table_column_domain()) {
            @Override
            public String getStringValue(final CAPTCHA_TYPE ctype) {
                final String domain = ctype.getDomain();
                return domain != null ? domain : "";
            }

            @Override
            protected String getTooltipText(final CAPTCHA_TYPE ctype) {
                return ctype.getDomain();
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }
        };
    }

    private ExtTextColumn<CAPTCHA_TYPE> createDescriptionColumn() {
        return new ExtTextColumn<CAPTCHA_TYPE>("Description") {
            @Override
            public String getStringValue(final CAPTCHA_TYPE ctype) {
                final String description = ctype.getDescription();
                return description != null ? description : "";
            }

            @Override
            protected String getTooltipText(final CAPTCHA_TYPE ctype) {
                return ctype.getDescription();
            }
        };
    }

    private ExtTextColumn<CAPTCHA_TYPE> createSupportedColumn() {
        final Icon icon_okay = NewTheme.I().getIcon(IconKey.ICON_OK, 16);
        final Icon icon_error = NewTheme.I().getIcon(IconKey.ICON_ERROR, 16);
        return new ExtTextColumn<CAPTCHA_TYPE>("Supported by this service") {
            @Override
            public String getStringValue(final CAPTCHA_TYPE ctype) {
                return accessor.isSupported(ctype) ? "Yes" : "No";
            }

            @Override
            public Icon getIcon(final CAPTCHA_TYPE ctype) {
                return accessor.isSupported(ctype) ? icon_okay : icon_error;
            }

            @Override
            protected String getTooltipText(final CAPTCHA_TYPE ctype) {
                if (accessor.isSupported(ctype)) {
                    return "This captcha type is supported by this service";
                } else {
                    return "This captcha type is NOT supported by this service";
                }
            }
        };
    }

    private ExtTextColumn<CAPTCHA_TYPE> createLastUsedColumn() {
        return new ExtTextColumn<CAPTCHA_TYPE>("Last Used by you") {
            {
                setRowSorter(new ExtDefaultRowSorter<CAPTCHA_TYPE>() {
                    @Override
                    public int compare(final CAPTCHA_TYPE o1, final CAPTCHA_TYPE o2) {
                        final long v1 = getTimestamp(o1);
                        final long v2 = getTimestamp(o2);
                        if (v1 == v2) {
                            final String name1 = o1.getDisplayName();
                            final String name2 = o2.getDisplayName();
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
                            if (v1 == 0 && v2 > 0) {
                                return 1;
                            }
                            if (v2 == 0 && v1 > 0) {
                                return -1;
                            }
                            return v1 > v2 ? -1 : 1;
                        } else {
                            if (v1 == 0 && v2 > 0) {
                                return -1;
                            }
                            if (v2 == 0 && v1 > 0) {
                                return 1;
                            }
                            return v2 > v1 ? -1 : 1;
                        }
                    }

                    private long getTimestamp(final CAPTCHA_TYPE ctype) {
                        final CaptchaHistoryEntry entry = CaptchaHistoryManager.getInstance().getLastUsedTimestampByCaptchaType(ctype);
                        return entry != null ? entry.getTimestamp() : 0;
                    }
                });
            }

            @Override
            public String getStringValue(final CAPTCHA_TYPE ctype) {
                final CaptchaHistoryEntry lastEntry = CaptchaHistoryManager.getInstance().getLastUsedTimestampByCaptchaType(ctype);
                if (lastEntry == null) {
                    return "never";
                }
                return new ElapsedTimeFormatter().setUseNaturalLanguage(true).formatTimestamp(lastEntry.getTimestamp());
            }

            @Override
            protected String getTooltipText(final CAPTCHA_TYPE ctype) {
                final CaptchaHistoryEntry lastEntry = CaptchaHistoryManager.getInstance().getLastUsedTimestampByCaptchaType(ctype);
                if (lastEntry == null) {
                    return "This captcha type has never been used";
                }
                final String formattedDate = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date(lastEntry.getTimestamp()));
                return "Last used at: " + formattedDate + " via " + lastEntry.getDomain();
            }
        };
    }

    private ExtTextColumn<CAPTCHA_TYPE> createUsedForServicesColumn() {
        return new ExtTextColumn<CAPTCHA_TYPE>("Used by you for services") {
            @Override
            public String getStringValue(final CAPTCHA_TYPE ctype) {
                final List<CaptchaHistoryEntry> entries = CaptchaHistoryManager.getInstance().getEntriesByCaptchaType(ctype);
                final String text_none = "none";
                if (entries == null || entries.isEmpty()) {
                    return text_none;
                }
                final List<String> domains = new ArrayList<String>();
                for (final CaptchaHistoryEntry entry : entries) {
                    final String domain = entry.getDomain();
                    if (!domains.contains(domain)) {
                        domains.add(domain);
                    }
                }
                if (domains.isEmpty()) {
                    return text_none;
                }
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
            protected String getTooltipText(final CAPTCHA_TYPE ctype) {
                final List<CaptchaHistoryEntry> entries = CaptchaHistoryManager.getInstance().getEntriesByCaptchaType(ctype);
                if (entries == null || entries.isEmpty()) {
                    return "This captcha type has never been used";
                }
                final List<String> domains = new ArrayList<String>();
                for (final CaptchaHistoryEntry entry : entries) {
                    final String domain = entry.getDomain();
                    if (!domains.contains(domain)) {
                        domains.add(domain);
                    }
                }
                if (domains.isEmpty()) {
                    return "";
                }
                final StringBuilder sb = new StringBuilder("Used for: ");
                for (int i = 0; i < domains.size(); i++) {
                    if (i > 0) {
                        sb.append(", ");
                    }
                    sb.append(domains.get(i));
                }
                return sb.toString();
            }

            @Override
            public boolean isDefaultVisible() {
                return shouldShowUsedServicesColumn;
            }
        };
    }

    private ExtTextColumn<CAPTCHA_TYPE> createDemoUrlColumn() {
        return new ExtTextColumn<CAPTCHA_TYPE>("Demo URL") {
            @Override
            public String getStringValue(final CAPTCHA_TYPE ctype) {
                final String demoUrl = ctype.getDemoUrl();
                return demoUrl != null ? demoUrl : "";
            }

            @Override
            protected String getTooltipText(final CAPTCHA_TYPE ctype) {
                return ctype.getDemoUrl();
            }

            @Override
            public boolean onDoubleClick(final MouseEvent e, final CAPTCHA_TYPE ctype) {
                final String demoUrl = ctype.getDemoUrl();
                if (!StringUtils.isEmpty(demoUrl) && CrossSystem.isOpenBrowserSupported()) {
                    CrossSystem.openURL(demoUrl);
                    return true;
                }
                return false;
            }

            @Override
            public boolean isDefaultVisible() {
                return shouldShowDemoUrlColumn;
            }
        };
    }

    private ExtTextColumn<CAPTCHA_TYPE> createSupportedByJDownloaderColumn() {
        final Icon icon_okay = NewTheme.I().getIcon(IconKey.ICON_OK, 16);
        final Icon icon_error = NewTheme.I().getIcon(IconKey.ICON_ERROR, 16);
        return new ExtTextColumn<CAPTCHA_TYPE>("Supported by JD") {
            @Override
            public String getStringValue(final CAPTCHA_TYPE ctype) {
                return ctype.isJDownloaderSupported() ? "Yes" : "No";
            }

            @Override
            public Icon getIcon(final CAPTCHA_TYPE ctype) {
                return ctype.isJDownloaderSupported() ? icon_okay : icon_error;
            }

            @Override
            protected String getTooltipText(final CAPTCHA_TYPE ctype) {
                if (ctype.isJDownloaderSupported()) {
                    return "This captcha type is supported by JDownloader";
                } else {
                    return "This captcha type is NOT supported by JDownloader";
                }
            }

            @Override
            public boolean isDefaultVisible() {
                return shouldShowJDownloaderSupportedColumn;
            }
        };
    }

    public interface CaptchaTypeAccessor {
        boolean isEnabled(CAPTCHA_TYPE ctype);

        void setEnabled(CAPTCHA_TYPE ctype, boolean enabled);

        boolean isSupported(CAPTCHA_TYPE ctype);
    }

    public static class AccountCaptchaTypeAccessor implements CaptchaTypeAccessor {
        private final Account account;

        public AccountCaptchaTypeAccessor(final Account account) {
            if (account == null) {
                throw new IllegalArgumentException("account must not be null");
            }
            this.account = account;
        }

        private static String getEnabledPropertyKey(final CAPTCHA_TYPE ctype) {
            return "captcha_type_enabled_" + ctype;
        }

        @Override
        public boolean isEnabled(final CAPTCHA_TYPE ctype) {
            return account.getBooleanProperty(getEnabledPropertyKey(ctype), true);
        }

        @Override
        public void setEnabled(final CAPTCHA_TYPE ctype, final boolean enabled) {
            account.setProperty(getEnabledPropertyKey(ctype), enabled);
        }

        @Override
        public boolean isSupported(final CAPTCHA_TYPE ctype) {
            final PluginForHost plg = account.getPlugin();
            if (!(plg instanceof abstractPluginForCaptchaSolver)) {
                return false;
            }
            final List<CAPTCHA_TYPE> supportedTypes = ((abstractPluginForCaptchaSolver) plg).getSupportedCaptchaTypes(account);
            if (supportedTypes == null) {
                return false;
            }
            return supportedTypes.contains(ctype);
        }
    }

    public static class SolverServiceCaptchaTypeAccessor implements CaptchaTypeAccessor {
        private final SolverService solver;

        public SolverServiceCaptchaTypeAccessor(final SolverService solver) {
            this.solver = solver;
        }

        @Override
        public boolean isEnabled(final CAPTCHA_TYPE ctype) {
            return solver.isEnabled();
        }

        @Override
        public void setEnabled(final CAPTCHA_TYPE ctype, final boolean enabled) {
            solver.setEnabled(enabled);
        }

        @Override
        public boolean isSupported(final CAPTCHA_TYPE ctype) {
            // TODO: implement per-type support check
            return true;
        }
    }
}