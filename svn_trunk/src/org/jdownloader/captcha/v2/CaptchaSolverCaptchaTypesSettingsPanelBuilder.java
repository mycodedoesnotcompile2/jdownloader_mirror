package org.jdownloader.captcha.v2;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;

import org.appwork.swing.MigPanel;
import org.appwork.swing.exttable.ExtColumn;
import org.appwork.swing.exttable.ExtDefaultRowSorter;
import org.appwork.swing.exttable.ExtTableHeaderRenderer;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtComponentColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.DebugMode;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.components.captchasolver.abstractPluginForCaptchaSolver;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.plugins.Account;
import jd.plugins.CaptchaType;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.ElapsedTimeFormatter;
import jd.plugins.PluginConfigPanelNG;
import jd.plugins.PluginForHost;

/**
 * Builds a table that lists all processable captcha types and lets the user enable or disable each type. The concrete enable/disable target
 * depends on the {@link CaptchaTypeAccessor} the builder is constructed with: {@link AccountCaptchaTypeAccessor} toggles the types per
 * account (stored as account properties), while {@link SolverServiceCaptchaTypeAccessor} toggles the types per captcha solver (stored in
 * the solver's {@link CaptchaSolverConfigV3} disabled-set). Besides the enable checkbox the table offers several informational columns
 * (support status, usage history, etc.), most of which are hidden by default and can be enabled via the column header context menu.
 */
public class CaptchaSolverCaptchaTypesSettingsPanelBuilder {
    private final CaptchaTypeAccessor accessor;
    private final List<CAPTCHA_TYPE>  captchaTypes;
    private final boolean             shouldShowJDownloaderSupportedColumn;
    private int                       numberofNonJDSupportedCaptchaTypes = 0;

    public CaptchaSolverCaptchaTypesSettingsPanelBuilder(final CaptchaTypeAccessor accessor) {
        if (accessor == null) {
            throw new IllegalArgumentException("accessor must not be null");
        }
        this.accessor = accessor;
        final List<CAPTCHA_TYPE> ctypes = CaptchaType.getProcessableCaptchaTypes();
        this.captchaTypes = ctypes;
        // Determine which columns should be visible by default
        boolean showJDownloaderSupported = false;
        for (final CAPTCHA_TYPE ctype : captchaTypes) {
            if (!showJDownloaderSupported && !ctype.isJDownloaderSupported()) {
                showJDownloaderSupported = true;
            }
            if (!ctype.isJDownloaderSupported()) {
                numberofNonJDSupportedCaptchaTypes += 1;
            }
        }
        this.shouldShowJDownloaderSupportedColumn = showJDownloaderSupported;
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
                addColumn(createEverNeededColumn());
                if (numberofNonJDSupportedCaptchaTypes > 0) {
                    addColumn(createSupportedByJDownloaderColumn());
                }
                addColumn(createDomainColumn());
                addColumn(createDescriptionColumn());
                addColumn(createLastUsedColumn());
                addColumn(createUsedForServicesColumn());
                addColumn(createNumberOfCaptchasColumn());
                addColumn(createDemoUrlColumn());
                if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                    addColumn(createTestCaptchaSolverColumn());
                }
            }

            @Override
            public void init(final String id) {
                super.init(id);
                ExtColumn<CAPTCHA_TYPE> lastUsedColumn = null;
                for (final ExtColumn<CAPTCHA_TYPE> column : getColumns()) {
                    if (_GUI.T.CaptchaTypesTable_column_lastUsed().equals(column.getName())) {
                        lastUsedColumn = column;
                        break;
                    }
                }
                if (lastUsedColumn != null) {
                    this.sort(CaptchaSolverCaptchaTypesSettingsPanelBuilder.this.captchaTypes, lastUsedColumn);
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
        return new ExtTextColumn<CAPTCHA_TYPE>(_GUI.T.CaptchaTypesTable_column_name()) {
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

            @Override
            protected String getTooltipText(final CAPTCHA_TYPE ctype) {
                /* The description column is hidden by default, so its text is offered here as the name's tooltip. */
                return ctype.getDescription();
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
        return new ExtTextColumn<CAPTCHA_TYPE>(_GUI.T.CaptchaTypesTable_column_description()) {
            @Override
            public String getStringValue(final CAPTCHA_TYPE ctype) {
                final String description = ctype.getDescription();
                return description != null ? description : "";
            }

            @Override
            protected String getTooltipText(final CAPTCHA_TYPE ctype) {
                return ctype.getDescription();
            }

            @Override
            public boolean isDefaultVisible() {
                /* Hidden by default; the description is shown as the tooltip of the name column instead. */
                return false;
            }
        };
    }

    private ExtTextColumn<CAPTCHA_TYPE> createSupportedColumn() {
        final Icon icon_okay = NewTheme.I().getIcon(IconKey.ICON_OK, 16);
        final Icon icon_error = NewTheme.I().getIcon(IconKey.ICON_ERROR, 16);
        return new ExtTextColumn<CAPTCHA_TYPE>(_GUI.T.CaptchaTypesTable_column_supportedBySolver()) {
            @Override
            public String getStringValue(final CAPTCHA_TYPE ctype) {
                return accessor.isSupported(ctype) ? _GUI.T.lit_yes() : _GUI.T.lit_no();
            }

            @Override
            public Icon getIcon(final CAPTCHA_TYPE ctype) {
                return accessor.isSupported(ctype) ? icon_okay : icon_error;
            }

            @Override
            protected String getTooltipText(final CAPTCHA_TYPE ctype) {
                if (accessor.isSupported(ctype)) {
                    return _GUI.T.CaptchaTypesTable_tooltip_supportedBySolver();
                } else {
                    return _GUI.T.CaptchaTypesTable_tooltip_notSupportedBySolver();
                }
            }
        };
    }

    /** "Ever used by you?" column: yes/no based on whether this captcha type was ever used by this JD instance. */
    private ExtTextColumn<CAPTCHA_TYPE> createEverNeededColumn() {
        final Icon icon_okay = NewTheme.I().getIcon(IconKey.ICON_OK, 16);
        final Icon icon_error = NewTheme.I().getIcon(IconKey.ICON_ERROR, 16);
        return new ExtTextColumn<CAPTCHA_TYPE>(_GUI.T.CaptchaTypesTable_column_everUsed()) {
            private boolean wasEverNeeded(final CAPTCHA_TYPE ctype) {
                final List<CaptchaHistoryEntry> entries = CaptchaHistoryManager.getInstance().getEntriesByCaptchaType(ctype);
                return entries != null && !entries.isEmpty();
            }

            @Override
            public String getStringValue(final CAPTCHA_TYPE ctype) {
                return wasEverNeeded(ctype) ? _GUI.T.lit_yes() : _GUI.T.lit_no();
            }

            @Override
            public Icon getIcon(final CAPTCHA_TYPE ctype) {
                return wasEverNeeded(ctype) ? icon_okay : icon_error;
            }

            @Override
            protected String getTooltipText(final CAPTCHA_TYPE ctype) {
                final CaptchaHistoryEntry lastEntry = CaptchaHistoryManager.getInstance().getLastUsedTimestampByCaptchaType(ctype);
                if (lastEntry == null) {
                    return _GUI.T.CaptchaTypesTable_tooltip_neverUsedByInstance();
                }
                final String elapsed = new ElapsedTimeFormatter().setUseNaturalLanguage(true).formatTimestamp(lastEntry.getTimestamp());
                return _GUI.T.CaptchaTypesTable_tooltip_lastUsedForService(elapsed, lastEntry.getDomain());
            }
        };
    }

    /** "number of captchas ever used" column: total captchas (download + login) used for this type by this JD instance. */
    private ExtTextColumn<CAPTCHA_TYPE> createNumberOfCaptchasColumn() {
        return new ExtTextColumn<CAPTCHA_TYPE>(_GUI.T.CaptchaTypesTable_column_numberOfCaptchas()) {
            {
                setRowSorter(new ExtDefaultRowSorter<CAPTCHA_TYPE>() {
                    @Override
                    public int compare(final CAPTCHA_TYPE o1, final CAPTCHA_TYPE o2) {
                        final int c1 = getTotalCount(o1);
                        final int c2 = getTotalCount(o2);
                        if (c1 == c2) {
                            return 0;
                        }
                        if (this.getSortOrderIdentifier() != ExtColumn.SORT_ASC) {
                            return c1 > c2 ? -1 : 1;
                        } else {
                            return c1 > c2 ? 1 : -1;
                        }
                    }
                });
            }

            private int getTotalCount(final CAPTCHA_TYPE ctype) {
                final List<CaptchaHistoryEntry> entries = CaptchaHistoryManager.getInstance().getEntriesByCaptchaType(ctype);
                return entries != null ? entries.size() : 0;
            }

            @Override
            public String getStringValue(final CAPTCHA_TYPE ctype) {
                return String.valueOf(getTotalCount(ctype));
            }

            @Override
            protected String getTooltipText(final CAPTCHA_TYPE ctype) {
                final List<CaptchaHistoryEntry> entries = CaptchaHistoryManager.getInstance().getEntriesByCaptchaType(ctype);
                int login = 0;
                int download = 0;
                if (entries != null) {
                    for (final CaptchaHistoryEntry entry : entries) {
                        if (entry.isLoginCaptcha()) {
                            login++;
                        } else {
                            download++;
                        }
                    }
                }
                return _GUI.T.CaptchaTypesTable_tooltip_captchaCounts(String.valueOf(download), String.valueOf(login));
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }
        };
    }

    private ExtTextColumn<CAPTCHA_TYPE> createLastUsedColumn() {
        return new ExtTextColumn<CAPTCHA_TYPE>(_GUI.T.CaptchaTypesTable_column_lastUsed()) {
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
                    return _GUI.T.lit_never();
                }
                return new ElapsedTimeFormatter().setUseNaturalLanguage(true).formatTimestamp(lastEntry.getTimestamp());
            }

            @Override
            protected String getTooltipText(final CAPTCHA_TYPE ctype) {
                final CaptchaHistoryEntry lastEntry = CaptchaHistoryManager.getInstance().getLastUsedTimestampByCaptchaType(ctype);
                if (lastEntry == null) {
                    return _GUI.T.CaptchaTypesTable_tooltip_neverUsed();
                }
                final String formattedDate = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date(lastEntry.getTimestamp()));
                return _GUI.T.CaptchaTypesTable_tooltip_lastUsedAt(formattedDate, lastEntry.getDomain());
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }
        };
    }

    private ExtTextColumn<CAPTCHA_TYPE> createUsedForServicesColumn() {
        return new ExtTextColumn<CAPTCHA_TYPE>(_GUI.T.CaptchaTypesTable_column_usedForServices()) {
            @Override
            public String getStringValue(final CAPTCHA_TYPE ctype) {
                final List<CaptchaHistoryEntry> entries = CaptchaHistoryManager.getInstance().getEntriesByCaptchaType(ctype);
                final String text_none = _GUI.T.CaptchaTypesTable_lit_none();
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
                    return _GUI.T.CaptchaTypesTable_tooltip_neverUsed();
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
                final StringBuilder sb = new StringBuilder();
                for (int i = 0; i < domains.size(); i++) {
                    if (i > 0) {
                        sb.append(", ");
                    }
                    sb.append(domains.get(i));
                }
                return _GUI.T.CaptchaTypesTable_tooltip_usedFor(sb.toString());
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }
        };
    }

    private ExtTextColumn<CAPTCHA_TYPE> createDemoUrlColumn() {
        return new ExtTextColumn<CAPTCHA_TYPE>(_GUI.T.CaptchaTypesTable_column_demoUrl()) {
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
                /* Hidden by default; can be enabled via the column header context menu. */
                return false;
            }
        };
    }

    /**
     * IDE-only, hidden by default (see the {@link DebugMode#TRUE_IN_IDE_ELSE_FALSE} guard in {@link #initColumns()}): manually triggers a
     * real, throwaway test {@link org.jdownloader.captcha.v2.Challenge} for this captcha type (see
     * {@link CAPTCHA_TYPE#getTestChallengeDownload()}/{@link CAPTCHA_TYPE#getTestChallengeLogin()}/
     * {@link CAPTCHA_TYPE#getTestChallengeCrawler()}), one button per {@link CaptchaRequestType}. Buttons are disabled for types without
     * test data (see {@link CAPTCHA_TYPE#hasTestChallenges()}).
     */
    private ExtComponentColumn<CAPTCHA_TYPE> createTestCaptchaSolverColumn() {
        return new ExtComponentColumn<CAPTCHA_TYPE>("Test Captcha solver") {
            private CAPTCHA_TYPE   editing;
            /*
             * Separate component instances for rendering and editing: a single JComponent instance cannot simultaneously serve as the
             * live cell editor (embedded in the table at the row currently being edited) AND be reused as the paint "stamp" for every
             * other visible row's renderer pass -- Swing repeatedly reparents it between the two roles, which is what caused the buttons
             * to be barely clickable and the whole table to flicker.
             */
            private final JButton  editorDownloadButton;
            private final JButton  editorLoginButton;
            private final JButton  editorCrawlerButton;
            private final MigPanel editorPanel;
            private final JButton  rendererDownloadButton;
            private final JButton  rendererLoginButton;
            private final JButton  rendererCrawlerButton;
            private final MigPanel rendererPanel;
            {
                editorDownloadButton = new JButton("DL-Captcha");
                editorDownloadButton.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent e) {
                        if (editing != null) {
                            CaptchaTestDialog.showFor(new CaptchaTestDialog.ChallengeFactory<Object>() {
                                @Override
                                @SuppressWarnings("unchecked")
                                public Challenge<Object> newChallenge() {
                                    return (Challenge<Object>) editing.getTestChallengeDownload();
                                }
                            }, "Test: " + editing.getDisplayName() + " (DL-Captcha)");
                        }
                    }
                });
                editorLoginButton = new JButton("Login-Captcha");
                editorLoginButton.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent e) {
                        if (editing != null) {
                            CaptchaTestDialog.showFor(new CaptchaTestDialog.ChallengeFactory<Object>() {
                                @Override
                                @SuppressWarnings("unchecked")
                                public Challenge<Object> newChallenge() {
                                    return (Challenge<Object>) editing.getTestChallengeLogin();
                                }
                            }, "Test: " + editing.getDisplayName() + " (Login-Captcha)");
                        }
                    }
                });
                editorCrawlerButton = new JButton("CrawlerCaptcha");
                editorCrawlerButton.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(final ActionEvent e) {
                        if (editing != null) {
                            CaptchaTestDialog.showFor(new CaptchaTestDialog.ChallengeFactory<Object>() {
                                @Override
                                @SuppressWarnings("unchecked")
                                public Challenge<Object> newChallenge() {
                                    return (Challenge<Object>) editing.getTestChallengeCrawler();
                                }
                            }, "Test: " + editing.getDisplayName() + " (CrawlerCaptcha)");
                        }
                    }
                });
                editorPanel = new MigPanel("ins 0", "[][][]", "[]");
                editorPanel.add(editorDownloadButton, "height 20!");
                editorPanel.add(editorLoginButton, "height 20!");
                editorPanel.add(editorCrawlerButton, "height 20!");
                /* Renderer buttons are a pure visual stamp (never actually clickable, Swing renderers never receive input); no listeners. */
                rendererDownloadButton = new JButton("DL-Captcha");
                rendererLoginButton = new JButton("Login-Captcha");
                rendererCrawlerButton = new JButton("CrawlerCaptcha");
                rendererPanel = new MigPanel("ins 0", "[][][]", "[]");
                rendererPanel.add(rendererDownloadButton, "height 20!");
                rendererPanel.add(rendererLoginButton, "height 20!");
                rendererPanel.add(rendererCrawlerButton, "height 20!");
                setClickcount(1);
            }

            @Override
            public boolean isSortable(final CAPTCHA_TYPE obj) {
                return false;
            }

            @Override
            public boolean isEditable(final CAPTCHA_TYPE ctype) {
                return true;
            }

            @Override
            protected JComponent getInternalEditorComponent(final CAPTCHA_TYPE value, final boolean isSelected, final int row, final int column) {
                return editorPanel;
            }

            @Override
            protected JComponent getInternalRendererComponent(final CAPTCHA_TYPE value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                return rendererPanel;
            }

            @Override
            public void configureEditorComponent(final CAPTCHA_TYPE value, final boolean isSelected, final int row, final int column) {
                editing = value;
                final boolean hasTestChallenges = value.hasTestChallenges();
                editorDownloadButton.setEnabled(hasTestChallenges);
                editorLoginButton.setEnabled(hasTestChallenges);
                editorCrawlerButton.setEnabled(hasTestChallenges);
            }

            @Override
            public void configureRendererComponent(final CAPTCHA_TYPE value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                final boolean hasTestChallenges = value.hasTestChallenges();
                rendererDownloadButton.setEnabled(hasTestChallenges);
                rendererLoginButton.setEnabled(hasTestChallenges);
                rendererCrawlerButton.setEnabled(hasTestChallenges);
            }

            @Override
            public boolean isDefaultVisible() {
                /* Hidden by default even in the IDE; can be enabled via the column header context menu. */
                return false;
            }

            @Override
            public void resetEditor() {
            }

            @Override
            public void resetRenderer() {
            }
        };
    }

    private ExtTextColumn<CAPTCHA_TYPE> createSupportedByJDownloaderColumn() {
        final Icon icon_okay = NewTheme.I().getIcon(IconKey.ICON_OK, 16);
        final Icon icon_error = NewTheme.I().getIcon(IconKey.ICON_ERROR, 16);
        return new ExtTextColumn<CAPTCHA_TYPE>(_GUI.T.CaptchaTypesTable_column_supportedByJD()) {
            @Override
            public String getStringValue(final CAPTCHA_TYPE ctype) {
                return ctype.isJDownloaderSupported() ? _GUI.T.lit_yes() : _GUI.T.lit_no();
            }

            @Override
            public Icon getIcon(final CAPTCHA_TYPE ctype) {
                return ctype.isJDownloaderSupported() ? icon_okay : icon_error;
            }

            @Override
            protected String getTooltipText(final CAPTCHA_TYPE ctype) {
                if (ctype.isJDownloaderSupported()) {
                    return _GUI.T.CaptchaTypesTable_tooltip_supportedByJD();
                } else {
                    return _GUI.T.CaptchaTypesTable_tooltip_notSupportedByJD();
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
            /* A captcha type is enabled unless it is in the solver's disabled-set. */
            final CaptchaSolverConfigV3 cfg = solver.getConfigV3();
            final java.util.Set<CAPTCHA_TYPE> disabled = cfg.getDisabledCaptchaTypes();
            return disabled == null || !disabled.contains(ctype);
        }

        @Override
        public void setEnabled(final CAPTCHA_TYPE ctype, final boolean enabled) {
            final CaptchaSolverConfigV3 cfg = solver.getConfigV3();
            final java.util.Set<CAPTCHA_TYPE> current = cfg.getDisabledCaptchaTypes();
            /* Copy so the setter sees a new instance and persists the change. */
            final java.util.HashSet<CAPTCHA_TYPE> disabled = current != null ? new java.util.HashSet<CAPTCHA_TYPE>(current) : new java.util.HashSet<CAPTCHA_TYPE>();
            if (enabled) {
                disabled.remove(ctype);
            } else {
                disabled.add(ctype);
            }
            cfg.setDisabledCaptchaTypes(disabled);
        }

        @Override
        public boolean isSupported(final CAPTCHA_TYPE ctype) {
            final List<CAPTCHA_TYPE> supportedTypes = solver.getSupportedCaptchaTypes();
            /* A solver without a list of supported captcha types supports nothing (see SolverService#getSupportedCaptchaTypes). */
            return supportedTypes != null && supportedTypes.contains(ctype);
        }
    }
}