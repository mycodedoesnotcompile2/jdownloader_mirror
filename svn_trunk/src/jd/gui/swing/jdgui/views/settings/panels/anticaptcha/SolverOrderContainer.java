package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;

import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import org.appwork.utils.swing.SwingUtils;
import org.jdownloader.captcha.v2.CaptchaSolverCaptchaTypesSettingsPanelBuilder;
import org.jdownloader.captcha.v2.CaptchaSolverCaptchaTypesSettingsPanelBuilder.SolverServiceCaptchaTypeAccessor;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.NewTheme;
import org.jdownloader.plugins.components.captchasolver.PluginForCaptchaSolverSolverService;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.gui.swing.jdgui.views.settings.components.SettingsComponent;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;
import jd.plugins.PluginConfigPanelNG;

public class SolverOrderContainer extends org.appwork.swing.MigPanel implements SettingsComponent {
    private final SolverOrderTable     solverOrder;
    private BasicJDTable<CAPTCHA_TYPE> detailTable;
    private JScrollPane                detailScrollPane;
    private JLabel                     detailLabel;
    private JLabel                     descriptionLabel;
    private JLabel                     limitsLabel;
    private JLabel                     settingsLabel;
    private JScrollPane                configScrollPane;

    public SolverOrderContainer(SolverOrderTable urlOrder) {
        /* gapy 0: no implicit inter-row gaps, so the height reserved in getConstraints() stays exact (all gaps are set explicitly). */
        super("ins 0, gapy 0", "[grow,fill]", "[][][][][][][]");
        this.solverOrder = urlOrder;
        /* Main solver order table. Slightly narrower on the right; never shows any scrollbar. */
        final JScrollPane sp = new JScrollPane(urlOrder);
        sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        passMouseWheelToParent(sp);
        SwingUtils.setOpaque(this, false);
        add(sp, "gapright 40, wrap");
        /* Header label shown above the detail table; separated from the solver table above by a gap. */
        detailLabel = new JLabel();
        detailLabel.setVisible(false);
        add(detailLabel, "gaptop 15, wrap");
        // Optional per-solver description shown above the detail table
        descriptionLabel = new JLabel();
        descriptionLabel.setVisible(false);
        add(descriptionLabel, "growx, wmin 10, gapright 40, wrap");
        // Detail table, initially hidden
        detailScrollPane = new JScrollPane();
        /* Never scroll: the detail table is always shown at full size (all rows). */
        detailScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        detailScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        detailScrollPane.setVisible(false);
        passMouseWheelToParent(detailScrollPane);
        add(detailScrollPane, "gaptop 6, growx, gapright 40, wrap");
        /* Server-side limits info line, shown below the captcha-types table for plugin-based (account) solvers only. */
        limitsLabel = new JLabel();
        limitsLabel.setVisible(false);
        add(limitsLabel, "gaptop 6, wrap");
        /* "<solver> Settings" section header (gear icon) shown BELOW the captcha-types table, above the config panel. */
        settingsLabel = new JLabel("Settings", NewTheme.I().getIcon(IconKey.ICON_SETTINGS, 18), JLabel.LEADING);
        settingsLabel.setVisible(false);
        add(settingsLabel, "gaptop 12, wrap");
        /* Config panel of the selected solver, shown below the "Settings" header. */
        configScrollPane = new JScrollPane();
        configScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        configScrollPane.setVisible(false);
        add(configScrollPane, "gaptop 6, growx");
        urlOrder.setSelectionListener(new SolverOrderTable.SelectionListener() {
            @Override
            public void onSolverSelected(SolverService solver) {
                if (solver == null) {
                    detailLabel.setVisible(false);
                    descriptionLabel.setVisible(false);
                    limitsLabel.setVisible(false);
                    settingsLabel.setVisible(false);
                    detailScrollPane.setVisible(false);
                    detailTable = null;
                    configScrollPane.setViewportView(null);
                    configScrollPane.setVisible(false);
                } else {
                    // Header label, prefixed with the selected solver's icon
                    detailLabel.setIcon(solver.getIcon(18));
                    detailLabel.setText(solver.getName() + ": Supported captcha types overview and settings");
                    detailLabel.setVisible(true);
                    /*
                     * Description above the table (no icon; the icon is shown on the header above): the solver's own description plus a
                     * hint that unchecking a captcha type disables it globally for this solver. Always shown when a solver is selected.
                     */
                    final StringBuilder description = new StringBuilder("<html>");
                    final String descriptionText = solver.getDescription();
                    if (descriptionText != null && descriptionText.length() > 0) {
                        description.append(descriptionText).append("<br>");
                    }
                    description.append("Unchecking a captcha type will disable it globally for this solver.");
                    description.append("</html>");
                    descriptionLabel.setText(description.toString());
                    descriptionLabel.setVisible(true);
                    /* Settings header below the table names the selected solver, e.g. "9kw.eu Settings". */
                    settingsLabel.setText(solver.getName() + " Settings");
                    settingsLabel.setVisible(true);
                    // Build detail table from a no-account builder
                    final CaptchaSolverCaptchaTypesSettingsPanelBuilder builder = new CaptchaSolverCaptchaTypesSettingsPanelBuilder(new SolverServiceCaptchaTypeAccessor(solver));
                    detailTable = builder.getCaptchaTypesTable();
                    /* Size the scroll pane to fit header plus all rows so every row is shown without a scrollbar. */
                    final int fullHeight = detailTableFullHeight();
                    final Dimension fullSize = new Dimension(detailTable.getPreferredSize().width, fullHeight);
                    detailTable.setPreferredScrollableViewportSize(new Dimension(detailTable.getPreferredSize().width, detailTable.getPreferredSize().height));
                    detailScrollPane.setViewportView(detailTable);
                    detailScrollPane.setPreferredSize(fullSize);
                    detailScrollPane.setMinimumSize(fullSize);
                    detailScrollPane.setVisible(true);
                    /*
                     * Server-side limits info line: only meaningful for plugin-based (account) solvers, which are the only ones that
                     * implement getServerSideMaxSimultaneousCaptchaThreadsLimit()/getServerSideMaxPollingTimeoutMillis().
                     */
                    if (solver instanceof PluginForCaptchaSolverSolverService) {
                        final PluginForCaptchaSolverSolverService pluginSolver = (PluginForCaptchaSolverSolverService) solver;
                        final int maxThreads = pluginSolver.getServerSideMaxSimultaneousCaptchaThreadsLimit();
                        final long maxPollingMillis = pluginSolver.getServerSideMaxPollingTimeoutMillis();
                        final String maxThreadsText = maxThreads == Integer.MAX_VALUE ? "~" : String.valueOf(maxThreads);
                        final String maxPollingText = maxPollingMillis == Long.MAX_VALUE ? "~" : String.valueOf(maxPollingMillis / 1000L);
                        limitsLabel.setText("<html>Max concurrent captcha threads: " + maxThreadsText + "<br>Server side max polling time: " + maxPollingText + "s</html>");
                        limitsLabel.setVisible(true);
                    } else {
                        limitsLabel.setVisible(false);
                    }
                    /* Show the selected solver's config below the table (plugin config if available, else the local solver's config). */
                    /* Built generically from the solver's V3 config interface, just like plugin config panels. */
                    final PluginConfigPanelNG configComponent = new PluginConfigPanelNG() {
                        @Override
                        public void updateContents() {
                        }

                        @Override
                        public void save() {
                        }
                    };
                    configComponent.build(solver.getConfigV3());
                    configScrollPane.setViewportView(configComponent);
                    configScrollPane.setVisible(true);
                }
                revalidate();
                repaint();
                if (getParent() != null) {
                    getParent().revalidate();
                    getParent().repaint();
                }
            }
        });
    }

    /**
     * A scroll pane without scrollbars still consumes mouse wheel events, so scrolling over such a table would do nothing. Replaces the
     * scroll pane's own wheel handling by one that forwards the event to the next scroll pane above it (the panel that actually scrolls).
     */
    private static void passMouseWheelToParent(final JScrollPane scrollPane) {
        for (final MouseWheelListener listener : scrollPane.getMouseWheelListeners()) {
            scrollPane.removeMouseWheelListener(listener);
        }
        scrollPane.addMouseWheelListener(new MouseWheelListener() {
            @Override
            public void mouseWheelMoved(final MouseWheelEvent e) {
                final Container parent = SwingUtilities.getAncestorOfClass(JScrollPane.class, scrollPane);
                if (parent != null) {
                    parent.dispatchEvent(SwingUtilities.convertMouseEvent(scrollPane, e, parent));
                }
            }
        });
    }

    /**
     * Full pixel height of a table (header + all rows) plus a few pixels for the scroll pane border, so the last row is never clipped and
     * no scrollbar is needed. getPreferredSize() already accounts for row height and inter-cell spacing of all rows. <br>
     * 2026-09-21: Please don't ask me why but without adding these extra 10px, also e.g. with a value lower than 10px extra, the last table
     * item will be partly cut and there will be a scrollbar which we don't want.
     */
    private static int tableFullHeight(final BasicJDTable<?> table) {
        if (table == null) {
            return 0;
        }
        final int headerHeight = table.getTableHeader() != null ? table.getTableHeader().getPreferredSize().height : 0;
        return headerHeight + table.getPreferredSize().height + 10;
    }

    private int detailTableFullHeight() {
        return tableFullHeight(detailTable);
    }

    /*
     * The total height is summed from the exact preferred heights of the visible components plus the explicit gaptop gaps used in the
     * layout (the layout sets gapy 0, so there are no implicit inter-row gaps to account for). This keeps the reserved height exact: the
     * tables are shown in full (no clipping) and there is no leftover space that would make the surrounding panel scrollable.
     */
    @Override
    public String getConstraints() {
        int height = tableFullHeight(solverOrder);
        if (detailScrollPane.isVisible() && detailTable != null) {
            /* gaptop 15 above the header label. */
            height += 15 + detailLabel.getPreferredSize().height;
            if (descriptionLabel.isVisible()) {
                height += descriptionLabel.getPreferredSize().height;
            }
            /* gaptop 6 above the captcha-types table. */
            height += 6 + detailTableFullHeight();
            if (limitsLabel.isVisible()) {
                /* gaptop 6 above the limits info line. */
                height += 6 + limitsLabel.getPreferredSize().height;
            }
            if (settingsLabel.isVisible()) {
                /* gaptop 12 above the "Settings" header. */
                height += 12 + settingsLabel.getPreferredSize().height;
            }
        }
        if (configScrollPane.isVisible() && configScrollPane.getViewport() != null && configScrollPane.getViewport().getView() != null) {
            /* gaptop 6 above the config panel. */
            height += 6 + configScrollPane.getViewport().getView().getPreferredSize().height;
        }
        return "height " + height + "!, wmin 10";
    }

    @Override
    public boolean isMultiline() {
        return true;
    }
}