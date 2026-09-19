package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Dimension;

import javax.swing.JLabel;
import javax.swing.JScrollPane;

import org.appwork.utils.swing.SwingUtils;
import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.settings.AbstractConfigPanel;
import org.jdownloader.images.NewTheme;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.gui.swing.jdgui.views.settings.components.SettingsComponent;
import jd.plugins.CaptchaSolverAccountSettingsPanelBuilder;
import jd.plugins.CaptchaSolverAccountSettingsPanelBuilder.SolverServiceCaptchaTypeAccessor;
import jd.plugins.CaptchaType.CAPTCHA_TYPE;

public class SolverOrderContainer extends org.appwork.swing.MigPanel implements SettingsComponent {
    private final SolverOrderTable     solverOrder;
    private BasicJDTable<CAPTCHA_TYPE> detailTable;
    private JScrollPane                detailScrollPane;
    private JLabel                     detailLabel;
    private JLabel                     descriptionLabel;
    private JLabel                     settingsLabel;
    private JScrollPane                configScrollPane;

    public SolverOrderContainer(SolverOrderTable urlOrder) {
        /* gapy 0: no implicit inter-row gaps, so the height reserved in getConstraints() stays exact (all gaps are set explicitly). */
        super("ins 0, gapy 0", "[grow,fill]", "[][][][][][]");
        this.solverOrder = urlOrder;
        /* Main solver order table. Slightly narrower on the right; never shows any scrollbar. */
        final JScrollPane sp = new JScrollPane(urlOrder);
        sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
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
        add(detailScrollPane, "gaptop 6, growx, gapright 40, wrap");
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
                     * Description above the table (no icon; the icon is shown on the header above): the solver's own description plus a hint
                     * that unchecking a captcha type disables it globally for this solver. Always shown when a solver is selected.
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
                    final CaptchaSolverAccountSettingsPanelBuilder builder = new CaptchaSolverAccountSettingsPanelBuilder(new SolverServiceCaptchaTypeAccessor(solver));
                    detailTable = builder.getCaptchaTypesTable();
                    /* Size the scroll pane to fit header plus all rows so every row is shown without a scrollbar. */
                    final int fullHeight = detailTableFullHeight();
                    final Dimension fullSize = new Dimension(detailTable.getPreferredSize().width, fullHeight);
                    detailTable.setPreferredScrollableViewportSize(new Dimension(detailTable.getPreferredSize().width, detailTable.getPreferredSize().height));
                    detailScrollPane.setViewportView(detailTable);
                    detailScrollPane.setPreferredSize(fullSize);
                    detailScrollPane.setMinimumSize(fullSize);
                    detailScrollPane.setVisible(true);
                    /* Show the selected solver's config below the table (plugin config if available, else the local solver's config). */
                    final AbstractConfigPanel configComponent = solver.getConfigComponent();
                    if (configComponent != null) {
                        configScrollPane.setViewportView(configComponent);
                        configScrollPane.setVisible(true);
                    } else {
                        configScrollPane.setViewportView(null);
                        configScrollPane.setVisible(false);
                    }
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
     * Full pixel height of a table (header + all rows) plus a few pixels for the scroll pane border, so the last row is never clipped and no
     * scrollbar is needed. getPreferredSize() already accounts for row height and inter-cell spacing of all rows.
     */
    private static int tableFullHeight(final BasicJDTable<?> table) {
        if (table == null) {
            return 0;
        }
        final int headerHeight = table.getTableHeader() != null ? table.getTableHeader().getPreferredSize().height : 0;
        return headerHeight + table.getPreferredSize().height + 4;
    }

    private int detailTableFullHeight() {
        return tableFullHeight(detailTable);
    }

    /*
     * The total height is summed from the exact preferred heights of the visible components plus the explicit gaptop gaps used in the layout
     * (the layout sets gapy 0, so there are no implicit inter-row gaps to account for). This keeps the reserved height exact: the tables are
     * shown in full (no clipping) and there is no leftover space that would make the surrounding panel scrollable.
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