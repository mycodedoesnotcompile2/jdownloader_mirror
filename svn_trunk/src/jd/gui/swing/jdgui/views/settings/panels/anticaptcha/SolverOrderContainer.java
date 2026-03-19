package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Dimension;

import javax.swing.JLabel;
import javax.swing.JScrollPane;

import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.utils.swing.SwingUtils;
import org.jdownloader.captcha.v2.SolverService;

import jd.gui.swing.jdgui.BasicJDTable;
import jd.gui.swing.jdgui.views.settings.components.SettingsComponent;
import jd.plugins.CaptchaSolverAccountSettingsPanelBuilder;
import jd.plugins.CaptchaType;

public class SolverOrderContainer extends org.appwork.swing.MigPanel implements SettingsComponent {
    private final SolverOrderTable    solverOrder;
    private BasicJDTable<CaptchaType> detailTable;
    private JScrollPane               detailScrollPane;
    private JLabel                    detailLabel;

    public SolverOrderContainer(SolverOrderTable urlOrder) {
        super("ins 0", "[grow,fill]", "[][][]");
        this.solverOrder = urlOrder;
        // Main solver order table
        final JScrollPane sp = new JScrollPane(urlOrder);
        sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        SwingUtils.setOpaque(this, false);
        add(sp, "wrap");
        // Label shown above detail table
        detailLabel = new JLabel();
        detailLabel.setVisible(false);
        add(detailLabel, "wrap");
        // Detail table, initially hidden
        detailScrollPane = new JScrollPane();
        detailScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        detailScrollPane.setVisible(false);
        add(detailScrollPane, "growx");
        urlOrder.setSelectionListener(new SolverOrderTable.SelectionListener() {
            @Override
            public void onSolverSelected(SolverService solver) {
                if (solver == null) {
                    detailLabel.setVisible(false);
                    detailScrollPane.setVisible(false);
                    detailTable = null;
                } else {
                    // Update label
                    detailLabel.setText(solver.getName() + ": Supported captcha types overview");
                    detailLabel.setVisible(true);
                    // Build detail table from a no-account builder
                    final CaptchaSolverAccountSettingsPanelBuilder builder = new CaptchaSolverAccountSettingsPanelBuilder(null);
                    final ExtTableModel<CaptchaType> model = builder.createTableModel();
                    model._fireTableStructureChanged(builder.getCaptchaTypes(), false);
                    detailTable = new BasicJDTable<CaptchaType>(model);
                    // Set preferred size to show all rows without scrollbar
                    detailTable.setPreferredScrollableViewportSize(new Dimension(detailTable.getPreferredSize().width, detailTable.getRowHeight() * detailTable.getRowCount()));
                    detailScrollPane.setViewportView(detailTable);
                    detailScrollPane.setVisible(true);
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

    @Override
    public String getConstraints() {
        int height = solverOrder.getPreferredSize().height + 27;
        if (detailScrollPane.isVisible() && detailTable != null) {
            height += detailLabel.getPreferredSize().height;
            height += detailTable.getRowHeight() * detailTable.getRowCount() + 27;
        }
        return "height " + height + "!, wmin 10";
    }

    @Override
    public boolean isMultiline() {
        return true;
    }
}