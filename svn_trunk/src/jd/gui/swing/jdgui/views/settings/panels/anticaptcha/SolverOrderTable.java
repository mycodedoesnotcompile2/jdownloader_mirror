package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Dimension;
import java.awt.event.MouseEvent;

import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.appwork.uio.UIOManager;
import org.jdownloader.captcha.v2.SolverService;

import jd.gui.swing.jdgui.BasicJDTable;

public class SolverOrderTable extends BasicJDTable<SolverService> {
    public interface SelectionListener {
        void onSolverSelected(SolverService solver);
    }

    private SelectionListener selectionListener;

    public void setSelectionListener(SelectionListener selectionListener) {
        this.selectionListener = selectionListener;
    }

    public SolverOrderTable() {
        super(new SolverOrderTableModel());
        setShowHorizontalLineBelowLastEntry(false);
        setShowHorizontalLines(true);
        setFocusable(false);
        getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(ListSelectionEvent e) {
                if (e.getValueIsAdjusting()) {
                    return;
                }
                if (selectionListener != null) {
                    int row = getSelectedRow();
                    SolverService solver = row >= 0 ? getModel().getObjectbyRow(row) : null;
                    selectionListener.onSolverSelected(solver);
                }
            }
        });
    }

    @Override
    protected boolean onDoubleClick(MouseEvent e, SolverService obj) {
        SolverPropertiesDialog d = new SolverPropertiesDialog(obj, obj.getConfigPanel());
        UIOManager.I().show(null, d);
        return true;
    }

    @Override
    public Dimension getPreferredScrollableViewportSize() {
        Dimension dim = super.getPreferredScrollableViewportSize();
        // here we return the pref height
        dim.height = getPreferredSize().height;
        return dim;
    }
}
