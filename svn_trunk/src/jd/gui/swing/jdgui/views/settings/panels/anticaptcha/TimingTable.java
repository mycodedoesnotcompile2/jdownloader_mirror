package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import javax.swing.AbstractAction;

import jd.gui.swing.jdgui.BasicJDTable;

import org.jdownloader.captcha.v2.SolverService;

public class TimingTable extends BasicJDTable<SolverService> {

    public TimingTable(SolverService solver) {
        super(new TimingTableModel(solver));
        setShowHorizontalLineBelowLastEntry(false);
        setShowHorizontalLines(true);
    }

    @Override
    protected boolean isDisplayFullsizeAlways() {
        return true;
    }

    public AbstractAction getResetAction() {
        return ((TimingTableModel) getModel()).getResetAction();
    }
}
