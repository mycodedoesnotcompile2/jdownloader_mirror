package jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter;

import java.util.List;

import javax.swing.Icon;

import org.jdownloader.controlling.filter.LinkFilterController;
import org.jdownloader.controlling.filter.LinkgrabberFilterRule;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;

public class ExceptionsTableModel extends AbstractFilterTableModel {

    private static final long serialVersionUID = -7756459932564776739L;

    public ExceptionsTableModel(String id) {
        super(id);
    }

    @Override
    protected String getConditionColumnName() {
        return _GUI.T.ExceptionsTableModel_initColumns_condition_();
    }

    @Override
    protected Icon createConditionHeaderIcon() {
        return new AbstractIcon(IconKey.ICON_OK, 14);
    }

    @Override
    public List<LinkgrabberFilterRule> getTableData() {
        return LinkFilterController.getInstance().listExceptions();
    }
}
