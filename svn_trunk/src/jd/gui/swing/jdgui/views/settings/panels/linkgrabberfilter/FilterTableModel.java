package jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter;

import java.util.List;

import javax.swing.Icon;

import org.jdownloader.controlling.filter.LinkFilterController;
import org.jdownloader.controlling.filter.LinkgrabberFilterRule;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;

public class FilterTableModel extends AbstractFilterTableModel {

    private static final long serialVersionUID = -7756459932564776739L;

    public FilterTableModel(String id) {
        super(id);
    }

    @Override
    protected String getConditionColumnName() {
        return _GUI.T.settings_linkgrabber_filter_columns_condition();
    }

    @Override
    protected Icon createConditionHeaderIcon() {
        return new AbstractIcon(IconKey.ICON_TRASH, 14);
    }

    @Override
    public List<LinkgrabberFilterRule> getTableData() {
        return LinkFilterController.getInstance().listFilters();
    }
}
