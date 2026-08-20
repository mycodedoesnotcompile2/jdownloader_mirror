package jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter;

import java.awt.Component;
import java.awt.Point;
import java.util.List;

import javax.swing.Icon;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;

import org.appwork.swing.components.tooltips.ExtTooltip;
import org.appwork.swing.exttable.ExtTableHeaderRenderer;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtCheckColumn;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.StringUtils;
import org.appwork.utils.event.predefined.changeevent.ChangeEvent;
import org.appwork.utils.event.predefined.changeevent.ChangeListener;
import org.jdownloader.controlling.filter.LinkFilterController;
import org.jdownloader.controlling.filter.LinkgrabberFilterRule;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.linkgrabber.quickfilter.ExceptionFilter;
import org.jdownloader.images.NewTheme;

/**
 * Shared model for the two filter rules settings tables (deny filters and accept exceptions). Both tables share the same columns and
 * rendering; only the condition column header (name + icon) and the underlying rule list differ, which subclasses provide via the abstract
 * hooks.
 */
public abstract class AbstractFilterTableModel extends ExtTableModel<LinkgrabberFilterRule> implements ChangeListener {
    private static final long serialVersionUID = -7756459932564776739L;

    public AbstractFilterTableModel(String id) {
        super(id);
        LinkFilterController.getInstance().getEventSender().addListener(this, false);
    }

    /** Header text of the condition column. */
    protected abstract String getConditionColumnName();

    /** Header icon of the condition column. */
    protected abstract Icon createConditionHeaderIcon();

    /** The rules shown in this table (deny filters or accept exceptions). */
    public abstract List<LinkgrabberFilterRule> getTableData();

    @Override
    protected void initColumns() {
        this.addColumn(new ExtCheckColumn<LinkgrabberFilterRule>(_GUI.T.settings_linkgrabber_filter_columns_enabled()) {
            private static final long serialVersionUID = -4667150369226691276L;

            public ExtTableHeaderRenderer getHeaderRenderer(final JTableHeader jTableHeader) {
                final ExtTableHeaderRenderer ret = new ExtTableHeaderRenderer(this, jTableHeader) {
                    private static final long serialVersionUID = 3938290423337000265L;
                    private final Icon        ok               = NewTheme.I().getIcon(IconKey.ICON_OK, 14);

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
            public boolean isHidable() {
                return false;
            }

            @Override
            protected boolean getBooleanValue(LinkgrabberFilterRule value) {
                return value.isEnabled();
            }

            @Override
            public boolean isEditable(LinkgrabberFilterRule obj) {
                return true;
            }

            @Override
            public ExtTooltip createToolTip(Point position, LinkgrabberFilterRule obj) {
                return createTooltip(obj);
            }

            @Override
            protected void setBooleanValue(boolean value, LinkgrabberFilterRule object) {
                object.setEnabled(value);
                LinkFilterController.getInstance().update();
            }
        });
        addColumn(new ExtTextColumn<LinkgrabberFilterRule>(_GUI.T.settings_linkgrabber_filter_columns_name()) {
            @Override
            public boolean isEnabled(LinkgrabberFilterRule value) {
                return value.isEnabled();
            }

            protected Icon getIcon(final LinkgrabberFilterRule value) {
                final String key = value.getIconKey();
                final Icon baseIcon = key == null ? null : NewTheme.I().getIcon(key, 18);
                if (value.isNegated()) {
                    // Mark inverted rules with a badge next to the user-selected icon (badge shown alone when no icon is set).
                    return new ExceptionFilter.InvertedFilterIcon(baseIcon, 11);
                }
                return baseIcon;
            }

            @Override
            public ExtTooltip createToolTip(Point position, LinkgrabberFilterRule obj) {
                return createTooltip(obj);
            }

            @Override
            public String getStringValue(LinkgrabberFilterRule value) {
                return value.getName();
            }
        });
        addColumn(new ExtTextColumn<LinkgrabberFilterRule>(getConditionColumnName()) {
            public ExtTableHeaderRenderer getHeaderRenderer(final JTableHeader jTableHeader) {
                final ExtTableHeaderRenderer ret = new ExtTableHeaderRenderer(this, jTableHeader) {
                    private static final long serialVersionUID = 3938290423337000265L;

                    @Override
                    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
                        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                        setIcon(createConditionHeaderIcon());
                        return this;
                    }
                };
                return ret;
            }

            @Override
            public boolean isEnabled(LinkgrabberFilterRule value) {
                return value.isEnabled();
            }

            @Override
            public ExtTooltip createToolTip(Point position, LinkgrabberFilterRule obj) {
                return createTooltip(obj);
            }

            @Override
            public String getStringValue(LinkgrabberFilterRule value) {
                final String conditionText = value.toString();
                final String condition = _GUI.T.settings_linkgrabber_filter_columns_if(conditionText);
                final boolean hasZeroConditions = StringUtils.isEmpty(conditionText);
                if (value.isNegated()) {
                    // Prefix inverted rules with "Inverted:" so the different (view) semantics are obvious in the settings table.
                    if (hasZeroConditions) {
                        return _GUI.T.settings_linkgrabber_filter_columns_inverted();
                    }
                    return _GUI.T.settings_linkgrabber_filter_columns_inverted() + " | " + condition;
                }
                if (hasZeroConditions) {
                    /* Zero conditions */
                    return "";
                }
                return condition;
            }
        });
    }

    protected ExtTooltip createTooltip(LinkgrabberFilterRule obj) {
        // if (obj == null) return null;
        // tooltip.updateRule(obj);
        // return tooltip;
        return null;
    }

    public void onChangeEvent(ChangeEvent event) {
        _fireTableStructureChanged(getTableData(), true);
    }
}
