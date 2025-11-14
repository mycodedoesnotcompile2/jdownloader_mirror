package org.jdownloader.gui.views.downloads.overviewpanel;

import java.text.NumberFormat;

import javax.swing.JComponent;
import javax.swing.JLabel;

import org.appwork.storage.config.handler.BooleanKeyHandler;
import org.appwork.swing.MigPanel;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.SwingUtils;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.settings.staticreferences.CFG_GUI;

public abstract class DataEntry<T> {

    private JLabel total;

    public JLabel getTotal() {
        return total;
    }

    public JLabel getFiltered() {
        return filtered;
    }

    public JLabel getSelected() {
        return selected;
    }

    private JLabel               filtered;
    private JLabel               selected;
    final private String         label;
    protected String             id;
    protected final NumberFormat numberFormat;

    public String getId() {

        return id;
    }

    public String getLabel() {
        return label;
    }

    @Override
    public String toString() {
        return getLabel();
    }

    public DataEntry(String label) {
        id = getClass().getName();
        id = id.substring(id.lastIndexOf(".") + 1);
        this.label = label;
        total = new JLabel("-1");
        filtered = new JLabel("-1");
        selected = new JLabel("-1");
        total.setToolTipText(_GUI.T.DownloadOverview_DownloadOverview_tooltip1());
        filtered.setToolTipText(_GUI.T.DownloadOverview_DownloadOverview_tooltip2());
        selected.setToolTipText(_GUI.T.DownloadOverview_DownloadOverview_tooltip3());
        updateVisibility(false);
        this.numberFormat = getNumberFormat();
    }

    protected NumberFormat getNumberFormat() {
        return NumberFormat.getInstance();
    }

    public String getPopupLabel() {
        return getLabel().replace(":", "");
    }

    private JComponent createHeaderLabel(String label) {
        JLabel lbl = new JLabel(label);
        SwingUtils.toBold(lbl);
        lbl.setEnabled(false);
        return lbl;
    }

    public void addTo(MigPanel info) {
        addTo(info, null);
    }

    public void addTo(MigPanel info, String constrains) {
        info.add(createHeaderLabel(label), "alignx right" + (constrains == null ? "" : constrains));
        info.add(total, "hidemode 3");
        info.add(filtered, "hidemode 3");
        info.add(selected, "hidemode 3");
    }

    public void setTotal(Object value) {
        total.setText(toString(value));
    }

    private String toString(Object value) {
        if (value instanceof String) {
            return (String) value;
        }
        if (value instanceof Number) {
            return StringUtils.toString(numberFormat, ((Number) value).longValue());
        }
        return value.toString();
    }

    public void setSelected(Object value) {
        selected.setText(toString(value));
    }

    public void setFiltered(Object value) {
        filtered.setText(toString(value));
    }

    public void updateVisibility(final boolean hasSelectedObjects) {
        final boolean smart = CFG_GUI.OVERVIEW_PANEL_SMART_INFO_VISIBLE.isEnabled();
        final boolean visibleOnly = CFG_GUI.OVERVIEW_PANEL_VISIBLE_ONLY_INFO_VISIBLE.isEnabled();
        final boolean selectedOnly = CFG_GUI.OVERVIEW_PANEL_SELECTED_INFO_VISIBLE.isEnabled();
        final boolean totalVisible = CFG_GUI.OVERVIEW_PANEL_TOTAL_INFO_VISIBLE.isEnabled();
        if (smart || (!visibleOnly && !totalVisible && !selectedOnly)) {
            if (hasSelectedObjects) {
                filtered.setVisible(false);
                total.setVisible(false);
                selected.setVisible(true);
            } else {
                filtered.setVisible(true);
                total.setVisible(false);
                selected.setVisible(false);
            }
        } else {
            filtered.setVisible(visibleOnly);
            total.setVisible(totalVisible);
            selected.setVisible(selectedOnly);
        }
    }

    abstract public void setData(T total, T filtered, T selected);

    abstract public BooleanKeyHandler getVisibleKeyHandler();

}
