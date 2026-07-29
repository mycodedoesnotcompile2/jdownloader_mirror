package jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog;

import org.appwork.storage.Storable;
import org.jdownloader.gui.translate._GUI;

public class LinkEnabledFilter extends BooleanStatusFilter implements Storable {
    public LinkEnabledFilter() {
        // Storable
    }

    public LinkEnabledFilter(Matchtype matchType, boolean selected) {
        super(matchType, selected);
    }

    @Override
    protected String getTrueLabel() {
        return _GUI.T.FilterRule_BooleanStatusFilter_generic_enabled();
    }

    @Override
    protected String getFalseLabel() {
        return _GUI.T.FilterRule_BooleanStatusFilter_generic_disabled();
    }

    public static String getTrueLabelStatic() {
        return _GUI.T.FilterRule_BooleanStatusFilter_generic_enabled();
    }

    public static String getFalseLabelStatic() {
        return _GUI.T.FilterRule_BooleanStatusFilter_generic_disabled();
    }
}
