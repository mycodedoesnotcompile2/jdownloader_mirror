package jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog;

import org.appwork.exceptions.WTFException;
import org.appwork.storage.Storable;
import org.jdownloader.controlling.filter.Filter;

/**
 * Generic base for filters that match a single boolean status (e.g. "Package is enabled", "Link is enabled", ...) against
 * IS_TRUE/IS_FALSE. Subclasses only provide the concrete labels for both states.
 */
public abstract class BooleanStatusFilter extends Filter implements Storable {
    private Matchtype matchType = Matchtype.IS_TRUE;

    protected BooleanStatusFilter() {
        // Storable
    }

    protected BooleanStatusFilter(Matchtype matchType, boolean selected) {
        this.matchType = matchType;
        this.enabled = selected;
    }

    public Matchtype getMatchType() {
        return matchType;
    }

    public void setMatchType(Matchtype matchType) {
        this.matchType = matchType;
    }

    protected abstract String getTrueLabel();

    protected abstract String getFalseLabel();

    public String toString() {
        switch (matchType) {
        case IS_TRUE:
            return getTrueLabel();
        case IS_FALSE:
            return getFalseLabel();
        }
        throw new WTFException();
    }

    public static enum Matchtype {
        IS_TRUE,
        IS_FALSE
    }
}
