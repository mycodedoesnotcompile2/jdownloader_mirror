package org.jdownloader.settings.advanced;

import org.jdownloader.gui.translate._GUI;

public class RangeValidator extends Validator {

    private Number min;
    private Number steps = 1;

    public Number getMin() {
        return min;
    }

    public Number getSteps() {
        return steps;
    }

    public void setSteps(Number steps) {
        this.steps = steps;
    }

    public void setMin(Number min) {
        this.min = min;
    }

    public String toString() {
        return _GUI.T.RangeValidator_toString_object_(min.toString(), max.toString());
    }

    public Number getMax() {
        return max;
    }

    public void setMax(Number max) {
        this.max = max;
    }

    private Number max;

    protected RangeValidator(Number min, Number max, Number steps) {
        this.min = min;
        this.max = max;
        this.steps = steps;
    }

}
