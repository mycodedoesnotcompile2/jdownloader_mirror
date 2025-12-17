package jd.gui.swing.jdgui.views.settings.components;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.JSpinner;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.appwork.storage.config.handler.IntegerKeyHandler;
import org.appwork.storage.config.swing.models.ConfigIntSpinnerModel;
import org.appwork.swing.components.ExtSpinner;
import org.appwork.utils.reflection.Clazz;

public class Spinner extends ExtSpinner implements SettingsComponent {
    /**
     *
     */
    private static final long               serialVersionUID = 1L;
    private StateUpdateEventSender<Spinner> eventSender;
    private final AtomicInteger             setting          = new AtomicInteger(0);

    public Spinner(int min, int max) {
        this(new SpinnerNumberModel(min, min, max, 1));
    }

    public Spinner(SpinnerNumberModel extSpinnerConfigModel) {
        super(extSpinnerConfigModel);
        init();
    }

    protected String getDecimalFormatPattern(SpinnerModel model) {
        if (model instanceof SpinnerNumberModel) {
            final Number stepSize = ((SpinnerNumberModel) model).getStepSize();
            if (stepSize != null && Clazz.isFloatingPointNumber(stepSize.getClass())) {
                final Set<String> numbers = new HashSet<String>();
                final Comparable<?> max = ((SpinnerNumberModel) model).getMaximum();
                final Comparable<?> min = ((SpinnerNumberModel) model).getMinimum();
                if (max instanceof Number) {
                    numbers.add(max.toString());
                }
                if (min instanceof Number) {
                    numbers.add(min.toString());
                }
                numbers.add(stepSize.toString());
                int nachKomma = 1;
                int vorKomma = 1;
                for (final String number : numbers) {
                    nachKomma = Math.max(nachKomma, number.contains(".") ? number.length() - number.indexOf('.') - 1 : 0);
                    vorKomma = Math.max(vorKomma, number.contains(".") ? number.indexOf('.') : number.length());
                }
                final char[] arr = new char[vorKomma + 1 + nachKomma];
                Arrays.fill(arr, '#');
                arr[vorKomma] = '.';
                return String.valueOf(arr);
            }
        }
        return "#";
    }

    protected void init() {
        setEditor(new JSpinner.NumberEditor(this, getDecimalFormatPattern(getModel())));
        eventSender = new StateUpdateEventSender<Spinner>();
        this.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                // System.out.println(1 + " -c " + (setting.get() == 0));
                if (setting.get() == 0) {
                    eventSender.fireEvent(new StateUpdateEvent<Spinner>(Spinner.this));
                }
            }
        });
    }

    public Spinner(IntegerKeyHandler cfg) {
        this(new ConfigIntSpinnerModel(cfg));
    }

    @Override
    public void setModel(SpinnerModel model) {
        setting.getAndIncrement();
        try {
            super.setModel(model);
        } finally {
            setting.decrementAndGet();
        }
    }

    public void setValue(Number value) {
        setting.getAndIncrement();
        try {
            super.setValue(value);
        } finally {
            setting.decrementAndGet();
        }
    }

    public void setValue(long value) {
        setting.getAndIncrement();
        try {
            super.setValue(value);
        } finally {
            setting.decrementAndGet();
        }
    }

    public void setValue(int value) {
        setting.getAndIncrement();
        try {
            super.setValue(value);
        } finally {
            setting.decrementAndGet();
        }
    }

    /**
     * @deprecated USer {@link #setValue(int)} or {@link #setValue(long)} or {@link #setValue(Number)} instead!!
     */
    public void setValue(Object value) {
        super.setValue(value);
    }

    public String getConstraints() {
        return "sgy LINE";
    }

    public boolean isMultiline() {
        return false;
    }

    /**
     * Set the Spinner renderer and editor format.
     *
     * @see http ://download.oracle.com/javase/1.4.2/docs/api/java/text/DecimalFormat .html
     * @param formatString
     */
    public void setFormat(String formatString) {
        setEditor(new JSpinner.NumberEditor(this, formatString));
    }

    public void addStateUpdateListener(StateUpdateListener listener) {
        eventSender.addListener(listener);
    }
}
