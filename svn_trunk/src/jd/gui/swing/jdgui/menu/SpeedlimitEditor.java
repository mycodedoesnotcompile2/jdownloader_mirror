package jd.gui.swing.jdgui.menu;

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.JLabel;
import javax.swing.JSpinner.DefaultEditor;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import jd.controlling.downloadcontroller.DownloadLinkCandidate;
import jd.controlling.downloadcontroller.DownloadLinkCandidateResult;
import jd.controlling.downloadcontroller.DownloadWatchDog;
import jd.controlling.downloadcontroller.DownloadWatchDogProperty;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.controlling.downloadcontroller.event.DownloadWatchdogListener;
import net.miginfocom.swing.MigLayout;

import org.appwork.storage.config.swing.models.ConfigIntSpinnerModel;
import org.appwork.swing.components.ExtCheckBox;
import org.appwork.swing.components.SizeSpinner;
import org.appwork.utils.formatter.SizeFormatter;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.settings.GraphicalUserInterfaceSettings.SPEEDUNIT;
import org.jdownloader.settings.staticreferences.CFG_GUI;

public class SpeedlimitEditor extends MenuEditor implements DownloadWatchdogListener {
    /**
     *
     */
    private static final long      serialVersionUID = 5406904697287119514L;
    private JLabel                 lbl;
    private SizeSpinner            speedSpinner;
    private SizeSpinner            pauseSpinner;
    private ExtCheckBox            speedCheckbox;
    private ExtCheckBox            pauseCheckbox;
    private static final SPEEDUNIT maxSpeedUnit     = CFG_GUI.CFG.getMaxSpeedUnit();

    public SpeedlimitEditor() {
        this(false);
    }

    public SpeedlimitEditor(boolean b) {
        super(b);
        setLayout(new MigLayout("ins " + getInsetsString() + ", hidemode 3", "6[grow,fill][][]", "[" + getComponentHeight() + "!]"));
        setOpaque(false);
        lbl = getLbl(_GUI.T.SpeedlimitEditor_SpeedlimitEditor_(), new AbstractIcon(IconKey.ICON_SPEED, 18));
        /* explain that the global limit is split statically between all active connections */
        setToolTipText(_GUI.T.SpeedlimitEditor_tooltip());
        /* regular controls, natively bound to the download speed limit config */
        speedSpinner = createSpinner(new ConfigIntSpinnerModel(org.jdownloader.settings.staticreferences.CFG_GENERAL.DOWNLOAD_SPEED_LIMIT));
        speedCheckbox = new ExtCheckBox(org.jdownloader.settings.staticreferences.CFG_GENERAL.DOWNLOAD_SPEED_LIMIT_ENABLED, lbl);
        speedCheckbox.setVerticalAlignment(SwingConstants.CENTER);
        /*
         * Pause controls, laid out in the same cells as the regular ones and only shown while paused. The pause spinner
         * is natively bound to the separate pause speed config (so keyboard up/down and the displayed value work
         * correctly), and the pause checkbox is a display-only dummy: always checked and greyed out. Pause mode
         * therefore never touches the persistent download speed limit config.
         */
        pauseSpinner = createSpinner(new ConfigIntSpinnerModel(org.jdownloader.settings.staticreferences.CFG_GENERAL.PAUSE_SPEED));
        pauseCheckbox = new ExtCheckBox();
        pauseCheckbox.setSelected(true);
        pauseCheckbox.setEnabled(false);
        pauseCheckbox.setVerticalAlignment(SwingConstants.CENTER);
        /* start in the regular (non-pause) look; the pause controls are shown on state change */
        pauseSpinner.setVisible(false);
        pauseCheckbox.setVisible(false);
        add(lbl, "cell 0 0");
        add(speedCheckbox, "cell 1 0, width 20!");
        add(pauseCheckbox, "cell 1 0, width 20!");
        add(speedSpinner, "cell 2 0, width " + getEditorWidth() + "!");
        add(pauseSpinner, "cell 2 0, width " + getEditorWidth() + "!");
        DownloadWatchDog.getInstance().getEventSender().addListener(this, true);
        DownloadWatchDog.getInstance().notifyCurrentState(this);
    }

    /**
     * Creates a speed spinner with the shared text/format behaviour, bound to the given config model.
     */
    private SizeSpinner createSpinner(final ConfigIntSpinnerModel model) {
        final SizeSpinner sp = new SizeSpinner(model) {
            /**
             *
             */
            private static final long serialVersionUID = 1L;

            @Override
            protected Object textToObject(final String input) {
                String text = input;
                if (text != null) {
                    text = text.replaceFirst("(?i)/s(ec|secound)?\\s*$", "");
                }
                final boolean isBit = text != null && text.matches("(?i).+bit\\s*$");
                if (text != null) {
                    text = text.replaceFirst("(?i)it\\s*$", "");
                }
                Object ret;
                if (text != null && text.trim().matches("^[0-9]+$")) {
                    ret = text + " kb";
                }
                final boolean kibi = text != null && (text.contains("i") || text.contains("I"));
                ret = SizeFormatter.getSize(numberFormat, text, kibi, true);
                if (ret instanceof Number && isBit) {
                    ret = ((Number) ret).intValue() / 8;
                }
                return ret;
            }

            protected String longToText(long longValue) {
                if (longValue <= 0) {
                    return _GUI.T.SpeedlimitEditor_format(SPEEDUNIT.formatValue(maxSpeedUnit, numberFormat, 0));
                } else {
                    return _GUI.T.SpeedlimitEditor_format(SPEEDUNIT.formatValue(maxSpeedUnit, numberFormat, longValue));
                }
            }
        };
        try {
            ((DefaultEditor) sp.getEditor()).getTextField().addFocusListener(new FocusListener() {
                @Override
                public void focusLost(FocusEvent e) {
                }

                @Override
                public void focusGained(FocusEvent e) {
                    // requires invoke later!
                    SwingUtilities.invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            ((DefaultEditor) sp.getEditor()).getTextField().selectAll();
                        }
                    });
                }
            });
        } catch (Exception e) {
            e.printStackTrace();
            // too much fancy Casting.
        }
        return sp;
    }

    @Override
    public void onDownloadWatchdogDataUpdate() {
    }

    /**
     * Switches between the regular controls and the pause controls. While paused the pause spinner/checkbox are shown on
     * top of (in the same cells as) the regular ones, so the editor displays and edits the pause speed without ever
     * touching the persistent download speed limit config.
     */
    private void updateEditorState(final boolean paused) {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                speedCheckbox.setVisible(!paused);
                speedSpinner.setVisible(!paused);
                pauseCheckbox.setVisible(paused);
                pauseSpinner.setVisible(paused);
                /* while paused the limit is always active, so keep the label enabled; otherwise follow the checkbox */
                lbl.setEnabled(paused || speedCheckbox.isSelected());
                revalidate();
                repaint();
            }
        };
    }

    @Override
    public void onDownloadWatchdogStateIsIdle() {
        updateEditorState(false);
    }

    @Override
    public void onDownloadWatchdogStateIsPause() {
        updateEditorState(true);
    }

    @Override
    public void onDownloadWatchdogStateIsRunning() {
        updateEditorState(false);
    }

    @Override
    public void onDownloadWatchdogStateIsStopped() {
        updateEditorState(false);
    }

    @Override
    public void onDownloadWatchdogStateIsStopping() {
        updateEditorState(false);
    }

    @Override
    public void onDownloadControllerStart(SingleDownloadController downloadController, DownloadLinkCandidate candidate) {
    }

    @Override
    public void onDownloadControllerStopped(SingleDownloadController downloadController, DownloadLinkCandidate candidate, DownloadLinkCandidateResult result) {
    }

    @Override
    public void onDownloadWatchDogPropertyChange(DownloadWatchDogProperty propertyChange) {
    }
}
