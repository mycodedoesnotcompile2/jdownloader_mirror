package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.Component;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JLabel;
import javax.swing.JList;

import org.jdownloader.captcha.v2.SolverService;
import org.jdownloader.gui.translate._GUI;

/** Renders a {@link SolverService} with its icon and name, or {@code null} as "applies to all solvers". */
class SolverListCellRenderer extends DefaultListCellRenderer {
    private static final long serialVersionUID = 1L;

    @Override
    public Component getListCellRendererComponent(final JList list, final Object value, final int index, final boolean isSelected, final boolean cellHasFocus) {
        final JLabel label = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        if (value instanceof SolverService) {
            final SolverService service = (SolverService) value;
            label.setText(service.getName());
            label.setIcon(service.getIcon(18));
        } else {
            label.setText(_GUI.T.CaptchaRules_allSolvers());
            label.setIcon(null);
        }
        return label;
    }
}
