package org.jdownloader.captcha.v2.challenge.keycaptcha.dialog;

import java.awt.Component;
import java.awt.Point;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JPanel;

import jd.gui.swing.dialog.DialogType;
import net.miginfocom.swing.MigLayout;

import org.appwork.exceptions.ThrowUncheckedException;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.DomainInfo;
import org.jdownloader.captcha.v2.AbstractCaptchaDialog;
import org.jdownloader.captcha.v2.challenge.keycaptcha.KeyCaptcha;
import org.jdownloader.captcha.v2.challenge.keycaptcha.KeyCaptchaImages;
import org.jdownloader.captcha.v2.challenge.keycaptcha.KeyCaptchaPuzzleChallenge;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.updatev2.gui.LAFOptions;

public class KeyCaptchaPuzzleDialog extends AbstractCaptchaDialog<String> implements ActionListener {
    private JLayeredPane     drawPanel;
    // private BufferedImage[] kcImages;
    // private int kcSampleImg;
    private JPanel           p;
    // private final Dimension dimensions;
    ArrayList<Integer>       mouseArray;

    private KeyCaptchaImages imageData;

    public KeyCaptchaPuzzleDialog(KeyCaptchaPuzzleChallenge captchaChallenge, int flag, DialogType type, DomainInfo domain) {
        super(captchaChallenge, flag, _GUI.T.KeyCaptchaDialog(domain.getTld()), type, domain, _GUI.T.KeyCaptchaDialog_explain(domain.getTld()));
        // super(flag | Dialog.STYLE_HIDE_ICON | UIOManager.LOGIC_COUNTDOWN, title, null, null, null);
        // dimensions = new Dimension(465, 250);
        imageData = captchaChallenge.getHelper().getPuzzleData().getImages();
    }

    protected KeyCaptcha getKeyCaptcha() {
        return ((KeyCaptchaPuzzleChallenge) challenge).getHelper();
    }

    protected int getPreferredHeight() {
        return -1;
    }

    @Override
    protected int getPreferredWidth() {
        return -1;
    }

    @Override
    protected boolean isResizable() {
        return false;
    }

    @Override
    protected String createReturnValue() {
        if (!Dialog.isOK(getReturnmask())) {
            return null;
        }
        try {
            return getKeyCaptcha().sendPuzzleResult(mouseArray, getPosition(drawPanel));
        } catch (Exception e) {
            // Dirty hack as createReturnValue doesn't throw anything
            ThrowUncheckedException.throwUncheckedException(e);
            return null;
        }
    }

    private String getPosition(final JLayeredPane drawPanel) {
        int i = 0;
        String positions = "";
        final Component[] comp = drawPanel.getComponents();
        for (int c = comp.length - 1; c >= 0; c--) {
            if (comp[c].getMouseListeners().length == 0) {
                continue;
            }
            final Point p = comp[c].getLocation();
            positions += (i != 0 ? "." : "") + String.valueOf(p.x) + "." + String.valueOf(p.y);
            i++;
        }
        return positions;
    }

    // @Override
    // public Dimension getPreferredSize() {
    // return dimensions;
    // }
    @Override
    protected MigLayout getDialogLayout() {
        return super.getDialogLayout();
    }

    @Override
    protected JPanel createCaptchaPanel() {
        final KeyCaptchaPuzzleChallenge challenge = (KeyCaptchaPuzzleChallenge) this.challenge;
        // loadImage(imageUrl);
        // use a container
        p = new JPanel(new MigLayout("ins 0,wrap 1", "[grow,fill]", "[][grow,fill]"));
        JLabel lbl;
        p.add(lbl = new JLabel("<html>" + getHelpText().replace("\r\n", "<br>") + "</html>"));
        SwingUtils.setOpaque(lbl, false);
        drawPanel = new JLayeredPane();
        LAFOptions.applyBackground(LAFOptions.getInstance().getColorForPanelBackground(), p);
        LAFOptions.applyBackground(LAFOptions.getInstance().getColorForPanelBackground(), drawPanel);
        int offset = 4;
        KeyCaptchaDrawBackgroundPanel background;
        // boolean sampleImg = false;
        drawPanel.add(background = new KeyCaptchaDrawBackgroundPanel(imageData.backgroundImage), JLayeredPane.DEFAULT_LAYER, JLayeredPane.DEFAULT_LAYER);
        mouseArray = new ArrayList<Integer>();
        drawPanel.add(new KeyCaptchaDragPieces(imageData.sampleImage, offset, true, mouseArray, challenge), JLayeredPane.DEFAULT_LAYER + 0, JLayeredPane.DEFAULT_LAYER + 0);
        System.out.println("PIeces " + imageData.pieces.size());
        for (int i = 0; i < imageData.pieces.size(); i++) {
            drawPanel.add(new KeyCaptchaDragPieces(imageData.pieces.get(i), offset, false, mouseArray, challenge), JLayeredPane.DEFAULT_LAYER + (i + 1), JLayeredPane.DEFAULT_LAYER + (i + 1));
            offset += 4;
        }
        p.add(drawPanel);
        drawPanel.setPreferredSize(background.getPreferredSize());
        return p;
    }
}