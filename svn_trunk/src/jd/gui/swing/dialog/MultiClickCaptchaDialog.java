//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.

package jd.gui.swing.dialog;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;

import javax.swing.JComponent;

import org.appwork.swing.components.ExtTextField;
import org.appwork.uio.UIOManager;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.DomainInfo;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickedPoint;
import org.jdownloader.gui.translate._GUI;

/**
 * This Dialog is used to display a Inputdialog for the captchas
 */
public class MultiClickCaptchaDialog extends AbstractImageCaptchaDialog<MultiClickedPoint> {

    private ArrayList<Point> rp = null;
    private final MultiClickCaptchaChallenge clickChallenge;
    private ExtTextField counterField;

    // public ClickCaptchaDialog(final int flag, DialogType type, final DomainInfo DomainInfo, final Image image, final String explain) {
    // this(flag, type, DomainInfo, new Image[] { image }, explain);
    // }

    public MultiClickCaptchaDialog(MultiClickCaptchaChallenge captchaChallenge, int flag, DialogType type, DomainInfo domainInfo, Image[] images, String explain) {
        /* Single click captchas are confirmed by the click itself, see mouseReleased. */
        super(captchaChallenge, flag | Dialog.STYLE_HIDE_ICON | (captchaChallenge.isSingleClick() ? UIOManager.BUTTONS_HIDE_OK : 0), _GUI.T.gui_captchaWindow_askForInput(domainInfo.getTld()), type, domainInfo, explain, images);
        this.clickChallenge = captchaChallenge;
    }

    @Override
    public JComponent layoutDialogContent() {
        final JComponent ret = super.layoutDialogContent();
        iconPanel.setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
        iconPanel.setToolTipText(getHelpText());
        iconPanel.addMouseListener(new MouseListener() {

            @Override
            public void mouseReleased(MouseEvent e) {
                final Point resultPoint = e.getPoint();
                resultPoint.x -= getOffset().x;
                resultPoint.y -= getOffset().y;
                resultPoint.x *= getScaleFaktor();
                resultPoint.y *= getScaleFaktor();
                if (rp == null) {
                    rp = new ArrayList<Point>();
                }
                rp.add(resultPoint);
                updateClickCounter();
                iconPanel.repaint();
                final int maxClicks = clickChallenge.getMaxClicks();
                if (maxClicks > 0 && rp.size() >= maxClicks) {
                    /* Expected amount of clicks reached -> confirm automatically. */
                    setReturnmask(true);
                    dispose();
                }
            }

            @Override
            public void mousePressed(MouseEvent e) {
            }

            @Override
            public void mouseExited(MouseEvent e) {
            }

            @Override
            public void mouseEntered(MouseEvent e) {
            }

            @Override
            public void mouseClicked(MouseEvent e) {
            }
        });
        return ret;
    }

    @Override
    protected JComponent createInputComponent() {
        final ExtTextField ret = new ExtTextField();
        ret.setEditable(false);
        counterField = ret;
        updateClickCounter();
        return ret;
    }

    /**
     * Shows the amount of clicks done so far, e.g. "3/8 Klicks" (or "3 Klicks" if there is no maximum), followed by the help text.
     */
    private void updateClickCounter() {
        if (counterField == null) {
            return;
        }
        final int clicks = rp == null ? 0 : rp.size();
        final int maxClicks = clickChallenge.getMaxClicks();
        final String counter;
        if (maxClicks > 0) {
            counter = clicks + "/" + maxClicks + " Klicks";
        } else {
            counter = clicks + " Klicks";
        }
        counterField.setText(counter + " - " + getHelpText());
    }

    /**
     * Marks every click with a red X. Click points are stored in image coordinates, so they have to be converted back to the scaled
     * display coordinates.
     */
    @Override
    protected void paintIconComponent(final Graphics g, final int width, final int height, final int xOffset, final int yOffset, final Image scaled) {
        if (rp == null || rp.isEmpty()) {
            return;
        }
        final double scale = getScaleFaktor();
        if (scale <= 0) {
            return;
        }
        final Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setColor(Color.RED);
            g2.setStroke(new BasicStroke(2.5f));
            final int size = 6;
            for (final Point p : rp) {
                final int x = (int) Math.round(p.x / scale) + xOffset;
                final int y = (int) Math.round(p.y / scale) + yOffset;
                g2.drawLine(x - size, y - size, x + size, y + size);
                g2.drawLine(x - size, y + size, x + size, y - size);
            }
        } finally {
            g2.dispose();
        }
    }

    @Override
    protected MultiClickedPoint createReturnValue() {
        if (rp == null) {
            return null;
        }
        final MultiClickedPoint mcp = new MultiClickedPoint();
        final int[] x = new int[rp.size()];
        final int[] y = new int[rp.size()];
        int i = 0;
        for (final Point p : rp) {
            x[i] = p.x;
            y[i] = p.y;
            i++;
        }
        mcp.setX(x);
        mcp.setY(y);
        return mcp;
    }

}