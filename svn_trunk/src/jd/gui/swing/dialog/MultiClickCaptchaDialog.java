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
import java.awt.event.MouseMotionAdapter;
import java.util.ArrayList;

import javax.swing.JComponent;

import org.appwork.swing.components.ExtTextField;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.DomainInfo;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickCaptchaChallenge;
import org.jdownloader.captcha.v2.challenge.multiclickcaptcha.MultiClickedPoint;
import org.jdownloader.gui.translate._GUI;

/**
 * This Dialog is used to display a Inputdialog for the captchas
 */
public class MultiClickCaptchaDialog extends AbstractImageCaptchaDialog<MultiClickedPoint> {

    /* Half size of the red X in pixels. */
    private static final int MARKER_SIZE       = 14;
    /* Clicking within this distance (pixels) of a marker removes it. */
    private static final int MARKER_HIT_RADIUS = 18;
    private ArrayList<Point> rp = null;
    private final MultiClickCaptchaChallenge clickChallenge;
    private ExtTextField counterField;

    // public ClickCaptchaDialog(final int flag, DialogType type, final DomainInfo DomainInfo, final Image image, final String explain) {
    // this(flag, type, DomainInfo, new Image[] { image }, explain);
    // }

    public MultiClickCaptchaDialog(MultiClickCaptchaChallenge captchaChallenge, int flag, DialogType type, DomainInfo domainInfo, Image[] images, String explain) {
        super(captchaChallenge, flag | Dialog.STYLE_HIDE_ICON,_GUI.T.gui_captchaWindow_askForInput(domainInfo.getTld()), type, domainInfo, explain, images);
        this.clickChallenge = captchaChallenge;
    }

    @Override
    public JComponent layoutDialogContent() {
        final JComponent ret = super.layoutDialogContent();
        iconPanel.setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
        iconPanel.setToolTipText(getHelpText());
        /* okButton exists now, so apply the initial state (0 clicks). */
        updateOkButton();
        iconPanel.addMouseListener(new MouseListener() {

            @Override
            public void mouseReleased(MouseEvent e) {
                final Point existing = getClickAt(e.getPoint());
                if (existing != null) {
                    /* Click on an existing marker -> remove that click again. */
                    rp.remove(existing);
                    iconPanel.setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
                    updateClickCounter();
                    iconPanel.repaint();
                    return;
                }
                final Point resultPoint = e.getPoint();
                resultPoint.x -= getOffset().x;
                resultPoint.y -= getOffset().y;
                resultPoint.x *= getScaleFaktor();
                resultPoint.y *= getScaleFaktor();
                if (rp == null) {
                    rp = new ArrayList<Point>();
                }
                final int maxClicks = clickChallenge.getMaxClicks();
                if (maxClicks > 0 && rp.size() >= maxClicks) {
                    /* Maximum reached: the user has to remove a click first. */
                    return;
                }
                rp.add(resultPoint);
                updateClickCounter();
                iconPanel.repaint();
                if (maxClicks == 1) {
                    /* Single click captcha -> confirm automatically. Otherwise the user must be able to remove wrong clicks first. */
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
        iconPanel.addMouseMotionListener(new MouseMotionAdapter() {
            @Override
            public void mouseMoved(MouseEvent e) {
                /* Hand cursor over a click marker = clicking removes it. */
                final boolean overMarker = getClickAt(e.getPoint()) != null;
                iconPanel.setCursor(Cursor.getPredefinedCursor(overMarker ? Cursor.HAND_CURSOR : Cursor.CROSSHAIR_CURSOR));
                iconPanel.setToolTipText(overMarker ? _GUI.T.MultiClickCaptchaDialog_marker_tooltip() : getHelpText());
            }
        });
        return ret;
    }

    /** Returns the nearest already placed click whose marker (hit area) contains the given display position, or null. */
    private Point getClickAt(final Point displayPoint) {
        if (rp == null || rp.isEmpty()) {
            return null;
        }
        final double scale = getScaleFaktor();
        if (scale <= 0) {
            return null;
        }
        Point best = null;
        double bestDistance = Double.MAX_VALUE;
        for (final Point p : rp) {
            final double dx = p.x / scale + getOffset().x - displayPoint.x;
            final double dy = p.y / scale + getOffset().y - displayPoint.y;
            final double distance = Math.sqrt(dx * dx + dy * dy);
            if (distance <= MARKER_HIT_RADIUS && distance < bestDistance) {
                best = p;
                bestDistance = distance;
            }
        }
        return best;
    }

    @Override
    protected JComponent createInputComponent() {
        /* Click info below the image as plain text. The challenge description is shown above the image, see addBeforeImage. */
        counterField = new ExtTextField();
        counterField.setEditable(false);
        updateClickCounter();
        return counterField;
    }

    /** Shows the amount of clicks done so far and how many are allowed. */
    private void updateClickCounter() {
        if (counterField == null) {
            return;
        }
        final int clicks = rp == null ? 0 : rp.size();
        final int maxClicks = clickChallenge.getMaxClicks();
        final int minClicks = clickChallenge.getMinClicks();
        final String counter;
        if (maxClicks > 0 && minClicks == maxClicks) {
            /* Exact amount required */
            counter = _GUI.T.MultiClickCaptchaDialog_clicks_exact(clicks, maxClicks);
        } else if (maxClicks > 0) {
            /* Optional amount: anything between min and max is allowed */
            counter = _GUI.T.MultiClickCaptchaDialog_clicks_range(clicks, maxClicks, Math.max(minClicks, 0));
        } else {
            counter = _GUI.T.MultiClickCaptchaDialog_clicks_unlimited(clicks, Math.max(minClicks, 0));
        }
        counterField.setText(counter);
        updateOkButton();
    }

    /**
     * Disables the OK button as long as the minimum amount of clicks has not been reached yet.
     */
    private void updateOkButton() {
        if (okButton == null) {
            return;
        }
        final int clicks = rp == null ? 0 : rp.size();
        okButton.setEnabled(clicks >= clickChallenge.getMinClicks());
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
            final int size = MARKER_SIZE;
            for (final Point p : rp) {
                final int x = (int) Math.round(p.x / scale) + xOffset;
                final int y = (int) Math.round(p.y / scale) + yOffset;
                /* White outline first so the marker is visible on any background. */
                g2.setColor(Color.WHITE);
                g2.setStroke(new BasicStroke(7f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
                g2.drawLine(x - size, y - size, x + size, y + size);
                g2.drawLine(x - size, y + size, x + size, y - size);
                g2.setColor(Color.RED);
                g2.setStroke(new BasicStroke(4f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
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