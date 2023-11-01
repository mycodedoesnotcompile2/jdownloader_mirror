/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.swing.components.circlebar;

import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.event.MouseEvent;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BoundedRangeModel;
import javax.swing.DefaultBoundedRangeModel;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.basic.BasicProgressBarUI;

import org.appwork.swing.components.IconComponentInterface;
import org.appwork.swing.components.tooltips.ExtTooltip;
import org.appwork.swing.components.tooltips.ToolTipController;
import org.appwork.swing.components.tooltips.ToolTipHandler;
import org.appwork.swing.components.tooltips.TooltipFactory;
import org.appwork.swing.components.tooltips.TooltipTextDelegateFactory;
import org.appwork.utils.event.predefined.changeevent.ChangeEventSender;

public class CircledProgressBar extends JComponent implements ToolTipHandler, IconComponentInterface {

    /**
     *
     */
    private static final long       serialVersionUID = -3518805542131925575L;

    private BoundedRangeModel       model;

    private ChangeListener          changeListener;

    private boolean                 indeterminate;
    private final ChangeEventSender eventSender;
    private IconPainter             valueClipPainter;
    private IconPainter             nonvalueClipPainter;
    /**
     * @see #getUIClassID
     */
    private static final String     UI_CLASS_ID      = "CircleProgressBarUI";

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.tooltips.ToolTipHandler#createExtTooltip()
     */
    private TooltipFactory          tooltipFactory;

    /**
     *
     */
    public CircledProgressBar() {
        this(new DefaultBoundedRangeModel());

    }

    public boolean isFocusable() {
        return false;
    }

    /**
     * @param model
     */
    public CircledProgressBar(final BoundedRangeModel model) {
        eventSender = new ChangeEventSender();
        tooltipFactory = new TooltipTextDelegateFactory(this);
        installPainer();
        setModel(model);
        BasicProgressBarUI.class.getAnnotations();
        updateUI();
        setIndeterminate(false);

    }

    /**
     * delegates the changeevents. you can override this method to implement different eventhandling
     *
     * @return
     */
    protected ChangeListener createChangeListener() {

        return new ChangeListener() {

            @Override
            public void stateChanged(final ChangeEvent e) {
                eventSender.fireEvent(new org.appwork.utils.event.predefined.changeevent.ChangeEvent(e.getSource()));
            }

        };
    }

    @Override
    public boolean isTooltipWithoutFocusEnabled() {
        // TODO Auto-generated method stub
        return true;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.tooltips.ToolTipHandler#createExtTooltip (java.awt.Point)
     */
    @Override
    public ExtTooltip createExtTooltip(final Point mousePosition) {
        return tooltipFactory.createTooltip();
    }

    /**
     * @return
     */
    public int getAnimationFPS() {
        return 25;
    }

    /**
     * @return
     */
    public float getCyclesPerSecond() {

        return 0.5f;
    }

    public ChangeEventSender getEventSender() {
        return eventSender;
    }

    public int getMaximum() {
        return getModel().getMaximum();
    }

    public int getMinimum() {
        return getModel().getMinimum();
    }

    public BoundedRangeModel getModel() {
        return model;
    }

    public IconPainter getNonvalueClipPainter() {
        return nonvalueClipPainter;
    }

    public TooltipFactory getTooltipFactory() {
        return tooltipFactory;
    }

    @Override
    public Dimension getSize() {
        // TODO Auto-generated method stub
        return super.getSize();
    }

    @Override
    public void setSize(final int width, final int height) {
        // TODO Auto-generated method stub
        super.setSize(width, height);
    }

    @Override
    public void setSize(final Dimension d) {
        // TODO Auto-generated method stub
        super.setSize(d);
    }

    @Override
    public String getUIClassID() {
        return CircledProgressBar.UI_CLASS_ID;
    }

    public int getValue() {
        return getModel().getValue();
    }

    public IconPainter getValueClipPainter() {
        return valueClipPainter;
    }

    /**
     *
     */
    private void installPainer() {
        valueClipPainter = new IconPainter() {

            @Override
            public void paint(final CircledProgressBar bar, final Graphics2D g2, final Shape shape, final int diameter, final double progress) {
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2.setColor(CircledProgressBar.this.getForeground());
                final Area a = new Area(shape);
                a.intersect(new Area(new Ellipse2D.Float(-diameter / 2, -diameter / 2, diameter, diameter)));

                g2.fill(a);

            }

            private Dimension dimension;

            {
                dimension = new Dimension(32, 32);
            }

            @Override
            public Dimension getPreferredSize() {
                return dimension;
            }
        };

    }

    /**
     * This methods flips value and nonvalue painter. This can be used to simulate a continuous running bar, just by inverting the painters
     * and setting the value to 0 if the model reaches it's maximum
     */
    protected void invertPainter() {

        final IconPainter fg = valueClipPainter;
        valueClipPainter = nonvalueClipPainter;
        nonvalueClipPainter = fg;

    }

    public boolean isIndeterminate() {
        return indeterminate;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.tooltips.ToolTipHandler# isTooltipDisabledUntilNextRefocus()
     */
    @Override
    public boolean isTooltipDisabledUntilNextRefocus() {
        // TODO Auto-generated method stub
        return true;
    }

    /**
     * @param b
     */
    public void setIndeterminate(final boolean newValue) {
        final boolean oldValue = indeterminate;
        if (oldValue == newValue) {
            return;
        }
        indeterminate = newValue;
        this.firePropertyChange("indeterminate", oldValue, indeterminate);
    }

    public void setMaximum(final int n) {
        getModel().setMaximum(n);
    }

    public void setMinimum(final int n) {
        getModel().setMinimum(n);
    }

    /**
     * @param model
     */
    public synchronized void setModel(final BoundedRangeModel model) {
        if (this.model == model) {
            return;
        }
        if (this.model != null) {
            model.removeChangeListener(changeListener);
        }
        changeListener = createChangeListener();
        model.addChangeListener(changeListener);
        this.model = model;

        this.repaint();

    }

    public void setNonvalueClipPainter(final IconPainter backgroundPainter) {
        nonvalueClipPainter = backgroundPainter;
    }

    /**
     * @param string
     */
    public void setString(final String string) {
        // TODO Auto-generated method stub

    }

    /**
     * @param b
     */
    public void setStringPainted(final boolean b) {
        // TODO Auto-generated method stub

    }

    public void setTooltipFactory(final TooltipFactory tooltipFactory) {
        this.tooltipFactory = tooltipFactory;
        ToolTipController.getInstance().register(this);
    }

    @Override
    public void setToolTipText(final String text) {

        putClientProperty(JComponent.TOOL_TIP_TEXT_KEY, text);

        if (text == null || text.length() == 0) {
            ToolTipController.getInstance().unregister(this);
        } else {
            ToolTipController.getInstance().register(this);
        }
    }

    public void setUI(final CircleProgressBarUI ui) {
        super.setUI(ui);

    }

    public void setValue(final int n) {
        final BoundedRangeModel brm = getModel();
        final int oldValue = brm.getValue();
        brm.setValue(n);

    }

    /**
     * @param iconPainter
     */
    public void setValueClipPainter(final IconPainter iconPainter) {
        valueClipPainter = iconPainter;

    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.tooltips.ToolTipHandler#updateTooltip(org .appwork.swing.components.tooltips.ExtTooltip,
     * java.awt.event.MouseEvent)
     */
    @Override
    public int getTooltipDelay(final Point mousePositionOnScreen) {
        return 0;
    }

    @Override
    public boolean updateTooltip(final ExtTooltip activeToolTip, final MouseEvent e) {

        return false;
    }

    /**
     * Resets the UI property to a value from the current look and feel.
     *
     * @see JComponent#updateUI
     */
    @Override
    public void updateUI() {
        // final CircleProgressBarUI newUI = (CircleProgressBarUI)
        // UIManager.getUI(this);
        this.setUI(new BasicCircleProgressBarUI());
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.IconComponentInterface#getIcon(int, int)
     */
    @Override
    public List<Icon> getIcons(int x, int y) {
        ArrayList<Icon> ret = new ArrayList<Icon>();

        if (valueClipPainter != null && valueClipPainter instanceof ImagePainter) {
            Icon ico = ((ImagePainter) valueClipPainter).getImage();
            if (ico != null) {
                ret.add(ico);
            }

        }
        if (nonvalueClipPainter != null && nonvalueClipPainter instanceof ImagePainter) {
            Icon ico = ((ImagePainter) nonvalueClipPainter).getImage();
            if (ico != null) {
                ret.add(ico);
            }

        }

        return ret;
    }

}
