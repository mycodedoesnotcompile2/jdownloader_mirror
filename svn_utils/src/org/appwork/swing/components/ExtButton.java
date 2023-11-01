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
package org.appwork.swing.components;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;

import org.appwork.swing.action.BasicAction;
import org.appwork.swing.components.tooltips.ExtTooltip;
import org.appwork.swing.components.tooltips.ToolTipController;
import org.appwork.swing.components.tooltips.ToolTipHandler;
import org.appwork.swing.components.tooltips.TooltipFactory;
import org.appwork.swing.components.tooltips.TooltipTextDelegateFactory;
import org.appwork.utils.KeyUtils;
import org.appwork.utils.StringUtils;

public class ExtButton extends JButton implements ToolTipHandler {
    /**
     *
     */
    private static final long serialVersionUID    = -7151290227825542967L;
    private TooltipFactory    tooltipFactory;
    private MouseAdapter      rollOverlistener;
    private KeyStroke         accelerator;
    private int               calculatingPrefSize = 0;

    /**
     *
     */
    public ExtButton() {
        this(null);
        // TODO Auto-generated constructor stub
    }

    protected void actionPropertyChanged(final Action action, final String propertyName) {
        super.actionPropertyChanged(action, propertyName);
        if (propertyName == Action.ACCELERATOR_KEY) {
            setAccelerator((KeyStroke) action.getValue(Action.ACCELERATOR_KEY));
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.JComponent#getPreferredSize()
     */
    @Override
    public Dimension getPreferredSize() {
        if (getIcon() == null || getVerticalTextPosition() != SwingConstants.CENTER) {
            return super.getPreferredSize();
        }
        // buttons with icons should have the same height as buttons without icons.
        calculatingPrefSize++;
        Dimension withoutIcon;
        try {
            withoutIcon = super.getPreferredSize();
            if (super.getIcon() == null) {
                return withoutIcon;
            }
        } finally {
            calculatingPrefSize--;
        }
        Dimension withIcon = super.getPreferredSize();
        withIcon.height = withoutIcon.height;
        return withIcon;
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.JComponent#getMinimumSize()
     */
    @Override
    public Dimension getMinimumSize() {
        return getPreferredSize();
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.AbstractButton#getIcon()
     */
    @Override
    public Icon getIcon() {
        if (calculatingPrefSize > 0) {
            return null;
        }
        return super.getIcon();
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.AbstractButton#setEnabled(boolean)
     */
    @Override
    public void setEnabled(final boolean b) {
        // TODO Auto-generated method stub
        super.setEnabled(b);
    }

    /**
     * @param autoDetectAction
     */
    public ExtButton(final AbstractAction action) {
        super(action);
        tooltipFactory = new TooltipTextDelegateFactory(this);
        if (!StringUtils.isEmpty(getToolTipText())) {
            setTooltipsEnabled(true);
        }
        if (action instanceof BasicAction) {
            if (((BasicAction) action).getTooltipFactory() != null) {
                tooltipFactory = ((BasicAction) action).getTooltipFactory();
                setTooltipsEnabled(true);
            }
            if (!StringUtils.isEmpty(((BasicAction) action).getTooltipText())) {
                setTooltipsEnabled(true);
            }
            setAccelerator((KeyStroke) action.getValue(Action.ACCELERATOR_KEY));
        }
    }

    public void setAccelerator(final KeyStroke newAccelerator) {
        final InputMap inputmap = getInputMap(JButton.WHEN_IN_FOCUSED_WINDOW);
        if (accelerator != null) {
            inputmap.remove(newAccelerator);
            getActionMap().remove(KeyUtils.getShortcutString(accelerator, true));
            accelerator = null;
            setTooltipsEnabled(StringUtils.isNotEmpty(getToolTipText()));
        }
        if (newAccelerator != null) {
            accelerator = newAccelerator;
            final String shortcutString = KeyUtils.getShortcutString(newAccelerator, true);
            inputmap.put(newAccelerator, shortcutString);
            setTooltipsEnabled(true);
            getActionMap().put(shortcutString, new AbstractAction() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    doClick();
                }
            });
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.tooltips.ToolTipHandler#createExtTooltip (java.awt.Point)
     */
    @Override
    public ExtTooltip createExtTooltip(final Point mousePosition) {
        return getTooltipFactory().createTooltip();
    }

    public TooltipFactory getTooltipFactory() {
        if (getAction() instanceof BasicAction) {
            final TooltipFactory ret = ((BasicAction) getAction()).getTooltipFactory();
            if (ret != null) {
                return ret;
            }
        }
        return tooltipFactory;
    }

    @Override
    public String getToolTipText() {
        String ret = super.getToolTipText();
        if (ret == null || "".equals(ret)) {
            if (getAction() instanceof BasicAction) {
                ret = ((BasicAction) getAction()).getTooltipText();
            }
        }
        if (accelerator != null) {
            if (ret == null) {
                ret = "";
            }
            if (ret.length() > 0) {
                ret += " ";
            }
            ret += "[" + KeyUtils.getShortcutString(accelerator, true) + "]";
        }
        return ret;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.tooltips.ToolTipHandler# isTooltipDisabledUntilNextRefocus()
     */
    @Override
    public boolean isTooltipDisabledUntilNextRefocus() {
        return true;
    }

    @Override
    public boolean isTooltipWithoutFocusEnabled() {
        // TODO Auto-generated method stub
        return true;
    }

    /**
     *
     */
    protected void onRollOut() {
        setContentAreaFilled(false);
    }

    /**
     *
     */
    protected void onRollOver() {
        setContentAreaFilled(true);
    }

    /**
     * @param b
     */
    public void setRolloverEffectEnabled(final boolean b) {
        if (b) {
            if (rollOverlistener == null) {
                rollOverlistener = new MouseAdapter() {
                    @Override
                    public void mouseEntered(final MouseEvent e) {
                        ExtButton.this.onRollOver();
                    }

                    @Override
                    public void mouseExited(final MouseEvent e) {
                        ExtButton.this.onRollOut();
                    }

                    /*
                     * (non-Javadoc)
                     *
                     * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
                     */
                    @Override
                    public void mouseReleased(final MouseEvent e) {
                        ExtButton.this.onReleased();
                    }
                };
            }
            addMouseListener(rollOverlistener);
            onRollOut();
        } else {
            if (rollOverlistener != null) {
                removeMouseListener(rollOverlistener);
                rollOverlistener = null;
            }
        }
    }

    private Icon disabledIcon = null;

    public Icon getDisabledIcon() {
        if (disabledIcon == null) {
            disabledIcon = org.appwork.resources.AWUTheme.I().getDisabledIcon(getIcon());
            if (disabledIcon != null) {
                firePropertyChange(DISABLED_ICON_CHANGED_PROPERTY, null, disabledIcon);
            }
        }
        return disabledIcon;
    }

    /**
     *
     */
    protected void onReleased() {
        // TODO Auto-generated method stub
    }

    public void setTooltipFactory(final TooltipFactory tooltipFactory) {
        this.tooltipFactory = tooltipFactory;
        ToolTipController.getInstance().register(this);
    }

    public ExtButton setTooltipsEnabled(final boolean b) {
        if (b) {
            ToolTipController.getInstance().register(this);
        } else {
            ToolTipController.getInstance().unregister(this);
        }
        return this;
    }

    @Override
    public void setToolTipText(final String text) {
        putClientProperty(JComponent.TOOL_TIP_TEXT_KEY, text);
        setTooltipsEnabled(StringUtils.isNotEmpty(getToolTipText()));
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
    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.tooltips.ToolTipHandler#getTooltipDelay()
     */
}
