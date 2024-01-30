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

import java.awt.Point;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import javax.swing.ButtonModel;
import javax.swing.JCheckBox;
import javax.swing.JComponent;

import org.appwork.storage.config.handler.BooleanKeyHandler;
import org.appwork.storage.config.swing.ValueProvider;
import org.appwork.storage.config.swing.models.ConfigToggleButtonModel;
import org.appwork.storage.config.swing.models.KeyHandlerProviderBridge;
import org.appwork.swing.components.tooltips.ExtTooltip;
import org.appwork.swing.components.tooltips.ToolTipHandler;
import org.appwork.swing.components.tooltips.TooltipTextDelegateFactory;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;

public class ExtCheckBox extends JCheckBox implements ToolTipHandler {
    /**
     *
     */
    private static final long          serialVersionUID = 3223817461429862778L;
    private JComponent[]               dependencies;
    private TooltipTextDelegateFactory tooltipFactory;

    /**
     * @param filename
     * @param lblFilename
     */
    public ExtCheckBox(final JComponent... components) {
        super();
        tooltipFactory = new TooltipTextDelegateFactory(this);
        SwingUtils.setOpaque(this, false);
        // addActionListener(this);
        setDependencies(components);
    }

    public ExtCheckBox(final BooleanKeyHandler keyhandler, final JComponent... components) {
        this(new KeyHandlerProviderBridge<Boolean>(keyhandler), components);

    }

    /**
     * @param class1
     * @param string
     * @param table
     * @param btadd
     * @param btRemove
     */
    public ExtCheckBox(final ValueProvider<Boolean> provider, final JComponent... components) {
        this(components);
        setModel(new ConfigToggleButtonModel(provider));
        updateDependencies();
    }

    public void setModel(final ButtonModel newModel) {
        super.setModel(newModel);
        newModel.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(final ItemEvent e) {
                new EDTRunner() {
                    @Override
                    protected void runInEDT() {
                        updateDependencies();
                    }
                };
            }
        });
    }

    public JComponent[] getDependencies() {
        return dependencies;
    }

    public void setDependencies(final JComponent... dependencies) {
        this.dependencies = dependencies;
        updateDependencies();
    }

    public TooltipTextDelegateFactory getTooltipFactory() {
        return tooltipFactory;
    }

    public void setTooltipFactory(final TooltipTextDelegateFactory tooltipFactory) {
        this.tooltipFactory = tooltipFactory;
    }

    public void updateDependencies() {
        if (dependencies != null) {
            ArrayList<ExtCheckBox> boxes = new ArrayList<ExtCheckBox>();
            for (final JComponent c : dependencies) {
                c.setEnabled(getDependenciesLogic(c, isSelected()));
                if (c instanceof ExtCheckBox) {
                    boxes.add((ExtCheckBox) c);
                }
            }
            for (ExtCheckBox box : boxes) {
                box.updateDependencies();
            }
        }
    }

    /**
     * @param c
     * @param b
     * @return
     */
    protected boolean getDependenciesLogic(final JComponent c, final boolean b) {
        return b;
    }

    @Override
    public boolean isTooltipWithoutFocusEnabled() {
        return true;
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

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.swing.components.tooltips.ToolTipHandler# isTooltipDisabledUntilNextRefocus()
     */
    @Override
    public boolean isTooltipDisabledUntilNextRefocus() {
        return true;
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
     * @param jLabel
     * @return
     */
    public JComponent addDependency(JComponent c) {
        JComponent[] newDeps = new JComponent[dependencies.length + 1];
        System.arraycopy(dependencies, 0, newDeps, 0, dependencies.length);
        newDeps[newDeps.length - 1] = c;
        dependencies = newDeps;
        updateDependencies();
        return c;
    }
}
