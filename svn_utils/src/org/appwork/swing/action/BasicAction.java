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
package org.appwork.swing.action;

import java.awt.event.KeyEvent;
import java.lang.reflect.Field;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.KeyStroke;

import org.appwork.swing.components.tooltips.TooltipFactory;
import org.appwork.utils.KeyUtils;

/**
 * @author thomas
 * 
 */
public abstract class BasicAction extends AbstractAction {

    /**
     * 
     */
    private static final long serialVersionUID = -198177718803470771L;

    public static int charToMnemonic(final char mnemonic) {
        try {

            final Field f = KeyEvent.class.getField("VK_" + Character.toUpperCase(mnemonic));
            return (Integer) f.get(null);

        } catch (final Exception e) {

        }
        return 0;
    }

    private TooltipFactory tooltipFactory;

    private boolean        toggle = false;

    public BasicAction() {
        super();
    }

    /**
     * @param routerSendAction_RouterSendAction_
     */
    public BasicAction(final String name) {
        super(name);
    }

    public String getName() {
        return (String) this.getValue(Action.NAME);
    }

    public String getShortCutString() {
        final Object value = this.getValue(Action.ACCELERATOR_KEY);

        return value == null ? null : KeyUtils.getShortcutString((KeyStroke) this.getValue(Action.ACCELERATOR_KEY), true);
    }

    public Icon getSmallIcon() {
        return (Icon) this.getValue(Action.SMALL_ICON);
    }

    /**
     * @return
     */
    public TooltipFactory getTooltipFactory() {
        return this.tooltipFactory;
    }

    /**
     * Returns the actions description
     */
    public String getTooltipText() {
        try {
            final Object ret = this.getValue(Action.SHORT_DESCRIPTION);
            return ret == null ? null : ret.toString();
        } catch (final Exception e) {
            return null;
        }
    }

    // /**
    // * Sets the shortcut fort this action. a System dependend behaviour is
    // * choosen. e,g. WIndows+ Strg+ Acceleratir
    // *
    // * example: action.setAccelerator("ENTER"); defines a Enter shortcut
    // *
    // * @param accelerator
    // * @depcreated. use {@link #setAccelerator(KeyStroke)}
    // */
    // @Deprecated
    // public void setAccelerator(final String accelerator) {
    // KeyStroke ks;
    // if (accelerator != null && accelerator.length() > 0 &&
    // !accelerator.equals("-")) {
    // final Class<?> b = KeyEvent.class;
    // final String[] split = accelerator.split("\\+");
    // int mod = 0;
    // try {
    // final int splitLength = split.length;
    // for (int i = 0; i < splitLength - 1; ++i) {
    // if (new Regex(split[i], "^CTRL$").matches()) {
    // mod = mod | Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
    // } else if (new Regex(split[i], "^SHIFT$").matches()) {
    // mod = mod | KeyEvent.SHIFT_DOWN_MASK;
    // } else if (new Regex(split[i], "^ALTGR$").matches()) {
    // mod = mod | KeyEvent.ALT_GRAPH_DOWN_MASK;
    // } else if (new Regex(split[i], "^ALT$").matches()) {
    // mod = mod | KeyEvent.ALT_DOWN_MASK;
    // } else if (new Regex(split[i], "^META$").matches()) {
    // mod = mod | KeyEvent.META_DOWN_MASK;
    // } else {
    //       org.appwork.loggingv3.LogV3.info(getName() + " Shortcuts: skipping wrong modifier " + mod +
    // " in " + accelerator);
    // }
    // }
    //
    // final Field f = b.getField("VK_" + split[splitLength - 1].toUpperCase());
    // final int m = (Integer) f.get(null);
    // putValue(AbstractAction.ACCELERATOR_KEY, ks = KeyStroke.getKeyStroke(m,
    // mod));
    //       org.appwork.loggingv3.LogV3.finest(getName() + " Shortcuts: mapped " + accelerator + " to " +
    // ks);
    // } catch (final Exception e) {
    // // JDLogger.exception(e);
    // putValue(AbstractAction.ACCELERATOR_KEY, ks =
    // KeyStroke.getKeyStroke(accelerator.charAt(accelerator.length() - 1),
    // Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    //       org.appwork.loggingv3.LogV3.finest(getName() + " Shortcuts: mapped " + accelerator + " to " +
    // ks + " (Exception)");
    // }
    // }
    // }

    /**
     * For toggle actions, this method returns if it is currently selected
     * 
     * @return
     */
    public boolean isSelected() {
        final Object value = this.getValue(Action.SELECTED_KEY);
        return value == null ? false : (Boolean) value;
    }

    public boolean isToggle() {
        return this.toggle;
    }

    public BasicAction setAccelerator(final KeyStroke stroke) {
        this.putValue(Action.ACCELERATOR_KEY, stroke);
        return this;
    }

    /**
     * Sets the Mnemonic for this icon. Mnemonics are used to activate actions
     * using the keyboard (ALT + Mnemonic) usualy the mnemonic is part of the
     * name, and thus gets underlined in menus.
     * 
     * Always set the Mnemonic AFTER! setting the title
     * 
     * @param key
     */

    public void setMnemonic(String key) {
        if (this.getName() == null) { throw new IllegalStateException("First set Name"); }
        if (key == null) {
            key = "-";
        }
        final char mnemonic = key.charAt(0);

        if (mnemonic != 0 && !key.contentEquals("-")) {

            final int m = BasicAction.charToMnemonic(mnemonic);
            this.putValue(Action.MNEMONIC_KEY, m);
            this.putValue(Action.DISPLAYED_MNEMONIC_INDEX_KEY, this.getName().indexOf(m));
        }
    }

    public void setName(final String name) {
        this.putValue(Action.NAME, name);

    }

    /**
     * Sets the action selected. WARNING. Swing usualy handles the selection
     * state
     * 
     * @param selected
     */
    public void setSelected(final boolean selected) {
        this.putValue(Action.SELECTED_KEY, selected);
        this.toggle = true;
    }

    public void setSmallIcon(final Icon icon) {
        this.putValue(Action.SMALL_ICON, icon);
    }

    public void setTooltipFactory(final TooltipFactory factory) {
        this.tooltipFactory = factory;
    }

    public void setTooltipText(final String text) {
        this.putValue(Action.SHORT_DESCRIPTION, text);
    }

}
