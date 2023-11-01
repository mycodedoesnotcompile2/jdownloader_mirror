package org.appwork.utils.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.FocusTraversalPolicy;
import java.util.ArrayList;
import java.util.List;

import javax.swing.AbstractButton;
import javax.swing.JTextField;
import javax.swing.text.JTextComponent;

public class ListFocusTraversalPolicy extends FocusTraversalPolicy {
    /**
     * @param comp
     * @return
     */
    public static List<Component> getFocusableComponents(Container comp) {
        ArrayList<Component> ret = new ArrayList<Component>();
        for (Component c : comp.getComponents()) {
            if (c.isFocusable()) {
                boolean allowed = false;
                // Class<?> sub = c.getClass().getSuperclass();
                allowed |= c instanceof AbstractButton;
                allowed |= c instanceof JTextComponent && ((JTextComponent) c).isEditable();
                allowed |= c instanceof JTextField;
                if (allowed) {
                    ret.add(c);
                }
                if (c instanceof Container) {
                    ret.addAll(getFocusableComponents((Container) c));
                }
            }
        }
        return ret;
    }

    private final List<Component> focusOrder;

    public ListFocusTraversalPolicy(List<Component> focusOrder) {

        this.focusOrder = focusOrder;
    }

    @Override
    public Component getLastComponent(Container aContainer) {
        return focusOrder.get(focusOrder.size() - 1);
    }

    @Override
    public Component getFirstComponent(Container aContainer) {
        return focusOrder.get(0);
    }

    @Override
    public Component getDefaultComponent(Container aContainer) {
        return focusOrder.get(0);
    }

    @Override
    public Component getComponentBefore(Container aContainer, Component aComponent) {
        Component newC = null;
        int index = focusOrder.indexOf(aComponent);
        System.out.println(index);
        if (index == -1) {
            index = 0;
        }
        index--;
        for (int i = 0; i < focusOrder.size(); i++) {
            int bi = (index - i);
            while (bi < 0) {
                bi += focusOrder.size();
            }

            newC = focusOrder.get(bi);
            if (newC.isEnabled() && newC.isVisible()) {

                break;
            }
        }

        return newC;
    }

    @Override
    public Component getComponentAfter(Container aContainer, Component aComponent) {
        Component newC = null;
        int index = focusOrder.indexOf(aComponent) + 1;
        for (int i = 0; i < focusOrder.size(); i++) {

            newC = focusOrder.get((index + i) % focusOrder.size());
            if (newC.isEnabled() && newC.isVisible()) {
                break;
            }
        }

        return newC;
    }
}