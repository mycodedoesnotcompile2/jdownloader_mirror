package jd.gui.swing.jdgui.views.settings.panels;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;

import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import jd.gui.swing.jdgui.views.settings.components.SettingsComponent;
import jd.gui.swing.jdgui.views.settings.panels.urlordertable.UrlOrderTable;

public class UrlOrderContainer extends org.appwork.swing.MigPanel implements SettingsComponent {

    private UrlOrderTable urlOrder;

    public UrlOrderContainer(UrlOrderTable urlOrder) {
        super("ins 0", "[grow,fill]", "[]");
        this.urlOrder = urlOrder;
        final UrlOrderTable table = urlOrder;
        /*
         * The table always has a fixed number of rows, so the scrollpane is forced to the full table height (header + all rows). Without
         * this, the layout may shrink the scrollpane to a fraction of a row.
         */
        JScrollPane sp = new JScrollPane(urlOrder) {
            @Override
            public Dimension getPreferredSize() {
                final Dimension dim = super.getPreferredSize();
                dim.height = getFullHeight();
                return dim;
            }

            @Override
            public Dimension getMinimumSize() {
                final Dimension dim = super.getMinimumSize();
                dim.height = getFullHeight();
                return dim;
            }

            private int getFullHeight() {
                final Insets insets = getInsets();
                int height = insets.top + insets.bottom + table.getPreferredSize().height;
                if (table.getTableHeader() != null) {
                    height += table.getTableHeader().getPreferredSize().height;
                }
                return height;
            }
        };
        sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        /*
         * Let mouse wheel events reach the parent scrollpane of the settings panel. Merely disabling wheel scrolling is not enough: the
         * installed wheel listener still swallows the event, so it is replaced by a listener that forwards the event to the parent.
         */
        sp.setWheelScrollingEnabled(false);
        for (MouseWheelListener l : sp.getMouseWheelListeners()) {
            sp.removeMouseWheelListener(l);
        }
        final MouseWheelListener forwarder = new MouseWheelListener() {
            @Override
            public void mouseWheelMoved(MouseWheelEvent e) {
                final Component source = (Component) e.getSource();
                final Container parent = source.getParent();
                if (parent != null) {
                    parent.dispatchEvent(SwingUtilities.convertMouseEvent(source, e, parent));
                }
            }
        };
        sp.addMouseWheelListener(forwarder);
        table.addMouseWheelListener(forwarder);
        if (table.getTableHeader() != null) {
            table.getTableHeader().addMouseWheelListener(forwarder);
        }

        add(sp);
    }

    @Override
    public String getConstraints() {
        return null;
        // return "height n:n:" + (urlOrder.getPreferredSize().height + 32);
    }

    @Override
    public boolean isMultiline() {
        return true;
    }
}
