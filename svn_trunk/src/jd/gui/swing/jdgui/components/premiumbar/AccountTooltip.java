package jd.gui.swing.jdgui.components.premiumbar;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

import jd.gui.swing.jdgui.views.settings.panels.accountmanager.AccountEntry;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.MultiHostHost;
import net.miginfocom.swing.MigLayout;

import org.appwork.swing.components.tooltips.PanelToolTip;
import org.appwork.swing.components.tooltips.ToolTipController;
import org.appwork.swing.components.tooltips.TooltipPanel;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.locator.AbstractLocator;
import org.jdownloader.DomainInfo;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.updatev2.gui.LAFOptions;

public class AccountTooltip extends PanelToolTip {
    private Color                 color;
    private AccountListTable      table;
    private AccountListTableModel model;
    private AccountTooltipOwner   owner;

    public Point getDesiredLocation(JComponent activeComponent, Point ttPosition) {
        if (owner instanceof ServicePanel) {
            ttPosition.y = activeComponent.getLocationOnScreen().y - getPreferredSize().height;
            ttPosition.x = activeComponent.getLocationOnScreen().x;
            return AbstractLocator.correct(ttPosition, getPreferredSize());
        }
        final Point mouseLocation = ToolTipController.getMouseLocation();
        if (mouseLocation != null) {
            return mouseLocation;
        }
        return ttPosition;
    }

    public AccountTooltip(AccountTooltipOwner owner, AccountServiceCollection accountCollection) {
        super(new TooltipPanel("ins 0,wrap 1", "[]", "[][][][][grow,fill]") {
            @Override
            public Dimension getPreferredSize() {
                Dimension pref = super.getPreferredSize();
                // pref.width = 850;
                // pref.height = 600;
                // System.out.println(pref);
                return pref;
            }
        });
        this.owner = owner;
        color = (LAFOptions.getInstance().getColorForTooltipForeground());
        final LinkedList<AccountEntry> domains = new LinkedList<AccountEntry>();
        for (Account acc : accountCollection) {
            domains.add(new AccountEntry(acc));
        }
        table = new AccountListTable(model = new AccountListTableModel(this, owner));
        model.setData(domains);
        model.addTableModelListener(new TableModelListener() {
            @Override
            public void tableChanged(TableModelEvent e) {
                if (AccountTooltip.this.owner != null) {
                    AccountTooltip.this.owner.redraw();
                }
                table.getTableHeader().repaint();
            }
        });
        table.getTableHeader().setOpaque(false);
        JScrollPane sp;
        String txt = accountCollection.getDomainInfo().getTld();
        if (accountCollection.isMulti()) {
            txt = _GUI.T.AccountTooltip_AccountTooltip_multi(accountCollection.getDomainInfo().getTld());
        }
        JLabel label = new JLabel(txt, accountCollection.getDomainInfo().getFavIcon(), JLabel.LEFT);
        SwingUtils.toBold(label);
        label.setForeground(LAFOptions.getInstance().getColorForTooltipForeground());
        panel.add(label, "gapleft 5,pushx,growx");
        panel.add(table.getTableHeader());
        panel.add(table);
        if (accountCollection.isMulti()) {
            panel.setLayout(new MigLayout("ins 0,wrap 1", "[grow,fill]", "[][][][][grow,fill]"));
            label = new JLabel(_GUI.T.AccountTooltip_AccountTooltip_supported_hosters());
            SwingUtils.toBold(label);
            label.setForeground(LAFOptions.getInstance().getColorForTooltipForeground());
            panel.add(label);
            List<DomainInfo> dis = getDomainInfos(accountCollection);
            final JList list = new JList(dis.toArray(new DomainInfo[] {}));
            // list.setPreferredSize(new Dimension(400, 750));
            list.setLayoutOrientation(JList.VERTICAL_WRAP);
            final ListCellRenderer org = list.getCellRenderer();
            list.setCellRenderer(new ListCellRenderer() {
                public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
                    DomainInfo di = (DomainInfo) value;
                    JLabel ret = (JLabel) org.getListCellRendererComponent(list, "", index, isSelected, cellHasFocus);
                    ret.setForeground(LAFOptions.getInstance().getColorForTooltipForeground());
                    ret.setText(di.getTld());
                    ret.setIcon(di.getFavIcon());
                    ret.setOpaque(false);
                    ret.setBackground(null);
                    return ret;
                }
            });
            list.setVisibleRowCount(dis.size() / 5);
            // list.setFixedCellHeight(22);
            // list.setFixedCellWidth(22);
            list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            list.setOpaque(false);
            panel.add(list);
        } else {
            panel.setLayout(new MigLayout("ins 0,wrap 1", "[grow,fill]", "[][][grow,fill]"));
        }
        // panel.add(sp = new JScrollPane(table));
        // sp.setBackground(null);
        // table.setBackground(LAFOptions.getInstance().getColorForTooltipBackground());
        // table.setOpaque(true);
        // table.getTableHeader().setBackground(LAFOptions.getInstance().getColorForTooltipBackground());
        // panel.setPreferredSize(new Dimension(500, 100));
        // panel.setPreferredSize(new Dimension(panel.getPreferredSize().width, 400));
    }

    @Override
    public Dimension getPreferredSize() {
        return super.getPreferredSize();
    }

    private List<DomainInfo> getDomainInfos(final AccountServiceCollection accountCollection) {
        final HashSet<DomainInfo> domains = new HashSet<DomainInfo>();
        for (final Account acc : accountCollection) {
            final AccountInfo ai = acc.getAccountInfo();
            if (ai == null) {
                continue;
            }
            final List<MultiHostHost> supported = ai.getMultiHostSupportV2();
            if (supported == null) {
                continue;
            }
            for (final MultiHostHost sup : supported) {
                switch (sup.getStatus()) {
                case WORKING:
                case WORKING_UNSTABLE:
                    domains.add(sup.getDomainInfo());
                    break;
                default:
                    break;
                }
            }
        }
        final ArrayList<DomainInfo> ret = new ArrayList<DomainInfo>(domains);
        if (ret.size() < 2) {
            return ret;
        }
        Collections.sort(ret, new Comparator<DomainInfo>() {
            @Override
            public int compare(DomainInfo o1, DomainInfo o2) {
                return o1.getTld().compareToIgnoreCase(o2.getTld());
            }
        });
        return ret;
    }

    public void update() {
        table.getModel().fireTableStructureChanged();
    }
}
